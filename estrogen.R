library(pacman)
p_load(char = c('tidyverse','broom','pdftools', 'fs', 'future','furrr',
                'janitor','haven'))

parse_datadict <- function(f){
  if(!file_exists(f)) stop('File not found')
  dd <- pdf_text(f) %>%
    str_split('\\r\\n') %>%
    unlist()
  dd <- dd[dd!='']
  dd <- dd[str_detect(dd, ', 2016', negate=T)]
  DD <- tibble(raw = dd) %>%
    mutate(dataset = ifelse(str_detect(raw, 'Data Set Name'), raw, NA)) %>%
    fill(dataset) %>%
    mutate(dataset = str_remove(dataset, 'Data Set Name: '),
           raw = str_remove_all(raw, '\\w+\\.'),
           raw = str_squish(raw)) %>%
    filter(str_detect(raw, 'Data Set Name', negate = T),
           str_detect(raw, 'Variable', negate = T)) %>%
    separate(raw, c('num','variable','type','length','description'), sep = ' ',
             remove = T, extra = 'merge')
  return(DD)
}

data_dict1 <- path('data','raw','archives','framcohort','Data_Dictionary.pdf')
file_exists(data_dict1)
data_dict2 <- path('data', 'raw','archives','framoffspring','Data_Dictionary.pdf')
file_exists(data_dict2)

DD1 <- parse_datadict(data_dict1)
DD2 <- parse_datadict(data_dict2)

estrogen1 <- sort(unique(grep('ESTROGEN', DD1$description, value = T)))
estrogen2 <- sort(unique(grep('ESTROGEN', DD2$description, value = T)))
estrogen_descr <- union(estrogen1, estrogen2) # Given to Bhattacharyya


estrogen_select <- as.numeric( # supplied by Bhattacharyya
  c("2", "3", "6", "11", "12", "13", "14", "22",
    "23", "24", "39", "30", "33", "35", "49", "40",
    '41','42','43','44','45', "46", "56"))
estrogen_use <- estrogen_descr[estrogen_select]
ind1 <- match(estrogen_use, DD1$description)
ind2 <- match(estrogen_use, DD2$description)

sas_dir_orig <- path('data','raw','archives','framcohort','Datasets'); dir_exists(sas_dir_orig)
sas_dir_offspring <- path('data','raw','archives','framoffspring','Datasets'); dir_exists(sas_dir_offspring)

from_orig <- DD1[ind1[!is.na(ind1)],] %>% arrange(dataset) %>%
  mutate(exam = c(27,27,28,17,18,19,19,21,22,23,24,25,26)) # parsing out exam numbers
from_orig <- from_orig %>%
  mutate(dat = map2(dataset, variable,
                    ~ read_sas(path(sas_dir_orig, .x), col_select = c('PID',.y)))) %>%
  group_by(exam)
dats_by_exam_orig = group_split(from_orig) %>% map(~Reduce(left_join, .$dat)) %>%
  map(~mutate(., estr = rowSums(select(., -PID), na.rm=T)>0)) %>%
  map(~select(., PID, estr))
names(dats_by_exam_orig) = as.character(group_keys(from_orig)$exam)
estrogen_orig <- bind_rows(dats_by_exam_orig, .id = 'exam') %>%
  mutate(estr = ifelse(estr, 1, 0)) %>% clean_names() %>%
  mutate(exam=as.integer(exam))


from_offspring <- DD2[ind2[!is.na(ind2)],] %>% arrange(dataset) %>%
  mutate(exam = c(1,2,8,3,4,5,5,1,2,3,4,5,6,7))
from_offspring <- from_offspring %>%
  mutate(dat = map2(dataset, variable,
                    ~read_sas(path(sas_dir_offspring, .x),
                              col_select = c(ifelse(str_detect(.x, 'lex'), 'pid', 'PID'),.y)) %>% #Needed for some files
                      janitor::clean_names())) %>%
  group_by(exam)
dats_by_exam_offspring <- group_split(from_offspring) %>%
  map(~Reduce(left_join, .$dat)) %>%
  map(~mutate(., estr = rowSums(select(., -pid), na.rm=T)>0)) %>%
  map(~select(., pid, estr))
names(dats_by_exam_offspring) <- group_keys(from_offspring)$exam
estrogen_offspring <- bind_rows(dats_by_exam_offspring, .id = 'exam') %>%
  mutate(estr = ifelse(estr, 1,0)) %>% clean_names() %>%
  mutate(exam = as.integer(exam))


# Add age, sex and dates for exams ---------------------------------------------

info_orig <- read_sas('data/raw/newdat.date/Cohort/vr_dates_2014_a_0912d_yr.sas7bdat') %>%
  clean_names() %>%
  select(pid,starts_with('age'), starts_with('examyr'), sex)
info_offspring <- read_sas('data/raw/newdat.date/Offspring/vr_dates_2014_a_0912d_yr.sas7bdat') %>%
  clean_names() %>%
  select(pid, starts_with('age'), starts_with('examyr'), sex)

process_fn <- function(d){
  d1 <- d %>%
    select(pid, starts_with('age')) %>%
    gather(variable, age, -pid) %>%
    separate(variable, c('dummy','exam'), sep=3, convert = T) %>%
    select(-dummy)
  d2 <- d %>%
    select(pid, starts_with('examyr')) %>%
    gather(variable, examyr, -pid) %>%
    separate(variable, c('dummy','exam'), sep = 6, convert=T) %>%
    select(-dummy)
  out <-  d1 %>% left_join(d2) %>% left_join(d %>% select(pid, sex))
  return(out)
}

info_orig <- process_fn(info_orig)
info_offspring <- process_fn(info_offspring)

estrogen_orig <- estrogen_orig %>% left_join(info_orig)
estrogen_offspring <- estrogen_offspring %>% left_join(info_offspring)
# Questions about males ---------------------------------------------------

estrogen_orig %>% tabyl(exam, estr, sex) %>% adorn_percentages() %>% adorn_pct_formatting()
estrogen_offspring %>% tabyl(exam, estr, sex) %>% adorn_percentages() %>% adorn_pct_formatting()


# Just females ------------------------------------------------------------

estrogen_orig_f <- estrogen_orig %>% filter(sex==2)
estrogen_offspring_f <- estrogen_offspring %>% filter(sex==2)

estrogen_f <- list('orig' = estrogen_orig_f,
                   'offspring' = estrogen_offspring_f) %>%
  bind_rows(.id='cohort') %>%
  select(pid, exam, estr, sex, age, examyr, everything()) %>%
  as_tibble() %>%
  mutate(year_cat = cut(examyr,
                        breaks = seq(1978, 2008, by=5),
                        include.lowest=F,
                        right = F),
         age_cat = cut(age,
                       breaks = c(0, seq(20,80, by=10), Inf),
                       labels = c('< 20','20-30','30-40','40-50',
                                  '50-60','60-70','70-80','80 and above'))) %>%
  mutate(year_mid = case_when(
    year_cat=='[1978,1983)'~1980,
    year_cat=='[1983,1988)'~1985,
    year_cat=='[1988,1993)'~1990,
    year_cat=='[1993,1998)'~1995,
    year_cat=='[1998,2003)'~2000,
    year_cat=='[2003,2008)'~2005
  ),
  year_mid = as.factor(year_mid))
openxlsx::write.xlsx(estrogen_f, file = 'Estrogen.xlsx')

out <- tabyl(filter(estrogen_f, age>=50), year_mid, estr, age_cat) %>%
  adorn_percentages() %>%
  bind_rows(.id = 'age_cat') %>%
  filter(!is.na(`1`), !is.na(year_mid))
