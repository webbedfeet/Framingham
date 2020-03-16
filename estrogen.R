library(pacman)
p_load(char = c('tidyverse','broom','pdftools', 'fs', 'future','furrr'))

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
data_dict2 <- path('data', 'raw','archives','framoffspring','Data_Dictionary.pdf')
DD1 <- parse_datadict(data_dict1)
DD2 <- parse_datadict(data_dict2)

estrogen1 <- sort(unique(grep('ESTROGEN', DD1$description, value = T)))
estrogen2 <- sort(unique(grep('ESTROGEN', DD2$description, value = T)))
estrogen_descr <- union(estrogen1, estrogen2)


x <- str_split("2 3 6 11 12 13 14 22 23 24 39 30 33 35 49 40 41 42 43 44 45 46 56")
estrogen_select <- as.numeric(
  c("2", "3", "6", "11", "12", "13", "14", "22",
    "23", "24", "39", "30", "33", "35", "49", "40",
    '41','42','43','44','45', "46", "56"))

