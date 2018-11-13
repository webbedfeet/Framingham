## %######################################################%##
#                                                          #
####                    Data munging                    ####
#                                                          #
## %######################################################%##

# Setup ---------------------------------------------------------------------------------------

ProjTemplate::reload()
datadir <- set_datadir()

# Matching participation in study with risk factor measurments ------------

# load(file.path(datadir, "rda", "munged_data_Oct17.rda"))
# load(file.path(datadir, "rda", "event_data.rda"))
#
# for (id in unique(original_datum$PID)) {
#   for (yr in unique(original_datum$year)) {
#     if (yr %nin% dat_attend_exploded$yrs_in_study[dat_attend_exploded$PID == id]) {
#       original_datum[
#         original_datum$PID == id & original_datum$year == yr,
#         3:ncol(original_datum)
#       ] <- NA
#     }
#   }
# }
#
#
# original_datum %>%
#   select(year, beta:bmi, diab:steroid) %>%
#   group_by(year) %>%
#   summarize_all(mean, na.rm = T) -> risk_summaries
# tmp <- dat_attend_timedep %>%
#   mutate(py = Stop - Start) %>% # Quick person-year calculation
#   group_by(decade_in_study) %>%
#   summarise(events = sum(Status), py = sum(py)) %>%
#   mutate(rate = events / py) %>%
#   select(-events, -py) %>%
#   filter(decade_in_study %in% c("1980-1989", "1990-1999", "2000-2009")) %>%
#   mutate(year = as.character(c(1980, 1990, 2000))) -> incidence_summary
#
# incidence_summary %>%
#   left_join(risk_summaries) %>%
#   select(-decade_in_study, -bmi) %>%
#   mutate(year = as.numeric(year)) %>%
#   gather(variable, value, -year) %>%
#   ggplot(aes(year, value, color = variable)) +
#   geom_line()
#
# decade_dat <- incidence_summary %>%
#   left_join(risk_summaries) %>%
#   select(-decade_in_study)
#

# Unifying data for time-dependent analyses: Attendance and Fractures -------------------------------

## Original cohort
d11 <- read_sas(file.path(datadir, "sas", "vr_dates_2014_a_0912d_yr_fram.sas7bdat"))
d21 <- read_sas(file.path(datadir, "sas", "vr_fxrev_2012_0_0746d_yr_fram.sas7bdat"))


#' # Mon Apr 23 23:45:13 2018
#' We will assume that all individuals started their time in the study on January 1st of the year
#' of their first exam. Starting from that assumption, we can compute the dates of each exam, as
#' well as their final end date. This is what we should  use for all our computations. The degree
#' of error with this approach is minimal. We can then compute the person times, based on end times
#' being the minimum of the fracture date, the death date and 2 years after the last exam date. This
#' is the extent of the extrapolation we'll deal with in this study.


# integrating fracture data -------------------------------------------------------------------

## Original cohort

#' The requirements for creating the event/feature/time dataset are that we compute the
#' actual years in the study, identify events within the duration of examinations (to within
#' two years of the last exam), and for each individual, create a record for each calendar year
#' in the study, using LVCF to fill in the feature information. This should be repeatable for the
#' offspring cohort since most files have similar formats and variable names.
#'

## Gather times of exams, along with initial age and gender
dat_exams_orig <- d11 %>%
  select(PID, age1, sex, examyr1, starts_with('date')) %>%
  gather(exam, dt, starts_with('date')) %>%
  rename('start_yr' = 'examyr1') %>%
  mutate(exam = str_remove(exam, 'date')) %>%
  right_join(d11 %>%
               select(PID, age1, sex, starts_with('examyr')) %>%
               gather(exam, exam_yr, starts_with('examyr')) %>%
               mutate(exam = str_remove(exam, 'examyr')),
             by = c("PID", "exam", 'age1','sex')) %>%
  mutate(exam = as.numeric(exam)) %>%
  mutate(start_yr = ifelse(exam==1, exam_yr, start_yr),
         dt = ifelse(exam==1, 0, dt)) %>%
  filter(!is.na(dt)) %>%
  arrange(PID, exam)

## Summarize the duration of exams per individual

dat_exams_duration_orig <- dat_exams_orig %>%
  nest(exam, dt, exam_yr) %>%
  mutate(last_exam_day = map_dbl(data, ~max(.$dt))) %>%
  select(-data)

## Add date of first fracture, since this determines the end of follow-up

first_frac_orig <- d21 %>%
  group_by(PID) %>%
  filter(fxdate == min(fxdate)) %>%
  ungroup() %>%
  select(PID, fxdate) %>% distinct()

dat_exams_duration_orig <- dat_exams_duration_orig %>%
  left_join(first_frac_orig) %>%
  mutate(last_exam_dt = ymd(paste0(start_yr,'0101'))+last_exam_day,
         fx_dt = ymd(paste0(start_yr, '0101')) + fxdate) %>%
  filter(is.na(fx_dt) | (year(fx_dt) >= start_yr & year(fx_dt) <= year(last_exam_dt) + 2)) %>%
  mutate(end_dt = pmax(last_exam_dt %m+% years(2), fx_dt, na.rm = T)) %>%
  mutate(end_yr = year(end_dt),
         dec_end_yr = decimal_date(end_dt)) %>%
  distinct()

## Create year-based dataset

dat_years_orig <- dat_exams_duration_orig %>% select(PID, start_yr, end_yr) %>%
  mutate(years = map2(start_yr, end_yr, ~seq(.x, .y))) %>%
  select(PID, years) %>%
  unnest() %>%
  left_join(dat_exams_duration_orig %>% select(PID, age1, sex, start_yr, end_dt)) %>% # subject-based data
  group_by(PID) %>%
  mutate(age = age1 + years - min(years)) %>% # Age in each year
  ungroup() %>%
  select(-age1) %>%
  left_join(dat_exams_duration_orig %>% filter(!is.na(fx_dt)) %>% mutate(years = year(fx_dt)) %>%
              select(PID, years, fx_dt),
            by = c("PID",'years')) %>% # Grab fracture dates
  mutate(pyears = 1) %>%
  mutate(pyears = ifelse(years != year(end_dt), pyears, decimal_date(end_dt) - years),
         fx_status = ifelse(is.na(fx_dt), 0,1)) %>% # Compute person years exposed, and fracture status
  # add last exam number for each year
  left_join(dat_exams_orig %>% select(PID, exam, exam_yr), by = c('PID'='PID','years' = 'exam_yr')) %>%
  group_by(PID) %>% fill(exam) %>% ungroup()

## Offspring cohort

#' The requirements for creating the event/feature/time dataset are that we compute the
#' actual years in the study, identify events within the duration of examinations (to within
#' two years of the last exam), and for each individual, create a record for each calendar year
#' in the study, using LVCF to fill in the feature information. This should be repeatable for the
#' offspring cohort since most files have similar formats and variable names.
#'

## Gather times of exams, along with initial age and gender


d31 <- read_sas(file.path(datadir, "sas", "vr_dates_2014_a_0912d_yr_offspring.sas7bdat"))
d41 <- read_sas(file.path(datadir, "sas", "vr_fxrev_2012_1_0747d_yr_offspring.sas7bdat"))

dat_exams_off <- d31 %>%
  select(PID, age1, sex, examyr1, starts_with('date')) %>%
  gather(exam, dt, starts_with('date')) %>%
  rename('start_yr' = 'examyr1') %>%
  mutate(exam = str_remove(exam, 'date')) %>%
  right_join(d31 %>%
               select(PID, age1, sex, starts_with('examyr')) %>%
               gather(exam, exam_yr, starts_with('examyr')) %>%
               mutate(exam = str_remove(exam, 'examyr')),
             by = c("PID", "exam", 'age1','sex')) %>%
  mutate(exam = as.numeric(exam)) %>%
  mutate(start_yr = ifelse(exam==1, exam_yr, start_yr),
         dt = ifelse(exam==1, 0, dt)) %>%
  filter(!is.na(dt)) %>%
  arrange(PID, exam)

## Summarize the duration of exams per individual

dat_exams_duration_off <- dat_exams_off %>%
  nest(exam, dt, exam_yr) %>%
  mutate(last_exam_day = map_dbl(data, ~max(.$dt))) %>%
  select(-data)

first_frac_off <- d41 %>%
  group_by(PID) %>%
  filter(of_fxdate == min(of_fxdate)) %>%
  ungroup() %>%
  select(PID, of_fxdate) %>% distinct() %>%
  rename(fxdate = of_fxdate)

dat_exams_duration_off <- dat_exams_duration_off %>%
  left_join(first_frac_off) %>%
  mutate(last_exam_dt = ymd(paste0(start_yr,'0101')) + last_exam_day,
         fx_dt = ymd(paste0(start_yr, '0101')) + fxdate) %>%
  filter(is.na(fx_dt) | (year(fx_dt) >= start_yr & year(fx_dt) <= year(last_exam_dt) + 2)) %>% # Only fracs during exam period are counted
  mutate(end_dt = pmin(last_exam_dt %m+% years(2), fx_dt, na.rm = T)) %>%
  mutate(end_yr = year(end_dt),
         dec_end_yr = decimal_date(end_dt)) %>%
  distinct()

## Create year-based dataset

dat_years_off <- dat_exams_duration_off %>% select(PID, start_yr, end_yr) %>%
  mutate(years = map2(start_yr, end_yr, ~seq(.x, .y))) %>%
  select(PID, years) %>%
  unnest() %>%
  left_join(dat_exams_duration_off %>% select(PID, age1, sex, start_yr, end_dt)) %>% # subject-based data
  group_by(PID) %>%
  mutate(age = age1 + years - min(years)) %>% # Age in each year
  ungroup() %>%
  select(-age1) %>%
  left_join(dat_exams_duration_off %>% filter(!is.na(fx_dt)) %>% mutate(years = year(fx_dt)) %>%
              select(PID, years, fx_dt),
            by = c("PID",'years')) %>% # Grab fracture dates
  mutate(pyears = 1) %>%
  mutate(pyears = ifelse(years != year(end_dt), pyears, decimal_date(end_dt) - years),
         fx_status = ifelse(is.na(fx_dt), 0,1)) %>% # Compute person years exposed, and fracture status
  # add last exam number for each year
  left_join(dat_exams_off %>% select(PID, exam, exam_yr), by = c('PID'='PID','years' = 'exam_yr')) %>%
  group_by(PID) %>% fill(exam) %>% ungroup()


save(dat_years_off, dat_years_orig, dat_exams_duration_off, dat_exams_duration_orig,
     dat_exams_orig, dat_exams_off, file = 'data/rda/updatedEventsPY.rda', compress = T)

# Unifying data for time-dependent analyses: Attendance and Fractures -------------------------------

  ## Original cohort
  d11 <- read_sas(file.path(datadir, "sas", "vr_dates_2014_a_0912d_yr_fram.sas7bdat"))
d21 <- read_sas(file.path(datadir, "sas", "vr_fxrev_2012_0_0746d_yr_fram.sas7bdat"))

first_fracture_orig <- d21 %>%
  group_by(PID) %>%
  filter(fxdate == min(fxdate)) %>%
  ungroup() %>%
  select(PID, fxdate, YrFrac) %>%
  distinct()

dat_attend_orig <- d11 %>%
  select(PID, age1, sex, starts_with("examyr")) %>% # Work with calendar time
  gather(visit, yr, -PID, -age1, -sex) %>%
  separate(visit, c("label", "visit_no"), sep = 6, convert = T) %>% # Extracts exam number
  select(-label) %>%
  filter(!is.na(yr)) %>% # The ones missing are the ones that weren't seen at that exam
  nest(visit_no, yr) %>% # Stratify on exam, calendar year
  mutate(
    start_yr = map_dbl(data, ~min(.$yr)),
    end_yr = map_dbl(data, ~max(.$yr))
  ) %>% # Identify period that subject is in study
  select(-data) %>%
  left_join(first_fracture_orig) %>%
  mutate(
    end_duration = ifelse(is.na(YrFrac), end_yr, YrFrac), # Stop time at first fracture
    frac_indic = ifelse(is.na(YrFrac), 0, 1),
    year1 = start_yr,
    no_yrs = end_duration - start_yr + 1
  ) %>%
  mutate(end_duration = ifelse(end_duration - end_yr > 2, end_yr+2, end_duration)) %>%  # don't go beyond 2 yrs of last exam
  mutate(status = ifelse(!is.na(YrFrac) & (end_duration == YrFrac), 1, 0))

dat_attend_orig_exploded <-
  dat_attend_orig %>% mutate(yrs = map2(start_yr, end_duration, ~seq(.x, .y, by=1))) %>%
  mutate(age_cur = map2(age1, yrs, ~.x + seq_along(.y)-1)) %>% unnest() %>%
  mutate(yr_grp = cut(yrs, seq(1950, 2010, by=5))) %>%
  mutate(age_grp = cut(age_cur, c(0,40,50,60,70,80,105))) %>%
  mutate(frac_ind = ifelse(!is.na(YrFrac) & YrFrac == yrs, 1, 0)) %>%
  select(PID, sex, year1, yrs, age_cur, yr_grp, age_grp, frac_ind)


## Offspring cohort
d31 <- read_sas(file.path(datadir, "sas", "vr_dates_2014_a_0912d_yr_offspring.sas7bdat"))
d41 <- read_sas(file.path(datadir, "sas", "vr_fxrev_2012_1_0747d_yr_offspring.sas7bdat"))

dat_attend_offspring <- d31 %>%
  select(PID, age1, sex, starts_with("examyr")) %>%
  gather(exam, yr, starts_with("examyr")) %>%
  separate(exam, c("label", "exam_no"), sep = 6, convert = T) %>%
  select(-label) %>%
  filter(!is.na(yr)) %>%
  nest(exam_no, yr) %>%
  mutate(start_yr = map_dbl(data, ~min(.$yr))) %>%
  mutate(end_yr = map_dbl(data, ~max(.$yr))) %>%
  select(-data)

first_fracture_offspring <- d41 %>%
  filter(of_FxSite == 1) %>%
  group_by(PID) %>%
  filter(of_fxdate == min(of_fxdate, na.rm = T)) %>%
  ungroup() %>%
  select(PID, of_fxdate, YrFrac)

dat_attend_offspring <- dat_attend_offspring %>%
  left_join(first_fracture_offspring) %>%
  mutate(frac_ind = ifelse(is.na(YrFrac), 0, 1)) %>%
  filter((is.na(YrFrac) | ((YrFrac >= start_yr) & (YrFrac <= end_yr)))) %>% # Only accept fractures happening during the duration of exams
  distinct() %>%
  mutate(
    end_duration = pmin(end_yr, YrFrac, na.rm = T),
    no_yrs = end_duration - start_yr + 1
  ) %>%
  mutate(end_duration = ifelse(end_duration - end_yr > 2, end_yr + 2, end_duration)) %>%
  mutate(status = ifelse(!is.na(YrFrac) & (YrFrac == end_duration), 1, 0)) %>%
  mutate(year1 = start_yr)

dat_attend_offspring_exploded <-
  dat_attend_offspring %>% mutate(yrs = map2(start_yr, end_duration, ~seq(.x, .y, by=1))) %>%
  mutate(age_cur = map2(age1, yrs, ~.x + seq_along(.y)-1)) %>% unnest() %>%
  mutate(yr_grp = cut(yrs, seq(1970, 2015, by = 5))) %>%
  mutate(age_grp = cut(age_cur, c(0,40,50,60,70,80,105))) %>%
  mutate(frac_ind = ifelse(!is.na(YrFrac) & YrFrac == yrs, 1, 0)) %>%
  select(PID, sex, year1, yrs, age_cur, yr_grp, age_grp, frac_ind)



# Death -------------------------------------------------------------------

death_orig <- read_sas(file.path(datadir, 'newdat','framcohort','Datasets','vr_survdth_2011_m_0786d.sas7bdat'))
death_off <-  read_sas(file.path(datadir, 'newdat','framoffspring','Datasets','vr_survdth_2011_m_0786d.sas7bdat')) %>%
  filter(IDTYPE == 1)

tmp <- dat_attend_orig %>%
  select(PID, start_yr, end_yr) %>%
  mutate(start_dt = as.Date(paste0(as.character(start_yr),'-01-01'))) %>%
  left_join(death_orig) %>%
  mutate(death_dt = (start_dt + datedth),
         last_dt = (start_dt + lastatt)) %>%
  select(PID, start_dt, DTHRVWD, datedth, death_dt, lastatt, last_dt) %>%
  rename(death_indic = DTHRVWD, death_date = datedth)
dat_attend_orig <- dat_attend_orig %>% left_join(tmp)


tmp <- dat_attend_offspring %>%
  select(PID, start_yr, end_yr) %>%
  mutate(start_dt = as.Date(paste0(as.character(start_yr),'-01-01'))) %>%
  left_join(death_off) %>%
  mutate(death_dt = (start_dt + datedth),
         last_dt = (start_dt + lastatt)) %>%
  select(PID, start_dt, DTHRVWD, datedth, death_dt, lastatt, last_dt) %>%
  rename(death_indic = DTHRVWD, death_date = datedth)
dat_attend_offspring <- dat_attend_offspring %>% left_join(tmp)

# Adding exam numbers to years --------------------------------------------
d11 %>%
  select(PID, starts_with('examyr')) %>%
  gather(exam, exam_yr, starts_with('examyr')) %>%
  separate(exam, c('label', 'exam_no'), sep = 6, convert = T) %>%
  arrange(PID) %>%
  select(-label) -> bl
dat_attend_orig_exploded %<>% left_join(bl, by = c('PID' = 'PID', 'yrs' = 'exam_yr')) %>% tidyr::fill(exam_no)

d31 %>%
  select(PID, starts_with('examyr')) %>%
  gather(exam, exam_yr, starts_with('examyr')) %>%
  separate(exam, c('label', 'exam_no'), sep = 6, convert = T) %>%
  arrange(PID) %>%
  select(-label) -> bl
dat_attend_offspring_exploded %<>% left_join(bl, by = c('PID' = 'PID', 'yrs' = 'exam_yr')) %>% tidyr::fill(exam_no)

save(dat_attend_offspring, dat_attend_offspring_exploded, dat_attend_orig, dat_attend_orig_exploded, file = 'data/rda/TimeAndFractures.rda', compress=T)


# Some person-years computations --------------------------------------------------------------

dat_attend_orig <- dat_attend_orig %>%
  mutate(frac_dt = start_dt + fxdate,
         end_dt = pmin(frac_dt, last_dt, na.rm = T))

# Risk factors by exam ------------------------------------------------------------------------

## Diabetes

diab_orig <- read_sas(file.path(datadir, 'newdat','framcohort','Datasets','vr_diab_ex28_0_0601d.sas7bdat')) %>%
  select(PID, starts_with('BG140_curr')) %>%
  gather(variable, diab, -PID) %>%
  separate(variable, c('label','exam_no'), sep = 15) %>%
  select(-label) %>%
  arrange(PID) %>%
  mutate(exam_no = as.integer(exam_no),
         diab = as.integer(diab)) %>%
  group_by(PID) %>%
  tidyr::fill(diab) %>%
  ungroup()
diab_off <- read_sas(file.path(datadir, 'newdat','framoffspring', 'Datasets', 'ldia1_7.sas7bdat')) %>%
  set_names(toupper(names(.))) %>%
  select(PID, starts_with("DIAB")) %>%
  gather(variable, diab, -PID) %>%
  separate(variable, c('label','exam_no'), sep = 4) %>%
  mutate(exam_no = as.integer(exam_no),
         diab = as.integer(diab)) %>%
  select(-label) %>%
  group_by(PID) %>%
  tidyr::fill(diab) %>%
  ungroup()

## Menopause

meno_orig <- read_sas(file.path(datadir, 'newdat','framcohort','Datasets','vr_meno_ex14_0_0153d.sas7bdat')) %>%
  select(PID, AM2, AM5) %>%
  rename(age_meno = AM2, exam_meno = AM5) %>%
  mutate(early_meno = ifelse(age_meno < 45, 1, 0)) %>%
  mutate_all(as.integer) %>%
  # mutate(PID = as.character(PID)) %>%
  arrange(PID)

meno_off <- read_sas(file.path(datadir, 'newdat','framoffspring','Datasets','vr_meno_ex07_1_0152d.sas7bdat')) %>%
  select(PID, STOP_AGE, starts_with("MSTAT")) %>%
  mutate_at(vars(starts_with("MSTAT")), function(x) ifelse(x == 0, 0, 1)) %>%
  gather(variable, value, starts_with('MSTAT')) %>%
  group_by(PID) %>%
  filter( row_number() == detect_index(value, function(x) x == 1)) %>%
  separate(variable, c('label','exam_meno'), sep = 5) %>%
  select(-label, -value) %>%
  mutate(early_meno = ifelse(STOP_AGE < 45, 1, 0)) %>%
  rename(age_meno = STOP_AGE) %>%
  arrange(PID) %>%
  mutate_all(as.integer) %>%
  ungroup() %>%
  mutate(PID = as.character(PID))

## BMI

bmi_orig <- read_sas(file.path(datadir,'newdat','framcohort','Datasets','bmi.sas7bdat')) %>%
  set_names(toupper(names(.))) %>%
  select(PID, starts_with("BMI")) %>%
  gather(exam_no, bmi, -PID) %>%
  mutate(exam_no = as.integer(str_replace(exam_no,'BMI',''))) %>%
  arrange(PID)

### Not using BMI from offspring cohort since it is only recorded at exam 2

bmi_off <- read_sas(file.path(datadir,'newdat','framoffspring','Datasets','bmi_ex2.sas7bdat')) %>%
  set_names(toupper(names(.))) %>%
  select(-IDTYPE) %>%
  arrange(PID) %>%
  select(PID, everything()) %>%
  set_names(c('PID','exam_no','bmi'))

# Other exposures -----------------------------------------------------------------------------

## Smoking

source('munging_smoking.R')

## Drinking

source('munging_drinking.R')


# putting the datasets together ---------------------------------------------------------------

dat_orig <- dat_attend_orig_exploded %>%
  left_join(diab_orig) %>%
  nest(-PID, -sex) %>%
  left_join(meno_orig, by = "PID") %>%
  mutate(data = map2(data, age_meno,
                     function(x,y) { x$meno = ifelse(x$age_cur >= y, 1, 0); return(x)})) %>%
  select(PID, sex, data, early_meno) %>%
  unnest() %>%
  left_join(bmi_orig, by = c('PID' = 'PID', 'exam_no' = 'exam_no')) %>%
  left_join(smoke_orig, by = c('PID' = 'PID', 'exam_no' = 'exam')) %>%
  left_join(drink_origin, by = c('PID' = 'PID', 'exam_no' = 'exam'))

dat_offspring <- dat_attend_offspring_exploded %>%
  left_join(diab_off) %>%
  nest(-PID, -sex) %>%
  left_join(meno_off %>% mutate(PID = as.numeric(PID)), by = 'PID') %>%
  mutate(data = map2(data, age_meno,
                     function(x,y) { x$meno = ifelse(x$age_cur >= y, 1, 0); return(x)})) %>%
  select(PID, sex, data, early_meno) %>%
  unnest() %>%
  left_join(smoke_off, by = c('PID' = 'pid', 'exam_no' = 'exam')) %>%
  left_join(drink_offspring, by = c('PID' = 'pid', 'exam_no' = 'exam')) %>%
  left_join(bmi_off %>% select(-exam_no)) # added exam2 bmi to all exams

save(dat_orig, dat_offspring, file = 'data/rda/predictors.rda', compress = T)


