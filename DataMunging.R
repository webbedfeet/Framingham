## %######################################################%##
#                                                          #
####                    Data munging                    ####
#                                                          #
## %######################################################%##

ProjTemplate::reload()
`%nin%` <- Negate(`%in%`)

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
  )

dat_attend_orig_exploded <-
  dat_attend_orig[rep(seq_len(nrow(dat_attend_orig)), dat_attend_orig$no_yrs), ] %>%
  select(-no_yrs) %>%
  nest(age1, start_yr:year1) %>%
  mutate(data = map(data, ~ .x %>%
    mutate(
      yrs = unique(start_yr) + seq_len(nrow(.x)) - 1,
      frac_indic = ifelse(!is.na(YrFrac) & YrFrac == yrs, 1, 0),
      age_cur = unique(age1) + seq_len(nrow(.x)) - 1
    ))) %>%
  unnest() %>%
  select(PID:sex, yrs, frac_indic, age_cur)

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
  )

dat_attend_offspring_exploded <- dat_attend_offspring[rep(seq_len(nrow(dat_attend_offspring)), dat_attend_offspring$no_yrs), ] %>%
  select(-no_yrs) %>%
  nest(age1, start_yr:end_duration) %>%
  mutate(data = map(data, ~ .x %>%
    mutate(
      yrs = unique(start_yr) + seq_len(nrow(.x)) - 1,
      frac_indic = ifelse(!is.na(YrFrac) & YrFrac == yrs, 1, 0),
      age_cur = unique(age1) + seq_len(nrow(.x)) - 1
    ))) %>%
  unnest() %>%
  select(PID:sex, yrs, frac_indic, age_cur)


# Death -------------------------------------------------------------------

death_orig <- read_sas(file.path(datadir, 'newdat','framcohort','Datasets','vr_survdth_2011_m_0786d.sas7bdat'))
death_off <-  read_sas(file.path(datadir, 'newdat','framoffspring','Datasets','vr_survdth_2011_m_0786d.sas7bdat')) %>%
  filter(IDTYPE == 1)

tmp <- dat_attend_orig %>%
  select(PID, start_yr, end_yr) %>%
  mutate(start_dt = as.Date(paste0(as.character(start_yr),'-07-01'))) %>%
  left_join(death_orig) %>%
  mutate(death_dt = (start_dt + datedth)) %>%
  select(PID, start_dt, DTHRVWD, datedth, death_dt) %>%
  rename(death_indic = DTHRVWD, death_date = datedth)
dat_attend_orig <- dat_attend_orig %>% left_join(tmp)


tmp <- dat_attend_offspring %>%
  select(PID, start_yr, end_yr) %>%
  mutate(start_dt = as.Date(paste0(as.character(start_yr),'-07-01'))) %>%
  left_join(death_off) %>%
  mutate(death_dt = (start_dt + datedth)) %>%
  select(PID, start_dt, DTHRVWD, datedth, death_dt) %>%
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

# Risk factors by exam ------------------------------------------------------------------------

## Diabetes

diab_orig <- read_sas(file.path(datadir, 'newdat','framcohort','Datasets','vr_diab_ex28_0_0601d.sas7bdat'))
diab_off <- read_sas()
