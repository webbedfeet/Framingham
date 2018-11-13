#'---
#'title: Incidence summaries for Framingham cohorts
#'author: Abhijit Dasgupta
#'date: "`r Sys.Date()`"
#'output:
#'  html_document:
#'    toc: true
#'---
#'
#+ preamble, include=F
ProjTemplate::reload()
library(glue)
knitr::opts_chunk$set(cache = T, echo = F, fig.width=11)
load('data/rda/predictors.rda')

#' # Crude incidence rates
#'
#' We need to compute the person-years at risk in each 5 year period, and see if the incidence rates
#' of fractures declined over time. This also has to be done on an age-adjusted basis, since we know
#' that age is the biggest risk factor for fractures.
#'
#' The timeline for each person has to end at either time of fracture or two years after last exam,
#' whichever is earlier. This then determines the person-years at risk within each 5-year interval
#' and age group.
#'
#+ echo = F
py_orig <- dat_orig %>%
  group_by(yr_grp, age_grp) %>%
  summarize(py = n(), events = sum(frac_ind)) %>%
  mutate(inc_rate = 1000*events/py) %>%
  ungroup() %>%
  filter(!is.na(yr_grp), !is.na(age_grp))

py_off <- dat_offspring %>% group_by(yr_grp, age_grp) %>%
  summarize(py = n(), events = sum(frac_ind)) %>%
  mutate(inc_rate = 1000* events/py) %>%
  ungroup() %>%
  filter(!is.na(yr_grp), !is.na(age_grp))

dat_overall <- rbind(dat_orig, dat_offspring)

py_overall <- dat_overall %>%
  group_by(age_grp, yr_grp) %>%
  summarize(py = n(), events=sum(frac_ind)) %>%
  mutate(inc_rate = 1000*events/py) %>%
  ungroup() %>%
  filter(!is.na(yr_grp), !is.na(age_grp))

# py_overall %>%
#   filter(py > 100 & !is.na(age_grp) & !is.na(yr_grp)) %>%
#   filter(age_grp != '(0,40]') %>%
#   ggplot(aes(x = yr_grp, y = inc_rate))+geom_point() +
#   facet_wrap(~age_grp, scales = 'free_y') +
#   theme(axis.text.x = element_text(angle=45, hjust=1))

fix_grps <- function(x){
  str_extract_all(as.character(x), '\\d+') %>%
    map(as.numeric) %>%
    map(function(y) as.character(glue('{y[1]+1}-{y[2]}'))) %>%
    unlist()
}

#+ echo = F, fig.height = 4, fig.width = 11
py_orig %>% filter(age_grp != '(0,40]', py > 100, !is.na(yr_grp), !is.na(age_grp)) %>%
  mutate(yr_grp = fct_relabel(yr_grp, fix_grps),
         age_grp = fct_relabel(age_grp, fix_grps)) %>%
ggplot(aes(x = yr_grp, y = inc_rate)) + geom_point() +
  stat_summary(fun.y = 'mean', geom = 'line', mapping = aes(group = age_grp))+
  facet_wrap(~age_grp, scales = 'free_y', nrow=1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = 'Year', y = 'Incidence rate (per 1000 py)') +
  ggtitle('Original cohort')

py_off %>% filter(age_grp != '(0,40]', py > 100, !is.na(yr_grp), !is.na(age_grp)) %>%
  mutate(yr_grp = fct_relabel(yr_grp, fix_grps),
         age_grp = fct_relabel(age_grp, fix_grps)) %>%
  ggplot(aes(x = yr_grp, y = inc_rate)) + geom_point() +
  stat_summary(fun.y = 'mean', geom = 'line', mapping = aes(group = age_grp))+
  facet_wrap(~age_grp, scales = 'free_y', nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = 'Year', y = 'Incidence rate (per 1000 py)')+
  ggtitle('Offspring cohort')

#+ echo = F, fig.height = 4, fig.width=11
py_overall %>% filter(age_grp != '(0,40]', py > 100, !is.na(yr_grp), !is.na(age_grp)) %>%
  mutate(yr_grp = fct_relabel(yr_grp, fix_grps),
         age_grp = fct_relabel(age_grp, fix_grps)) %>%
  ggplot(aes(x = yr_grp, y = inc_rate)) + geom_point() +
  stat_summary(fun.y = 'mean', geom = 'line', mapping = aes(group = age_grp))+
  facet_wrap(~age_grp, scales = 'free_y', nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Year', y = 'Incidence rate (per 1000 py)')+
  ggtitle('Combined cohorts')

#'
#' # Ecological patterns of comorbidities
#'
#+ include=F
comorb_orig <- dat_orig %>% group_by(age_grp, yr_grp) %>%
  summarize_at(vars(diab:rf_etoh), mean, na.rm=T) %>%
  filter(age_grp != '(0,40]', !is.na(yr_grp), !is.na(age_grp)) %>%
  left_join(dat_orig %>% count(age_grp, yr_grp)) %>% ungroup()
comorb_off <- dat_offspring %>% group_by(age_grp, yr_grp) %>%
  summarize_at(vars(diab:bmi), mean, na.rm=T) %>%
  filter(age_grp != '(0,40]', !is.na(yr_grp), !is.na(age_grp)) %>%
  left_join(dat_offspring %>% count(age_grp, yr_grp)) %>% ungroup()

comorb_combined <- list('original' = comorb_orig, 'offspring' = comorb_off) %>%
  bind_rows(.id = 'cohort') %>%
  mutate(cohort = fct_relevel(cohort, 'original'),
         yr_grp = factor(yr_grp)) %>%
  mutate_at(vars(contains('grp')), fix_grps)

#' ## Diabetes
#+ echo = F
ggplot(comorb_combined, aes(yr_grp, diab*100))+geom_point()+facet_grid(cohort ~ age_grp)+
  labs(y = 'Diabetes', x = 'Year')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' ## Heavy Drinking
#+ echo = F
ggplot(comorb_combined, aes(yr_grp, rf_etoh*100))+geom_point()+facet_grid(cohort ~ age_grp)+
  labs(y = 'Heavy Drinking', x = 'Year')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' ## Smoking
#+ echo = F
ggplot(comorb_combined, aes(yr_grp, smoking*100))+geom_point()+facet_grid(cohort ~ age_grp)+
labs(y = 'Smoking', x = 'Year')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' ## BMI
#+ echo = F
ggplot(comorb_combined, aes(yr_grp, bmi))+geom_point()+facet_grid(cohort ~ age_grp)+
  labs(y = 'BMI', x = 'Year')+
  geom_hline(yintercept = c(25,30), color='red',linetype=2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' __Caveat__: BMI from offspring cohort is based on one recorded BMI at exam 2.
#'
#' # Some conclusions
#'
#' It appears that the decreasing rate of fractures over time, adjusted for age, only appears in the
#' offspring cohort. These estimates may be unstable, since there are only 66 fractures reported in
#' this cohort.
#'
#' In fact, it is interesting to note that the offspring cohort has much lower rates of fractures than
#' original cohort members at similar ages, by around 50 percent, for the same age groups and same years,
#' at least qualitatively, in the later years.
#'
#' Factors that may be worth exploring are the relationships with BMI, and to some exten, smoking and diabetes,
#'  which qualitatively
#' also have similar ecological trends as fracture incidence.
#'
