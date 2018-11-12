##%######################################################%##
#                                                          #
####       Annualized analysis of Framingham data       ####
#                                                          #
##%######################################################%##


# Setup ---------------------------------------------------------------------------------------

ProjTemplate::reload()
load('data/rda/TimeAndFractures.rda') # Data with fracture time, and death time
load('data/rda/predictors.rda') # Data with predictors


# Creating dataset for moving 5-year risk -----------------------------------------------------

dat_annual_orig <- dat_attend_orig %>% filter(!is.na(YrFrac)) %>%
  nest(-PID) %>%
  mutate(yrs_5yr_risk = map(data, ~seq(.$YrFrac-4, .$YrFrac))) %>%
  select(-data) %>%
  unnest() %>%
  mutate(risk_5yr = 1) %>%
  right_join(dat_orig,
             by = c('PID' = 'PID', 'yrs_5yr_risk' = 'yrs')) %>%
  mutate(risk_5yr = ifelse(is.na(risk_5yr), 0, 1)) %>%
  rename(yrs = yrs_5yr_risk)

dat_annual_orig %>% filter(age_cur >=50, age_cur < 60) %>%
  group_by(yrs) %>%
  summarize(N = n(),
            risk_5yr = mean(risk_5yr, na.rm=T)*1000,
            diab = ifelse(sum(!is.na(diab)) > 20, mean(diab, na.rm=T),NA),
            smoking = ifelse(sum(!is.na(smoking))>20, mean(smoking, na.rm=T),NA),
            drinking = ifelse(sum(!is.na(rf_etoh)) > 20, mean(rf_etoh, na.rm= T), NA)) %>%
  filter(N > 30) %>%
  select(-N) -> tmp
dygraph(tmp %>% select(yrs, risk_5yr),
        main = 'Age group 50-59', ylab = '5 year risk of fracture (per 1000)', group='frac')
dygraph(tmp %>% select(yrs, diab),  group = 'frac') %>%
  dyAxis('y', label = 'Prevalence of Diabetes',
         axisLabelFormatter = 'function(d){return Math.round(d*100) + "%"}')
dygraph(tmp %>% select(yrs, smoking), ylab = 'Prevalence of smoking', group = 'frac') %>%
  dyAxis('y', label = 'Prevalence of Smokimg',
         axisLabelFormatter = 'function(d){return Math.round(d*100) + "%"}')
dygraph(tmp %>% select(yrs, drinking), xlab='Year', group='frac') %>%
  dyAxis('y', label = 'Prevalence of Heavy Drinking',
         axisLabelFormatter = 'function(d){return Math.round(d*100) + "%"}')


# Competing risks analysis --------------------------------------------------------------------

stop_row <- function(d){
  x = d$cens
  i1 = which.max(x == 1)
  i2 = which.max(x == 2)
  ind <- case_when(
    all(x == 0) ~ nrow(d),
    i1 == 1 & i2 == 1 ~ min(i1, i2),
    i1 > 1 & i2 == 1 ~ i1,
    i1 == 1 & i2 > 1 ~ i2,
    i1 > 1 & i2 > 1 ~ min(i1, i2)
  )
  return(ind)
}

tmp <- dat_attend_orig %>% mutate(YrDead = year(death_dt)) %>%
  mutate(YrStop = pmin(YrFrac, YrDead, na.rm = T)) %>%
  mutate(cens = case_when(
    YrFrac == YrStop ~ 1, # Fracture is the event
    YrDead == YrStop ~ 2, # Death is the competing risk
    YrStop - year(last_dt) > 2 ~ 0, # Censor if event happens 2 years after last visit
    TRUE ~ 0)) %>%
  mutate(YrStop = YrStop - 1) %>% # We're doing event in next year, so we'll calibrate with covs from previous year
  select(PID, YrStop, cens) %>%
  right_join(dat_orig,
             by = c("PID" = "PID", 'YrStop' = 'yrs')) %>%
  mutate(cens = ifelse(is.na(cens), 0, cens)) %>%
  rename(yrs = YrStop) %>%
  mutate(start_dt = ymd(paste0(yrs, '-01-01')), end_dt = ymd(paste0(yrs, '-12-31'))) %>%
  group_by(PID) %>%
  mutate(start_day = as.numeric(start_dt - min(start_dt)),
         end_day = as.numeric(end_dt - min(start_dt))) %>%
  fill(diab, smoking, rf_etoh, early_meno, meno,  .direction = 'up') %>%
  ungroup() %>%
  select(-start_dt, -end_dt) %>%
  nest(-PID) %>% mutate(stoprow = map_int(data, stop_row)) %>%
  mutate(filtdata = map2(data, stoprow, ~.x[1:.y,])) %>%
  select(PID, filtdata) %>%
  unnest()
tmp <- tmp %>%
  mutate(Age = scale(age_cur, scale = F), Age2 = Age^2, Age3 = Age^3)

library(survival)
library(rms)
surv1 <- coxph(Surv(start_day, end_day, ifelse(cens == 1, 1, 0)) ~
                 rcs(age_cur) * (factor(sex) + diab + smoking + rf_etoh),
               data = tmp)
surv11 <- coxph(Surv(start_day, end_day, ifelse(cens == 1, 1, 0)) ~
                  age_cur * (factor(sex) + diab + smoking + rf_etoh),
                data = tmp)

anova(surv1)
surv2 <- coxph(Surv(start_day, end_day, ifelse(cens == 1, 1, 0)) ~
                 age_cur * (early_meno + diab + smoking + rf_etoh),
               data = tmp %>% filter(sex == 2))

