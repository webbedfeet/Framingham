ProjTemplate::reload()

# d1 <- read_csv('vr_dates_2014_a_0912d_yr_fram.csv')[,-1] # Cohort data
# d2 <- read_csv('vr_fxrev_2012_0_0746d_yr_fram.csv')[,-1] # Hip fracture data

d11 <- read_sas(file.path(datadir,'sas','vr_dates_2014_a_0912d_yr_fram.sas7bdat'))
d21 <- read_sas(file.path(datadir, 'sas','vr_fxrev_2012_0_0746d_yr_fram.sas7bdat'))
d31 <- read_sas(file.path(datadir,'sas','vr_dates_2014_a_0912d_yr_offspring.sas7bdat'))
d41 <- read_sas(file.path(datadir,'sas','vr_fxrev_2012_1_0747d_yr_offspring.sas7bdat'))

first_fracture <- d21 %>%
group_by(PID) %>%
filter(fxdate == min(fxdate)) %>%
ungroup() %>%
  select(PID, fxdate, YrFrac) %>%
  distinct()


# nrow(first_fracture)==length(unique(first_fracture$PID)) ## Check


dat_attend <- d11 %>%
          select(PID, age1, sex, starts_with('examyr')) %>% # Work with calendar time
          gather(visit, yr, -PID, -age1, -sex)  %>%
          separate(visit, c('label','visit_no'), sep=6, convert=T) %>% # Extracts exam number
          select(-label) %>%
          filter(!is.na(yr)) %>% # The ones missing are the ones that weren't seen at that exam
          nest(visit_no,yr) %>%  # Stratify on exam, calendar year
          mutate(start_yr = map_dbl(data, ~min(.$yr)),
                 end_yr = map_dbl(data, ~max(.$yr))) %>%  # Identify period that subject is in study
          select(-data) %>%
          left_join(first_fracture) %>%
          mutate(end_duration = ifelse(is.na(YrFrac), end_yr, YrFrac), # Stop time at first fracture
              frac_indic = ifelse(is.na(YrFrac), 0, 1),
              year1 = start_yr)


# sum(dat_attend$frac_indic)==nrow(first_fracture) ## Check
# length(unique(dat_attend$PID))==nrow(dat_attend) ## Check


explode_func <- function(d){
 # This function takes a single record with start and stop times and explodes it into
 # a dataset with 1 record per year in the study
out <- tibble(
   yrs_in_study = as.integer(seq(d$start_yr, d$end_duration, by=1)),
   ages_in_study = as.integer(seq(d$age1, d$age1+(d$end_duration-d$start_yr), by=1)),
   status = 0,
   age1=d$age1,
   year1 = d$year1)
  out$status[nrow(out)] <- ifelse(d$frac_indic==1,1,0)
  return(out)
  }


dat_attend_exploded <- dat_attend %>%
 nest(-PID) %>%
 mutate(newdat = map(data, ~explode_func(.))) %>% # Explode for each subject and put back together
 select(-data) %>%
 unnest()




# Grouping by decades -----------------------------------------------------


x <- seq(1940, 2010, by=10)
decade_labels = paste0(x,'-',x+9)


dat_attend_timedep <- dat_attend_exploded %>%
  mutate(status = as.integer(status),
              decade_in_study = cut(yrs_in_study,seq(1940, 2020, by=10),
                                 include.lowest=T, label = decade_labels,
                                 right=F),
              current_agegrp = cut(ages_in_study, seq(20,110,by=10),
                                   label = paste0(seq(20,100,by=10),'-',seq(29,109,by=10)),
                                   include.lowest=T, right=F)) %>% # Create categorical calendar time and age
 select(-ages_in_study) %>%
 nest(yrs_in_study, status) %>%
 mutate(Start = map_int(data, ~min(.$yrs_in_study))-year1,
              Stop = map_int(data, ~max(.$yrs_in_study))-year1+1,
             Status = map_int(data, ~sum(.$status))) %>% # Create (Start, Stop, Status) data
  select(-data)


sum(dat_attend_timedep$Status) == nrow(first_fracture)  ## Check
length(unique(dat_attend_timedep$PID)) == 5079 ## Check


## Compute rates
options(knitr.kable.NA='') # Suppresses printing NA


tmp <- dat_attend_timedep %>% mutate(py = Stop - Start) # Quick person-year calculation
tmp %>% group_by(decade_in_study, current_agegrp) %>% # By each unique decade-age combination....
  summarise(events = sum(Status), py = sum(py)) %>%   # Find numerator and denominator data...
  mutate(rate = events/py) %>%                        # Compute the incidence rate
  select(-events, -py) %>%
  filter(!(current_agegrp %in% c('20-29','30-39','40-49','100-109')),
              rate > 0) %>%
# ggplot(aes(x = decade_in_study, y = rate, group=current_agegrp, color=current_agegrp))+geom_line()+
# theme(axis.text.x = element_text(angle=45, hjust=1))+
# labs(x = 'Decade', y = 'Incidence rate', color='Age group') # Draw graph
spread(current_agegrp, rate) %>% knitr::kable(digits=5) # Create table

tmp %>% group_by(decade_in_study) %>%
  summarise(events = sum(Status), py = sum(py)) %>%
  mutate(rate = events/py) %>%
  select(-events, -py) %>%
  filter(decade_in_study %in% c('1980-1989','1990-1999','2000-2009'))
save(dat_attend, dat_attend_exploded, dat_attend_timedep,
     file = file.path(datadir,'rda','event_data.rda'), compress=T)


# Using original epochs ---------------------------------------------------


x <- c(1948,1977,1986,1992, 2004,2011)
epochs = paste0(x[-6],'-',x[-1]-1)
dat_by_epoch <- dat_attend_exploded %>%
  mutate(status = as.integer(status),
        current_agegrp = cut(ages_in_study, seq(20,110,by=10),
                            label = paste0(seq(20,100,by=10),'-',seq(29,109,by=10)),
                            include.lowest=T, right=F),
              epochs = cut(yrs_in_study, x,
                          label = epochs, include.lowest=T, right=F)) %>% # Categorize age and calendar time
 nest(yrs_in_study, ages_in_study, status) %>%
 mutate(Start = map_int(data, ~min(.$yrs_in_study))-year1,
               Stop = map_int(data, ~max(.$yrs_in_study))-year1+1,
               Status = map_int(data, ~sum(.$status))) %>% #Create (Start, Stop, Status) data
  select(-data)


out <- dat_by_epoch %>% mutate(py = Stop - Start) %>% # Calculate person-years
  group_by(epochs, current_agegrp) %>% # For each unique epoch-age combination....
  summarise(events = sum(Status), py = sum(py), rate = events/py)  # Compute numerator, denominator, rate


out  %>% select(-events, -py) %>%
  filter(!(current_agegrp %in% c('20-29','30-39','40-49','100-109')),
            rate > 0) %>%
# spread(current_agegrp, rate) %>% knitr::kable(digits=5)
ggplot(aes(x=epochs, y = rate, group=current_agegrp, color=current_agegrp))+geom_line()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = 'Epochs', y = 'Incidence rate', color='Age groups')
