# Matching participation in study with risk factor measurments

ProjTemplate::reload()
`%nin%` <- Negate(`%in%`)
load(file.path(datadir,'rda','munged_data_Oct17.rda'))
load(file.path(datadir,'rda','event_data.rda'))

for(id in unique(original_datum$PID)){
  for(yr in unique(original_datum$year)){
    if(yr %nin% dat_attend_exploded$yrs_in_study[dat_attend_exploded$PID == id]){
      original_datum[original_datum$PID == id & original_datum$year == yr,
                     3:ncol(original_datum)] <- NA
    }
  }
}


original_datum %>%
  select(year, beta:bmi, diab:steroid) %>%
  group_by(year) %>%
  summarize_all(mean, na.rm=T)-> risk_summaries
tmp <- dat_attend_timedep %>%
  mutate(py = Stop - Start) %>% # Quick person-year calculation
  group_by(decade_in_study) %>%
  summarise(events = sum(Status), py= sum(py)) %>%
  mutate(rate = events/py) %>%
  select(-events, -py) %>%
  filter(decade_in_study %in% c('1980-1989','1990-1999','2000-2009')) %>%
  mutate(year = as.character(c(1980,1990, 2000)))-> incidence_summary

incidence_summary %>% left_join(risk_summaries) %>%
  select(-decade_in_study, -bmi) %>%
  mutate(year = as.numeric(year)) %>%
  gather(variable, value, -year) %>%
  ggplot(aes(year, value, color=variable))+
    geom_line()
