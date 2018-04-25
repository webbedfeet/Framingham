ProjTemplate::reload()
load('data/rda/updatedEventsPY.rda')

dat_years <- bind_rows(list('original' = dat_years_orig, 'offspring' = dat_years_off),
                       .id = 'cohort')

dat_years %>%
  mutate(age_decades = as.factor(paste0((age %/% 10) * 10,'s')),
         decades = (years %/% 10) * 10) %>%
  mutate(age_decades = forcats::fct_reorder(age_decades, age)) %>%
  filter(age > 20) %>%
  group_by(age_decades, decades) %>%
  summarize(rate = sum(fx_status)/sum(pyears), pyears = sum(pyears)) %>%
  ungroup() %>%
  ggplot(aes(x = decades, y = rate))+geom_point(aes(size = pyears))+
  facet_wrap(~ age_decades) +
  theme(legend.position = 'bottom')+
  labs(x = 'Decades', y = 'Incidence rate of fractures', size = 'Person-years exposed')
