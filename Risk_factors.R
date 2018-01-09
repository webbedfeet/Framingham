ProjTemplate::reload()
load('data/rda/munged_data_Oct17.rda')

## Binary variable summaries by year/decade


binary_summaries_orig <- original_datum %>% select(year, diab:smoke) %>%
  group_by(year) %>%
  summarise_all(funs(100*mean(., na.rm = T)))

binary_summaries_off <- offspring_datum %>% select(year, diab:smoke, -premarin) %>%
  group_by(year) %>%
  summarise_all(funs(100*mean(., na.rm = T)))
