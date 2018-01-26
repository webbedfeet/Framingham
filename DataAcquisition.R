
# Load updated munged data from October -----------------------------------

ProjTemplate::reload()
datadir <- set_datadir()

## Original cohort
xlfile <- file.path(datadir, 'munged','original.munging_done_10-9-17.xlsx')

bl2 <- tidyxl::xlsx_cells(xlfile)

headers <- bl2 %>% filter(col != 1) %>%
  filter(row %in% c(1,2)) %>%
  mutate(character = ifelse(is.na(character), as.character(numeric), character)) %>%
  select(row, col, character) %>%
  spread(row, character) %>%
  unite(head, `2`,`1`, sep='_')

bl2 %>% filter(row > 3, col > 1) %>% count(data_type)

original_datum <- bl2 %>%
  filter(row > 3, col > 1) %>%
  select(row, col, numeric) %>% # Checked that all the data is numeric
  spread(col, numeric) %>%
  select(-row) %>%
  set_names(pull(headers, head)) %>%
  rename(PID = NA_PID) %>%
  gather(variable, value, -PID) %>%
  mutate(variable = stringr::str_replace(variable, 'rf_etof','rf.etof')) %>%
  separate(variable, c('variable','year'), sep = '_') %>%
  spread(variable, value)


## Offspring cohort

xlfile <- file.path(datadir,'munged','offspring_munging_done_10-9-17.xlsx')

bl2 <- tidyxl::xlsx_cells(xlfile)

headers <- bl2 %>% filter(col!=1) %>%
  filter(row %in% c(1,2)) %>%
  mutate(character = ifelse(is.na(character), as.character(numeric), character)) %>%
  select(row, col, character) %>%
  spread(row, character) %>%
  unite(head, `2`,`1`,sep='_')

bl2 %>% filter(row>3, col>1) %>% count(data_type)

offspring_datum <- bl2 %>%
  filter(row > 3, col > 1) %>%
  select(row, col, numeric) %>% # Checked that all the data is numeric
  spread(col, numeric) %>%
  select(-row) %>%
  set_names(pull(headers, head)) %>%
  rename(PID = NA_PID) %>%
  gather(variable, value, -PID) %>%
  mutate(variable = stringr::str_replace(variable, 'rf_etof','rf.etof')) %>%
  separate(variable, c('variable','year'), sep='_') %>%
  spread(variable, value)

save(original_datum, offspring_datum, file = 'data/rda/munged_data_Oct17.rda', compress = T)
