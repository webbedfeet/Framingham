ptime <- function(d, first_frac) {
  d %>% select(PID, starts_with('date')) %>%
    gather(exam, days, -PID) %>%
    mutate(exam = as.numeric(str_remove(exam, 'date'))) %>%
    right_join(
      d %>% select(PID, starts_with('exam')) %>%
        gather(exam, year, -PID) %>%
        mutate(exam = as.numeric(str_remove(exam, 'examyr')))
    ) %>%
    arrange(PID, exam) %>%
    mutate(days = ifelse(exam==1, 0, days)) %>%
    filter(!is.na(days)) %>%
    group_by(PID) %>%
    mutate(computed_dt = as.Date(paste0(as.character(min(year)),'-01-01')) + days) %>%
    mutate(computed_yr = lubridate::year(computed_dt)) %>%
    ungroup() -> exam_time

  bl <- exam_time %>% group_by(PID) %>% summarize(start_yr = min(year(computed_dt)),
                                                  end_yr = max(year(computed_dt)),
                                                  no_yrs = length(seq(start_yr, end_yr)),
                                                  max_days = max(days),
                                                  end_dt = max(computed_dt)) %>%
    ungroup() %>%
    left_join(first_frac) %>%
    mutate(fx_dt = as.Date(paste0(as.character(start_yr),'-01-01')) + fxdate) %>%
    mutate(tmp_dt = end_dt %m+% years(2)) %>% # Accomodates an impossible leap day
    mutate(last_dt = pmin(tmp_dt, fx_dt, na.rm = T)) %>%
    mutate(last_yr = year(last_dt),
           dec_yr = decimal_date(last_dt)) %>%
    select(-fx_dt, -tmp_dt, -YrFrac)
  bl %>% select(PID, dec_yr, start_yr, last_yr)  %>%
    mutate(years = map2(start_yr, last_yr, ~seq(.x, .y))) %>%
    select(PID, dec_yr, years) %>%
    unnest() %>%
    group_by(PID) %>%
    mutate(pyears = 1) %>%
    mutate(pyears = ifelse(years == max(years), dec_yr - trunc(dec_yr), pyears)) %>%
    ungroup() %>%
    select(-dec_yr) -> pyears
  return(pyears)
}
