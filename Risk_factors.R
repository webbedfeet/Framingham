#' ---
#' title: Summary results from Framingham ecological analysis
#' author: Abhijit Dasgupta
#' ---
#'
#+ preamble, echo = FALSE
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 6,
                      fig.height = 4)

#' ## Introduction
#'
#' We are looking at ecological relationships between fracture rates and several predictors based
#' on the Framingham Heart Study, for the period 1980-2010. The current analysis uses only data from
#' the original cohort. The predictors we use are
#'
#' 1. Prevalence of diabetes
#' 2. Prevalence of estrogen supplement use among women
#' 3. Proportion of women who are post-menopausal
#' 4. Prevalence of smoking
#' 5. Prevalence of heavy drinking
#'
#' These prevalences are computed at 1980, 1990, and 2000, and compared to the incidence rate of
#' fractures in the following decade, not stratified by age groups or gender
#' (since the prevalence data supplied was not stratified in any way).
#'
#' ## Analysis
#' Fracture rates were computed using a person-year analysis. Predictor prevalences were computed
#' based on exam dates nearest the sentinel years of 1980, 1990 and 2000. The final analytic data is given
#' in Table 1, below. The fracture rate is computed as number of fractures per person-year of exposure
#' during the decade, and the predictor values are prevalence rates at the sentinel years.
#'
#+ tab1, results = 'asis'
ProjTemplate::reload()
load('data/rda/munged_data_Oct17.rda')

## Binary variable summaries by year/decade

binary_summaries_orig <- original_datum %>% select(year,beta,  diab:smoke) %>%
  group_by(year) %>%
  summarise_all(funs(100*mean(., na.rm = T)))

binary_summaries_off <- offspring_datum %>% select(year, beta, bmi,diab:smoke, -premarin) %>%
  group_by(year) %>%
  summarise_all(funs(100*mean(., na.rm = T)))

load(file.path(datadir,'rda','event_data.rda'))
# dat_attend_timedep is based on original database, not offspring
tmp <- dat_attend_timedep %>% mutate(py = Stop - Start) # Quick person-year calculation
# tmp %>% group_by(decade_in_study, current_agegrp) %>% # By each unique decade-age combination....
#   summarise(events = sum(Status), py = sum(py)) %>%   # Find numerator and denominator data...
#   mutate(rate = events/py) %>%                        # Compute the incidence rate
#   select(-events, -py) %>%
#   filter(!(current_agegrp %in% c('20-29','30-39','40-49','100-109')),
#          rate > 0) %>%
#   # ggplot(aes(x = decade_in_study, y = rate, group=current_agegrp, color=current_agegrp))+geom_line()+
#   # theme(axis.text.x = element_text(angle=45, hjust=1))+
#   # labs(x = 'Decade', y = 'Incidence rate', color='Age group') # Draw graph
#   spread(current_agegrp, rate) %>% knitr::kable(digits=5) # Create table

analytic_data_orig <- tmp %>% group_by(decade_in_study) %>%
  summarise(events = sum(Status), py = sum(py)) %>%
  mutate(rate = events/py) %>%
  select(-events, -py) %>%
  filter(decade_in_study %in% c('1980-1989','1990-1999','2000-2009')) %>%
  mutate(year = str_extract(decade_in_study, '^\\d{4}')) %>%
  left_join(binary_summaries_orig) %>% ungroup() %>%
  rename(Diabetes = diab, Estrogen = estrogen, Menopause = menopause,
         `Heavy drinking` = rf.etof, Smoking = smoke, BetaBlockers = beta)

knitr::kable(analytic_data_orig,digits = 3,
             col.names = c('Decade','Fracture Rate','Year','Beta Blockers','Diabetes','Estrogen','Menopause','Heavy drinking','Smoking'),
             caption = "**Table 1:** Summary table of fracture rates and predictor prevalences")
#ggplot(analytic_data_orig, aes(year, 100*rate, group=1))+geom_point()+geom_line(stat='identity')

#' We first look at the changes in prevalences compared to the changes in incidence rates over these
#' three decades in the following graph. The incidence rate is given as number of events per 1000
#' years, to allow the plot to be scaled appropriately, and is shown as the wide black line in the graph.
#'
#+ fig1, fig.align = 'center', fig.cap = '**Figure 1:** Changes in rates over 3 decades'

dat_for_plot <- analytic_data_orig %>% select(-decade_in_study) %>% mutate(rate  = 1000*rate) %>%
  gather(variable, value, -year)
ggplot() +
  geom_line(data = filter(dat_for_plot, variable != 'rate'), aes(x = year, y = value, group = variable,
                                                                 color = variable)) +
  geom_line(data = filter(dat_for_plot, variable == 'rate'), aes(x = year, y = value, group = variable),
            size = 2, color='black') +
  xlab('Year') + ylab('') + scale_color_discrete('Predictor')

#' We see from this figure that there appears to be strong positive correlation between fracture rate
#' and  diabetes prevalence and estrogen use prevalence, and strong negative correlation with smoking
#' prevalence.
#'
#' Figure 2 shows the relationship between the rate of fractures (x-axis) with each of the predictor
#' prevalences. We see that diabetes and estrogen use have almost linear association with the fracture rate
#+ fig2, fig.align='center', fig.cap = '**Figure 2:** Association between fracture rate and predictor prevalences'
analytic_data_orig %>% select(rate, BetaBlockers:Smoking) %>%
  mutate(rate = 1000*rate) %>%
  gather(variable, value, -rate) %>%
  ggplot(aes(x = rate, y = value, group=variable, color=variable))+geom_line() +
  xlab('Fracture rate (per 1000 person-years)')+ ylab('')

#' If we now wish to see what proportion of the variabilily of the fracture rate might be
#' explained by each predictor prevalence rate, we run linear regression of the fracture rate against
#' each predictor and compute the R^2^ value, which gives this proportion. This is shown in Table 2, below
#'
#+ tab2, results = 'asis'
analytic_data_orig %>% select(rate, BetaBlockers:Smoking) %>% mutate(rate = 1000*rate) %>%
  gather(variable, value, -rate) %>%
  nest(-variable) %>%
  mutate(mods = map(data, ~lm(rate ~ value, data = .))) %>%
  mutate(rsq = map_dbl(mods, ~summary(.)$r.squared)) %>%
  select(variable, rsq) %>%
  # mutate(variable = c('Diabetes','Estrogen','Menopause','Heavy Drinking','Smoking')) %>%
  knitr::kable( col.names = c('Variable', 'R^2^'), digits = 3, format='pandoc',
               caption = '**Table 2:** Proportion of variability in fracture rates attributable to each predictor')

#' ## Limitations
#' We are limited in this analysis by having only 3 data points for each variable, which in turn
#' limits the power of the regression analyses done here. We are also using the proportion of variance
#' explained in the classical sense derived from analysis of variance, which implies a linear regression;
#' what proportion of the variability in the outcome is explained by a _linear_ relationship with another
#' predictor. There are other measures that can measure the proportion of variance explained by a
#' _monotonic_ relationship (increasing or decreasing) rather than an linear relationship, but I
#' did not go there given the sample size limitations of the summary data.
#'
#' The data in table 2 is also based on separate univariate linear regressions rather than a
#' multivariate linear regression, since in a multiple linear regression the number of predictors must
#' be strictly less than the number of observations (here, we have 3)
#'
#' ## Suggestions for proper analysis of this data
#'
#' The primary interest in this data is to see how changes in the prevalence of different predictors affect the risk
#' of fractures over time. First and foremost, this needs to be adjusted by age and gender, since
#' the marginal risk of fracture increases with age, given a particular cohort. One approach to this
#' may be derivative of methods employed in [age-period-cohort analysis](https://www.mailman.columbia.edu/research/population-health-methods/age-period-cohort-analysis), albeit within a single cohort (original or offspring cohorts).
#'
#' A proper reckoning of the effects of the predictors on fracture rate would probably require a
#' survival analysis (time-to-fracture) based on baseline age and various interaction terms of each
#' predictor with time (or year, or exam). This would leverage all the data available, though we
#' may have to deal with differential missingness of certain predictors over time, depending on how
#' the exam protocols changed.
