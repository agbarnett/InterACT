# 99_trial_information.R
# basic trial information
trial_dates = read.xlsx('study timings.xlsx',detectDates = T) %>% clean_names()
trial_start = '2020-05-25'
trial_end = '2021-06-30'
trial_sites = c("The Prince Charles Hospital","Royal Brisbane & Women'S Hospital","Gold Coast University Hospital")
comparator_sites = c("Sunshine Coast University Hospital","Princess Alexandra Hospital","Logan Hospital")

study_weeks = filter(trial_dates,study_period %in% c('Usual care','Intervention exposure')) %>% mutate(study_weeks = ceiling(as.numeric(end_date-start_date,'weeks'))) %>%
  select(site_name,study_period,study_weeks) %>% spread(study_period,study_weeks) %>%
  clean_names()
