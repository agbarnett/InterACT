# 0_checks.R
# checks of data processing
# Sep 2020
library(dplyr)
load('data/FullData.RData') # from 0_read_data.R

## Random checks for Chris to compare processed data with REDCap data
# a) care directive
r1 = filter(care_directive, redcap_version==3, change_var=='Yes') %>%
  sample_n(3)
r2 = filter(care_directive, redcap_version==3, change_var=='No') %>%
  sample_n(3)
r3 = filter(care_directive, redcap_version==3, change_var=='Prior') %>%
  sample_n(3)
d_check = bind_rows(r1, r2, r3) %>%
  select(participant_id, form_date, change_var, outcome_date, time_change) %>%
  mutate(form = 'Care directive')
# b) clinician led review
r1 = filter(clinicianled_review, redcap_version==3, change_var=='Yes') %>%
  sample_n(3)
r2 = filter(clinicianled_review, redcap_version==3, change_var=='No') %>%
  sample_n(3)
r3 = filter(clinicianled_review, redcap_version==3, change_var=='Prior') %>%
  sample_n(3)
c_check = bind_rows(r1, r2, r3) %>%
  select(participant_id, form_date, change_var, outcome_date, time_change) %>%
  mutate(form = 'Clinician led review')
# c) pall care
r1 = filter(palliative_care_referral, redcap_version==3, change_var=='Yes') %>%
  sample_n(3)
r2 = filter(palliative_care_referral, redcap_version==3, change_var=='No') %>%
  sample_n(3)
r3 = filter(palliative_care_referral, redcap_version==3, change_var=='Prior') %>%
  sample_n(3)
p_check = bind_rows(r1, r2, r3) %>%
  select(participant_id, form_date, change_var, outcome_date, time_change) %>%
  mutate(form = 'Palliative care')
# concatenate
all_check = bind_rows(d_check, c_check, p_check) %>%
  select(form, participant_id, form_date, outcome_date, time_change, change_var) %>%
  arrange(form, change_var, participant_id)
write.csv(all_check, file='check_outcomes.csv', quote=FALSE, row.names = FALSE)


### No longer working from here: to fix when I get time ###

problems = function(){
  
  ## # quick checks of times - could just be data issue (longer than 120 minutes or negative time)
  problems = bind_rows(baseline, care_directive) %>% # use all forms with times
    select("participant_id",'hospital', starts_with('time_')) %>%
    tidyr::gather(value='minutes', key='form', -"participant_id", -"hospital") %>%
    filter(minutes<0 | minutes>120) %>%
    left_join(baseline, by=c("participant_id",'hospital')) %>%
    mutate(start = NA,
           start = ifelse(minutes == time_hosp, start_date_time_hosp, start),
           start = ifelse(minutes == time_dem, start_date_time_dem, start),
           start = ifelse(minutes == time_funct, start_date_time_funct, start),
           end = NA,
           end = ifelse(minutes == time_hosp, end_date_time_hosp, end),
           end = ifelse(minutes == time_dem, end_date_time_dem, end),
           end = ifelse(minutes == time_funct, end_date_time_funct, end)) %>%
    select("participant_id",'hospital', 'form', 'minutes', 'start', 'end') %>%
    mutate(start = as.POSIXct(as.numeric(start), origin='1970-01-01'), # need to convert to dates and times
           end = as.POSIXct(as.numeric(end), origin='1970-01-01'),
           form = case_when(form=='time_hosp' ~ 'Hospital admissions',
                            form=='time_dem' ~ 'Patient demographics',
                            form=='time_funct' ~ 'Functional status',
                            form=='time_comorb' ~ 'Comorbidities',
                            form=='time_4' ~ 'Clinician-led review discussion',
                            form=='time_5' ~ 'Care directive measure',
                            form=='time_6' ~ 'Palliative care referral',
                            form=='time_dcharg' ~ 'Screening completion'))
  if(nrow(problems)>0){
    write.csv(problems, file='checks/date_checks.csv', quote=FALSE, row.names = FALSE)
  }
}
