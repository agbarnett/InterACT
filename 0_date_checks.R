# 0_date_checks.R
# checks of dates
# April 2021
library(openxlsx)
library(dplyr)
library(stringr)
load('data/FullData.RData') # from 0_read_data_redcap.R

# have to rename date/times as they all use same name
care_directive = rename(care_directive, 
                        'time_4' = 'time_change',
                        'form_date_care_directive' = 'form_date',
                        'outcome_date_care_directive' = 'outcome_date') %>%
  select(-at_risk_date) # just keep for one form
clinicianled_review = rename(clinicianled_review, 
                         'time_5' = 'time_change',
                         'form_date_clinicianled_review' = 'form_date',
                         'outcome_date_clinicianled_review' = 'outcome_date')%>%
  select(-at_risk_date) # just keep for one form
palliative_care_referral = rename(palliative_care_referral, 
                                  'time_6' = 'time_change',
                                  'form_date_palliative_care' = 'form_date',
                                  'outcome_date_palliative_care' = 'outcome_date')


## 1) looking at times 
# 
problems = bind_rows(baseline, care_directive, clinicianled_review, palliative_care_referral) %>% # use all forms with times
  select("participant_id",'hospital', starts_with('time_')) %>%
  filter(str_detect(string=participant_id, pattern='_V3_')) %>% # just version 3
  tidyr::gather(value='minutes', key='form', -"participant_id", -"hospital") %>%
  filter(!is.na(minutes), # lots of missing because of lack of overlap (not true missing)
         minutes<0 | minutes>120) %>%
  mutate(
    num = as.numeric(str_remove_all(string=participant_id, pattern='[A-Z][A-Z][A-Z][A-Z]_V3_')),
    minutes = round(minutes*10)/10, # round
    form = case_when(form=='time_hosp' ~ 'Hospital admissions',
                     form=='time_dem' ~ 'Patient demographics',
                     form=='time_funct' ~ 'Functional status',
                     form=='time_comorb' ~ 'Comorbidities',
                     form=='time_4' ~ 'Clinician-led review discussion',
                     form=='time_5' ~ 'Care directive measure',
                     form=='time_6' ~ 'Palliative care referral',
                     form=='time_dcharg' ~ 'Screening completion')) %>%
  arrange(hospital, num) %>%
  select(-num)

## 2) looking at dates
excess_time = 60*60*24*180 # time in days (using last number)

# format dates
dates = bind_rows(baseline, care_directive, clinicianled_review, palliative_care_referral) %>% # use all forms with times
  select("participant_id", contains('date')) %>%
  filter(str_detect(string=participant_id, pattern='_V3_')) %>% # just version 3
  tidyr::gather(value='date', key='form', -"participant_id") %>%
  unique() %>%
  filter(!is.na(date),
         !form %in% c('admission_date','date_seen')) %>% # drop for now, these are in date format, others are date/time
  group_by(participant_id) %>%
  mutate(
    hospital = str_sub(participant_id, 1, 4),
    n = n(), # number of observations
    date = as.POSIXct(date , origin = '1970-01-01', tz = "Australia/Brisbane"), 
    median = median(date),
    diff = abs(date - median)) %>%
  ungroup()
# get problem IDs
problem_dates = filter(dates, 
                       n > 1, # must have more than 1 date
                       diff >= excess_time,
                       !str_detect(form, pattern = 'outcome_date')) %>% # lots are prior
  select(participant_id) %>%
  unique()
# now merge back with time data
problem_ids = left_join(problem_dates, dates, by='participant_id') %>%
  arrange(participant_id, date) %>%
  mutate(problem = diff >= 10000000) %>%
  select(-median, -diff, -n)

## output to excel
hs1 <- createStyle(fgFill = "black", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Barnett")
#
addWorksheet(wb, sheetName = "GCUH")
f = filter(problem_ids, hospital=='GCUH')
writeData(wb, sheet=1, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
#
addWorksheet(wb, sheetName = "TPCH")
f = filter(problem_ids, hospital=='TPCH')
writeData(wb, sheet=2, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 2, cols = 1:6, widths = "auto")
#
addWorksheet(wb, sheetName = "RBWH")
f = filter(problem_ids, hospital=='RBWH')
writeData(wb, sheet=3, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 3, cols = 1:6, widths = "auto")
#
saveWorkbook(wb, file = "checks/date_checks.xlsx", overwrite = TRUE)

