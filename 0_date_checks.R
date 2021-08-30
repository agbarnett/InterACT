# 0_date_checks.R
# checks of dates
# June 2021
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
load('data/FullData.RData') # from 0_read_data_redcap.R

# have to rename date/times as they all use same name
care_directive = rename(care_directive, 
                        'form_date' = 'start_date_time_5' ,
                        'outcome_date' = 'date_time_5' ) 
clinicianled_review = rename(clinicianled_review, 
                         'form_date' = 'start_date_time_4',
                         'outcome_date' = 'time_care_review' )
palliative_care_referral = rename(palliative_care_referral, 
                      'form_date' = 'start_date_time_6',
                      'outcome_date' = 'pall_date')


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

## output to Excel
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

############
## check for discharge dates before admissions ##
discharge = select(complete, redcap_version, participant_id, hosp_discharge_date)
admission = select(baseline, participant_id, admission_datetime)
together = full_join(admission, discharge, by='participant_id') %>%
  filter(redcap_version==3, 
         as.Date(admission_datetime) > as.Date('2020-05-25'), # after new start date
         !is.na(admission_datetime), # must have admission date/time
         hosp_discharge_date <= admission_datetime ) %>%
  select(-redcap_version) %>%
  mutate(problem = case_when(
    hosp_discharge_date == admission_datetime ~ "Same admission and discharge date/time",
    hosp_discharge_date < admission_datetime ~ "Discharge before admission"
  ))
table(together$problem)
# check for missing discharge for those with discharge yes
discharge = select(complete, redcap_version, participant_id, discharge_hosp, hosp_discharge_date)
admission = select(baseline, participant_id, admission_datetime)
together2 = full_join(admission, discharge, by='participant_id') %>%
  filter(redcap_version==3, 
         as.Date(admission_datetime) > as.Date('2020-05-25'), # after new start date
         discharge_hosp == 'Yes',
         !is.na(admission_datetime), # must have admission date/time
         is.na(hosp_discharge_date)) %>%
  select(-redcap_version, -discharge_hosp) %>%
  mutate(problem = 'Discharged but no date')
# Put both problems together
together = bind_rows(together, together2) %>%
  mutate(num = as.numeric(str_remove_all(participant_id, 'V3|_|[A-Z]'))) %>%
  arrange(num) %>%
  select(-num)
  
## output to Excel
hs1 <- createStyle(fgFill = "black", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Barnett")
#
addWorksheet(wb, sheetName = "GCUH")
f = filter(together, str_sub(participant_id,1,4)=='GCUH')
writeData(wb, sheet=1, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:4, widths = "auto")
#
addWorksheet(wb, sheetName = "TPCH")
f = filter(together, str_sub(participant_id,1,4)=='TPCH')
writeData(wb, sheet=2, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 2, cols = 1:4, widths = "auto")
#
addWorksheet(wb, sheetName = "RBWH")
f = filter(together, str_sub(participant_id,1,4)=='RBWH')
writeData(wb, sheet=3, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 3, cols = 1:4, widths = "auto")
#
saveWorkbook(wb, file = "checks/date_checks_discharge.xlsx", overwrite = TRUE)

### DOES NOT WORK, AS IDS WERE NOT CHRONOLOGICAL: "Sometimes we had to delete records as they were duplicates or something like that, then this left a blank record in REDCap that the auditors filled in next time "
# plot dates by ID to look for outliers
diffs = filter(baseline, redcap_version == 3) %>%
  mutate(num = as.numeric(str_remove_all(participant_id, '_V3|_|GCUH|TPCH|RBWH'))) %>%
  group_by(hospital) %>%
  arrange(hospital, num) %>%
  select(participant_id, hospital, num, admission_date)
with(diffs, plot(admission_date))
d = as.numeric( diff(diffs$admission_date))
index = which(d > 100)
problems = diffs[index,] %>% # get problem IDs
  mutate(id=num)
# now look at people either side - TO DO
problems1 = mutate(problems, id=num, num = num - 1)
problems2 = mutate(problems, id=num, num = num + 1)
all_problems = bind_rows(problems, problems1, problems2, .id='problem') %>%
  select(-admission_date, -participant_id) %>% # remove same admission date
  left_join(diffs, by=c('hospital','num')) %>%
  arrange(hospital, id, num) %>%
  select(hospital, id, participant_id, admission_date)
# add empty rows for excel
empty = select(all_problems, hospital, id) %>% unique()
all_problems = bind_rows(all_problems, empty) %>% 
  arrange(hospital, id)
## output to Excel
hs1 <- createStyle(fgFill = "black", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Barnett")
#
addWorksheet(wb, sheetName = "GCUH")
f = filter(all_problems, hospital=='GCUH')
writeData(wb, sheet=1, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:4, widths = "auto")
#
addWorksheet(wb, sheetName = "TPCH")
f = filter(all_problems, hospital=='TPCH')
writeData(wb, sheet=2, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 2, cols = 1:4, widths = "auto")
#
addWorksheet(wb, sheetName = "RBWH")
f = filter(all_problems, hospital=='RBWH')
writeData(wb, sheet=3, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 3, cols = 1:4, widths = "auto")
#
saveWorkbook(wb, file = "checks/date_checks_differences.xlsx", overwrite = TRUE)

## impossible dates for outcomes 4,5,6
#
filter(clinicianled_review, outcome_date <= ISOdatetime(2019,1,1,0,0,0)) %>% select(participant_id, outcome_date)
filter(clinicianled_review, outcome_date > ISOdatetime(2021,7,1,0,0,0)) %>% select(participant_id, outcome_date)
#
filter(palliative_care_referral, outcome_date <= ISOdatetime(2019,1,1,0,0,0)) %>% select(participant_id, outcome_date)
filter(palliative_care_referral, outcome_date > ISOdatetime(2021,7,1,0,0,0)) %>% select(participant_id, outcome_date)
#
filter(care_directive, outcome_date  <= ISOdatetime(2019,1,1,0,0,0)) %>% select(participant_id, outcome_date )
filter(care_directive, outcome_date  > ISOdatetime(2021,7,1,0,0,0)) %>% select(participant_id, outcome_date )
