# 0_checks.R
# checks of data processing
# Nov 2020
library(openxlsx)
library(dplyr)
library(stringr)
load('data/FullData.RData') # from 0_read_data_redcap.R

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
write.csv(all_check, file='checks/check_outcomes.csv', quote=FALSE, row.names = FALSE)

## checks for Gold Coast
# list of those prior
r3 = filter(care_directive, redcap_version==3, change_var=='Prior', hospital=='GCUH') %>%
  select(-redcap_version, -hospital, -index, -strings, -int_time, -change_var) %>%
  mutate(participant_id = as.numeric(str_remove_all(string=participant_id, 'GCUH_V3_'))) %>%
  arrange(participant_id)
## output to excel
hs1 <- createStyle(fgFill = "black", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Barnett")
addWorksheet(wb, sheetName = "Prior Gold coast")
writeData(wb, sheet=1, x=r3, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:10, widths = "auto")
saveWorkbook(wb, file = "checks/GCUH_prior.xlsx", overwrite = TRUE)

## output to excel
wb <- createWorkbook("Barnett")
#
addWorksheet(wb, sheetName = "GCUH")
f = filter(problems, hospital=='GCUH')
writeData(wb, sheet=1, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:4, widths = "auto")
#
addWorksheet(wb, sheetName = "TPCH")
f = filter(problems, hospital=='TPCH')
writeData(wb, sheet=2, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 2, cols = 1:4, widths = "auto")
#
addWorksheet(wb, sheetName = "RBWH")
f = filter(problems, hospital=='RBWH')
writeData(wb, sheet=3, x=f, headerStyle = hs1)
setColWidths(wb, sheet = 3, cols = 1:4, widths = "auto")
#
saveWorkbook(wb, file = "checks/time_checks.xlsx", overwrite = TRUE)
