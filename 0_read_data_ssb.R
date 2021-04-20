# 0_read_data_ssb.R
# read the data on QHAPDC and deaths from SSB (see linkage_report_shield.pdf for details)
# these data are from a specific request we made with treating doctor IDs from our hospital
# for the outcomes Length of stay, ICU admission , Re-admissions, Discharge outcomes/death , Referrals
# not for comparator data (on control hospitals)
# November 2020
library(dplyr)
library(janitor)

data_folder = 'U:\\Research\\Projects\\ihbi\\aushsi\\interact\\TRIAL\\Data collection\\Historical datasets\\SSB\\linkage_report-shield-interact_historical\\'

# a) admissions main
file = "hist_admit_main"
admissions = read.table(paste(data_folder, file, '.csv', sep=''), sep=',', header=TRUE, quote='') %>%
  clean_names() %>%
  filter(fclty_id %in% c('00201','00004','00936')) %>% # must be on of Interact hospitals - other hospitals included as we gave SSB the treating doctor ID
  mutate(start_date = as.POSIXct(start_date, format='%d%b%Y:%H:%M:%S', tz="Australia/Brisbane"), # admission date/time
         admission_date = as.Date(start_date),
         end_date = as.POSIXct(end_date, format='%d%b%Y:%H:%M:%S', tz="Australia/Brisbane"),
         los = as.numeric(difftime(end_date, start_date,units='hours')),# calculate length of stay
         hosp = case_when(
           fclty_id =='00201' ~ 'rbwh',
           fclty_id =='00004' ~ 'tpch',
           fclty_id =='00936' ~ 'gcuh'
         )) %>% # for merging with redcap data
  select(-idx, -treating_dr_id, -group, -fclty_id, -fclty_name, -end_date) %>% # drop variables no longer needed
  rename('admission_datetime' = 'start_date')

# b) transfers
trans_file = 'hist_admit_tfr'
transfers = read.table(paste(data_folder, trans_file, '.csv', sep=''), sep=',', header=TRUE, quote='') %>%
  clean_names()  %>%
  filter(icu_flag==1) %>% # just ICU transfers
  mutate(tfr_date = as.POSIXct(tfr_date, format='%d%b%Y:%H:%M:%S', tz="Australia/Brisbane"), # 
         tfr_end_date = as.POSIXct(tfr_end_date, format='%d%b%Y:%H:%M:%S', tz="Australia/Brisbane")) %>%
  select(person_id, admit_ep, tfr_date) %>%
  group_by(person_id, admit_ep) %>%
  arrange(person_id, admit_ep, tfr_date) %>%
  slice(1) # take earliest ICU admission if there are multiple transfers per patient

# c) deaths
death_file = 'hist_cod_main'
deaths = read.table(paste(data_folder, death_file, '.csv', sep=''), sep=',', header=TRUE, quote='\"') %>%
  clean_names() %>%
  mutate(death_date = as.Date(death_date, format='%d/%m/%Y')) %>%
  select(-cause, -coded_cause , -fclty_name) # drop variables not needed

# merge admissions and ICU transfers
ad = left_join(admissions, transfers, by=c('person_id','admit_ep')) %>% # person_id is unique
  mutate(icu_outcome = !is.na(tfr_date)) # binary outcome based on transfer date existing
# check for multiple results per person-episode
check = group_by(ad, person_id, admit_ep) %>%
  tally() %>%
  filter(n>1)

# merge admissions and deaths
routine = left_join(ad, deaths, by='person_id') # person_id is unique

# save
save(routine, file='data/SSB_data.RData')

# check ICU rates
prop.table(table(routine$icu_outcome))

