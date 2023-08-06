#0_process_cohort_admissions.R
#19-Sep-22; updated SSB filepaths to amended datasets in folder ending _150922
source('99_packages.R')
source('99_trial_information.R')

#1. cohort lookup to match study_id with person_id in SSB datasets
cohort_lookup = read.csv('../../Data linkage/New_SSB chort_comparator_data_150922/cohort_lookup_amd.csv')


#2. read in screening dates for the at-risk group to help identify relevant admissions in the full dataset
screening_dates = list() 
screening_dates[["Gold Coast University Hospital"]] = read.xlsx('../../Data linkage/admit_discharge dates/GCUH_V3_at risk demographic.xlsx',detectDates = T) %>% clean_names()
screening_dates[["Royal Brisbane and Women's"]] = read.xlsx('../../Data linkage/admit_discharge dates/RBWH_V3_at risk_demographic.xlsx', detectDates = T) %>% clean_names()
screening_dates[["The Prince Charles Hospital"]] = read.xlsx('../../Data linkage/admit_discharge dates/TPCH_V3_at risk_demographic.xlsx', detectDates = T) %>% clean_names()

#fix up datetime format and bind into a single df (tz specified for AEST to match other dates processed)
screening_dates = lapply(screening_dates,function(x) x %>% mutate_at(vars("admission_date_time","discharge_date"),~convert_to_datetime(as.numeric(.),tz="Australia/Brisbane"))) %>%
  bind_rows(.id='fclty_name') %>% rename('screening_date_time' = admission_date_time,'at_risk_discharge_date_time'=discharge_date)

# add cohort_lookup; remove 1 missing study_id (redundant record)
screening_dates = full_join(cohort_lookup, screening_dates, by='study_id') %>% 
  clean_names() %>% 
  filter(!is.na(person_id)) %>% 
  select(-at_risk_discharge_date_time)

#check if all index admission occur within the trial timeframe
screening_dates %>% filter(screening_date_time < trial_start|screening_date_time>trial_end) %>% nrow()
screening_dates %>% filter(screening_date_time >= trial_start,screening_date_time<=trial_end) %>% nrow()

#save
save(cohort_lookup, screening_dates, file='processed/at_risk_info.rda')


#3. Read in hospital admissions data for al patients in the dataset
# large dataset, takes a while to load; indexed by person_id only as includes both trial and comparator sites
cohort_admissions = read.csv('../../Data linkage/New_SSB chort_comparator_data_150922/cohort_admit_main_amd.csv') %>% clean_names() %>%
  dplyr::select(person_id,group,admit_ep:fclty_name,start_date,end_date,orig_ref_code,sepn_mode)

#split datetimes and fix up formatting
cohort_admissions = cohort_admissions %>% mutate("start_date_1"=str_split_fixed(start_date,pattern=':',n=2)[,1],
                                                 "start_date_2"=str_split_fixed(start_date,pattern=':',n=2)[,2],
                                                 "end_date_1"=str_split_fixed(end_date,pattern=':',n=2)[,1],
                                                 "end_date_2"=str_split_fixed(end_date,pattern=':',n=2)[,2],
                                                 "admit_start_date_time"=as.POSIXct(paste(start_date_1,start_date_2),format="%d%B%Y %H:%M:%S"),
                                                 "admit_end_date_time"=as.POSIXct(paste(end_date_1,end_date_2),format="%d%B%Y %H:%M:%S")) %>%
  select(-c(start_date,start_date_1,start_date_2,end_date,end_date_1,end_date_2))

tz(cohort_admissions$admit_start_date_time) <- "Australia/Brisbane" 
tz(cohort_admissions$admit_end_date_time) <- "Australia/Brisbane"


#add information on trial vs comparator sites
cohort_admissions = cohort_admissions %>% mutate(site_group = case_when(
  fclty_name %in% trial_sites ~ 'Trial',fclty_name %in% comparator_sites ~ 'Comparator',
  !fclty_name %in% c(trial_sites,comparator_sites) ~ 'Other'))

# add at-risk information
cohort_admissions = cohort_admissions %>% mutate(patient_group = case_when(
  person_id %in% screening_dates[['person_id']]  ~ 'At risk',
  !person_id %in% screening_dates[['person_id']]  ~ '75+ years'))


#PHA amendment - change fclty_name for rbwh to match original coding
cohort_admissions = cohort_admissions %>% mutate_at('fclty_name',~ifelse(.=="Royal Brisbane & Women'S Hospital","Royal Brisbane and Women's",.))


# #save
save(cohort_admissions,file='processed/hospital_admissions_data.rda')
