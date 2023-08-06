# 2_add_screening_info.R
#XL edits added on 26-May-22
source('99_packages.R')
source('99_trial_information.R')

#load relevant datasets (all processed)
load('processed/at_risk_info.rda') # from 0_process_cohort_admissions.R
load('processed/combined_admission_transfer_data.rda')

#For the at risk group, join screening information on cristal/spict to identify admission(s) where patient was define as at risk
#applying inner_join gives admissions for atrisk person_id at trial sites only; aim is to identify first encounter
screening_admissions_atrisk = inner_join(cohort_admissions_icu,screening_dates,by=c('person_id','fclty_name'))

#find likely admission where the patient was screened
##once candidate admission(s) are found, collapse by: study_id,person_id,group,fclty_name
### start with exact date_time matches
exact_matches = screening_admissions_atrisk %>% filter((admit_start_date_time==screening_date_time))
           
### for patients without and exact match, find screening_date_time between admit start and end dates;take out exact matches
within_admission_matches = filter(screening_admissions_atrisk,!study_id %in% exact_matches[['study_id']],
                                  (admit_start_date_time<screening_date_time & screening_date_time<admit_end_date_time)) 
                                  

## for remaining patients, catch screening date_time before recorded admit_start but occurred on the same day
before_admission_matches = filter(screening_admissions_atrisk,!study_id %in% c(exact_matches[['study_id']],within_admission_matches[['study_id']]),
                                  (screening_date_time<admit_start_date_time & difftime(admit_start_date_time,screening_date_time,units='days')<=1)) %>%
  #for 1 study_id with multiple matches, take the admit_start_date_time closest to screening_date_time
  group_by(study_id) %>% filter(difftime(admit_start_date_time,screening_date_time,units='days')==min(difftime(admit_start_date_time,screening_date_time,units='days'))) %>% ungroup()

#bind_rows to update screening_admission_atrisk
#icu_flag is per admit_ep. to get full admission, unest icu_tfr_date_time_l
cristal_spict_admission_info = bind_rows(at_admit = exact_matches,
                                         within_admit = within_admission_matches,
                                         before_admit = before_admission_matches,.id="cristal_spict_date_match_type") %>% 
  arrange(study_id,person_id,age_on_admission,sex,clinical_team,group,admit_ep) %>%
  rename('cristal_spict_date_time'=screening_date_time)  %>% 
  select(study_id,person_id,age_on_admission,sex,clinical_team,group,admit_ep,fclty_name,cristal_spict_date_time,cristal_spict_date_match_type) 

#identify unmatched records in at risk cohort
outstanding_records = screening_dates %>% filter(!study_id %in% cristal_spict_admission_info[['study_id']])

outstanding_records = outstanding_records %>% left_join(cohort_admissions_icu,by=c('person_id','fclty_name'))


#save
save(cohort_admissions_icu,cristal_spict_admission_info,outstanding_records,file='analysis_ready/all_hospital_admissions.rda')



