#1_merge_admissions_transfers.R

#ICU transfer information is organised by person_id and admit_ep
#all identified transfers (icu_flag==1) were identified and left_joined to cohort_admissions; missing icu_flag replaced with 0.


source('99_packages.R')

#load relevant datasets
load('processed/hospital_admissions_data.rda')
load('processed/transfer_data.rda')

#arrange cohort transfers by person_id, admit_ep, trf_start_date_time (general tidy up)
cohort_tfr = cohort_tfr %>% arrange(person_id,admit_ep,tfr_start_date_time)

#identify all icu transfers by person_id and admit_ep; add number of icu tfr within a single admit ep and date/time of ICU tranfer(s)

cohort_icu_tfr = cohort_tfr %>% filter(icu_flag==1) %>% group_by(person_id,admit_ep) %>% summarise(n_icu_tfr = n(),icu_tfr_date_time_l = list(tfr_start_date_time),.groups='drop')

#join admissions and collapsed icu_flag by person_id, admit_ep
#full join - all icu transfers accounted for (6 June 22) (gives same result as left join)
cohort_admissions_icu = full_join(cohort_admissions,cohort_icu_tfr,by=c('person_id','admit_ep')) %>%
  mutate(n_icu_tfr = replace_na(n_icu_tfr,0))


#save
save(cohort_admissions_icu,file = 'processed/combined_admission_transfer_data.rda')
