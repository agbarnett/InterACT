# 3_define_outcomes.R
#Primary outcome analysis: Proportion of patients with one or more ICU admissions
## ICU admissions defined during the current hospital stay (group) from the date first recorded as high-risk cristal and spict-positive
## Updated 29-Jul-22 to define readmissions, add met data

source('99_packages.R')
source('99_trial_information.R')

#load data from analysis_ready folder
load('analysis_ready/all_hospital_admissions.rda') # from ?.R
#load deaths data
load('processed/death_data.rda')
#load met data
load('processed/met_data.rda')

#Outline of main data processing steps
##1. join screening information as start of measurement period and to add study_id; left_join to filter to 'index admissions'/exposure only; admit_ep no longer needed as admissions already collapsed; fclty_name will be the trial site where the patient was first identified as atrisk
##2. For length of stay and ICU admissions, filter 
##3. take all admissions for at-risk group (admissions_icu_atrisk) and collapse by person_id, group; take first admit_start and last admit_end; add admit and discharge fclty, final disposition; collapse icu_flag, n_icu_tfr, length_of_stay_mins

#overall admission information for trial cohort;  take all admit_ep that occur on or after admit_ep linked to cristal_spict identification
N = list()

#start of admission first
cristal_spict_admission_info = cristal_spict_admission_info %>% left_join(select(cohort_admissions_icu,person_id:admit_ep,orig_ref_code,admit_start_date_time),by=c('person_id','group','admit_ep'))

N[['total matching cristal spict']] = cristal_spict_admission_info %>% distinct(study_id,person_id) %>% nrow()

duplicated_screening = cristal_spict_admission_info[duplicated(select(cristal_spict_admission_info,person_id,group)),] 


#take the first admit_ep within a group -> confirm with CIs
cristal_spict_admission_info = anti_join(cristal_spict_admission_info,duplicated_screening)

N[['total matches after removing duplicates']] = cristal_spict_admission_info %>% distinct(person_id) %>% nrow()

#define episode of care at trial facility (UPDATED following CI meeting on 11 JUL 2022)

#find last admit_ep to define end of admission details; updated: take distinct to account for duplicates
#filter to trial sites only
trial_sites[2]<-"Royal Brisbane and Women's"
trial_admissions_icu = filter(cohort_admissions_icu,fclty_name %in% trial_sites)

#add information corresponding to cristal/spict admission;join by person_id, fclty_name and group
#filter to admit_ep equal to or after cristal_spict index admit_ep
ad <- select(cristal_spict_admission_info,study_id,person_id,group,admit_ep,fclty_name,cristal_spict_date_time)
trial_admissions_icu = inner_join(trial_admissions_icu,ad,by=c('person_id','group','fclty_name'),suffix=c('_cohort','_index')) %>% filter(admit_ep_cohort>=admit_ep_index)


#add outcome based on sepn_mode. the first non-na outcome will be taken as the end of the episode of care. care type changes will be NA as patient still in the same hospital
#defined at admit_ep_cohort level within group
sepn_mode_discharged = c('Home/Usual Residence','Racs Not Usual Place Of Residence','Racs Usual Place Of Residence','Residential Aged Care Service','Other Health Care Accommodation','Other')

trial_admissions_icu = trial_admissions_icu %>% arrange(study_id,person_id,group,admit_ep_cohort) %>%
  mutate(fclty_outcome = case_when(
  sepn_mode %in% sepn_mode_discharged ~ 'Discharged alive',
  sepn_mode == 'Died In Hospital' ~ 'Died in hospital',
  sepn_mode == 'Transferred To Another Hospital' ~ 'Censored'))

#define the end of each admission as the first discharge/death from the trial facility
end_admission_info = filter(trial_admissions_icu,!is.na(fclty_outcome)) %>% group_by(study_id,fclty_name) %>% slice_min(admit_ep_cohort) %>% ungroup() %>%
  select(study_id,person_id,group,admit_ep_cohort,sepn_mode,fclty_outcome,admit_end_date_time)

unknown_fclty_outcome<- setdiff(cristal_spict_admission_info[['study_id']],end_admission_info[['study_id']])

#for unknown admission outcomes, censor at the end datetime for the last admit_ep in admissions data; all cases are censored at care type change
ad = filter(trial_admissions_icu,study_id %in% unknown_fclty_outcome) %>% group_by(study_id) %>% slice_max(admit_ep_cohort) %>% ungroup() %>% 
  select(study_id,person_id,group,admit_ep_cohort,sepn_mode,admit_end_date_time) %>% add_column('fclty_outcome'='Censored',.before='admit_end_date_time')
 
end_admission_info <- bind_rows(end_admission_info,ad) %>% arrange(study_id,person_id,group,admit_ep_cohort)  %>% rename('admit_ep'=admit_ep_cohort)

#join with cristal_spict_admission_info to get summary of each screened admission
cristal_spict_admission_info = left_join(cristal_spict_admission_info, end_admission_info,by=c('study_id','person_id','group'),suffix=c('_start','_end'))

#remove objects no longer in use
rm(unknown_fclty_outcome,ad)

# add deaths data
ad = cohort_deaths %>% select(person_id,death_date,death_place)
cristal_spict_admission_info = cristal_spict_admission_info %>% left_join(ad,by='person_id')

#update end_admission, final disposition based on death_date
#if death_date on or before end_admission, change fclty_outcome to Died
cristal_spict_admission_info  = mutate(cristal_spict_admission_info ,fclty_outcome = ifelse(!is.na(death_date) & death_date<=as.Date(admit_end_date_time) & death_place=='Hospital','Died in hospital', fclty_outcome))


# define time zone for admit start and end dates to match cristal_spict screening time
tz(cristal_spict_admission_info$admit_start_date_time) <- "Australia/Brisbane"
tz(cristal_spict_admission_info$admit_end_date_time) <- "Australia/Brisbane"
rm(ad)

#define readmissions 
#readmission time starts at the end of the current continuous episode of care (group)
index_admission_info <- cristal_spict_admission_info %>% select(study_id,person_id,group)

#find the end of each index admission as the maximum admit end date/time
discharges <- index_admission_info %>% left_join(cohort_admissions_icu,by=c('person_id','group')) %>% 
  select(study_id,person_id,group,fclty_name,admit_end_date_time,sepn_mode) %>%
  group_by(study_id,person_id,group) %>% slice_max(admit_end_date_time) %>% ungroup() %>%
  rename('discharge_group'=group,'discharge_fclty'=fclty_name,'discharge_date_time'=admit_end_date_time)

#add discharge outcome
discharges = discharges %>% mutate(discharge_outcome = case_when(
  sepn_mode %in% sepn_mode_discharged ~ 'Discharged alive',
  sepn_mode == 'Died In Hospital' ~ 'Died in hospital',
  sepn_mode %in% c('Transferred To Another Hospital','Care Type Change') ~ 'Censored'))


#find all public readmissions
#NB: selected person_ids will have multiple discharges, but will have different study_ids; can be identified be person_id and gruop
trial_public_readmissions = cohort_admissions_icu %>% filter(!fclty_name %in% c("Private"))

ad<- discharges %>% select(study_id,person_id,discharge_group,discharge_fclty,discharge_date_time,discharge_outcome)

trial_public_readmissions = left_join(ad,trial_public_readmissions,by='person_id') %>% filter(group>discharge_group)

#for each study_id, take the first (minimum) admit_start_date_time as the time of readmission
trial_public_readmissions = trial_public_readmissions %>% 
  group_by(study_id) %>% slice_min(admit_start_date_time) %>% ungroup() %>%
  rename("readmit_group"=group,'readmit_fclty'=fclty_name,'readmit_admit_ep'=admit_ep,'readmit_start_date_time'=admit_start_date_time,'readmit_end_date_time'=admit_end_date_time) %>%
  select(study_id,person_id,discharge_group,discharge_outcome,discharge_fclty,readmit_group,readmit_fclty,orig_ref_code,discharge_date_time,readmit_start_date_time,readmit_end_date_time) %>%
  arrange(study_id,person_id)
rm(ad)


# add discharge information and time to  readmission 
ad <- discharges %>% select(study_id,discharge_date_time,discharge_outcome)
cristal_spict_admission_info <- left_join(cristal_spict_admission_info,ad,by='study_id')

ad <- trial_public_readmissions %>% select(study_id,readmit_start_date_time,readmit_fclty)
cristal_spict_admission_info <- left_join(cristal_spict_admission_info,ad,by='study_id')

#fix discharge outcome|fclty_outcome
cristal_spict_admission_info <- cristal_spict_admission_info %>% mutate(discharge_outcome = ifelse(fclty_outcome=='Died in hospital','Died in hospital',discharge_outcome))

cristal_spict_admission_info %>% count(fclty_outcome,discharge_outcome)

#add trial dates
# trial_dates = trial_dates %>% mutate_at(vars('start_date','end_date'),~as.Date(.,tz='Australia/Brisbane'))
tz(trial_dates$start_date) <- "Australia/Brisbane"
tz(trial_dates$end_date) <- "Australia/Brisbane"

cristal_spict_admission_info = cristal_spict_admission_info %>% rowwise() %>%
  mutate(study_period = case_when(
    #Usual care (pre COVID)
    between(as.Date(cristal_spict_date_time),
            filter(trial_dates,study_period=='Usual care (pre COVID)')[['start_date']] %>% as.Date(),
            filter(trial_dates,study_period=='Usual care (pre COVID)')[['end_date']] %>% as.Date()) ~ 'Usual care (pre COVID)',
    #Usual care (COVID)
    between(as.Date(cristal_spict_date_time),
            filter(trial_dates,study_period=='Usual care (COVID)')[['start_date']] %>% as.Date(),
            filter(trial_dates,study_period=='Usual care (COVID)')[['end_date']] %>% as.Date()) ~ 'Usual care (COVID)',
    #Usual care
    between(as.Date(cristal_spict_date_time),
            filter(trial_dates,site_name==fclty_name,study_period=='Usual care')[['start_date']] %>% as.Date(),
            filter(trial_dates,site_name==fclty_name,study_period=='Usual care')[['end_date']] %>% as.Date()) ~ 'Usual care',
    #Intervention establishment
    between(as.Date(cristal_spict_date_time),
            filter(trial_dates,site_name==fclty_name,study_period=='Intervention establishment')[['start_date']] %>% as.Date(),
            filter(trial_dates,site_name==fclty_name,study_period=='Intervention establishment')[['end_date']] %>% as.Date()) ~ 'Intervention establishment',
    between(as.Date(cristal_spict_date_time),
            filter(trial_dates,site_name==fclty_name,study_period=='Intervention exposure')[['start_date']] %>% as.Date(),
            filter(trial_dates,site_name==fclty_name,study_period=='Intervention exposure')[['end_date']] %>% as.Date()) ~ 'Intervention exposure')) %>%
  ungroup()

#apply censoring based on study phase (sectino 13.2, full protocol)
## temporarily attached trial_dates to cristal_spict_admission_info
ad_intervention_dates = filter(trial_dates,!grepl("Usual care",study_period),!is.na(site_name)) %>% select(site_name,study_period,start_date) %>% spread(study_period,start_date) %>% clean_names
cristal_spict_admission_info = left_join(cristal_spict_admission_info,ad_intervention_dates,by=c("fclty_name"="site_name")) %>% add_column(study_end = as.POSIXct("2021-06-06",tz='Australia/Brisbane'))
rm(ad_intervention_dates)

## Censoring criteria
#1. Usual care exposure period patient data will only include patients admitted to a clinical team and identified as 
#high-risk CriSTAL or SPICT-positive during the usual care exposure period 

#2. Data collected in the intervention establishment phase will not be included in the statistical analysis. 
#Patients who remain in the wards at the change-over time to the establishment phase are censored on the day prior to the change-over. 

#3. Only patients admitted and identified as high-risk CriSTAL or SPICT-positive in the intervention exposure phase 
#will be included as data for the intervention exposure phase.

#4. Patients who remain in the hospital at the end of the intervention exposure phase will be censored on the last study day. 

#create two variables to identify included patients (include) and censoring of admissions (censored_admit)
## include first
cristal_spict_admission_info = mutate(cristal_spict_admission_info,
                                      include = case_when(
                                        #1.
                                        study_period=="Usual care" & admit_start_date_time<=intervention_establishment & cristal_spict_date_time<=intervention_establishment ~ 1,
                                        study_period=="Usual care" & (admit_start_date_time>intervention_establishment|cristal_spict_date_time>intervention_establishment) ~ 0,
                                        #2. 
                                        study_period=="Intervention establishment" ~ 0,
                                        #3.
                                        study_period=="Intervention exposure" & admit_start_date_time>=intervention_exposure & cristal_spict_date_time>=intervention_exposure ~ 1,
                                        study_period=="Intervention exposure" & (admit_start_date_time<intervention_exposure|cristal_spict_date_time<intervention_exposure) ~ 0))
#filter to include=1 only
excluded_on_censoring = filter(cristal_spict_admission_info,include==0)
cristal_spict_admission_info = filter(cristal_spict_admission_info,include==1)

## censor; applies to admit_end_date_time, not readmit_start_date_time but added in case needed to time-to-event curve
cristal_spict_admission_info = mutate(cristal_spict_admission_info,
                                      admit_end_date_time_c = case_when(
                                        #usual care
                                        study_period=="Usual care" & admit_end_date_time<=intervention_establishment ~ admit_end_date_time,
                                        study_period=="Usual care" & admit_end_date_time>intervention_establishment ~ intervention_establishment-days(1), #censor day prior to change-over
                                        #intervention exposure
                                        study_period=='Intervention exposure' & admit_end_date_time>study_end ~ study_end,
                                        study_period=='Intervention exposure' & admit_end_date_time<=study_end ~ admit_end_date_time
                                        ),
                                      censored_admit =  case_when(admit_end_date_time==admit_end_date_time_c ~ 0,admit_end_date_time_c<admit_end_date_time ~ 1),
                                      readmit_start_date_time_c = case_when(
                                        #usual care
                                        study_period=="Usual care" & readmit_start_date_time<=intervention_establishment ~ readmit_start_date_time,
                                        study_period=="Usual care" & readmit_start_date_time>intervention_establishment ~ intervention_establishment-days(1), #censor day prior to change-over
                                        #intervention exposure
                                        study_period=='Intervention exposure' & readmit_start_date_time>study_end ~ study_end,
                                        study_period=='Intervention exposure' & readmit_start_date_time<=study_end ~ readmit_start_date_time),
                                      censored_readmit = case_when(readmit_start_date_time==readmit_start_date_time_c ~ 0,readmit_start_date_time_c<readmit_start_date_time ~ 1))

#tidy up variable order
cristal_spict_admission_info = cristal_spict_admission_info %>% 
  select(study_id:group,fclty_name,admit_ep_start,admit_ep_end,study_period,cristal_spict_date_time,cristal_spict_date_match_type,orig_ref_code,sepn_mode,fclty_outcome,
         admit_start_date_time,admit_end_date_time,admit_end_date_time_c,discharge_outcome,discharge_date_time,readmit_start_date_time,readmit_start_date_time_c,readmit_fclty,death_date,death_place,censored_admit,censored_readmit)

#update admit_end_date_time and fclty_outcome to apply censoring
cristal_spict_admission_info = cristal_spict_admission_info %>% mutate(fclty_outcome = ifelse(censored_admit==1,'Censored',fclty_outcome))
filter(cristal_spict_admission_info,censored_admit==0) %>% count(fclty_outcome,discharge_outcome)
filter(cristal_spict_admission_info,censored_admit==1) %>% count(fclty_outcome,discharge_outcome)

#if facility admission is censored, then so is the continuous episode of care
cristal_spict_admission_info = cristal_spict_admission_info %>% mutate(discharge_outcome = ifelse(censored_admit==1,'Censored',discharge_outcome))

#define length of stay from date of cristal/spict to final disposition; censored and observed
cristal_spict_admission_info = cristal_spict_admission_info %>% mutate(length_of_stay_days = as.numeric(admit_end_date_time - cristal_spict_date_time,'days'),
                                                                       length_of_stay_days_c = as.numeric(admit_end_date_time_c - cristal_spict_date_time,'days'))



#define time to public hospital readmission; censored and observed from discharge_date_time
cristal_spict_admission_info = cristal_spict_admission_info %>% mutate(discharge_to_readmit_days = as.numeric(readmit_start_date_time - discharge_date_time,'days'),
                                                                       discharge_to_readmit_c = as.numeric(readmit_start_date_time_c - discharge_date_time,'days'))

#define icu transfers from cristal_spict_date_time
#subset all known icu transfers
icu_transfers = cohort_admissions_icu %>% select(person_id,group,fclty_name,icu_tfr_date_time_l) %>% unnest(icu_tfr_date_time_l,keep_empty=F) %>%
  rename('fclty_icu_tfr' = fclty_name) %>% mutate(icu_tfr_site = case_when(fclty_icu_tfr %in% trial_sites ~ 'Trial',
                                                                           fclty_icu_tfr %in% comparator_sites ~ 'Comparator',
                                                                           !fclty_icu_tfr %in% c(trial_sites,comparator_sites,'Private') ~ 'Other public hospital',
                                                                           fclty_icu_tfr=="Private" ~ 'Private hospital'))
tz(icu_transfers$icu_tfr_date_time_l) <- "Australia/Brisbane" 

#at screening date and fclty; filter to on or after cristal_spict_date_time; then collapse before joining iwth admissions_icu_atrisk_c
#icu transfers only counted prior to censoring 
trial_icu_transfers = inner_join(cristal_spict_admission_info,icu_transfers,by=c('person_id','group')) %>% 
  #time of icu transfer must be between screening dat and (censored) end of admission
  filter(cristal_spict_date_time<=icu_tfr_date_time_l,icu_tfr_date_time_l<=admit_end_date_time_c) %>%
  group_by(person_id,group) %>% 
  summarise(first_icu_tfr_date = min(icu_tfr_date_time_l,na.rm=T),n_icu_tfr = n(),.groups='drop') %>% add_column(icu_flag=1,.after='group')

cristal_spict_admission_info = cristal_spict_admission_info %>% left_join(trial_icu_transfers,by=c('person_id','group')) %>%
  mutate(icu_flag = replace_na(icu_flag,0),n_icu_tfr = replace_na(n_icu_tfr,0))

#add met calls
met_calls = atrisk_met %>% unnest(met_date_time_l,keep_empty=F) %>% inner_join(cristal_spict_admission_info %>% select(study_id,cristal_spict_date_time,admit_end_date_time_c),by='study_id')

#filter to calls within trial admission
met_calls = met_calls %>% filter(cristal_spict_date_time<=met_date_time_l,met_date_time_l<=admit_end_date_time_c) %>% group_by(study_id) %>%
  summarise(first_met_date_time = min(met_date_time_l),n_met = n(),.groups='drop') %>% add_column(met_flag=1,.after='study_id')

#add to cristal_spict_admission_info
#define met status with discharge alive and died as competing risks
cristal_spict_admission_info = cristal_spict_admission_info %>% left_join(met_calls,by=c('study_id')) %>%
  mutate(status_met = case_when(!is.na(met_flag) ~ 1,
                                (is.na(met_flag) & discharge_outcome=='Discharged alive') ~ 2,
                                (is.na(met_flag) & discharge_outcome=='Died in hospital') ~ 3,
                                (is.na(met_flag) & discharge_outcome=='Censored') ~ 0),
         n_met = replace_na(n_met,0),met_flag=replace_na(met_flag,0))

#add time from screening to met
#for patients with no met call, define time as length_of_stay_days_c  
cristal_spict_admission_info = mutate(cristal_spict_admission_info,screening_to_met_days = case_when(status_met==1 ~ as.numeric(first_met_date_time - cristal_spict_date_time,'days'),
                                                                                                    status_met!=1 ~ length_of_stay_days_c))



# # add discharge to death and screening to death 
# #given death_date is not in date/time format, take ceiling() for whole days
# add admission to screening (hours)

cristal_spict_admission_info = mutate(cristal_spict_admission_info,
                                      admission_to_screening_hrs = as.numeric(cristal_spict_date_time - admit_start_date_time,'hours'),
                                      discharge_to_death_days = ceiling(as.numeric(death_date - admit_end_date_time,'days')),
                                      screening_to_death_days = ceiling(as.numeric(death_date - cristal_spict_date_time,'days')))


#define readmit flag with censoring at death and trial end date: 0=censored; 1=readmitted within 12 weeks; 2=died within 12 weeks of discharge without readmission
cristal_spict_admission_info = mutate(cristal_spict_admission_info,
                                      readmit_flag = case_when(
                                        (is.na(discharge_to_readmit_days) & is.na(discharge_to_death_days)) ~ 0, #were not readmitted and still alive at end of study
                                        (!is.na(discharge_to_readmit_days) & is.na(discharge_to_death_days)) ~ 1, #were readmitted and still alive
                                        is.na(discharge_to_readmit_days) & !is.na(discharge_to_death_days) ~ 2, #were not readmitted and died before the end of the study
                                        #readmission relative to death within study timeframe: no cases where death_date<date_readmit (checked in case of data quality issue)
                                        !is.na(discharge_to_readmit_days) & !is.na(discharge_to_death_days) & death_date==date(readmit_start_date_time) ~ 1, #readmitted but died on same day (only date of death available confirm as sensitivity analysis)
                                        !is.na(discharge_to_readmit_days) & !is.na(discharge_to_death_days) & discharge_to_death_days>discharge_to_readmit_days ~ 1), #were readmitted but died between readmission and the end of the study
                                      discharge_to_readmit_days = pmin(discharge_to_death_days,discharge_to_readmit_days,na.rm=T),
                                      status_readmit = case_when(readmit_flag==0 ~ 0, #not readmitted
                                                                 #readmitted
                                                                 (readmit_flag==1 & discharge_to_readmit_days<=84) ~ 1,
                                                                 (readmit_flag==1 & discharge_to_readmit_days>84) ~ 0,
                                                                 #died before readmission
                                                                 (readmit_flag==2 & discharge_to_readmit_days<=84) ~ 2,
                                                                 (readmit_flag==2 & discharge_to_readmit_days>84) ~ 0),
                                      discharge_to_readmit_days_c = pmin(discharge_to_readmit_days,84,na.rm=T))

#add clinical_team x trial site
cristal_spict_admission_info = cristal_spict_admission_info %>% mutate(clinical_team_fclty = paste(clinical_team,fclty_name,sep="_"),
                     clinical_team_fclty = factor(clinical_team_fclty,levels=unique(clinical_team_fclty),labels=1:length(unique(clinical_team_fclty))))

save(cristal_spict_admission_info, 
     excluded_on_censoring, 
     duplicated_screening, 
     discharges, 
     trial_public_readmissions, 
     outstanding_records, 
     file='analysis_ready/trial_outcomes_data.rda')
