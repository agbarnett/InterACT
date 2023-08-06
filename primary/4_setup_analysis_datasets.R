# 4_setup_analysis_datasets.R

source('99_packages.R')
source('99_trial_information.R')
load('analysis_ready/trial_outcomes_data.rda') # from 3_define_outcomes.R


study_start <- as.Date("2020-05-25")

#select variables required
base_vars <- c('study_id','study_period','fclty_name','clinical_team_fclty','age_on_admission','sex','cristal_spict_date_time','discharge_outcome')
binary_vars <- c('icu_flag') #outcome 1
surv_vars <- c('fclty_outcome','length_of_stay_days_c', #outcome 2
               'discharge_to_readmit_days_c','status_readmit','readmit_flag', # outcome 3; added readmit flag to get total numbers
               'screening_to_met_days','status_met') #outcome 7

#extra variables to defined calendar time scale for survival
date_vars <- c('admit_start_date_time','admit_end_date_time_c','cristal_spict_date_time')

# cristal_spict_admission_info = cristal_spict_admission_info %>% mutate(length_of_stay_days = as.numeric(admit_end_date_time - cristal_spict_date_time,'days'),
#                                                                        length_of_stay_days_c = as.numeric(admit_end_date_time_c - cristal_spict_date_time,'days'))
# 


#define dataset for analysis
dat_analysis = select(cristal_spict_admission_info,all_of(c(base_vars,binary_vars,surv_vars,date_vars))) %>%  
  #define status for outcome 2
  mutate(status_los = case_when(fclty_outcome=='Censored'~0,
                            fclty_outcome=='Discharged alive'~1,
                            fclty_outcome=='Died in hospital'~2),
         status_los = factor(status_los,0:2,labels=c('censor','Discharged alive','Died in hospital')),
          #redefine start and stop times on calendar time scale       
         start_los = pmax(0,as.numeric(cristal_spict_date_time - as.POSIXct(study_start),'days')), #truncate at zero - affects 5 records screened just before admission
         stop_los = as.numeric(admit_end_date_time_c - as.POSIXct(study_start),'days'),
         #rename status for outcome 3
         status_readmit = factor(status_readmit,0:2,labels=c('censor','Readmitted to public hospital','Died after hospital discharge')),
         
         #outcome 7 (met)
         status_met = factor(status_met,0:3,labels=c('censor','MET call','Discharged alive','Died in hospital')),
         
         #define fixed effects for the intervention (binary) and age (centered at 85yrs, scaled by 10)
         intervention = case_when(study_period=='Usual care'~0,study_period=='Intervention exposure'~1),
         age_scaled = (age_on_admission - 85)/5,sex=factor(sex,levels=c("Female","Male")),
         #add study week for calendar time effect: take floor to start at week 0
         study_week = floor(as.numeric(date(cristal_spict_date_time) - study_start,'weeks')),
         admit_date = date(admit_start_date_time)) %>%
  #rename met_flag, icu_flag for consistency
  rename('status_icu'=icu_flag) %>%
  #add useful labels to readmit_flag
  mutate_at('readmit_flag',~case_when(.==0 ~ 'Alive, not readmitted to public hospital',.==1 ~ 'Readmitted to public hospital',.==2 ~ 'Died without re-admission to public hospital')) %>%
  #remove duplicate variables
  select(-c(fclty_outcome,study_period,age_on_admission,cristal_spict_date_time,admit_start_date_time))


#add information on intervention start dates; for linear trend models; cluster number for de-identification of hospitals
ad = filter(trial_dates,study_period=='Intervention exposure') %>% mutate(intervention_start_week = as.numeric(start_date - study_start,'weeks')) %>% select(site_name,intervention_start_week,cluster)

dat_analysis = dat_analysis %>% left_join(ad,by=c('fclty_name'='site_name')) %>% mutate(intervention_week = pmax(0,study_week - intervention_start_week))

save(dat_analysis,file='analysis_ready/final_dataset.rda')
