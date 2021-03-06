# 0_read_data.R
# read the data from redcap using the export function from REDCap
# Use all three versions of REDCAP databases
# September 2020
library(readxl)
library(tidyr)
library(dplyr)
options(dplyr.summarise.inform=FALSE)  # turn off annoying warning
library(stringr)
library(janitor) # for variable names
setwd("U:/Research/Projects/ihbi/aushsi/interact/Rstuff")
source('99_functions.R')
load('data/date_changes.RData') # key date changes from 0_date_changes.R

## How to get data in:
# data exports from REDCap using R
# click "removed all tagged identifiers"
# untick "Hash the record ID"
# untick both to remove 'free form text' fields
# have to comment out `rm(list=ls())` in first line of .r files produced by REDCap

## Section 0: read the exported data from REDCap ##
# move to data sub-folder
setwd('data') #
d1 = dir('V1', pattern='.r$') # what data sets are there (check each version folder)
d2 = dir('V2', pattern='.r$') # 
d3 = dir('V3', pattern='.r$') # 
d = c(d1, d2, d3)
gc = sum(str_count(d, 'GCUH'))
rbwh = sum(str_count(d, 'RBWH'))
tpch = sum(str_count(d, 'TPCH'))
(hosps = c(gc, rbwh, tpch)) # should be three for each hospital
# check for duplicates
all_data = NULL
for (this in d){
  hosp =  str_remove_all(this, 'V3|V2|_R|_|[0-9]|InterACT|\\.r|-') # extract hospital name from file name
  version = ifelse(str_detect(this, 'V2')==TRUE, 2, 1)
  version = ifelse(str_detect(this, 'V3')==TRUE, 3, version)
  # move to version folder
  setwd(paste('V', version, sep=''))
  source(this) # source the R file exported by REDCap
  setwd('..') # move back to data folder
  data = clean_names(data) %>%
    mutate(hospital = hosp,
           redcap_version = version) %>%
    select(-ends_with('_complete'))
  # remove labels as this was causing errors with slightly different labels in concatenation
  data = mutate_all(data, clear.labels)
  ## fix team variable
  # gcuh
  if(hosp=='GCUH' & version==2){
    data = select(data, -rbwh_team) %>%
      rename('gcuh_team_factor' = 'rbwh_team_factor') # wrong hospital name
  }
  if(hosp=='GCUH' & version==3){
    data = select(data, -team) %>%
      rename('gcuh_team_factor' = 'team_factor')
  }
  # tpch
  if(hosp=='TPCH' & version==2){
    data = select(data, -rbwh_team) %>%
      rename('tpch_team_factor' = 'rbwh_team_factor') # wrong hospital name
  }
  if(hosp=='TPCH' & version==3){
    data = select(data, -team) %>%
      rename('tpch_team_factor' = 'team_factor')
  }
  # rbwh
  if(hosp=='RBWH' & version==3){
    data = select(data, -team) %>%
      rename('rbwh_team_factor' = 'team_factor')
  }
  # concatenate
  all_data = bind_rows(all_data, data)
}
# get latest file date (use version 3)
setwd('V3')
dates = file.info(dir())$ctime
setwd('../..') # move back to project folder
data_date = as.Date(max(dates))
# check forms
table(all_data$redcap_event_name_factor)
table(all_data$redcap_repeat_instrument)
table(all_data$redcap_version)
table(all_data$cristal_stroke_factor) # check error (fixed below)
table(all_data$gcuh_team_factor, all_data$redcap_version) # check change
table(all_data$rbwh_team_factor, all_data$redcap_version) # check change
table(all_data$tpch_team_factor, all_data$redcap_version) # check change
# discharge variables check; may have changed by version?
table(all_data$discharge_type_factor, all_data$redcap_version) # 
table(all_data$discharge_forms_factor, all_data$redcap_version) # 
table(all_data$discharge_hosp_factor, all_data$redcap_version) # 

# drop unused levels from TPCH team
levels = c('General Medicine','Respiratory','Orthopaedics')
all_data = mutate(all_data, tpch_team_factor = as.factor(as.character(tpch_team_factor, levels=levels)))

## Section 0: initial cleans ##
# a) remove duplicate IDs (only in version 1 or 2)
id.complete = read_excel('data/REDCap screening IDs.xlsx', sheet=1, skip=1) %>% # ID numbers of completed baseline screening
  gather(key='where', value='participant_id') %>%
  filter(!is.na(participant_id)) %>%  # remove missing rows
  mutate(hospital = str_sub(where,1,4),
         redcap_version = as.numeric(str_sub(where,7,8)))
with(id.complete, table(hospital, redcap_version))       
# here are the doubles
id.doubles = read_excel('data/REDCap screening IDs.xlsx', sheet=2, skip=1) %>% # ID numbers of doubles in old and new
  mutate(excel_row = 1:n()) %>%
  gather(key='where', value='participant_id', -`excel_row`) %>%
  filter(!is.na(participant_id)) %>%  # remove missing rows
  mutate(hospital = str_sub(where,1,4),
         redcap_version = as.numeric(str_sub(where,7,8))) %>%
  select(-where) %>%
  group_by(excel_row, hospital) %>%
  spread(redcap_version, participant_id) %>%
  ungroup()
# now can knock out all v1 patients
knock.out = select(id.doubles, -excel_row, -`2`) %>%
  mutate(redcap_version=1) %>%
  rename('participant_id' = `1`)
all_data = anti_join(all_data, knock.out, by=c('participant_id','hospital','redcap_version'))

# b) remake ID to include hospital and version
all_data = mutate(all_data, participant_id = paste(hospital, '_V', redcap_version, '_', participant_id, sep='')) # to avoid overlapping numbers

## Section 1: split into data sets that match REDCap forms ##
## a) study completion
complete = filter(all_data, redcap_event_name_factor == 'Study completion') %>%
  select("participant_id",'hospital','redcap_version',
         'start_date_time_dcharg','end_date_time_dcharg','total_spict_cristal_pipe',
         'discharge_type_factor','discharge_forms_factor','discharge_hosp_factor',
         'hosp_discharge_date','discharge_factor','covid19_factor','covid19_date',
         'screening_completion_complete_factor') %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) # remove _factor from variable names
for (k in 1:nrow(complete)){
  complete$hosp_discharge_date[k] = ifelse(complete$hosp_discharge_date[k]=='', NA, as.POSIXct(complete$hosp_discharge_date[k], tz='Australia/Brisbane'))
  complete$start_date_time_dcharg[k] = ifelse(complete$start_date_time_dcharg[k]=='', NA, as.POSIXct(complete$start_date_time_dcharg[k], tz='Australia/Brisbane'))
  complete$end_date_time_dcharg[k] = ifelse(complete$end_date_time_dcharg[k]=='', NA, as.POSIXct(complete$end_date_time_dcharg[k], tz='Australia/Brisbane'))
}
# work on dates
complete = mutate(complete,
                  time_dcharg = (as.numeric(end_date_time_dcharg) - as.numeric(start_date_time_dcharg))/60,
                  start_date_time_dcharg = as.POSIXct(as.numeric(start_date_time_dcharg), origin='1970-01-01'),
                  end_date_time_dcharg = as.POSIXct(as.numeric(end_date_time_dcharg), origin='1970-01-01'),
                  hosp_discharge_date = as.POSIXct(as.numeric(hosp_discharge_date), origin='1970-01-01'))

# b) baseline
baseline = filter(all_data, redcap_event_name_factor == 'Baseline screening') %>%
  select('participant_id','hospital','redcap_version','pt_age','pt_sex_factor',
         'admission_date',
         'continue_456_factor', # changed on 17 March, used to be "AND" for both scores
         ends_with('_team_factor'), # teams per hospital
         starts_with('start_date_time'), starts_with('end_date_time'),
         'spict_unplan_hosp_factor', # only use factor versions of variables as they are neater
         'spict_care_others_factor',
         'spict_perform_status_factor',
         'spict_weight_loss_factor',
         "spict_persist_sympt_factor",
         "spict_care_focus_factor",
         'spict_score',
         'cristal_admit_ed_factor',
         'cristal_admit_source_factor',
         'cristal_previous_admit_factor',
         'cristal_icu_factor',
         'cristal_icu_current_factor',
         # 'cristal_cfs_factor', # "Has a Clinical Frailty Scale already been completed for this patient at this admission?" - not needed
         'cristal_cfs_score', # 
         contains('cristal_score'), # all the scores
         'cristal_cancer_factor',
         'cristal_proteinuria_factor',
         'cristal_ckd_factor',
         'cristal_ecg_factor',
         'cristal_ami_factor',
         'cristal_chf_factor',
         'cristal_copd_factor',
         'cristal_stroke', # not factor, see fix below
         'cristal_cognitive_factor',
         'cristal_liver_factor',
         "cristal_gcs_factor",
         "cristal_sbp_factor",
         "cristal_resp_factor",
         "cristal_hr_factor",
         "cristal_02_factor",
         "cristal_bgl_factor",
         "cristal_seizures_factor",
         "cristal_urine_factor",
         'presence_eol_plan_factor','eol_plan_name') %>%
  mutate(
    # admission time and date:
    hour = as.numeric(str_sub(admission_date, 12, 13)), # extract from text
    min = as.numeric(str_sub(admission_date, 15, 16)),
    admission_datetime = admission_date, # also keep date/time
    admission_time = hour + (min/60),
    admission_date = as.Date(str_sub(admission_date, 1, 10)),
    # fix REDCap error in stroke response
    cristal_stroke_factor = factor(cristal_stroke, levels=1:3, labels=c('Yes',"No",'Unknown')),
    # clinical frailty score
    cristal_cfs_score = ifelse(cristal_cfs_score==1, NA, cristal_cfs_score), # 1 is unknown ...
    cristal_cfs_score = cristal_cfs_score - 1, # ... now shift scores down by 1
    # cristal score (changed by version)
    cristal_score = ifelse(redcap_version==2, cristal_score, total_cristal_score),
    # at risk, so cristal or spict positive
    at_risk = pmax(cristal_score>=5, spict_score>=2),
    at_risk = factor(at_risk, levels=0:1, labels=c('Not at risk','At risk'))) %>% 
  select(-cristal_stroke) %>% # remove stroke; just this one because of data error
  # renaming
  rename('age' = "pt_age") %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) %>% # remove _factor from variable names
  select(-hour, -min, -total_cristal_score) # drop calculated variables and duplicates
# calculate times for form completion
for (k in 1:nrow(baseline)){
  baseline$admission_datetime[k] = ifelse(baseline$admission_datetime[k]=='', NA, as.POSIXct(baseline$admission_datetime[k], tz='Australia/Brisbane'))
  baseline$start_date_time_dem[k] = ifelse(baseline$start_date_time_dem[k]=='', NA, as.POSIXct(baseline$start_date_time_dem[k], tz='Australia/Brisbane'))
  baseline$end_date_time_dem[k] = ifelse(baseline$end_date_time_dem[k]=='', NA, as.POSIXct(baseline$end_date_time_dem[k], tz='Australia/Brisbane'))
  baseline$start_date_time_hosp[k] = ifelse(baseline$start_date_time_hosp[k]=='', NA, as.POSIXct(baseline$start_date_time_hosp[k], tz='Australia/Brisbane'))
  baseline$end_date_time_hosp[k] = ifelse(baseline$end_date_time_hosp[k]=='', NA, as.POSIXct(baseline$end_date_time_hosp[k], tz='Australia/Brisbane'))
  baseline$start_date_time_clinical[k] = ifelse(baseline$start_date_time_clinical[k]=='', NA, as.POSIXct(baseline$start_date_time_clinical[k], tz='Australia/Brisbane'))
  baseline$end_date_time_clinical[k] = ifelse(baseline$end_date_time_clinical[k]=='', NA, as.POSIXct(baseline$end_date_time_clinical[k], tz='Australia/Brisbane'))
  baseline$start_date_time_funct[k] = ifelse(baseline$start_date_time_funct[k]=='', NA, as.POSIXct(baseline$start_date_time_funct[k], tz='Australia/Brisbane'))
  baseline$end_date_time_funct[k] = ifelse(baseline$end_date_time_funct[k]=='', NA, as.POSIXct(baseline$end_date_time_funct[k], tz='Australia/Brisbane'))
  baseline$start_date_time_comorb[k] = ifelse(baseline$start_date_time_comorb[k]=='', NA, as.POSIXct(baseline$start_date_time_comorb[k], tz='Australia/Brisbane'))
  baseline$end_date_time_comorb[k] = ifelse(baseline$end_date_time_comorb[k]=='', NA, as.POSIXct(baseline$end_date_time_comorb[k], tz='Australia/Brisbane'))
}
## add screening start time by taking earliest date from REDCap forms
form_dates = select(baseline, participant_id, 
                   # start_date_time_hosp, # do not use hospital form , this was sometimes completed long after other forms
                   start_date_time_dem,
                    start_date_time_clinical,
                    start_date_time_funct,
                    start_date_time_comorb) %>%
  gather(key='form', value='datetime', -`participant_id`) %>%
  mutate(datetime = as.numeric(datetime)) %>% # convert to number
  filter(!is.na(datetime))  # remove missing
median = group_by(form_dates, participant_id) %>% # 
  summarise(median_form = min(datetime), # median time, excludes outliers
            range = max(datetime)-min(datetime)) %>% # look for unusual variance
  ungroup()
## check form dates ##
check = filter(median, range > 5*24*60*60) %>% # longer than 5 days between earliest and latest form
  left_join(form_dates, by='participant_id') %>%
  mutate(datetime = as.POSIXct(datetime, origin='1970-01-01', tz='Australia/Brisbane'),
         date = as.Date(datetime),
         form = str_remove(form, pattern='^start_date_time_')) %>% # simplify to date (drop time)
  select(-median_form, -datetime, -range) %>% # not needed
  spread(form, date)
write.csv(check, file='checks/form_dates.csv', row.names = FALSE, quote=FALSE)
# add median date to baseline data
baseline = left_join(baseline, median, by='participant_id') %>%
  select(-range) # no longer needed
# everyone should have a date, so should be nobody
missing_date = filter(baseline, is.na(median_form))

# convert form completion time differences to numbers
baseline = mutate(baseline, 
                  time_dem = (as.numeric(end_date_time_dem) - as.numeric(start_date_time_dem))/60, # time in minutes
                  time_hosp = (as.numeric(end_date_time_hosp) - as.numeric(start_date_time_hosp))/60, # time in minutes
                  time_funct = (as.numeric(end_date_time_funct) - as.numeric(start_date_time_funct))/60,
                  time_comorb = (as.numeric(end_date_time_comorb) - as.numeric(start_date_time_comorb))/60,
) 

# add complete to baseline
completed = filter(complete, screening_completion_complete == 'Complete')
baseline = mutate(baseline, 
                  baseline_screening_complete = as.numeric(participant_id %in% completed$participant_id),
                  baseline_screening_complete = ifelse(continue_456=='Yes', 1, baseline_screening_complete), # also if continue to forms 456 is positive
                  baseline_screening_complete = ifelse(is.na(baseline_screening_complete)==TRUE, 0, baseline_screening_complete), # replace missing
                  baseline_screening_complete = factor(baseline_screening_complete, levels=0:1, labels=c('No','Yes')))

# consistently format dates
baseline = select(baseline, -starts_with('start_date_time'), -starts_with('end_date_time')) %>% # do not need these date/time variables
  mutate(admission_datetime = as.POSIXct(as.numeric(admission_datetime), origin='1970-01-01'),
         median_form = as.POSIXct(as.numeric(median_form), origin='1970-01-01'),
         # add single team variable
         t1 = paste('GCUH - ', gcuh_team, sep=''),
         t2 = paste('RBWH - ', rbwh_team, sep=''),
         t3 = paste('TPCH - ', tpch_team, sep=''),
         team = '',
         team = ifelse(is.na(gcuh_team)==FALSE, t1, team),
         team = ifelse(is.na(rbwh_team)==FALSE, t2, team),
         team = ifelse(is.na(tpch_team)==FALSE, t3, team)
         ) %>%
  select(-t1, -t2, -t3) # no longer needed

#### add intervention time to baseline ###
baseline = int_time(baseline) # see 99_functions.R


## c) follow-up data ##
# i) clinician led review, do not include 'datediff_clinician_review' as calculated using function instead
clinicianled_review = filter(all_data, redcap_repeat_instrument == 'clinicianled_review_discussion') %>%
  select('participant_id','hospital','redcap_version',
         'start_date_time_4','end_date_time_4',
         'care_review_factor','time_care_review',
         'care_review_type_factor','care_review_type_other','care_review_conflict_factor') %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) %>% # remove _factor from variable names
  mutate(care_review_type = as.character(care_review_type), # fix one category label
         care_review_type = ifelse(care_review_type=='patient and clinician only', 'Patient and clinician only', care_review_type),
         care_review_type = factor(care_review_type))
# format date/times
for (k in 1:nrow(clinicianled_review)){
  clinicianled_review$start_date_time_4[k] = ifelse(clinicianled_review$start_date_time_4[k]=='', NA, as.POSIXct(clinicianled_review$start_date_time_4[k], tz='Australia/Brisbane'))
  clinicianled_review$end_date_time_4[k] = ifelse(clinicianled_review$end_date_time_4[k]=='', NA, as.POSIXct(clinicianled_review$end_date_time_4[k], tz='Australia/Brisbane'))
  clinicianled_review$time_care_review[k] = ifelse(clinicianled_review$time_care_review[k]=='', NA, as.POSIXct(clinicianled_review$time_care_review[k], tz='Australia/Brisbane'))
}
# work on dates
clinicianled_review = mutate(clinicianled_review,
                             # calculate time taken to complete form
                             time_4 = (as.numeric(end_date_time_4) - as.numeric(start_date_time_4))/60,
                             # format date/times
                             start_date_time_4 = as.POSIXct(as.numeric(start_date_time_4), origin='1970-01-01'),
                             end_date_time_4 = as.POSIXct(as.numeric(end_date_time_4), origin='1970-01-01'),
                             time_care_review = as.POSIXct(as.numeric(time_care_review), origin='1970-01-01'))
# time to first review
clinicianled_review = time_to_first(form = clinicianled_review, 
                                    change_var = 'care_review', # yes/no variable that determines event of interest
                                    outcome_date = 'time_care_review',  # date/time review occurred
                                    form_date = 'start_date_time_4', # date form was completed
                                    at_risk_date = 'admission_datetime') # could be start_date_time_comorb, but assuming at risk from admission
# add intervention time
int_time_data = select(baseline, participant_id, int_time) # data with just intervention time
clinicianled_review = left_join(clinicianled_review, int_time_data, by='participant_id')

# ii) care directive
# do not include 'datediff_care_directive', as calculated using function instead
care_directive = filter(all_data, redcap_repeat_instrument == 'care_directive_measure') %>%
  select('participant_id','hospital','redcap_version','change_5_factor',
         'start_date_time_5','end_date_time_5','date_time_5',
         "type_care_directive_1_factor", "type_care_directive_2_factor",
         "type_care_directive_3_factor", "type_care_directive_4_factor", "type_care_directive_5_factor",
         "type_care_directive_6_factor", "type_care_directive_7_factor", "type_care_directive_8_factor",
         'care_directive_other','tracker_factor') %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) # remove _factor from variable names
for (k in 1:nrow(care_directive)){
  care_directive$start_date_time_5[k] = ifelse(care_directive$start_date_time_5[k]=='', NA, as.POSIXct(care_directive$start_date_time_5[k], tz='Australia/Brisbane'))
  care_directive$date_time_5[k] = ifelse(care_directive$date_time_5[k]=='', NA, as.POSIXct(care_directive$date_time_5[k], tz='Australia/Brisbane'))
  care_directive$end_date_time_5[k] = ifelse(care_directive$end_date_time_5[k]=='', NA, as.POSIXct(care_directive$end_date_time_5[k], tz='Australia/Brisbane'))
}
# work on dates
care_directive = mutate(care_directive,
                        # calculate time to complete form
                        time_5 = (as.numeric(end_date_time_5) - as.numeric(start_date_time_5))/60,
                        # format date/times
                        start_date_time_5 = as.POSIXct(as.numeric(start_date_time_5), origin='1970-01-01'),
                        end_date_time_5 = as.POSIXct(as.numeric(end_date_time_5), origin='1970-01-01'),
                        date_time_5 = as.POSIXct(as.numeric(date_time_5), origin='1970-01-01'))
care_directive =  time_to_first(form = care_directive, 
                                change_var = 'change_5', 
                                outcome_date = 'date_time_5', 
                                form_date = 'start_date_time_5',
                                at_risk_date = 'admission_datetime') # could be start_date_time_comorb, but assuming at risk from admission
care_directive = left_join(care_directive, int_time_data, by='participant_id')

## checks ##
# 1) check negative dates (lots!) #
check = filter(care_directive, time_change < 0 ) %>%
  select(participant_id, at_risk_date, outcome_date, time_change) %>%
  mutate(at_risk_date = as.POSIXct(as.numeric(at_risk_date), origin='1970-01-01'))
# 2) check for patients with two "Yes" outcomes, they do exist #
check = filter(care_directive, change_var=='Yes') %>%
  group_by(participant_id) %>%
  filter(n()>1)

# iii) palliative_care_referral
# do not use 'datediff_pallcare', it is calculated in REDCap; calculated time using time_to_first function instead
palliative_care_referral = filter(all_data, redcap_repeat_instrument == 'palliative_care_referral') %>%
  select('participant_id','hospital','redcap_version',
         'start_date_time_6','end_date_time_6',
         'pall_care_6_factor','pall_date_6','palliative_care_referral_complete_factor') %>%
  rename('palliative_care_referral_complete' ='palliative_care_referral_complete_factor',
         'pall_care' ='pall_care_6_factor',
         'pall_date' = 'pall_date_6')
for (k in 1:nrow(palliative_care_referral)){
  palliative_care_referral$start_date_time_6[k] = ifelse(palliative_care_referral$start_date_time_6[k]=='', NA, as.POSIXct(palliative_care_referral$start_date_time_6[k], tz='Australia/Brisbane'))
  palliative_care_referral$pall_date[k] = ifelse(palliative_care_referral$pall_date[k]=='', NA, as.POSIXct(palliative_care_referral$pall_date[k], tz='Australia/Brisbane'))
  palliative_care_referral$end_date_time_6[k] = ifelse(palliative_care_referral$end_date_time_6[k]=='', NA, as.POSIXct(palliative_care_referral$end_date_time_6[k], tz='Australia/Brisbane'))
}
# work on dates
palliative_care_referral = mutate(palliative_care_referral,
                                  # calculate time to complete form
                                  time_6 = (as.numeric(end_date_time_6) - as.numeric(start_date_time_6))/60,
                                  # format date/times
                                  start_date_time_6 = as.POSIXct(as.numeric(start_date_time_6), origin='1970-01-01'),
                                  end_date_time_6 = as.POSIXct(as.numeric(end_date_time_6), origin='1970-01-01'),
                                  pall_date = as.POSIXct(as.numeric(pall_date), origin='1970-01-01'))
# time to first referral
palliative_care_referral = time_to_first(form = palliative_care_referral, 
                                         change_var = 'pall_care', 
                                         outcome_date = 'pall_date', 
                                         form_date = 'start_date_time_6',
                                         at_risk_date = 'admission_datetime') # could be start_date_time_comorb, but assuming at risk from admission
palliative_care_referral = left_join(palliative_care_referral, int_time_data, by='participant_id')

## consistently format all dates/times ##
care_directive = mutate(care_directive,
                        at_risk_date = as.POSIXct(as.numeric(at_risk_date), origin='1970-01-01'))
clinicianled_review = mutate(clinicianled_review,
                             at_risk_date = as.POSIXct(as.numeric(at_risk_date), origin='1970-01-01'))
palliative_care_referral = mutate(palliative_care_referral,
                                  at_risk_date = as.POSIXct(as.numeric(at_risk_date), origin='1970-01-01'))

### any 4,5,6 forms completed to baseline
any_456 = bind_rows(care_directive, palliative_care_referral, clinicianled_review) %>%
  select(participant_id) %>%
  unique() # at least one form
baseline = mutate(baseline,
                  complete_456 = ifelse(participant_id%in%any_456$participant_id, "Yes", "No"))

## vector of SPICT and CRISTAL variables
# SPICT
spict.vars = c("spict_unplan_hosp",
               "spict_perform_status",
               "spict_care_others",
               "spict_weight_loss",
               "spict_persist_sympt",
               "spict_care_focus"  )
# Cristal, don't include 'age' because everyone has it
cristal.vars = c('cristal_admit_ed','cristal_admit_source',
                 'cristal_previous_admit','cristal_icu',
                 'cristal_cfs_score','cristal_cancer','cristal_proteinuria',
                 'cristal_ckd','cristal_ecg','cristal_ami','cristal_chf',
                 'cristal_copd','cristal_stroke','cristal_cognitive','cristal_liver',
                 "cristal_gcs","cristal_sbp", "cristal_resp", "cristal_hr",
                 "cristal_02", "cristal_bgl", "cristal_seizures", "cristal_urine")

## thresholds
thresholds = list()
# add the score thresholds (positive if on or above)
thresholds$cristal_threshold = 6
thresholds$spict_threshold = 2
# these two thresholds (although 1 is unknown in REDCap, the score has been adjusted in 0_read_data)
thresholds$frailty_threshold = 5
thresholds$frailty_threshold2 = 7 # second threshold added in May 2020

# save
save(thresholds, data_date, spict.vars, cristal.vars, baseline, complete, 
     care_directive, palliative_care_referral, clinicianled_review, file='data/FullData.RData')

# names(baseline)[grep('cristal_score', names(baseline))]

### Make list of teams, for use by 1_create_multiple_reports_teams.R
teams = select(baseline, hospital, contains('team')) %>%
  unique() %>%
  mutate(
    team = '',
    team = ifelse(is.na(gcuh_team)==FALSE, as.character(gcuh_team), team),
    team = ifelse(is.na(rbwh_team)==FALSE, as.character(rbwh_team), team),
    team = ifelse(is.na(tpch_team)==FALSE, as.character(tpch_team), team)) %>%
  filter(team != '') %>% # small number with no team
  select(hospital, team)
# add all teams and all hospitals
all_teams = data.frame(hospital = c('GCUH','RBWH','TPCH'), team='AllTeams')
all_hospitals = data.frame(hospital = c('All'), team='AllTeams')
teams = bind_rows(teams, all_teams, all_hospitals)
save(teams, file='data/teams.RData')

