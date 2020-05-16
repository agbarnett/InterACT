# 0_read_data.R
# read the data from redcap using the export function from REDCap
# Use both versions of REDCAP databases
# May 2020
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(janitor) # for variable names
setwd("U:/Research/Projects/ihbi/aushsi/interact/Rstuff")

# data exports from REDCap using R
# click "removed all tagged identifiers"
# untick "Hash the record ID"
# click both to remove 'free form text' fields
# have to comment out `rm(list=ls())` in first line of .r files

# to do? seems okay without
# add this as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),?
# could make team into one variable, e.g. rbwh_team

## Section 0: read the data from REDCap ##
# move to data sub-folder
setwd('data') #
d = dir(pattern='.r$') # what data sets are there
gc = sum(str_count(d, 'GCUH'))
rbwh = sum(str_count(d, 'RBWH'))
tpch = sum(str_count(d, 'TPCH'))
(hosps = c(gc, rbwh, tpch))
# check for duplicates
all_data = NULL
for (this in d){
  source(this) # source the R file exported by REDCap
  hosp =  str_remove_all(this, 'V2|_R|_|[0-9]|InterACT|\\.r|-') # extract hospital name from file name
  version = ifelse(str_detect(this, 'V2')==TRUE, 2, 1)
  data = clean_names(data) %>%
    mutate(hospital = hosp,
           redcap_version = version) %>%
    select(-ends_with('_complete'))
  all_data = bind_rows(all_data, data)
}
setwd('..') # move back
# check forms
table(all_data$redcap_event_name_factor)
table(all_data$redcap_repeat_instrument)

## Section 0: initial cleans ##
# a) remove duplicate IDs
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

## Section 1: split into data sets ##
# a) baseline
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
         'cristal_cfs_factor',
         'cristal_cfs_score', # 6 or above
         contains('cristal_score'), # all the scores
         'cristal_cancer_factor',
         'cristal_proteinuria_factor',
         'cristal_ckd_factor',
         'cristal_ecg_factor',
         'cristal_ami_factor',
         'cristal_chf_factor',
         'cristal_copd_factor',
         'cristal_stroke_factor',
         'cristal_cognitive_factor',
         'cristal_liver_factor',
         'presence_eol_plan_factor','eol_plan_name') %>%
  # admission time and date:
  mutate(hour = as.numeric(str_sub(admission_date, 12, 13)), # extract from text
         min = as.numeric(str_sub(admission_date, 15, 16)),
         admission_time = hour + (min/60),
         admission_date = as.Date(str_sub(admission_date, 1, 10)),
  # clinical frailty score
         cristal_cfs_score = ifelse(cristal_cfs_score==1, NA, cristal_cfs_score), # 1 is unknown ...
         cristal_cfs_score = cristal_cfs_score - 1, # ... now shift scores down by 1
  # cristal score (changed by version)
         cristal_score = ifelse(redcap_version==2, cristal_score, total_cristal_score),
  # at risk, so cristal or spict positive
         at_risk = pmax(cristal_score>=5, spict_score>=2),
         at_risk = factor(at_risk, levels=0:1, labels=c('Not at risk','At risk'))) %>% 
  # renaming
  rename('age' = "pt_age") %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) %>% # remove _factor from variable names
  select(-hour, -min, -total_cristal_score) # drop calculated variables and duplicates
# calculate times for form completion
for (k in 1:nrow(baseline)){
  baseline$start_date_time_dem[k] = ifelse(baseline$start_date_time_dem[k]=='', NA, as.POSIXct(baseline$start_date_time_dem[k], tz='Australia/Brisbane'))
  baseline$end_date_time_dem[k] = ifelse(baseline$end_date_time_dem[k]=='', NA, as.POSIXct(baseline$end_date_time_dem[k], tz='Australia/Brisbane'))
  baseline$start_date_time_hosp[k] = ifelse(baseline$start_date_time_hosp[k]=='', NA, as.POSIXct(baseline$start_date_time_hosp[k], tz='Australia/Brisbane'))
  baseline$end_date_time_hosp[k] = ifelse(baseline$end_date_time_hosp[k]=='', NA, as.POSIXct(baseline$end_date_time_hosp[k], tz='Australia/Brisbane'))
  baseline$start_date_time_funct[k] = ifelse(baseline$start_date_time_funct[k]=='', NA, as.POSIXct(baseline$start_date_time_funct[k], tz='Australia/Brisbane'))
  baseline$end_date_time_funct[k] = ifelse(baseline$end_date_time_funct[k]=='', NA, as.POSIXct(baseline$end_date_time_funct[k], tz='Australia/Brisbane'))
  baseline$start_date_time_comorb[k] = ifelse(baseline$start_date_time_comorb[k]=='', NA, as.POSIXct(baseline$start_date_time_comorb[k], tz='Australia/Brisbane'))
  baseline$end_date_time_comorb[k] = ifelse(baseline$end_date_time_comorb[k]=='', NA, as.POSIXct(baseline$end_date_time_comorb[k], tz='Australia/Brisbane'))
  baseline$start_date_time_4[k] = ifelse(baseline$start_date_time_4[k]=='', NA, as.POSIXct(baseline$start_date_time_4[k], tz='Australia/Brisbane'))
  baseline$end_date_time_4[k] = ifelse(baseline$end_date_time_4[k]=='', NA, as.POSIXct(baseline$end_date_time_4[k], tz='Australia/Brisbane'))
  baseline$start_date_time_5[k] = ifelse(baseline$start_date_time_5[k]=='', NA, as.POSIXct(baseline$start_date_time_5[k], tz='Australia/Brisbane'))
  baseline$end_date_time_5[k] = ifelse(baseline$end_date_time_5[k]=='', NA, as.POSIXct(baseline$end_date_time_5[k], tz='Australia/Brisbane'))
  baseline$start_date_time_6[k] = ifelse(baseline$start_date_time_6[k]=='', NA, as.POSIXct(baseline$start_date_time_6[k], tz='Australia/Brisbane'))
  baseline$end_date_time_6[k] = ifelse(baseline$end_date_time_6[k]=='', NA, as.POSIXct(baseline$end_date_time_6[k], tz='Australia/Brisbane'))
  baseline$start_date_time_dcharg[k] = ifelse(baseline$start_date_time_dcharg[k]=='', NA, as.POSIXct(baseline$start_date_time_dcharg[k], tz='Australia/Brisbane'))
  baseline$end_date_time_dcharg[k] = ifelse(baseline$end_date_time_dcharg[k]=='', NA, as.POSIXct(baseline$end_date_time_dcharg[k], tz='Australia/Brisbane'))
}
# convert time differences to numbers
baseline = mutate(baseline, 
                  time_dem = (as.numeric(end_date_time_dem) - as.numeric(start_date_time_dem))/60, # time in minutes
                  time_hosp = (as.numeric(end_date_time_hosp) - as.numeric(start_date_time_hosp))/60, # time in minutes
                  time_funct = (as.numeric(end_date_time_funct) - as.numeric(start_date_time_funct))/60,
                  time_comorb = (as.numeric(end_date_time_comorb) - as.numeric(start_date_time_comorb))/60,
                  time_4 = (as.numeric(end_date_time_4) - as.numeric(start_date_time_4))/60,
                  time_5 = (as.numeric(end_date_time_5) - as.numeric(start_date_time_5))/60,
                  time_6 = (as.numeric(end_date_time_6) - as.numeric(start_date_time_6))/60,
                  time_dcharg = (as.numeric(end_date_time_dcharg) - as.numeric(start_date_time_dcharg))/60
                  )
# quick checks of times - could just be data issue (longer than 120 minutes or negative time)
problems = select(baseline, "participant_id",'hospital', starts_with('time_')) %>%
  tidyr::gather(value='minutes', key='timevar', -"participant_id", -"hospital") %>%
  filter(minutes<0 | minutes>120) %>%
  left_join(baseline, by=c("participant_id",'hospital')) %>%
  mutate(start = NA,
         start = ifelse(minutes == time_hosp, start_date_time_hosp, start),
         start = ifelse(minutes == time_dem, start_date_time_dem, start),
         start = ifelse(minutes == time_funct, start_date_time_funct, start),
         end = NA,
         end = ifelse(minutes == time_hosp, end_date_time_hosp, end),
         end = ifelse(minutes == time_dem, end_date_time_dem, end),
         end = ifelse(minutes == time_funct, end_date_time_funct, end)) %>%
  select("participant_id",'hospital', 'timevar', 'minutes', 'start', 'end') %>%
  mutate(start = as.POSIXct(as.numeric(start), origin='1970-01-01'), # need to convert to dates and times
         end = as.POSIXct(as.numeric(end), origin='1970-01-01'))
if(nrow(problems)>0){
  write.csv(problems, file='checks/date_checks.csv', quote=FALSE, row.names = FALSE)
}

## b) follow-up
# i) care directive
care_directive = filter(all_data, redcap_repeat_instrument == 'care_directive_measure') %>%
  select('participant_id','hospital','redcap_version','change_5_factor','date_time_5','datediff_care_directive',
         "type_care_directive_1_factor", "type_care_directive_2_factor",
         "type_care_directive_3_factor", "type_care_directive_4_factor", "type_care_directive_5_factor",
         "type_care_directive_6_factor", "type_care_directive_7_factor", "type_care_directive_8_factor",
         'care_directive_other','tracker_factor') %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) # remove _factor from variable names

# ii) clinicianled_review
clinicianled_review = filter(all_data, redcap_repeat_instrument == 'clinicianled_review_discussion') %>%
  filter(care_review==1) %>% # only where a review has occurred
  select('participant_id','hospital','redcap_version','care_review_factor','time_care_review','datediff_clinician_review',
         'care_review_type_factor','care_review_type_other','care_review_conflict_factor') %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) # remove _factor from variable names

# iii) palliative_care_referral
temporary = function(){ # not yet in data
palliative_care_referral = filter(all_data, redcap_repeat_instrument == 'palliative_care_referral') %>%
  select('participant_id','hospital','redcap_version','pall_care_6_factor','pall_date_6','datediff_pallcare','palliative_care_referral_complete') %>%
  rename('pall_care' ='pall_care_6_factor',
         'pall_date' = 'pall_date_6')
}

# c) complete
complete = filter(all_data, redcap_event_name_factor == 'Study completion') %>%
  select("participant_id",'hospital','redcap_version',
         'start_date_time_dcharg','end_date_time_dcharg','total_spict_cristal_pipe',
         'discharge_type_factor','discharge_forms_factor','discharge_hosp_factor',
         'hosp_discharge_date','discharge_factor','covid19_factor','covid19_date',
         'screening_completion_complete_factor') %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) # remove _factor from variable names
# add complete to baseline
completed = filter(complete, screening_completion_complete == 'Complete')
baseline = mutate(baseline, 
                  baseline_screening_complete = as.numeric(participant_id %in% completed$participant_id),
                  baseline_screening_complete = ifelse(continue_456=='Yes', 1, baseline_screening_complete), # also if continue to forms 456 is positive
                  baseline_screening_complete = ifelse(is.na(baseline_screening_complete)==TRUE, 0, baseline_screening_complete), # replace missing
                  baseline_screening_complete = factor(baseline_screening_complete, levels=0:1, labels=c('No','Yes')))

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
                 'cristal_copd','cristal_stroke','cristal_cognitive','cristal_liver')

# final tidy
baseline = select(baseline, -starts_with('start_date_time'), -starts_with('end_date_time')) # do not need these date/time variables

# save
# to add once available: palliative_care_referral
save(spict.vars, cristal.vars, baseline, complete, care_directive, clinicianled_review, file='data/FullData.RData')


# names(baseline)[grep('cristal_score', names(baseline))]