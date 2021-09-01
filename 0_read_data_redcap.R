# read the data from redcap using the export function from REDCap
# Use all three versions of REDCAP databases
# August 2021
library(readxl)
library(tidyr)
library(dplyr)
options(dplyr.summarise.inform=FALSE)  # turn off annoying warning
library(stringr)
library(janitor) # for variable names
setwd("U:/Research/Projects/ihbi/aushsi/interact/Rstuff")
source('98_survival_function.R') # for survival
source('99_functions.R')
load('data/date_changes.RData') # key date & date/time changes for stepped-wedged design from 0_date_changes.R

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
    select(-ends_with('_complete')) # need this now
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
      mutate(urn = as.character(urn)) %>%
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

# c) remove patients where the admission was before the re-start
to_remove = filter(all_data, redcap_version==3, redcap_event_name == 'baseline_screening_arm_1') %>%
  mutate(admission_date = as.Date(admission_date)) %>%
  select(participant_id, admission_date) %>%
  filter(admission_date < as.Date('2020-05-25')) %>%
  pull(participant_id)
#
cat('There were ', length(to_remove), ' removed as their admission was before the re-start.\n', sep='')
all_data = filter(all_data, !participant_id %in% to_remove)


## Section 1: split into data sets that match REDCap forms ##
# to do, convert covid19_date, but very few so far
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
                  start_date_time_dcharg = as.POSIXct(as.numeric(start_date_time_dcharg), origin='1970-01-01', tz='Australia/Brisbane'),
                  end_date_time_dcharg = as.POSIXct(as.numeric(end_date_time_dcharg), origin='1970-01-01', tz='Australia/Brisbane'),
                  hosp_discharge_date = as.POSIXct(as.numeric(hosp_discharge_date), origin='1970-01-01', tz='Australia/Brisbane'))

## b) baseline
baseline = filter(all_data, redcap_event_name_factor == 'Baseline screening') %>%
  select('participant_id','urn','hospital','redcap_version','pt_age','pt_sex_factor',
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
         'presence_eol_plan_factor','eol_plan_name',
         'int_hosp_trans' # new variable (20-11-2020), just RBWH Vascular
         ) %>%
  mutate(
    pt_sex_factor = droplevels(pt_sex_factor), # drop `unknown`
    int_hosp_trans = factor(int_hosp_trans, levels=1:3, labels=c('Yes',"No",'Unknown')),
    # tidy up URN:
    urn = str_remove_all(urn, pattern='^   |^  |^ '), # remove starting spaces
    urn = ifelse(str_detect(urn, pattern='^[A-Z|a-z]')==TRUE, str_sub(urn, 2, nchar(urn)), urn), # remove starting letter
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
    cristal_score = ifelse(redcap_version==2, cristal_score, total_cristal_score)) %>%
  select(-cristal_stroke) %>% # remove stroke; just this one because of data error
  # renaming
  rename('age' = "pt_age") %>%
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) %>% # remove _factor from variable names
  select(-hour, -min, -total_cristal_score) # drop calculated variables and duplicates

# calculate times for form completion; need to do now because need median.form date for spict threshold
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
  summarise(median_form = median(datetime), # median time, excludes outliers
            range = max(datetime)-median(datetime)) %>% # look for unusual variance
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
# consistently format dates
baseline = mutate(baseline,
         admission_datetime = as.POSIXct(as.numeric(admission_datetime), origin='1970-01-01', tz='Australia/Brisbane'),
         median_form = as.POSIXct(as.numeric(median_form), origin='1970-01-01', tz='Australia/Brisbane'))
# everyone should have a date, so should be nobody
missing_date = filter(baseline, is.na(median_form))
if(nrow(missing_date)>0){
  cat('Warning missing date for ', missing_date$participant_id,'.\n',sep='')
}

# at risk, so cristal or spict positive - this is time-dependent, fixed August 2021
source('0_make_at_risk_dates.R')
baseline = mutate(baseline,
                  date_seen = coalesce(admission_datetime, median_form), # use date transferred to this team, this was the time they were at risk from - sometimes missing; use median_form for those where this is missing
                  date_seen = as.Date(format(date_seen, '%Y-%m-%d')), 
      t1 = paste('GCUH - ', gcuh_team, sep=''),  # add single team variable
      t2 = paste('RBWH - ', rbwh_team, sep=''),
      t3 = paste('TPCH - ', tpch_team, sep=''),
      team = '',
      team = ifelse(is.na(gcuh_team)==FALSE, t1, team),
      team = ifelse(is.na(rbwh_team)==FALSE, t2, team),
      team = ifelse(is.na(tpch_team)==FALSE, t3, team)) %>%
  select(-t1, -t2, -t3) %>% # no longer needed
  left_join(time_dependent_threshold, by=c('team','date_seen')) %>% # merge in time-dependent SPICT threshold by team and date
  mutate(at_risk = pmax(cristal_score >= 6, spict_score >= spict_threshold), # mistake (fixed 18-Nov-2020, cristal was at 5)
         at_risk = factor(at_risk, levels=0:1, labels=c('Not at risk','At risk'))) 

# tidy up URN, had to do using loop
for (k in 1:nrow(baseline)){
  baseline$urn[k]= ifelse(str_detect(baseline$urn[k], pattern='-')==TRUE, 
                          str_sub(baseline$urn[k], 1, str_locate_all(baseline$urn[k], pattern='-')[[1]][1]-1), baseline$urn[k]) # remove visit number from some URNs
}
# check for conversion errors, this flags mis-entered URNs; there are none for version 3
z = baseline$urn
x = as.numeric(baseline$urn)
z[is.na(x)]
baseline = mutate(baseline, urn = as.numeric(urn)) # convert to number for matching

# convert form completion time differences to numbers
baseline = mutate(baseline, 
                  time_dem = (as.numeric(end_date_time_dem) - as.numeric(start_date_time_dem))/60, # time in minutes
                  time_hosp = (as.numeric(end_date_time_hosp) - as.numeric(start_date_time_hosp))/60, # time in minutes
                  time_funct = (as.numeric(end_date_time_funct) - as.numeric(start_date_time_funct))/60,
                  time_comorb = (as.numeric(end_date_time_comorb) - as.numeric(start_date_time_comorb))/60) %>%
  select(-starts_with('start_date_time'), -starts_with('end_date_time')) # do not need these date/time variables any more

# add complete to baseline and change at-risk if baseline not complete
completed = filter(complete, screening_completion_complete == 'Complete')
baseline = mutate(baseline, 
                  baseline_screening_complete = as.numeric(participant_id %in% completed$participant_id),
                  baseline_screening_complete = ifelse(continue_456=='Yes', 1, baseline_screening_complete), # also if continue to forms 456 is positive
                  baseline_screening_complete = ifelse(is.na(baseline_screening_complete)==TRUE, 0, baseline_screening_complete), # replace missing
                  baseline_screening_complete = factor(baseline_screening_complete, levels=0:1, labels=c('No','Yes'))) %>%
  mutate(at_risk = ifelse(baseline_screening_complete=="No", 1, at_risk), # not at risk if baseline screening not complete (likely left hospital); changed 12-dec-2020
         at_risk = factor(at_risk, levels=1:2, labels=c('Not at risk','At risk'))) # have to reformat factor
# minor change to at-risk
baseline = mutate(baseline, at_risk = ifelse(baseline_screening_complete=="No", 1, at_risk), # not at risk if baseline screening not complete (likely left hospital); changed 12-dec-2020
                  at_risk = factor(at_risk, levels=1:2, labels=c('Not at risk','At risk'))) # have to reformat factor

#### add intervention dates to baseline based on admission and form dates ###
baseline = int_time(baseline) # see 99_functions.R
int_time_data = select(baseline, participant_id, int_time) # smaller data with just intervention time (used below)

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
         care_review_type = factor(care_review_type),
         care_review_type_other = ifelse(care_review_type_other=='', NA, care_review_type_other)) %>% # make missing
  filter(!is.na(care_review))  # remove records that are essentially missing
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
                             start_date_time_4 = as.POSIXct(as.numeric(start_date_time_4), origin='1970-01-01', tz='Australia/Brisbane'),
                             end_date_time_4 = as.POSIXct(as.numeric(end_date_time_4), origin='1970-01-01', tz='Australia/Brisbane'),
                             time_care_review = as.POSIXct(as.numeric(time_care_review), origin='1970-01-01', tz='Australia/Brisbane'))
clinicianled_review = left_join(clinicianled_review, int_time_data, by='participant_id') # add categorical intervention time
# now get the survival data (function takes a little while)
surv_data_both = make_survival_times( # from 98_survival_function.R
  indata = baseline,
  date_changes_time = date_changes_time,
  form = clinicianled_review ,
  change_var = 'care_review',
  outcome_date = 'time_care_review',  # date/time review occurred
  form_date = 'start_date_time_4',
  at_risk_date = 'admission_datetime') 
# make both versions of the survival data (with and without discharge censoring)
survival_data1 = survival_data2 = NULL # start with NULL
v1 = mutate(surv_data_both$version1, outcome = 'care_review')
v2 = mutate(surv_data_both$version2, outcome = 'care_review')
survival_data1 = bind_rows(survival_data1, v1)
survival_data2 = bind_rows(survival_data2, v2)

# ii) care directive
# do not include 'datediff_care_directive', as calculated using function instead
care_directive = filter(all_data, redcap_repeat_instrument == 'care_directive_measure') %>%
  select('participant_id','hospital','redcap_version','change_5_factor',
         'start_date_time_5','end_date_time_5','date_time_5',
         "type_care_directive_1_factor", "type_care_directive_2_factor",
         "type_care_directive_3_factor", "type_care_directive_4_factor", "type_care_directive_5_factor",
         "type_care_directive_6_factor", "type_care_directive_7_factor", "type_care_directive_8_factor",
         'care_directive_other','tracker_factor') %>%
  mutate(care_directive_other = ifelse(care_directive_other=='', NA, care_directive_other)) %>% # make empty missing
  rename_at(vars(ends_with("_factor")),funs(str_replace(.,"_factor",""))) %>% # remove _factor from variable names
  filter(!is.na(change_5))  # remove records that are essentially missing
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
                        start_date_time_5 = as.POSIXct(as.numeric(start_date_time_5), origin='1970-01-01', tz='Australia/Brisbane'),
                        end_date_time_5 = as.POSIXct(as.numeric(end_date_time_5), origin='1970-01-01', tz='Australia/Brisbane'),
                        date_time_5 = as.POSIXct(as.numeric(date_time_5), origin='1970-01-01', tz='Australia/Brisbane'))
care_directive = left_join(care_directive, int_time_data, by='participant_id') # add categorical intervention time
# now get the survival data (function takes a little while)
surv_data_both = make_survival_times(
  indata = baseline,
  date_changes_time = date_changes_time,
  form = care_directive ,
  change_var = 'change_5',
  form_date = 'start_date_time_5',
  outcome_date = 'date_time_5', 
  at_risk_date = 'admission_datetime')  # assuming at risk from admission
# make both versions of the survival data (with and without discharge censoring)
v1 = mutate(surv_data_both$version1, outcome = 'care_directive')
v2 = mutate(surv_data_both$version2, outcome = 'care_directive')
survival_data1 = bind_rows(survival_data1, v1)
survival_data2 = bind_rows(survival_data2, v2)

# iii) palliative_care_referral
# do not use 'datediff_pallcare', it is calculated in REDCap; calculated time using time_to_first function instead
palliative_care_referral = filter(all_data, redcap_repeat_instrument == 'palliative_care_referral') %>%
  select('participant_id','hospital','redcap_version',
         'start_date_time_6','end_date_time_6',
         'pall_care_6_factor','pall_date_6','palliative_care_referral_complete_factor') %>%
  rename('palliative_care_referral_complete' ='palliative_care_referral_complete_factor',
         'pall_care' ='pall_care_6_factor',
         'pall_date' = 'pall_date_6') %>%
  filter(!is.na(pall_care))  # remove records that are essentially missing
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
                                  start_date_time_6 = as.POSIXct(as.numeric(start_date_time_6), origin='1970-01-01', tz='Australia/Brisbane'),
                                  end_date_time_6 = as.POSIXct(as.numeric(end_date_time_6), origin='1970-01-01', tz='Australia/Brisbane'),
                                  pall_date = as.POSIXct(as.numeric(pall_date), origin='1970-01-01', tz='Australia/Brisbane'))
palliative_care_referral = left_join(palliative_care_referral, int_time_data, by='participant_id') # add categorical intervention time
# now make the survival data - time to first referral (function takes a little while)
surv_data_both = make_survival_times(
  indata = baseline,
  date_changes_time = date_changes_time,
  form = palliative_care_referral ,
  change_var = 'pall_care',
  form_date = 'start_date_time_6',
  outcome_date = 'pall_date', 
  at_risk_date = 'admission_datetime') # assuming at risk from admission
#
# make both versions of the survival data (with and without discharge censoring)
v1 = mutate(surv_data_both$version1, outcome = 'palliative_care_referral')
v2 = mutate(surv_data_both$version2, outcome = 'palliative_care_referral')
survival_data1 = bind_rows(survival_data1, v1)
survival_data2 = bind_rows(survival_data2, v2)


# small data set of just at-risk
small = select(baseline, participant_id, at_risk)
# tidy up survival data
# a) version 1 with no competing risk of discharge
survival_data1 = left_join(survival_data1, int_time_data, by='participant_id') %>% # add intervention time (categorical variable to survival data)
  left_join(small, by='participant_id') %>%
  filter(at_risk == 'At risk') %>% # only those at risk
  select(-at_risk) %>% # no longer needed
  mutate(hospital = str_sub(participant_id, 1, 4)) 
# b) version 2 with competing risk of discharge
survival_data2 = left_join(survival_data2, int_time_data, by='participant_id') %>% # add intervention time (categorical variable to survival data)
  left_join(small, by='participant_id') %>%
  filter(at_risk == 'At risk') %>% # only those at risk
  select(-at_risk) %>% # no longer needed
  mutate(hospital = str_sub(participant_id, 1, 4)) 
# August 2021, combined version that uses version 2 for RBWH/GCUH and version 1 for TPCH
survival_data1 = filter(survival_data1, hospital == 'TPCH')
survival_data2 = filter(survival_data2, hospital != 'TPCH')
survival_data = bind_rows(survival_data1, survival_data2)

### any 4,5,6 forms completed to baseline
any_456 = bind_rows(care_directive, palliative_care_referral, clinicianled_review) %>%
  select(participant_id) %>%
  unique() # at least one form
baseline = mutate(baseline,
                  complete_456 = ifelse(participant_id%in%any_456$participant_id, "Yes", "No"))

# get spict/cristal variables and thresholds
source('0_spict_cristal_vars.R')

# save
save(thresholds, data_date, spict.vars, cristal.vars, baseline, complete, 
     survival_data, 
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
