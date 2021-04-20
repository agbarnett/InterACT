# 99_functions.R
# helpful functions for InterACT
# August 2020

# function for rounding numbers with zeros kept
roundz = function(x, digits){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# rename for SPICT variables (version with and without line breaks)
nice.rename.spict = function(x, line_breaks=FALSE){
  if(line_breaks==FALSE){
y = case_when(
  x == 'spict_unplan_hosp' ~ 'Unplanned hospital',
  x == 'spict_perform_status' ~ 'Performance poor status',
  x == 'spict_care_others' ~ 'Depends on others for care',
  x == 'spict_weight_loss' ~ 'Weight loss',
  x == 'spict_persist_sympt' ~ 'Persistent symptoms',
  x == 'spict_care_focus' ~ 'Palliative care request')
  }
  if(line_breaks==TRUE){
    y = case_when(
      x == 'spict_unplan_hosp' ~ 'Unplanned\nhospital adm',
      x == 'spict_perform_status' ~ 'Performance\npoor status',
      x == 'spict_care_others' ~ 'Depends on\nothers for\ncare',
      x == 'spict_weight_loss' ~ 'Weight\nloss',
      x == 'spict_persist_sympt' ~ 'Persistent\nsymptoms',
      x == 'spict_care_focus' ~ 'Palliative\ncare request')
  }
return(y)  
}


# rename for CRISTAL variables (version with and without line breaks)
nice.rename.cristal = function(x, line_breaks=FALSE){
  if(line_breaks==FALSE){
    y = case_when(
    x == 'cristal_admit_ed' ~ 'Admitted via ED',
    x == 'cristal_admit_source' ~ 'From nursing home',
    x == 'age' ~ 'Age over 75', # not needed as all over 75, kept just in case
    x == 'pt_age' ~ 'Age over 75', # not needed as all over 75, kept just in case
    x == 'cristal_previous_admit' ~ 'Previous hospitalisation',
    x == 'cristal_icu' ~ 'ICU admission',
    x == 'cristal_cfs_score' ~ 'Frailty score', ## overall CFS score
    x == 'cristal_cfs_score1' ~ 'Frailty score >= 5', ## lower threshold
    x == 'cristal_cfs_score2' ~ 'Frailty score >= 7', ## upper threshold
    x == "cristal_cancer" ~ 'Advanced cancer',
    x == "cristal_proteinuria"  ~ 'Proteinuria',
    x == "cristal_ckd"  ~ 'Chronic kidney disease',
    x == "cristal_ecg" ~ 'Abnormal ECG',
    x == "cristal_ami" ~ 'Acute myocardial infarction',
    x == "cristal_chf"  ~ 'Chronic heart failure',          
    x == "cristal_copd" ~ 'COPD',
    x == "cristal_stroke" ~ 'Stroke',
    x == "cristal_cognitive" ~ 'Cognitive impairment',     
    x == "cristal_liver" ~ 'Liver disease',
    x == "cristal_gcs" ~ 'Glasgow Coma Scale',
    x == "cristal_sbp" ~ 'Systolic blood pressure',
    x == "cristal_resp" ~ 'Respiratory rate',
    x == "cristal_hr" ~ 'Pulse rate',
    x == "cristal_02" ~ 'Oxygen required or saturation < 90%',
    x == "cristal_bgl" ~ 'Hypoglycaemia',
    x == "cristal_seizures" ~ 'Seizures',
    x == "cristal_urine" ~ 'Low urine output'
    )}
  if(line_breaks==TRUE){
    y = case_when(
      x == 'cristal_admit_ed' ~ 'Admitted\nvia ED',
      x == 'cristal_admit_source' ~ 'From nursing\nhome',
      x == 'age' ~ 'Age over\n75', # not needed as all over 75, kept just in case
      x == 'pt_age' ~ 'Age over\n75', # not needed as all over 75, kept just in case
      x == 'cristal_previous_admit' ~ 'Previous\nhospitalisation',
      x == 'cristal_icu' ~ 'ICU\nadmission',
      x == 'cristal_cfs_score' ~ 'Frailty\nscore', ## overall CFS score
      x == 'cristal_cfs_score1' ~ 'Frailty score\n>= 5', ## lower threshold
      x == 'cristal_cfs_score2' ~ 'Frailty score\n>= 7', ## upper threshold
      x == "cristal_cancer" ~ 'Advanced\ncancer',
      x == "cristal_proteinuria"  ~ 'Proteinuria',
      x == "cristal_ckd"  ~ 'Chronic kidney\ndisease',
      x == "cristal_ecg" ~ 'Abnormal\nECG',
      x == "cristal_ami" ~ 'Acute\nmyocardial\ninfarction',
      x == "cristal_chf"  ~ 'Chronic heart\nfailure',          
      x == "cristal_copd" ~ 'COPD',
      x == "cristal_stroke" ~ 'Stroke',
      x == "cristal_cognitive" ~ 'Cognitive\nimpairment',     
      x == "cristal_liver" ~ 'Liver\ndisease',
      x == "cristal_gcs" ~ 'Glasgow Coma\nScale',
      x == "cristal_sbp" ~ 'Systolic blood\npressure',
      x == "cristal_resp" ~ 'Respiratory\nrate',
      x == "cristal_hr" ~ 'Pulse\nrate',
      x == "cristal_02" ~ 'Oxygen required\nor saturation\n< 90%',
      x == "cristal_bgl" ~ 'Hypogly-\ncaemia',
      x == "cristal_seizures" ~ 'Seizures',
      x == "cristal_urine" ~ 'Low urine\noutput'
    )}
  return(y)  
}

# rename for CRISTAL variables using acronyms
nice.rename.cristal.acronyms = function(x){
  y = case_when(
    x == 'cristal_admit_ed' ~ 'Admitted via ED',
    x == 'cristal_admit_source' ~ 'From nursing home',
    x == 'age' ~ 'Age over\n75',
    x == 'pt_age' ~ 'Age over\n75',
    x == 'cristal_previous_admit' ~ 'Previous hospitalisation',
    x == 'cristal_icu' ~ 'ICU admission',
    x == 'cristal_cfs_score' ~ 'Frailty score', ## overall CFS score
    x == 'cristal_cfs_score1' ~ 'Frailty score >= 5', ## lower threshold
    x == 'cristal_cfs_score2' ~ 'Frailty score >= 7', ## upper threshold
    x == "cristal_cancer" ~ 'Advanced cancer',
    x == "cristal_proteinuria"  ~ 'Proteinuria',
    x == "cristal_ckd"  ~ 'CKD', # acronym
    x == "cristal_ecg" ~ 'Abnormal ECG',
    x == "cristal_ami" ~ 'AMI', # acronym
    x == "cristal_chf"  ~ 'CHF', # acronym
    x == "cristal_copd" ~ 'COPD',
    x == "cristal_stroke" ~ 'Stroke',
    x == "cristal_cognitive" ~ 'Cognitive impairment',     
    x == "cristal_liver" ~ 'Liver disease',
    x == "cristal_gcs" ~ 'GCS', # acronym
    x == "cristal_sbp" ~ 'SBP', # acronym
    x == "cristal_resp" ~ 'Respiratory rate',
    x == "cristal_hr" ~ 'Pulse rate',
    x == "cristal_02" ~ 'O2 required or saturation < 90%',
    x == "cristal_bgl" ~ 'Hypoglycaemia',
    x == "cristal_seizures" ~ 'Seizures',
    x == "cristal_urine" ~ 'Low urine output'
  )
  
  return(y)  
}

# rename for care directive
nice.rename.directive = function(x){
  y = case_when(
    x == 'type_care_directive_1' ~	'Active treatment reduced or ceased',
    x == 'type_care_directive_2' ~	'Active treatment continued',
    x == 'type_care_directive_3' ~	'Active treatment increased',
    x == 'type_care_directive_4' ~	'Comfort care initiated or increased',
    x == 'type_care_directive_5' ~	'Resuscitation plan completed or updated',
    x == 'type_care_directive_6' ~	'Advance Care Plan completed or initiated',
    x == 'type_care_directive_7' ~	'Statement of choices completed or initiated',
    x == 'type_care_directive_8' ~	'Other'
  )
  return(y)  
}

# list of names in order (used to arrange results on geom_bar)
care_directives = c('Active treatment reduced or ceased', 'Active treatment continued', 
'Active treatment increased', 'Comfort care initiated or increased', 
'Resuscitation plan completed or updated', 'Advance Care Plan completed or initiated', 
'Statement of choices completed or initiated', 'Other')

# remove Hmisc labels (used by 0_read_data.R)
# https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in seq_along(x)) {
      class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
      attr(x[[i]],"label") <- NULL
    } 
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

## find time to first change accounting for censoring (outcome defined in protocol), used by 0_read_data.R
time_to_first = function(form, 
                         change_var, # yes/no variable that determines if event of interest occurred
                         outcome_date, # date/time that outcome happened
                         form_date, # date/time that form was started
                         at_risk_date, # date/time to start survival from
                         random.check=TRUE ){
  # rename variables to make function generic
  form = rename(form,
          'change_var' = as.character(change_var),
          'outcome_date' = as.character(outcome_date),
          'form_date' = as.character(form_date)) %>%
    # if outcome time is missing '00:00' then set to the end of the day
    mutate(is.missing = stringr::str_sub(outcome_date, 12, 16)=='00:00',
           outcome_date = as.character(outcome_date),
           outcome_date = ifelse(is.missing==TRUE, paste(str_sub(outcome_date, 1, 10), '23:59:00'), outcome_date),
           outcome_date = as.POSIXct(outcome_date)) %>%
    select(-is.missing) # no longer needed
  
  #### make a list of events per patients, include 'yes', 'no', 'censored' and 'dead' ####
  # TO DO, add date of death once it is available  
  
  ## get censoring dates and add dates to all patients
  # i) censoring of those still in hospital at change-over time
  # ii) censoring at end of the intervention exposure (by hospital)
  load('data/date_changes.RData') # 0_date_changes.R
  complete_d = select(complete, participant_id, hosp_discharge_date) # small data set for merging below
  censor_dates = select(date_changes, hospital, date_intervention, date_post) %>% # keep dates for intervention and end of intervention
    mutate(date_intervention = as.POSIXct(paste(date_intervention,'23:59', tz='Australia/Brisbane')), # make dates into date/times; one minute before midnight for end of day
           date_post = as.POSIXct(paste(date_post,'23:59', tz='Australia/Brisbane'))) %>% 
    right_join(form, by='hospital') %>%
    right_join(complete_d, by='participant_id') %>% # add team discharge date
    select(participant_id, hosp_discharge_date, date_intervention, date_post) %>%
    tidyr::gather(`date_intervention`,`date_post`, key='change_var', value='form_date', -`participant_id`, -`hosp_discharge_date`) %>% # names to match REDCap data
    unique() %>% # just one set of dates per patient
    filter(form_date < hosp_discharge_date) %>% # only if censoring occurred before discharge
    select(-hosp_discharge_date) # no longer needed

  ## get dates of `yes` and `no` from forms
  yes_no = select(form, participant_id, form_date, outcome_date, change_var) %>%
    filter(!is.na(form_date),
           !is.na(change_var)) # remove small amount with missing data
  
  ## in-hospital death date, to do. Only deaths in hospital during the same admission. not collected this data
  # death_date = select()
  
  # find first yes
  first_yes = filter(yes_no, change_var=='Yes') %>%
    group_by(participant_id) %>%
    arrange(form_date) %>% # earliest form completed
    slice(1) %>% # first per patient
    ungroup()
  # find first censored
  first_censored = filter(censor_dates, change_var %in% c('date_intervention','date_post')) %>%
    group_by(participant_id) %>%
    arrange(form_date) %>% # earliest form completed
    slice(1) %>% # first per patient
    ungroup() %>%
    mutate(change_var = 'Censored')
  # find last no or in-hospital death per patient
  last_no = filter(yes_no, change_var %in% c('No','Unknown','Dead')) %>%
    group_by(participant_id) %>%
    arrange(desc(form_date)) %>% # latest form date
    slice(1) %>% # last per patient (last time we were sure it was `no` or censored)
    select(-outcome_date) %>% # remove date and time ...
    mutate(outcome_date = form_date) %>% # replace with form time (for censoring)
    filter(!participant_id %in% unique(first_yes$participant_id), # not any patient with a yes, because 'yes' always beats 'no'
           !participant_id %in% unique(first_censored$participant_id)) # not any patient with a censored, because 'censored' always beats 'no'
  
  ## combine all dates (add death date here if it becomes available)
  all_dates = bind_rows(first_yes, last_no, first_censored) %>%
    arrange(participant_id, form_date) %>%
    group_by(participant_id) %>%
    slice(1) %>% # first event
    ungroup()
  
  ## now merge dates with other key information 
  # get admission date/time (used for start of survival analysis); get "at risk" binary variable
  at_risk = select(baseline, participant_id, at_risk, as.character(at_risk_date)) %>%
    rename('at_risk_date' = as.character(at_risk_date))
  # merge additional information
  final_data = left_join(all_dates, at_risk, by='participant_id') %>%
    filter(at_risk == 'At risk') %>% # only at risk (should already be done, but done again for safety)
    mutate(time_change = (as.numeric(outcome_date) - as.numeric(at_risk_date))/(60*60*24), # in days
           # if date is negative (within 6 hours) then it happened at time of admission:  
           outcome_date = ifelse(time_change < 0 & time_change >= -(2/24) & change_var=='Yes', at_risk_date, outcome_date), # 
           outcome_date = as.POSIXct(as.numeric(outcome_date), origin='1970-01-01'), # format
           time_change = ifelse(time_change < 0 & time_change >= -(2/24) & change_var=='Yes', 0, time_change), # also update time change
           # if date is negative then it happened before this admission:  
           change_var = as.character(change_var), # from factor to character
           change_var = ifelse(time_change < 0 & change_var=='Yes', "Prior", change_var)) %>% # if time change is negative and they had a form, then it happened prior to this admission
    select(-at_risk)  # drop no longer needed
  ## final bespoke steps per outcome
  # i) additional step for prior for outcome 4 only
  if(change_var=='care_review'){
    ## add back  information
    form_details = select(form, 'participant_id','hospital','redcap_version','form_date', starts_with('care_review_'))
    final_data = left_join(final_data, form_details, by=c('participant_id','form_date')) %>%
      unique() # overcome odd error of one patient being merged twice
  }
  
  # ii) additional step for prior for outcome 5 only, will not be needed once variable is added 
  if(change_var=='change_5'){
    
    ## add back care directive information
    form_details = select(form, 'participant_id','hospital','redcap_version','form_date', starts_with('type_care_'), 'care_directive_other','tracker')
    final_data = left_join(final_data, form_details, by=c('participant_id','form_date')) %>%
      unique() # overcome odd error of one patient being merged twice
    ## TO DO, replace this with analysis done by hand ##
    arp_strings = c('arp valid for this admission') # lower-case strings that define prior ARP
    change_date = as.Date('2020-10-01') # date when change was made, will need to update
    old_data = filter(final_data,
                    as.Date(form_date) < change_date) %>%
    mutate(index = time_change==0, # date of form and at-risk date (admission) are the same
           index = ifelse(is.na(index), FALSE, index), # remove missing dates (count as "No")
           strings = str_detect(string=tolower(care_directive_other), pattern=arp_strings),
           change_var = ifelse(index==TRUE & strings==TRUE & type_care_directive_8=='Checked', 'Prior', change_var))
    new_data = filter(final_data,
                    as.Date(form_date) >= change_date)
    # put two data sets back together
    final_data = bind_rows(old_data, new_data)
  }
  # output list where ARP strings look good but time change is not zero  - to do
  
  # iii) additional step for prior for palliative care only, will not be needed once variable is added 
  if(change_var=='pall_care'){
    
    # 
    any_prior = filter(form, change_var=='Known to Pall Care') # 
    # first add at risk
    any_prior = left_join(any_prior, at_risk, by='participant_id') %>%
      filter(at_risk == 'At risk') %>% # only at risk (should already be done, but done again for safety)
      mutate(time_change = NA, # no outcome data
             change_var = 'Prior') %>% # defined as prior
      select("participant_id","form_date","outcome_date","change_var","at_risk_date","time_change" ) # same variables as final data
    # concatenate with yes/no data         
    final_data = bind_rows(final_data, any_prior)
    
    ## add back information
    form_details = select(form, 'participant_id','hospital','redcap_version','form_date') # not much to add
    final_data = left_join(final_data, form_details, by=c('participant_id','form_date')) %>%
      unique() # overcome odd error of one patient being merged twice
    
  }
  
  # make change var into a factor
  final_data = mutate(final_data, change_var = factor(change_var, levels=c('Prior','Yes','No','Censored')))
    
  # optional random check
  if(random.check==TRUE){
    check = select(final_data, participant_id, change_var, at_risk_date, outcome_date, time_change) %>%
      sample_n(5) %>% # randomly select 5
      mutate(at_risk_date = as.POSIXct(as.numeric(at_risk_date), origin='1970-01-01'))
    print(check) # 
  }
  return(final_data)
  
} # end of function

#### function to create circular plots of days and times ###
circular_plots = function(indata){
  # create date/time variables
  to_plot = filter(indata, change_var == 'Yes') %>% # just where there was an outcome
    mutate(dow = format(outcome_date, '%w'),
           dow = factor(dow, levels=0:6, labels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')),
           hour = as.numeric(format(outcome_date, '%H')),
           missing = substr(outcome_date, 12, 16) %in% c('00:00','23:59'), # two dummy times for missing
           missing = ifelse(is.na(outcome_date)==TRUE, TRUE, missing),
           hour = ifelse(missing==TRUE, NA, hour)) %>% # blank hour if it's missing
    select(participant_id, outcome_date, dow, hour, missing)
  n_missing = sum(to_plot$missing)
  to_plot = filter(to_plot, missing==FALSE)
  # day of the week
  week_plot = ggplot(data=to_plot, aes(x=dow))+
    ggtitle('Day of week')+
    geom_histogram(stat='count', fill='khaki1')+
    coord_polar()+
    xlab('')+
    ylab('Count')+
    theme_bw()
  # hour of the day
  hour_plot = ggplot(data=to_plot, aes(x=hour))+
    ggtitle('Hour of day')+
      geom_histogram(stat='count', fill='plum2')+
      coord_polar()+
    scale_x_continuous(limits=c(0,24), breaks=c(0,6,12,18))+
      xlab('')+
      ylab('Count')+
    theme_bw()
  ret = list()
  ret$week = week_plot
  ret$hour = hour_plot
  ret$missing_hour = n_missing
  return(ret)
}

# add intervention time, used by 0_read_data.R and 1_interact_summary.Rmd
int_time = function(indata){
  load('data/date_changes.RData') # from 0_date_changes.R
  indata = left_join(indata, date_changes, by='hospital') %>%
  mutate(
    date_seen = as.Date(format(median_form, '%Y-%m-%d')), # use median form date/time
    int_time = case_when(
    date_seen < date_usual_care ~ 0, # just versions 1 and 2, pre-covid
    date_seen >= date_usual_care & date_seen < date_establishment ~ 1,
    date_seen >= date_establishment & date_seen < date_intervention ~ 2,
    date_seen >= date_intervention & date_seen < date_post ~ 3,
    date_seen >= date_post & date_seen < date_end ~ 4,
    TRUE ~ 5 # catch remainder, likely errors
  ),
  int_time = factor(int_time, levels=0:5, labels=c('Pre-COVID','Usual care','Establishment','Intervention','Post-intervention','Error'))) %>%
    select(-'date_usual_care',-'date_establishment',-'date_intervention',-'date_post',-'date_end') # no longer needed
  return(indata)  
}

# nice rename for variables in multiple variable models
nice_rename = function(invar){
  invar = case_when(
    str_detect(invar, pattern='age/5') == TRUE ~ "Age (+5 years)",
    str_detect(invar, pattern='pt_sexFemale') == TRUE ~ "Sex (Female)",
    str_detect(invar, pattern='pt_sexUnknown') == TRUE ~ "Sex (Unknown)",
    str_detect(invar, pattern='int_timeIntervention') == TRUE ~ "Intervention",
    TRUE ~ invar
  )
  return(invar)
}
