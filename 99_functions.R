# 99_functions.R
# helpful functions for InterACT
# July 2021

# date formating
my.date.format = function(x){y= format(x, "%d %b %Y"); return(y)} 

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
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
  
  # alternative attempt ...
  
  ## get censoring dates and add dates to all patients
  # i) censoring of those still in hospital at change-over time
  # ii) censoring at end of the intervention exposure (by hospital)
  # iii) censoring for those discharged (competing risk)
  load('data/date_changes.RData') # 0_date_changes.R
  complete_d = select(complete, participant_id, hosp_discharge_date) # small data set for merging below
  censor_dates = select(date_changes, hospital, date_intervention, date_post) %>% # keep dates for intervention and end of intervention
    mutate(date_intervention = as.POSIXct(paste(date_intervention,'23:59', tz='Australia/Brisbane')), # make dates into date/times; one minute before midnight for end of day
           date_post = as.POSIXct(paste(date_post,'23:59', tz='Australia/Brisbane'))) %>% 
    right_join(form, by='hospital') %>%
    right_join(complete_d, by='participant_id') %>% # add hospital/team discharge date
    select(participant_id, hosp_discharge_date, date_intervention, date_post) %>%
    tidyr::gather(`date_intervention`,`date_post`, key='change_var', value='form_date', -`participant_id`, -`hosp_discharge_date`) %>% # names to match REDCap data
    unique() %>% # just one set of dates per patient
    filter(form_date < hosp_discharge_date) %>% # only if censoring occurred before discharge
    select(-hosp_discharge_date) # no longer needed

  ## get dates of `yes` and `no` from forms
  yes_no = select(form, participant_id, form_date, outcome_date, change_var) %>%
    filter(!is.na(form_date),
           !is.na(change_var)) # remove small amount with missing data
  
  ## TO DO: in-hospital death date. Only deaths in hospital during the same admission. not collected this data
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
           change_var = ifelse(time_change <= 0 & change_var=='Yes', "Prior", change_var)) %>% # if time change is negative/zero and they had a form, then it happened prior to this admission
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
# standardise by time otherwise comparison is meaningless #
circular_plots = function(indata, var, datetime){
  # simple rename
  names(indata)[names(indata)==var] = 'change_var'
  names(indata)[names(indata)==datetime] = 'outcome_date'
  # create date/time variables
  to_plot = filter(indata, 
                   int_time %in% c('Usual care','Intervention'), # too busy to also plot Establishment  
                   change_var == 'Yes') %>% # just where there was an outcome
    mutate(dow = format(outcome_date, '%w'),
           dow = factor(dow, levels=0:6, labels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')),
           hour = as.numeric(format(outcome_date, '%H')),
           missing = substr(outcome_date, 12, 16) %in% c('00:00','23:59'), # two dummy times for missing
           missing = ifelse(is.na(outcome_date)==TRUE, TRUE, missing),
           hour = ifelse(missing==TRUE, NA, hour)) %>% # blank hour if it's missing
    select(participant_id, outcome_date, int_time, dow, hour, missing, hospital)
  n_missing = sum(to_plot$missing) # number missing
  n_missing_percent = roundz(100*n_missing/nrow(to_plot))
  to_plot = filter(to_plot, missing==FALSE)
  # standardise numbers by days of period; standardise to per week
  load('data/date_changes.RData')
  date_changes = mutate(date_changes, 
                        days_uc = 1 + as.numeric(date_establishment) - as.numeric(date_usual_care),
                        days_int = 1 + as.numeric(date_post) - as.numeric(date_intervention  )) %>%
    select(hospital, starts_with('days_'))
  standard_dow = left_join(to_plot, date_changes, by='hospital') %>%
    group_by(hospital, dow, int_time, days_uc, days_int) %>%
    tally() %>%
    mutate(stan = ifelse(int_time=='Usual care', n/(days_uc/7), n/(days_int/7)),
           hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH'))) %>% 
    ungroup() 
  standard_hour = left_join(to_plot, date_changes, by='hospital') %>%
    group_by(hospital, hour, int_time, days_uc, days_int) %>%
    tally() %>%
    mutate(stan = ifelse(int_time=='Usual care', n/(days_uc/7), n/(days_int/7)),
           hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH'))) %>% 
    ungroup() 
  # day of the week
  week_plot = ggplot(data=standard_dow, aes(x=dow, y=stan, fill=hosp_ordered))+ # order by intervention time
    ggtitle('Day of week')+
    geom_histogram(stat='identity')+
    scale_fill_manual('Hospital', values=cbPalette)+
    coord_polar()+
    xlab('')+
    ylab('Numbers per week')+
    theme_bw()+
    theme(legend.position = 'top') + 
    facet_wrap(~int_time)
  # hour of the day
  hour_plot = ggplot(data=standard_hour, aes(x=hour, y=stan, fill=hosp_ordered))+
    ggtitle('Hour of day')+
    geom_histogram(stat='identity')+
    scale_fill_manual('Hospital', values=cbPalette)+
    coord_polar()+
    scale_x_continuous(limits=c(0,24), breaks=c(0,6,12,18))+
    xlab('')+
    ylab('Numbers per week')+
    theme_bw()+
    theme(legend.position = 'top') + 
    facet_wrap(~int_time)
  ret = list()
  ret$week = week_plot
  ret$hour = hour_plot
  ret$missing_hour = n_missing
  ret$missing_hour_percent = n_missing_percent
  return(ret)
}

# add intervention time, used by 0_read_data.R and 1_interact_summary.Rmd
int_time = function(indata){
  load('data/date_changes.RData') # from 0_date_changes.R
  outdata = left_join(indata, date_changes, by='hospital') %>%
  mutate(
    date_seen = coalesce(admission_datetime, median_form), # use date transferred to this team, this was the time they were at risk from - sometimes missing; use median_form for those where this is missing
    date_seen = as.Date(format(date_seen, '%Y-%m-%d')), 
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
  return(outdata)  
}

# nice rename for variables in multiple variable models
nice_rename = function(invar){
  invar = case_when(
    str_detect(invar, pattern='age.5') == TRUE ~ "Age (+5 years)",
    str_detect(invar, pattern='^age') == TRUE ~ "Age (+5 years)",
    str_detect(invar, pattern='^pt_sex=Female') == TRUE ~ "Sex (Female)",
    str_detect(invar, pattern='^pt_sex$') == TRUE ~ "Sex (Female)",
    str_detect(invar, pattern='pt_sexFemale') == TRUE ~ "Sex (Female)",
    str_detect(invar, pattern='pt_sexUnknown') == TRUE ~ "Sex (Unknown)",
    str_detect(invar, pattern='int_timeIntervention') == TRUE ~ "Intervention",
    str_detect(invar, pattern='int.time.n$') == TRUE ~ "Intervention",
    str_detect(invar, pattern='^arm$') == TRUE ~ "Intervention", # for survival difference
    str_detect(invar, pattern='in_hoursTRUE') == TRUE ~ "In-hours",
    str_detect(invar, pattern='^cristal_score$') == TRUE ~ "CriSTAL score",
    str_detect(invar, pattern='^spict_score$') == TRUE ~ "SPICT score",
    str_detect(invar, pattern='^int_time_n.cristal_score$') == TRUE ~ "CriSTAL x Intervention",
    str_detect(invar, pattern='^int_time_n.spict_score$') == TRUE ~ "SPICT x Intervention",
    str_detect(invar, pattern='^int_time_n...cristal_score$') == TRUE ~ "CriSTAL x Intervention",
    str_detect(invar, pattern='^int_time_n...spict_score$') == TRUE ~ "SPICT x Intervention",
    
    TRUE ~ invar
  )
  return(invar)
}

### make longitudinal survival data with in-hours versus out-of-hours
# in hours = 8am to 4pm Monday to Friday (based on MERT study at RBHW)
weekend_time = function(indata){
  long_data = NULL
  # loop through each patient
  for (k in 1:nrow(indata)){
    this = indata[k,]
    in_minutes = data.frame(datetime = seq(this$datetime_1, this$datetime_2, 60)) %>%# progress in minutes
      mutate(day = as.numeric(format(datetime, '%u')), # 1 = Monday
             hour = as.numeric(format(datetime, '%H')),
             in_hours = day<=5 & hour>=8 & hour<=15) %>% # in hours 8:00am to 3:59pm or not weekend
      select(-day, -hour) # no longer needed
    
    # now find time changes
    index = which(diff(in_minutes$in_hours)!=0)
    index = c(1, index, nrow(in_minutes)) # at first and last observations
    rows = in_minutes[index,] %>% # pick out key times
      mutate(dummy = 1,
             start = 0,
             end = (as.numeric(datetime) - as.numeric(datetime[1]))/(60*60*24), # time in fraction of days
             event = 0)
    # now make survival times start and end times
    rows$start[2:nrow(rows)] = rows$end[1:(nrow(rows)-1)]
    rows$event[nrow(rows)] = 1 # only last row has the event
    rows = filter(rows, end > 0) 
    # add patient-level data
    to_add = select(this, -event, -starts_with('datetime')) %>%
      mutate(dummy = 1)
    rows = left_join(to_add, rows, by='dummy') %>%
      mutate(event = event* as.numeric(this$event=='Yes')) # event only on last day and if 'Yes'
    # concatenate 
    long_data = bind_rows(long_data, rows)
  }
  
  #
  return(long_data)
}


### make survival data using calendar time
calendar_time = function(indata, changes){
  # start of usual care (start time in all three hospitals)
  ref_datetime = ISOdatetime(year=2020, month=5, day=25, hour=0, min=0, sec=1, tz='Australia/Brisbane')
  ref_datetime = as.numeric(ref_datetime)
  
  # remove establishment phase (added August 2021) - need to adjust back before plotting
  changes = select(changes, hospital, date_intervention) # slim down
  outdata = left_join(indata, changes, by='hospital') %>%
    mutate(index = ifelse(as.Date(datetime_1) >= date_intervention, 1, 0),
           datetime_1 = ifelse(index==1, datetime_1 - (28 * 60*60*24), datetime_1 ), # knock off four weeks (establishment) to avoid gap; original data on date/time scale
           datetime_2 = ifelse(index==1, datetime_2 - (28 * 60*60*24), datetime_2 ))
  
  # calculate difference from usual care date/time (after adjusting for establishment)
  outdata = mutate(outdata, 
                   start = (as.numeric(datetime_1) - ref_datetime)/(60*60*24),
                   end = (as.numeric(datetime_2) - ref_datetime)/(60*60*24))
    
  #
  return(outdata)
}

# function to get establishment dates as days in calendar time
e_dates = function(changes){
  ref_date = data.frame(start = as.Date(ISOdate(year=2020, month=5, day=25)))
  changes = select(changes, hospital, date_intervention) %>% # slim down
    merge(ref_date, by=NULL) %>% # merge to all rows
    mutate(cut = as.numeric(date_intervention - start)) %>%
    select(hospital, cut)
  return(changes)
}
  

## function to calculate risk difference for survival models, with bootstrap CIs
risk_diff = function(
  indata = NULL,
  inmodel = NULL, # survival model, used to get formula
  dp = 2, # decimal places
  B = 100, # number of boostrap statistics
  pred_time = 0 # time to predict difference at
){
  # see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015956/
  # find largest team
  tab = table(indata$team)
  tab = tab[order(-tab)]
  largest = names(tab[1])
  # adjust formula
  char_formula = as.character(inmodel$call)
  old = 'I\\(int_time == \"Intervention\"\\)'
  char_formula = str_replace(char_formula, pattern=old, replacement = 'int_time_n')
  new_formula = str_replace(string=char_formula[2], pattern='strata', replacement='strat')
  #paste(c(char_formula[2], '~', str_replace(string=char_formula[3], pattern='strata', replacement='strat')), collapse=' ') # rms uses strat no strata
  # fit cumulative model
  #crr_model =   
  # refit Cox model using rms package
  cox_rms = cph(as.formula(new_formula), data = indata, surv=TRUE, x=TRUE, y=TRUE)
  # set up predictions, age at median, female, largest team
  pred_data = data.frame(int_time_n = c(0,1), pt_sex = 'Female', age=84, spict_score=3, cristal_score=5, team=largest)
  pred_sur = survest(cox_rms, newdata=pred_data, times=pred_time)
  # bootstrap intervals
  boots <- boot(data = indata,
                  statistic = rdnnt, # see other function
                  R = B, # number of bootstraps
                  frm = as.formula(new_formula),
                  pred_time = pred_time,
                  pred_data = pred_data)
  ci = boot.ci(boots, conf=0.95, type= 'norm') 
  ci = as.numeric(ci$normal[2:3]) # just extract CIs
  #
  preds = as.data.frame(pred_sur) %>%
    mutate(
      Event = roundz(1 - surv, dp), # 1 minus to give those experiencing event
      lower = roundz(1 - lower, dp),
      upper = roundz(1 - upper, dp),
      CI = paste(lower, ' to ', upper, sep=''),
      Phase = c('Usual care','Intervention')) %>%
    select(Phase, Event, CI)
  # difference
  frame = data.frame(Phase = 'Difference', 
                     Event = roundz(diff(pred_sur$surv)*(-1), dp), # -1 to give intervention minus usual care
                     CI = paste(roundz(ci[1],dp), ' to ', roundz(ci[2],dp), sep=''))
  to_table = bind_rows(preds, frame)
  return(to_table)
}

## function for bootstrap confidence intervals for absolute risk difference in Cox model
# adapted from https://atm.amegroups.com/article/viewFile/18926/pdf
rdnnt <- function(data, 
                  ii, 
                  frm, # formula 
                  pred_time, # time to predict at
                  pred_data) # data frame for predictions
{
  dd <- data[ii,]; # allows boot to select a sample
  cfit <- cph(formula = frm, data=dd,
              surv=TRUE, x=TRUE, y=TRUE);
  pred_sur = survest(cfit, newdata=pred_data, times=pred_time)
  RD <- diff(pred_sur$surv)*(-1) # risk difference (intervention minus usual care)
  #cat('.'); # updater
  return(RD);
}

## function to calculate adjusted restricted mean survival difference
rmst_diff = function(
  indata = NULL,
  pred_time = 0 # time to predict difference at
){
  
  # prepare data for rmst function
for_rmst = mutate(indata,
                  status = as.numeric(event=='Yes'),
                  arm = int_time_n - 1) # 0 = usual care, 1 = intervention
# set up covariate matrix
x = data.frame(for_rmst[, c('age','pt_sex')]) %>%
  mutate(
    age = age / 5, # standardise
    pt_sex = as.numeric(pt_sex == 'Female')) # function does not like factors
# run function
res = rmst2(
  time = for_rmst$time,
  status = for_rmst$status, 
  arm = for_rmst$arm,
  tau = pred_time,
  covariates = x
)
# tidy up estimates
ests = data.frame(res$RMST.difference.adjusted) %>%
  clean_names()
ests$var = row.names(ests); row.names(ests) = NULL
ests = filter(ests, var != 'intercept') %>%
  select(-se_coef, -z)

return(ests)

} # end of function


### function for cristal/spict interaction

score_interaction = function(indata){
  ## a) Standard Cox model stratified on team within hospital
  review_sensitivity = coxph(Surv(time = time, event = event== 'Yes') ~ I(age/5) + pt_sex + int_time_n +  cristal_score + spict_score + cristal_score:int_time_n + spict_score:int_time_n + strata(team), data = indata)
# nice table
ests = tidy(review_sensitivity, conf.int = TRUE) %>%
  filter(!is.na(estimate)) %>%
  mutate(
    Variable = nice_rename(term), # see 99_functions
    HR = roundz(exp(estimate),2),
    conf.low = roundz(exp(conf.low),2),
    conf.high = roundz(exp(conf.high),2),
    CI = paste(conf.low, ' to ' , conf.high, sep='')) %>%
  select(Variable, HR, CI)

## b) had to refit in rms instead of coxph for predict function
# prepare data
for_model_cph = mutate(indata, 
            age5 = (age - 75)/5, # centre and scale
            spict_score = spict_score - 3, # subtract median
            cristal_score = cristal_score - 5) # 

# refit model
cox_model_sensitivity_cph = cph(Surv(time = time, event = event== 'Yes') ~ age5 + pt_sex + int_time_n +  cristal_score + spict_score + cristal_score:int_time_n + spict_score:int_time_n + strat(team), data = for_model_cph)
# get predictions
preds1 = Predict(cox_model_sensitivity_cph, 
                 age5 = 0, # 75
                 pt_sex = 'Female',
                 int_time_n = c(1,2),
                 cristal_score = c(0,1), # one unit increase from median
                 spict_score = 0, # median
                 fun = exp) # for HR
preds2 = Predict(cox_model_sensitivity_cph, 
                 age5 = 0, # 75
                 pt_sex = 'Female',
                 int_time_n = c(1,2),
                 cristal_score = 0, # median
                 spict_score = c(0,1), # one unit increase from median
                 fun = exp) # for HR
# prepare for plot with separate panels for scores
preds1 = data.frame(preds1) %>%
  rename('score' = 'cristal_score') %>%
  select(-spict_score)
preds2 = data.frame(preds2) %>%
  rename('score' = 'spict_score') %>%
  select(-cristal_score)
for_plot = bind_rows(preds1, preds2, .id='facet') %>%
  select(-age5, -pt_sex, -team) %>%
  mutate(yhat = as.numeric(yhat),
         int_time_n = ifelse(score == 0, int_time_n - 0.05, int_time_n + 0.05), # jitter
         facet = ifelse(facet==1, 'CriSTAL', 'SPICT')) 
# plot
palette = RColorBrewer::brewer.pal(n=2, name='Dark2')
gplot = ggplot(data=for_plot, aes(x=int_time_n, y=yhat, ymin=lower, ymax=upper, col=factor(score))) +
  geom_hline(lty=2, yintercept=1, col='grey66')+
  geom_point(size=4) +
  geom_errorbar(width=0, size=1.05)+
  geom_line(size=1.05) +
  scale_color_manual('SPICT or CriSTAL score', values=palette, labels=c('Median', 'Median + 1')) +
  scale_x_continuous(breaks=c(1,2), labels=c('Usual\ncare','Intervention'))+
  ylab('Hazard ratio') +
  xlab('')+
  g.theme +
  theme(legend.position = 'top',
        panel.spacing = unit(2, "lines"))+ # increase gap between panels
  facet_wrap(~facet)

# return tables and plots
to_return = list()
to_return$ests = ests
to_return$plot = gplot
to_return$model = cox_model_sensitivity_cph # for VIF
return(to_return)

}

## function to scramble baseline hazard
scramble_hazard = function(indata){
  d = diff(indata$hazard) # differences between hazards
  d = d[d>=0] # only positive changes
  cum = 0 
  for(k in 2:nrow(indata)){
    if(indata$hospital[k-1] != indata$hospital[k]){cum=0; next}
    cum = cum + sample(d, size=1)
    indata$hazard[k] = cum
  }
  return(indata)
} # end of function

# function to make zero cells in tables
zeros = function(x){
  y = ifelse(x=='', '0 (0)', x)
  y = ifelse(is.na(x)==TRUE, '0 (0)', x)
  return(y)
}

### function to create a plot of the difference in the baseline hazard over calendar time for plotting
base_haz_difference = function(inbase){
  # interpolate diagonal reference line - add after scrambling
  startend = group_by(inbase, hospital) %>%
    arrange(time) %>%
    mutate(row = row_number()) %>%
    filter(row==1 | row==n()) %>% # first and last
    mutate(hstart = lag(hazard),
           tstart = lag(time),
           tdiff = time - lag(time), 
           diff = hazard - lag(hazard),
           delta = diff/tdiff) %>% 
    filter(!is.na(tdiff)) %>%
    select(hospital, hstart, tstart, delta) 
  #
  diff = full_join(inbase, startend, by=c('hospital')) %>%
    group_by(hospital) %>%
    mutate(diagonal = hstart + (time-tstart)*delta,
           diff = hazard - diagonal,
           hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH')))
  return(diff)
}