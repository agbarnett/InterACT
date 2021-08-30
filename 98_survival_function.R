# 98_survival_function.R
# make the survival data for the in-hospital outcomes (outcomes 4, 5 and 6)
# surprisingly difficult!
# June 2021

make_survival_times = function(
  indata = baseline,
  date_changes_time = date_changes_time,
  form = clinicianled_review ,
  change_var = 'care_review',
  form_date = 'start_date_time_4', # date/time that form was completed, needed for "no" events
  outcome_date = '', # date/time that outcome happened
  at_risk_date = 'admission_datetime'){
  
  # rename variables to make function generic
  form = rename(form,
                'event' = as.character(change_var),
                'outcome_date' = as.character(outcome_date),
                'form_date' = as.character(form_date)) %>%
    # if outcome time is missing '00:00' then set to the end of the day
    mutate(is.missing = stringr::str_sub(outcome_date, 12, 16)=='00:00',
           outcome_date = as.character(outcome_date),
           outcome_date = ifelse(is.missing==TRUE, paste(str_sub(outcome_date, 1, 10), '23:59:00'), outcome_date),
           outcome_date = as.POSIXct(outcome_date)) %>%
    select(-is.missing) # no longer needed
  
  ## get all key dates for one patient in a chain
  event_data = prior = all_events = NULL
  for (k in 1:nrow(indata)){
    this_id = indata$participant_id[k]
    this_redcap = indata$redcap_version[k]
    # exclude if missing admission date
    admission = filter(indata, participant_id == this_id) %>% pull(admission_datetime)
    if(is.na(admission)){next}
    # patient admission and step-wedge design dates
    this = filter(indata, participant_id == this_id) %>%
      select(participant_id, redcap_version, hospital, admission_datetime) %>% # 
      left_join(date_changes_time, by='hospital') %>% # add date of intervention version with date/times
      select(-hospital) %>% # No longer needed
      pivot_longer(cols=contains('date'), names_to='event', values_to='datetime') %>% # all dates
      mutate(datetime = datetime+1) # add one second to admission so that any events with same time as admission appear earlier
    # discharge
    this_d = filter(complete, participant_id == this_id) %>%
      select(participant_id, redcap_version, hosp_discharge_date) %>%
      mutate(hosp_discharge_date = ifelse(hosp_discharge_date <= admission, NA, hosp_discharge_date), # if discharge is prior to admission then blank
             hosp_discharge_date = as.POSIXct(hosp_discharge_date, origin='1970-01-01', tz='Australia/Brisbane')) %>% # keep in date format
      pivot_longer(cols=contains('date'), names_to='event', values_to='datetime') # all dates (just one)
    ## outcome, ignore "Unknown" so they become like missing
    #
    this_o = filter(form, participant_id == this_id) %>%
      mutate(event = as.character(event),
             datetime = ifelse(event=='No', form_date, outcome_date), # date used depends on outcome
             datetime = as.POSIXct(datetime, origin='1970-01-01', tz='Australia/Brisbane')) %>%
      select(participant_id, redcap_version, datetime, event) %>%
      filter(!is.na(event)) # remove few missing event
    if(nrow(this_o)==0){next} # do not add, can only do time to outcome in patients with at least one assessment
    # Prior if already known to palliative care
    if(any(this_o$event == 'Known to Pall Care')){
      frame = data.frame(participant_id = this_id, redcap_version=this_redcap, event='Prior')
      prior = bind_rows(prior, frame) 
      next 
    }
    # b) put all dates in a chain
    all_dates = bind_rows(this, this_d, this_o) %>%
      filter(!is.na(datetime)) %>% # exclude any missing dates
      arrange(datetime) %>% # arrange by date and time
      mutate(admission = as.numeric(event == 'admission_datetime'),
             change_over = as.numeric(event == 'date_intervention'),
             end_follow_up = as.numeric(event == 'date_post'),
             yes = as.numeric(event == 'Yes'),
             discharge = as.numeric(event == 'hosp_discharge_date'),
             post_admission = cumsum(admission), # include admission day
             post_follow_up = cumsum(end_follow_up) - end_follow_up,
             post_yes = cumsum(yes) - yes,
             post_discharge = cumsum(discharge) - discharge, # find days after discharge (excluding day of discharge)
             post_intervention = cumsum(change_over) - change_over) # find days after intervention 
    # if "yes" is first date then patient is 'prior' and not suitable for survival analysis
    # should possibly be any yes before admission
    if(all_dates$event[1] =='Yes'){
      frame = data.frame(participant_id = this_id, redcap_version=this_redcap, event='Prior')
      prior = bind_rows(prior, frame)
      next 
    }
    all_dates = filter(all_dates, post_admission >=1, # remove any dates before admission
                       post_yes <=0,  # remove any dates after first yes, so double yes is not an isse
                       post_discharge <=0,  # remove any dates after discharge
                       post_follow_up <=0) %>% # remove any dates after end of follow-up
      group_by(event) %>%
      arrange(event, desc(datetime)) %>% # oldest datetime first
      slice(1) %>% # if two or more No's in a row choose latest (works because there are no two No's in a row)
      ungroup() %>%
      arrange(datetime)%>%
      mutate(event_short = case_when(
        event == 'admission_datetime' ~ 'A',
        event == 'No' ~ 'N',
        event == 'Yes' ~ 'Y',
        event == 'hosp_discharge_date' ~ 'D',
        event == 'date_intervention' ~ 'I',
        event == 'date_post' ~ 'P',
        is.character(event) ~ event
      ))
    
    # make string of events, to here
    events_string = data.frame(participant_id=this_id, events=paste(all_dates$event_short, collapse=', '))
    all_events= bind_rows(all_events, events_string)
    
    # events
    event_data = bind_rows(event_data, all_dates)
  }
  # now select ending date depending on pattern
  # see the Word document "Setting up the data for the survival model"
  
  cat('All possible events:\n')
  print(table(all_events$events))
  
  ## version 1 with no competing risk of discharge
  # time to first yes, not "A, Y, H" because follow-up ended after Y; not any with I before Y
  ids = filter(all_events, events %in% c('A, N, Y', 'A, Y')) %>% pull(participant_id)
  to_yes = filter(event_data, participant_id %in% ids,
                  event_short %in% c('A','Y')) # just admission and yes
  # time to last no; not any with I before N
  ids = filter(all_events, events %in% c('A, N, D', 'A, N, P', 'A, N, I', 'A, N, I, D', 'A, N, I, P')) %>% pull(participant_id)
  to_no = filter(event_data, participant_id %in% ids,
                 event_short %in% c('A','N')) # just admission and no
  ## version 2 with competing risk of discharge (time to yes remains the same)
  # time to intervention (censor there)
  ids = filter(all_events, events %in% c('A, I, D','A, I, N, D','A, I, N, P','A, I, N, Y','A, I, P','A, I, Y','A, N, I, D','A, N, I, P')) %>% pull(participant_id)
  to_intervention = filter(event_data, participant_id %in% ids,
                 event_short %in% c('A','I')) # just admission and intervention
  # time to post (censor there)
  ids = filter(all_events, events %in% c('A, P', 'A, N, P')) %>% pull(participant_id)
  to_post = filter(event_data, participant_id %in% ids,
                           event_short %in% c('A','P')) # just admission and post
  # time to discharge
  ids = filter(all_events, events %in% c('A, D', 'A, N, D')) %>% pull(participant_id)
  to_disch = filter(event_data, participant_id %in% ids,
                 event_short %in% c('A','D')) # just admission and discharge
  
  ## now make one row per participant with survival time
  # version 1
  one_row = bind_rows(to_yes, to_no) %>%
    select(participant_id, redcap_version, event, datetime) %>%
    group_by(participant_id, redcap_version) %>%
    mutate(cols = 1:n()) %>%
    pivot_wider(id_cols=c(participant_id,redcap_version), names_from=cols, values_from=c(event, datetime)) %>%
    ungroup() %>%
    mutate(time = (as.numeric(datetime_2) - as.numeric(datetime_1))/(24*60*60)) %>% # difference in days
    select(-event_1) %>% # is always admission
    rename('event' = 'event_2')
  # now put together patients with prior and those with survival time
  to_export = bind_rows(one_row, prior)
  # version 2
  one_row2 = bind_rows(to_yes, to_intervention, to_post, to_disch) %>%
    select(participant_id, redcap_version, event, datetime) %>%
    group_by(participant_id, redcap_version) %>%
    mutate(cols = 1:n()) %>%
    pivot_wider(id_cols=c(participant_id,redcap_version), names_from=cols, values_from=c(event, datetime)) %>%
    ungroup() %>%
    mutate(time = (as.numeric(datetime_2) - as.numeric(datetime_1))/(24*60*60)) %>% # difference in days
    select(-event_1) %>% # is always admission
    rename('event' = 'event_2')
  # now put together patients with prior and those with survival time
  to_export2 = bind_rows(one_row2, prior) %>%
    mutate(event = case_when(
      event == 'date_intervention' ~ 'Intervention',
      event == 'date_post' ~ 'Post-intervention',
      event == 'hosp_discharge_date' ~ 'Discharge',
      is.character(event)==TRUE ~ event
    ))
  
  ## random checks
  # yes
  yes = filter(to_export, event=='Yes') %>% sample_n(3) %>% arrange(participant_id)
  cat('*Yes*:\n')
  cat('From processed data:\n')
  print(yes)
  cat('From original data:\n')
  yes_form = filter(form, participant_id %in% yes$participant_id)  %>% arrange(participant_id)
  print(yes_form)
  # no
  cat('*No*:\n')
  no = filter(to_export, event=='No') %>% sample_n(3)  %>% arrange(participant_id)
  cat('From processed data:\n')
  print(no)
  cat('From original data:\n')
  no_form = filter(form, participant_id %in% no$participant_id) %>% arrange(participant_id)
  print(no_form)
  
  # return
  to_return = list()
  to_return$version1 = to_export
  to_return$version2 = to_export2
  return(to_return) 
  
} # end of function
