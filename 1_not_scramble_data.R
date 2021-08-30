# 1_not_scramble_data.R
# create version of data that is NOT scrambled
# July 2021

## get the data
data_to_use = 'real'
if(data_to_use== 'real'){load('data/FullData.RData')} # from 0_read_data_redcap.R 
if(data_to_use== 'dummy'){load('data/DummyData.RData'); file_plus='_dummy'} # dummy data from 0_make_dummy_data.R 

# just use version three
baseline = filter(baseline, redcap_version==3)
complete = filter(complete, redcap_version==3)
care_directive = filter(care_directive, redcap_version==3)
palliative_care_referral = filter(palliative_care_referral, redcap_version==3)
clinicianled_review = filter(clinicianled_review, redcap_version==3)
survival_data = filter(survival_data, redcap_version==3) %>%
  arrange(datetime_1) # useful for residual checks and influence plots

# remove errors, should not be needed for real data - because of unfixed data errors
baseline = filter(baseline, int_time !='Error')
palliative_care_referral = filter(palliative_care_referral, int_time !='Error')
care_directive = filter(care_directive, int_time !='Error')
clinicianled_review = filter(clinicianled_review, int_time !='Error')
survival_data = filter(survival_data, int_time !='Error')

# sort out intervention time as a factor by dropping unused levels
baseline = mutate(baseline, int_time  = droplevels(int_time))
care_directive = mutate(care_directive, int_time  = droplevels(int_time))
palliative_care_referral = mutate(palliative_care_referral, int_time  = droplevels(int_time))
clinicianled_review = mutate(clinicianled_review, int_time  = droplevels(int_time))
survival_data = mutate(survival_data, 
                       int_time  = droplevels(int_time),
                       int_time_n = as.numeric(int_time == 'Intervention'))

# quick check
quick_check = function(){
  gplot = ggplot(data=baseline, aes(x=int_time, y=admission_date))+
    geom_point()+
    facet_wrap(~hospital)+
    coord_flip()
  gplot
  
  gplot = ggplot(data=baseline, aes(x=int_time, y=median_form))+
    geom_point()+
    facet_wrap(~hospital)+
    coord_flip()
  gplot
  #
  f = filter(baseline, hospital=='RBWH', int_time =='Establishment', admission_date < as.Date('2020-11-16')) %>%
    select(participant_id, int_time, median_form, admission_datetime)
}

