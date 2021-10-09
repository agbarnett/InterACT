# 1_not_scramble_data.R
# create version of data that is NOT scrambled
# Sep 2021

## get the data
data_to_use = 'real'
if(data_to_use== 'real'){load('data/FullData_imputed.RData')} # from 1_impute_arp.R
if(data_to_use== 'dummy'){load('data/DummyData.RData'); file_plus='_dummy'} # dummy data from 0_make_dummy_data.R 

# just use version three
baseline = filter(baseline, redcap_version==3)
complete = filter(complete, redcap_version==3)
care_directive = filter(care_directive, redcap_version==3)
palliative_care_referral = filter(palliative_care_referral, redcap_version==3)
clinicianled_review = filter(clinicianled_review, redcap_version==3)
survival_data = filter(survival_data, redcap_version==3) %>%
  arrange(datetime_1) # useful for residual checks and influence plots
# imputed
for (k in 1:10){
  care_directive_imputed[[k]] = filter(care_directive_imputed[[k]], redcap_version==3) 
  survival_data_imputed[[k]] = filter(survival_data_imputed[[k]], redcap_version==3) %>%
    arrange(datetime_1)
}

# remove errors, should not be needed for real data - because of unfixed data errors
not_needed = function(){
baseline = filter(baseline, int_time !='Error')
palliative_care_referral = filter(palliative_care_referral, int_time !='Error')
care_directive = filter(care_directive, int_time !='Error')
clinicianled_review = filter(clinicianled_review, int_time !='Error')
survival_data = filter(survival_data, int_time !='Error')
# imputed
for (k in 1:10){
  survival_data_imputed[[k]] = filter(survival_data_imputed[[k]], int_time !='Error')
}
}

# sort out intervention time as a factor by dropping unused levels
baseline = mutate(baseline, int_time  = droplevels(int_time))
care_directive = mutate(care_directive, int_time  = droplevels(int_time))
palliative_care_referral = mutate(palliative_care_referral, int_time  = droplevels(int_time))
clinicianled_review = mutate(clinicianled_review, int_time  = droplevels(int_time))
survival_data = mutate(survival_data, 
                       int_time  = droplevels(int_time),
                       int_time_n = as.numeric(int_time == 'Intervention'))
# imputed
for (k in 1:10){
  survival_data_imputed[[k]] = mutate(survival_data_imputed[[k]], 
                                      int_time  = droplevels(int_time),
                                      int_time_n = as.numeric(int_time == 'Intervention'))
  care_directive_imputed[[k]] = mutate(care_directive_imputed[[k]], int_time  = droplevels(int_time))
  
}

# for ordering of hospital in plots by intervention time
baseline = mutate(baseline,
                  hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH')))
care_directive = mutate(care_directive,
                  hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH')))
clinicianled_review = mutate(clinicianled_review,
                  hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH')))
palliative_care_referral = mutate(palliative_care_referral,
                  hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH')))
# imputed
for (k in 1:10){
  care_directive_imputed[[k]] = mutate(care_directive_imputed[[k]], 
                                       hosp_ordered = factor(hospital, levels=c('TPCH','RBWH','GCUH')))
}


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

