# 1_scramble_data.R
# create version of data that is scrambled, second attempt
# July 2021
library(TeachingDemos) # for random seed
char2seed('denmark') # random seed

# jumble most columns independently
jumble = function(indata, not, strata=NULL){
  to_jumble = names(indata) # get variable names to jumble
  to_jumble = to_jumble[to_jumble %in% not ==FALSE]
  outdata = indata # start with old data
  N = nrow(outdata)
  if(is.null(strata) == TRUE){
    for (v in to_jumble){
      jumble = outdata[,v] 
      if(class(jumble)[1] == 'tbl_df'){
        jumble = pull(jumble, v)
      }
      jumble = jumble[sample(1:N)] # jumble
      outdata = outdata[, names(outdata) != v] # remove
      outdata = bind_cols(outdata, jumble, .name_repair = 'minimal') # replace
      names(outdata)[ncol(outdata)] = v # rename
    }
  }
  if(is.null(strata) == FALSE){
    to_jumble = to_jumble[to_jumble %in% strata ==FALSE] # do not jumble strata variable
    str = select(outdata, all_of(strata)) %>% pull(strata) # get strata levels
    final = NULL
    for (o in unique(str)){
      index = which(str == o) # just strata
      tempdata = outdata[index,]
      N = length(index) # update data size for strata
      for (v in to_jumble){
        jumble = tempdata[, v] 
        if(class(jumble)[1] == 'tbl_df'){
          jumble = pull(jumble, v)
        }
        jumble = jumble[sample(1:N)] # jumble
        tempdata = tempdata[, names(tempdata) != v] # remove
        tempdata = bind_cols(tempdata, jumble, .name_repair = 'minimal') # replace
        names(tempdata)[ncol(tempdata)] = v # rename
      }
      final = bind_rows(final, tempdata) # concatenate
    }
    outdata = final # rename
  }
  #
  return(outdata)
} # End of function

baseline = jumble(baseline, not = c('participant_id','urn','hospital','redcap_version'))
clinicianled_review = jumble(clinicianled_review, not = c('participant_id','hospital','redcap_version'))
care_directive = jumble(care_directive, not = c('participant_id','hospital','redcap_version'))
palliative_care_referral = jumble(palliative_care_referral, not = c('participant_id','hospital','redcap_version'))
# don't jumble times as this creates errors in survival model:
# and stratify on outcome so event rates are kept
survival_data1 = jumble(survival_data1, not = c('participant_id','redcap_version', 'datetime_1', 'datetime_2'), strata='outcome') 
survival_data2 = jumble(survival_data2, not = c('participant_id','redcap_version', 'datetime_1', 'datetime_2'), strata='outcome') 

## if dates are there, then change to yes (bit of unscrambling)
#
clinicianled_review = mutate(clinicianled_review,
                             care_review = ifelse(!is.na(time_care_review), 'Yes', care_review),
                             care_review = ifelse(is.na(time_care_review), 'No', care_review))
#
care_directive = mutate(care_directive,
                        change_5 = ifelse(!is.na(date_time_5), 'Yes', change_5),
                        change_5 = ifelse(is.na(date_time_5), 'No', change_5))
#
palliative_care_referral = mutate(palliative_care_referral,
                                  pall_care = ifelse(!is.na(pall_date), 'Yes', pall_care),
                                  pall_care = ifelse(is.na(pall_date), 'No', pall_care))

# if blank date/time then must be prior
survival_data1 = mutate(survival_data1,
                       event = ifelse(is.na(time), 'Prior', event),
                       event = ifelse(is.na(datetime_1), 'Prior', event))
survival_data2 = mutate(survival_data2,
                        event = ifelse(is.na(time), 'Prior', event),
                        event = ifelse(is.na(datetime_1), 'Prior', event))

# quick check (dates should be everywhere)
quick_check = function(){
  gplot = ggplot(data=baseline, aes(x=int_time, y=admission_datetime))+
    geom_point()+
    facet_wrap(~hospital)+
    coord_flip()
  gplot
  
}


