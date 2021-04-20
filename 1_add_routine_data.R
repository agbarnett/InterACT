# 1_add_routine_data.R
# add the routinely collected data, e.g., MERT calls
# just test code, this will be merged by SSB eventually
# November 2020
library(dplyr)
options(dplyr.summarise.inform=FALSE)  # turn off annoying warning

## Not yet complete, need to get more up to date date on MERT and deaths/QPADHC

# get the REDCap and routine data
load('data/mert_tpch.RData') # MET calls from 0_read_data_mert.R
load('data/SSB_data.RData') # from 0_read_data_ssb.R
load('data/FullData.RData') # from 0_read_data_redcap.R
baseline = filter(baseline, 
                  redcap_version == 3, # just version 3
                  at_risk == 'At risk') %>% # just those at risk
  filter(!is.na(admission_date)) # remove small number with no admission (cannot match)

# have to loop because of dates
new_baseline = NULL
for (k in 1:nrow(baseline)){
  this_patient = baseline[k,]
  any_mert = filter(mert, ur_number == this_patient$urn,
                    hospital == this_patient$hospital) # can be multiple rows
  if(nrow(any_mert)>0){ # some matching MERT data
    cat(as.Date(any_mert$admitted_date, origin='1970-01-01'),'\n')
    cat(as.Date(this_patient$admission_date, origin='1970-01-01'), '\n')
    cat('diff = ', difftime(any_mert$admitted_date, this_patient$admission_date), '\n')
    # merge if dates match
    any_mert = filter(any_mert, 
           admitted_date >= this_patient$admission_date - 1,
           admitted_date <= this_patient$admission_date + 1) # within day of redcap admission
    if(nrow(any_mert)>0){
      cat('k=', k, '\n')
      cat(any_mert$ur_number, '\n')
      # if any matches
      any_mert = arrange(any_mert, call_datetime) %>%
        slice(1) %>% # take earliest time
        select(-hospital) # no longer needed
      this_patient = left_join(this_patient, any_mert, by=c('urn'='ur_number'))
      # check
      age_diff = abs(this_patient$age - any_mert$age[1])
      if(age_diff > 1){ # beyond two years because of date range
        cat('Age issue, URN = ', any_mert$ur_number[1], ', ages=', this_patient$age, ' & ', any_mert$age[1], '\n', sep='')
      }
    }
  }
  new_baseline = bind_rows(new_baseline, this_patient)
}

# admission_date = this is to current ward



# merge by urn and admission date within 8 hour window
both = left_join(baseline, admissions, by=c('urn'))

admission_date

# save
#save(x, file='data/FullData_plus.RData')
