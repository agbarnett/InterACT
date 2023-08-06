#0_process_transfer_data.R
source('99_packages.R')

#4. cohort transfer data
cohort_tfr = read.csv('../../Data linkage/New_SSB chort_comparator_data_150922/cohort_admit_tfr_amd.csv') %>% clean_names() 

#split datetimes and fix up formatting as per cohort_admission
cohort_tfr = cohort_tfr %>% mutate("start_date_1"=str_split_fixed(tfr_date,pattern=':',n=2)[,1],
                                   "start_date_2"=str_split_fixed(tfr_date,pattern=':',n=2)[,2],
                                   "end_date_1"=str_split_fixed(tfr_end_date,pattern=':',n=2)[,1],
                                   "end_date_2"=str_split_fixed(tfr_end_date,pattern=':',n=2)[,2],
                                   "tfr_start_date_time"=as.POSIXct(paste(start_date_1,start_date_2),format="%d%B%Y %H:%M:%S"),
                                   "tfr_end_date_time"=as.POSIXct(paste(end_date_1,end_date_2),format="%d%B%Y %H:%M:%S")) %>%
  select(person_id,admit_ep,stnd_unit_code,stnd_ward_code,ward,tfr_start_date_time,tfr_end_date_time,icu_flag)


tz(cohort_tfr$tfr_start_date_time) <- "Australia/Brisbane" 
tz(cohort_tfr$tfr_end_date_time) <- "Australia/Brisbane"


save(cohort_tfr,file='processed/transfer_data.rda')
