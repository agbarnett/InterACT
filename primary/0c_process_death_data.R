#0_process_death_data.R
source('99_packages.R')
source('99_trial_information.R')

cohort_deaths = read.csv('../../Data linkage/New_SSB chort_comparator_data_150922/cohort_cod_main_amd.csv') %>% clean_names() 


#fix date format
cohort_deaths = cohort_deaths %>% mutate_at(vars("death_date"),~as.Date(.,format="%d/%m/%Y")) #original data coding:cohort_deaths %>% mutate_at(vars("death_date"),~convert_to_date(.))
tz(cohort_deaths$death_date) <- "Australia/Brisbane" 

#reorder columns
cohort_deaths = cohort_deaths %>% select(person_id,death_date,death_place,fclty_name,coded_cause,cause)

save(cohort_deaths,file='processed/death_data.rda')
