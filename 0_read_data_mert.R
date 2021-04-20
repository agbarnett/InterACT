# 0_read_data_mert.R
# read the mert data 
# for outcome 7: "Time in hours from first positive record review to medical emergency calls"
# will need to match with interact on ur_number, admission date/time
# November 2020
#library(readxl) # could not read excel sheet, too many formatting issues
library(dplyr)
library(janitor)

# get the data from the hospital
data_folder = "U:/Research/Projects/ihbi/aushsi/interact/TRIAL/Data collection/Historical datasets/TPCH/"
file = "TPCH_RRT_DATA_2018_2019_2020_InterACT"
raw = read.table(paste(data_folder, file, '.txt', sep=''), sep='\t', header=TRUE, quote='') %>%
  clean_names() %>%
  filter(
    call_type=='MET', # MET calls only
    ur_number!='', # remove few missing UR number (cannot match)
         admitted_date !='', # remove few missing admission date (cannot match)
         admitted_time !='' , # remove few missing admission time (cannot match)
         call_date !='' , # remove few missing admission time (cannot match)
         switch_call_time!='') %>% # remove few missing admission time (cannot match)
  filter(nchar(call_date) >=9,
         nchar(admitted_date) >=9) %>% # remove few partially complete dates 
  mutate(
    ur_number = as.numeric(ur_number),
    call_datetime = as.POSIXct(paste(as.character(call_date), ' ', as.character(switch_call_time), sep=''), format='%d/%m/%Y %H:%M', tz="Australia/Brisbane"), # make date/time
    admitted_datetime = as.POSIXct(paste(as.character(admitted_date), ' ', as.character(admitted_time), sep=''), format='%d/%m/%Y %H:%M', tz="Australia/Brisbane"),
    dob = as.Date(dob, format='%d/%m/%Y'),
    call_date = as.Date(call_date, format='%d/%m/%Y'),
    admitted_date = as.Date(admitted_date, format='%d/%m/%Y'),
    agex = floor(as.numeric(admitted_date - dob)/365.25) # recalculate age because a few are missing
  ) %>%
  filter(agex > 70, agex < 120) %>% # only aged 70+ and remove few with bad dates
  select(-age) %>%
  rename('age' = 'agex') %>% # rename age
  select(ur_number, admitted_date, admitted_datetime, age, call_date, call_datetime) %>%
  mutate(hospital = 'TPCH')

# save
mert = raw
save(mert, file='data/mert_tpch.RData')

# check of numbers per day
check = group_by(mert, call_date) %>%
  tally()
library(ggplot2)
gplot = ggplot(check, aes(x=call_date, y=n))+
  geom_line()
gplot
summary(check$n)
