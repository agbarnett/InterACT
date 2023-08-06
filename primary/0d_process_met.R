#0d_process_met.R
source('99_packages.R')

#load data for each trial facility
met_rbwh = read.xlsx('../../Data linkage/MET data/interACT_Study_25052020_06062021.xlsx',sheet='Export Worksheet',detectDates = T) %>% clean_names()
met_gcuh = readxl::read_excel('../../Data linkage/MET data/GCUH_InterACT-MetCalls_25052020-06062021.xlsx',sheet='Data',trim_ws=T) %>% clean_names()
met_tpch = readxl::read_excel('../../Data linkage/MET data/TPCH_at risk_METcalls.xlsx',sheet='Combined',trim_ws=T) %>% clean_names() %>% select(ur_number,call_type,call_date,switch_call_time)
met_tpch_ad = read.xlsx('../../Data linkage/MET data/TPCH_at risk_METcalls.xlsx',sheet='TPCH',detectDates=T) %>% clean_names() %>% select(study_id,urn)



#date processing
#rbwh
met_rbwh = met_rbwh %>% mutate(met_date_time = convert_to_datetime(event_call_time,tz='Australia/Brisbane')) %>% select(study_id,met_date_time) %>% filter(!is.na(met_date_time))
#gcuh
met_gcuh = met_gcuh %>% filter(grepl("^MET",all_criteria)) %>% 
  mutate(met_time=as.character(gsub(".* ","",rapid_response_call_time)),met_date=convert_to_date(rapid_response_call_date,tx='Australia/Brisbane'))
met_gcuh = filter(met_gcuh,!is.na(met_date),!is.na(met_time)) %>% mutate(met_date_time = as.POSIXct(paste(met_date,met_time))) %>% select(study_id,met_date_time)

#tpch
met_tpch = met_tpch %>% mutate(met_time=as.character(gsub(".* ","",switch_call_time)),met_date=convert_to_date(call_date,tx='Australia/Brisbane'))
met_tpch = filter(met_tpch,!is.na(met_date),!is.na(met_time)) %>% mutate(met_date_time = as.POSIXct(paste(met_date,met_time))) %>% select(ur_number,met_date_time)
met_tpch = inner_join(met_tpch_ad,met_tpch,by=c('urn'='ur_number')) %>% select(-urn)

atrisk_met = bind_rows(met_rbwh,met_gcuh,met_tpch) %>% filter(!is.na(met_date_time)) %>%
  group_by(study_id) %>% summarise(met_date_time_l = list(met_date_time),.groups='drop')

save(atrisk_met,file='processed/met_data.rda')
