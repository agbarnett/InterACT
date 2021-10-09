# 1_prep_SSB.R
# October 2021
# prepare data to send to SSB for linkage; access processed data and from REDCap
# there are some patients with multiple presentations and different addresses
# ... and also prepare data to send to hospitals for MERT
library(dplyr)
library(tidyr)
library(RCurl) # for making the API request
library(jsonlite) # for converting JSON data
library(stringr) # 
source('REDCap.API/config.R') # get my API token, not visible to others

# get the processed data
load('data/FullData.RData') # from 0_read_data_redcap.R
at_risk = filter(baseline, 
                 redcap_version == 3, # just version 3
                 at_risk == 'At risk') %>% #  Filter by at-risk
  select(participant_id, hospital, admission_datetime) 

# access REDcap for address data
all_data = names_to_check = address_to_check = NULL
hospitals = c('GCUH','TPCH','RBWH')
for (hospital_loop in hospitals){
  if(hospital_loop == 'GCUH') {this.token = api_token_gc_v3} # API tokens
  if(hospital_loop == 'TPCH') {this.token = api_token_tpch_v3}
  if(hospital_loop == 'RBWH') {this.token = api_token_rbwh_v3}
  
  result <- postForm(
    api_url,
    token=api_token,
    content='event',
    format='json',
    returnFormat='json',
    arms=''
  )
  
  ## Get data from REDCap ##
  result = postForm(
    api_url,
    token = this.token,
    content = 'record',
    format = 'json',
   # returnFormat = 'json',
    type = 'flat', # one record per row
    forms = c('baseline_screening_arm_1'),
    .opts = list(ssl.verifypeer = TRUE) # see https://redcap.ihbi.qut.edu.au/api/help/?content=security
  )
  # convert from JSON to data frame
  redcap = fromJSON(result) %>%
    filter(redcap_event_name == 'baseline_screening_arm_1') %>%
    select('participant_id','admission_date','urn','pt_first_name', "pt_last_name", 'dob','pt_sex', 'pt_address') %>%
    mutate(pt_first_name  = str_remove_all(pt_first_name , pattern=','), # tidy the text
           pt_last_name = str_remove_all(pt_last_name, pattern=','),
           pt_first_name = str_squish(pt_first_name),
           pt_last_name = str_squish(pt_last_name),
           pt_address = str_replace_all(pt_address, pattern='\r\n', replacement = ', '),
           pt_address = str_remove_all(pt_address, pattern='\t'),
           pt_address = str_replace_all(pt_address, pattern=' ,', replacement = ','),
           pt_address = str_squish(pt_address),
           pt_address = str_replace_all(pt_address, pattern=',,', replacement = ','),
           pt_address = str_replace_all(pt_address, pattern='(?<=[A-Z])4', replacement = ' 4'), # add space before postcode if there is none using look ahead
           pt_address = str_remove_all(pt_address, pattern=',$'), # remove comma at end
           urn = str_squish(urn),
           pt_sex = ifelse(pt_sex==1, "Male", "Female"),
           participant_id = paste(hospital_loop, '_V3_', participant_id, sep='')) %>% # make participant id match processed version; only using version 3 of redcap
    unique() # some people in twice (will double them up with their multiple dates below)
  # convert to date, needed to get latest address (had to do in loop)
  for (k in 1:nrow(redcap)){
    redcap$admission_date[k] = ifelse(redcap$admission_date[k]=='', NA, as.POSIXct(redcap$admission_date[k], tz='Australia/Brisbane'))
  }
  
  ## check for different addresses ##
  check = select(redcap, urn, pt_address) %>%
    group_by(urn) %>%
    mutate(n = 1:n(),
           pt_address = tolower(pt_address)) %>% # lower case is fine for comparison
    pivot_wider(names_from = n, values_from = pt_address) %>%
    filter(!is.na(`2`)) %>% # only those with 2 addresses
    ungroup()
  # make into a matrix
  matrix = select(check, -urn) # lower case
  all_check_address = NULL
  for(one in 1:(ncol(matrix)-1)){ # loop through every combination
    for(two in ((one+1):ncol(matrix))){
      index = matrix[,one] != matrix[,two]
      if(any(index, na.rm=TRUE) == TRUE){
        index = which(index)
        keep = matrix[index,]
        all_check_address = rbind(all_check_address, keep)
      }
    }
  } 
  # add back urn and participant ID
  small = select(redcap, participant_id, urn)
  if(is.null(all_check_address) == FALSE){
    matrix = check[,1:2] # just first two columns
    with_urn = left_join(all_check_address, matrix, by='1') %>%
      unique() %>% # knock out repeats
      mutate(hospital = hospital_loop) %>% # add hospital
      left_join(small, by='urn') # add ID
    address_to_check = bind_rows(address_to_check, with_urn) # concatenate for checking
  }
  
  ## get latest address per patient ##
  latest = group_by(redcap, urn) %>%
    arrange(urn, desc(admission_date)) %>% # sort by descending date
    slice(1) %>%
    ungroup() %>%
    select(urn, pt_address)
  # now replace all addresses with the latest address
  redcap = select(redcap, -pt_address) %>% # drop old address
    left_join(latest, by='urn')
  
  # merge processed data with redcap data
  this_hospital = filter(at_risk, hospital == hospital_loop) %>%
    left_join(redcap, by='participant_id') %>%
    arrange(pt_sex, pt_first_name, pt_last_name) # for spotting multiple admissions
  
  ## check for different names per URN ##
  check = mutate(this_hospital, name = paste(pt_first_name, pt_last_name)) %>%
    select(urn, name) %>%
    group_by(urn) %>%
    mutate(n = 1:n()) %>%
    pivot_wider(names_from = n, values_from = name) %>%
    filter(!is.na(`2`)) %>% # just those with two names
    ungroup()
  # make into a matrix
  matrix = select(check, -urn)
  all_check = NULL
  for(one in 1:(ncol(matrix)-1)){ # loop through every combination
      for(two in ((one+1):ncol(matrix))){
        index = matrix[,one] != matrix[,two]
        if(any(index, na.rm=TRUE) == TRUE){
          index = which(index)
          keep = matrix[index,]
          all_check = rbind(all_check, keep)
        }
    }
  } 
  # add back urn and participant ID
  small = select(this_hospital, participant_id, urn)
  if(is.null(all_check) == FALSE){
    matrix = check[,1:2] # just first two columns
    with_urn = left_join(all_check, matrix, by='1') %>%
      unique() %>% # knock out repeats
      mutate(hospital = hospital_loop) %>% # add hospital
      left_join(small, by='urn') # add ID
    names_to_check = bind_rows(names_to_check, with_urn) # concatenate for checking
  }

  # check for missing data
  if(any(is.na(this_hospital$pt_sex))){cat('Warning, some missing sex for ', hospital_loop,'.\n', sep='')}
  if(any(is.na(this_hospital$dob))){cat('Warning, some missing DOB for ', hospital_loop,'.\n', sep='')}
  if(any(nchar(this_hospital$pt_address) < 20)){
    cat('Warning, some missing/short address for ', hospital_loop,'.\n', sep='')
    ids = filter(this_hospital, nchar(pt_address)<20) %>%
      pull(participant_id )
    cat('ID = ', paste(ids, collapse=', '), '\n', sep='')
  }
  
  # concatenate
  all_data = bind_rows(all_data, this_hospital)
  
}

## export data to tab-delimited text because Excel is a nightmare with dates (need to password protect file)
# (check what format do they want it in?)
to_export = select(all_data, participant_id, admission_datetime, hospital, urn, pt_first_name, pt_last_name, dob, pt_sex, pt_address) %>%
  rename('study_id' = 'participant_id') # just letting SSB know that this is our ID
#
write.table(to_export, file='data/interact_for_ssb.txt', quote=TRUE, row.names = FALSE, sep='\t')
# zip and password protect
psw = 'vndzwdzpnv'
addFlags="-j"
zip(
  zipfile = 'data/interact_for_ssb', 
  files = 'data/interact_for_ssb.txt', # just one file to add
  flags = paste0("-r --password ", psw, " ", addFlags)
)

## export individual hospitals for MERT data
### TO HERE< NOT CHECKED THIS CODE YET
psw = list()
psw[['TPCH']] = 'snkjmssehv'
psw[['RBWH']] = 'xpzcqmrkfm'
psw[['GCUH']] = 'brrkzmgndc'
for (hosp in c('TPCH','GCUH','RBWH')){
  this_export = filter(to_export, hospital == hosp)
  this_zip = paste('data/interact_for_', hosp, sep='')
  outfile = paste('data/interact_for_hospital_', hosp, '.txt', sep='')
  write.table(this_export, file=outfile, quote=TRUE, row.names = FALSE, sep='\t')
  # zip and password protect
  addFlags="-j"
  this_psw = psw[[hosp]]
  zip(
    zipfile = this_zip, 
    files = outfile, # just one file to add
    flags = paste0("-r --password ", this_psw, " ", addFlags)
  )
}


## export checks of names and addresses
library(openxlsx)
address_to_check = select(address_to_check, 'hospital', urn, participant_id, everything()) # order rows
names_to_check = select(names_to_check, 'hospital', urn, participant_id, everything()) # order rows
#
hs1 <- createStyle(fgFill = "black", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Barnett")
#
addWorksheet(wb, sheetName = "Names")
writeData(wb, sheet=1, x=names_to_check, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:9, widths = "auto")
#
addWorksheet(wb, sheetName = "Address")
writeData(wb, sheet=2, x=address_to_check, headerStyle = hs1)
setColWidths(wb, sheet = 2, cols = 1:9, widths = "auto")
#
saveWorkbook(wb, file = "checks/name_address_checks.xlsx", overwrite = TRUE)

# check for odd urn
summary(nchar(to_export$urn))
# check for odd dob
summary(as.Date(to_export$dob))
# check study number
summary(nchar(to_export$study_id))
head(filter(to_export, nchar(study_id)==12))
# check sex
table(to_export$pt_sex)
# check hospital
table(to_export$hospital)
# check admission
summary(to_export$admission_datetime)
