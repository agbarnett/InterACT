# 1_create_multiple_reports_teams.R
# create multiple reports for teams
# September 2020

## Packages
library(knitr)
library(rmarkdown)
library(dplyr)
library(stringr)

# get list of teams
load('data/teams.RData') # from 0_read_data.R

## Loop through all teams and hospitals
N = nrow(teams)
for (index in 1:N){

  # get the data (need to repeat for every loop)
  load('data/FullData.RData') # from 0_read_data.R
  # remove pre-covid data (just version 3 REDcap onwards)
  baseline = filter(baseline, 
                    redcap_version ==3)
  complete = filter(complete, redcap_version==3)
  care_directive = filter(care_directive, redcap_version==3)
  clinicianled_review = filter(clinicianled_review, redcap_version==3)
  palliative_care_referral = filter(palliative_care_referral, redcap_version==3)
  
  # get key data for this team
  this_hospital = teams$hospital[index]
  this_team = teams$team[index]
  team_var = paste(tolower(this_hospital), '_team', sep='') # create team variable (exists in baseline data)

  # create report
  outfile = paste("team_reports/", this_hospital, str_remove_all(string=this_team, pattern=' '), ".pdf", sep='')
  rmarkdown::render(input = "2_interact_summary_teams.Rmd",
                output_format = "pdf_document",
                output_file = outfile)
}


#### create report for ortho split by unplanned or planned admission (16 sep 2020) ##
# get the data (need to repeat for every loop)
load('data/FullData.RData') # from 0_read_data.R
# remove pre-covid data (just version 3 REDcap onwards)
baseline = filter(baseline, 
                  redcap_version ==3)
complete = filter(complete, redcap_version==3)
care_directive = filter(care_directive, redcap_version==3)
clinicianled_review = filter(clinicianled_review, redcap_version==3)
palliative_care_referral = filter(palliative_care_referral, redcap_version==3)

# get key data for this team
this_hospital = 'TPCH'
this_team = 'Orthopaedics'
team_var = paste(tolower(this_hospital), '_team', sep='') # create team variable (exists in baseline data)

# create report
outfile = paste("team_reports/", this_hospital, str_remove_all(string=this_team, pattern=' '), "_split.pdf", sep='')
rmarkdown::render(input = "2_interact_summary_teams_split.Rmd",
                  output_format = "pdf_document",
                  output_file = outfile)
