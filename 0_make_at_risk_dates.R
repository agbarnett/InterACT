# 0_make_at_risk_dates.R
# make time-dependent at risk dates for SPICT (cristal was fixed over time)
# August 2021

# vector all teams
teams = c('GCUH - General Medicine','GCUH - Cardiology','GCUH - Nephrology','GCUH - Respiratory','RBWH - Stroke','RBWH - 2B Medicine','RBWH - 3B Medicine','RBWH - Vascular','RBWH - Cardiology','RBWH - 2A Medicine','RBWH - Neurosurgery','TPCH - General Medicine','TPCH - Orthopaedics','TPCH - Respiratory')
# make date range of all possible dates
date.range = seq(as.Date('2020-03-09'), as.Date('2021-11-16'), 1) # too big for now, used to cover date errors
time_dependent_threshold = expand.grid(teams, date.range) 
names(time_dependent_threshold) = c('team','date_seen')
time_dependent_threshold = mutate(time_dependent_threshold,
             team = as.character(team),
             spict_threshold = case_when(
               date_seen < as.Date('2020-12-14') & team =='TPCH - General Medicine' ~ 2,
               date_seen < as.Date('2021-03-21') & team =='TPCH - Respiratory' ~ 2,
               date_seen >= as.Date('2020-12-14') & team =='TPCH - General Medicine' ~ 3,
               date_seen >= as.Date('2021-03-21') & team =='TPCH - Respiratory' ~ 3,
               TRUE ~ 2)) %>% # otherwise
  ungroup()
# quick checks
#with(time_dependent_threshold, table(team, spict_threshold))
#with(time_dependent_threshold, table(date_seen >= as.Date('2020-12-14'), spict_threshold))
#sample_n(time_dependent_threshold, 5)
