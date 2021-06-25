# 0_date_changes.R
# date changes per hospital, including key randomised dates
# June 2021
library(dplyr)
library(tidyr)
library(ggplot2)

# start dates for key changes
date_changes = read.table(header=TRUE, stringsAsFactors=FALSE, sep=',', text='
hospital,date_usual_care,date_establishment,date_intervention,date_post,date_end
TPCH,2020-05-25,2020-09-14,2020-10-12,2021-06-07,2021-08-29
RBWH,2020-05-25,2020-11-16,2020-12-14,2021-06-07,2021-08-29
GCUH,2020-05-25,2021-01-18,2021-02-15,2021-06-07,2021-08-29') %>%
  gather(key='event', value='date', -`hospital`) %>%
  mutate(date = as.Date(date))

# date_intervention = date of change over to intervention
# date_post = end of follow-up?

# quick check
check_plot = ggplot(data=date_changes, aes(x=hospital, y=date, col=factor(event)))+
  geom_point(size=5)+
  coord_flip()+
  theme_bw()
check_plot

# long to wide
date_changes = spread(date_changes, event, date) %>%
  select('hospital','date_usual_care','date_establishment','date_intervention','date_post','date_end') # ordering

# keep dates for intervention and end of intervention (date_post) and change to times
date_changes_time = select(date_changes, hospital, date_intervention, date_post) %>% # 
  mutate(date_intervention = as.POSIXct(paste(date_intervention,'23:59', tz='Australia/Brisbane')), # make dates into date/times; one minute before midnight for end of day
         date_post = as.POSIXct(paste(date_post,'23:59', tz='Australia/Brisbane')))  

# save
save(date_changes, date_changes_time, file='data/date_changes.RData')
