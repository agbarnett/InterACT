# 3_synthetic_data.R
# make synthetic data for public sharing
# use method described here: https://elifesciences.org/articles/53275
# October 2022

# Load required packages
packages <- c("synthpop", "tidyverse", "cowplot", "car",
              "simstudy", "mice", "StatMeasures")
lapply(packages, require, character.only = TRUE)

################################

### get the analysis data
load('data/2_analysis_ready.RData') # from 2_interact_summary_456_blinded.Rmd

### Create synthetic data ###
seed = TeachingDemos::char2seed('walsall')
## care directive
# select the data
data_in = mutate(for_model_care_directive[[1]], # just use one imputation,
         team = as.numeric(as.factor(team))) %>% # blind team
  select(event, time, int_time, hospital, event, pt_sex, age, cristal_score, spict_score, team) # slim down
#
for_model_care_directive_sim <- syn(data_in, seed = seed) # 
## care review
# select the data
data_in = mutate(for_model_care_review[[1]], # just use one imputation,
                 team = as.numeric(as.factor(team))) %>% # blind team
  select(event, time, int_time, hospital, event, pt_sex, age, cristal_score, spict_score, team) # slim down
#
for_model_care_review_sim <- syn(data_in, seed = seed) # 
## palliative care 
# select the data
data_in = mutate(for_model_palliative[[1]], # just use one imputation,
                 team = as.numeric(as.factor(team))) %>% # blind team
  select(event, time, int_time, hospital, event, pt_sex, age, cristal_score, spict_score, team) # slim down
#
for_model_palliative_sim <- syn(data_in, seed = seed) # 

# run a comparison
palliative_compare <- compare(
  for_model_palliative_sim, # The synthetic dataset
  data_in, # The original dataset
  vars = c("pt_sex", "age",
           "time", "cristal_score"), # The variables for comparison
  print.coef = TRUE, # Print tables of estimates for original and synthetic data
  ncol = 4, # The number of columns in the plot
  breaks = 16, # Gaps between columns 
  stat = "counts", # Present the raw counts for each variable
  cols = c("#62B6CB", "#1B4965") # Setting the colours in the plot
) # Visual comparison of original and synthetic datasets

# comparison plot
fig_1a <- palliative_compare$plots + # Extracts plots from the "care_directive_compare" object
  scale_y_continuous(expand = c(0, 0)) + # Forces the y-axis to start at zero
  theme_minimal_hgrid(12) + # Applies a theme from the 'cowplot' package
  theme(#axis.text.x = element_text(angle = 60, hjust = 1),  # Adjusts x-axis tick labels
        axis.title.x = element_blank()) + # Removes x-axis title
  labs(fill = "Dataset") # Renames legend title

fig_1a # Prints figure
jpeg('figures/synthetic_palliative_compare.jpeg', width=10, height=7, units='in', res=500, quality=100)
print(fig_1a)
dev.off()

# cross-tabulations of original and synthetic data
with(for_model_care_directive_sim$syn, table(team, hospital))
with(data_in, table(team, hospital))

# combine three source
synthetic_data = bind_rows(for_model_care_directive_sim$syn,
                           for_model_care_review_sim$syn,
                           for_model_palliative_sim$syn, .id = 'outcome') %>%
  mutate(outcome = 
           case_when(
             outcome == 1 ~ 'care_directive',
             outcome == 2 ~ 'care_review',
             outcome == 3 ~ 'palliative_care'
           ),
         int_timec = as.character(int_time),
         int_timec = ifelse(int_timec=='Usual care', 'UsualCare', int_timec), # ggcompetingrisks cannot handle spaces
         event = case_when(
             event == 'Intervention' ~ 'Censored',
             event == 'Post-intervention' ~ 'Censored',
             event == 'No' ~ 'Censored',
             TRUE ~ as.character(event)
         ))


# check survival analysis
library(cmprsk)
cum_inc = with(filter(synthetic_data, outcome=='care_review'), cuminc(ftime=time, fstatus=event, group=int_timec, cencode='Censored'))
plot(cum_inc)

# export
save(synthetic_data, file = 'data/synthetic.RData')
write.csv(x = synthetic_data, file='data/synthetic.csv', quote=FALSE, row.names=FALSE)
