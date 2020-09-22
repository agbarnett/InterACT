# 0_make_dummy_data.R
# make dummy data for testing code
# September 2020
library(dplyr)
set.seed(404040)

# start with real data
load('data/FullData.RData')
N = 1000 # number of dummy patients

# start with one just row
one_baseline = baseline[1,]
one_complete = complete[1,]
one_care = care_directive[1,]
one_pall = palliative_care_referral[1,]
one_clin = clinicianled_review[1,]
# 
baseline = complete = care_directive = palliative_care_referral = clinicianled_review = NULL
for (i in 1:N){
  baseline = bind_rows(baseline, one_baseline)
  complete = bind_rows(complete, one_complete)
  care_directive = bind_rows(care_directive, one_care)
  palliative_care_referral = bind_rows(palliative_care_referral, one_pall)
  clinicianled_review = bind_rows(clinicianled_review, one_clin)
}
## now generate random variables
#
baseline = mutate(baseline,
    participant_id = 1:N,
    redcap_version = 3,
    hospital = sample(c('RBHW','TPCH','GCUH'), replace = TRUE, size = N),
    pt_sex = sample(c('Male','Female'), replace = TRUE, size = N),
    spict_care_others = as.factor(sample(c("Yes","No","Unknown"), prob=c(0.5,0.45,0.05), replace=TRUE, size=N)),
    cristal_icu = as.factor(sample(c("Yes","No","Unknown"), prob=c(0.5,0.45,0.05), replace=TRUE, size=N)),
    age = 75 + round(rgamma(n=N, rate=1/10, shape=0.5)),
    baseline_screening_complete = sample(c('No','Yes'), replace = TRUE, size = N),
    at_risk = sample(c('Not at risk','At risk'), replace = TRUE, size = N)
)
#
care_directive = mutate(care_directive,
    participant_id = 1:N,
    change_var = as.factor(sample(c("Prior","Yes","No"), prob=c(0.01,0.09,0.90), replace=TRUE, size=N)),
    redcap_version = 3)
#
palliative_care_referral = mutate(palliative_care_referral,
    participant_id = 1:N,
    change_var = as.factor(sample(c("Prior","Yes","No"), prob=c(0.01,0.09,0.90), replace=TRUE, size=N)),
    redcap_version = 3)
#
clinicianled_review = mutate(clinicianled_review,
    participant_id = 1:N,
    change_var = as.factor(sample(c("Prior","Yes","No"), prob=c(0.01,0.09,0.90), replace=TRUE, size=N)),
    redcap_version = 3)
#
complete = mutate(complete,
    participant_id = 1:N,
    redcap_version = 3)

# save
save(thresholds, data_date, spict.vars, cristal.vars, baseline, complete, 
     care_directive, palliative_care_referral, clinicianled_review, file='data/DummyData.RData')
