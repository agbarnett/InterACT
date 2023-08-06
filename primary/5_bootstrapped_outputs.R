# 5_bootstrapped_outputs.R
# takes a while to run
# June 2023
n_boot = 3000 # number of bootstrap samples for risk difference, increase to 3000 for final run
TeachingDemos::char2seed('Germany')
source('99_packages.R')
source('99_functions.R')
source('../99_functions_not_shared.R') # for blinding
load('analysis_ready/final_dataset.rda')
# AGB, change one team to usual care only as they never got the emails
itt = FALSE # intention-to-treat, run twice (TRUE and FALSE)
# filenames dependent on ITT
outfile_icu = 'bootstrap output/icu.rda'
outfile_los = 'bootstrap output/length of stay.rda'
outfile_met = 'bootstrap output/met.rda'
outfile_readmission = 'bootstrap output/hospital readmission.rda'
if(itt==TRUE){
  outfile_icu = 'bootstrap output/icu itt.rda'
  outfile_los = 'bootstrap output/length of stay itt.rda'
 outfile_met = 'bootstrap output/met itt.rda'
 outfile_readmission = 'bootstrap output/hospital readmission itt.rda'
}

#outcomes 1, 2, 3 and 7

#add extra variables created in markdown

dat_analysis = dat_analysis %>% mutate(Hospital = str_remove_all(study_id, '_.*'), 
                    Hospital = deidentify_hospital(Hospital), # blinded, AGB
                    #seasonality terms: sin and cos
                    admit_frac = season::yrfraction(admit_date), 
                    sinw = sin(2*pi*admit_frac), cosw=cos(2*pi*admit_frac), 
                    #LOS and outcome censored at 90 days
                    status_los_sens = case_when(length_of_stay_days_c>90~0, 
                                  length_of_stay_days_c<=90 & status_los=='censor' ~ 0, 
                                  length_of_stay_days_c<=90 & status_los=='Discharged alive' ~ 1, 
                                  length_of_stay_days_c<=90 & status_los=='Died in hospital' ~ 2), 
                    length_of_stay_days_c_sens = pmin(90, length_of_stay_days_c), 
                    #MET time and outcome censored at 90 days
                    status_met_sens = case_when(screening_to_met_days>90~0, 
                                  screening_to_met_days<=90 & status_met=='censor' ~ 0, 
                                  screening_to_met_days<=90 & status_met=='MET call' ~ 1, 
                                  screening_to_met_days<=90 & status_met=='Discharged alive' ~ 2, 
                                  screening_to_met_days<=90 & status_met=='Died in hospital' ~ 3), 
                    screening_to_met_days_sens = pmin(90, screening_to_met_days)) %>%
 mutate_at('clinical_team_fclty', ~as.character(.)) %>%
 mutate_at('status_los_sens', ~factor(., 0:2, labels=c('censor', 'Discharged alive', 'Died in hospital'))) %>%
 mutate_at('status_met_sens', ~factor(., 0:3, labels=c('censor', 'MET call', 'Discharged alive', 'Died in hospital')))

# switch one team to all usual care
if(itt == FALSE){
 dat_analysis = mutate(dat_analysis, 
            intervention = ifelse(clinical_team_fclty=='4', 0, intervention))
}


hospitals <-unique(dat_analysis$Hospital)

# outcome 1: bootstrap did not converge due to small number of events, so used cloglog link
# no seasonal adjustment
 mod.icu <- glmer(status_icu ~ intervention + study_week + age_scaled + sex + (1|clinical_team_fclty), family=binomial('cloglog'), data=dat_analysis)
 # no seasonal adjustment
 mod.icu_seas <- glmer(status_icu ~ intervention + study_week + age_scaled + sex + cosw + sinw + (1|clinical_team_fclty), family=binomial('cloglog'), data=dat_analysis)
 # set up predictions
 largest = dat_analysis %>% count(clinical_team_fclty) %>% slice_max(n) %>% pull(clinical_team_fclty)
 admitfrac = season::yrfraction(as.Date('2021-06-06')) #end of study
 pred_data = data.frame(intervention = c(0, 1), 
                        sex = 'Female', # lower probability; more common group
                        age_scaled = -2, # go for 75 year old due to rare events
                        sinw = sin(2*pi*admitfrac), 
                        cosw = cos(2*pi*admitfrac), 
                        clinical_team_fclty = largest, 
                        study_week = 53)
 icu_pred <- function(.) {predict(., newdata=pred_data)}
 
 # use.u = FALSE to ignore random effects
 bb = bootMer(mod.icu, FUN = icu_pred, nsim = n_boot, use.u=FALSE) # takes a long while
 bb_seas = bootMer(mod.icu_seas, FUN = icu_pred, use.u=FALSE, nsim = n_boot) # takes a long while
 
 # estimate probabilities and the difference
 # non-seasonal
 probs = as.data.frame(bb$t) %>%
   mutate(p_uc = inv.cloglog(`1`), # inverse cloglog
          p_in = inv.cloglog(`2`),
          diff = p_in - p_uc) # intervention minus usual care
 # seasonal
 probs_seas = as.data.frame(bb_seas$t) %>%
   mutate(p_uc = inv.cloglog(`1`), # inverse cloglog
          p_in = inv.cloglog(`2`),
          diff = p_in - p_uc) # intervention minus usual care
 
 # concatenate
 r1 = summarise(probs, Analysis = 'Intention-to-treat', phase = 'Usual care', Mean = mean(p_uc), lower = quantile(p_uc, 0.025), upper = quantile(p_uc, 0.975))
 r2 = summarise(probs, Analysis = 'Intention-to-treat', phase = 'Intervention', Mean = mean(p_in), lower = quantile(p_in, 0.025), upper = quantile(p_in, 0.975))
 r3 = summarise(probs, Analysis = 'Intention-to-treat', phase = 'Difference', Mean = mean(diff), lower = quantile(diff, 0.025), upper = quantile(diff, 0.975))
 r1s = summarise(probs_seas, Analysis = 'Sensitivity analysis', phase = 'Usual care', Mean = mean(p_uc), lower = quantile(p_uc, 0.025), upper = quantile(p_uc, 0.975))
 r2s = summarise(probs_seas, Analysis = 'Sensitivity analysis', phase = 'Intervention', Mean = mean(p_in), lower = quantile(p_in, 0.025), upper = quantile(p_in, 0.975))
 r3s = summarise(probs_seas, Analysis = 'Sensitivity analysis', phase = 'Difference', Mean = mean(diff), lower = quantile(diff, 0.025), upper = quantile(diff, 0.975))
 diff_icu = bind_rows(r1, r2, r3, r1s, r2s, r3s)
 # save
 save(diff_icu, file = outfile_icu)
  
#outcome 2

# ## crr
# crr_difference_los_died = risk_diff_crr(indata = dat_analysis, event_time='length_of_stay_days_c_sens', event_type='status_los_sens', outcome="Died in hospital", B = n_boot, pred_time = 90, dp=4)
# crr_difference_los_discharged = risk_diff_crr(indata = dat_analysis, event_time='length_of_stay_days_c_sens', event_type='status_los_sens', outcome="Discharged alive", B = n_boot, pred_time = 90, dp=4)
# names(crr_difference_los_died)<-names(crr_difference_los_discharged)<-paste('Day', pred_time)
# 
# crr_difference_los = list('Died in hospital'=bind_rows(crr_difference_los_died, .id='Prediction time'), 'Discharged alive'=bind_rows(crr_difference_los_discharged, .id='Prediction time')) %>%
#  bind_rows(.id='Hospital outcome')


##cph - cause-specific models with strata included
## Intention-to-treat
### overall
mod.los_died <- coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Died in hospital') ~ intervention + age_scaled + sex + strata(clinical_team_fclty), data=dat_analysis, id=study_id)
mod.los_discharged <- coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Discharged alive') ~ intervention + age_scaled + sex + strata(clinical_team_fclty), data=dat_analysis, id=study_id)

#risk_diff_cox
cph_difference_los_died = risk_diff_cox(indata=dat_analysis, inmodel=mod.los_died, dp=4, B=n_boot, pred_time = 90)
cph_difference_los_discharged = risk_diff_cox(indata=dat_analysis, inmodel=mod.los_discharged, dp=4, B=n_boot, pred_time = 90)


risk_difference_los_pp = list('Died in hospital'=cph_difference_los_died$risk_diff, 'Discharged alive'=cph_difference_los_discharged$risk_diff) %>% bind_rows(.id='Hospital outcome')
rmst_difference_los_pp = list('Died in hospital'=cph_difference_los_died$rmst_diff, 'Discharged alive'=cph_difference_los_discharged$rmst_diff) %>% bind_rows(.id='Hospital outcome')


##sensitivity analysis; seasonality terms added
mod.los_died <- coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Died in hospital') ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=dat_analysis, id=study_id)
mod.los_discharged <- coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Discharged alive') ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=dat_analysis, id=study_id)
cph_difference_los_died = risk_diff_cox(indata=dat_analysis, inmodel=mod.los_died, dp=4, B=n_boot, pred_time = 90)
cph_difference_los_discharged = risk_diff_cox(indata=dat_analysis, inmodel=mod.los_discharged, dp=4, B=n_boot, pred_time = 90)
risk_difference_los_sens = list('Died in hospital'=cph_difference_los_died$risk_diff, 
                'Discharged alive'=cph_difference_los_discharged$risk_diff) %>% bind_rows(.id='Hospital outcome') 
rmst_difference_los_sens= list('Died in hospital'=cph_difference_los_died$rmst_diff, 
                'Discharged alive'=cph_difference_los_discharged$rmst_diff) %>% bind_rows(.id='Hospital outcome')

#store results
risk_difference_los = list('Intention-to-treat'=risk_difference_los_pp, 'Sensitivity'=risk_difference_los_sens) %>% bind_rows(.id='Analysis')
rmst_difference_los = list('Intention-to-treat'=rmst_difference_los_pp, 'Sensitivity'=rmst_difference_los_sens) %>% bind_rows(.id='Analysis')
fixef_los = list('Died in hospital'=cph_difference_los_died$boot_fixef, 'Discharged alive'=cph_difference_los_discharged$boot_fixef) 

save(risk_difference_los, rmst_difference_los, fixef_los, 
   file = outfile_los)

#outcome 3: time-independent effect
mod.readmit <- coxph(Surv(discharge_to_readmit_days_c, status_readmit=="Readmitted to public hospital") ~ intervention + age_scaled + sex + strata(clinical_team_fclty), data=filter(dat_analysis, discharge_outcome=='Discharged alive'))
cph_difference_readmit_pp = risk_diff_cox(indata=dat_analysis, inmodel=mod.readmit, dp=4, B=n_boot, pred_time = 84)
mod.readmit <- coxph(Surv(discharge_to_readmit_days_c, status_readmit=="Readmitted to public hospital") ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=filter(dat_analysis, discharge_outcome=='Discharged alive'))
cph_difference_readmit_sens = risk_diff_cox(indata=dat_analysis, inmodel=mod.readmit, dp=4, B=n_boot, pred_time = 84)

risk_difference_readmit = list('Intention-to-treat'=cph_difference_readmit_pp$risk_diff, 'Sensitivity'=cph_difference_readmit_sens$risk_diff) %>% bind_rows(.id='Analysis')
rmst_difference_readmit= list('Intention-to-treat'=cph_difference_readmit_pp$rmst_diff, 'Sensitivity'=cph_difference_readmit_sens$rmst_diff) %>% bind_rows(.id='Analysis')
fixef_readmit = cph_difference_readmit_sens$boot_fixef 

#by hospital
hospitals <- unique(dat_analysis$Hospital)
mod.readmit_h <- lapply(hospitals, function(x) coxph(Surv(discharge_to_readmit_days_c, status_readmit=="Readmitted to public hospital") ~ intervention + age_scaled + sex + strata(clinical_team_fclty), data=dat_analysis, subset=(discharge_outcome=='Discharged alive' & Hospital==x), id=study_id))
names(mod.readmit_h)<-hospitals
cph_difference_readmit_pp_h = lapply(hospitals, function(x) risk_diff_cox(indata=dat_analysis, inmodel=mod.readmit_h[[x]], dp=4, B=n_boot, pred_time = 84))


rmst_difference_readmit_n= list('Intention-to-treat'=cph_difference_readmit_pp$rmst_diff, 'Sensitivity'=cph_difference_readmit_sens$rmst_diff) %>% bind_rows(.id='Analysis')



#cannot add tt() to cph

save(risk_difference_readmit, rmst_difference_readmit, fixef_readmit, 
   file = outfile_readmission)



#outcome 7
mod.met <- coxph(Surv(screening_to_met_days_sens, status_met_sens=="MET call") ~ intervention + age_scaled + sex + strata(clinical_team_fclty), data=dat_analysis)
cph_difference_met_pp = risk_diff_cox(indata=dat_analysis, inmodel=mod.met, dp=4, B=n_boot, pred_time = 90)
mod.met <- coxph(Surv(screening_to_met_days_sens, status_met_sens=="MET call") ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=dat_analysis)
cph_difference_met_sens = risk_diff_cox(indata=dat_analysis, inmodel=mod.met, dp=4, B=n_boot, pred_time = 90)

risk_difference_met = list('Intention-to-treat'=cph_difference_met_pp$risk_diff, 'Sensitivity'=cph_difference_met_sens$risk_diff) %>% bind_rows(.id='Analysis')
rmst_difference_met= list('Intention-to-treat'=cph_difference_met_pp$rmst_diff, 'Sensitivity'=cph_difference_met_sens$rmst_diff) %>% bind_rows(.id='Analysis')
fixef_met = cph_difference_met_sens$boot_fixef 

save(risk_difference_met, rmst_difference_met, fixef_met, 
   file = outfile_met)



# 
# #outcome 3 - need to change model within functions to tt() TODO
# 
# 
# #outcome 7
# pred_time = seq(10, 90, 10) # up to 90 days
# 
# ## cph
# mod.met <- coxph(Surv(screening_to_met_days_sens, status_met_sens=="MET call") ~ intervention + age_scaled + sex + strata(clinical_team_fclty), data=dat_analysis)
# risk_difference_met = lapply(pred_time, function(x) risk_diff_cox(indata=dat_analysis, inmodel=mod.met, dp=4, B=n_boot, pred_time = x))
# names(risk_difference_met)<-paste('Day', pred_time)
# risk_difference_met = bind_rows(risk_difference_met, .id='Prediction time')
# 
# save(risk_difference_met, file='bootstrap output/met.rda')
# 
# 
# 
# #ICU
# mod.icu_sens <- glmer(status_icu ~ intervention + study_week + age_scaled + sex + sinw + cosw + (1|clinical_team_fclty), family=binomial('logit'), data=dat_analysis)
# 
# ftab.icu_sens = summary(mod.icu_sens)$coefficients %>% as.data.frame() %>% rownames_to_column(var='term') %>%
#  select(term, Estimate)
# 
# #bootstrap
# dat_analysis = mutate(dat_analysis, 
#            status_los_sens = case_when(length_of_stay_days_c>90~0, 
#                          length_of_stay_days_c<=90 & status_los=='censor' ~ 0, 
#                          length_of_stay_days_c<=90 & status_los=='Discharged alive' ~ 1, 
#                          length_of_stay_days_c<=90 & status_los=='Died in hospital' ~ 2), 
#            length_of_stay_days_c_sens = pmin(90, length_of_stay_days_c)) %>%
#  mutate_at('status_los_sens', ~factor(., 0:2, labels=c('censor', 'Discharged alive', 'Died in hospital')))
# 
# mod.los_sens <- coxph(Surv(length_of_stay_days_c_sens, status_los_sens) ~ intervention + age_scaled + sex + sinw +cosw , data=dat_analysis, id=study_id)
# 
# 
# B=100
# boot_est <- function(.){coef(.)}
# 
# 
# 
# mod.los_boot <- boot::boot(mod.los_sens, boot_est, R=B)
