# 5_bootstrapped_outputs_byhospital.R
# takes a while to run
# June 2023
n_boot = 3000 # number of bootstrap samples for risk difference, increase to 3000 for final run
TeachingDemos::char2seed('Germany')
source('99_packages.R')
source('99_functions.R')
source('../99_functions_not_shared.R') # for blinding
#
load('analysis_ready/final_dataset.rda')
# AGB, change one team to usual care only as they never got the emails
itt = FALSE # intention-to-treat, run twice (TRUE and FALSE)
# filenames dependent on ITT
outfile_los = 'bootstrap output/length of stay by hospital.rda'
outfile_met = 'bootstrap output/met by hospital.rda'
outfile_readmission = 'bootstrap output/hospital readmission by hospital.rda'
if(itt==TRUE){
  outfile_los = 'bootstrap output/length of stay by hospital itt.rda'
  outfile_met = 'bootstrap output/met by hospital itt.rda'
  outfile_readmission = 'bootstrap output/hospital readmission by hospital itt.rda'
}

#outcome 1: did not converge

#outcomes 2,3 and 7

dat_analysis = dat_analysis %>% mutate(Hospital = str_remove_all(study_id,'_.*'),
                                       Hospital = deidentify_hospital(Hospital), # blinded, AGB
                                       #seasonality terms: sin and cos
                                       admit_frac = season::yrfraction(admit_date),
                                       sinw = sin(2*pi*admit_frac),cosw=cos(2*pi*admit_frac),
                                       #LOS and outcome censored at 90 days
                                       status_los_sens = case_when(length_of_stay_days_c>90~0,
                                                                   length_of_stay_days_c<=90 & status_los=='censor' ~ 0,
                                                                   length_of_stay_days_c<=90 & status_los=='Discharged alive' ~ 1,
                                                                   length_of_stay_days_c<=90 & status_los=='Died in hospital' ~ 2),
                                       length_of_stay_days_c_sens = pmin(90,length_of_stay_days_c),
                                       #MET time and outcome censored at 90 days
                                       status_met_sens = case_when(screening_to_met_days>90~0,
                                                                   screening_to_met_days<=90 & status_met=='censor' ~ 0,
                                                                   screening_to_met_days<=90 & status_met=='MET call' ~ 1,
                                                                   screening_to_met_days<=90 & status_met=='Discharged alive' ~ 2,
                                                                   screening_to_met_days<=90 & status_met=='Died in hospital' ~ 3),
                                       screening_to_met_days_sens = pmin(90,screening_to_met_days)) %>%
  mutate_at('clinical_team_fclty',~as.character(.)) %>%
  mutate_at('status_los_sens',~factor(.,0:2,labels=c('censor','Discharged alive','Died in hospital'))) %>%
  mutate_at('status_met_sens',~factor(.,0:3,labels=c('censor','MET call','Discharged alive','Died in hospital')))

hospitals <-unique(dat_analysis$Hospital)




#outcome 2: LOS
## per-protocol

mod.los_died_h <- lapply(hospitals,function(x) coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Died in hospital') ~ intervention + age_scaled + sex +  strata(clinical_team_fclty), data=dat_analysis,subset=(Hospital==x),id=study_id))
mod.los_discharged_h <- lapply(hospitals,function(x) coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Discharged alive') ~ intervention + age_scaled + sex +  strata(clinical_team_fclty), data=dat_analysis,subset=(Hospital==x),id=study_id))

cph_difference_los_died_h = lapply(seq_along(hospitals),function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.los_died_h[[x]],dp=4,B=n_boot,pred_time = 90))
cph_difference_los_discharged_h = lapply(seq_along(hospitals), function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.los_discharged_h[[x]],dp=4,B=n_boot,pred_time = 90))

risk_difference_los_pp_h = list('Died in hospital'=lapply(seq_along(hospitals),function(x) cph_difference_los_died_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                                'Discharged alive'=lapply(seq_along(hospitals),function(x) cph_difference_los_discharged_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Hospital outcome')

rmst_difference_los_pp_h = list('Died in hospital'=lapply(seq_along(hospitals),function(x) cph_difference_los_died_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                                'Discharged alive'=lapply(seq_along(hospitals),function(x) cph_difference_los_discharged_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Hospital outcome')


## sensitivity
mod.los_died_h <- lapply(hospitals,function(x) coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Died in hospital') ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=dat_analysis,subset=(Hospital==x),id=study_id))
mod.los_discharged_h <- lapply(hospitals,function(x) coxph(Surv(length_of_stay_days_c_sens, status_los_sens=='Discharged alive') ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=dat_analysis,subset=(Hospital==x),id=study_id))

cph_difference_los_died_h = lapply(seq_along(hospitals),function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.los_died_h[[x]],dp=4,B=n_boot,pred_time = 90))
cph_difference_los_discharged_h = lapply(seq_along(hospitals), function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.los_discharged_h[[x]],dp=4,B=n_boot,pred_time = 90))

risk_difference_los_sens_h = list('Died in hospital'=lapply(seq_along(hospitals),function(x) cph_difference_los_died_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                                  'Discharged alive'=lapply(seq_along(hospitals),function(x) cph_difference_los_discharged_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Hospital outcome')
rmst_difference_los_sens_h = list('Died in hospital'=lapply(seq_along(hospitals),function(x) cph_difference_los_died_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                                  'Discharged alive'=lapply(seq_along(hospitals),function(x) cph_difference_los_discharged_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Hospital outcome')

#combined results
risk_difference_los_h = list('Per-protocol'=risk_difference_los_pp_h,'Sensitivity'=risk_difference_los_sens_h) %>% bind_rows(.id='Analysis')
rmst_difference_los_h = list('Per-protocol'=rmst_difference_los_pp_h,'Sensitivity'=rmst_difference_los_sens_h) %>% bind_rows(.id='Analysis')

#save
save(risk_difference_los_h, rmst_difference_los_h, 
     file = outfile_los)

### end outcome 2 ###

#outcome 3: time to readmission
mod.readmit_h <- lapply(hospitals,function(x) coxph(Surv(discharge_to_readmit_days_c, status_readmit=="Readmitted to public hospital") ~ intervention + age_scaled + sex + strata(clinical_team_fclty),data=filter(dat_analysis,discharge_outcome=='Discharged alive'),subset=(Hospital==x)))
cph_difference_readmit_pp_h = lapply(seq_along(hospitals), function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.readmit_h[[x]],dp=4,B=n_boot,pred_time = 84))

mod.readmit_h <- lapply(hospitals,function(x) coxph(Surv(discharge_to_readmit_days_c, status_readmit=="Readmitted to public hospital") ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty),data=filter(dat_analysis,discharge_outcome=='Discharged alive'),subset=(Hospital==x)))
cph_difference_readmit_sens_h = lapply(seq_along(hospitals), function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.readmit_h[[x]],dp=4,B=n_boot,pred_time = 84))

#up to here
risk_difference_readmit_h = list('Per-protocol'=lapply(seq_along(hospitals),function(x) cph_difference_readmit_pp_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                               'Sensitivity'=lapply(seq_along(hospitals),function(x) cph_difference_readmit_sens_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Analysis')

rmst_difference_readmit_h= list('Per-protocol'=lapply(seq_along(hospitals), function(x) cph_difference_readmit_pp_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                                'Sensitivity'=lapply(seq_along(hospitals), function(x) cph_difference_readmit_sens_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Analysis')

#save
save(risk_difference_readmit_h, rmst_difference_readmit_h,
     file=outfile_readmission)

### end outcome 3 ###

#outcome7: met
mod.met_h <- lapply(hospitals,function(x) coxph(Surv(screening_to_met_days_sens, status_met_sens=="MET call") ~ intervention + age_scaled + sex +  strata(clinical_team_fclty), data=filter(dat_analysis,Hospital==x)))
cph_difference_met_pp_h = lapply(seq_along(hospitals), function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.met_h[[x]],dp=4,B=n_boot,pred_time = 90))

mod.met_h <- lapply(hospitals,function(x) coxph(Surv(screening_to_met_days_sens, status_met_sens=="MET call") ~ intervention + age_scaled + sex + sinw + cosw + strata(clinical_team_fclty), data=filter(dat_analysis,Hospital==x)))
cph_difference_met_sens_h = lapply(seq_along(hospitals), function(x) risk_diff_cox(indata=filter(dat_analysis,Hospital==hospitals[x]),inmodel=mod.met_h[[x]],dp=4,B=n_boot,pred_time = 90))
#upt o ehre
risk_difference_met_h = list('Per-protocol'=lapply(seq_along(hospitals), function(x) cph_difference_met_pp_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                             'Sensitivity'=lapply(seq_along(hospitals), function(x) cph_difference_met_sens_h[[x]]$risk_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Analysis')

rmst_difference_met_h = list('Per-protocol'=lapply(seq_along(hospitals), function(x) cph_difference_met_pp_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals)),
                             'Sensitivity'=lapply(seq_along(hospitals), function(x) cph_difference_met_sens_h[[x]]$rmst_diff) %>% bind_rows(.id='Hospital') %>% mutate_at('Hospital',~factor(.,levels=1:3,labels=hospitals))) %>% bind_rows(.id='Analysis')

save(risk_difference_met_h, rmst_difference_met_h,
     file = outfile_met)
