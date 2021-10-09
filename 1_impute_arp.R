# 1_impute_arp.R
# impute the missing ARP data using a Bayesian multinomial model
# September 2021
library(R2WinBUGS)
library(dplyr)
library(tidyverse)
library(TeachingDemos)
source('98_survival_function.R') # for survival

# get the data
load('data/FullData.RData') # from 0_read_data_redcap.R

# add baseline variables to care directive
vars_in_care_directive = names(care_directive)
to_add = select(baseline, participant_id, at_risk, age, pt_sex, team, presence_eol_plan, int_hosp_trans, spict_score, cristal_score)#starts_with('cristal_'), starts_with('spict_')) # more here
care_directive = left_join(care_directive, to_add, by='participant_id') %>%
  filter(at_risk == 'At risk') %>% # must be at risk
  mutate(row = 1:n()) # for merging with imputed data (must be done after at-risk filter applied)
for_imputation = select(care_directive, -contains('date_time'), -'care_directive_other', -'redcap_version') %>%
  mutate(team = factor(team)) %>%
  arrange(row)

## use Bayesian model to impute ARP
# write the code
bfile = 'bugs_impute_model.txt'
obugs = file(bfile, 'w')
cat('model
	{
	##  PRIORS
	# intercepts
	  alpha[1] <- 0;       # zero contrast corner-point
	  for (k in 2 : K) { alpha[k] ~ dnorm(0, 0.00001)} # vague priors
 	 # Loop around predictors
 	 for (i in 1:P){
  	  beta[1,i] <- 0;       # zero contrast corner-point
	    for (k in 2 : K) { beta[k,i] ~ dnorm(0, 0.00001)} # vague priors
 	 }
	# random intercepts
	for (t in 1:T){
	  gamma[1, t] <- 0
	  gamma_c[1, t] <- 0
  	for (k in 2:3){ # other 2 ARP categories
  	  gamma[k, t] ~ dnorm(0, tau.gamma[k])
  	  gamma_c[k, t] <- gamma[k, t] - mu.gamma[k] # centre for convergence
  	}
	}
	tau.gamma[2] ~ dgamma(0.01, 0.01)
	tau.gamma[3] ~ dgamma(0.01, 0.01)
	mu.gamma[2] <- mean(gamma[2, 1:T])
	mu.gamma[3] <- mean(gamma[3, 1:T])
	  
  # LIKELIHOOD	
  for (i in 1 : N) { # loop through patients

	# Multinomial response
         ARP[i,1:K] ~ dmulti( p[i,1:K] , 1 )
         for (k in 1:K) {     # loop over ARP categories
            p[i,k]        <- phi[i,k] / sum(phi[i,])
            log(phi[i,k]) <- alpha[k] + gamma_c[k,team[i]] +  (beta[k,1] * X[i,1]) + 
            (beta[k,2] * X[i,2]) + 
            (beta[k,3] * X[i,3]) + 
            (beta[k,4] * X[i,4])
           }
  	  }
}', file=obugs)
close(obugs)

## data for bugs
N = nrow(for_imputation)
# ARP as matrix with patients missing their ARP
ARP = model.matrix.lm(hospital ~ -1 +existing_arp, data=for_imputation, na.action = "na.pass")
K = ncol(ARP)
X = mutate(for_imputation,
           age = (age - 83)/5, # standardise for convergence
           cristal_score = cristal_score - 5,
           spict_score = spict_score - 2,
           pt_sex = as.numeric(pt_sex == 'Female')) %>%
  select(age, pt_sex, cristal_score, spict_score) %>%
  as.matrix()
team = as.numeric(as.factor(for_imputation$team))
T = max(team) # number of teams
P = ncol(X) # number of predictors
bdata = list(N = N, 
             K = K,
             P = P,
             T = T,
             X = X, 
             team = team,
             ARP = ARP
             )
             
## MCMC options
n.chains = 1
MCMC = 2000
thin = 4
seed = char2seed('Newport')
debug = FALSE

## initial values
#
beta = matrix(data=0, ncol=P, nrow=K)
beta[1,] = NA
inits = list(alpha = c(NA, rep(0, K-1)), beta=beta, tau.gamma=c(NA,1,1))
inits = rep(list(inits), n.chains) # repeat per chains

parms = c('alpha','beta','gamma_c','gamma','tau.gamma') # need both gamma and gamma_c
exists = length(dir('results/bugs_impute.RData')) > 0
if(exists==FALSE){
  bugs = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
            n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
            bugs.directory="c:/Program Files/WinBUGS14")
  save(bugs, file='results/bugs_impute.RData')
}
if(exists==TRUE){
  load('results/bugs_impute.RData')
}

## use Rmarkdown to create summary report
out_file = '1_imputation_results.pdf'
attachment = paste(out_file, sep='') 
rmarkdown::render(input = "1_imputation_results.Rmd",
                  output_format = "pdf_document",
                  output_file = attachment)

## do another smaller run starting from previous to give imputations for ARP
# get initial values from end of previous chain and add NAs
inits = bugs$last.values
inits[[1]]$alpha = c(NA,inits[[1]]$alpha)
inits[[1]]$beta = rbind(rep(NA,P), inits[[1]]$beta)
inits[[1]]$gamma = rbind(rep(NA,T), inits[[1]]$gamma)
inits[[1]]$tau.gamma = c(NA,inits[[1]]$tau.gamma)
inits[[1]]$gamma_c = NULL
MCMC = 10 # number of imputations
thin = 20 # big gaps between imputations
parms = c('ARP')
exists = length(dir('results/bugs_impute_arp.RData')) > 0
if(exists==FALSE){
  bugs2 = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                      n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                      bugs.directory="c:/Program Files/WinBUGS14")
  save(bugs2, file='results/bugs_impute_arp.RData')
}
if(exists==TRUE){
  load('results/bugs_impute_arp.RData')
}

# reconstruct ARP from MCMC samples
arp_imputed = reshape2::melt(bugs2$sims.matrix) %>%
  mutate(Var2 = as.character(Var2),
         strip = str_remove_all(Var2, '[^0-9|,]')) %>%
  separate(col=strip, into=c('row','existing_arp_imputed'), sep=',', convert=TRUE) %>%  # split on comma, can ignore warnings
  rename('chain' = 'Var1') %>%
  filter(value !=0) %>% # just get one positive response per patient (row) and chain
  select(-Var2, -value) %>%
  mutate(existing_arp_imputed = factor(existing_arp_imputed, levels=1:3, labels=c('Yes, valid','Yes, invalid','No, none')))
cat("There are ", nrow(arp_imputed) / MCMC, " patients with imputed data.\n", sep='')
table(arp_imputed$existing_arp_imputed) # quick check

# make imputed versions of the data
care_directive_imputed = list()
for(k in 1:MCMC){
  # start with complete case data
  care_directive_imp = care_directive
  # get imputations
  to_impute = filter(arp_imputed, chain==k) %>% select(-chain)
  # merge by row
  care_directive_imp = left_join(care_directive_imp, to_impute, by='row') %>%
    mutate(existing_arp = coalesce(existing_arp, existing_arp_imputed)) %>%
    select(-existing_arp_imputed)
  #
  care_directive_imputed[[k]] = care_directive_imp
}

## now add imputations to survival data
survival_data = filter(survival_data, outcome != 'care_directive') # remove from survival data
## need to re-run function
load('data/date_changes.RData') # key date & date/time changes for stepped-wedged design from 0_date_changes.R
survival_data1 = survival_data2 = list()
for (k in 1:MCMC){
  # now get the survival data (function takes a little while)
  surv_data_both = make_survival_times(
    indata = baseline,
    date_changes_time = date_changes_time,
    form = care_directive_imputed[[k]] , # using imputed data
    change_var = 'change_5',
    form_date = 'start_date_time_5',
    outcome_date = 'date_time_5', 
    at_risk_date = 'admission_datetime')  # assuming at risk from admission
  # make both versions of the survival data (with and without discharge censoring)
  survival_data1[[k]] = mutate(surv_data_both$version1, outcome = 'care_directive')
  survival_data2[[k]] = mutate(surv_data_both$version2, outcome = 'care_directive')
}  

## add intervention time and at-risk status ##
int_time_data = select(baseline, participant_id, at_risk, int_time) # smaller data with just intervention time (used below)
for (k in 1:MCMC){
  # a) version 1 with no competing risk of discharge
  survival_data1[[k]] = left_join(survival_data1[[k]], int_time_data, by='participant_id') %>% # add intervention time (categorical variable to survival data)
    filter(at_risk == 'At risk') %>% # only those at risk
    select(-at_risk) %>% # no longer needed
    mutate(hospital = str_sub(participant_id, 1, 4)) 
  # b) version 2 with competing risk of discharge
  survival_data2[[k]] = left_join(survival_data2[[k]], int_time_data, by='participant_id') %>% # add intervention time (categorical variable to survival data)
    filter(at_risk == 'At risk') %>% # only those at risk
    select(-at_risk) %>% # no longer needed
    mutate(hospital = str_sub(participant_id, 1, 4)) 
}

## last fix for survival data
# Sep 2021, combined version that uses version 2 for RBWH/GCUH and version 1 for some TPCH
library(readxl)
tpch_version2 = read_excel('data/InterACTTPCHV3-AtRiskWithNoFollowUps_all.xlsx') # REDCap IDs where outcome 4, 5 and 6 were completed retrospectively
names(tpch_version2) = 'participant_id'
tpch_version2 = mutate(tpch_version2, participant_id = paste('TPCH_V3_', participant_id, sep=''))
# IDs for version 2
all_ids = data.frame(participant_id = unique(baseline$participant_id))
all_ids = mutate(all_ids,
                 version = case_when(
                   str_sub(participant_id,1,4) == 'RBWH' ~ 2, # if RBWH ...
                   str_sub(participant_id,1,4) == 'GCUH' ~ 2, # ... or if GCUH ...
                   participant_id %in% tpch_version2$participant_id ~ 2, # ... or if selected from TPCH
                   TRUE ~ 1 # otherwise # (some in TPCH)
                 ))
id_v1 = filter(all_ids, version==1) %>% pull(participant_id)
id_v2 = filter(all_ids, version==2) %>% pull(participant_id)
survival_data_imputed = list()
for (k in 1:MCMC){
  s_data1 = filter(survival_data1[[k]], participant_id %in% id_v1)
  s_data2 = filter(survival_data2[[k]], participant_id %in% id_v2)
  survival_data_imputed[[k]] = bind_rows(s_data1, s_data2)
}

    
## save
save(thresholds, data_date, spict.vars, cristal.vars, baseline, complete, 
     survival_data, survival_data_imputed,
     care_directive_imputed,
     care_directive, palliative_care_referral, clinicianled_review, file='data/FullData_imputed.RData')
