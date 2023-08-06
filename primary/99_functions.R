#99_functions.R

inv.cloglog = function(x){1 - exp(-exp(x))} # inverse cloglog

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

g.theme =   theme_bw() + theme(legend.text = element_text(size=12),axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),legend.position = 'top',legend.direction = 'horizontal',legend.title = element_blank(),panel.grid.minor = element_blank())

FitFlextableToPage <- function(ft, pgwidth = 6){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

roundz = function(x, digits){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}


#risk difference functions - need to adapt

## function to calculate risk difference for survival models, with bootstrap CIs
risk_diff_crr = function(
    indata = NULL,
    event_time = NULL,
    event_type = NULL,
    outcome = NULL,
    dp = 2, # decimal places
    B = 100, # number of bootstrap statistics
    pred_time = 0 # time to predict difference at
){
  
  # prepare data
  indata = mutate(indata, 
                  teamf = as.numeric(as.factor(clinical_team_fclty))) %>%
    rename('time'=!!(event_time),'eventSimple'=!!(event_type)) %>%
    mutate_at('eventSimple',~ifelse(.==outcome,'Yes',.))
  
  # make covariate matrix (fixed variables)
  cov1 = select(indata, intervention, age_scaled, sex) %>% mutate(sex = as.numeric(sex == 'Female')) %>% as.matrix()
  
  # fit cumulative model
  crr_model = with(indata, crr(ftime=time, fstatus=eventSimple, cov1 = cov1, cengroup=teamf, failcode='Yes', cencode='Censored'))
  
  # predictions at centred values (see above)
  pred_data = as.matrix(data.frame(intervention=c(0,1), age_scaled=0, sex=0))
  pred_sur = predict(crr_model, cov1=pred_data)
  # time just before prediction time
  index = max(which(pred_sur[,1] - pred_time < 0)) 
  pred_sur = pred_sur[index,2:3] # just take cumulative probabilities
  diff = pred_sur[2] - pred_sur[1] # intervention minus usual care
  pred_sur = c(pred_sur, diff)
  
  # bootstrap intervals
  boots <- boot(data = indata,
                cov = cov1,
                statistic = rdnnt_crr, # see other function
                R = B, # number of bootstraps
                pred_time = pred_time,
                pred_data = pred_data)
  # intervals for ...
  ci1 = boot.ci(boots, conf=0.95, type= 'norm', index=1)  # ... intervention
  ci1 = as.numeric(ci1$normal[2:3]) # just extract CIs
  ci2 = boot.ci(boots, conf=0.95, type= 'norm', index=2)  # ... usual care
  ci2 = as.numeric(ci2$normal[2:3]) # just extract CIs
  ci3 = boot.ci(boots, conf=0.95, type= 'norm', index=3)  # ... difference
  ci3 = as.numeric(ci3$normal[2:3]) # just extract CIs
  ci = data.frame(rbind(ci1, ci2, ci3))
  names(ci) = c('lower','upper')
  #
  preds = data.frame(Phase = c('Usual care','Intervention','Difference'),
                     Event = pred_sur) %>%
    bind_cols(ci) %>%
    mutate(
      Event = roundz(Event, dp), # 
      lower = roundz(lower, dp),
      upper = roundz(upper, dp),
      CI = paste(lower, ' to ', upper, sep='')) %>%
    select(Phase, Event, CI)
  rownames(preds) = NULL
  return(preds)
}

## function for bootstrap confidence intervals for absolute risk difference in cumulative models
# adapted from https://atm.amegroups.com/article/viewFile/18926/pdf
rdnnt_crr <- function(data, 
                      cov, # covariate matrix
                      ii, 
                      pred_time, # time to predict at
                      pred_data) # data frame for predictions
{
  dd <- data[ii,]; # allows boot to select a sample
  cc <- cov[ii,]; # allows boot to select a sample
  
  # fit cumulative model
  crr_model = with(dd, crr(ftime=time, fstatus=eventSimple, cov1 = cc, cengroup=teamf, failcode='Yes', cencode='censor'))
  # predictions at centred values (see above)
  pred_sur = predict(crr_model, cov1=pred_data)
  # time just before prediction time
  index = max(which(pred_sur[,1] - pred_time < 0)) 
  pred_sur = pred_sur[index,2:3] # just take cumulative probabilities
  diff = pred_sur[2] - pred_sur[1] # intervention minus usual care
  pred_sur = c(pred_sur, diff) # add difference
  return(pred_sur);
}


## function to calculate risk difference for survival models, with bootstrap CIs
## cph does not allow for competing events
risk_diff_cox = function(
    indata = NULL,
    inmodel = NULL, # survival model, used to get formula
    dp = 2, # decimal places
    B = 100, # number of boostrap statistics
    pred_time = 0 # time to predict difference at
){
  # see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015956/
  # find largest team
  tab = table(indata$clinical_team_fclty)
  tab = tab[order(-tab)]
  largest = names(tab[1])
  admitfrac = season::yrfraction(as.Date('2021-06-06')) #end of study
  # adjust formula
  char_formula = as.character(inmodel$call)
  #old = 'I\\(int_time == \"Intervention\"\\)'
  #char_formula = str_replace(char_formula, pattern=old, replacement = 'int_time_n')
  new_formula = str_replace(string=char_formula[2], pattern='strata', replacement='strat')
  # crr does not allow strata
  #paste(c(char_formula[2], '~', str_replace(string=char_formula[3], pattern='strata', replacement='strat')), collapse=' ') # rms uses strat not strata
  # refit Cox model using rms package
  cox_rms = cph(as.formula(new_formula), data = indata, surv=TRUE, x=TRUE, y=TRUE)
  
  # set up predictions, age_scaled at 0 (85 years), Female, largest team
  pred_data = data.frame(intervention = c(0,1), sex = 'Female', age_scaled=0, sinw=sin(2*pi*admitfrac), cosw=cos(2*pi*admitfrac), clinical_team_fclty=largest)
  #pred_sur = survest(cox_rms, newdata=pred_data, times=pred_time)
  surv_output = summary(survfit(cox_rms, newdata=pred_data), times=pred_time, extend=TRUE)
  pred_sur = tibble(intervention=as.numeric(surv_output$strata)-1, 
                    surv=surv_output$surv,
                    lower=surv_output$lower,
                    upper=surv_output$upper,
                    rmst=surv_output$table[,"rmean"])
  
  # bootstrap intervals
  boots <- boot(data = indata,
                statistic = rdnnt_cox, # see other function
                R = B, # number of bootstraps
                frm = as.formula(new_formula),
                pred_time = pred_time,
                pred_data = pred_data)
  
  #risk difference
  npar = ncol(boots$t)
  ci_risk = boot.ci(boots, conf=0.95, type= 'norm',index=1)$normal[2:3]
  ci_time = boot.ci(boots, conf=0.95, type= 'norm',index=2)$normal[2:3]

  #ci = data.frame(out = c('risk_diff','rmst_diff'),lower=c(ci_risk[1],ci_time[1]),upper=c(ci_risk[2],ci_time[2]))

  #bootstrapped means
  mean.boot = apply(boots$t,2,mean)
  
  preds = as.data.frame(pred_sur) %>%
    mutate(
      Event = roundz(1 - surv, dp), # 1 minus to give those experiencing event
      lower = roundz(1 - lower, dp),
      upper = roundz(1 - upper, dp),
      CI = paste(lower, ' to ', upper, sep=''),
      Phase = c('Usual care','Intervention')) %>%
    select(Phase, Event, CI)
  
  
  # difference
  frame = data.frame(Phase = 'Difference', 
                     Event = roundz(diff(1-pred_sur$surv), dp), # to give intervention minus usual care
                     CI = paste(roundz(ci_risk[1],dp), ' to ', roundz(ci_risk[2],dp), sep=''))
  to_table = bind_rows(preds, frame)
  
  
  predt = tibble(Phase = c('Usual care','Intervention'),RMST=roundz(pred_sur$rmst,dp)) %>% spread(Phase,RMST)
  predt_diff = paste(roundz(mean.boot[2],dp),';',roundz(ci_time[1],dp), ' to ', roundz(ci_time[2],dp), sep='')
  to_table_t = predt %>% add_column('Difference (Intervention - Usual care)'=predt_diff) %>% select(`Usual care`,Intervention,`Difference (Intervention - Usual care)`)
  
  boot_fixef = boots$t[,-(1:2)]; colnames(boot_fixef)<-names(coef(cox_rms))
  return(list(risk_diff = to_table, rmst_diff=to_table_t, boot_fixef=boot_fixef))
}



## function for bootstrap confidence intervals for absolute risk difference in Cox model
# adapted from https://atm.amegroups.com/article/viewFile/18926/pdf
rdnnt_cox <- function(data, 
                      ii, 
                      frm, # formula 
                      pred_time, # time to predict at
                      pred_data) # data frame for predictions
{
  dd <- data[ii,]; # allows boot to select a sample
  cfit <- cph(formula = frm, data=dd,
              surv=TRUE, x=TRUE, y=TRUE);
  surv_output = summary(survfit(cfit,newdata=pred_data),times=pred_time,extend=T)
  pred_sur = tibble(intervention=as.numeric(surv_output$strata)-1,
                    surv=surv_output$surv,
                    lower=surv_output$lower,
                    upper=surv_output$upper,
                    rmst=surv_output$table[,"rmean"])

  RD <- diff(1-pred_sur$surv) # risk difference (intervention minus usual care)
  LD <- diff(pred_sur$rmst)
  CF <- c(coef(cfit))
  #cat('.'); # updater
  return(c(RD,LD,CF));
}






# 
# 
# 
# ## function to calculate adjusted restricted mean survival difference
# #rename('time'=!!(event_time),'eventSimple'=!!(event_type)
# rmst_diff = function(
#     indata = NULL,
#     pred_time = 0, # time to predict difference at
#     event_time,
#     event_type = NULL,
#     outcome = NULL,
# ){
#   
#   # prepare data for rmst function
#   for_rmst = mutate(indata,status = as.numeric(get(event_type)==outcome)) %>% mutate_at('sex',~as.numeric(.=="Female")) %>% rename('time'=!!(event_time)) %>%
#     filter(time>=0)
#   # set up covariate matrix
#   x = data.frame(for_rmst[, c('age_scaled','sex')]) 
#   # run function
#   res = rmst2(
#     time = for_rmst$time,
#     status = for_rmst$status, 
#     arm = for_rmst$intervention,
#     tau = pred_time,
#     covariates = x
#   )
#   # tidy up estimates
#   ests = data.frame(res$RMST.difference.adjusted) %>%
#     janitor::clean_names()
#   ests$var = row.names(ests); row.names(ests) = NULL
#   ests = filter(ests, var != 'intercept') %>%
#     select(-se_coef, -z)
#   
#   return(ests)
#   
# } # end of function
