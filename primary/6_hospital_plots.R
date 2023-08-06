#6_hospital_plots.R
#code copied from 5_outcome_analysis.R
#cumulative probability plots for outcomes 2, 3 and 7
source('99_packages.R')
source('99_functions.R')

load('analysis_ready/final_dataset.rda')

dat_analysis = dat_analysis %>% mutate(Hospital = str_remove_all(study_id,'_.*'),study_period = case_when(intervention==0~'Usual care',intervention==1~'Intervention exposure') %>% factor(.,level=c('Usual care','Intervention exposure')))
dat_analysis = mutate_at(dat_analysis,'clinical_team_fclty',~as.character(.))

################### OUTCOME 2 ###################
dat_analysis = mutate(dat_analysis,
                      status_los_sens = case_when(length_of_stay_days_c>90~0,
                                                  length_of_stay_days_c<=90 & status_los=='censor' ~ 0,
                                                  length_of_stay_days_c<=90 & status_los=='Discharged alive' ~ 1,
                                                  length_of_stay_days_c<=90 & status_los=='Died in hospital' ~ 2),
                      length_of_stay_days_c_sens = pmin(90,length_of_stay_days_c)) %>%
  mutate_at('status_los_sens',~factor(.,0:2,labels=c('censor','Discharged alive','Died in hospital')))


hospitals <- unique(dat_analysis$Hospital)
mod.cif_los <- lapply(hospitals,function(x)
  with(filter(dat_analysis,Hospital==x),cuminc(length_of_stay_days_c_sens,status_los_sens,group=intervention,strata=as.character(clinical_team_fclty),cencode='censor')))

g<-lapply(1:3, function(x) ggcompetingrisks(mod.cif_los[[x]],
                                            gnames = c('Usual care:Discharged alive','Intervention exposure:Discharged alive','Usual care:Died in hospital','Intervention exposure:Died in hospital'),
                                            gsep=':',
                                            xlab = 'Days since first recorded as high-risk CriSTAL or SPICT-positive',
                                            ylab = 'Cumulative probability',
                                            title = '',
                                            legend.title='',
                                            risk.table = TRUE,
                                            conf.int = TRUE))
names(g) <- hospitals

#save a plot for each hospital
plot_list = list()
for(i in seq_along(hospitals)){
  p = g[[i]]$data %>%
    mutate_at("name",~str_wrap(.,30)) %>%
    ggplot(aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=name,colour=group,fill=group,linetype=event))+
    geom_ribbon(alpha=.2)+geom_path(size=1.25)+
    scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)+
    scale_x_continuous('Days since first recorded as high-risk CriSTAL or SPICT-positive',breaks=seq(0,90,10))+
    expand_limits(y=1)+scale_y_continuous('Cumulative probability',breaks=seq(0,1,0.1))+g.theme
  plot_list[[i]] = p
}
names(plot_list)<-hospitals

# Save plots to png. Makes a separate file for each plot.
for (i in seq_along(hospitals)) {
  file_name = paste("plots for Chris/Outcome_2_", hospitals[i], ".png", sep="")
  png(file_name,height=10,width=10,units = 'in',res=600)
  print(plot_list[[i]])
  dev.off()
}

################### OUTCOME 3 ###################
mod.cif_readmit <- lapply(hospitals,function(x)
  with(filter(dat_analysis,Hospital==x,discharge_outcome=='Discharged alive'),cuminc(discharge_to_readmit_days_c,status_readmit,strata=as.character(clinical_team_fclty),group=intervention,cencode='censor')))

g<-lapply(1:3, function(x) ggcompetingrisks(mod.cif_readmit[[x]],
                                            gnames = c('Usual care:Readmitted','Intervention exposure:Readmitted','Usual care:Died','Intervention exposure:Died'),
                                            gsep=':',
                                            xlab = 'Days since discharge alive from hospital',
                                            ylab = 'Cumulative probability',
                                            title = '',
                                            legend.title='',
                                            risk.table = TRUE,
                                            conf.int = TRUE))

names(g) <- hospitals

plot_list = list()
for(i in seq_along(hospitals)){
p = g[[i]]$data %>%
  mutate_at("name",~str_wrap(.,30)) %>%
  ggplot(aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=name,colour=group,fill=group,linetype=event))+
  geom_ribbon(alpha=.2)+geom_path(size=1.25)+
  scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)+
  scale_x_continuous('Days since discharge alive from hospital',breaks=seq(0,90,10))+
  expand_limits(y=0.6)+scale_y_continuous('Cumulative probability',breaks=seq(0,0.6,0.05))+g.theme

plot_list[[i]] = p
}

names(plot_list)<-hospitals

# Save plots to png. Makes a separate file for each plot.
for (i in seq_along(hospitals)) {
  file_name = paste("plots for Chris/Outcome_3_", hospitals[i], ".png", sep="")
  png(file_name,height=10,width=10,units = 'in',res=600)
  print(plot_list[[i]])
  dev.off()
}


################### OUTCOME 7 ###################
dat_analysis = mutate(dat_analysis,
                      status_met_sens = case_when(screening_to_met_days>90~0,
                                                  screening_to_met_days<=90 & status_met=='censor' ~ 0,
                                                  screening_to_met_days<=90 & status_met=='MET call' ~ 1,
                                                  screening_to_met_days<=90 & status_met=='Discharged alive' ~ 2,
                                                  screening_to_met_days<=90 & status_met=='Died in hospital' ~ 3),
                      screening_to_met_days_sens = pmin(90,screening_to_met_days)) %>%
  mutate_at('status_met_sens',~factor(.,0:3,labels=c('censor','MET call','Discharged alive','Died in hospital')))


mod.cif_met <- lapply(hospitals,function(x)
  with(filter(dat_analysis,Hospital==x),cuminc(screening_to_met_days_sens,status_met_sens,group=intervention,strata=as.character(clinical_team_fclty),cencode='censor')))


g<-lapply(1:3, function(x) ggcompetingrisks(mod.cif_met[[x]],
                                            gnames = c('Usual care:MET','Intervention exposure:MET','Usual care:Discharged alive','Intervention exposure:Discharged alive','Usual care:Died in hospital','Intervention exposure:Died in hospital'),
                                            gsep=':',
                                            xlab = 'Days since first recorded as high-risk CriSTAL or SPICT-positive',
                                            ylab = 'Cumulative probability',
                                            title = '',
                                            legend.title='',
                                            multiple_panels = T,
                                            risk.table = TRUE,
                                            conf.int = TRUE))
names(g) <- hospitals

#MET call
plot_list = list()
for(i in seq_along(hospitals)){
  p = g[[i]]$data %>% 
    filter(event=="MET") %>%
    mutate_at("name",~str_wrap(.,30)) %>%
    ggplot(aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=group,colour=group,fill=group))+
    geom_ribbon(alpha=.2)+geom_path()+
    scale_x_continuous('Days since first recorded as high-risk CriSTAL or SPICT-positive',breaks=seq(0,90,10))+
    scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)+
    expand_limits(y=0.2)+scale_y_continuous('Cumulative probability',breaks=seq(0,0.2,0.04))+g.theme
  plot_list[[i]] = p
  
}

for (i in seq_along(hospitals)) {
  file_name = paste("plots for Chris/Outcome_7_", hospitals[i], ".png", sep="")
  png(file_name,height=10,width=10,units = 'in',res=600)
  print(plot_list[[i]])
  dev.off()
}



