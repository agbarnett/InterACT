# 2_plot_paper_outcome456.R
# plots for paper
# -- survival curves
# -- calendar time plot
# October 2021
source('99_blind_hospital.R')
library(dplyr)
# graphics things:
library(ragg) # because of ribbon issue
library(ggplot2)
library(gridExtra)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#999999", "#CC79A7") # colour-blind palette

### Section 1 - survival curves ###

# figures from 2_interact_summary_456.Rmd
load('results/plot_data.RData')
pred_time = 15 # try shorter day than 21 to focus on majority of events 

# blind_hospital
to_plot_care_review = mutate(to_plot_care_review, 
                                hosp_blinded = blind(hospital))
to_plot_care_directive = mutate(to_plot_care_directive, 
                                hosp_blinded = blind(hospital))
to_plot_palliative = mutate(to_plot_palliative, 
                                hosp_blinded = blind(hospital))

# 
cr_plot = ggplot(to_plot_care_review, aes(x=time, y=est, ymin=lower, ymax=upper, linetype=event, col=group)) +
  ggtitle("Clinician-led care review discussion")+
#  geom_ribbon(alpha=0.2) + # reduce clutter
  geom_line(size = 1.05) +  # make lines thicker
  scale_y_continuous(limits=c(0, 0.9)) + # reduce white space
  coord_cartesian(xlim=c(0,pred_time))+ # limit based on censoring
  g.theme +
  scale_color_manual('Phase:', values=cbPalette, labels=c('Intervention phase', 'Usual care phase')) +
  scale_linetype_manual('Phase:', values=c(2,1), labels=NULL) +
  xlab('') +
  ylab('Cumulative probability') +
  guides(colour = guide_legend(nrow = 1), linetype = 'none')+
  theme(legend.position = c(0.5, 1.3),
        legend.text = element_text(size=12),
        plot.margin = margin(0.7, 0.1, -0.3, 0.1, "cm")) +  # increase top margin for legend
  facet_wrap(~hosp_blinded)
# 
cd_plot = ggplot(to_plot_care_directive, aes(x=time, y=est, ymin=lower, ymax=upper, linetype=event, col=group)) +
  ggtitle("Review of care directive measures")+ # updated feb 2022
  #geom_ribbon(alpha=0.2) +
  geom_line(size = 1.05) +  # make lines thicker
  scale_y_continuous(limits=c(0, 0.72)) + # reduce white space
  coord_cartesian(xlim=c(0,pred_time))+ # limit based on censoring
  g.theme +
  scale_color_manual('Phase:', values=cbPalette, labels=c('Intervention', 'Usual care')) +
  scale_linetype_manual('Phase:', values=c(2,1), labels=c('Discharge/Death','Outcome')) +
  xlab('') +
  ylab('Cumulative probability') +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 0.1, -0.3, 0.1, "cm"))+ # reduce white space for blank x-label
  facet_wrap(~hosp_blinded)
# 
pal_plot = ggplot(to_plot_palliative, aes(x=time, y=est, ymin=lower, ymax=upper, linetype=event, col=group)) +
  ggtitle("Palliative care referral")+
 # geom_ribbon(alpha=0.2) +
  geom_line(size = 1.05) +  # make lines thicker
  scale_y_continuous(limits=c(0, 0.9)) + # reduce white space
  coord_cartesian(xlim=c(0,pred_time))+ # limit based on censoring
  g.theme +
  scale_color_manual('Phase:', values=cbPalette, labels=c('Intervention', 'Usual care')) +
  scale_linetype_manual('Phase:', values=c(2,1), labels=c('Discharge/Death','Outcome')) +
  xlab('Days since patient came under the care of the clinical team') +
  ylab('Cumulative probability') +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) + # top, right, bottom, left
  facet_wrap(~hosp_blinded)

# using agg due to ribbon
agg_jpeg('figures/456_for_paper.jpeg', width=6, height=9, units='in', res=500)
grid.arrange(cr_plot, cd_plot, pal_plot, ncol=1, heights=c(1.1,1,1)) # increase top height for legend
dev.off()

# section 1a, version with confidence intervals
cr_plot = cr_plot + geom_ribbon(alpha=0.2)
cd_plot = cd_plot + geom_ribbon(alpha=0.2)
pal_plot = pal_plot + geom_ribbon(alpha=0.2)
agg_jpeg('figures/456_for_paper_plus_ci.jpeg', width=6, height=9, units='in', res=500)
grid.arrange(cr_plot, cd_plot, pal_plot, ncol=1, heights=c(1.1,1,1)) # increase top height for legend
dev.off()

### Section 2

# figures from 2_interact_summary_456.Rmd
load('results/plot_calendar_data.RData')

# blind_hospital; re-order levels so that legend has better appearance
levels = c('Prior','Yes','No','Discharge','Intervention','Post-intervention')
labels = c('Prior','Yes','No\n(censored)','Discharge','Change-over\n(censored)','Post-intervention\n(censored)')
colours = cbPalette[c(2,3,6,1,7,8)]
smooth_prop_care_review = mutate(smooth_prop_care_review, 
                             hosp_blinded = blind(hospital),
                             event = factor(event, levels=levels, labels=labels))
smooth_prop_care_directive = mutate(smooth_prop_care_directive, 
                                hosp_blinded = blind(hospital),
                                event = factor(event, levels=levels, labels=labels))
smooth_prop_palliative = mutate(smooth_prop_palliative, 
                            hosp_blinded = blind(hospital),
                            event = factor(event, levels=levels, labels=labels))
# blind hospital in lines
lines = mutate(lines, hosp_blinded = blind(hospital))

# redo figures with titles and blinded hospital
#
plot1 = ggplot(data=smooth_prop_care_review, aes(x=time, y=n, fill=event)) +
  ggtitle("Clinician-led care review discussion")+
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=lines, aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('') +
  ylab('Number of patients') +
  g.theme+
  facet_wrap(~hosp_blinded, scales='free_y')+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position = c(0.5, 1.43), # to force legend above title
        plot.margin = grid::unit(c(10, 1, -2, 1), "mm"), # reduce white space for blank x-axis label
        axis.text.x = element_text(angle = 45, hjust=1))
#
plot2 = ggplot(data=smooth_prop_care_directive, aes(x=time, y=n, fill=event)) +
  ggtitle("Review of care directive measures")+ # update feb 2022
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=lines, aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('') +
  ylab('Number of patients') +
  g.theme+
  facet_wrap(~hosp_blinded, scales='free_y')+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position='none', # turn off legend
        plot.margin= grid::unit(c(0, 1, -2, 1), "mm"), # reduce white space for blank x-axis label
        axis.text.x = element_text(angle = 45, hjust=1))
#
plot3 = ggplot(data=smooth_prop_palliative, aes(x=time, y=n, fill=event)) +
  ggtitle("Palliative care referral")+
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=lines, aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('Calendar time') +
  ylab('Number of patients') +
  g.theme+
  facet_wrap(~hosp_blinded, scales='free_y')+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position='none',
        plot.margin= grid::unit(c(0, 1, 0, 1), "mm"), # to match others (top, right, bottom, left)
        axis.text.x = element_text(angle = 45, hjust=1))

# export
agg_jpeg('figures/456_calendar_for_paper.jpeg', width=6, height=9, units='in', res=500)
grid.arrange(plot1, plot2, plot3, ncol=1, heights=c(1.12, 1, 1)) # heights to account for legend
dev.off()


# redo figures with titles and blinded hospital
#
plot1 = ggplot(data=smooth_prop_care_review, aes(x=time, y=p, fill=event)) +
  ggtitle("Clinician-led care review discussion")+
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=lines, aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('') +
  ylab('Proportion of patients') +
  g.theme+
  facet_wrap(~hosp_blinded, scales='free_y')+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position = c(0.5, 1.43), # to force legend above title
        plot.margin = grid::unit(c(10, 1, -2, 1), "mm"), # reduce white space for blank x-axis label
        axis.text.x = element_text(angle = 45, hjust=1))
#
plot2 = ggplot(data=smooth_prop_care_directive, aes(x=time, y=p, fill=event)) +
  ggtitle("Review of care directive measures")+ # feb 2022
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=lines, aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('') +
  ylab('Proportion of patients') +
  g.theme+
  facet_wrap(~hosp_blinded, scales='free_y')+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position='none', # turn off legend
        plot.margin= grid::unit(c(0, 1, -2, 1), "mm"), # reduce white space for blank x-axis label
        axis.text.x = element_text(angle = 45, hjust=1))
#
plot3 = ggplot(data=smooth_prop_palliative, aes(x=time, y=p, fill=event)) +
  ggtitle("Palliative care referral")+
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=lines, aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('Calendar time') +
  ylab('Proportion of patients') +
  g.theme+
  facet_wrap(~hosp_blinded, scales='free_y')+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position='none',
        plot.margin= grid::unit(c(0, 1, 0, 1), "mm"), # to match others (top, right, bottom, left)
        axis.text.x = element_text(angle = 45, hjust=1))

# export
agg_jpeg('figures/456_calendar_for_paper_proportion.jpeg', width=6, height=9, units='in', res=500)
grid.arrange(plot1, plot2, plot3, ncol=1, heights=c(1.12, 1, 1)) # heights to account for legend
dev.off()

### supplement figure comparing prior and no prior ###
load('results/prior_or_not.RData') # from 2_interact_summary_456.Rmd
to_plot = filter(results, term=='int_time_n') %>%
  mutate(hosp_blinded = blind(hospital),
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high))
gplot = ggplot(to_plot, aes(x=outcome, y=estimate, ymin=conf.low, ymax=conf.high, colour=with_prior))+
  geom_hline(yintercept = 1, lty=2)+
  geom_point(position = position_dodge(width=0.2), size=2)+
  geom_errorbar(position = position_dodge(width=0.2), width=0, size=1.05)+
  scale_color_manual("With prior outcomes", values=c('skyblue','indianred1'))+
  scale_x_discrete(expand=c(0.05,0.05))+
  g.theme+
  theme(legend.position = 'top',
        legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
        legend.box.margin	= margin(t=0, r=0, b=0, l=0), # reduce space around legend
        legend.margin = margin(t=0, r=0, b=0, l=0, unit='mm') # reduce space around legend
  )+
  xlab('')+
  ylab('Hazard ratio')+
  coord_flip()+
  facet_wrap(~hosp_blinded)
gplot

# export
agg_jpeg('figures/456_calendar_for_paper_prior_yes_no.jpg', width=5.5, height=3, units='in', res=500)
print(gplot)
dev.off()


### Section 3, calendar plots without colours ###
gcuh = bind_rows(smooth_prop_care_review) %>% # just one because results are the same for three outcomes
  filter(hospital == 'GCUH')
plot_gcuh = ggplot(data=gcuh, aes(x=time, y=n)) +
  geom_bar(stat='identity', position='stack') +
  geom_vline(data=filter(lines, hospital=='GCUH'), aes(xintercept=time), lty=2) + # vertical reference line at change
  scale_fill_manual(NULL, values=colours)+
  scale_x_continuous(breaks=ticks$ticks, labels=ticks$labels, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  xlab('') +
  ylab('Number of patients') +
  g.theme+
  guides(fill=guide_legend(nrow = 1)) + # for legend
  theme(legend.position = c(0.5, 1.43), # to force legend above title
        plot.margin = grid::unit(c(10, 1, -2, 1), "mm"), # reduce white space for blank x-axis label
        axis.text.x = element_text(angle = 45, hjust=1))
agg_jpeg('figures/456_calendar_gcuh_no_colour.jpg', width=4, height=4, units='in', res=500)
print(plot_gcuh)
dev.off()

