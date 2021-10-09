# 2_plot_paper_outcome456.R
# plots for paper
# -- survival curves
# -- calender time plot
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
pred_time = 21 

# blind_hospital
to_plot_care_review = mutate(to_plot_care_review, 
                                hosp_blinded = blind(hospital))
to_plot_care_directive = mutate(to_plot_care_directive, 
                                hosp_blinded = blind(hospital))
to_plot_palliative = mutate(to_plot_palliative, 
                                hosp_blinded = blind(hospital))

# 
cr_plot = ggplot(to_plot_care_review, aes(x=time, y=est, ymin=lower, ymax=upper, col=group)) +
  ggtitle("Clinician-led care review discussion")+
  geom_ribbon(alpha=0.2) +
  geom_line(size = 1.05) +  # make lines thicker
  scale_y_continuous(limits=c(0, 1)) + # reduce white space
  coord_cartesian(xlim=c(0,pred_time))+ # limit based on censoring
  g.theme +
  scale_color_manual(NULL, values=cbPalette) +
  xlab('') +
  ylab('Cumulative probability') +
  theme(legend.position = c(0.2,0.2),
        plot.margin = margin(0.1, 0.1, -0.3, 0.1, "cm")) + 
  facet_wrap(~hosp_blinded)
# 
cd_plot = ggplot(to_plot_care_directive, aes(x=time, y=est, ymin=lower, ymax=upper, col=group)) +
  ggtitle("Review of care directive documents")+
  geom_ribbon(alpha=0.2) +
  geom_line(size = 1.05) +  # make lines thicker
  scale_y_continuous(limits=c(0, 0.8)) + # reduce white space
  coord_cartesian(xlim=c(0,pred_time))+ # limit based on censoring
  g.theme +
  scale_color_manual(NULL, values=cbPalette) +
  xlab('') +
  ylab('Cumulative probability') +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 0.1, -0.3, 0.1, "cm"))+ # reduce white space for blank x-label
  facet_wrap(~hosp_blinded)
# 
pal_plot = ggplot(to_plot_palliative, aes(x=time, y=est, ymin=lower, ymax=upper, col=group)) +
  ggtitle("Palliative care referral")+
  geom_ribbon(alpha=0.2) +
  geom_line(size = 1.05) +  # make lines thicker
  scale_y_continuous(limits=c(0, 0.3)) + # reduce white space
  coord_cartesian(xlim=c(0,pred_time))+ # limit based on censoring
  g.theme +
  scale_color_manual(NULL, values=cbPalette) +
  xlab('Days since patient came under the care of the clinical team') +
  ylab('Cumulative probability') +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) + # top, right, bottom, left
  facet_wrap(~hosp_blinded)

# using agg due to ribbon
agg_jpeg('figures/456_for_paper.jpeg', width=6, height=9, units='in', res=500)
grid.arrange(cr_plot, cd_plot, pal_plot, ncol=1)
dev.off()

### Section 2

# figures from 2_interact_summary_456.Rmd
load('results/plot_calendar_data.RData')

