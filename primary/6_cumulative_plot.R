# 6_cumulative_plot.R
# create combined cumulative plot for paper
# July 2023
library(ggplot2)
library(gridExtra)
#library(survminer)
library(ggpubr) # for get_legend

# get the plots
load('results/cumulative_plots.RData') # from 5_outcome_analysis.Rmd

# sort out the legend
margins = c(0,0,0.1,0) # reduce space around plots
legend = get_legend(g.cif_los_a)
g.cif_los_a = g.cif_los_a + theme(legend.position='none') + # now remove legend
  ggtitle('In-hospital death')+
  coord_cartesian(xlim = c(0,50)) + # focus where there's more events - not much change after 50 days
  ylab('Cumulative probability')+
  theme(plot.margin=unit(margins, "cm"))
g.cif_los_b = g.cif_los_b + theme(legend.position='none') + # now remove legend
  ggtitle('Discharge')+
  coord_cartesian(xlim = c(0,50)) + # focus where there's more events
  scale_y_continuous(breaks=seq(0,1,0.2))+ # looks too squashed
  ylab('Cumulative probability')+
  theme(plot.margin=unit(margins, "cm"))
g.cif_met = g.cif_met + 
  ggtitle('Medical Emergency Team call')+
  coord_cartesian(xlim = c(0,50)) + # focus where there's more events
  scale_y_continuous(breaks=seq(0,0.2,0.05))+ # previous scale looks too squashed
  ylab('Cumulative probability')+
  theme(plot.margin=unit(margins, "cm"),
        legend.position='none') # now remove legend
g.cif_readmit_a = g.cif_readmit_a + 
  scale_x_continuous(breaks=seq(0,90,10))+ # to match other plots
  coord_cartesian(xlim = c(0,80)) + # focus where there's more events - not much change after 50 days
  scale_y_continuous(breaks=seq(0,0.6,0.2))+ # previous scale looks too squashed
  ylab('Cumulative probability')+
  xlab('Days since discharge alive from hospital')+
  theme(plot.margin=unit(margins, "cm"),
        legend.position='none') # now remove legend
g.cif_readmit_b = g.cif_readmit_b + 
  scale_x_continuous(breaks=seq(0,90,10))+ # to match other plots
  coord_cartesian(xlim = c(0,80)) + # focus where there's more events - not much change after 50 days
  xlab('Days since discharge alive from hospital')+
  theme(plot.margin=unit(margins, "cm"),
        legend.position='none') # now remove legend

# export
jpeg('manuscript/figures/cumulative_curves.jpg', width=8, height=10.5, units='in', res=400)
grid.arrange(legend, g.cif_los_a, g.cif_los_b, g.cif_met, g.cif_readmit_a, g.cif_readmit_b, nrow=6, heights=c(1,3,3,3,3,3))
invisible(dev.off())
