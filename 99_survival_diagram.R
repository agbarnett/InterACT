# 99_survival_diagram.R
# make a diagram to illustrate the survival analysis
# Sep 2021
library(dplyr)
library(ggplot2)

# patients
to_plot = data.frame(id = rep(1:3, 2), 
                     intervention = c(1,1,1,2,2,2),
                     start = c(0.1,0.3,0.2,1.85,1.2,1.3),
                     end = c(0.5,0.5,1.05,2.4,1.9,1.6),
                     label = c('D','Y','Y','Y','D','Y'))

# establishment phase
between = data.frame(x=c(-Inf, Inf), ymin=0.95, ymax=1.05, intervention=1)

# labels
label1 = data.frame(x=3.2, y=0.4, start=0, end=0, intervention=1, label='Usual care')
label2 = data.frame(x=2, y=1, start=0, end=0, intervention=1, label='Establishment (4 weeks)')
label3 = data.frame(x=3.2, y=1.6, start=0, end=0, intervention=1, label='Intervention')
label4 = data.frame(x=3.15, y=0.95, start=0, end=0, intervention=1, label='Censored')
label5 = data.frame(x=1.15, y=2.07, start=0, end=0, intervention=1, label='Censored')

# plot
colours = c('black', 'darkseagreen')
gplot = ggplot(to_plot, aes(x=id, ymin=start, ymax=end, col=factor(intervention)))+
  scale_color_manual(NULL, values=colours)+
  geom_errorbar(size=1.1, width=0)+
  geom_point(data=to_plot, aes(x=id, y=end, col=factor(intervention)), pch=19, size=7)+
  geom_text(data=to_plot, aes(x=id, y=end, label=label), col='white', size=4)+
  geom_ribbon(data=between, aes(x=x, ymin=ymin, ymax=ymax), col='transparent', alpha=0.5)+
  theme_bw()+
  theme(legend.position = 'none',
        axis.ticks = element_blank(), # remove all tick marks
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(labels=NULL)+
  scale_x_continuous(breaks=1:3, labels=NULL, limits=c(1,3.2))+
  coord_flip(ylim=c(0, 2))+
  geom_text(data=label1, aes(x=x, y=y, label=label))+
  geom_text(data=label2, aes(x=x, y=y, label=label), angle=90, col='white')+
  geom_text(data=label3, aes(x=x, y=y, label=label), col=colours[2])+
  geom_text(data=label4, aes(x=x, y=y, label=label), fontface='italic', hjust='right', vjust='right')+ # censored
  geom_text(data=label5, aes(x=x, y=y, label=label), fontface='italic', hjust='right', vjust='right')+ # censored
  xlab('')+
  ylab('Calendar time')
gplot

jpeg('figures/survival_diagram.jpg', width=16, height=7, units='cm', res=400)
print(gplot)
dev.off()
