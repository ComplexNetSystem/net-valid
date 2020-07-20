library('rEDM');
library("rJava")
library(ggplot2)
library(reshape2)
library(readr)

data(sardine_anchovy_sst)
sardine <- sardine_anchovy_sst$sardine
anchovy <- sardine_anchovy_sst$anchovy

year <- sardine_anchovy_sst$year

sardineProcess <- abs(diff(sardine))
anchovyProcess <- abs(diff(anchovy))
yearProcess <- year[2:length(year)]


df.SarAncProcess <- data.frame(
  sardineProcess,
  anchovyProcess,
  yearProcess
)

vizData <- melt(df.SarAncProcess,
                id.vars = 'yearProcess',variable.name = 'Species',value.name = 'Abd')

plt <- ggplot(vizData,aes(x = yearProcess,y = Abd,color = Species)) + 
  xlab('Year') + ylab('Relative Abundance') + 
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c('Sardine','Anchovy') ,values=c('blue','red')) + 
  # theme(panel.grid.major = element_line(color = 'black'),
  #       axis.line = element_line(color = 'black'),
  #       panel.background = element_blank()) +
  # theme(legend.key = element_blank()) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20))
plt
