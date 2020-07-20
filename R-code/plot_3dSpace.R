library(plotly)
library(ggplot2)
library(reshape2)

numPair <- 14L
df_PairTE <- as.data.frame(teTimePair)
colnames(df_PairTE) <- (1:numPair)
df_PairTE$Time <- timeScaleCut

data_PairTE <- melt(df_PairTE,id.vars = 'Time',variable.name = 'Pair',value.name = 'TE')
data_PairTE$CCM <- as.vector(ccmTimePair)

timePair <- cbind(teTimePair,ccmTimePair)
timePairNorm <- timePair/max(abs(timePair))
df_PairTE_CCM <- as.data.frame(timePairNorm)

write.table(df_PairTE_CCM,"Pair-TE-CCM.csv",row.names=F,sep = ',')

# p <- plot_ly(data_PairTE, x = ~Time, y = ~Pair, z = ~CCM,
#              type = 'scatter3d', mode = 'lines', color = ~Pair)

# p <- plot_ly(data_PairTE, x = ~CCM, y = ~Pair, z = ~TE, color = ~Pair)

# p

plt_Pair <- ggplot(df_PairTE_CCM,aes(x = V18,y = V4)) + 
  xlab('Interaction from CCM') + ylab('Interaction from TE') + 
  xlim(c(-1,1)) + ylim(c(0,1)) +
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = df_PairTE_CCM$V18 ~ df_PairTE_CCM$V4))[2],2)))),
    values=c('Fitting' = 'black')) +
  
  # scale_color_manual(labels = c('Mean Interaction',
  #                    paste0('k=',as.character(round(coefficients(lm(
  #                      formula = teMeanTime ~ ccmMeanTime))[2],2)))),
  #                    values=c('black' = 'black',
  #                             'Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  # stat_smooth(method="lm",se=TRUE,fill=NA,aes(color="k = "),
  #             formula=y ~ poly(x, 1, raw=TRUE),color="black",size = 2) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
plt_Pair