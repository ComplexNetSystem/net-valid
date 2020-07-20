library(ggplot2)
library(reshape2)
selectLen <- 30L
timeAllLen <- nrow(data_fishCommunity)
alphaOverTime <- alphaDiversity[selectLen:timeAllLen]
aveTE_TEThre <- (alphaNetworkTimeTE + alphaNetworkTimeTEThre)/2

# alphaNetworkTimeTENorm <- alphaNetworkTimeTE/max(alphaNetworkTimeTE)
# alphaNetworkTimeCCMNorm <- alphaNetworkTimeCCM/max(alphaNetworkTimeCCM)
# alphaOverTimeNorm <- alphaOverTime/max(alphaOverTime)

df.alpha <- data.frame(
  timeScaleCut,
  
  alphaNetworkTimeTE,
  alphaNetworkTimeTEThre,
  
  alphaNetworkTimeCCM,
  alphaNetworkTimeCCMThre,
  
  alphaOverTime
)

plt_AlphaTE <- ggplot(df.alpha,aes(x = alphaOverTime,y = alphaNetworkTimeTE)) +
  xlab('Alpha Diversity') + ylab('Alpha from TE') +
  xlim(c(0,15)) + ylim(c(0,15)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank())
plt_AlphaTE


####-------------- alpha(CCM) & alpha(TE) & Real Alpha -----------####
df.alphaCCM_TE <- data.frame(
  timeScaleCut,
  alphaNetworkTimeCCM,
  alphaNetworkTimeTE,
  alphaOverTime
)
vizAlphaCCM_TE <- melt(df.alphaCCM_TE,id.vars = 'timeScaleCut',
                     variable.name = 'alphaType',value.name = 'AlphaValue')

pltAlpha_CCM_TE <- ggplot(vizAlphaCCM_TE,aes(x = timeScaleCut,y = AlphaValue,color = alphaType)) + 
  xlab('Year') + ylab(expression(paste(alpha,'(t)'))) + 
  ylim(c(0,15)) +
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c(expression(paste(alpha[CCM],'(t)')),
                                expression(paste(alpha[TE],'(t)')),
                                expression(paste('Real ',alpha,'(t)'))),
                     values=c('blue','red','forestgreen')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(plot.margin=unit(c(1,1,1,1),'lines')) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
pltAlpha_CCM_TE


####-------------- High Threshold: alpha(CCM) & alpha(TE) & Real Alpha -----------####
df.alphaCCM_TE_Thre <- data.frame(
  timeScaleCut,
  alphaNetworkTimeCCMThre_optimal,
  alphaNetworkTimeTEThre_optimal,
  # aveTE_TEThre,
  alphaOverTime
)
vizAlphaCCM_TE_Thre <- melt(df.alphaCCM_TE_Thre,id.vars = 'timeScaleCut',
                       variable.name = 'alphaTypeThre',value.name = 'AlphaValueThre')

pltAlpha_CCM_TE_Thre <- ggplot(vizAlphaCCM_TE_Thre,aes(x = timeScaleCut,y = AlphaValueThre,color = alphaTypeThre)) + 
  xlab('Year') + ylab(expression(paste(alpha,'(t)',',',alpha,'(g)'))) + 
  ylim(c(0,15)) +
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c('CCM','TE',expression(paste('Real ',alpha,'(t)'))),
                     values=c('blue','red','forestgreen')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  scale_x_continuous(breaks=seq(2002,2014,1))
pltAlpha_CCM_TE_Thre




# 
# 
# plt_AlphaCCM <- ggplot(df.alpha,aes(x = alphaNetworkTimeCCM,y = alphaOverTime)) +
#   xlab('Alpha from CCM') + ylab('Alpha Diversity') +
#   xlim(c(0,15)) + ylim(c(0,15)) +
#   geom_point(size = 3) +
#   geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
#               se = FALSE, size = 2,aes(color = ('FittingTE'))) +
#   scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
#     formula = alphaOverTime ~ alphaNetworkTimeCCM))[2],2)))),
#     values=c('FittingTE' = 'black')) +
#   theme_bw() +
#   theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
#   theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
#   theme(legend.position = 'top',
#         legend.direction = 'horizontal',
#         legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
#   theme(legend.title=element_blank())
# plt_AlphaCCM

# plt_AlphaCCMTE <- ggplot(df.alpha,aes(x = alphaNetworkTimeCCM,y = alphaNetworkTimeTE)) + 
#   xlab('Alpha from CCM') + ylab('Alpha from TE') + 
#   xlim(c(0,15)) + ylim(c(0,15)) +
#   geom_point(size = 3) + 
#   geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
#               se = FALSE, size = 2,aes(color = ('FittingTE'))) +
#   scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
#     formula = alphaNetworkTimeTE ~ alphaNetworkTimeCCM))[2],2)))),
#     values=c('FittingTE' = 'black')) +
#   theme_bw() + 
#   theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
#   theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
#   theme(legend.position = 'top',
#         legend.direction = 'horizontal',
#         legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
#   theme(legend.title=element_blank())
# plt_AlphaCCMTE



####-------Average Estimated Alpha over Real Alpha (from TE & CCM)-------####

uniqueAlphaDiversity <- sort(unique(alphaOverTime))
alphaNetworkTimeCCM_average <- matrix(nrow = length(uniqueAlphaDiversity),ncol = 1)
alphaNetworkTimeTE_average <- matrix(nrow = length(uniqueAlphaDiversity),ncol = 1)
for (ii in 1:length(uniqueAlphaDiversity)) {
  alphaNetworkTimeCCM_average[ii,] <- sum(alphaNetworkTimeCCM[which(alphaOverTime == uniqueAlphaDiversity[ii])])/
    length(alphaNetworkTimeCCM[which(alphaOverTime == uniqueAlphaDiversity[ii])])
  alphaNetworkTimeTE_average[ii,] <- sum(alphaNetworkTimeTE[which(alphaOverTime == uniqueAlphaDiversity[ii])])/
    length(alphaNetworkTimeTE[which(alphaOverTime == uniqueAlphaDiversity[ii])])
}


df.alphaAverage <- data.frame(
  uniqueAlphaDiversity,
  alphaNetworkTimeTE_average,
  alphaNetworkTimeCCM_average
)

plt_AveAlpha_TE <- ggplot(df.alphaAverage,aes(x = uniqueAlphaDiversity,y = alphaNetworkTimeTE_average)) +
  xlab('Alpha Diversity') + ylab('Average Alpha from TE') +
  xlim(c(0,15)) + ylim(c(0,15)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = ('FittingTE'))) +
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = alphaNetworkTimeTE_average ~ uniqueAlphaDiversity))[2],2)))),
    values=c('FittingTE' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank())
plt_AveAlpha_TE

plt_AveAlpha_CCM <- ggplot(df.alphaAverage,aes(x = uniqueAlphaDiversity,y = alphaNetworkTimeCCM_average)) +
  xlab('Alpha Diversity') + ylab('Average Alpha from CCM') +
  xlim(c(0,15)) + ylim(c(0,15)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = ('FittingTE'))) +
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = alphaNetworkTimeCCM_average ~ uniqueAlphaDiversity))[2],2)))),
    values=c('FittingTE' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank())
plt_AveAlpha_CCM


# plot(uniqueAlphaDiversity,alphaNetworkTimeTE_average, type = "o",
#      col = "blue", lwd = 2,panel.first = grid(),xlim = c(0,15),ylim = c(0,15),
#      xlab = 'Alpha Diversity', ylab = 'Average Alpha (TE)', cex.axis = 3,cex.lab = 3)
# plot(uniqueAlphaDiversity,alphaNetworkTimeCCM_average, type = "o",
#      col = "blue", lwd = 2,panel.first = grid(),xlim = c(0,15),ylim = c(0,15),
#      xlab = 'Alpha Diversity', ylab = 'Average Alpha (TE)', cex.axis = 3,cex.lab = 3)



####------- Shannon Index over Time & Alpha over Time ------####
df.alpha_shannon <- data.frame(
  timeScaleCut,
  shannonIndex,
  alphaOverTime
)
vizAlpha_Shannnon <- melt(df.alpha_shannon,id.vars = 'timeScaleCut',
                            variable.name = 'alphaIndexType',value.name = 'AlphaValueTime')
pltAlpha_Shannon <- ggplot(vizAlpha_Shannnon,aes(x = timeScaleCut,y = AlphaValueTime,color = alphaIndexType)) + 
  xlab('Year') + ylab('Alpha Diversity') + 
  # ylim(c(0,15)) +
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c(expression(paste(H[alpha],'(t)')),expression(paste(alpha,'(t)'))),
                     values=c('red','forestgreen')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20))
pltAlpha_Shannon

plt_RealAlpha <- ggplot(df.alpha_shannon,aes(x = timeScaleCut,y = alphaOverTime)) +
  xlab('Year') + ylab(expression(paste(alpha,'(t)'))) +
  # xlim(c(0,15)) + 
  ylim(c(0,15)) +
  geom_line(size = 1,colour = 'forestgreen') + 
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(plot.margin=unit(c(10,1,10,1),'lines')) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
plt_RealAlpha

plt_ShannonIndex <- ggplot(df.alpha_shannon,aes(x = timeScaleCut,y = shannonIndex)) +
  xlab('Year') + ylab(expression(paste(H[alpha],'(t)'))) + 
  geom_line(size = 1,colour = 'red') + 
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(plot.margin=unit(c(10,1,10,1),'lines')) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
plt_ShannonIndex

df.alpha_TE_Thre <- data.frame(
  timeScaleCut,
  # alphaNetworkTimeCCMThre,
  alphaNetworkTimeTEThre,
  # aveTE_TEThre,
  alphaOverTime
)
vizAlpha_TE_Thre <- melt(df.alpha_TE_Thre,id.vars = 'timeScaleCut',
                            variable.name = 'alphaTypeThre',value.name = 'AlphaValueThre')

pltAlpha_TE_Thre <- ggplot(vizAlpha_TE_Thre,aes(x = timeScaleCut,y = AlphaValueThre,color = alphaTypeThre)) + 
  xlab('Year') + ylab(expression(paste(alpha,'(t)'))) + 
  ylim(c(0,15)) +
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c(expression(paste(alpha[TE>0.5],'(t)')),expression(paste('Real ',alpha,'(t)'))),
                     values=c('red','forestgreen')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(plot.margin=unit(c(1,1,1,1),'lines')) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
pltAlpha_TE_Thre

pltAlphaTE_ThreScatter <- ggplot(df.alpha_TE_Thre,aes(x = alphaOverTime,y = alphaNetworkTimeTEThre)) + 
  xlab('alpha diversity') + ylab('TE alpha') + 
  # xlim(c(0.14,0.26)) + 
  # ylim(c(0.5,1))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = ('FittingAlpha'))) +
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = teEigTimeNorm ~ ccmEigTimeNorm))[2],2)))),
    values=c('FittingAlpha' = 'black')) +
  theme_bw() + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank())
pltAlphaTE_ThreScatter

df.alpha_CCM_Thre <- data.frame(
  timeScaleCut,
  alphaNetworkTimeCCMThre,
  # alphaNetworkTimeTEThre,
  # aveTE_TEThre,
  alphaOverTime
)
vizAlpha_CCM_Thre <- melt(df.alpha_CCM_Thre,id.vars = 'timeScaleCut',
                         variable.name = 'alphaTypeThre',value.name = 'AlphaValueThre')

pltAlpha_CCM_Thre <- ggplot(vizAlpha_CCM_Thre,aes(x = timeScaleCut,y = AlphaValueThre,color = alphaTypeThre)) + 
  xlab('Year') + ylab(expression(paste(alpha,'(t)'))) + 
  ylim(c(0,15)) +
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c(expression(paste(alpha[CCM>0],'(t)')),expression(paste('Real ',alpha,'(t)'))),
                     values=c('red','forestgreen')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(plot.margin=unit(c(1,1,1,1),'lines')) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
pltAlpha_CCM_Thre

df.MeanAlpha_TE <- data.frame(
  timeScaleCut,
  # alphaNetworkTimeCCMThre,
  # alphaNetworkTimeTEThre,
  aveTE_TEThre,
  alphaOverTime
)
vizMeanAlpha_TE <- melt(df.MeanAlpha_TE,id.vars = 'timeScaleCut',
                         variable.name = 'alphaTypeThre',value.name = 'AlphaValueThre')

pltMeanAlpha_TE <- ggplot(vizMeanAlpha_TE,aes(x = timeScaleCut,y = AlphaValueThre,color = alphaTypeThre)) + 
  xlab('Year') + ylab(expression(paste(alpha,'(t)'))) + 
  ylim(c(0,15)) +
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c(expression(paste('Mean ',alpha[TE],'(t)')),
                                expression(paste('Real ',alpha,'(t)'))),
                     values=c('red','forestgreen')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(plot.margin=unit(c(1,1,1,1),'lines')) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
pltMeanAlpha_TE