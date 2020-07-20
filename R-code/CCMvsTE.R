library(ggplot2)
library(reshape2)

eigenDigitizer <- read_csv("EigenDigitizer.csv")
meanDigitizer <- read_csv("MeanDigitizer.csv")

ccmEigTimeDigitalizer <- eigenDigitizer$Data
ccmMeanTimeDigitalizer <- meanDigitizer$Data

# ccmEigTimeNorm <- ccmEigTime/max(abs(ccmEigTime))
# ccmMeanTimeNorm <- ccmMeanTime/max(abs(ccmMeanTime))
eigNormFactor <- max(max(abs(teEigTime)),max(abs(ccmEigTimeDigitalizer)))
meanNormFactor <- max(max(abs(teMeanTime)),max(abs(ccmMeanTimeDigitalizer)))

teEigTimeNorm <- teEigTime/eigNormFactor
ccmEigTimeNorm <- ccmEigTimeDigitalizer/eigNormFactor

teMeanTimeNorm <- teMeanTime/meanNormFactor
ccmMeanTimeNorm <- ccmMeanTimeDigitalizer/meanNormFactor
# teEigTimeNorm <- teEigTime/max(abs(teEigTime))
# teMeanTimeNorm <- teMeanTime/max(abs(teMeanTime))

df.CCMvsTE <- data.frame(
  timeScaleCut,
  
  ccmEigTimeDigitalizer,
  ccmMeanTimeDigitalizer,
  ccmEigTimeNorm,
  ccmMeanTimeNorm,
  ccmRatio,
  ccmNumRatio,
  
  teEigTime,
  teMeanTime,
  teEigTimeNorm,
  teMeanTimeNorm,
  teRatio,
  teNumRatio
)

df.EigenTE_CCM <- data.frame(
  timeScaleCut,
  ccmEigTimeDigitalizer <- eigenDigitizer$Data,
  teEigTime
)
df.MeanTE_CCM <- data.frame(
  timeScaleCut,
  ccmMeanTimeDigitalizer <- meanDigitizer$Data,
  teMeanTime
)
vizDataEigen <- melt(df.EigenTE_CCM,id.vars = 'timeScaleCut',
                variable.name = 'EigenType',value.name = 'Eigenvalue')

vizDataMean <- melt(df.MeanTE_CCM,id.vars = 'timeScaleCut',
                     variable.name = 'MeanType',value.name = 'Meanvalue')

pltEigen_CCM_TE <- ggplot(vizDataEigen,aes(x = timeScaleCut,y = Eigenvalue,color = EigenType)) + 
  xlab('Year') + ylab('Eigenvalue') + 
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c('CCM','TE') ,values=c('blue','red')) + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  scale_x_continuous(breaks=seq(2002,2014,1))
pltEigen_CCM_TE

pltMean_CCM_TE <- ggplot(vizDataMean,aes(x = timeScaleCut,y = Meanvalue,color = MeanType)) + 
  xlab('Year') + ylab('Mean value') + 
  geom_line(size = 1) + 
  theme_bw() + 
  scale_color_manual(labels = c('CCM','TE') ,values=c('blue','red')) + 
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
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  scale_x_continuous(breaks=seq(2002,2014,1))
pltMean_CCM_TE



plt_Eig <- ggplot(df.CCMvsTE,aes(x = ccmEigTimeNorm,y = teEigTimeNorm)) + 
  xlab('Eigenvalue from CCM') + ylab('Eigenvalue from TE') + 
  xlim(c(0.14,0.26)) + 
  # ylim(c(0.5,1))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = ('FittingEigen'))) +
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = teEigTimeNorm ~ ccmEigTimeNorm))[2],2)))),
    values=c('FittingEigen' = 'black')) +
  theme_bw() + 
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(legend.title=element_blank())
plt_Eig

plt_Mean <- ggplot(df.CCMvsTE,aes(x = ccmMeanTimeNorm,y = teMeanTimeNorm)) + 
  xlab('Mean from CCM') + ylab('Mean from TE') + 
  xlim(c(0.14,0.26)) + 
  # ylim(c(0.5,1))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
                                  formula = teMeanTimeNorm ~ ccmMeanTimeNorm))[2],2)))),
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
plt_Mean

# lm(formula = teMeanTime ~ ccmMeanTime)
# coefficients(lm(formula = teMeanTime ~ ccmMeanTime))
