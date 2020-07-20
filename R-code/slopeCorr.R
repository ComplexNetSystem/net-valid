library(data.table)

df.slopeAbd <- fread('slopeFishAbd.csv', header = FALSE, sep = ',')
slope <- as.matrix(df.slopeAbd)

slopeVector <- slope[lower.tri(slope,diag = TRUE)]
corrVectorLower <- corrMat[lower.tri(corrMat,diag = FALSE)]

df.corrSlope <- data.frame(
  corrVectorLower,
  slopeVector
)

slopeVScorr <- ggplot(df.corrSlope,aes(x = corrVectorLower,y = slopeVector)) + 
  xlab(expression(sigma)) + ylab('Slope') + 
  # xlim(c(-0.2,0.8)) +
  # ylim(c(0,0.8))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = slopeVector ~ corrVectorLower))[2],2)))),
    values=c('Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
slopeVScorr