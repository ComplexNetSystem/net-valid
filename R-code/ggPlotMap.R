library(ggplot2)
library(reshape2)
source('mat2vector.R')

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")
numSpecies <- 15L
teMatNorm <- teMat/max(teMat)
ccmMatNorm <- ccmMat/max(ccmMat)

####-------- data frame construction -----------####

df_TE <- as.data.frame(teMatNorm)
df_CCM <- as.data.frame(ccmMatNorm)
df_COR <- as.data.frame(corrMat)
# rownames(df_TE) <- paste('Spec',1:15,sep = '_')
# colnames(df_TE) <- paste('Var',1:15,sep = '_')
rownames(df_TE) <- as.character(1:numSpecies)
colnames(df_TE) <- as.character(1:numSpecies)
rownames(df_CCM) <- as.character(1:numSpecies)
colnames(df_CCM) <- as.character(1:numSpecies)
rownames(df_COR) <- as.character(1:numSpecies)
colnames(df_COR) <- as.character(1:numSpecies)

df_TE$Source <- rownames(df_TE)
data_TE <- melt(df_TE, id.vars=c("Source"),variable.name = 'Target',value.name = 'Weight')
data_TE$type <- rep('directed',numSpecies^2)

df_CCM$Source <- rownames(df_CCM)
data_CCM <- melt(df_CCM, id.vars=c("Source"),variable.name = 'Target',value.name = 'Weight')
data_CCM$type <- rep('directed',numSpecies^2)

df_COR$Source <- rownames(df_COR)
data_COR <- melt(df_COR, id.vars=c("Source"),variable.name = 'Target',value.name = 'Weight')
data_COR$type <- rep('directed',numSpecies^2)

####-------- Save the data_CCM and data_TE as .csv File -----------####

write.table(data_TE[data_TE$Weight>0.05,],"edges-TE.csv",row.names=F,sep = ',')
write.table(data_CCM[data_CCM$Weight>0,],"edges-CCM.csv",row.names=F,sep = ',')

####-------- Data Visualization -----------####

pTE <- ggplot(data_TE,aes(y = Target,x = factor(Source,levels = as.character(1:15)))) + #reorder(variable,Species)
  xlab("Species") +
  ylab("Species") +
  # # title("TE/abs(TE)") +
  # labs(x = "Species",y = "Species",title = "TE/abs(TE)") + 
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(legend.key = element_blank())  +
  theme(axis.text.x = element_text(hjust=1, vjust=1,size = 25)) +
  theme(axis.text.y = element_text(hjust=1, vjust=1,size = 25)) +
  theme(legend.text = element_text(size = 20)) +  
  theme(legend.position = "right") +  
  theme(title=element_text(hjust = 0.5, vjust = 0.5,size = 25)) + 
  geom_tile(aes(fill = Weight)) + 
  scale_fill_gradient2("TE/max(TE)",low="blue", high="red", mid="yellow",
                       midpoint= (max(data_TE$Weight)+min(data_TE$Weight))/2) + 
  theme(aspect.ratio=1)
pTE

pCCM <- ggplot(data_CCM,aes(y = Target,x = factor(Source,levels = as.character(1:15)))) + #reorder(variable,Species)
  xlab("Species") +
  ylab("Species") +
  # # title("TE/abs(TE)") +
  # labs(x = "Species",y = "Species",title = "TE/abs(TE)") + 
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(legend.key = element_blank())  +
  theme(axis.text.x = element_text(hjust=1, vjust=1,size = 25)) +
  theme(axis.text.y = element_text(hjust=1, vjust=1,size = 25)) +
  theme(legend.text = element_text(size = 20)) +  
  theme(legend.position = "right") +  
  theme(title=element_text(hjust = 0.5, vjust = 0.5,size = 25)) + 
  geom_tile(aes(fill = Weight)) + 
  scale_fill_gradient2(expression(rho * '/max(' * rho * ')'),low="blue", high="red", mid="yellow",
                       midpoint= (max(data_CCM$Weight)+min(data_CCM$Weight))/2) + 
  theme(aspect.ratio=1)
pCCM

pCOR <- ggplot(data_COR,aes(y = Target,x = factor(Source,levels = as.character(1:15)))) + #reorder(variable,Species)
  xlab("Species") +
  ylab("Species") +
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(legend.key = element_blank())  +
  theme(axis.text.x = element_text(hjust=1, vjust=1,size = 25)) +
  theme(axis.text.y = element_text(hjust=1, vjust=1,size = 25)) +
  theme(legend.text = element_text(size = 20)) +  
  theme(legend.position = "right") +  
  theme(title=element_text(hjust = 0.5, vjust = 0.5,size = 25)) + 
  geom_tile(aes(fill = Weight)) + 
  scale_fill_gradient2('Correlation',low="blue", high="red", mid="yellow",
                       midpoint= (max(data_COR$Weight)+min(data_COR$Weight))/2) + 
  theme(aspect.ratio=1)
pCOR

# teVector  <- as.vector(t(teMat))
# rhoVector <- as.vector(t(ccmMat))
# corVector <- as.vector(t(corrMat))
teVector  <- mat2vector(teMat)
rhoVector <- mat2vector(ccmMat)
corVector <- mat2vector(corrMat)

df.fishTErho <- data.frame(
  teVector,
  rhoVector,
  corVector
)
teVSrho <- ggplot(df.fishTErho,aes(x = rhoVector,y = teVector)) + 
  xlab(expression(rho)) + ylab('TE') + 
  xlim(c(0,0.8)) +
  ylim(c(0,0.8))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = teVector ~ rhoVector))[2],2)))),
    values=c('Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
teVSrho

rhoVScor <- ggplot(df.fishTErho,aes(x = corVector,y = rhoVector)) + 
  xlab(expression(sigma)) + ylab(expression(rho)) + 
  xlim(c(-0.2,0.8)) +
  ylim(c(0,0.8))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = rhoVector ~ corVector))[2],2)))),
    values=c('Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
rhoVScor

teVScor <- ggplot(df.fishTErho,aes(x = corVector,y = teVector)) + 
  xlab(expression(sigma)) + ylab('TE') + 
  xlim(c(-0.2,0.8)) +
  ylim(c(0,0.8))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = teVector ~ corVector))[2],2)))),
    values=c('Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
teVScor

corrMaxIndex <- which(corrMat == max(corVector),arr.ind = TRUE)
corrMinIndex <- which(corrMat == min(corVector),arr.ind = TRUE)

corrMaxSpecies1 <- data_fishCommunity[[corrMaxIndex[1,1]+3]]
corrMaxSpecies2 <- data_fishCommunity[[corrMaxIndex[1,2]+3]]
corrMinSpecies1 <- data_fishCommunity[[corrMinIndex[1,1]+3]]
corrMinSpecies2 <- data_fishCommunity[[corrMinIndex[1,2]+3]]

df.abdCorr <- data.frame(
  corrMaxSpecies1,
  corrMaxSpecies2,
  corrMinSpecies1,
  corrMinSpecies2
)
max1vs2 <- ggplot(df.abdCorr,aes(x = corrMaxSpecies1,y = corrMaxSpecies2)) + 
  xlab('Abundance') + ylab('Abundance') + 
  # xlim(c(-0.2,0.8)) +
  # ylim(c(0,0.8))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = corrMaxSpecies2 ~ corrMaxSpecies1))[2],2)))),
    values=c('Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
max1vs2

min1vs2 <- ggplot(df.abdCorr,aes(x = corrMinSpecies1,y = corrMinSpecies2)) + 
  xlab('Abundance') + ylab('Abundance') + 
  # xlim(c(-0.2,0.8)) +
  # ylim(c(0,0.8))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula=y ~ poly(x, 1, raw=TRUE),
              se = FALSE, size = 2,aes(color = c('Fitting'))) +
  
  scale_color_manual(labels = c(paste0('k=',as.character(round(coefficients(lm(
    formula = corrMinSpecies2 ~ corrMinSpecies1))[2],2)))),
    values=c('Fitting' = 'black')) +
  theme_bw() +
  theme(axis.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  theme(axis.title = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) + 
  theme(legend.title=element_blank())
min1vs2
