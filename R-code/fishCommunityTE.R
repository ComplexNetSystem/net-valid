library('rEDM');
library("rJava")
library('lattice')
library('ggplot2')
library(entropy)
library(readr)
source('teCalFunc_JIDT.R')
source('teCalFunc_JIDT_lag.R')
.jinit()
.jaddClassPath("infodynamics.jar")

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")
numSpecies <- 15L
t_lag <- 1L
binWidth <- 0.25

teMat <- matrix(nrow = numSpecies,ncol = numSpecies)
corrMat <- matrix(nrow = numSpecies,ncol = numSpecies)
shannonEnt <- matrix(nrow = numSpecies,ncol = 1)
teExample <- matrix(nrow = 2,ncol = t_lag)

for (kk in 1:numSpecies) {
  for (jj in 1:numSpecies) {
    corrMat[kk,jj] <- cor(data_fishCommunity[[3+kk]],data_fishCommunity[[3+jj]])
  }
}

for (kk in 1:numSpecies) {
  for (jj in 1:numSpecies) {
    if (kk == jj)
      teMat[kk,jj] <- 0
    else
      teMat[kk,jj] <- teCalFunc_JIDT_lag(data_fishCommunity[[3+kk]],data_fishCommunity[[3+jj]],t_lag,binWidth)
  }
  
}

for (jj in 1:numSpecies) {
  shannonEnt[jj,] <- entropy(data_fishCommunity[[3+jj]],method = 'ML')
}
thre <- seq(min(teMat),max(teMat),length.out = 100)
teThre <- teOverThreshold(teMat,thre)
networkEntOverThre <- sum(shannonEnt) + teThre

par(mar = c(5.2,6.5,3,2),mgp = c(4,1.5,0))
plot(thre,networkEntOverThre, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Threshold', ylab = 'Network Entropy',
     xlim = c(0,1),ylim = c(50,100),
     cex.axis = 3,cex.lab = 3)


teMat[teMat < 0] <- 0
at_colorLabel <- seq(0,1,by = 0.2)
at_colorRange <- seq(0,1,by = 0.01)


lattice::levelplot(teMat/max(teMat),
                   xlab = list('Species(1:15)',cex = 2),
                   ylab = list('Species(1:15)',cex = 2),
                   xlim = 1:numSpecies,
                   ylim = 1:numSpecies,
                   scales = list(cex = 2), 
                   main = list(expression('TE/max(TE)' * ' ' * '('*tau*'='*1*')'),cex = 2),
                   at = at_colorRange, 
                   colorkey = list(labels = list(at = at_colorLabel,cex = 2)), 
                   col.regions = colorRamps::matlab.like(length(at_colorRange)))
