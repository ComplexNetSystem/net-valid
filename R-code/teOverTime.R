library('rEDM');
library("rJava")
library('plot3D')
library(plotly)
library(readr)
source('teCalFunc_JIDT.R')
.jinit()
.jaddClassPath("infodynamics.jar")

####----------- Data and Parameter Initialization -----------####

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")

timeAllLen <- nrow(data_fishCommunity)
numSpecies <- 15L
selectLen <- 30L
timeStep <- 1L
numPair <- 14L
t_lag <- 1L
calTimes <- timeAllLen - selectLen + 1L
teTimePair <- matrix(nrow = calTimes,ncol = numPair)
# teTimePair3D <- array(dim = c(1,calTimes,numPair)) # Create a 3D array to save data.
selectPair <- matrix(as.integer(c(17,8,8,17,8,4,12,11,11,7,6,10,15,13,
                    16,11,9,14,6,15,18,8,14,18,14,13,14,10)),
                    nrow = numPair,ncol = 2,byrow = TRUE)

####----------- Sample to Time Scale -----------####

startYear <- 2002
startMonth <- 6.5    # June-Early
endYear <- 2014
endMonth <- 4.5      # April-Early
samplePerYear <- 24
samplePerMonth <- 2
timeInterval <- 1/samplePerYear
fromSeq <- startYear + (startMonth-1)*samplePerMonth*timeInterval
toSeq <- endYear + (endMonth-1)*samplePerMonth*timeInterval
timeScale <- seq(fromSeq,toSeq,length.out = timeAllLen)
timeScaleCut <- timeScale[selectLen:timeAllLen]

####----------- TE over Time Calculation -----------####

for (jj in 1:numPair) {
  srcSpecies <- data_fishCommunity[[selectPair[jj,1]]]
  dstSpecies <- data_fishCommunity[[selectPair[jj,2]]]
  for (kk in 1:calTimes) {
    srcData <- srcSpecies[kk:(kk+selectLen-1)]   ## "()" is a must
    dstData <- dstSpecies[kk:(kk+selectLen-1)]
    teTimePair[kk,jj] <- teCalFunc_JIDT_lag(srcData,dstData,t_lag)
  }
}


####----------- Data Visualization -----------####

## Please see plot_3dSpace.R

####----------- Histogram of Mean Interaction -----------####

meanTE <- colMeans(teTimePair)
par(mar=c(5,6,4,2)+1)
h <- hist(meanTE, breaks=20, col="blue",xlim = c(0,0.8),
          main = '',xlab = 'Mean(TE)',cex.axis = 3,cex.lab = 3)

