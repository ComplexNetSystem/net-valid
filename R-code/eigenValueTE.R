library('rEDM');
library("rJava")
library('plot3D')
library(plotly)
library(readr)
library(entropy)
source('teCalFunc_JIDT.R')
source('alphaOverNet.R')
.jinit()
.jaddClassPath("infodynamics.jar")

####----------- Data and Parameter Initialization -----------####

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")

timeAllLen <- nrow(data_fishCommunity)
numSpecies <- 15L
selectLen <- 30L
timeStep <- 1L
t_lag <- 1L
calTimes <- timeAllLen - selectLen + 1L
teMatTime <- matrix(nrow = numSpecies,ncol = numSpecies)
teEigTime <- matrix(nrow = calTimes,ncol = 1)
teMeanTime <- matrix(nrow = calTimes,ncol = 1)
teRatio <- matrix(nrow = calTimes,ncol = 1)
teNumRatio <- matrix(nrow = calTimes,ncol = 1)
simpsonDiversity <- matrix(nrow = timeAllLen,ncol = 1)
alphaDiversity <- matrix(nrow = timeAllLen,ncol = 1)
alphaNetworkTimeTE <- matrix(nrow = calTimes,ncol = 1)
alphaNetworkTimeTEThre <- matrix(nrow = calTimes,ncol = 11)
shannonTimeSpecies <- matrix(nrow = numSpecies,ncol = 1)
shannonIndex <- matrix(nrow = calTimes,ncol = 1)

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

####----------- Dominant Eigenvalue Calculation of TE -----------####

for (tt in 1:calTimes) {
  for (kk in 1:numSpecies) {
    srcSpecies <- data_fishCommunity[[3+kk]]
    srcData <- srcSpecies[tt:(tt+selectLen-1)]
    shannonTimeSpecies[kk,] <- entropy::entropy(srcData,method = 'CS') # ML,CS,Laplace,
    for (jj in 1:numSpecies) {
      if (kk == jj)
        teMatTime[kk,jj] <- 0
      else{
        dstSpecies <- data_fishCommunity[[3+jj]]
        dstData <- dstSpecies[tt:(tt+selectLen-1)]
        teMatTime[kk,jj] <- teCalFunc_JIDT_lag(srcData,dstData,t_lag)
      }
        
    }
    
  }
  shannonIndex[tt,] <- sum(shannonTimeSpecies)
  teEigTime[tt,] <- max(Re(eigen(teMatTime)$values))
  teMeanTime[tt,] <- sum(teMatTime)/(numSpecies^2-numSpecies)
  
  teMatTimeVector <- as.vector(teMatTime)
  teMatTimeVectorNoDiag <- teMatTimeVector[-seq(1,numSpecies^2,(numSpecies+1))] # Remove Diagonal elements
  # teRatio[tt,] <- sum(teMatTimeVectorNoDiag>=median(teMatTimeVectorNoDiag))/(numSpecies^2-numSpecies)
  teRatio[tt,] <- sum(teMatTimeVectorNoDiag[teMatTimeVectorNoDiag>=median(teMatTimeVectorNoDiag)])/sum(teMatTimeVectorNoDiag)
  teNumRatio[tt,] <- length(teMatTimeVectorNoDiag[teMatTimeVectorNoDiag>=median(teMatTimeVectorNoDiag)])/length(teMatTimeVectorNoDiag)
  
  alphaNetworkTimeTE[tt,] <- alphaOverNetwork(teMatTime,0)
  for (ii in 0:10) {
    alphaNetworkTimeTEThre[tt,ii+1] <- alphaOverNetwork(teMatTime,ii*0.1)
  }
  
}

cc_alpha <- 0
for (ii in seq(dim(alphaNetworkTimeTEThre)[2])) {
  # cc_update <- cor(alphaOverTime, alphaNetworkTimeTEThre[,ii])
  cc_update <- mutinformation(alphaOverTime, alphaNetworkTimeTEThre[,ii])
  if(is.na(cc_update)){
    next
  } else if (cc_update >= cc_alpha){
    cc_alpha <- cc_update
    alphaNetworkTimeTEThre_optimal <- alphaNetworkTimeTEThre[,ii]
    print(ii)
    print(cc_alpha)
  }
}


####----------- Simpson's Diversity Index -----------####

dataMat_fishCommunity <- as.matrix(data_fishCommunity[4:18])  # with colnames
#  colnames(dataMat_fishCommunity) <- NULL                    # Remove Column Names
for (mm in 1:timeAllLen) {
  data_mm <- dataMat_fishCommunity[mm,]
  simpsonDiversity[mm,] <- 1 - sum(data_mm*(data_mm-1))/(sum(data_mm)*(sum(data_mm)-1))
  alphaDiversity[mm,] <- numSpecies - as.matrix(table(data_mm))[1]
}

####----------- Data Visualization -----------####

par(mar = c(5.2,6.5,3,2),mgp = c(4,1.5,0))

plot(timeScaleCut,teEigTime, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Eigenvalue (TE)', ylim = c(0,8),cex.axis = 3,cex.lab = 3)
plot(timeScaleCut,teMeanTime, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Mean interaction', cex.axis = 3,cex.lab = 3)
# plot(timeScaleCut,teRatio, type = "l", col = "blue", lwd = 2,panel.first = grid(),
#      xlab = 'Year', ylab = 'High ratio', cex.axis = 3,cex.lab = 3)
# plot(timeScaleCut,teNumRatio, type = "l", col = "blue", lwd = 2,panel.first = grid(),
#      xlab = 'Year', ylab = 'High ratio', cex.axis = 3,cex.lab = 3)
plot(timeScale,simpsonDiversity, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Simpson\'s Diversity', ylim = c(0,1),cex.axis = 3,cex.lab = 3)
plot(timeScaleCut,alphaNetworkTimeTEThre, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Alpha (TE>0.5)',ylim = c(0,15), cex.axis = 3,cex.lab = 3)
# lines(timeScaleCut,alphaNetworkTimeTE, type = "l", col = "red", lwd = 2)
plot(timeScaleCut,shannonIndex, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'H(t)', cex.axis = 3,cex.lab = 3)

