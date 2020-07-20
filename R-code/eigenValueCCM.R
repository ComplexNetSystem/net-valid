# For Fish Community Data.
library('rEDM')
library(readr)
source('alphaOverNet.R')

####----------- Data and Parameter Initialization -----------####

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")

timeAllLen <- nrow(data_fishCommunity)
numSpecies <- 15L
selectLen <- 30L
timeStep <- 1L
calTimes <- timeAllLen - selectLen + 1L
ccmMatTime <- matrix(nrow = numSpecies,ncol = numSpecies)
ccmEigTime <- matrix(nrow = calTimes,ncol = 1)
ccmMeanTime <- matrix(nrow = calTimes,ncol = 1)
ccmRatio <- matrix(nrow = calTimes,ncol = 1)
ccmNumRatio <- matrix(nrow = calTimes,ncol = 1)
alphaNetworkTimeCCM <- matrix(nrow = calTimes,ncol = 1)
alphaNetworkTimeCCMThre <- matrix(nrow = calTimes,ncol = 11)

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

####----------- Dominant Eigenvalue Calculation of CCM-----------####

for (tt in 1:calTimes) {
  data_FishCut <- data_fishCommunity[tt:(tt+selectLen-1),]
  for (kk in 1:numSpecies) {
    for (jj in 1:numSpecies) {
      if(kk == jj)
        ccmMatTime[kk,jj] <- 0
      else{
        ccmTablePair <- ccm(data_FishCut, E = 3, 
                            lib_column = colnames(data_fishCommunity)[[3+kk]],
                            target_column = colnames(data_fishCommunity)[[3+jj]],
                            lib_sizes = seq(10,30,by = 5),
                            num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
        ccmTablePairMean <- ccm_means(ccmTablePair)
        ccmMatTime[kk,jj] <- ccmTablePairMean$rho[length(ccmTablePairMean$rho)]
      }
    }
  }
  ccmMatTime[is.na(ccmMatTime)] <- 0
  
  ccmEigTime[tt,] <- max(Re(eigen(ccmMatTime)$values))
  ccmMeanTime[tt,] <- sum(ccmMatTime)/(numSpecies^2-numSpecies)
  
  ccmMatTimeVector <- as.vector(ccmMatTime)
  ccmMatTimeVectorNoDiag <- ccmMatTimeVector[-seq(1,numSpecies^2,(numSpecies+1))] # Remove Diagonal elements
  ccmRatio[tt,] <- sum(ccmMatTimeVectorNoDiag[ccmMatTimeVectorNoDiag>=median(ccmMatTimeVectorNoDiag)])/sum(ccmMatTimeVectorNoDiag)
  ccmNumRatio[tt,] <- length(ccmMatTimeVectorNoDiag[ccmMatTimeVectorNoDiag>=median(ccmMatTimeVectorNoDiag)])/length(ccmMatTimeVectorNoDiag)
  
  alphaNetworkTimeCCM[tt,] <- alphaOverNetwork(ccmMatTime,-1)
  for (ii in 0:10) {
    alphaNetworkTimeCCMThre[tt,ii+1] <- alphaOverNetwork(ccmMatTime,ii*0.1)
  }
  
}

cc_alpha <- 0
for (ii in seq(dim(alphaNetworkTimeCCMThre)[2])) {
  # cc_update <- cor(alphaOverTime, alphaNetworkTimeCCMThre[,ii])
  cc_update <- mutinformation(alphaOverTime, alphaNetworkTimeCCMThre[,ii])
  if(is.na(cc_update)){
    next
  } else if (cc_update >= cc_alpha){
    cc_alpha <- cc_update
    alphaNetworkTimeCCMThre_optimal <- alphaNetworkTimeCCMThre[,ii]
    print(ii)
  }
}


####----------- Data Visualization -----------####

par(mar = c(5.2,6.5,3,2),mgp = c(4,1.5,0))

plot(timeScaleCut,ccmEigTime, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Eigen Value', cex.axis = 3,cex.lab = 3)
plot(timeScaleCut,ccmMeanTime, type = "l", col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Mean interaction', cex.axis = 3,cex.lab = 3)
# plot(timeScaleCut,ccmRatio, type = "l", col = "blue", lwd = 2,panel.first = grid(),
#      xlab = 'Year', ylab = 'High ratio', cex.axis = 3,cex.lab = 3)
# plot(timeScaleCut,ccmNumRatio, type = "l", col = "blue", lwd = 2,panel.first = grid(),
#      xlab = 'Year', ylab = 'Mean interaction', cex.axis = 3,cex.lab = 3)
# plot(teNumRatio,ccmNumRatio, col = "blue", lwd = 2,panel.first = grid(),
#      xlab = 'Year', ylab = 'Mean interaction', cex.axis = 3,cex.lab = 3)
plot(timeScaleCut,alphaNetworkTimeCCMThre, type = "l",col = "blue", lwd = 2,panel.first = grid(),
     xlab = 'Year', ylab = 'Alpha (CCM>0)', ylim = c(0,15), cex.axis = 3,cex.lab = 3)
