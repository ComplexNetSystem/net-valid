# For Fish Community Data.
library('rEDM')
library(readr)

####----------- Data and Parameter Initialization -----------####

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")

timeAllLen <- nrow(data_fishCommunity)
numSpecies <- 15L
selectLen <- 30L
timeStep <- 1L
numPair <- 14L
t_lag <- 1L
calTimes <- timeAllLen - selectLen + 1L
ccmTimePair <- matrix(nrow = calTimes,ncol = numPair)
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


####----------- CCM over Time Calculation -----------####

for (jj in 1:numPair) {
  for (kk in 1:calTimes) {
    data_FishCut <- data_fishCommunity[kk:(kk+selectLen-1),]  ## "()" is a must
    # teTimePair[kk,jj] <- teCalFunc_JIDT_lag(srcData,dstData,t_lag)
    ccmTablePair <- ccm(data_FishCut, E = 3, 
                      lib_column = colnames(data_fishCommunity)[[selectPair[jj,1]]],
                      target_column = colnames(data_fishCommunity)[[selectPair[jj,2]]], 
                      lib_sizes = seq(10,30,by = 5),
                      num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
    ccmTablePairMean <- ccm_means(ccmTablePair)
    ccmTimePair[kk,jj] <- ccmTablePairMean$rho[length(ccmTablePairMean$rho)]
  }
}

ccmTimePair[is.na(ccmTimePair)] <- 0


