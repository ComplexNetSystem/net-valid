library('rEDM')
library(readr)
data_betaXY0 <- read_csv("data_betaXY0.csv")
numBeta <- ncol(data_betaXY0)
dataLen <- nrow(data_betaXY0)
ccmXY <-  matrix(nrow = 10,ncol = 101)
ccmYX <-  matrix(nrow = 10,ncol = 101)
for (kk in 3:numBeta) {
  ccmTableXY <- ccm(data_betaXY0, E = 3, lib_column = "y0", 
           target_column = paste0('y',as.character(kk-2)), lib_sizes = seq(10, 100, by = 10), 
           num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
  ccmTableXYMean <- ccm_means(ccmTableXY)
  ccmXY[,kk-2] <- matrix(ccmTableXYMean$rho)
  
  ccmTableYX <- ccm(data_betaXY0, E = 3, lib_column = paste0('y',as.character(kk-2)), 
                  target_column = "y0", lib_sizes = seq(10, 100, by = 10), 
                  num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
  ccmTableYXMean <- ccm_means(ccmTableYX)
  ccmYX[,kk-2] <- matrix(ccmTableYXMean$rho)
}
plot(seq(0,1,0.01), ccmXY[1,], type = "l", col = "blue", lwd = 2,
     xlab = expression(beta[yx]), ylab = expression(rho), ylim = c(0, 1),cex.axis = 3);
lines(seq(0,1,0.01), ccmYX[1,], lwd = 2, col = "red");
legend(x = "topleft", legend = c("X to Y","Y to X"), col = c("blue","red"), lwd = 2,
     bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.5)
