# For Simulation Data.
library('rEDM')
library(readr)
data_betaXY0_2_Y_X <- read_csv("data_betaXY0_2_Y_X.csv")
numBeta <- ncol(data_betaXY0_2_Y_X)
dataLen <- nrow(data_betaXY0_2_Y_X)
ccmXY_0.5 <-  matrix(nrow = 10,ncol = 101)
ccmYX_0.5 <-  matrix(nrow = 10,ncol = 101)
for (kk in 2:102) {
  ccmTableXY <- ccm(data_betaXY0_2_Y_X, E = 3, lib_column = paste0('x',as.character(kk-1)), 
                    target_column = paste0('y',as.character(kk-1)), lib_sizes = seq(10, 100, by = 10), 
                    num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
  ccmTableXYMean <- ccm_means(ccmTableXY)
  ccmXY_0.5[,kk-2] <- matrix(ccmTableXYMean$rho)
  
  ccmTableYX <- ccm(data_betaXY0_2_Y_X, E = 3, lib_column = paste0('y',as.character(kk-1)), 
                    target_column = paste0('x',as.character(kk-1)), lib_sizes = seq(10, 100, by = 10), 
                    num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
  ccmTableYXMean <- ccm_means(ccmTableYX)
  ccmYX_0.5[,kk-2] <- matrix(ccmTableYXMean$rho)
}
plot(seq(0,1,0.01), ccmXY_0.5[1,], type = "l", col = "blue", lwd = 2,
     xlab = expression(beta[yx]), ylab = expression(rho), ylim = c(0, 1),cex.axis = 3);
lines(seq(0,1,0.01), ccmYX_0.5[1,], lwd = 2, col = "red");
legend(x = "topleft", legend = c("X to Y","Y to X"), col = c("blue","red"), lwd = 2,
       bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.5)
