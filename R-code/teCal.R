library("rJava")
library(readr)
# setwd('D:/rProject/ccm')
source('teCalFunc_JIDT.R')
data_betaXY0 <- read_csv("data_betaXY0.csv")
numBeta <- ncol(data_betaXY0)
teXY <-  matrix(nrow = 1,ncol = 101)
teYX <-  matrix(nrow = 1,ncol = 101)
.jinit()
.jaddClassPath("infodynamics.jar")

for (kk in 3:numBeta){
  teXY[,kk-2] <- teCalFunc_JIDT(data_betaXY0$y0,data_betaXY0[[kk]])
  teYX[,kk-2] <- teCalFunc_JIDT(data_betaXY0[[kk]],data_betaXY0$y0)
}
plot(seq(0,1,0.01), teXY, type = "l", col = "blue", lwd = 2,
     xlab = expression(beta[yx]), ylab = 'TE',ylim = c(0,2),cex.axis = 3);
lines(seq(0,1,0.01), teYX, lwd = 2, col = "red");
legend(x = "topleft", legend = c("X to Y","Y to X"), col = c("blue","red"), lwd = 2,
       bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.5)