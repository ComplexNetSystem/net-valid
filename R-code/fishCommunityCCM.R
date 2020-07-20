# For Fish Community Data.
library('rEDM')
library(readr)
data_fishCommunity <- read.csv("Maizuru_dominant_sp.csv")
numSpecies <- 15L
ccmMat <- matrix(nrow = numSpecies,ncol = numSpecies)

for (kk in 1:numSpecies) {
  for (jj in 1:numSpecies) {
    if (kk == jj)
      ccmMat[kk,jj] <- 0
    else{
      ccmTableXY <- ccm(data_fishCommunity, E = 16, lib_column = colnames(data_fishCommunity)[[3+kk]],
                        target_column = colnames(data_fishCommunity)[[3+jj]], 
                        lib_sizes = seq(15,length(data_fishCommunity$Aurelia.sp),by = 10),
                        num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
      ccmTableXYMean <- ccm_means(ccmTableXY)
      ccmMat[kk,jj] <- ccmTableXYMean$rho[length(ccmTableXYMean$rho)]
    }
  }
}

ccmMat[ccmMat<0] <- 0
at_colorLabel <- seq(0,1,by = 0.2)
at_colorRange <- seq(0,1,by = 0.001)


lattice::levelplot(ccmMat/max(ccmMat),
                   xlab = list('Species(1:15)',cex = 2),
                   ylab = list('Species(1:15)',cex = 2),
                   xlim = 1:numSpecies,
                   ylim = 1:numSpecies,
                   scales = list(cex = 2), 
                   main = list(expression(rho * '/max(' * rho * ')'),cex = 2),
                   at = at_colorRange, 
                   colorkey = list(labels = list(at = at_colorLabel,cex = 2)), 
                   col.regions = colorRamps::matlab.like(length(at_colorRange)))

# ccmTableXY <- ccm(data_fishCommunity, E = 3, lib_column = colnames(data_fishCommunity)[[12]], 
#            target_column = colnames(data_fishCommunity)[[11]], lib_sizes = seq(15,285,by = 10),
#            num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
# ccmTableYX <- ccm(data_fishCommunity, E = 3, lib_column = colnames(data_fishCommunity)[[11]], 
#           target_column = colnames(data_fishCommunity)[[12]], lib_sizes = seq(15,285,by = 10),
#           num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)
# ccmTableXYMean <- ccm_means(ccmTableXY)
# ccmTableYXMean <- ccm_means(ccmTableYX)
# 
# plot(ccmTableXYMean$lib_size, pmax(0, ccmTableXYMean$rho), type = "l", col = "red", lwd = 2,
#      xlab = "L", ylab = expression(rho), ylim = c(0, 1), cex.axis = 3);
# lines(ccmTableYXMean$lib_size, pmax(0, ccmTableYXMean$rho), lwd = 2, col = "blue");
# legend(x = "topleft", legend = c("Anchovy to Sardine", "Sardine to Anchovy"), col = c("red", 
#     "blue"), lwd = 1, bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.8)
