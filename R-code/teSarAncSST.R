library('rEDM');
library("rJava")
library(readr)
source('teCalFunc_JIDT.R')
.jinit()
.jaddClassPath("infodynamics.jar")

data(sardine_anchovy_sst)
t_lag <- 20L

teSar_Anc <-  matrix(nrow = 2,ncol = t_lag)
teSar_SST <-  matrix(nrow = 2,ncol = t_lag)
teAnc_SST <-  matrix(nrow = 2,ncol = t_lag)


for(kk in 1:t_lag){
  teSar_Anc[1,kk] <- teCalFunc_JIDT_lag(sardine_anchovy_sst$sardine,sardine_anchovy_sst$anchovy,kk)
  teSar_Anc[2,kk] <- teCalFunc_JIDT_lag(sardine_anchovy_sst$anchovy,sardine_anchovy_sst$sardine,kk)
  
  teSar_SST[1,kk] <- teCalFunc_JIDT_lag(sardine_anchovy_sst$sardine,sardine_anchovy_sst$sio_sst,kk)
  teSar_SST[2,kk] <- teCalFunc_JIDT_lag(sardine_anchovy_sst$sio_sst,sardine_anchovy_sst$sardine,kk)
  
  teAnc_SST[1,kk] <- teCalFunc_JIDT_lag(sardine_anchovy_sst$anchovy,sardine_anchovy_sst$np_sst,kk)
  teAnc_SST[2,kk] <- teCalFunc_JIDT_lag(sardine_anchovy_sst$np_sst,sardine_anchovy_sst$anchovy,kk)
}

plot(teSar_Anc[1,], type = "l", col = "blue", lwd = 2,
     xlab = 'Time Lag', ylab = 'TE',ylim = c(0,1), cex.axis = 3);
lines(teSar_Anc[2,], lwd = 2, col = "red", cex.axis = 2);
legend(x = "topleft", legend = c("Sardine to Anchovy","Anchovy to Sardine"), col = c("blue","red"), lwd = 2,
       bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.8)

plot(teSar_SST[1,], type = "l", col = "blue", lwd = 2,
     xlab = 'Time Lag', ylab = 'TE',ylim = c(0,1), cex.axis = 3);
lines(teSar_SST[2,], lwd = 2, col = "red", cex.axis = 2);
legend(x = "topleft", legend = c("Sardine to SST","SST to Sardine"), col = c("blue","red"), lwd = 2,
       bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.8)

plot(teAnc_SST[1,], type = "l", col = "blue", lwd = 2,
     xlab = 'Time Lag', ylab = 'TE',ylim = c(0,1), cex.axis = 3);
lines(teAnc_SST[2,], lwd = 2, col = "red", cex.axis = 2);
legend(x = "topleft", legend = c("Anchovy to SST","SST to Anchovy"), col = c("blue","red"), lwd = 2,
       bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.8)

teSar_Anc_ds <- downsample(teSar_Anc,2,0)
teSar_Anc_rho <- teSar_Anc_ds[[2]][2,1:8]
teSar_Anc_rho <- teSar_Anc_rho/max(teSar_Anc_rho)

teSar_SST_ds <- downsample(teSar_SST,2,0)
teSar_SST_rho <- teSar_SST_ds[[2]][2,1:8]
teSar_SST_rho <- teSar_SST_rho/max(teSar_SST_rho)

teAnc_SST_ds <- downsample(teAnc_SST,2,0)
teAnc_SST_rho <- teAnc_SST_ds[[2]][2,1:8]
teAnc_SST_rho <- teAnc_SST_rho/max(teAnc_SST_rho)

