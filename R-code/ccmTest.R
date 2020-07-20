library('rEDM');
data(sardine_anchovy_sst);
anchovy_xmap_sst <- ccm(sardine_anchovy_sst, E = 3, lib_column = "sardine", 
                        target_column = "anchovy", lib_sizes = seq(10, 80, by = 10), num_samples = 100, 
                        random_libs = TRUE, replace = TRUE, silent = TRUE);
sst_xmap_anchovy <- ccm(sardine_anchovy_sst, E = 3, lib_column = "anchovy", target_column = "sardine", 
                        lib_sizes = seq(10, 80, by = 10), num_samples = 100, random_libs = TRUE, 
                        replace = TRUE, silent = TRUE);
str(anchovy_xmap_sst);
a_xmap_t_means <- ccm_means(anchovy_xmap_sst);
t_xmap_a_means <- ccm_means(sst_xmap_anchovy);

plot(a_xmap_t_means$lib_size, pmax(0, a_xmap_t_means$rho), type = "l", col = "red", lwd = 2,
     xlab = "L", ylab = expression(rho), ylim = c(0, 1), cex.axis = 3);
lines(t_xmap_a_means$lib_size, pmax(0, t_xmap_a_means$rho), lwd = 2, col = "blue");
legend(x = "topleft", legend = c("Anchovy to Sardine", "Sardine to Anchovy"), col = c("red", 
    "blue"), lwd = 1, bty = "n", inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 0.8)

ccmMax = pmax(0, a_xmap_t_means$rho)/max(pmax(0, a_xmap_t_means$rho))
teValue = teAnc_SST_rho
# rho_te_df=data.frame(x=teValue,y=ccmMax)
# lm.rho_te_df <- lm(y ~ x,data=rho_te_df)

plot(teValue,pmax(0, a_xmap_t_means$rho),xlab = 'TE',ylab = expression(rho),xlim = c(0,1)
     ,ylim = c(0,1), pch = 20, cex.axis = 3,col = "red", cex = 4)
legend(x = "topright", legend = c("Anchovy to Sardine"),
      col = c("red"), bty = "n", pch = 20,
       pt.cex = 4,inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 1)
# abline(lm.rho_te_df,col = "red",lwd = 3)

# legend(x = "topright", legend = c("SST to Anchovy",
#        paste0('k = ',as.character(round(lm.rho_te_df$coefficients[[2]],3)))), 
#        col = c("red","red"), bty = "n", pch = 20, lty = 1, lwd = 3,
#        pt.cex = 4,inset = 0.02, cex = 1, x.intersp = 0.2,y.intersp = 0.5,seg.len = 1)
