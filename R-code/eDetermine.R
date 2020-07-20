# For Fish Community Data.
library('rEDM')
library(readr)

####----------- Data and Parameter Initialization -----------####

data_fishCommunity <- read_csv("Maizuru_dominant_sp.csv")
numSpecies <- 15L
lenData <- nrow(data_fishCommunity)
lib <- c(1, lenData)
pred <- c(1, lenData)

for (ii in 1:numSpecies) {
  simplex_output <- simplex(data_fishCommunity[[3+ii]],E = 1:24, lib, pred, tau = 1)
  par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set up margins for plotting
  plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
       ylab = "Forecast Skill (rho)")
}