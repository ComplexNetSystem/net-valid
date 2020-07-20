library(data.table)
library(ggplot2)
library(GGally)

data_fishCommunity <- fread("Maizuru_dominant_sp.csv")
numSpecies <- 15
dataAbd <- data_fishCommunity[,4:18]
dataAbd[dataAbd == 0] <- 1
names(dataAbd) <- as.character(seq(1:numSpecies))

# define new log10 functions
log10_diagonal <- function(data, mapping, ...) {
  ggally_densityDiag(data, mapping, ...) + scale_x_log10()
}
log10_points <- function(data, mapping, ...) {
  ggally_points(data, mapping, ...) + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = 'lm',se=FALSE,colour = 'blue')
}
log10_cor <- function(data, mapping, ...) {
  # preprocess the data for the correlation calculations
  print(mapping$x)
  print(quo_name(mapping$x))
  data[[quo_name(mapping$x)]] <- log10(data[[quo_name(mapping$x)]])
  data[[quo_name(mapping$y)]] <- log10(data[[quo_name(mapping$y)]])
  
  ggally_cor(data, mapping, ...) + # grids will not match. hide them
    theme(
      panel.grid.major = element_blank(), 
      panel.border = element_rect(color = "black", fill = NA)
    )
}

ggpairs(dataAbd,xlab = 'log(Abundance)',ylab = 'log(Abundance)',
        upper = list(continuous = log10_cor),
        lower = list(continuous = log10_points),
        diag = list(continuous = log10_diagonal))

figName <- 'abd-abd-log.eps'
ggsave(file = figName, width = 20, height = 12)