library(data.table)
library(GGally)

data_fishCommunity <- fread("Maizuru_dominant_sp.csv")
numSpecies <- 15
dataAbd <- data_fishCommunity[,4:18]
names(dataAbd) <- as.character(seq(1:numSpecies))


plot_func <- function(data, mapping,pts=list(),smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt) 
}

corPanel <- function(data, mapping, ...) {
  # preprocess the data for the correlation calculations
  
  ggally_cor(data, mapping, ...) + # grids will not match. hide them
    theme(
      panel.grid.major = element_blank(), 
      panel.border = element_rect(color = "black", fill = NA)
    )
}


ggpairs(dataAbd,xlab = 'Abundance',ylab = 'Abundance',
        upper = list(continuous = corPanel),
        # lower = list(continuous=wrap("smooth", colour="blue")), # Change the color of points
        # lower = list(continuous = "smooth"),
        lower = list(continuous = wrap(plot_func,
                            pts=list(colour="black"),
                            smt=list(method="lm",se=FALSE,colour="blue"))),
        diag = list(continuous = "densityDiag"))

figName <- 'abd_abd.eps'
ggsave(file = figName, width = 20, height = 12)