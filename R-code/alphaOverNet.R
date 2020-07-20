alphaOverNetwork <- function(netMat,threshold){
  numSpecies <- nrow(netMat)
  
  if (threshold >= 0){
    netMat[netMat <= threshold] <- 0
  } else {
    print('No need to set threshold!')
  }
  
  for (i in 1:numSpecies) {
    if(netMat[,i] == 0 && netMat[i,] == 0){
      numSpecies <- numSpecies - 1
    }
  }
  return(numSpecies)
}

# alphaOverNetwork <- function(netMat,filterRatio,matType){
#   # matType: 0 --> Network Matrix obtained by CCM.
#   #          1 --> Network Matrix obtained by TE.
#   numSpecies <- nrow(netMat)
#   deltaValue <- max(netMat)-min(netMat)
#   threshold <- filterRatio * deltaValue
#   
#   if (matType == 0){   #### For CCM Network Matrix.
#     if (filterRatio >= 0){
#       netMat[netMat <= threshold & netMat >= -threshold] <- 0
#     } else {
#       print('No need to set threshold!')
#     }
#   } else {             #### For TE Network Matrix.
#     if (filterRatio >= 0){
#       netMat[netMat <= threshold] <- 0
#     } else {
#       print('No need to set threshold!')
#     }
#   }
#   
#   for (i in 1:numSpecies) {
#     if(netMat[,i] == 0 && netMat[i,] == 0){
#       numSpecies <- numSpecies - 1
#     }
#   }
#   return(numSpecies)
# }