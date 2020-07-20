mat2vector <- function(matData){
  # remove the diagonal first.
  # convert the matrix to a vector.
  vectorLen <- nrow(matData) * (ncol(matData)-1)
  vectorData <- vector(mode = 'numeric',length = vectorLen)
  kk <- 1
  for (i in 1:nrow(matData)) {
    for (j in 1:ncol(matData)) {
      if (i == j)
        next
      else
        vectorData[kk] <- matData[i,j]
      kk <- kk + 1
    }
  }
  return(vectorData)
}