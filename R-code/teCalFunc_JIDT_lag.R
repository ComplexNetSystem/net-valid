# = Example 3 - Transfer entropy on continuous data using kernel estimators =

# Simple transfer entropy (TE) calculation on continuous-valued data using the (box) kernel-estimator TE calculator.

# Load the rJava library and start the JVM
# library("rJava")
# .jinit()

# Change location of jar to match yours:
#  IMPORTANT -- If using the default below, make sure you have set the working directory
#   in R (e.g. with setwd()) to the location of this file (i.e. demos/r) !!
# .jaddClassPath("infodynamics.jar")

teCalFunc_JIDT_lag <- function(src,dst,lag,width){
  
  # Create a TE calculator and run it:
  teCalc<-.jnew("infodynamics/measures/continuous/kernel/TransferEntropyCalculatorKernel")
  .jcall(teCalc,"V","setProperty", "NORMALISE", "true") # Normalise the individual variables
  .jcall(teCalc,"V","initialise", lag, width) # Use history length 1 (Schreiber k=1), kernel width of 0.5 normalised units
  .jcall(teCalc,"V","setObservations", src, dst)
  result <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
  return(result)
  
}

teOverThreshold <- function(te,thre){
  teThre <- matrix(nrow = length(thre),ncol = 1)
  for (jj in 1:length(thre)) {
    teThre[jj,] <- sum(te[te >= thre[jj]])
  }
  return(teThre)
}



