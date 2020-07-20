library("rJava")
.jinit()

# Change location of jar to match yours:
#  IMPORTANT -- If using the default below, make sure you have set the working directory
#   in R (e.g. with setwd()) to the location of this file (i.e. demos/r) !!
.jaddClassPath("D:/rProject/infodynamics-dist-1.5/infodynamics.jar")

teCal_jidt_ksg_func <- function(srcArr,dstArr,k,histLen){
  # k: "4" as an example; histLen: 1L as an example
  # Create a TE calculator:
  teCalc<-.jnew("infodynamics/measures/continuous/kraskov/TransferEntropyCalculatorKraskov")
  .jcall(teCalc,"V","setProperty", "k", k) # Use Kraskov parameter K=4 for 4 nearest points
  
  # Perform calculation with correlated source:
  .jcall(teCalc,"V","initialise", histLen) # Use history length 1 (Schreiber k=1)
  .jcall(teCalc,"V","setObservations", srcArr, dstArr)
  result <- .jcall(teCalc,"D","computeAverageLocalOfObservations") # nat
  result_bit <- log2(exp(1))*result    # nat to bit
  return(result_bit)
}

teCal_jidt_knl_func <- function(srcArr,dstArr,histLen,width){
  # histLen: 1L as an example; width: 0.5 as an example
  # Create a TE calculator and run it:
  teCalc<-.jnew("infodynamics/measures/continuous/kernel/TransferEntropyCalculatorKernel")
  .jcall(teCalc,"V","setProperty", "NORMALISE", "true") # Normalise the individual variables
  .jcall(teCalc,"V","initialise", histLen, width) # Use history length 1 (Schreiber k=1), kernel width of 0.5 normalised units
  .jcall(teCalc,"V","setObservations", srcArr, dstArr)
  # For copied source, should give something close to expected value for correlated Gaussians:
  result <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
  return(result)
}
