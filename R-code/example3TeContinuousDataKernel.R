# Load the rJava library and start the JVM
library("rJava")
.jinit()

# Change location of jar to match yours:
#  IMPORTANT -- If using the default below, make sure you have set the working directory
#   in R (e.g. with setwd()) to the location of this file (i.e. demos/r) !!
.jaddClassPath("../../infodynamics.jar")

# Generate some random normalised data.
numObservations<-1000
covariance<-0.4
sourceArray<-rnorm(numObservations)
destArray = c(0, covariance*sourceArray[1:numObservations-1] + (1-covariance)*rnorm(numObservations-1, 0, 1))
sourceArray2<-rnorm(numObservations) # Uncorrelated source

# Create a TE calculator and run it:
teCalc<-.jnew("infodynamics/measures/continuous/kernel/TransferEntropyCalculatorKernel")
.jcall(teCalc,"V","setProperty", "NORMALISE", "true") # Normalise the individual variables
.jcall(teCalc,"V","initialise", 1L, 0.5) # Use history length 1 (Schreiber k=1), kernel width of 0.5 normalised units
.jcall(teCalc,"V","setObservations", sourceArray, destArray)
# For copied source, should give something close to expected value for correlated Gaussians:
result <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
cat("TE result ",  result, "bits; expected to be close to ", log(1/(1-covariance^2))/log(2), " bits for these correlated Gaussians but biased upwards\n")

.jcall(teCalc,"V","initialise") # Initialise leaving the parameters the same
.jcall(teCalc,"V","setObservations", sourceArray2, destArray)
# For random source, it should give something close to 0 bits
result2 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
cat("TE result ",  result2, "bits; expected to be close to 0 bits for uncorrelated Gaussians but will be biased upwards\n")

# We can get insight into the bias by examining the null distribution:
nullDist <- .jcall(teCalc,"Linfodynamics/utils/EmpiricalMeasurementDistribution;",
                   "computeSignificance", 100L)
cat("Null distribution for unrelated source and destination",
    "(i.e. the bias) has mean", .jcall(nullDist, "D", "getMeanOfDistribution"),
    "bits and standard deviation", .jcall(nullDist, "D", "getStdOfDistribution"), "\n")