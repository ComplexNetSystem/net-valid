library(readxl)
library(poweRlaw)

data_health_A <- read_excel("Healthy_A_IBS.xlsx")
data_health_B <- read_excel("Healthy_B_IBS.xlsx")
data_health <- c(as.vector(as.matrix(data_health_A[,-1])),
                 as.vector(as.matrix(data_health_B[,-1])))
data_health = data_health[-which(data_health == 0)]

data_transitory_C <- read_excel("Transitory_C_IBS.xlsx")
data_transitory_C1 <- read_excel("Transitory_C1_IBS.xlsx")
data_transitory <- c(as.vector(as.matrix(data_transitory_C[,-1])),
                     as.vector(as.matrix(data_transitory_C1[,-1])))
data_transitory = data_transitory[-which(data_transitory == 0)]

data_patient_1 <- read_excel("Patient_1_IBS.xlsx")
data_patient_2 <- read_excel("Patient_2_IBS.xlsx")
data_patient <- c(as.vector(as.matrix(data_patient_1[,-1])),
                  as.vector(as.matrix(data_patient_2[,-1])))
data_patient = data_patient[-which(data_patient == 0)]

fit_health <- conpl$new(data_health)
xMin_health <- estimate_xmin(fit_health)
fit_health$setXmin(xMin_health)
plot(fit_health)
lines(fit_health, col="red", lwd=2)

fit_transitory <- conpl$new(data_transitory)
xMin_transitory <- estimate_xmin(fit_transitory)
fit_transitory$setXmin(xMin_transitory)
plot(fit_transitory)
lines(fit_transitory, col="red", lwd=2)

fit_patient <- conpl$new(data_patient)
xMin_patient <- estimate_xmin(fit_patient)
fit_patient$setXmin(xMin_patient)
plot(fit_patient)
lines(fit_patient, col="red", lwd=2)


