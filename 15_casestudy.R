require(MASS)
install.packages("ggplot")
library(ggplot2)
install.packages("fBasics")
library(fBasics)
install.packages("logspline")
library(logspline)
install.packages("fitdistrplus")
library(fitdistrplus)
install.packages("mc2d")
library(mc2d)

set.seed(1)
setwd("C:/Users/Karsyn/Documents/ManSystems2")
M1<- read.csv("M1data.csv", header=FALSE)
M1REAL <- na.omit(M1)
M1REAL$ID <- seq.int(nrow(M1REAL))
#change format of dates so we can subtract them 
start1 <-as.POSIXct(M1REAL$V1, format= "%m/%d/%Y %I:%M:%S %p")

finish1 <-as.POSIXct(M1REAL$V2, format= "%m/%d/%Y %I:%M:%S %p")
#find the processing times by subtracting
for (t in M1REAL$ID) {
  M1REAL$PROC1 <- difftime(finish1,start1,units="mins")
}
PROC1 <-as.numeric(M1REAL$PROC1)
#histogram of processing times
hist.default(PROC1)

descdist(PROC1, discrete = FALSE)
#gamma distribution of processing on machine 1
P1GAM<-fitdist(PROC1, "gamma", lower=c(0,0))
plot(P1GAM)
summary(P1GAM)

#normal distribution of processing on machine 1
P1NORM<-fitdist(PROC1, "norm")
plot(P1NORM)
summary(P1NORM)
#lognormal distribution of processing on machine 1
P1LNORM<-fitdist(PROC1, "lnorm")
plot(P1LNORM)
summary(P1LNORM)
#logisitc disttribution
P1LOGIS<-fitdist(PROC1, "logis")
plot(P1LOGIS)
summary(P1LOGIS)

#THIS IS THE BEST FIT FOR MACHINE 1 PROCESSING TIMES: WEIBULL
P1WEIB<-fitdist(PROC1, "weibull")
plot(P1WEIB, title="Weibull Distribution of Machine 1 Processing Times")
summary(P1WEIB)

# P1TRI<-fitdist(PROC1, "triang", method = "mge")
# plot(P1TRI)
# summary(P1TRI)

#uniform distribution of processing on machine 1
P1UNI<-fitdist(PROC1, "unif")
plot(P1UNI)
summary(P1UNI)
#exponential distribution of processing on machine 1
P1EXPO<-fitdist(PROC1, "exp")
plot(P1EXPO)
summary(P1EXPO)



#MACHINE 2
M2<- read.csv("M2data.csv", header=FALSE)
M2REAL <- na.omit(M2)
M2REAL$ID <- seq.int(nrow(M2REAL))
start2 <-as.POSIXct(M2REAL$V1, format= "%m/%d/%Y %I:%M:%S %p")
finish2 <-as.POSIXct(M2REAL$V2, format= "%m/%d/%Y %I:%M:%S %p")

for (t in M2REAL$ID) {
  M2REAL$PROC2 <- difftime(finish2,start2,units="mins")
}
PROC2 <- as.numeric(M2REAL$PROC2)
hist.default(PROC2)

descdist(PROC2, discrete = FALSE)

P2GAM<-fitdist(PROC2, "gamma", lower=c(0,0))
plot(P2GAM)
summary(P2GAM)

P2NORM<-fitdist(PROC2, "norm")
plot(P2NORM)
summary(P2NORM)
#LNORM IS BEST FIT DISTRIBUTION FOR MACHINE 2
P2LNORM<-fitdist(PROC2, "lnorm")
plot(P2LNORM)
summary(P2LNORM)
#logisitc disttribution
P2LOGIS<-fitdist(PROC2, "logis")
plot(P2LOGIS)
summary(P2LOGIS)
#CAUCHY distribution
P2CAUCHY<-fitdist(PROC2, "cauchy")
plot(P2CAUCHY)
summary(P2CAUCHY)
P2WEIB<-fitdist(PROC2, "weibull")
plot(P2WEIB)
summary(P2WEIB)

P2TRI<-fitdist(PROC2, "triang", gof="CvM")
plot(P2TRI)
summary(P2TRI)

P2UNI<-fitdist(PROC2, "unif")
plot(P2UNI)
summary(P2UNI)

P2EXPO<-fitdist(PROC2, "exp")
plot(P2EXPO)
summary(P2EXPO)




#MACHINE 3
M3<- read.csv("M3data.csv", header=FALSE)
M3REAL <- na.omit(M3)
M3REAL$ID <- seq.int(nrow(M3REAL))
start3 <-as.POSIXct(M3REAL$V1, format= "%m/%d/%Y %I:%M:%S %p")
finish3 <-as.POSIXct(M3REAL$V2, format= "%m/%d/%Y %I:%M:%S %p")

for (t in M3REAL$ID) {
  M3REAL$PROC3 <- difftime(finish3,start3,units="mins")
}
PROC3 <- as.numeric(M3REAL$PROC3)
hist.default(PROC3)

descdist(PROC3, discrete = TRUE)

P3GAM<-fitdist(PROC3, "gamma", lower=c(0,0))
plot(P3GAM)
summary(P3GAM)

P3NORM<-fitdist(PROC3, "norm")
plot(P3NORM)
summary(P3NORM)

P3LNORM<-fitdist(PROC3, "lnorm")
plot(P3LNORM)
summary(P3LNORM)

#logisitc disttribution
P3LOGIS<-fitdist(PROC3, "logis")
plot(P3LOGIS)
summary(P3LOGIS)

#CAUCHY distribution
P3CAUCHY<-fitdist(PROC3, "cauchy")
plot(P3CAUCHY)
summary(P3CAUCHY)

P3WEIB<-fitdist(PROC3, "weibull")
plot(P3WEIB)
summary(P3WEIB)

P3TRI<-fitdist(PROC3, "triang", gof="CvM")
plot(P3TRI)
summary(P3TRI)

P3UNI<-fitdist(PROC3, "unif")
plot(P3UNI)
summary(P3UNI)

P3EXPO<-fitdist(PROC3, "exp")
plot(P3EXPO)
summary(P3EXPO)

#MACHINE 4
M4<- read.csv("M4data.csv", header=FALSE)
M4REAL <- na.omit(M4)
M4REAL$ID <- seq.int(nrow(M4REAL))
start4 <-as.POSIXct(M4REAL$V1, format= "%m/%d/%Y %I:%M:%S %p")

finish4 <-as.POSIXct(M4REAL$V2, format= "%m/%d/%Y %I:%M:%S %p")

for (t in M4REAL$ID) {
  M4REAL$PROC4 <- difftime(finish4,start4,units="mins")
}
PROC4 <-as.numeric(M4REAL$PROC4)

hist.default(PROC4)

descdist(PROC4, discrete = FALSE)

P4GAM<-fitdist(PROC4, "gamma", lower=c(0,0))
plot(P4GAM)
summary(P4GAM)

P4NORM<-fitdist(PROC4, "norm")
plot(P4NORM)
summary(P4NORM)

P4LNORM<-fitdist(PROC4, "lnorm")
plot(P4LNORM)
summary(P4LNORM)

#logisitc disttribution
P4LOGIS<-fitdist(PROC4, "logis")
plot(P4LOGIS)
summary(P4LOGIS)
#CAUCHY distribution
P4CAUCHY<-fitdist(PROC4, "cauchy")
plot(P4CAUCHY)
summary(P4CAUCHY)

P4WEIB<-fitdist(PROC4, "weibull", method="mme")
plot(P4WEIB)
summary(P4WEIB)

P4TRI<-fitdist(PROC4, "triang", gof="CvM")
plot(P4TRI)
summary(P4TRI)

P4UNI<-fitdist(PROC4, "unif")
plot(P4UNI)
summary(P4UNI)
#best fit for machine 4
P4EXPO<-fitdist(PROC4, "exp")
plot(P4EXPO)
summary(P4EXPO)


#MACHINE 5
M5<- read.csv("M5data.csv", header=FALSE)
M5REAL <- na.omit(M5)
M5REAL$ID <- seq.int(nrow(M5REAL))
start5 <-as.POSIXct(M5REAL$V1, format= "%m/%d/%Y %I:%M:%S %p")

finish5 <-as.POSIXct(M5REAL$V2, format= "%m/%d/%Y %I:%M:%S %p")

for (t in M5REAL$ID) {
  M5REAL$PROC5 <- difftime(finish5,start5,units="mins")
}

PROC5 <-as.numeric(M5REAL$PROC5)
hist.default(PROC5)

descdist(PROC5, discrete = FALSE)

P5GAM<-fitdist(PROC5, "gamma", lower=c(0,0))
plot(P5GAM)
summary(P5GAM)

P5NORM<-fitdist(PROC5, "norm")
plot(P5NORM)
summary(P5NORM)

P5LNORM<-fitdist(PROC5, "lnorm")
plot(P5LNORM)
summary(P5LNORM)

#logisitc disttribution
P5LOGIS<-fitdist(PROC5, "logis")
plot(P5LOGIS)
summary(P5LOGIS)
#CAUCHY distribution
P5CAUCHY<-fitdist(PROC5, "cauchy")
plot(P5CAUCHY)
summary(P5CAUCHY)

P5WEIB<-fitdist(PROC5, "weibull")
plot(P5WEIB)
summary(P5WEIB)

P5TRI<-fitdist(PROC5, "triang", gof="CvM")
plot(P5TRI)
summary(P5TRI)

P5UNI<-fitdist(PROC5, "unif")
plot(P5UNI)
summary(P5UNI)

P5EXPO<-fitdist(PROC5, "exp")
plot(P5EXPO)
summary(P5EXPO)


#################################
#PART B
###############################
#machine 1 repair times
#identical(M1REAL$V3, "FALSE")
#create vector with 0. (for some reason it wont let me create an empty vector)
REP1<- c(0)
R<-0
fail1<-0
#for loop to find the repair times IF the machine was unsuccessful
for (k in M1REAL$ID){
if (M1REAL$V3[k]=="FALSE") {R <- difftime(start1[k+1],finish1[k],units="mins")
  REP1<-append(REP1, R)
  fail1<- fail1+1}
}
REP1<-as.numeric(REP1)
#BACKUP CODE#
# for (k in M1REAL$ID){
#   if (M1REAL$V3[k]=="FALSE") M1REAL$REP1[k]<-start1[k+1] - finish1[k]
#   else (M1REAL$REP1[k] == 0)
# }
#histogram of our repair times. Have to start at 2 because we had to add the 0 in the beginning?
hist.default(REP1[2:length(REP1)])
#gamma distribution of repair times of machine 1
R1GAM<-fitdist(REP1[2:length(REP1)], "gamma", lower=c(0,0))
plot(R1GAM)
summary(R1GAM)
#normal distribution of repair times of machine 1
R1NORM<-fitdist(REP1[2:length(REP1)], "norm")
plot(R1NORM)
summary(R1NORM)
#lognormal distribution of repair times of machine 1
R1LNORM<-fitdist(REP1[2:length(REP1)], "lnorm")
plot(R1LNORM)
summary(R1LNORM)
#weibull distribution of repair times of machine 1
R1WEIB<-fitdist(REP1[2:length(REP1)], "weibull")
plot(R1WEIB)
summary(R1WEIB)
#triangular distribution of repair times of machine 1
R1TRI<-fitdist(REP1[2:length(REP1)], "triang")
plot(R1TRI)
summary(R1TRI)
#uniform distribution of repair times of machine 1
R1UNI<-fitdist(REP1[2:length(REP1)], "unif")
plot(R1UNI)
summary(R1UNI)
#exponential distribution of repair times of machine 1
R1EXPO<-fitdist(REP1[2:length(REP1)], "exp")
plot(R1EXPO)
summary(R1EXPO)



#machine 2 repair times
REP2<- c(0)
R<-0
fail2 <-0
for (k in M2REAL$ID){
  if (M2REAL$V3[k]=="FALSE") {R <- difftime(start2[k+1],finish2[k],units="mins")
  REP2<-append(REP2, R)
  fail2<- fail2+1}
}
REP2<- as.numeric(REP2)
# for (k in M1REAL$ID){
#   if (M1REAL$V3[k]=="FALSE") M1REAL$REP1[k]<-start1[k+1] - finish1[k]
#   else (M1REAL$REP1[k] == 0)
# }
hist.default(REP2[2:length(REP2)])
R2GAM<-fitdist(REP2[2:length(REP2)], "gamma", lower=c(0,0))
plot(R2GAM)
summary(R2GAM)

R2NORM<-fitdist(REP2[2:length(REP2)], "norm")
plot(R2NORM)
summary(R2NORM)

R2LNORM<-fitdist(REP2[2:length(REP2)], "lnorm")
plot(R2LNORM)
summary(R2LNORM)

R2WEIB<-fitdist(REP2[2:length(REP2)], "weibull")
plot(R2WEIB)
summary(R2WEIB)

R2TRI<-fitdist(REP2[2:length(REP2)], "triang")
plot(R2TRI)
summary(R2TRI)

R2UNI<-fitdist(REP2[2:length(REP2)], "unif")
plot(R2UNI)
summary(R2UNI)

R2EXPO<-fitdist(REP2[2:length(REP2)], "exp")
plot(R2EXPO)
summary(R2EXPO)


#MACHINE 3 REPAIR TIMES
REP3<- c(0)
R<-0
fail3<-0
for (k in M3REAL$ID){
  if (M3REAL$V3[k]=="FALSE") {R <- difftime(start3[k+1],finish3[k],units="mins")
  REP3<-append(REP3, R)
  fail3<- fail3+1}
}
REP3<-as.numeric(REP3)
# for (k in M1REAL$ID){
#   if (M1REAL$V3[k]=="FALSE") M1REAL$REP1[k]<-start1[k+1] - finish1[k]
#   else (M1REAL$REP1[k] == 0)
# }
hist.default(REP3[2:length(REP3)])
R3GAM<-fitdist(REP3[2:length(REP3)], "gamma", lower=c(0,0))
plot(R3GAM)
summary(R3GAM)

R3NORM<-fitdist(REP3[2:length(REP3)], "norm")
plot(R3NORM)
summary(R3NORM)

R3LNORM<-fitdist(REP3[2:length(REP3)], "lnorm")
plot(R3LNORM)
summary(R3LNORM)

R3WEIB<-fitdist(REP3[2:length(REP3)], "weibull")
plot(R3WEIB)
summary(R3WEIB)

R3TRI<-fitdist(REP3[2:length(REP3)], "triang")
plot(R3TRI)
summary(R3TRI)

R3UNI<-fitdist(REP3[2:length(REP3)], "unif")
plot(R3UNI)
summary(R3UNI)

R3EXPO<-fitdist(REP3[2:length(REP3)], "exp")
plot(R3EXPO)
summary(R3EXPO)


#MACHINE 4 REPAIR TIMES
REP4<- c(0)
R<-0
fail4<-0
for (k in M4REAL$ID){
  if (M4REAL$V3[k]=="FALSE") {R <- difftime(start4[k+1],finish4[k],units="mins")
  REP4<-append(REP4, R)
  fail4<- fail4+1}
}
REP4<-as.numeric(REP4)
# for (k in M1REAL$ID){
#   if (M1REAL$V3[k]=="FALSE") M1REAL$REP1[k]<-start1[k+1] - finish1[k]
#   else (M1REAL$REP1[k] == 0)
# }
hist.default(REP4[2:length(REP4)])
print(REP4)
R4GAM<-fitdist(REP4[2:length(REP4)], "gamma", lower=c(0,0))
plot(R4GAM)
summary(R4GAM)

R4NORM<-fitdist(REP4[2:length(REP4)], "norm")
plot(R4NORM)
summary(R4NORM)

R4LNORM<-fitdist(REP4[2:length(REP4)], "lnorm")
plot(R4LNORM)
summary(R4LNORM)

R4WEIB<-fitdist(REP4[2:length(REP4)], "weibull")
plot(R4WEIB)
summary(R4WEIB)

R4TRI<-fitdist(REP4[2:length(REP4)], "triang")
plot(R4TRI)
summary(R4TRI)

R4UNI<-fitdist(REP4[2:length(REP4)], "unif")
plot(R4UNI)
summary(R4UNI)

R4EXPO<-fitdist(REP4[2:length(REP4)], "exp")
plot(R4EXPO)
summary(R4EXPO)
mean(PROC)

#MACHINE 5 REPAIR TIMES - PROBLEM HERE WITH REPAIR TIMES!!!!
REP5<- c(0)
R<-0
fail5<-0
for (k in M5REAL$ID){
  if (M5REAL$V3[k]=="FALSE") {R <- difftime(start5[k+1],finish5[k],units="mins")
  REP5<-append(REP5, R)
  fail5<- fail5+1
  }
}
REP5<-as.numeric(REP5)

# for (k in M1REAL$ID){
#   if (M1REAL$V3[k]=="FALSE") M1REAL$REP1[k]<-start1[k+1] - finish1[k]
#   else (M1REAL$REP1[k] == 0)
# }

hist.default(REP5[2:length(REP5)])
print(REP5)
R5GAM<-fitdist(REP5[2:length(REP5)], "gamma", lower=c(0,0))
plot(R5GAM)
summary(R5GAM)

R5NORM<-fitdist(REP5[2:length(REP5)], "norm")
plot(R5NORM)
summary(R5NORM)

R5LNORM<-fitdist(REP5[2:length(REP5)], "lnorm")
plot(R5LNORM)
summary(R5LNORM)

R5WEIB<-fitdist(REP5[2:length(REP5)], "weibull")
plot(R5WEIB)
summary(R5WEIB)

R5TRI<-fitdist(REP5[2:length(REP5)], "triang")
plot(R5TRI)
summary(R5TRI)

R5UNI<-fitdist(REP5[2:length(REP5)], "unif")
plot(R5UNI)
summary(R5UNI)

R5EXPO<-fitdist(REP5[2:length(REP5)], "exp")
plot(R5EXPO)
summary(R5EXPO)

print(REP5)


#################################
#PART C
################################
#prob of machine 1 failing 
# RT1 <- sum(PROC1)
# MTBF1 <- RT1/fail1
# alpha1 <- (1/MTBF1)
# sumrep1<-sum(REP1)
# MTTR1<- sumrep1/fail1
# b1<-(1/MTTR1)
# probrep1 <- (1/(1+(b1/alpha1)))
# suc1<- 1-probrep1
# print(probrep1)
downtime1<-fail1/length(M1REAL$ID)

# RT2 <- sum(PROC2)
# MTBF2 <- RT2/fail2
# alpha2 <- (1/MTBF2)
# sumrep2<-sum(REP2)
# MTTR2<- sumrep2/fail2
# b2<-(1/MTTR2)
# probrep2 <- (1/(1+(b2/alpha2)))
# print(probrep2)
# suc2<-1-probrep2
# prop2<- sumrep2/(sum(RT2+sumrep2))
# print(prop2)
downtime2<-fail2/length(M2REAL$ID)

# RT3 <- sum(PROC3)
# MTBF3 <- RT3/fail3
# alpha3 <- (1/MTBF3)
# sumrep3<-sum(REP3)
# MTTR3<- sumrep3/fail3
# b3<-(1/MTTR3)
# probrep3 <- (1/(1+(b3/alpha3)))
# suc3<-1-probrep3
downtime3<-fail3/length(M3REAL$ID)

# RT4 <- sum(PROC4)
# MTBF4 <- RT4/fail4
# alpha4 <- (1/MTBF4)
# sumrep4<-sum(REP4)
# MTTR4<- sumrep4/fail4
# b4<-(1/MTTR4)
# probrep4 <- (1/(1+(b4/alpha4)))
# suc4<-1-probrep4
downtime4<-fail4/length(M4REAL$ID)

# RT5 <- sum(PROC5)
# MTBF5 <- RT5/fail5
# alpha5 <- (1/MTBF5)
# sumrep5<-sum(REP5)
# MTTR5<- sumrep5/fail5
# b5<-(1/MTTR5)
# probrep5 <- (1/(1+(b5/alpha5)))
# suc5<- 1-probrep5
downtime5<-fail5/length(M5REAL$ID)



######################################
#TASK 2
#####################################
#part a
variances <- c()
means<-c()
VAR1<- var(PROC1)
MEAN1<-mean(PROC1)
variances<-append(variances, VAR1)
means<-append(means, MEAN1)
VAR2<- var(PROC2)
MEAN2<-mean(PROC2)
variances<-append(variances, VAR2)
means<-append(means, MEAN2)
VAR3<-var(PROC3)
MEAN3<-mean(PROC3)
variances<-append(variances, VAR3)
means<-append(means, MEAN3)
VAR4<- var(PROC4)
MEAN4<-mean(PROC4)
variances<-append(variances, VAR4)
means<-append(means, MEAN4)
VAR5<- var(PROC5)
MEAN5<-mean(PROC5)
variances<-append(variances, VAR5)
means<-append(means, MEAN5)
TASK2VAR<-mean(variances)
task2sd<- sqrt(TASK2VAR)
TASK2MEAN<-mean(means)
CV1<- (sqrt(TASK2VAR))/TASK2MEAN
print(CV1)
rel1<-0.63
noBuf1TP<- rel1*(3)
print(noBuf1TP)

#part b
buffersize<-10*CV1
buf6TP<- noBuf1TP+0.8*(3-noBuf1TP)

#part c
TPinv<- ((1/3)^(-1))
numerator<-(1.67*(5-1)*CV1)
denominator1<- 1+5+0.31*CV1+((1.67*5*buffersize)/(2*CV1))
denominator2<-1+5+0.31*CV1+((1.67*5*0)/(2*CV1))
mid1<- 1+(numerator/denominator1)
mid2<-1+(numerator/denominator2)
inve1<- mid1^-1
inve2<- mid2^-1
TPpartb<- inve1*TPinv
TPparta<- inve2*TPinv


#############################################
#TASK 3
#############################################
repavgVAR<- c()
repavgMEAN<-c()
RVAR1<- var(REP1[2:length(REP1)])
RMEAN1<-mean(REP1[2:length(REP1)])
repavgMEAN<- append(repavgMEAN,RMEAN1)
repavgVAR<-append(repavgVAR, RVAR1)
RVAR2<- var(REP2[2:length(REP2)])
RMEAN2<-mean(REP2[2:length(REP2)])
repavgMEAN<- append(repavgMEAN,RMEAN2)
repavgVAR<-append(repavgVAR, RVAR2)
RVAR3<- var(REP3[2:length(REP3)])
RMEAN3<-mean(REP3[2:length(REP3)])
repavgMEAN<- append(repavgMEAN,RMEAN3)
repavgVAR<-append(repavgVAR, RVAR3)
RVAR4<- var(REP4[2:length(REP4)])
RMEAN4<-mean(REP4[2:length(REP4)])
repavgMEAN<- append(repavgMEAN,RMEAN4)
repavgVAR<-append(repavgVAR, RVAR4)
RVAR5<- var(REP5[2:length(REP5)])
RMEAN5<-mean(REP5[2:length(REP5)])
repavgMEAN<- append(repavgMEAN,RMEAN5)
repavgVAR<-append(repavgVAR, RVAR5)
REPAIRvar<- mean(repavgVAR)
REPAIRmean<- mean(repavgMEAN)
CVR<-(sqrt(REPAIRvar))/REPAIRmean
proctime<- 60/TASK2MEAN

#alpha
realalpha<-(fail1+fail2+fail3+fail4+fail5)/(length(M1REAL$ID)+length(M2REAL$ID)+length(M3REAL$ID)+length(M4REAL$ID)+length(M5REAL$ID))

#b
b<-1/REPAIRmean
Einfinity<- 1/(1+(realalpha/b))
MAXtpwithFAIL<- proctime*Einfinity
MAXTPdaily<-MAXtpwithFAIL*24

#part b
E0<- 1/(1+((5*realalpha)/b))
TPunbuffered<- proctime*E0
TPunbufDAY<- TPunbuffered*24
#part c
Z2<- (4*(1+CVR^2))/b
TPZ2 <- 0.8*(MAXtpwithFAIL-TPunbuffered)+TPunbuffered
