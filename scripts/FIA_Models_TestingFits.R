#--------------------------------------------------------------------------------
# FIA Growth Modeling -- Data Testing Fits
#
# version 12/28/2025
#-------------------------------------------------------------------------------

# These are test growth model equation fits using FIA data for Douglas-fir from 
# Oregon. This is just a feasibility test to identify possible issues and the 
# fits produced have not been studied to see if they even make sense so should NOT
# BE USED!

#Three issues identified are:
# 1. what is the best way to deal with plots with multiple conditions?
# 2. how to characterize plot productivity (e.g., site index)? SI is available for
#    most plots as SICOND or SICOND_FVS. These are likely based on one site tree
#    per plot condition type (major species).
# 3. remeasurement period length (REMPER) are no necessarily 10-years or even
#    growth years as plots are measured through out the year.
# 4. the biometrics.utiltiy package is great and makes working up covariates
#    much easier -- however, there are a lot of species that would need to be
#    given parameters and/or mapped.

# Trees used are Douglas-fir (SPCD = 202) and site index is 202 with a 50-year
# base. This could represent different sources.

# All parameters for computing CCF, CCFL, CCH and others are set to Douglas-fir for
# all species.

# Most of the variables should be recognizable and most keep the FIA database 
# names. Note CFlag = 0 for all CONDITIONS (CONDCD) on a plot are forested and 
# = 1 means at least 1 condition on the plot is "nonforested". 

###
###  static models
###

TREEdata <- read.csv("C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/OR_TREEdata.CSV", header=TRUE, sep=",")

### fit HT-DBH

# get species
tempHD <- subset(TREEdata, SPCD == 202)
# get measured trees
tempHD <- subset(tempHD, HTCD == 1 & is.na(DIA) == FALSE & is.na(HT) == FALSE)

nrow(tempHD)
summary(tempHD[,c("DIA","HT")])
plot(tempHD$DIA, tempHD$HT, pch=c(19), 
     xlab="DBH (inches)", ylab="Total Height (feet)", main="HT-DBH Data Douglas-fir")
     
# unweighted fit
fit <- nls(HT ~ 4.5 + exp(AA + BB*DIA^CC), data=tempHD,
                   start=list(AA=7.12760, BB=-5.36420, CC=-0.261749), trace=TRUE)
summary(fit)

### fit HTLC

# get species
tempHC <- subset(TREEdata, SPCD == 202)
# get measured trees
tempHC <- subset(tempHC, HTCD == 1 & is.na(HTLC) == FALSE & is.na(HT) == FALSE & 
                         is.na(DIA) == FALSE & is.na(CCFL) == FALSE & 
                         is.na(BAPA) == FALSE) 

tempHC <- subset(tempHC, is.na(SICOND) == FALSE & is.na(SIBASE) == FALSE & 
                         is.na(SISP) == FALSE & is.na(STDAGE) == FALSE &
                         SIBASE == 50 & SISP == 202 & STDAGE > 20)

# get trees that have a FVS-site index with a base-age of 50
#tempHC <- subset(tempHC, is.na(SICOND_FVS) == FALSE & is.na(SIBASE_FVS) == FALSE & 
#                           SIBASE_FVS == 50)

nrow(tempHC)
summary(tempHC[,c("HTLC","DIA","HT","CCFL","BAPA","SICOND")])
#summary(tempHC[,c("HTLC","DIA","HT","CCFL","BAPA","SICOND_FVS")])
#table(tempHC$SIEQN_REF_CD_FVS)

# unweighted fit
fit <- nls(HTLC ~ HT*( 1.0/(1.0 + exp(B0 + B1*HT + B2*CCFL + B3*log(BAPA) + B4*DIA/HT + B5*SICOND_FVS))), 
                 data=tempHC,  start=list(B0=2.59959, B1=-0.00725950, B2=-0.00458228, 
                       B3=-0.441557, B4=1.61311, B5=0.00467539), trace=TRUE)
summary(fit)

###
### fit dynamic models
###

# These are fit to a 10-year growth period, although the data is avaiable to 
# fit annual models using start and end of period and the interative fit moethod.

# The 10 year period was computed as 10*(end - start)/REMPER which gets to a 
# 10-year period, but is not necessarily growing years. It would probably be 
# better to use the actual measurement dates (months/years) and compute fractional
# growth years.

CHANGEdata <- read.csv("C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/OR_CHANGEdata.CSV", header=TRUE, sep=",")

### fit diameter growth

# get species
dataDG <- subset(CHANGEdata,  startSPCD == 202)
# get live trees
tempDG <- subset(dataDG, is.na(endTreeStatus) == FALSE & endTreeStatus == 1)
# get measured trees
tempDG <- subset(tempDG, is.na(startDIA) == FALSE & is.na(endDIA) == FALSE &
                         is.na(startCR) == FALSE & 
                         is.na(startBAPA) == FALSE & is.na(startBAL) == FALSE)
# get trees that have a FVS-site index with a base-age of 50
#tempDG <- subset(tempDG, is.na(SICOND_FVS) == FALSE & is.na(SIBASE_FVS) == FALSE & 
#                           SIBASE_FVS == 50)
tempDG <- subset(tempDG, is.na(SICOND) == FALSE & is.na(SIBASE) == FALSE & 
                         is.na(SISP) == FALSE &  SIBASE == 50 & SISP == 202)

# convert to 10-year periodic growth
tempDG$DG <- ((tempDG$endDIA - tempDG$startDIA)/tempDG$REMPER)*10
tempDG$startCR <- tempDG$startCR/100

nrow(tempDG)
summary(tempDG[,c("DG","startDIA","startCR","startBAPA","startBAL","SICOND")])
#table(tempDG$SIEQN_REF_CD_FVS)

# unweighted fit
fit <- nls(DG ~ exp(B0 + B1*log(startDIA+5) + B2*startDIA^2 + B3*log((startCR+0.2)/1.2) + 
                         B4*log(SICOND-4.5) + B5*startBAL^2/(log(startDIA+2.7)) + B6*sqrt(startBAPA)),
                 data=tempDG,  start=list(B0=-3.332, B1=0.401, B2=-0.00044, 
                       B3=1.347, B4=0.778, B5=-0.00005, B6=-0.0512), trace=TRUE)

summary(fit)

### fit height growth

# get species
dataHG <- subset(CHANGEdata,  startSPCD == 202)
# get live trees
tempHG <- subset(dataHG, is.na(endTreeStatus) == FALSE & endTreeStatus == 1)
# get measured trees
tempHG <- subset(tempHG, startHTCD == 1 & endHTCD == 1 &
                         is.na(startHT) == FALSE & is.na(endHT) == FALSE 
                 & is.na(startCR) == FALSE & is.na(startBAPA) == FALSE &
                 is.na(startBAL) == FALSE)
tempHG <- subset(tempHG, is.na(SICOND) == FALSE & is.na(SIBASE) == FALSE & 
                         is.na(SISP) == FALSE & is.na(STDAGE) == FALSE &
                         SIBASE == 50 & SISP == 202 & STDAGE > 20)

# estimate BH age
tempHG$BHage <- ifelse(tempHG$SICOND > 135, tempHG$STDAGE-5, 
                  ifelse(tempHG$SICOND < 135 & tempHG$SICOND > 115, tempHG$STDAGE-6, 
                    ifelse(tempHG$SICOND < 115 & tempHG$SICOND > 95, tempHG$STDAGE-7, 
                    ifelse(tempHG$SICOND < 95 & tempHG$SICOND > 75, tempHG$STDAGE-8, tempHG$STDAGE-9))))

# estimate potential height growth

Bruce_potHTG <- function(SI, HT, period.length=5)
{
   X1 <- 13.25 - SI/20.0
   X2 <- 63.25 - SI/20.0
   B3 <- -0.447762 - 0.894427*SI/100.0 + 0.793548*(SI/100.0)^2 - 0.171666*(SI/100.0)^3
   B2 <- log(4.5/SI)/(X1^B3 - X2^B3)
   XX1 <- log(HT/SI)/B2 + X2^B3
   GEAGE <- XX1^(1.0/B3)-X1
   #GEAGE
   PHT <- SI*exp(B2*((GEAGE + period.length + X1)^B3 - X2^B3))
   PHTGRO <- PHT - HT
   return(PHTGRO)
}

tempHG$potHG <- NA
for (j in 1:nrow(tempHG))
{
    tempHG$potHG[j] <- Bruce_potHTG(SI=tempHG$SICOND[j], HT=tempHG$startHT40[j], period.length=10)
}

tempHG <- subset(tempHG, is.na(tempHG$potHG) == FALSE)
tempHG$HG <- ((tempHG$endHT - tempHG$startHT)/tempHG$REMPER)*10

nrow(tempHG)
summary(tempHG[,c("HG","potHG","startCR","startCCH","SICOND","STDAGE","BHage")])

# unweighted fit
fit <- nls(HG ~ potHG * C0 * (C1*exp(C2*startCCH) + 
                           (exp(C3*sqrt(startCCH)) - C1*exp(C2*startCCH))*
                           (-C5*exp(C7*sqrt(startCCH))*(1.0-startCR)^2)), data=tempHG,
                start=c(C0=1.064, C1=0.877, C2=-0.0365, C3=-0.051, C5=1.56, C7=0.14), trace=TRUE)           

summary(fit)

### fit mortality

# NOTE:  There are some cut trees in with the dead trees that need to be separated
#        out. Both have a STATUSCD of 2 and need to be separated out by the AGENTCD
#        code.

# get species
dataMORT <- subset(CHANGEdata,  startSPCD == 202)
# get live and dead trees
tempMORT <- subset(dataHG, is.na(startTreeStatus) == FALSE & startTreeStatus == 1 &
                           is.na(endTreeStatus) == FALSE)
# get measured trees
tempMORT <- subset(tempMORT, is.na(startDIA) == FALSE & is.na(startCR) == FALSE &
                             is.na(SICOND) == FALSE & is.na(startBAL) == FALSE & 
                             is.na(SIBASE) == FALSE & SIBASE == 50)
                          
nrow(tempMORT)
summary(tempMORT[,c("startDIA","startCR","SICOND","startBAL")])
table(tempMORT$endTreeStatus)

tempMORT$MORT <- tempMORT$endTreeStatus - 1  # 1 = live 2 = dead

# unweighted fit
# fit  logistic regression

fit <- glm(MORT ~ startDIA + startCR + SICOND + startBAL, data=tempMORT, binomial)

summary(fit)

# this is a bit of a problem since period lengths are not necessarily 10 years

fit <- nls(MORT ~ (1.0/exp(B0 + B1*startDIA + B2*startCR + B3*SICOND + B4*startBAL))^(-REMPER/10),
          data=tempMORT, start=c(B0=-1.00 ,B1=-0.043 , B2=-0.026 , B3=0.011 , B4=-0.000026 ), trace=TRUE)

summary(fit)

#-------------------------------------------------------------------------------
     