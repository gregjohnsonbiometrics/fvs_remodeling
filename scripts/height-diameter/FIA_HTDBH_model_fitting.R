#--------------------------------------------------------------------------------
# FIA HT-DBH Model Fitting
# version: 07/06/2026
#-------------------------------------------------------------------------------

# Stuff To Do
# 1. [DONE] test B1, B2 and B3 for reasonableness
# 2. [BETTER] integrate B3 test somehow?
# 3. [DONE] add AIC
# 4. [DONE] add weighting to fits and summary output
# 5. [DONE[ add mean residual difference?
# 6. [DONE] add ierr = 4 for fits with nonsignficant parameters
# 7. [DONE] mean residual by thirds of the DBH distribution
# 8. [DONE] add coefficient histograms by model
# 9. [DONE] add function for too few observations (easier to change in 1 place)
# 10. [DONE] output minimum and maximum HT and DBH for each species
# 11. need to implement some filtering really for bad measurements

# Code sections 

# 1. load libraries and functions used
# 2. Read Data
# 3. FIA HT-DBH Model Fitting
# 4. fit summaries (testing)
# 5. FIA HT-DBH data checking for a species (working)
# 6. working area for development and testing new code

#--------------------------------------------------------------------------------
# load libraries and functions used
#-------------------------------------------------------------------------------

# load R libraries

library(dplyr)

# utility function to read the processed data files: PLOTdata, TREEdata and CHANGEdata

ReadFIATreeModelingTREEData <- function(STATENM, xdate, data_path)
{
   # read TREE data
   con <- unz( paste0(data_path, STATENM, "_FIAdata_",xdate,".zip"), paste0(STATENM,"_TREEdata.CSV"))
   TREEdata <- read.table(con, header=T, sep=",", stringsAsFactors=FALSE)
   return(TREEdata)
}

###
### FIA HT-DBH Model Fitting Functions
###

# Functions for fitting nonlinear HT-DBH Curves

#    model 1: HT = bh + exp(B1 + B2*DBH^(-1.0))
#    model 2: HT = bh + exp(B1 + B2*DBH^B3)
#    model 3: HT = bh + B1*(1.0-exp(B2*DBH))^B3    # Chapman-Richards
#    model 4: HT = bh + exp(B1+B2/(DBH+1.0))
#    model 5: HT = bh + exp(B1+B2/(DBH+B3))
#    model 6: HT = bh + B1*(1.0-exp(B2*DBH^B3))    # Weibull

#      weights=1/tempHD$DIA,
#      residuals(fitnls1w, type = "pearson")  # scaled by weights

#w <- data[[weight_var]]
#model <- lm(formula, data = data, weights = w)

# ierr values
#  0  fit converged
#  1  too few observations (< minOBS)
#  2  did not converge or other issues (e.g. singularity)
#  3  unreasonable (wrong sign) parameters
#  4  some parameters are not significant(Ho: b=0, alpha 0.05)

### function to plot data

plot_HD_data <- function(tempHD, species=NA)
{
   if (is.na(species) == FALSE)
   {
      tempHD <- subset(tempHD, SPCD == species)
   }

   plot(tempHD$DIA, tempHD$HT, pch=c(19), 
            xlab="DBH (inches)", ylab="Total Height (feet)",
            main=paste0("Oregon FIA HT-DBH data for ", species))
}

#subset(outFits, is.na(B2) == FALSE & B2 > 0)
#tempHD <- subset(dataHD, SPCD == 912)
#nrow(tempHD)

#plot_HD_data(dataHD, species=912)
#x <- seq(0,50,1)
#y <- 4.5 + exp(3.001654 + 0.1814723/(x+1))
#lines(x, y)


### function to recover fitted parameters and other information for reporting

# called from HDfit_summary and HT_DBH_Model_x functions

get_nls_fits <- function(fit, npar)
{
   # get parameter SE and Pvalues
   SEs <- coef(summary(fit))[, "Std. Error"]
   PVs <- coef(summary(fit))[, "Pr(>|t|)"]
   # get parameters
   B1 <- as.numeric(coef(fit)[[1]])
   seB1 <- SEs[1]
   pB1 <- PVs[1]
   B2 <- as.numeric(coef(fit)[[2]])
   seB2 <- SEs[2]
   pB2 <- PVs[2]
   if (npar == 2)
   {
      B3 = NA
      seB3 <- NA
      pB3 <- NA
   } else {
     B3 <- as.numeric(coef(fit)[[3]])
     seB3 <- SEs[3]
     pB3 <- PVs[3]
   }
   AICx=AIC(fit)
   nobs <- length(residuals(fit))
   meanBIAS <- sum((-1.0)*residuals(fit))/nobs   # pred - act
   meanAbsBIAS <- sum(abs((-1.0)*residuals(fit)))/nobs   # pred - act

   params <- data.frame(B1=B1, B2=B2, B3=B3, seB1=seB1, seB2=seB2, seB3=seB3, 
                        pB1=pB1, pB2=pB2, pB3=pB3, RMSE=summary(fit)$sigma, 
                        AIC=AICx, meanBIAS=meanBIAS, meanAbsBIAS=meanAbsBIAS)
   return(params)
}

### function to compute residuals for thirds of DBH range

# note residuals are predicted - actual
# outNL1 = outNL[[1]]

# possible problem with cut2 is if DBH distribution is very skewed 33% = 66% will
# only give 2 groups and not 3 (same for quantile function)
#   quantile(tempHD$DIA, c(0, 0.33, 0.66, 0.99))
# therefore using dplyr function ntile

DBHclass_Residuals <- function (tempHD, outNL1)
{
#   library(Hmisc)
#   trunc(nrow(tempR)/3)
#   # Split into 3 quantile groups
   tempR <- data.frame(DIA=tempHD$DIA, RES=(-1.0)*residuals(outNL1))
#   tempR$group <- cut2(tempR$DIA, g = 3)
#   #tempR$group <- cut2(tempR$DIA, g = 3, m=10)
#   #tempR$group <- cut(tempR$DIA, g = 3)
#   tempR$group <- as.numeric(tempR$group)
#   table(tempR$group)

#   tempR <- data.frame(DIA=tempHD$DIA, RES=(-1.0)*residuals(outNL1))
#   lower_threshold_DBH <- as.numeric(quantile(tempR$DIA, 0.33))
#   upper_threshold_DBH <- as.numeric(quantile(tempR$DIA, 0.66))

   #min(tempR$DIA)
   #max(tempR$DIA)
   #hist(tempR$DIA)
   #lower_threshold_DBH
   #33% 
   #4.5 
   #upper_threshold_DBH
   #66% 
   #7.3 
   #max(tempR$DIA)

#   lower_RESID <- mean(tempR$RES[tempR$DIA <= lower_threshold_DBH])
#   mid_RESID <- mean(tempR$RES[tempR$DIA > lower_threshold_DBH & tempR$DIA < upper_threshold_DBH])
#   upper_RESID <- mean(tempR$RES[tempR$DIA >= upper_threshold_DBH])

    # ntile() ignores ties and it will create evenly sized buckets even if the same 
    # value of x ends up in different buckets.

   df <- tempR %>% mutate(group = ntile(DIA, 3))
   lower_RESID <- mean(df$RES[df$group == 1], na.rm=TRUE)
   mid_RESID <- mean(df$RES[df$group == 2], na.rm=TRUE)
   upper_RESID <- mean(df$RES[df$group == 3], na.rm=TRUE)

   return(list(lower_RESID, mid_RESID, upper_RESID))
}

### function to summarize fits

HDfit_summary <- function(model, model.formula, npar, SPCD, eq, wt_var, ierr)
{
   # see if fit converged or had errors
   if(is.null(eq) == TRUE | typeof(eq)=="character")
   {
      # failed to converge or had errors or converged but parameters were not correct sign
      if (is.na(ierr) == FALSE & ierr == 0)
      {
         ierr <- 2     # did not converge or other fit issues
      } else {
         ierr <- 3     # converged but parameters are unreasonable
      }
      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=SPCD, nobs=nrow(tempHD),
                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
                        meanBIAS=NA, meanAbsBIAS=NA, 
                        meanLowDBHRes=NA, meanMidDBHRes=NA, meanUppDBHRes=NA,
                        B3_test=NA, ierr=ierr, notes=eq)
   } else {
      # fit was successful
      fit <- eq
      params <- get_nls_fits(fit, npar)

      B1 <- params$B1
      B2 <- params$B2
      B3 <- params$B3

      # need to test B3 is different from 1
      #test_parameter(fit, param="B3", test_value=1.0)
      # test of B1, B2 or B3 is not significant                              #<---

      seB1 <- params$seB1
      seB2 <- params$seB2
      seB3 <- params$seB3
      pB1 <- params$pB1
      pB2 <- params$pB2
      pB3 <- params$pB3

      AICeq <- params$AIC
      meanBIAS <- params$meanBIAS
      meanAbsBIAS <- params$meanAbsBIAS

      outR <- DBHclass_Residuals(tempHD, outNL1=eq)
      meanLowDBHRes <- outR[[1]]
      meanMidDBHRes <- outR[[2]]
      meanUppDBHRes <- outR[[3]]

      # flag if any parameters are not significant
      ierr <- 0
      fit_text <- "fit"
      sig_test <- CheckParameterSig(npar, pB1, pB2, pB3)
      if (is.na(sig_test) == FALSE) 
      { 
         ierr <- 4
         fit_text <- sig_test 
      }
 
      out <- data.frame(model=model, model.formula, bh, WT=wt_var, SPCD=species_list[i], nobs=nrow(tempHD),
                        B1, B2, B3, seB1, seB2, seB3, pB1, pB2, pB3, RMSE=summary(fit)$sigma, AIC=AICeq, 
                        meanBIAS, meanAbsBIAS, meanLowDBHRes, meanMidDBHRes, meanUppDBHRes,
                        B3_test=NA, ierr=ierr, notes=fit_text)
   }
   return(out)
}

#      outNL <- HT_DBH_Model_1(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts, wt_var=wt_var)


### function to test if parameter is different from a given value

test_parameter <- function(fit, param, test_value=1.0)
{
   param <- "B3"
   #test_value <- 1.0
   # Extract parameter estimate and SE for b
   #coef_b <- as.numeric(coef(fit)[[3]])
   coef_b <- as.numeric(coef(fit)[[param]])
   #se_b <- summary(fit)$parameters["B3","Std. Error"]
   se_b <- summary(fit)$parameters[param,"Std. Error"]
   # Wald t-test for H0: b = test_value
   t_stat <- (coef_b - test_value) / se_b
   df <- length(residuals(fit)) - length(coef(fit))  # residual df
   p_val <- 2 * pt(-abs(t_stat), df)
   # output results
   #cat("Estimate:", coef_b, "\n")
   #cat("t =", t_stat, ", df =", df, ", p =", p_val, "\n")
   return(p_val)
}

### function to flag nonsignificant parameters

CheckParameterSig <- function(npar, pB1, pB2, pB3)
{
   test <- FALSE
   if (npar == 3)
   {
      if (pB1 > 0.05 | pB2 > 0.05 | pB3 > 0.05) { Test <- TRUE }
   } else {
      if (pB1 > 0.05 | pB2 > 0.05) { test <- TRUE }
   }

   if (test == TRUE)
   {
      ierr <- 4
      err_text <- "Parameters Not Significant"
      if (pB1 > 0.05) { err_text <- paste0(err_text, " B1") }
      if (pB2 > 0.05) { err_text <- paste0(err_text, " B2") }
      if (npar > 2 & pB3 > 0.05) { err_text <- paste0(err_text, " B3") }
      return(err_text)
   }
   return(NA)
}
      
#CheckParameterSig(npar=3, pB1=0.02, pB2=0.02, pB3=0.02)
#CheckParameterSig(npar=2, pB1=0.02, pB2=0.60, pB3=NA)

### function to fill in output for no fit (too few observations)

NoOutput <- function(model, model.formula, bh, SPCD, nobs, ierr, notes)
{
      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
                        meanBIAS=NA, meanAbsBIAS=NA, 
                        meanLowDBHRes=NA, meanMidDBHRes=NA, meanUppDBHRes=NA,
                        B3_test=NA, ierr=1, notes="too few observations")
      return(out)
}

###
### model 1: HT = bh + exp(B1 + B2*DBH^(-1.0))
###

# Schumacher 1939; Curtis 1967

# Checks: B1 > 0; B2 < 0; B3 < 0

HT_DBH_Model_1 <- function(tempHD, SPCD, minOBS=0, useNLS=TRUE, wts=NA, wt_var=NA)
{
   model <- 1
   model.formula <- "HT = bh + exp(B1 + B2*DBH^(-1.0))"

   # test for minimum nobs
   if (nrow(tempHD) < minOBS)
   {
      ierr <- 1
      notes="too few observations"      
      out <- NoOutput(model, model.formula, bh, SPCD, nobs=nrow(tempHD), ierr, notes)
#      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
#                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
#                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
#                        meanBIAS=NA, meanAbsBIAS=NA, B3_test=NA, ierr=1, notes="too few observations")
      return(list(eq=NULL, out))
   }

   # weights
   if (any(is.na(wts) == TRUE) | length(wts) != nrow(tempHD))
   {
      wts <- rep(1,nrow(tempHD))
   }

   # get starting values
   
   Y <- log(tempHD$HT-bh)
   X <- tempHD$DIA^(-1.0)
   fit1 <- lm(Y ~ X)
   b0 <- as.numeric(coef(fit1)[1])
   b1 <- as.numeric(coef(fit1)[2])
   #m <- summary(fit1)
   #RSE=m$sigma
   #test <- data.frame(model.formula, bh, j=j, b0=b0, b1=b1, b2=CC[j], RSE=m$sigma)
   #testC <- rbind(testC, test) 
   starting <- c(B1=b0, B2=b1)
 
   #fit <- nls(HT ~ 4.5 + exp(B1+B2/(DIA+B3)), data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150))
   #starting <- c(B1=b0, B2=b1)
   #fit <- nls(HT ~ 4.5 + exp(B1+B2/(DIA+1.0)), data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150))

   if (useNLS == TRUE)
   {
      # use nls
      eq <- tryCatch({nls(HT ~ 4.5 + exp(B1 + B2*DIA^(-1.0)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.control(maxiter=150))}, 
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   } else {
      # use nlsML
      eq <- tryCatch({nlsLM(HT ~ bh ~ exp(B1 + B2*DBH^(-1.0)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.lm.control(maxiter=150))}, 
                      #error = function(e) {return(NULL)})
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   }

   # check that parameters are reasonable  if converged without errors
   ierr <- 0
   if(is.null(eq) == FALSE & typeof(eq) != "character")
   {
      B1 <- as.numeric(coef(eq)[1])
      B2 <- as.numeric(coef(eq)[2])
      B3 <- NA
      # Checks: B1 > 0; B2 < 0;
      if (B1 <= 0 | B2 >= 0) 
      { 
         ierr <- 3
         err_text <- "Parameters Unrealistic"
         if (B1 <= 0) { err_text <- paste0(err_text, " B1") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B2") }
         eq <- err_text
      }
   }
   
   # summarize fit
   out <- HDfit_summary(model, model.formula, npar=2, SPCD, eq, wt_var, ierr)
   return(list(eq, out))
}

###
### model 2: HT = bh + exp(B1 + B2*DBH^B3)
###

# Schumacher 1939; Curtis 1967

# Checks: B1 > 0; B2 < 0; B3 < 0

HT_DBH_Model_2 <- function(tempHD, SPCD, minOBS=0, useNLS=TRUE, wts=NA, wt_var=NA)
{
   model <- 2
   model.formula <- "HT = bh + exp(B1 + B2*DBH^B3)"

   # test for minimum nobs
   if (nrow(tempHD) < minOBS)
   {
      ierr <- 1
      notes="too few observations"      
      out <- NoOutput(model, model.formula, bh, SPCD, nobs=nrow(tempHD), ierr, notes)
#      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
#                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
#                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
#                        meanBIAS=NA, meanAbsBIAS=NA, B3_test=NA, ierr=1, notes="too few observations")
      return(list(eq=NULL, out))
   }
   
   # weights
   if (any(is.na(wts) == TRUE) | length(wts) != nrow(tempHD))
   {
      wts <- rep(1,nrow(tempHD))
   }

   # get starting values by linearlizing function with values of B2 (CC)
   # and use best fit based on lowest RSE
   CC <- c(-0.25, -0.5, -0.75, -1.00, -1.25, -1.50, -1.75)#, -2.00, -2.25, -2.50, -2.75, -3.00)
   testC <- NULL
   for (j in 1:length(CC))
   {
      Y <- log(tempHD$HT-bh)
      X <- tempHD$DIA^CC[j]
      fit1 <- lm(Y ~ X)
      b0 <- as.numeric(coef(fit1)[1])
      b1 <- as.numeric(coef(fit1)[2])
      m <- summary(fit1)
      RSE=m$sigma
      test <- data.frame(model.formula, bh, j=j, b0=b0, b1=b1, b2=CC[j], RSE=m$sigma)
      testC <- rbind(testC, test) 
   }
   # output options
   jbest <- which.min(testC$RSE)

   # fit nonlinear function
   starting <- c(B1=testC$b0[jbest], B2=testC$b1[jbest], B3=testC$b2[jbest])

   #nls(HT ~ 4.5 + exp(B1 + B2*DIA^B3), data=tempHD, start=starting,
   #                trace=FALSE, control=nls.control(maxiter=150)) 
   
   if (useNLS == TRUE)
   {
      # use nls
      eq <- tryCatch({nls(HT ~ 4.5 + exp(B1 + B2*DIA^B3), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.control(maxiter=150))}, 
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   } else {
      # use nlsML
      eq <- tryCatch({nlsLM(HT ~ bh ~ exp(B1 + B2*DBH^B3), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.lm.control(maxiter=150))}, 
                      #error = function(e) {return(NULL)})
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   }
                    
   # check that parameters are reasonable if converged without errors
   ierr <- 0
   if(is.null(eq) == FALSE & typeof(eq) != "character")
   {
      B1 <- as.numeric(coef(eq)[1])
      B2 <- as.numeric(coef(eq)[2])
      B3 <- as.numeric(coef(eq)[3])
      # Checks: B1 > 0; B2 < 0; B3 < 0
      if (B1 <= 0 | B2 >= 0 | B3 >= 0)
      { 
         ierr <- 3
         err_text <- "Parameters Unrealistic"
         if (B1 <= 0) { err_text <- paste0(err_text, " B1") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B2") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B3") }
         eq <- err_text
      }
   }
   
   # summarize fit
   out <- HDfit_summary(model, model.formula, npar=3, SPCD, eq, wt_var, ierr)
   return(list(eq, out))
}

###
### model 3: HT = bh + B1*(1.0-exp(B2*DBH))^B3
###

# Chapman-Richards function

# Checks: B1 > 0; B2 < 0; B3 < 0

HT_DBH_Model_3 <- function(tempHD, SPCD, minOBS=0, useNLS=TRUE, wts=NA, wt_var=NA)
{
   model <- 3 
   model.formula <- "HT = bh + B1*(1.0-exp(B2*DBH))^B3"

   # test for minimum nobs
   if (nrow(tempHD) < minOBS)
   {
      ierr <- 1
      notes="too few observations"      
      out <- NoOutput(model, model.formula, bh, SPCD, nobs=nrow(tempHD), ierr, notes)
#      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
#                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
#                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
#                        meanBIAS=NA, meanAbsBIAS=NA, B3_test=NA, ierr=1, notes="too few observations")
      return(list(eq=NULL, out))
   }

   # weights
   if (any(is.na(wts) == TRUE) | length(wts) != nrow(tempHD))
   {
      wts <- rep(1,nrow(tempHD))
   }

   # get starting values

   # get 95th percentile DBH threshold
   threshold_DBH <- quantile(tempHD$DIA, 0.95)
   # get mean height with DBH >= 95th percentile
   mean_HT <- mean(tempHD$HT[tempHD$DIA >= threshold_DBH])
   starting <- c(B1=mean_HT, B2=-0.01, B3=1.0)

   # set B3 = 1 (problem if mean_HT > HT-4.5 takes log of negative value and gives NA
   #Y <- log(1.0 - ((tempHD$HT - bh)/mean_HT))
   #X <- tempHD$DIA
   #fit1 <- lm(Y ~ X - 1)
   #b2 <- as.numeric(coef(fit1)[1])
   #starting <- c(B1=mean_HT, B2=b2, B3=1.0)
   #starting <- c(B1=22.0, B2=-0.27, B3=1.0)

   #nls(HT - bh ~ B1*(1.0-exp(B2*DIA))^B3, data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150)) 

   if (useNLS == TRUE)
   {
      # use nls
      eq <- tryCatch({nls(HT ~ 4.5 + B1*(1.0-exp(B2*DIA))^B3, data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.control(maxiter=150))}, 
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   } else {
      # use nlsML
      eq <- tryCatch({nlsLM(HT ~ bh ~ B1*(1.0-exp(B2*DIA))^B3, data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.lm.control(maxiter=150))}, 
                      #error = function(e) {return(NULL)})
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   }
   
   # check that parameters are reasonable  if converged without errors
   ierr <- 0
   if(is.null(eq) == FALSE & typeof(eq) != "character")
   {
      B1 <- as.numeric(coef(eq)[1])
      B2 <- as.numeric(coef(eq)[2])
      B3 <- as.numeric(coef(eq)[3])
      # Checks: B1 > 0; B2 < 0; B3 > 0
      if (B1 <= 0 | B2 >= 0 | B3 <= 0)
      { 
         ierr <- 3
         err_text <- "Parameters Unrealistic"
         if (B1 <= 0) { err_text <- paste0(err_text, " B1") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B2") }
         if (B2 <= 0) { err_text <- paste0(err_text, " B3") }
         eq <- err_text
      }
   }
   
   # summarize fit
   out <- HDfit_summary(model, model.formula, npar=3, SPCD, eq, wt_var, ierr)
   return(list(eq,out))
}

###
### model 4: HT = bh + exp(B1+B2/(DBH+1.0))
###

# Wykoff FVS model

# checks: B1 > 0; B2 < 0

HT_DBH_Model_4 <- function(tempHD, SPCD, minOBS=0, useNLS=TRUE, wts=NA, wt_var=NA)
{
   model <- 4
   model.formula <- "HT = bh + exp(B1+B2/(DBH+1.0))"

   # test for minimum nobs
   if (nrow(tempHD) < minOBS)
   {
      ierr <- 1
      notes="too few observations"      
      out <- NoOutput(model, model.formula, bh, SPCD, nobs=nrow(tempHD), ierr, notes)
#      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
#                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
#                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
#                        meanBIAS=NA, meanAbsBIAS=NA, B3_test=NA, ierr=1, notes="too few observations")
      return(list(eq=NULL, out))
   }

   # weights
   if (any(is.na(wts) == TRUE) | length(wts) != nrow(tempHD))
   {
      wts <- rep(1,nrow(tempHD))
   }

   # get starting values
   
   Y <- log(tempHD$HT-bh)
   X <- (tempHD$DIA+1)^(-1)
   fit1 <- lm(Y ~ X)
   b0 <- as.numeric(coef(fit1)[1])
   b1 <- as.numeric(coef(fit1)[2])
   #m <- summary(fit1)
   #RSE=m$sigma
   #test <- data.frame(model.formula, bh, j=j, b0=b0, b1=b1, b2=CC[j], RSE=m$sigma)
   #testC <- rbind(testC, test) 
   starting <- c(B1=b0, B2=b1)
 
   #fit <- nls(HT ~ 4.5 + exp(B1+B2/(DIA+B3)), data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150))
   #starting <- c(B1=b0, B2=b1)
   #fit <- nls(HT ~ 4.5 + exp(B1+B2/(DIA+1.0)), data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150))

   if (useNLS == TRUE)
   {
      # use nls
      eq <- tryCatch({nls(HT ~ 4.5 + exp(B1+B2/(DIA+1.0)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.control(maxiter=150))}, 
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   } else {
      # use nlsML
      library(minpack.lm)
      eq <- tryCatch({nlsLM(HT ~ 4.5 + exp(B1+B2/(DIA+1.0)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.lm.control(maxiter=150))}, 
                      #error = function(e) {return(NULL)})
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   }
   
   # check that parameters are reasonable  if converged without errors
   ierr <- 0
   if(is.null(eq) == FALSE & typeof(eq) != "character")
   {
      B1 <- as.numeric(coef(eq)[1])
      B2 <- as.numeric(coef(eq)[2])
      B3 <- NA
      # checks: B1 > 0; B2 < 0
      if (B1 <= 0 | B2 >= 0)
      { 
         ierr <- 3
         err_text <- "Parameters Unrealistic"
         if (B1 <= 0) { err_text <- paste0(err_text, " B1") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B2") }
         eq <- err_text
      }
   }
   
   # summarize fit
   out <- HDfit_summary(model, model.formula, npar=2, SPCD, eq, wt_var, ierr)
   return(list(eq, out))
}

###
### model 5: HT = bh + exp(B1+B2/(DBH+B3))
###

# Wykoff FVS model

# checks: B1 > 0; B2 < 0; B3 > 0

HT_DBH_Model_5 <- function(tempHD, SPCD, minOBS=0, useNLS=TRUE, wts=NA, wt_var=NA)
{
   model <- 5
   model.formula <- "HT = bh + exp(B1+B2/(DBH+B3))"

   # test for minimum nobs
   if (nrow(tempHD) < minOBS)
   {
      ierr <- 1
      notes="too few observations"      
      out <- NoOutput(model, model.formula, bh, SPCD, nobs=nrow(tempHD), ierr, notes)
#      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
#                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
#                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
#                        meanBIAS=NA, meanAbsBIAS=NA, B3_test=NA, ierr=1, notes="too few observations")
      return(list(eq=NULL, out))
   }

   # weights
   if (any(is.na(wts) == TRUE) | length(wts) != nrow(tempHD))
   {
      wts <- rep(1,nrow(tempHD))
   }

   # get starting values
   
   Y <- log(tempHD$HT-bh)
   X <- (tempHD$DIA+1)^(-1)
   fit1 <- lm(Y ~ X)
   b0 <- as.numeric(coef(fit1)[1])
   b1 <- as.numeric(coef(fit1)[2])
   #m <- summary(fit1)
   #RSE=m$sigma
   #test <- data.frame(model.formula, bh, j=j, b0=b0, b1=b1, b2=CC[j], RSE=m$sigma)
   #testC <- rbind(testC, test) 
   starting <- c(B1=b0, B2=b1, B3=1.00)
   #starting <- c(B1=6.59, B2=-5.62, B3=1.00)
 
   #fit <- nls(HT ~ 4.5 + exp(B1+B2/(DIA+B3)), data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150))
   #starting <- c(B1=b0, B2=b1)
   #fit <- nls(HT ~ 4.5 + exp(B1+B2/(DIA+1.0)), data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150))

   if (useNLS == TRUE)
   {
      # use nls
      eq <- tryCatch({nls(HT ~ 4.5 + exp(B1+B2/(DIA+B3)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.control(maxiter=150))}, 
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   } else {
      # use nlsML
      library(minpack.lm)
      eq <- tryCatch({nlsLM(HT ~ 4.5 + exp(B1+B2/(DIA+B3)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.lm.control(maxiter=150))}, 
                      #error = function(e) {return(NULL)})
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   }

   # check that parameters are reasonable  if converged without errors
   ierr <- 0
   if(is.null(eq) == FALSE & typeof(eq) != "character")
   {
      B1 <- as.numeric(coef(eq)[1])
      B2 <- as.numeric(coef(eq)[2])
      B3 <- as.numeric(coef(eq)[3])
      # checks: B1 > 0; B2 < 0; B3 > 0
      if (B1 <= 0 | B2 >= 0 | B3 <= 0)
      { 
         ierr <- 3
         err_text <- "Parameters Unrealistic"
         if (B1 <= 0) { err_text <- paste0(err_text, " B1") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B2") }
         if (B2 <= 0) { err_text <- paste0(err_text, " B3") }
         eq <- err_text
      }
   }
   
   # summarize fit
   out <- HDfit_summary(model, model.formula, npar=3, SPCD, eq, wt_var, ierr)
   return(list(eq, out))
}

###
### model 6: HT = bh + B1*(1.0-exp(B2*DBH^B3))
###

# Weibull function

# Checks: B1 > 0; B2 < 0; B3 < 0

HT_DBH_Model_6 <- function(tempHD, SPCD, minOBS=0, useNLS=TRUE, wts=NA, wt_var=NA)
{
   model <- 6 
   model.formula <- "HT = bh + B1*(1.0-exp(B2*DBH^B3))"

   # test for minimum nobs
   if (nrow(tempHD) < minOBS)
   {
      ierr <- 1
      notes="too few observations"      
      out <- NoOutput(model, model.formula, bh, SPCD, nobs=nrow(tempHD), ierr, notes)
#      out <- data.frame(model=model, model.formula, bh, WT=NA, SPCD=species_list[i], nobs=nrow(tempHD),
#                        B1=NA, B2=NA, B3=NA, seB1=NA, seB2=NA, seB3=NA, 
#                        pB1=NA, pB2=NA, pB3=NA, RMSE=NA, AIC=NA, 
#                        meanBIAS=NA, meanAbsBIAS=NA, B3_test=NA, ierr=1, notes="too few observations")
      return(list(eq=NULL, out))
   }

   # weights
   if (any(is.na(wts) == TRUE) | length(wts) != nrow(tempHD))
   {
      wts <- rep(1,nrow(tempHD))
   }

   # get starting values
   # starting <- c(B1=max(tempHD$HT), B2=-0.01, B3=0.93)
   # starting <- c(B1=max(tempHD$HT), B2=-0.01, B3=0.93)

   # get 95th percentile DBH threshold
   threshold_DBH <- quantile(tempHD$DIA, 0.95)
   # get mean height with DBH >= 95th percentile
   mean_HT <- mean(tempHD$HT[tempHD$DIA >= threshold_DBH])
   starting <- c(B1=mean_HT, B2=-0.01, B3=1.0)

   # set B3 = 1 (problem if mean_HT > HT-4.5 takes log of negative value and gives NA
   #Y <- log(1.0 - ((tempHD$HT - bh)/mean_HT))
   #X <- tempHD$DIA
   #fit1 <- lm(Y ~ X - 1)
   #b2 <- as.numeric(coef(fit1)[1])
   #starting <- c(B1=mean_HT, B2=b2, B3=1.0)
   #starting <- c(B1=200, B2=-0.03, B3=1.0)
   #starting <- c(B1=mean_HT, B2=-0.03, B3=1.0)

   #nls(HT - bh ~ B1*(1.0-exp(B2*DIA))^B3, data=tempHD, start=starting,
   #                   trace=TRUE, control=nls.control(maxiter=150)) 

   if (useNLS == TRUE)
   {
      # use nls
      eq <- tryCatch({nls(HT ~ 4.5 + B1*(1.0-exp(B2*DIA^B3)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.control(maxiter=150))}, 
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})
   } else {
      # use nlsML
      eq <- tryCatch({nlsLM(HT ~ bh ~ B1*(1.0-exp(B2*DIA^B3)), data=tempHD, start=starting,
                      weights=wts, trace=FALSE, control=nls.lm.control(maxiter=150))}, 
                      #error = function(e) {return(NULL)})
                      #error = function(e) {message("nls error occurred: ",e$message)
                      #return(NULL)})    # #, tol=1e-02))
                      # no details output
                      error = function(e) {return(e$message)})

   }
   
   # check that parameters are reasonable  if converged without errors
   ierr <- 0
   if(is.null(eq) == FALSE & typeof(eq) != "character")
   {
      B1 <- as.numeric(coef(eq)[1])
      B2 <- as.numeric(coef(eq)[2])
      B3 <- as.numeric(coef(eq)[3])
      # Checks: B1 > 0; B2 < 0; B3 > 0
      if (B1 <= 0 | B2 >= 0 | B3 <= 0)
      { 
         ierr <- 3
         err_text <- "Parameters Unrealistic"
         if (B1 <= 0) { err_text <- paste0(err_text, " B1") }
         if (B2 >= 0) { err_text <- paste0(err_text, " B2") }
         if (B2 <= 0) { err_text <- paste0(err_text, " B3") }
         eq <- err_text
      }
   }
   
   # summarize fit
   out <- HDfit_summary(model, model.formula, npar=3, SPCD, eq, wt_var, ierr)
   return(list(eq,out))
}

#--------------------------------------------------------------------------------
# Read Data
#-------------------------------------------------------------------------------

###
### read all modeling data
###

state_list <- c("AK","AL","AR","AZ","CA","CO","CT","DE","FL","GA","IA","ID","IL",
                "IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC",
                "ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC",
                "SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")

dataHD <- NULL

start_time <- Sys.time()
for (i in 1:length(state_list))
{
   #print(i)
   #cat("i, STATENM: ", i, state_list[i], "\n")
   data_path <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/Data_202603/"
   xdate <- 20260301  # data version date
   STATENM <- state_list[i]
   cat("i, STATENM: ", i, state_list[i], "\n")

   if (state_list[i] == "AK") { next }

   temp <- ReadFIATreeModelingTREEData(STATENM, xdate, data_path)
   # [1] "VERSION"          "PLOT"             "STATECD"          "UNITCD"          
   # [5] "COUNTYCD"         "CONDID"           "SUBP"             "INVYR"           
   # [9] "TREE"             "SPCD"             "SPGRPCD"          "STATUSCD"        
   #[13] "AGENTCD"          "DIA"              "DIAHTCD"          "HT"              
   #[17] "HTCD"             "ACTUALHT"         "CR"               "TREECLCD"        
   #[21] "CCLCD"            "TPA_UNADJ"        "EXPAN"            "NLive"           
   #[25] "HTLC"             "TPA"              "BAPA"             "QMD"             
   #[29] "HT40"             "DBH40"            "HTlorey"          "SDI"             
   #[33] "RD"               "CCF"              "BAL"              "CCFL"            
   #[37] "CCH"              "primarySPBA"      "primaryPctBAPA"   "secondarySPBA"   
   #[41] "secondaryPctBAPA" "DIAmissing"       "HTmissing"        "CRmissing"       
   #[45] "MDATE"            "GROWYR"           "DESIGNCD"         "LAT"             
   #[49] "LON"              "ELEV"             "COND_STATUS_CD"   "CONDPROP_UNADJ"  
   #[53] "OWNCD"            "FORTYPCD"         "STDAGE"           "SITECLCD"        
   #[57] "SITECL_METHOD"    "SICOND"           "SIBASE"           "SISP"            
   #[61] "SICOND_FVS"       "SIBASE_FVS"       "SISP_FVS"         "SIEQN_REF_CD_FVS"
   #[65] "SLOPE"            "ASPECT"           "PHYSCLCD"         "STDORGCD"        
   #[69] "STDORGSP"         "MEAS"             "NMEAS"           

   ### data filters

   # trees that have a measured DBH and total HT.
   # DIA = diameter at breast height (inches)
   # HT = total height (feet)
   # HTCD = height measurement method code (1 = field measured)
   # ACTUALHT = height up to the remaining stem (e.g. tip or broken top)
   tempHD <- subset(temp, is.na(HT) == FALSE & DIA > 0 & is.na(HT) == FALSE & HT > 4.5 & HTCD == 1)
   # there are trees with HTCD = 1 but HT > ACTUALHT
   #junk <- subset(dataHD, HT < ACTUALHT)  # these is 1 tree (error?)
   #junk <- subset(dataHD, HT == ACTUALHT)

   # add the number of trees and HT40 for the plot

   #library(dplyr)
   #options(dplyr.summarise.inform = FALSE)
   out <- tempHD %>% group_by(STATECD,UNITCD,COUNTYCD,PLOT,INVYR) %>% 
                   summarize(plotNTrees=n(), .groups = "keep")
                   #summarize(plotNTrees=n(), plotHT40=mean(HT40), .groups = "keep")

   tempHD <- merge(tempHD, out[,c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","plotNTrees")], 
                       by=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR"), all=TRUE)

   dataHD <- rbind(dataHD, tempHD)
}
end_time <- Sys.time()
end_time - start_time
#Time difference of 16.16115 mins

# does not include AK

nrow(dataHD)
#[1] 9590425  or 9509054 without AK

### summarize data

table(dataHD$SPCD)

junk <- data.frame(table(dataHD$SPCD))
nrow(junk)  # 439 species    # 438 without AK

#summary(junk$Freq)
##     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##      1.0      52.5    1001.0   21889.3   12299.5 1095025.0 
##      1.0      52.25    999.0   21710     12130   1095000    no AK
#hist(junk$Freq, xlab="Number of Heights", ylab="Number of Species")

#junk1 <- subset(junk, Freq < 1000)
#hist(junk1$Freq, xlab="Number of Heights", ylab="Number of Species")

###
### read testing data for a single state
###

#dataHD <- NULL

### testing data

#   data_path <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/Data_202603/"
#   xdate <- 20260301  # data version date
#   STATENM <- "OR"
#   temp <- ReadFIATreeModelingTREEData(STATENM, xdate, data_path)
#
#   tempHD <- subset(temp, is.na(HT) == FALSE & DIA > 0 & is.na(HT) == FALSE & HT > 4.5 & HTCD == 1)
#
#   out <- tempHD %>% group_by(STATECD,UNITCD,COUNTYCD,PLOT,INVYR) %>% 
#                   summarize(plotNTrees=n(), .groups = "keep")
#                   #summarize(plotNTrees=n(), plotHT40=mean(HT40), .groups = "keep")
#   tempHD <- merge(tempHD, out[,c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","plotNTrees")], 
#                       by=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR"), all=TRUE)
#   #dataHD <- rbind(dataHD, tempHD)
#   dataHD <- tempHD

### quick checks

subset(dataHD, DIA <= 0)
subset(dataHD, HT <= 4.5)

#--------------------------------------------------------------------------------
# FIA HT-DBH Model Fitting
#-------------------------------------------------------------------------------

species_list <- unique(dataHD$SPCD)
length(species_list)

#which(species_list == 902)    # 826
#which(species_list == 56)     # 259

bh <- 4.5

#  model.formula <- "HT = bh + exp(B1 + B2*DBH^B3)"

model.type <- 6  # fits individual model equation
#model.type <- 0  # fits all models (testing)
outFits <- NULL
outHD <- NULL

for (i in 1:length(species_list))
{
   #i <- 1
   print(i)
   species_list[i]
   tempHD <- subset(dataHD, SPCD == species_list[i])
   nrow(tempHD)

   #

   minDBH <- min(tempHD$DIA)
   maxDBH <- max(tempHD$DIA)
   minHT <- min(tempHD$HT)
   maxHT <- max(tempHD$HT)
   nobs <- nrow(tempHD)
   outHD <- rbind(outHD, data.frame(SPCD=species_list[i], nobs, minDBH, maxDBH, minHT, maxHT))

   # plot raw data for species
   #plot_HD_data(tempHD, species=species_list[i])

   # weights
   #wts <- rep(1.0, nrow(tempHD))   # no weighting
   # wt_var <- NA
   wts <- 1.0/tempHD$DIA
   wt_var <- "1/DBH"


#   model.type <- 1
   if ( model.type == 1 | model.type == 0)
   {
      #outNL <- HT_DBH_Model_1(tempHD, minOBS=3, useNLS=TRUE)
      outNL <- HT_DBH_Model_1(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts, wt_var=wt_var)
      outFits <- rbind(outFits, outNL[[2]])
   }

#   model.type <- 2
   if (model.type == 2 | model.type == 0)
   {
      outNL <- HT_DBH_Model_2(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts)
      # test if Ho: B3 = 1.0 (return p-value)
      if (outNL[[2]]$ierr == 0)
      {
         B3_test <- test_parameter(outNL[[1]], param="B3", test_value=-1.0)
         outNL[[2]]$B3_test <- B3_test
      }
      outFits <- rbind(outFits, outNL[[2]])
   }

#   model.type <- 3
   if (model.type == 3 | model.type == 0)
   {
      outNL <- HT_DBH_Model_3(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts)
      outFits <- rbind(outFits, outNL[[2]])
   }

#   model.type <- 4
   if (model.type == 4 | model.type == 0)
   {
      outNL <- HT_DBH_Model_4(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts)
      outFits <- rbind(outFits, outNL[[2]])
   }

#   model.type <- 5
   if (model.type == 5 | model.type == 0)
   {
      outNL <- HT_DBH_Model_5(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts)
      # test if Ho: B3 = 1.0 (return p-value)
      if (outNL[[2]]$ierr == 0)
      {
         B3_test <- test_parameter(outNL[[1]], param="B3", test_value=1.0)
         outNL[[2]]$B3_test <- B3_test
      }
      outFits <- rbind(outFits, outNL[[2]])
   }

#   model.type <- 6
   if (model.type == 6 | model.type == 0)
   {
      outNL <- HT_DBH_Model_6(tempHD, SPCD=species_list[i], minOBS=3, useNLS=TRUE, wts=wts)
      outFits <- rbind(outFits, outNL[[2]])
   }

}

table(outFits$ierr)

# model 1 Checks: B1 > 0; B2 < 0
#summary(outFits$B1)
#summary(outFits$B2)

# model 2 Checks: B1 > 0; B2 < 0; B3 < 0
#summary(outFits$B1)
#summary(outFits$B2)
#summary(outFits$B3)

# model 3 Checks: B1 > 0; B2 < 0; B3 > 0
#summary(outFits$B1)
#summary(outFits$B2)
#summary(outFits$B3)

# model 4 Checks: B1 > 0; B2 < 0
#summary(outFits$B1)
#summary(outFits$B2)

# model 5 Checks: B1 > 0; B2 < 0; B3 > 0
#summary(outFits$B1)
#summary(outFits$B2)
#summary(outFits$B3)

# model 6  Checks: B1 > 0; B2 < 0; B3 > 0
#summary(outFits$B1)
#summary(outFits$B2)
#summary(outFits$B3)


#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_1.CSV"
#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_2.CSV"
#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_3.CSV"
#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_4.CSV"
#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_5.CSV"
#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_6.CSV"

#write.table(outFits, file=file.name, sep=",", row.names=FALSE, col.names=TRUE)

#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Data.CSV"
#write.table(outHD, file=file.name, sep=",", row.names=FALSE, col.names=TRUE)

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Data.CSV"
outHD <- read.csv(file.name, header=TRUE, sep=",")

par(mfrow=c(1,2))

plot(outHD$maxDBH, outHD$maxHT, pch=c(19), col="black", xlim=c(0,200), ylim=c(0,1000), cex=0.50,
          xlab="DBH (inches)", ylab="Total Height (feet)")
points(outHD$minDBH, outHD$minHT, pch=c(19), col="red", cex=0.50)
#for (k in 1:nrow(outHD))
#{
#   lines(x=c(outHD$minDBH[k], outHD$maxDBH[k]), y=c(outHD$minHT[k], outHD$maxHT[k]), col="gray")
#}
#points(outHD$minDBH, outHD$minHT, pch=c(19), col="red")
#points(outHD$maxDBH, outHD$maxHT, pch=c(19), col="black")

outXX <- subset(outHD, maxHT < 400) 
plot(outXX$maxDBH, outXX$maxHT, pch=c(19), col="black", xlim=c(0,200), ylim=c(0,400), cex=0.50,
          xlab="DBH (inches)", ylab="Total Height (feet)")
points(outXX$minDBH, outXX$minHT, pch=c(19), col="red", cex=0.50)

par(mfrow=c(1,1))

summary(outHD$maxDBH)
summary(outHD$maxHT)


#file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/SpeciesMapping/REF_SPECIES.csv"
#ref_species <- read.csv(file.name, header=TRUE, sep=",")
#ref_speciesEW <- ref_species[,c("SPCD","E_SPGRPCD","W_SPGRPCD","C_SPGRPCD")]
#ref_speciesEW <- subset(ref_speciesEW, E_SPGRPCD <= 48)
#outHDx <- merge(outHD, ref_species[], by=c(), all.x)

                                                    # Model 1  Model 2



nrow(outFits)                                       #   439      439
nrow(outFits[outFits$B1 > 0,])                      #   413      339
junk <- subset(outFits, pB1 > 0.05)
junk <- subset(outFits, pB2 > 0.05)
junk <- subset(outFits, pB3 > 0.05)

subset(outFits, B3_test > 0.05)

# total RMSE
tempRMSE <- subset(outFits, RMSE > 0)
n <- nrow(tempRMSE)
# RMSE^2 = MSE
# total squared errors = MSE * n
sqrt(sum(tempRMSE$RMSE^2 * tempRMSE$nobs)/n)        #  607.4     600.1

#--------------------------------------------------------------------------------
# fit summaries
#-------------------------------------------------------------------------------

# read fits files

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_1.CSV"
dataM1 <- read.csv(file.name, header=TRUE, sep=",")
dataHDfits <- dataM1

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_2.CSV"
dataM2 <- read.csv(file.name, header=TRUE, sep=",")
dataHDfits <- rbind(dataHDfits, dataM2)

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_3.CSV"
dataM3 <- read.csv(file.name, header=TRUE, sep=",")
dataHDfits <- rbind(dataHDfits, dataM3)

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_4.CSV"
dataM4 <- read.csv(file.name, header=TRUE, sep=",")
dataHDfits <- rbind(dataHDfits, dataM4)

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_5.CSV"
dataM5 <- read.csv(file.name, header=TRUE, sep=",")
dataHDfits <- rbind(dataHDfits, dataM5)

file.name <- "C:/Users/DavidFolder/Projects/FIA_Data/ModelingProject/HT_DBH_Modeling/CONUS_HDfit_Model_6.CSV"
dataM6 <- read.csv(file.name, header=TRUE, sep=",")
dataHDfits <- rbind(dataHDfits, dataM6)

library(dplyr)

table(dataHDfits$ierr)

# part 1 - summarize fits

# total species
out1 <- dataHDfits %>% group_by(model) %>%  summarize(Nspecies=n())

# number of species fit
#out2 <- dataHDfits %>% group_by(model) %>% filter(is.na(B1) == FALSE) %>% summarize(Nfit=n())
out2 <- dataHDfits %>% group_by(model) %>% filter(ierr == 0) %>% summarize(Nfit=n())
out <- merge(out1, out2, by=c("model"), all.x=TRUE)
#out <- rbind(t(out1), t(out2[,2]))

# number of species with too few observations
out2a <- dataHDfits %>% group_by(model) %>% filter(ierr == 1) %>% summarize(Nminobs=n())
out <- merge(out, out2a, by=c("model"), all.x=TRUE)

# number of species that did not fit
out2b <- dataHDfits %>% group_by(model) %>% filter(ierr == 2) %>% summarize(Nnofit=n())
out <- merge(out, out2b, by=c("model"), all.x=TRUE)

# number of species with unreasonable parameters
out2c <- dataHDfits %>% group_by(model) %>% filter(ierr == 3) %>% summarize(Npoorcoef=n())
out <- merge(out, out2c, by=c("model"), all.x=TRUE)

# fits with non-significant parameters
out3 <- dataHDfits %>% group_by(model) %>% 
   filter((is.na(B1) == FALSE & pB1 > 0.05) | (is.na(B2) == FALSE & pB2 > 0.05) | (is.na(B3) == FALSE & B3 > 0 & pB3 > 0.05)) %>%
   #filter((B1 > 0 & pB1 > 0.05) | (B2 > 0 & pB2 > 0.05) | (is.na(B3) == FALSE & B3 > 0 & pB3 > 0.05)) %>%
   summarize(N_Bns=n())
out <- merge(out, out3, by=c("model"), all.x=TRUE)

# fits "good" fits
out4 <- dataHDfits %>% group_by(model) %>% 
   filter((is.na(B1) == FALSE & pB1 <= 0.05) | (is.na(B2) == FALSE & pB2 <= 0.05) | (is.na(B3) == FALSE & B3 > 0 & pB3 <= 0.05)) %>%
   summarize(N_Bgood=n())
out <- merge(out, out4, by=c("model"), all.x=TRUE)

# model 2 B3 significantly different from -1
junk <- subset(dataHDfits, model == 2)
out5a <- junk %>% filter(is.na(B3_test) == FALSE & B3_test <= 0.05) %>% summarize(N_B3test=n())
out5a <- cbind(model=c(2),out5a)
#out <- merge(out, out5a, by=c("model"), all.x=TRUE)

# model 5 B3 significantly different from 1
junk <- subset(dataHDfits, model == 5)
out5b <- junk %>% filter(is.na(B3_test) == FALSE & B3_test <= 0.05) %>% summarize(N_B3test=n())
out5b <- cbind(model=c(5),out5b)
#out <- merge(out, out5b, by=c("model"), all.x=TRUE)
out5 <- rbind(out5a, out5b)
out <- merge(out, out5, by=c("model"), all.x=TRUE)

# part 2 - summarize fit statistics (RMSE and mean bias and residuals)

junk1 <- subset(dataHDfits, model == 1 & ierr == 0, select=c("model","SPCD","RMSE","meanBIAS","meanAbsBIAS"))
names(junk1) <- c("model","SPCD","RMSE_1","meanBIAS_1","meanAbsBIAS_1")
junk2 <- subset(dataHDfits, model == 2 & ierr == 0, select=c("model","SPCD","RMSE","meanBIAS","meanAbsBIAS"))
names(junk2) <- c("model","SPCD","RMSE_2","meanBIAS_2","meanAbsBIAS_2")
junk3 <- subset(dataHDfits, model == 3 & ierr == 0, select=c("model","SPCD","RMSE","meanBIAS","meanAbsBIAS"))
names(junk3) <- c("model","SPCD","RMSE_3","meanBIAS_3","meanAbsBIAS_3")
junk4 <- subset(dataHDfits, model == 4 & ierr == 0, select=c("model","SPCD","RMSE","meanBIAS","meanAbsBIAS"))
names(junk4) <- c("model","SPCD","RMSE_4","meanBIAS_4","meanAbsBIAS_4")
junk5 <- subset(dataHDfits, model == 5 & ierr == 0, select=c("model","SPCD","RMSE","meanBIAS","meanAbsBIAS"))
names(junk5) <- c("model","SPCD","RMSE_5","meanBIAS_5","meanAbsBIAS_5")
junk6 <- subset(dataHDfits, model == 6 & ierr == 0, select=c("model","SPCD","RMSE","meanBIAS","meanAbsBIAS"))
names(junk6) <- c("model","SPCD","RMSE_6","meanBIAS_6","meanAbsBIAS_6")

#dataRMSE <- merge(junk1[,c("SPCD","RMSE_1")], junk2[,c("SPCD","RMSE_2")], by=c("SPCD"), all=TRUE)
#dataRMSE <- merge(dataRMSE, junk3[,c("SPCD","RMSE_3")], by=c("SPCD"), all=TRUE)
#dataRMSE <- merge(dataRMSE, junk4[,c("SPCD","RMSE_4")], by=c("SPCD"), all=TRUE)
#dataRMSE <- merge(dataRMSE, junk5[,c("SPCD","RMSE_5")], by=c("SPCD"), all=TRUE)

## summarize RMSE

# 1. count lowest RMSE by model
# 2. minimum RMSE
# 3. average difference in RMSE between models (largest to smallest RMSE) 

dataRMSE <- merge(junk1[,c("SPCD","RMSE_1")], junk2[,c("SPCD","RMSE_2")], by=c("SPCD"), all=TRUE)
dataRMSE <- merge(dataRMSE, junk3[,c("SPCD","RMSE_3")], by=c("SPCD"), all=TRUE)
dataRMSE <- merge(dataRMSE, junk4[,c("SPCD","RMSE_4")], by=c("SPCD"), all=TRUE)
dataRMSE <- merge(dataRMSE, junk5[,c("SPCD","RMSE_5")], by=c("SPCD"), all=TRUE)
dataRMSE <- merge(dataRMSE, junk6[,c("SPCD","RMSE_6")], by=c("SPCD"), all=TRUE)

dataRMSE$minRMSEmodel <- NA    # model with minimum RMSE
dataRMSE$minRMSE <- NA    # lowest RMSE
dataRMSE$maxRMSE <- NA    # lowest RMSE
dataRMSE$diffRMSE <- NA   # difference of min and max RMSE
dataRMSE$avgRMSE <- NA    # average RMSE for models
for (i in 1:nrow(dataRMSE))
{
   # drop species with no fits (too few obs or no convergence)
   if (is.na(dataRMSE$RMSE_1[i])==TRUE & is.na(dataRMSE$RMSE_2[i])==TRUE &
       is.na(dataRMSE$RMSE_3[i])==TRUE & is.na(dataRMSE$RMSE_4[i])==TRUE &
       is.na(dataRMSE$RMSE_5[i])==TRUE & is.na(dataRMSE$RMSE_6[i])==TRUE) { next }
   dataRMSE$minRMSEmodel[i] <- names(which.min(dataRMSE[i,c(2:7)]))
   dataRMSE$minRMSE[i] <- dataRMSE[i,1+as.numeric((which.min(dataRMSE[i,c(2:7)])))]
   dataRMSE$maxRMSE[i] <- dataRMSE[i,1+as.numeric((which.max(dataRMSE[i,c(2:7)])))]
   dataRMSE$diffRMSE[i] <- max(dataRMSE[i,c(2:7)], na.rm=TRUE) - min(dataRMSE[i,c(2:7)], na.rm=TRUE)
   dataRMSE$avgRMSE[i] <- mean(as.numeric(dataRMSE[i, c(2:7)]), na.rm=TRUE)
}

# get counts of minimum RMSE by model
temp <- dataRMSE %>% count(minRMSEmodel, name = "minRMSEcount") 
temp$model <- substr(temp$minRMSEmodel, 6, 6)
out11 <- temp[,c("model","minRMSEcount")]
# mean RMSE by models (note n is not the same)
meanRMSE <- c(mean(dataRMSE$RMSE_1, na.rm=TRUE),
              mean(dataRMSE$RMSE_2, na.rm=TRUE),
              mean(dataRMSE$RMSE_3, na.rm=TRUE),
              mean(dataRMSE$RMSE_4, na.rm=TRUE),
              mean(dataRMSE$RMSE_5, na.rm=TRUE),
              mean(dataRMSE$RMSE_6, na.rm=TRUE))

# get nobs for weighting
junkn <- subset(dataHDfits, model == 1, select=c("SPCD","nobs"))
dataRMSE <- merge(dataRMSE, junkn, by=c("SPCD"), all=TRUE)
meanRMSEw <- c(weighted.mean(dataRMSE$RMSE_1, dataRMSE$nobs, na.rm=TRUE),
               weighted.mean(dataRMSE$RMSE_2, dataRMSE$nobs, na.rm=TRUE),
               weighted.mean(dataRMSE$RMSE_3, dataRMSE$nobs, na.rm=TRUE),
               weighted.mean(dataRMSE$RMSE_4, dataRMSE$nobs, na.rm=TRUE),
               weighted.mean(dataRMSE$RMSE_5, dataRMSE$nobs, na.rm=TRUE),
               weighted.mean(dataRMSE$RMSE_6, dataRMSE$nobs, na.rm=TRUE))

out11 <- cbind(out11,meanRMSE)
out11 <- cbind(out11,meanRMSEw)

## summarize mean bias (pred - act)

dataBIAS <- merge(junk1[,c("SPCD","meanBIAS_1")], junk2[,c("SPCD","meanBIAS_2")], by=c("SPCD"), all=TRUE)
dataBIAS <- merge(dataBIAS, junk3[,c("SPCD","meanBIAS_3")], by=c("SPCD"), all=TRUE)
dataBIAS <- merge(dataBIAS, junk4[,c("SPCD","meanBIAS_4")], by=c("SPCD"), all=TRUE)
dataBIAS <- merge(dataBIAS, junk5[,c("SPCD","meanBIAS_5")], by=c("SPCD"), all=TRUE)
dataBIAS <- merge(dataBIAS, junk6[,c("SPCD","meanBIAS_6")], by=c("SPCD"), all=TRUE)

# need to fix this where minBIAS is nearest to 0

dataBIAS$minBIASmodel <- NA    # model with minimum RMSE
dataBIAS$minBIAS <- NA    # lowest RMSE
dataBIAS$maxBIAS <- NA    # lowest RMSE
dataBIAS$diffBIAS <- NA   # difference of min and max RMSE
dataBIAS$avgBIAS <- NA    # average RMSE for models
for (i in 1:nrow(dataBIAS))
{
   # drop species with no fits (too few obs or no convergence)
   if (is.na(dataBIAS$meanBIAS_1[i])==TRUE & is.na(dataBIAS$meanBIAS_2[i])==TRUE &
       is.na(dataBIAS$meanBIAS_3[i])==TRUE & is.na(dataBIAS$meanBIAS_4[i])==TRUE &
       is.na(dataBIAS$meanBIAS_5[i])==TRUE & is.na(dataBIAS$meanBIAS_6[i])==TRUE) { next }
   # minBIAS model is nearest to 0
   dataBIAS$minBIASmodel[i] <- names(which.min(abs(dataBIAS[i,c(2:7)])))
   dataBIAS$minBIAS[i] <- dataBIAS[i,1+as.numeric((which.min(dataBIAS[i,c(2:7)])))]
   dataBIAS$maxBIAS[i] <- dataBIAS[i,1+as.numeric((which.max(dataBIAS[i,c(2:7)])))]
   dataBIAS$diffBIAS[i] <- max(dataBIAS[i,c(2:7)], na.rm=TRUE) - min(dataBIAS[i,c(2:7)], na.rm=TRUE)
   dataBIAS$avgBIAS[i] <- mean(as.numeric(dataBIAS[i, c(2:7)]), na.rm=TRUE)
}

# get counts of minimum BIAS by model
temp <- dataBIAS %>% count(minBIASmodel, name = "minBIAScount") 
temp$model <- substr(temp$minBIASmodel, 10, 10)
out12 <- temp[,c("model","minBIAScount")]
# mean BIAS by models pred-act (note n is not the same)
meanBIAS <- c(mean(dataBIAS$meanBIAS_1, na.rm=TRUE),
              mean(dataBIAS$meanBIAS_2, na.rm=TRUE),
              mean(dataBIAS$meanBIAS_3, na.rm=TRUE),
              mean(dataBIAS$meanBIAS_4, na.rm=TRUE),
              mean(dataBIAS$meanBIAS_5, na.rm=TRUE),
              mean(dataBIAS$meanBIAS_6, na.rm=TRUE))

# get nobs for weighting
junkn <- subset(dataHDfits, model == 1, select=c("SPCD","nobs"))
dataBIAS <- merge(dataBIAS, junkn, by=c("SPCD"), all=TRUE)
meanBIASw <- c(weighted.mean(dataBIAS$meanBIAS_1, dataBIAS$nobs, na.rm=TRUE),
               weighted.mean(dataBIAS$meanBIAS_2, dataBIAS$nobs, na.rm=TRUE),
               weighted.mean(dataBIAS$meanBIAS_3, dataBIAS$nobs, na.rm=TRUE),
               weighted.mean(dataBIAS$meanBIAS_4, dataBIAS$nobs, na.rm=TRUE),
               weighted.mean(dataBIAS$meanBIAS_5, dataBIAS$nobs, na.rm=TRUE),
               weighted.mean(dataBIAS$meanBIAS_6, dataBIAS$nobs, na.rm=TRUE))

out12 <- cbind(out12,meanBIAS)
out12 <- cbind(out12,meanBIASw)

outF <- merge(out11, out12, by=c("model"), all=TRUE)
outF$meanRMSE <- round(outF$meanRMSE,2)
outF$meanRMSEw <- round(outF$meanRMSEw,2)
outF$meanBIAS <- round(outF$meanBIAS,4)
outF$meanBIASw <- round(outF$meanBIASw,4)
outF$model <- as.numeric(outF$model)

# summarize fit bias by DBH thirds

junk1 <- subset(dataHDfits, model == 1 & ierr == 0, select=c("model","SPCD","meanLowDBHRes","meanMidDBHRes","meanUppDBHRes"))
names(junk1) <- c("model","SPCD","meanLowDBHRes_1","meanMidDBHRes_1","meanUppDBHRes_1")
junk2 <- subset(dataHDfits, model == 2 & ierr == 0, select=c("model","SPCD","meanLowDBHRes","meanMidDBHRes","meanUppDBHRes"))
names(junk2) <- c("model","SPCD","meanLowDBHRes_2","meanMidDBHRes_2","meanUppDBHRes_2")
junk3 <- subset(dataHDfits, model == 3 & ierr == 0, select=c("model","SPCD","meanLowDBHRes","meanMidDBHRes","meanUppDBHRes"))
names(junk3) <- c("model","SPCD","meanLowDBHRes_3","meanMidDBHRes_3","meanUppDBHRes_3")
junk4 <- subset(dataHDfits, model == 4 & ierr == 0, select=c("model","SPCD","meanLowDBHRes","meanMidDBHRes","meanUppDBHRes"))
names(junk4) <- c("model","SPCD","meanLowDBHRes_4","meanMidDBHRes_4","meanUppDBHRes_4")
junk5 <- subset(dataHDfits, model == 5 & ierr == 0, select=c("model","SPCD","meanLowDBHRes","meanMidDBHRes","meanUppDBHRes"))
names(junk5) <- c("model","SPCD","meanLowDBHRes_5","meanMidDBHRes_5","meanUppDBHRes_5")
junk6 <- subset(dataHDfits, model == 6 & ierr == 0, select=c("model","SPCD","meanLowDBHRes","meanMidDBHRes","meanUppDBHRes"))
names(junk6) <- c("model","SPCD","meanLowDBHRes_6","meanMidDBHRes_6","meanUppDBHRes_6")

dataGRPres <- merge(junk1[,c("SPCD","meanLowDBHRes_1","meanMidDBHRes_1","meanUppDBHRes_1")], 
                    junk2[,c("SPCD","meanLowDBHRes_2","meanMidDBHRes_2","meanUppDBHRes_2")], by=c("SPCD"), all=TRUE)
dataGRPres <- merge(dataGRPres, junk3[,c("SPCD","meanLowDBHRes_3","meanMidDBHRes_3","meanUppDBHRes_3")], by=c("SPCD"), all=TRUE)
dataGRPres <- merge(dataGRPres, junk4[,c("SPCD","meanLowDBHRes_4","meanMidDBHRes_4","meanUppDBHRes_4")], by=c("SPCD"), all=TRUE)
dataGRPres <- merge(dataGRPres, junk5[,c("SPCD","meanLowDBHRes_5","meanMidDBHRes_5","meanUppDBHRes_5")], by=c("SPCD"), all=TRUE)
dataGRPres <- merge(dataGRPres, junk6[,c("SPCD","meanLowDBHRes_6","meanMidDBHRes_6","meanUppDBHRes_6")], by=c("SPCD"), all=TRUE)

# find which models have lowest bias for each third
dataGRPres$LOWmodel <- NA
dataGRPres$MIDmodel <- NA
dataGRPres$UPPmodel <- NA
# lower third
for (i in 1:nrow(dataGRPres))
{
   # drop species with no fits (too few obs or no convergence)
   if (is.na(dataGRPres$meanLowDBHRes_1[i])==TRUE & is.na(dataGRPres$meanLowDBHRes_2[i])==TRUE &
       is.na(dataGRPres$meanLowDBHRes_3[i])==TRUE & is.na(dataGRPres$meanLowDBHRes_4[i])==TRUE &
       is.na(dataGRPres$meanLowDBHRes_5[i])==TRUE & is.na(dataGRPres$meanLowDBHRes_6[i])==TRUE) { next }
   # minBIAS model is nearest to 0
   dataGRPres$LOWmodel[i] <- names(which.min(abs(dataGRPres[i,c(2,5,8,11,14,17)])))
}
# middle third
for (i in 1:nrow(dataGRPres))
{
   #print(i)
   # drop species with no fits (too few obs or no convergence)
   if (is.na(dataGRPres$meanMidDBHRes_1[i])==TRUE & is.na(dataGRPres$meanMidDBHRes_2[i])==TRUE &
       is.na(dataGRPres$meanMidDBHRes_3[i])==TRUE & is.na(dataGRPres$meanMidDBHRes_4[i])==TRUE &
       is.na(dataGRPres$meanMidDBHRes_5[i])==TRUE & is.na(dataGRPres$meanMidDBHRes_6[i])==TRUE) { next }
   # minBIAS model is nearest to 0
   dataGRPres$MIDmodel[i] <- names(which.min(abs(dataGRPres[i,c(3,6,9,12,15,18)])))
   #print(dataGRPres$MIDmodel[i])
}
# upper third
for (i in 1:nrow(dataGRPres))
{
   # drop species with no fits (too few obs or no convergence)
   if (is.na(dataGRPres$meanUppDBHRes_1[i])==TRUE & is.na(dataGRPres$meanUppDBHRes_2[i])==TRUE &
       is.na(dataGRPres$meanUppDBHRes_3[i])==TRUE & is.na(dataGRPres$meanUppDBHRes_4[i])==TRUE &
       is.na(dataGRPres$meanUppDBHRes_5[i])==TRUE & is.na(dataGRPres$meanUppDBHRes_6[i])==TRUE) { next }
   # minBIAS model is nearest to 0
   dataGRPres$UPPmodel[i] <- names(which.min(abs(dataGRPres[i,c(4,7,10,13,16,19)])))
}

tempL <- dataGRPres %>% count(LOWmodel, name = "minLOWcount") 
tempL$model <- substr(tempL$LOWmodel, 15, 15)
outLOW <- tempL[,c("model","minLOWcount")]

tempM <- dataGRPres %>% count(MIDmodel, name = "minMIDcount")     # <--- NAs
tempM$model <- substr(tempM$MIDmodel, 15, 15)
outMID <- tempM[,c("model","minMIDcount")]

tempU <- dataGRPres %>% count(UPPmodel, name = "minUPPcount") 
tempU$model <- substr(tempU$UPPmodel, 15, 15)
outUPP <- tempU[,c("model","minUPPcount")]


### plot coefficient 

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 1 & ierr == 0)
plot(junk$B1, junk$B2, xlab="B1", ylab="B2", main="Model 1")
#plot(junk$B1, junk$B3)
#plot(junk$B2, junk$B3)
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 2 & ierr == 0)
plot(junk$B1, junk$B2, xlab="B1", ylab="B2", main="Model 2")
plot(junk$B1, junk$B3, xlab="B1", ylab="B3", main="Model 2")
plot(junk$B2, junk$B3, xlab="B2", ylab="B3", main="Model 2")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 3 & ierr == 0)
plot(junk$B1, junk$B2, xlab="B1", ylab="B2", main="Model 3")
plot(junk$B1, junk$B3, xlab="B1", ylab="B3", main="Model 3")
plot(junk$B2, junk$B3, xlab="B2", ylab="B3", main="Model 3")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 4 & ierr == 0)
plot(junk$B1, junk$B2, xlab="B1", ylab="B2", main="Model 4")
#plot(junk$B1, junk$B3)
#plot(junk$B2, junk$B3)
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 5 & ierr == 0)
plot(junk$B1, junk$B2, xlab="B1", ylab="B2", main="Model 5")
plot(junk$B1, junk$B3, xlab="B1", ylab="B3", main="Model 5")
plot(junk$B2, junk$B3, xlab="B2", ylab="B3", main="Model 5")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 6 & ierr == 0)
plot(junk$B1, junk$B2, xlab="B1", ylab="B2", main="Model 5")
plot(junk$B1, junk$B3, xlab="B1", ylab="B3", main="Model 5")
plot(junk$B2, junk$B3, xlab="B2", ylab="B3", main="Model 5")
par(mfrow=c(1,1))

### histogram of coefficients

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 1 & ierr == 0)
hist(junk$B1, xlab="B1", ylab="Count", main="Model 1: B1")
hist(junk$B2, xlab="B2", ylab="Count", main="Model 1: B2")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 2 & ierr == 0)
hist(junk$B1, xlab="B1", ylab="Count", main="Model 2: B1")
hist(junk$B2, xlab="B2", ylab="Count", main="Model 2: B2")
hist(junk$B3, xlab="B3", ylab="Count", main="Model 2: B3")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 3 & ierr == 0)
hist(junk$B1, xlab="B1", ylab="Count", main="Model 3: B1")
hist(junk$B2, xlab="B2", ylab="Count", main="Model 3: B2")
hist(junk$B3, xlab="B3", ylab="Count", main="Model 3: B3")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 4 & ierr == 0)
hist(junk$B1, xlab="B1", ylab="Count", main="Model 4: B1")
hist(junk$B2, xlab="B2", ylab="Count", main="Model 4: B2")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 5 & ierr == 0)
hist(junk$B1, xlab="B1", ylab="Count", main="Model 5: B1")
hist(junk$B2, xlab="B2", ylab="Count", main="Model 5: B2")
hist(junk$B3, xlab="B3", ylab="Count", main="Model 5: B3")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
junk <- subset(dataHDfits, model == 3 & ierr == 0)
hist(junk$B1, xlab="B1", ylab="Count", main="Model 6: B1")
hist(junk$B2, xlab="B2", ylab="Count", main="Model 6: B2")
hist(junk$B3, xlab="B3", ylab="Count", main="Model 6: B3")
par(mfrow=c(1,1))

###  plot predictions by model

#dataHDfits$ierr <- ifelse(dataHDfits$ierr == 0 & is.na(dataHDfits$pB1) == FALSE & dataHDfits$pB1 > 0.05, 4, dataHDfits$ierr)
#dataHDfits$ierr <- ifelse(dataHDfits$ierr == 0 & is.na(dataHDfits$pB2) == FALSE & dataHDfits$pB2 > 0.05, 4, dataHDfits$ierr)
#dataHDfits$ierr <- ifelse(dataHDfits$ierr == 0 & is.na(dataHDfits$pB3) == FALSE & dataHDfits$pB3 > 0.05, 4, dataHDfits$ierr)

table(dataHDfits$ierr)

x <- seq(0,50,1)

par(mfrow=c(3,2))

# model 1: HT = bh + exp(B1 + B2*DBH^(-1.0))
temp <- subset(dataHDfits, model == 1 & is.na(B1) == FALSE)
plot(x=c(0,50), y=c(0,250), type="n", xlab="DBH (inches)", ylab="Height (feet)",
       main="Model 1 predictions")
for (i in 1:nrow(temp))
{
   if (is.na(temp$B1[i]) == TRUE) { next }
   B1 <- temp$B1[i] 
   B2 <- temp$B2[i]
   #B3 <- temp$B3[i]
   B3 <- -1.0
   y <- 4.5 + exp(B1 + B2*x^B3)
   lines(x, y, col=ifelse(temp$ierr[i] == 0, 1, 2))
   if (any(x >= 50 & y > 160)) { cat("Problem: ", i, "\n") }
}
# model 1 issues

# temp[259,]
#    model                     model.formula  bh    WT SPCD nobs       B1
#260     1 HT = bh + exp(B1 + B2*DBH^(-1.0)) 4.5 1/DBH  212   48 5.660705
#           B2 B3       seB1     seB2 seB3          pB1          pB2 pB3
#260 -21.44181 NA 0.05097648 1.507085   NA 1.521182e-57 1.840282e-18  NA
#        RMSE      AIC  meanBIAS meanAbsBIAS B3_test ierr notes
#260 4.688889 440.6973 -3.419273    17.47901      NA    0   fit

# model 2: HT = bh + exp(B1 + B2*DBH^B3)
temp <- subset(dataHDfits, model == 2 & is.na(B1) == FALSE)
plot(x=c(0,50), y=c(0,250), type="n", xlab="DBH (inches)", ylab="Height (feet)",
       main="Model 2 predictions")
for (i in 1:nrow(temp))
{
   print(i)
   if (is.na(temp$B1[i]) == TRUE) { next }
   B1 <- temp$B1[i] 
   B2 <- temp$B2[i]
   B3 <- temp$B3[i]
   y <- 4.5 + exp(B1 + B2*x^B3)
   lines(x, y, col=ifelse(temp$ierr[i] == 0, 1, 2))
}

# model 3: HT = bh + B1*(1.0-exp(B2*DBH))^B3
temp <- subset(dataHDfits, model == 3)
plot(x=c(0,50), y=c(0,250), type="n", xlab="DBH (inches)", ylab="Height (feet)",
       main="Model 3 predictions")
for (i in 1:nrow(temp))
{
   if (is.na(temp$B1[i]) == TRUE) { next }
   B1 <- temp$B1[i] 
   B2 <- temp$B2[i]
   B3 <- temp$B3[i]
   y <- 4.5 + B1*(1.0-exp(B2*x))^B3
   lines(x, y, col=ifelse(temp$ierr[i] == 0, 1, 2))
   #if (any(x >= 50 & y > 160)) { cat("Problem: ", i, "\n") }
}
#temp[214,]
#     model                     model.formula  bh   WT SPCD nobs       B1
#1090     3 HT = bh + B1*(1.0-exp(B2*DBH))^B3 4.5 <NA>  211 9478 223.3278
#              B2        B3    seB1        seB2        seB3 pB1           pB2
#1090 -0.02415897 0.9669631 4.81146 0.001003961 0.009835856   0 3.049023e-124
#     pB3     RMSE      AIC    meanBIAS meanAbsBIAS B3_test ierr notes
#1090   0 4.003161 78912.32 -0.03108796    13.73184      NA    0   fit
#temp[216,]
#     model                     model.formula  bh   WT SPCD  nobs       B1
#1092     3 HT = bh + B1*(1.0-exp(B2*DBH))^B3 4.5 <NA>  263 81261 171.1069
#              B2       B3      seB1         seB2        seB3 pB1 pB2 pB3
#1092 -0.05484198 1.200176 0.8219492 0.0005433294 0.004881571   0   0   0
#         RMSE      AIC    meanBIAS meanAbsBIAS B3_test ierr notes
#1092 4.395243 650033.7 -0.01643458    11.18772      NA    0   fit
#temp[217,]
#     model                     model.formula  bh   WT SPCD nobs       B1
#1093     3 HT = bh + B1*(1.0-exp(B2*DBH))^B3 4.5 <NA>   98 3146 220.4469
#              B2       B3     seB1        seB2      seB3           pB1
#1093 -0.02820835 1.008342 8.418428 0.002284547 0.0250506 6.826529e-137
#             pB2           pB3     RMSE      AIC    meanBIAS meanAbsBIAS
#1093 3.08687e-34 4.366256e-286 5.034898 27752.96 -0.01739793    17.23965
#     B3_test ierr notes
#1093      NA    0   fit
#temp[232,]
#     model                     model.formula  bh   WT SPCD nobs       B1
#1108     3 HT = bh + B1*(1.0-exp(B2*DBH))^B3 4.5 <NA>   22 6002 175.9518
#              B2       B3    seB1        seB2       seB3 pB1           pB2 pB3
#1108 -0.05108873 1.363574 2.71678 0.001651601 0.02027247   0 4.602538e-195   0
#         RMSE      AIC    meanBIAS meanAbsBIAS B3_test ierr notes
#1108 3.655722 46923.98 -0.03327762    10.42963      NA    0   fit
#temp[260,]
#     model                     model.formula  bh   WT SPCD nobs       B1
#1136     3 HT = bh + B1*(1.0-exp(B2*DBH))^B3 4.5 <NA>  212   48 386.0591
#              B2       B3     seB1        seB2      seB3         pB1        pB2
#1136 -0.01518699 1.200326 112.9414 0.007427882 0.1435811 0.001348465 0.04677135
#              pB3     RMSE      AIC    meanBIAS meanAbsBIAS B3_test ierr notes
#1136 1.037943e-10 3.478052 412.9648 0.003406134    13.44851      NA    0   fit

# model 4: HT = bh + exp(B1+B2/(DBH+1.0))
temp <- subset(dataHDfits, model == 4 & is.na(B1) == FALSE)
plot(x=c(0,50), y=c(0,250), type="n", xlab="DBH (inches)", ylab="Height (feet)",
       main="Model 4 predictions")
for (i in 1:nrow(temp))
{
   if (is.na(temp$B1[i]) == TRUE) { next }
   B1 <- temp$B1[i] 
   B2 <- temp$B2[i]
   #B3 <- temp$B3[i]
   B3 <- 1.0
   y <- 4.5 + exp(B1 + B2/(x+B3))
   lines(x, y, col=ifelse(temp$ierr[i] == 0, 1, 2))
   #if (any(x >= 50 & y > 160)) { cat("Problem: ", i, "\n") }
}
#temp[259,]
#     model                  model.formula  bh   WT SPCD nobs       B1        B2
#1574     4 HT = bh + exp(B1+B2/(DBH+1.0)) 4.5 <NA>  212   48 5.696383 -23.71334
#     B3       seB1    seB2 seB3          pB1          pB2 pB3     RMSE     AIC
#1574 NA 0.04960474 1.53779   NA 3.266464e-58 8.499826e-20  NA 4.471177 436.133
#      meanBIAS meanAbsBIAS B3_test ierr notes
#1574 -2.832084    16.57207      NA    0   fit
#temp[327,]  # only  5 observations and basically linear
#     model                  model.formula  bh   WT SPCD nobs       B1        B2
#1647     4 HT = bh + exp(B1+B2/(DBH+1.0)) 4.5 <NA> 8901    5 5.747548 -8.032588
#     B3     seB1     seB2 seB3        pB1        pB2 pB3     RMSE      AIC
#1647 NA 1.228303 2.696508   NA 0.01844058 0.05864932  NA 1.294388 20.92506
#         meanBIAS meanAbsBIAS B3_test ierr notes
#1647 -0.001871399   0.8508261      NA    4   fit

# model 5: HT = bh + exp(B1+B2/(DBH+B3))
temp <- subset(dataHDfits, model == 5 & is.na(B1) == FALSE)
plot(x=c(0,50), y=c(0,250), type="n", xlab="DBH (inches)", ylab="Height (feet)",
       main="Model 5 predictions")
for (i in 1:nrow(temp))
{
   if (is.na(temp$B1[i]) == TRUE) { next }
   B1 <- temp$B1[i] 
   B2 <- temp$B2[i]
   B3 <- temp$B3[i]
   y <- 4.5 + exp(B1 + B2/(x+B3))
   lines(x, y, col=ifelse(temp$ierr[i] == 0, 1, 2))
   #if (any(x >= 50 & y > 170)) { cat("Problem: ", i, "\n") }
   if (any(x == 1 & y > 40)) { cat("Problem: ", i, "\n") }
}

#temp[126,]      # no values above 22 inches so curves upwards 
#     model                 model.formula  bh   WT SPCD nobs      B1        B2
#1879     5 HT = bh + exp(B1+B2/(DBH+B3)) 4.5 <NA>  460   27 7.33614 -156.0034
#           B3     seB1    seB2     seB3       pB1       pB2       pB3     RMSE
#1879 36.46339 5.533898 494.018 69.78472 0.1974288 0.7548956 0.6061037 7.601002
#          AIC  meanBIAS meanAbsBIAS   B3_test ierr notes
#1879 240.5692 0.1629311    9.190617 0.6159645    4   fit
#temp[135,]     # no values above 22 inches so curves upwards 
#     model                 model.formula  bh   WT SPCD nobs       B1       B2
#1891     5 HT = bh + exp(B1+B2/(DBH+B3)) 4.5 <NA>  800  136 9.197784 -344.981
#           B3     seB1     seB2     seB3       pB1       pB2       pB3     RMSE
#1891 46.49677 5.649975 621.4877 49.09004 0.1059056 0.5797661 0.3452683 3.357051
#          AIC   meanBIAS meanAbsBIAS   B3_test ierr notes
#1891 875.5911 0.02288858    4.621838 0.3557079    4   fit
#temp[240,]    # no values above 18 inches so curves upwards 
#     model                 model.formula  bh   WT SPCD nobs       B1       B2
#2001     5 HT = bh + exp(B1+B2/(DBH+B3)) 4.5 <NA>   55  116 8.449895 -261.3651
#          B3     seB1     seB2     seB3        pB1       pB2     pB3     RMSE
#2001 39.0108 4.075885 371.0376 32.11261 0.04043258 0.4826225 0.22697 2.190576
#          AIC    meanBIAS meanAbsBIAS  B3_test ierr notes
#2001 680.9637 -0.04069661    3.561783 0.239027    4   fit
#temp[251,] 
#     model                 model.formula  bh   WT SPCD nobs     B1        B2
#2012     5 HT = bh + exp(B1+B2/(DBH+B3)) 4.5 <NA>  212   48 6.1677 -60.49752
#           B3      seB1     seB2     seB3          pB1          pB2
#2012 13.17488 0.1340289 11.80057 3.247697 1.727767e-39 6.026844e-06
#              pB3     RMSE      AIC   meanBIAS meanAbsBIAS      B3_test ierr
#2012 0.0001954606 3.467771 412.6806 -0.0560971    13.24192 0.0005047737    0
#temp[261,]    # no values above 15 inches so curves upwards
#     model                 model.formula  bh   WT SPCD nobs      B1        B2
#2023     5 HT = bh + exp(B1+B2/(DBH+B3)) 4.5 <NA>  997   34 7.70983 -198.7211
#           B3     seB1     seB2     seB3       pB1       pB2       pB3     RMSE
#2023 36.60801 4.820231 415.5078 44.92168 0.1198593 0.6358203 0.4213295 2.053515
#          AIC    meanBIAS meanAbsBIAS   B3_test ierr notes
#2023 202.2008 -0.01492701    3.421401 0.4339957    4   fit
#temp[256,]    # has a very high intercept (40 feet at 1 inche)
#     model                 model.formula  bh   WT SPCD nobs       B1        B2
#2018     5 HT = bh + exp(B1+B2/(DBH+B3)) 4.5 <NA>  511    9 5.673707 -44.65229
#           B3     seB1     seB2     seB3         pB1      pB2       pB3
#2018 22.38453 1.128609 89.38996 37.11823 0.002387008 0.635197 0.5685478
#         RMSE      AIC    meanBIAS meanAbsBIAS   B3_test ierr notes
#2018 2.881931 73.91545 0.009860314    8.209127 0.5854804    4   fit

# model 6: HT = bh + B1*(1.0-exp(B2*DBH^B3))
temp <- subset(dataHDfits, model == 6 & is.na(B1) == FALSE)
plot(x=c(0,50), y=c(0,250), type="n", xlab="DBH (inches)", ylab="Height (feet)",
       main="Model 6 predictions")
for (i in 1:nrow(temp))
{
   if (is.na(temp$B1[i]) == TRUE) { next }
   B1 <- temp$B1[i] 
   B2 <- temp$B2[i]
   B3 <- temp$B3[i]
   y <- 4.5 + B1*(1.0-exp(B2*x^B3))
   lines(x, y, col=ifelse(temp$ierr[i] == 0, 1, 2))
   #if (any(x >= 50 & y > 170)) { cat("Problem: ", i, "\n") }
   if (any(x == 8 & y > 70)) { cat("Problem: ", i, "\n") }
}
#temp[113,] 
#     model                     model.formula  bh   WT SPCD   nobs       B1
#2352     6 HT = bh + B1*(1.0-exp(B2*DBH^B3)) 4.5 <NA>  202 434130 255.3557
#              B2        B3     seB1         seB2        seB3 pB1 pB2 pB3
#2352 -0.02443909 0.9822992 1.612937 0.0001076462 0.001687112   0   0   0
#         RMSE     AIC     meanBIAS meanAbsBIAS B3_test ierr notes
#2352 4.788647 3625964 -0.005530593    13.87465      NA    0   fit
#temp[154,] 
#     model                     model.formula  bh   WT SPCD nobs       B1
#2407     6 HT = bh + B1*(1.0-exp(B2*DBH^B3)) 4.5 <NA>   98 3146 220.9705
#              B2     B3     seB1         seB2       seB3           pB1
#2407 -0.02748166 1.0043 9.775544 0.0008396557 0.01998279 5.929824e-105
#               pB2 pB3     RMSE      AIC    meanBIAS meanAbsBIAS B3_test ierr
#2407 1.913826e-202   0 5.034948 27753.03 -0.01611922    17.23962      NA    0
#temp[189,] 
#     model                     model.formula  bh   WT SPCD   nobs       B1
#2450     6 HT = bh + B1*(1.0-exp(B2*DBH^B3)) 4.5 <NA>  212   48 351.8923
#               B2       B3     seB1        seB2      seB3         pB1
#2450 -0.007381827 1.172584 100.4384 0.001377891 0.1064073 0.001050405
#              pB2          pB3     RMSE      AIC      meanBIAS meanAbsBIAS
#2450 2.775562e-06 2.276493e-14 3.474066 412.8548 -0.0002182522    13.41458
#     B3_test ierr notes
#2450      NA    0   fit
#temp[190,] 
#     model                     model.formula  bh   WT SPCD nobs       B1
#2451     6 HT = bh + B1*(1.0-exp(B2*DBH^B3)) 4.5 <NA>  137   63 104.6521
#             B2       B3     seB1        seB2      seB3         pB1         pB2
#2451 -0.0259889 1.187584 20.44702 0.003154288 0.1055128 3.42596e-06 1.91668e-11
#              pB3    RMSE      AIC     meanBIAS meanAbsBIAS B3_test ierr notes
#2451 2.010867e-16 1.65639 381.8363 0.0001943266    3.878685      NA    0   fit
#temp[226,]   # goes flat at about 6-inches (6 obs that are flat after about 6-inches)
#     model                     model.formula  bh   WT SPCD nobs       B1
#2524     6 HT = bh + B1*(1.0-exp(B2*DBH^B3)) 4.5 <NA>  857    6 73.73493
#              B2       B3     seB1      seB2     seB3         pB1       pB2
#2524 -0.02235353 2.697329 9.164084 0.0295404 1.205793 0.004009408 0.5042263
#           pB3     RMSE      AIC  meanBIAS meanAbsBIAS B3_test ierr notes
#2524 0.1112734 4.589604 50.93062 0.9903134    6.637872      NA    4   fit


par(mfrow=c(1,1))



   species <- 460
   tempHD <- subset(dataHD, SPCD == species)

   plot(tempHD$DIA, tempHD$HT, pch=c(19), xlim=c(0,50), ylim=c(0,250),
            xlab="DBH (inches)", ylab="Total Height (feet)",
            main=paste0("Oregon FIA HT-DBH data for ", species))

   B1 <- 7.33614
   B2 <- -156.0034
   B3 <- 36.46339
   x <- seq(0,50,1)
   #y <- 4.5 + exp(B1 + B2*x^B3)       # model 1 and 2
   #y <- 4.5 + B1*(1.0-exp(B2*x))^B3   # model 3
   y <- 4.5 + exp(B1 + B2/(x+B3))     # model 4 and 5
   #y <- 4.5 + B1*(1.0-exp(B2*x^B3))    # model 6
   lines(x, y, col=2)



# model 4 can be almost linear if no data indicating a leveling off (sigmoidal)
# model 5 can slope upwards if no data indicating a leveling off (sigmoidal)


#--------------------------------------------------------------------------------
# FIA HT-DBH data checking
#-------------------------------------------------------------------------------

species_list <- unique(dataHD$SPCD)
length(species_list)

#which(species_list == 902)    # 826
#which(species_list == 56)     # 259

i <- 22     # done through 22
   #print(i)
   species_list[i]
   tempHD <- subset(dataHD, SPCD == species_list[i])
   nrow(tempHD)

   min(tempHD$DIA)
   max(tempHD$DIA)
   min(tempHD$HT)
   max(tempHD$HT)

   plot(tempHD$DIA, tempHD$HT, pch=c(19), xlim=c(0,max(tempHD$DIA)), ylim=c(0,max(tempHD$HT)),
        xlab="DBH (inches)", ylab="HT (feet)", main=paste0("i = ", i, "  SPCD = ",species_list[i]))

tempX <- NULL

#out <- subset(tempHD, HT > 140, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)
#out <- subset(tempHD, DIA > 50, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)

out <- subset(tempHD, DIA > 38 & HT < 52, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
tempX <- rbind(tempX, out)
#out <- subset(tempHD, DIA > 28 & HT < 43, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)
#out <- subset(tempHD, DIA < 17 & HT > 116, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)
#out <- subset(tempHD, DIA < 21 & HT > 140, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)

#out <- subset(tempHD, DIA > 15 & DIA < 80 & HT < 36, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)
#out <- subset(tempHD, DIA > 34 & HT < 55, select=c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR","SUBP","TREE","SPCD","DIA","HT"))
#tempX <- rbind(tempX, out)

tempX
write.table(tempX, "clipboard", sep="\t", col.names=TRUE, row.names=FALSE)

nrow(tempX)
tempX

nrow(tempHD)
for (j in 1:nrow(tempX))
{
   tempHD <- tempHD[!(tempHD$STATECD == tempX$STATECD[j] & tempHD$UNITCD ==tempX$UNITCD[j] & 
                      tempHD$COUNTYCD == tempX$COUNTYCD[j] & tempHD$INVYR == tempX$INVYR[j] &
                      tempHD$PLOT == tempX$PLOT[j] & tempHD$SUBP == tempX$SUBP[j] & 
                      tempHD$TREE == tempX$TREE[j]) & tempHD$SPCD == tempX$SPCD[j], ]
}
nrow(tempHD)








#-------------------------------------------------------------------------------
# working 
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# done
#-------------------------------------------------------------------------------
