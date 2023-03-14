### Creating a set of costs for a PSA
### Written by Anneke Claypool
### Last updated 3/10/23

library(tidyverse)
### establish the baseline averages and ranges for each uncertain cost
### Baseline variables
discRate <- 0.03
startYear <- 2021
nYears <- 10
nPSAruns<- 1000
percentVaryPSA <- 0.25

set.seed(321)

iSaveCostinputs = 1

FILE_PATH_COSTS_QALYS<- ""
#Coverage Levels
EDvisits = 0.26 # The portion of people who have non-fatal overdose per year with misuse, OUD or HUD

#Costs included- 0 for healthcare perspective only, 1 for societal perspective
iProductivityCosts <- 1
iCriminalJusticeOn <- 1

### Costs in 2021 USD
cAnnualHealthUS <-  6520 
cOUDNoTxExcess <-  7598 
cOUDTxExcess <-  6078 

#MOUD 
cMethadone <-  7294 
cBuprenorphine <- 6650 
cNaltrexone <-  15674 

#Vary MOUD costs
minMethadone <- cMethadone*(1-percentVaryPSA)
maxMethadone <- cMethadone*(1+percentVaryPSA)
minBuprenorphine <- cBuprenorphine*(1-percentVaryPSA)
maxBuprenorphine <- cBuprenorphine*(1+percentVaryPSA)
minNaltrexone <- cNaltrexone*(1-percentVaryPSA)
maxNaltrexone <- cNaltrexone*(1+percentVaryPSA)

methadonePSA <- runif(nPSAruns, min = minMethadone, max = maxMethadone)
buprenorphinePSA <- runif(nPSAruns, min = minBuprenorphine, max = maxBuprenorphine)
naltrexonePSA <- runif(nPSAruns, min = minNaltrexone, max = maxNaltrexone)

#Overdose costs
cOverdoseNonfatal <- 4376.93	#Low = $3,341.52 , High = 4,792.67
cOverdoseFatal <- 3567.74 #Low = $2,757.61  , High = 4,562.35

#Productivity and consumption costs
cProductivity <- 1517261  #Based on assumption that average age 35-49
cConsumption <- 745408  #Based on assumption that average age 35-49

cNetProductivity <- iProductivityCosts*(cProductivity-cConsumption)

#Criminal Justice Costs- Based on assumption that average age 35-49
cCriminalJusticeHUDTx <- 1066.89
cCriminalJusticeOUDTx <- 566.49
cCriminalJusticeHUD <-4209.54
cCriminalJusticeOUD <-2616.3
cCriminalJusticeHUDRemission <-551.47
cCriminalJusticeOUDRemission <-342.74

#Intervention costs
cCM <- 3578 #Cost per patient in CM
minCM <- cCM*(1-percentVaryPSA)
maxCM <- cCM*(1+percentVaryPSA)
cTherapy <- 4540 #Cost per Patient in Therapy
minTherapy <- cTherapy*(1-percentVaryPSA)
maxTherapy <- cTherapy*(1+percentVaryPSA)
cHubSpoke <- 34558 #Cost per ***buprenorphine prescriber in Hub and Spoke network***
minHubSpoke <- cHubSpoke*(1-percentVaryPSA)
maxHubSpoke <- cHubSpoke*(1+percentVaryPSA)
cED <- 557.92  #Add cost of ED ## Update cost of ED ## Busch paper
minED <- cED*(1-percentVaryPSA)
maxED <- cED*(1+percentVaryPSA)
cTH <- 0


 
interventionRampUpYears <- 3 #Amount of time it takes to implement an intervention at the full coverage level


cmPSA <- runif(nPSAruns, min = minCM, max = maxCM)
therapyPSA <- runif(nPSAruns, min = minTherapy, max = maxTherapy)
hsPSA <- runif(nPSAruns, min = minHubSpoke, max = maxHubSpoke)
edPSA <- runif(nPSAruns, min = minED, max = maxED)
#thPSA <- runif(nPSAruns, min = minTH, max = maxTH)

### Run the basecase of discounted costs
### Discounting costs
discCosts <- data.frame(0:nYears)
discCosts$Time<- startYear:(startYear +nYears)
colnames(discCosts)[1]<- "yearNumber"
discCosts <- discCosts %>%
  mutate(dcAnnualHealthUS = cAnnualHealthUS/(1+discRate)^yearNumber,
         dcOUDNoTxExcess = cOUDNoTxExcess/(1+discRate)^yearNumber,
         dcOUDTxExcess = cOUDTxExcess/(1+discRate)^yearNumber,
         dcMethadone = cMethadone/(1+discRate)^yearNumber,
         dcBuprenorphine = cBuprenorphine/(1+discRate)^yearNumber,
         dcNaltrexone = cNaltrexone/(1+discRate)^yearNumber,
         dcOverdoseNonfatal = cOverdoseNonfatal/(1+discRate)^yearNumber,
         dcOverdoseFatal = cOverdoseFatal/(1+discRate)^yearNumber,
         dcNetProductivity = cNetProductivity/(1+discRate)^yearNumber,
         dcCM = cCM/(1+discRate)^yearNumber,
         dcTherapy = cTherapy/(1+discRate)^yearNumber,
         dcHubSpoke = cHubSpoke/(1+discRate)^yearNumber,
         dcED = cED/(1+discRate)^yearNumber,
         dcTH = cTH/(1+discRate)^yearNumber)

#Include reduced costs for linear ramp up time for the interventions
#calculating this here instead of in CEA script to reduce run time with for loop
discCosts$dcCM[discCosts$Time == startYear] <- 0
discCosts$dcTherapy[discCosts$Time == startYear] <- 0
discCosts$dcHubSpoke[discCosts$Time == startYear] <- 0
discCosts$dcED[discCosts$Time == startYear] <- 0
discCosts$dcTH[discCosts$Time == startYear] <- 0

for (i in 1:interventionRampUpYears) {
  discCosts$dcCM[discCosts$Time == startYear+i] <- discCosts$dcCM[discCosts$Time == startYear+i]*(i/interventionRampUpYears)
  discCosts$dcTherapy[discCosts$Time == startYear+i] <- discCosts$dcTherapy[discCosts$Time == startYear+i]*(i/interventionRampUpYears)
  discCosts$dcHubSpoke[discCosts$Time == startYear+i] <- discCosts$dcHubSpoke[discCosts$Time == startYear+i]*(i/interventionRampUpYears)
  discCosts$dcED[discCosts$Time == startYear+i] <- discCosts$dcED[discCosts$Time == startYear+i]*(i/interventionRampUpYears)
  discCosts$dcTH[discCosts$Time == startYear+i] <- discCosts$dcTH[discCosts$Time == startYear+i]*(i/interventionRampUpYears)
}

discCosts$nRun <- "Basecase"

### Create a dataframe for each run of the PSA- based on the earlier distributions 
discCostsPSA <- discCosts

#Specify 
for (j in 1:nPSAruns) { #i = 2
  cCM0 <- cmPSA[j]
  cTherapy0 <- therapyPSA[j]
  cHubSpoke0 <- hsPSA[j]
  cED0 <- edPSA[j]
  #cTH0 <- thPSA[j]
  cmethadone0 <- methadonePSA[j]
  cbup0 <- buprenorphinePSA[j]
  cnaltrexone0 <- naltrexonePSA[j] 

      ### Discounting costs
      discCostsRun <- NULL
      discCostsRun <- data.frame(0:nYears)
      discCostsRun$Time<- startYear:(startYear +nYears)
      colnames(discCostsRun)[1]<- "yearNumber"
      discCostsRun <- discCostsRun %>%
        mutate(dcAnnualHealthUS = cAnnualHealthUS/(1+discRate)^yearNumber,
               dcOUDNoTxExcess = cOUDNoTxExcess/(1+discRate)^yearNumber,
               dcOUDTxExcess = cOUDTxExcess/(1+discRate)^yearNumber,
               dcMethadone = cmethadone0/(1+discRate)^yearNumber,
               dcBuprenorphine = cbup0/(1+discRate)^yearNumber,
               dcNaltrexone = cnaltrexone0/(1+discRate)^yearNumber,
               dcOverdoseNonfatal = cOverdoseNonfatal/(1+discRate)^yearNumber,
               dcOverdoseFatal = cOverdoseFatal/(1+discRate)^yearNumber,
               dcNetProductivity = cNetProductivity/(1+discRate)^yearNumber,
               dcCM = cCM0/(1+discRate)^yearNumber,
               dcTherapy = cTherapy0/(1+discRate)^yearNumber,
               dcHubSpoke = cHubSpoke0/(1+discRate)^yearNumber,
               dcED = cED0/(1+discRate)^yearNumber,
               dcTH = cTH/(1+discRate)^yearNumber)
      
      #Include reduced costs for linear ramp up time for the interventions
      #calculating this here instead of in CEA script to reduce run time with for loop
      discCostsRun$dcCM[discCostsRun$Time == startYear] <- 0
      discCostsRun$dcTherapy[discCostsRun$Time == startYear] <- 0
      discCostsRun$dcHubSpoke[discCostsRun$Time == startYear] <- 0
      discCostsRun$dcED[discCostsRun$Time == startYear] <- 0
      discCostsRun$dcTH[discCostsRun$Time == startYear] <- 0
      
      for (i in 1:interventionRampUpYears) {
        discCostsRun$dcCM[discCostsRun$Time == startYear+i] <- discCostsRun$dcCM[discCostsRun$Time == startYear+i]*(i/interventionRampUpYears)
        discCostsRun$dcTherapy[discCostsRun$Time == startYear+i] <- discCostsRun$dcTherapy[discCostsRun$Time == startYear+i]*(i/interventionRampUpYears)
        discCostsRun$dcHubSpoke[discCostsRun$Time == startYear+i] <- discCostsRun$dcHubSpoke[discCostsRun$Time == startYear+i]*(i/interventionRampUpYears)
        discCostsRun$dcED[discCostsRun$Time == startYear+i] <- discCostsRun$dcED[discCostsRun$Time == startYear+i]*(i/interventionRampUpYears)
        discCostsRun$dcTH[discCostsRun$Time == startYear+i] <- discCostsRun$dcTH[discCostsRun$Time == startYear+i]*(i/interventionRampUpYears)
      }
    discCostsRun$nRun <- j
    discCostsPSA <- rbind(discCostsPSA, discCostsRun)
}


### save the sets of randomly selected costs (including discounting)
if(iSaveCostinputs){
  date.run <- Sys.Date()
  write.csv(discCostsPSA, file = paste0(FILE_PATH_COSTS_QALYS,"discCosts_PSA_",date.run,".csv"), row.names = F)
}


