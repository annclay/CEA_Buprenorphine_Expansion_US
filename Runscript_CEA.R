### Runscript for cost effectiveness analysis
### Uploads the model outputs, costs and QALYs
### By Anneke Claypool
### Last Updated 7/6/22

#Clear everything
rm(list = ls())

#### Upload libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(dampack)

#Specify saving and plots
coverage = 0.1
coveragePercent = coverage*100
iSaveResults <- 1
iPlotResults <- 0
iSaveCostQALYinputs <- 0

#Specify file paths for inputs and outputs
#Input file paths
FILE_PATH_QALY <- ""
FILE_PATH_SOURCE_MODEL_RUNS <- ""
FILE_PATH_COSTS_QALYS<- ""

#Output file paths
FILE_PATH_OUTPUTS <- ""
FILE_PATH_FIGURES <- ""
  
  
### Upload data
#QALYs
qualyStocks <- read.csv(paste0(FILE_PATH_QALY,'qalys_reformatted v2.csv'))
# SOURCE traces
statusQuo0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,'Baseline.csv'), header = F)
cm0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_Contingency Management.csv' ), header = F)
ther0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_Psychotherapy.csv'), header = F)
hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_Hub and Spoke.csv'), header = F)
port_cm_ther0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM and P.csv'), header = F)
port_cm_hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM and HS.csv'), header = F)
port_hs_ther0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_HS and P.csv'), header = F)
port_cm_ther_hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM P and HS.csv'), header = F)
ed0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init.csv'), header = F)
port_ed_cm0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init and CM.csv'), header = F)
port_ed_hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init and HS.csv'), header = F)
port_ed_ther0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init and P.csv'), header = F)
port_ed_cm_ther0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM and P.csv'), header = F)
port_ed_cm_hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM and HS.csv'), header = F)
port_ed_ther_hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init P and HS.csv'), header = F)
port_ed_cm_ther_hs0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM P and HS.csv'), header = F)
tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_Telehealth.csv'), header = F)
port_cm_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM and TH.csv' ), header = F)
port_ther_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_P and TH.csv'), header = F)
port_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_HS and TH.csv'), header = F)
port_ed_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init and TH.csv'), header = F)
port_cm_ther_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM P and TH.csv'), header = F)
port_cm_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM TH and HS.csv'), header = F)
port_ther_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_P TH and HS.csv'), header = F)
port_ed_cm_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM and TH.csv'), header = F)
port_ed_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init HS and TH.csv'), header = F)
port_ed_ther_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init P and TH.csv'), header = F)
port_ed_cm_ther_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM P and TH.csv'), header = F)
port_ed_cm_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM HS and TH.csv'), header = F)
port_ed_ther_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init HS P and TH.csv'), header = F)
port_cm_ther_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_CM P HS and TH.csv'), header = F)
port_ed_cm_ther_hs_tele0 <- read.csv(paste0(FILE_PATH_SOURCE_MODEL_RUNS,coveragePercent,'_ED Init CM P HS and TH.csv'), header = F)

### Baseline variables
discRate <- 0.03
startYear <- 2021
nYears <- 10

#Coverage Levels
EDvisits = 0.26 # The portion of people who have non-fatal overdose per year with misuse, OUD or HUD

#Costs included- 0 for healthcare perspective only, 1 for societal perspective
iProductivityCosts <- 1
iCriminalJusticeOn <- 1

### Costs in 2021 USD- From Melike's cost sheet
cAnnualHealthUS <-  6520 
cOUDNoTxExcess <-  7598 
cOUDTxExcess <-  6078 

#MOUD 
cMethadone <-  7294 
cBuprenorphine <- 6650 
cNaltrexone <-  15674 

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
cTherapy <- 4540 #Cost per Patient in Therapy
##cHubSpoke <- 7805 ## **Currently set as cost per patient, change to provider if necessary (in interventionCEA.R)**
cHubSpoke <- 34558 #Cost per ***buprenorphine prescriber in Hub and Spoke network***
cED <- 557.92  #Add cost of ED ## Update cost of ED ## Busch paper
cTH <- 0

interventionRampUpYears <- 3 #Amount of time it takes to implement an intervention at the full coverage level

### Health outcomes
qGainedSurvival <- 22.64#Based on age-weighted average

qNonDisHerUse<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. == "Nondisorderedheroin use"]              
qRxMisuse <- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx misuse no PY heroin"]                
qRxOUDnoHeroin <- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD no PY heroin no MOUD"]            
qRxOUDHeroin <- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD with PY heroin no MOUD"]         
qHUD<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="HUD no MOUD"]                            
qRxOUDBup<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD no heroin by MOUD[Bup]"]         
qRxOUDMMT<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD no heroin by MOUD[MMT]"]          
qRxOUDViv<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD no heroin by MOUD[Viv]"]         
qRxOUDHeroinBup<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD with heroin by MOUD[Bup]"]        
qRxOUDHeroinMMT<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD with heroin by MOUD[MMT]"]       
qRxOUDHeroinViv<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD with heroin by MOUD[Viv]"]        
qHUDBup <- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="HUD by MOUD[Bup]"]                      
qHUDMMT<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="HUD by MOUD[MMT]"]                       
qHUDViv<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="HUD by MOUD[Viv]"]                      
qRxOUDRemission <- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD no heroin in remission"]          
qRxOUDStableRemission<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD no heroin in stable remission"]  
qRxOUDHeroinRemission<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD with heroin in remission"]        
qRxOUDHeroinStableRemission<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="Rx OUD with heroin in stable remission"]
qHUDRemission<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="HUD in remission"]                       
qHUDStableRemission<- qualyStocks$Utility.Estimate[qualyStocks$Stock..Vensimname. =="HUD in stable remission"]


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

#Criminal Justice Costs
  discCriminalCosts <- data.frame(0:nYears)
  discCriminalCosts$Time<- startYear:(startYear +nYears)
  colnames(discCriminalCosts)[1]<- "yearNumber"
  discCriminalCosts <- discCriminalCosts %>%
    mutate(dcCriminalJusticeHUDTx = cCriminalJusticeHUDTx/(1+discRate)^yearNumber,
           dcCriminalJusticeOUDTx = cCriminalJusticeOUDTx/(1+discRate)^yearNumber,
           dcCriminalJusticeHUD = cCriminalJusticeHUD/(1+discRate)^yearNumber,
           dcCriminalJusticeOUD = cCriminalJusticeOUD/(1+discRate)^yearNumber,
           dcCriminalJusticeHUDRemission = cCriminalJusticeHUDRemission/(1+discRate)^yearNumber,
           dcCriminalJusticeOUDRemission = cCriminalJusticeOUDRemission/(1+discRate)^yearNumber)

# ### Discounting QALYs
discQALYs <- data.frame(0:nYears)
discQALYs$Time<- startYear:(startYear +nYears)
colnames(discQALYs)[1]<- "yearNumber"
discQALYs <- discQALYs %>%
  mutate(dqNonDisHerUse = qNonDisHerUse/(1+discRate)^yearNumber,
         dqRxMisuse = qRxMisuse/(1+discRate)^yearNumber,
         dqRxOUDnoHeroin = qRxOUDnoHeroin/(1+discRate)^yearNumber,
         dqRxOUDHeroin = qRxOUDHeroin/(1+discRate)^yearNumber,
         dqHUD= qHUD/(1+discRate)^yearNumber,
         dqRxOUDBup= qRxOUDBup/(1+discRate)^yearNumber,
         dqRxOUDMMT = qRxOUDMMT/(1+discRate)^yearNumber,
         dqRxOUDViv = qRxOUDViv/(1+discRate)^yearNumber,
         dqRxOUDHeroinBup = qRxOUDHeroinBup/(1+discRate)^yearNumber,
         dqRxOUDHeroinMMT = qRxOUDHeroinMMT/(1+discRate)^yearNumber,
         dqRxOUDHeroinViv = qRxOUDHeroinViv/(1+discRate)^yearNumber,
         dqHUDBup = qHUDBup/(1+discRate)^yearNumber,
         dqHUDMMT = qHUDMMT/(1+discRate)^yearNumber,
         dqHUDViv = qHUDViv/(1+discRate)^yearNumber,
         dqRxOUDRemission = qRxOUDRemission/(1+discRate)^yearNumber,
         dqRxOUDStableRemission = qRxOUDStableRemission/(1+discRate)^yearNumber,
         dqRxOUDHeroinRemission = qRxOUDHeroinRemission/(1+discRate)^yearNumber,
         dqRxOUDHeroinStableRemission = qRxOUDHeroinStableRemission/(1+discRate)^yearNumber,
         dqHUDRemission = qHUDRemission/(1+discRate)^yearNumber,
         dqHUDStableRemission = qHUDStableRemission/(1+discRate)^yearNumber,
         dqGainedSurvival = qGainedSurvival/(1+discRate)^yearNumber)

###Save costs, criminal justice costs, and QALYs for sensitivity analysis runs
if(iSaveCostQALYinputs){
  date.run <- Sys.Date()
  write.csv(discCosts, file = paste0(FILE_PATH_COSTS_QALYS,"discCosts",date.run,".csv"))
  write.csv(discCriminalCosts, file = paste0(FILE_PATH_COSTS_QALYS,"discCriminalCosts",date.run,".csv"))
  write.csv(discQALYs, file = paste0(FILE_PATH_COSTS_QALYS,"discQALYs",date.run,".csv"))
}

#Run the CEA and get the outputs for all of the interventions
source("runCEA.R")

#statusQuo0<-transpose(statusQuo0)
l_results_sq <- runCEA(interventionModelOutput0 = statusQuo0,
                       interventionCoverage = c(0, 0, 0, 0, 0),#interventionCoverage should be in format: [cm, ther, hs, ed, tele]
                       interventionName = "Status Quo",
                       abbrevName = "SQ",
                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_cm <- runCEA(interventionModelOutput0 = cm0,
                       interventionCoverage = c(coverage, 0, 0, 0, 0),#format: [cm, ther, hs, ed, tele]
                       interventionName = "Contingency Management",
                       abbrevName = "CM",
                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_ther <- runCEA(interventionModelOutput0 = ther0,
                       interventionCoverage = c(0, coverage, 0, 0, 0),#format: [cm, ther, hs, ed, tele]
                       interventionName = "Psychotherapy",
                       abbrevName = "P",
                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_hs <- runCEA(interventionModelOutput0 = hs0,
                         interventionCoverage = c(0, 0, coverage, 0, 0),#format: [cm, ther, hs, ed, tele]
                         interventionName = "Hub and Spoke",
                         abbrevName = "HS",
                         discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_ed <- runCEA(interventionModelOutput0 = ed0,
                       interventionCoverage = c(0, 0, 0, coverage, 0),#format: [cm, ther, hs, ed, tele]
                       interventionName = "ED Initiation",
                       abbrevName = "ED",
                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_tele <- runCEA(interventionModelOutput0 = tele0,
                       interventionCoverage = c(0, 0, 0, 0, coverage),#format: [cm, ther, hs, ed, tele]
                       interventionName = "Telehealth",
                       abbrevName = "TH",
                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_ther <- runCEA(interventionModelOutput0 = port_cm_ther0,
                       interventionCoverage = c(coverage, coverage, 0, 0, 0),#format: [cm, ther, hs, ed, tele]
                       interventionName = "Contingency Management + Psychotherapy",
                       abbrevName = "CM+P",
                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_hs <- runCEA(interventionModelOutput0 = port_cm_hs0,
                        interventionCoverage = c(coverage, 0, coverage, 0, 0),#format: [cm, ther, hs, ed, tele]
                        interventionName = "Contingency Management + Hub and Spoke",
                        abbrevName = "CM+HS",
                        discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_hs_ther <- runCEA(interventionModelOutput0 = port_hs_ther0,
                               interventionCoverage = c(0, coverage, coverage, 0, 0),#format: [cm, ther, hs, ed, tele]
                               interventionName = "Psychotherapy + Hub and Spoke",
                               abbrevName = "P+HS",
                               discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_ther_hs <- runCEA(interventionModelOutput0 = port_cm_ther_hs0,
                                 interventionCoverage = c(coverage, coverage, coverage, 0, 0),#format: [cm, ther, hs, ed, tele]
                                 interventionName = "Contingency Management + Psychotherapy + Hub and Spoke",
                                 abbrevName = "CM+P+HS",
                                 discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm <- runCEA(interventionModelOutput0 = port_ed_cm0,
                                    interventionCoverage = c(coverage, 0, 0, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                    interventionName = "Contingency Management + ED Initiation",
                                    abbrevName = "CM+ED",
                                    discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_port_ed_hs <- runCEA(interventionModelOutput0 = port_ed_hs0,
                                    interventionCoverage = c(0, 0, coverage, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                    interventionName = "Hub and Spoke + ED Initiation",
                                    abbrevName = "HS+ED",
                                    discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_ther <- runCEA(interventionModelOutput0 = port_ed_ther0,
                                    interventionCoverage = c(0, coverage, 0, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                    interventionName = "Psychotherapy + ED Initiation",
                                    abbrevName = "P+ED",
                                    discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_ther <- runCEA(interventionModelOutput0 = port_ed_cm_ther0,
                                      interventionCoverage = c(coverage, coverage, 0, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                      interventionName = "Contingency Management + Psychotherapy + ED Initiation",
                                      abbrevName = "CM+P+ED",
                                      discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_hs <- runCEA(interventionModelOutput0 = port_ed_cm_hs0,
                                    interventionCoverage = c(coverage, 0, coverage, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                    interventionName = "Contingency Management + Hub and Spoke + ED Initiation",
                                    abbrevName = "CM+HS+ED",
                                    discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_ther_hs <- runCEA(interventionModelOutput0 = port_ed_ther_hs0,
                                  interventionCoverage = c(0, coverage, coverage, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                  interventionName = "Psychotherapy + Hub and Spoke + ED Initiation",
                                  abbrevName = "P+HS+ED",
                                  discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_ther_hs <- runCEA(interventionModelOutput0 = port_ed_cm_ther_hs0,
                                    interventionCoverage = c(coverage, coverage, coverage, coverage, 0),#format: [cm, ther, hs, ed, tele]
                                    interventionName = "Contingency Management + Psychotherapy + Hub and Spoke + ED Initiation",
                                    abbrevName = "CM+P+HS+ED",
                                    discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_tele<- runCEA(interventionModelOutput0 = port_cm_tele0,
                                       interventionCoverage = c(coverage, 0, 0, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Telehealth",
                                       abbrevName = "CM+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ther_tele <- runCEA(interventionModelOutput0 = port_ther_tele0,
                                       interventionCoverage = c(0, coverage, 0, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Psychotherapy + Telehealth",
                                       abbrevName = "P+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_hs_tele <- runCEA(interventionModelOutput0 = port_hs_tele0,
                                       interventionCoverage = c(0, 0, coverage, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Hub and Spoke + Telehealth",
                                       abbrevName = "HS+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_tele <- runCEA(interventionModelOutput0 = port_ed_tele0,
                                       interventionCoverage = c(0, 0, 0, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "ED Initiation + Telehealth",
                                       abbrevName = "ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_ther_tele <- runCEA(interventionModelOutput0 = port_cm_ther_tele0,
                                       interventionCoverage = c(coverage, coverage, 0, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Psychotherapy + Telehealth",
                                       abbrevName = "CM+P+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_hs_tele<- runCEA(interventionModelOutput0 = port_cm_hs_tele0,
                                       interventionCoverage = c(coverage, 0, coverage, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Hub and Spoke + Telehealth",
                                       abbrevName = "CM+HS+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ther_hs_tele<- runCEA(interventionModelOutput0 = port_ther_hs_tele0,
                                       interventionCoverage = c(0, coverage, coverage, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Psychotherapy + Hub and Spoke + Telehealth",
                                       abbrevName = "P+HS+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_tele <- runCEA(interventionModelOutput0 = port_ed_cm_tele0,
                                       interventionCoverage = c(coverage, 0, 0, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + ED Initiation + Telehealth",
                                       abbrevName = "CM+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_hs_tele <- runCEA(interventionModelOutput0 = port_ed_hs_tele0,
                                       interventionCoverage = c(0, 0, coverage, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Hub and Spoke + ED Initiation + Telehealth",
                                       abbrevName = "HS+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_ther_tele <- runCEA(interventionModelOutput0 = port_ed_ther_tele0,
                                       interventionCoverage = c(0, coverage, 0, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Psychotherapy + ED Initiation +Telehealth",
                                       abbrevName = "P+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_ther_tele<- runCEA(interventionModelOutput0 = port_ed_cm_ther_tele0,
                                       interventionCoverage = c(coverage, coverage, 0, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Psychotherapy + ED Initiation + Telehealth",
                                       abbrevName = "CM+P+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_hs_tele <- runCEA(interventionModelOutput0 = port_ed_cm_hs_tele0,
                                       interventionCoverage = c(coverage, 0, coverage, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Hub and Spoke + ED Initiation +Telehealth",
                                       abbrevName = "CM+HS+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_ther_hs_tele <- runCEA(interventionModelOutput0 = port_ed_ther_hs_tele0,
                                       interventionCoverage = c(0, coverage, coverage, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Psychotherapy + Hub and Spoke + ED Initiation + Telehealth",
                                       abbrevName = "P+HS+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_cm_ther_hs_tele <- runCEA(interventionModelOutput0 = port_cm_ther_hs_tele0,
                                       interventionCoverage = c(coverage, coverage, coverage, 0, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Psychotherapy + Hub and Spoke + Telehealth",
                                       abbrevName = "CM+P+HS+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)

l_results_port_ed_cm_ther_hs_tele <- runCEA(interventionModelOutput0 = port_ed_cm_ther_hs_tele0,
                                       interventionCoverage = c(coverage, coverage, coverage, coverage, coverage),#format: [cm, ther, hs, ed, tele]
                                       interventionName = "Contingency Management + Psychotherapy + Hub and Spoke + ED Initiation + Telehealth",
                                       abbrevName = "CM+P+HS+ED+TH",
                                       discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear)


### List of names
# l_results_sq
# l_results_cm
# l_results_ther
# l_results_hs
# l_results_ed
# # l_results_tele
# l_results_port_cm_ther
# l_results_port_cm_hs
# l_results_port_hs_ther
# l_results_port_cm_ther_hs
# l_results_port_ed_cm
# l_results_port_port_ed_hs
# l_results_port_ed_ther
# l_results_port_ed_cm_ther
# l_results_port_ed_cm_hs
# l_results_port_ed_ther_hs 
# l_results_port_ed_cm_ther_hs
# l_results_port_cm_tele
# l_results_port_ther_tele
# l_results_port_hs_tele
# l_results_port_ed_tele
# l_results_port_cm_ther_tele
# l_results_port_cm_hs_tele
# l_results_port_ther_hs_tele
# l_results_port_ed_cm_tele
# l_results_port_ed_hs_tele
# l_results_port_ed_ther_tele
# l_results_port_ed_cm_ther_tele
# l_results_port_ed_cm_hs_tele
# l_results_port_ed_ther_hs_tele
# l_results_port_cm_ther_hs_tele
# l_results_port_ed_cm_ther_hs_tele

### Aggreate the outcomes among interventions

cumulOverdose <- rbind(l_results_sq$overdosesTime, l_results_cm$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_ther$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_ed$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_ther$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_hs_ther$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_ther_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_port_ed_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_ther$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_ther$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_ther_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_ther_hs$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ther_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_ther_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ther_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_ther_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_ther_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_ther_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_cm_ther_hs_tele$overdosesTime)
cumulOverdose <- rbind(cumulOverdose,l_results_port_ed_cm_ther_hs_tele$overdosesTime)

cumulOverdose <- cumulOverdose%>%
  group_by(Time)%>%
  #group_by(Intervention) %>%
  #mutate(total.overdose.deaths = c(NA,diff(Cumulative.overdose.deaths)))%>%
  mutate(Overdose.deaths.Averted = Total.overdose.deaths[Intervention == "Status Quo"]-Total.overdose.deaths,
         Nonfatal.overdose.averted = Total.nonfatal.overdoses[Intervention == "Status Quo"]-Total.nonfatal.overdoses,
         Percent.Overdose.deaths.Averted = (Total.overdose.deaths[Intervention == "Status Quo"]-Total.overdose.deaths)/Total.overdose.deaths[Intervention == "Status Quo"])%>%
  
  ungroup()%>%
  data.table()

OverdoseTable2 <- cumulOverdose %>%
  filter(Time == max(Time))%>%
  mutate(Cumulative.overdose.deaths.averted = Cumulative.overdose.deaths[Intervention == "Status Quo"]-Cumulative.overdose.deaths)%>%
  select(Intervention, Cumulative.nonfatal.overdoses, Cumulative.overdose.deaths, Cumulative.overdose.deaths.averted)
colnames(OverdoseTable2)<- c("Intervention", 
                             paste0("Total Nonfatal Overdoses 1999-",max(cumulOverdose$Time)),
                             paste0("Total Fatal Overdoses 1999-",max(cumulOverdose$Time)),
                             "Total Overdose Deaths Averted")
 
OverdoseTable2021_end <- cumulOverdose %>%
  group_by(Intervention)%>%
  mutate(Cumulative.overdose.deaths.time.period = sum(Total.overdose.deaths),
         Cumulative.nonfatal.overdose.time.period = sum(Total.nonfatal.overdoses))%>%
  ungroup()%>%
  filter(Time == max(Time))%>%
  mutate(Cumulative.overdose.deaths.averted = Cumulative.overdose.deaths[Intervention == "Status Quo"]-Cumulative.overdose.deaths,
         Cumulative.overdose.deaths.averted.time.period = Cumulative.overdose.deaths.time.period[Intervention == "Status Quo"]-Cumulative.overdose.deaths.time.period,
         Cumulative.nonfatal.overdose.averted.time.period = Cumulative.nonfatal.overdose.time.period[Intervention == "Status Quo"]-Cumulative.nonfatal.overdose.time.period)%>%
  select(Intervention, Cumulative.nonfatal.overdose.time.period, Cumulative.nonfatal.overdose.averted.time.period,
         Cumulative.overdose.deaths.time.period, Cumulative.overdose.deaths.averted)
colnames(OverdoseTable2021_end)<- c("Intervention", 
                             paste0("Total Nonfatal Overdoses ",min(cumulOverdose$Time),"-",max(cumulOverdose$Time)),
                             "Total Nonfatal Overdoses Averted",
                             paste0("Total Fatal Overdoses ",min(cumulOverdose$Time),"-",max(cumulOverdose$Time)),
                             "Total Overdose Deaths Averted")

#CEA Table- QALYs, Costs, NMB, ICERs
CEAtable <- rbind(l_results_sq$CEAtable, l_results_cm$CEAtable)
CEAtable <- rbind(CEAtable,l_results_ther$CEAtable)
CEAtable <- rbind(CEAtable,l_results_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_ed$CEAtable)
CEAtable <- rbind(CEAtable,l_results_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_ther$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_hs_ther$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_ther_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_port_ed_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_ther$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_ther$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_ther_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_ther_hs$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ther_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_ther_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ther_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_ther_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_ther_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_ther_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_cm_ther_hs_tele$CEAtable)
CEAtable <- rbind(CEAtable,l_results_port_ed_cm_ther_hs_tele$CEAtable)
                  
CEAtable <- CEAtable %>%
  mutate(incrementalCost = totalcost - totalcost[Intervention == "Status Quo"],
         incrementalQALY = totalQALY - totalQALY[Intervention == "Status Quo"],
         ICERvsStatusQuo = incrementalCost/incrementalQALY)
icers <- calculate_icers(cost = CEAtable$totalcost,
                effect = CEAtable$totalQALY,
                strategies = CEAtable$Intervention)


### Add QALYS to overdose/health outcomes table
qalyGained <- CEAtable %>%
  select(Intervention, InterventionAbbrev, incrementalQALY)

healthOutcomesTable <- left_join(OverdoseTable2021_end,qalyGained)%>%
  select("Intervention","InterventionAbbrev","Total Nonfatal Overdoses 2021-2032",
         "Total Nonfatal Overdoses Averted","Total Fatal Overdoses 2021-2032",   
         "Total Overdose Deaths Averted",                 
         "incrementalQALY" )

abbrevNameKey <- cumulOverdose %>%
  select(Intervention, abbrevName) %>%
  unique() %>%
  data.frame()

colnames(abbrevNameKey)[1]<-"Strategy"

colnames(CEAtable)[1]<- "Strategy"
colnames(CEAtable)[4]<- "abbrevName"
icers<- left_join(icers, abbrevNameKey)

icersCEAtable <- left_join(icers, CEAtable)%>%
  select(abbrevName, totalQALY, incrementalQALY, Cost, incrementalCost, ICER, Status)

#Create Figures
if(iPlotResults){
source('plotOverdose.R')
plotOverdose(cumulOverdose, coveragePercent)
source('plotCEAplane.R')
plotCEAplane(icers, coveragePercent)
}

#save the results
if(iSaveResults){
  date.run <- Sys.Date()
  write.csv(CEAtable, file = paste0(FILE_PATH_OUTPUTS,"CEAtable",coveragePercent,"cov",date.run,"healthcareOnly",".csv"))
  #write.csv(cumulOverdose, file = paste0(FILE_PATH_OUTPUTS,"Results/OverdosetableYearly",date.run,".csv"))
  #write.csv(OverdoseTable2021_end, file = paste0(FILE_PATH_OUTPUTS,"OverdoseTable2.",coveragePercent,"cov",date.run,".csv"))
  write.csv(healthOutcomesTable, file = paste0(FILE_PATH_OUTPUTS,"HealthOutcomesTable2.",coveragePercent,"cov",date.run,"healthcareOnly",".csv"))
  write.csv(icers, file = paste0(FILE_PATH_OUTPUTS,"ICERsTable.",coveragePercent,"cov",date.run,"healthcareOnly",".csv"))
  write.csv(icersCEAtable, file = paste0(FILE_PATH_OUTPUTS,"ICERsCEATable.",coveragePercent,"cov",date.run,"healthcareOnly",".csv"))
}
