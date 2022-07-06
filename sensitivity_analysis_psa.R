### Sensitivity Analysis- PSA
### By Anneke Claypool
### Written May 26, 2022

#Clear everything
rm(list = ls())

#### Upload libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(dampack)
library(tidyverse)

#Include constants
### Baseline variables
SaveOutputs = 0
plotSave = 0
discRate <- 0.03
startYear <- 2021
nYears <- 10
wtp = 100000
#Coverage Levels
coverage = 0.1 #0.2 #0.05 #0.1
coveragePercent <- coverage*100


#Costs included
iReRunAnalysis <- 0 # 1= rerun the sensivitiy analysis. This could take time. 0= read in sensitivity analsyis results
iProductivityCosts <- 1
iCriminalJusticeOn <- 1
#Baseline effectiveness
Bup.av.duration.baseline = 0.61 #years
#Amount of time it takes to implement an intervention at the full coverage level
interventionRampUpYears <- 3

###Upload Data
#Upload previously calculated discounted costs and QALYs- these were generated in Runscript_CEA_5_31_22
discCosts <- read.csv('C:/Users/ai219/Documents/GitHub/CEA-buprenorphine-for-OUD/Discounted_Costs_QALYs/discCosts2022-06-02.csv', header = T)
discCriminalCosts <- read.csv('C:/Users/ai219/Documents/GitHub/CEA-buprenorphine-for-OUD/Discounted_Costs_QALYs/discCriminalCosts2022-06-02.csv', header = T)
discQALYs <- read.csv('C:/Users/ai219/Documents/GitHub/CEA-buprenorphine-for-OUD/Discounted_Costs_QALYs/discQALYs2022-06-02.csv', header = T)

###Upload basecase CEA run ### Only needed for status quo (coverage level doesn't matter)
basecaseCEATable <- read.csv("C:/Users/ai219/Documents/GitHub/CEA-buprenorphine-for-OUD/Results/CEAtable10cov2022-06-02.csv")

FILE_PATH_RESULTS <- "C:/Users/ai219/Dropbox (Partners HealthCare)/Documents/Opioids/R Studio/Bup_CEA/Results/"

if(iReRunAnalysis){
###Upload raw sensitivity analysis outputs from source and run the CEA
filePathSensitivityAnalysis <- "C:/Users/ai219/Dropbox (Partners HealthCare)/Documents/Opioids/Sensitivity Analysis/"
# tbl <-
#   list.files(path = filePathSensitivityAnalysis,
#              pattern = "export_sensitivity_5_cm", 
#              full.names = T) %>% 
#   map_df(~read_csv(., col_types = cols(.default = "c"))) 

#Read each of
df.list = list.files(path = filePathSensitivityAnalysis,
                  pattern = paste0("clean_export_sensitivity_",coveragePercent))
#df.list <- paste0(filePathSensitivityAnalysis,df.list[1:3])#Testing sample
df.list <- paste0(filePathSensitivityAnalysis,df.list)

#library(plyr)
read_csv_filename <- function(filename){#filename <- df.list[2]
  sensInput0 <- read.csv(filename, header = T)
  editedFileName <- gsub(pattern = "C:/Users/ai219/Dropbox \\(Partners HealthCare\\)/Documents/Opioids/Sensitivity Analysis/clean_export_sensitivity_","",x = filename)
  editedFileName <- gsub(pattern = "_control.csv","",x = editedFileName)
  sensInput0$fileName <- as.character(editedFileName)
  if(!("Policy.change.Tx.seeking.rate.Rx.OUD.no.H.total.Bup."%in% colnames(sensInput0))){
    sensInput0$Policy.change.Tx.seeking.rate.Rx.OUD.no.H.total.Bup.<-0
  }
  sensInput<- sensInput0 %>%
                select("Simulation", "Time","Policy.change.Bup.effective.capacity.decay.constant" ,
                     "Policy.change.Bup.providers","Policy.change.Tx.average.duration.Bup.",
                     "Policy.change.Tx.seeking.rate.Rx.OUD.no.H.total.Bup.",
                     "Policy.rampup.duration","Cumulative.nonfatal.overdoses",
                     "Cumulative.overdose.deaths","Cumulative.UD.person.years",
                     "HUD.by.MOUD.Bup.","HUD.by.MOUD.MMT.","HUD.by.MOUD.Viv.",
                     "HUD.in.remission","HUD.in.stable.remission","HUD.no.MOUD",
                     "Nondisordered.heroin.use","Rx.misuse.no.PY.heroin",
                     "Rx.OUD.no.heroin.by.MOUD.Bup.", "Rx.OUD.no.heroin.by.MOUD.MMT." ,
                     "Rx.OUD.no.heroin.by.MOUD.Viv.","Rx.OUD.no.heroin.in.remission",
                     "Rx.OUD.no.heroin.in.stable.remission","Rx.OUD.no.PY.heroin.no.MOUD",
                     "Rx.OUD.with.heroin.by.MOUD.Bup.","Rx.OUD.with.heroin.by.MOUD.MMT.",
                     "Rx.OUD.with.heroin.by.MOUD.Viv.","Rx.OUD.with.heroin.in.remission" ,
                     "Rx.OUD.with.heroin.in.stable.remission","Rx.OUD.with.PY.heroin.no.MOUD",
                     "Bup.providers","Nonfatal.ODs.HUD","Nonfatal.ODs.NDHU",
                     "Nonfatal.ODs.Rx.misuse","Nonfatal.ODs.Rx.OUD.no.H","Nonfatal.ODs.Rx.OUD.with.H",
                     "Rx.OUD.by.MOUD.Bup.","Rx.OUD.by.MOUD.MMT.","Rx.OUD.by.MOUD.Viv.",
                     "Total.annual.Tx.receipt.by.MOUD.Bup.","Total.by.MOUD.Bup.",
                     "Total.nonfatal.overdoses","Total.overdose.deaths", "Total.Rx.misuse.initiation",
                     "Total.with.UD","Tx.engagement.total.Bup.",
                     "Tx.seeking.rate.HUD.Bup.","Tx.seeking.rate.HUD.MMT.",
                     "Tx.seeking.rate.HUD.Viv.","Tx.seeking.rate.Rx.OUD.no.H.Bup.",
                     "Tx.seeking.rate.Rx.OUD.no.H.MMT.","Tx.seeking.rate.Rx.OUD.no.H.Viv." ,
                     "Tx.seeking.rate.Rx.OUD.no.H.total.net.Bup.","Tx.seeking.rate.Rx.OUD.no.H.total.net.MMT.",
                     "Tx.seeking.rate.Rx.OUD.no.H.total.net.Viv." ,"Tx.seeking.rate.Rx.OUD.with.H.Bup.",
                     "Tx.seeking.rate.Rx.OUD.with.H.MMT.","Tx.seeking.rate.Rx.OUD.with.H.Viv.",
                     "FractionNFOD_HUD","FractionNFOD_RxOUDnoH","FractionNFOD_RxOUDwH",
                     "fractionNFOD_NDHU","FractionNFOD_RxMisuse","fileName")%>%
    data.frame()
  sensInput
  #sensInput0$interventionCoverage <- coverage
}

# import.list <- ldply(df.list, read_csv_filename)
# 
# startRead = 0
# l.import <- as.list()
# 
# modelOutputSensitivity0<-bind_rows(import.list)

sensResultsAllInterventions <- data.frame(matrix(ncol = 64, nrow = 0))

for (z in df.list) {
  results_intervention_SA <- read_csv_filename(z)
  sensResultsAllInterventions <- rbind(sensResultsAllInterventions,results_intervention_SA)
}

### Function to format the sensitivity outputs
formatSOURCEsensitivityOutput <- function(modelOutput0){ 
  modelOutput<- as.data.frame(modelOutput0)
  modelOutput <- modelOutput %>%
    mutate(Total.by.MOUD.Bup. = Rx.OUD.no.heroin.by.MOUD.Bup. + Rx.OUD.with.heroin.by.MOUD.Bup. +
             HUD.by.MOUD.MMT.,
           Total.by.MOUD.MMT. = Rx.OUD.no.heroin.by.MOUD.MMT. + Rx.OUD.with.heroin.by.MOUD.MMT. +
             HUD.by.MOUD.MMT.,
           Total.by.MOUD.Viv. = Rx.OUD.no.heroin.by.MOUD.Viv.+ Rx.OUD.with.heroin.by.MOUD.Viv. +
             HUD.by.MOUD.Viv.,
           Total.population = Rx.misuse.no.PY.heroin+                
             Rx.OUD.no.PY.heroin.no.MOUD+           
             Rx.OUD.no.heroin.by.MOUD.Bup.+         
             Rx.OUD.no.heroin.by.MOUD.MMT.+         
             Rx.OUD.no.heroin.by.MOUD.Viv.+         
             Rx.OUD.no.heroin.in.remission+         
             Rx.OUD.no.heroin.in.stable.remission+  
             Rx.OUD.with.PY.heroin.no.MOUD+         
             Rx.OUD.with.heroin.by.MOUD.Bup.+       
             Rx.OUD.with.heroin.by.MOUD.MMT.+       
             Rx.OUD.with.heroin.by.MOUD.Viv.+       
             Rx.OUD.with.heroin.in.remission+       
             Rx.OUD.with.heroin.in.stable.remission+
             HUD.no.MOUD+                          
             HUD.by.MOUD.Bup.+                      
             HUD.by.MOUD.MMT.+                      
             HUD.by.MOUD.Viv.+                      
             HUD.in.remission+                      
             HUD.in.stable.remission+             
             Nondisordered.heroin.use) %>%
    group_by(Simulation,fileName)%>%
    mutate(Total.nonfatal.overdoses = c(Cumulative.nonfatal.overdoses[1],diff(Cumulative.nonfatal.overdoses)) , #Calculation method for total and cumulative are different. Changing it so it is consistent
           Total.overdose.deaths = c(Cumulative.overdose.deaths[1],diff(Cumulative.overdose.deaths)))%>%
    ungroup()%>%
    mutate(Bup.duration = (1+Policy.change.Tx.average.duration.Bup.)*Bup.av.duration.baseline)
  
  return(modelOutput)
}

interventionModelOutputAll <- formatSOURCEsensitivityOutput(sensResultsAllInterventions)

###date.run <- Sys.Date()
###write.csv(interventionModelOutput, file = paste0(filePathSensitivityAnalysis,"formattedSensOutput",coveragePercent,"coverage",date.run,".csv"))

#}

### Prepare to run the CEA for all simulation runs and all interventions
  list.Interventions<-unique(interventionModelOutputAll$fileName)
  source("interventionCEA.R")
  
  #Specify the dataframe to bind all results to
  sensResultsAllInterventions <- data.frame(matrix(ncol = 6, nrow = 0))
  x <- c("Simulation", "CoverageAllInt", "Intervention","totalCosts","totalQALYS","totalODdeaths")
  colnames(sensResultsAllInterventions) <- x

for(i in list.Interventions){ #i<-list.Interventions[2]
  print(i)
  # Specify dataframe for saving all sensitivity runs
  sensResults <- NULL
  sensResults <- data.frame(Simulation = 1:max(interventionModelOutputAll$Simulation),
                                    CoverageAllInt = coverage,
                                    Intervention = i,
                                    totalCosts = NA,
                                    totalQALYS = NA, 
                                    totalODdeaths = NA)
  
  interventionModelOutput <- interventionModelOutputAll%>%
    filter(fileName == i)
  
  #Use filename to show coverage level
  if(grepl("_cm", i, fixed = TRUE)){
    cmCoverage0 = coverage
  }else{
    cmCoverage0 = 0
  }
  if(grepl("_p", i, fixed = TRUE)){
    therCoverage0 = coverage
  }else{
    therCoverage0 = 0
  }  
  if(grepl("_hs", i, fixed = TRUE)){
    hsCoverage0 = coverage
  }else{
    hsCoverage0 = 0
  }  
  if(grepl("_ed", i, fixed = TRUE)){
    edCoverage0 = coverage
  }else{
    edCoverage0 = 0
  } 
  if(grepl("_th", i, fixed = TRUE)){
    teleCoverage0 = coverage
  }else{
    teleCoverage0 = 0
  }
  
  for (j in 1:max(interventionModelOutput$Simulation)) { #j=2
    
  l_intervention <- list(modelOutput = interventionModelOutput[interventionModelOutput$Simulation == j,],
                         cmCoverage = cmCoverage0,
                         therCoverage = therCoverage0,
                         hsCoverage = hsCoverage0,
                         edCoverage = edCoverage0,
                         teleCoverage = teleCoverage0,
                         discCosts = discCosts,
                         discCriminalCosts = discCriminalCosts,
                         discQALYs = discQALYs,
                         interventionRampUpYears = interventionRampUpYears,
                         iCriminalJusticeOn = iCriminalJusticeOn,
                         startTime = startYear)
  
  #CEA for intervention and simulation
  resultsIntervention <- interventionCEA(l_intervention)
  
  sensResults$totalCosts[sensResults$Simulation == j]<- resultsIntervention[[9]][3]
  sensResults$totalQALYS[sensResults$Simulation == j]<- resultsIntervention[[9]][1]
  sensResults$totalODdeaths[sensResults$Simulation == j]<- resultsIntervention[[9]][2]
  }
  
  ### Add the intervention to the rest of t
  sensResultsAllInterventions<- rbind(sensResultsAllInterventions, sensResults)
  sensAll0 <- sensResultsAllInterventions
}

  ### Save Run! 
  date.run<- Sys.Date()
  write.csv(as.matrix(sensResultsAllInterventions), file = paste0(FILE_PATH_RESULTS,"SensitivityResults",coveragePercent,"cov",date.run,".csv"),
            row.names = F)

}else{
  #Read in the CEA output files from previous analysis
  sensAll0 <- read.csv(file = paste0(FILE_PATH_RESULTS,"SensitivityResults",coveragePercent,"cov","2022-06-06.csv"))
  
}


### Process the CEA results for all of the sensitivity runs
sensAll1 <- sensAll0%>%
  # mutate(InterventionName = case_when(Intervention == paste0(coveragePercent,"_cm") ~ "CM",
  #                       carb > 2  ~ "high"))
  mutate(InterventionName = gsub("10_", "", Intervention))%>%
  mutate(InterventionName = gsub("5_", "", InterventionName))%>%
  mutate(InterventionName = gsub("20_", "", InterventionName))%>%
  mutate(InterventionName = gsub("ed_init", "ED", InterventionName))%>%
  mutate(InterventionName = gsub("_", "+", InterventionName))%>%
  mutate(InterventionName = gsub("and", "", InterventionName))%>%
  mutate(InterventionName = gsub("cm", "CM", InterventionName))%>%
  mutate(InterventionName = gsub("p", "P", InterventionName))%>%
  mutate(InterventionName = gsub("hs", "HS", InterventionName))%>%
  mutate(InterventionName = gsub("telehealth", "TH", InterventionName)) %>%
  mutate(InterventionName = gsub("th", "TH", InterventionName))%>%
  mutate(InterventionName = gsub("t", "TH", InterventionName))%>%
  mutate(InterventionName = gsub("\\++", "+", InterventionName))%>%
  mutate(InterventionName = gsub("HS\\+P", "P+HS", InterventionName))%>%
  mutate(InterventionName = gsub("HS\\+P\\+ED", "P+HS+ED", InterventionName))%>%
  mutate(InterventionName = gsub("CM\\+TH\\+HS", "CM+HS+TH", InterventionName))%>%
  mutate(InterventionName = gsub("HS\\+P\\+TH", "P+HS+TH", InterventionName))%>%
  mutate(InterventionName = gsub("CM\\+P\\+TH\\+ED", "CM+P+ED+TH", InterventionName))%>%
  mutate(InterventionName = gsub("CM\\+P\\+HS\\+TH\\+ED", "CM+P+HS+ED+TH", InterventionName))

### Download the basecase run results
statusQuoCosts <- basecaseCEATable$totalcost[basecaseCEATable$Intervention == "Status Quo"]
statusQuoQALYs <- basecaseCEATable$totalQALY[basecaseCEATable$Intervention == "Status Quo"]
orderStrategies <- basecaseCEATable$InterventionAbbrev[-1]

### Calculate incremental costsa and QALYs
sensAll <- sensAll1 %>%
  mutate(IncrementalCosts= totalCosts-statusQuoCosts,
         IncrementalQALYs = totalQALYS - statusQuoQALYs,
         #ODdeathsAverted = totalOverdoseDeaths,
         NMB = IncrementalQALYs*wtp - IncrementalCosts)

#sensAll$Intervention <- gsub("C:/Users/ai219/Dropbox \\(Partners HealthCare\\)/Documents/Opioids/Sensitivity Analysis/clean_export_sensitivity_","",sensAll$Intervention)

# sensAll <- sensAll %>% 
#   mutate(Strategy = case_when("cm" %in% Intervention ~ "Contingency Management"))#,
#                                #TRUE ~ paste0("L_", Numbers)))
sensAllSummary <- sensAll %>%
  mutate(Intervention = as.factor(Intervention))%>%
  group_by(Intervention, InterventionName) %>%
  summarise(meanTotQALYs = mean(totalQALYS),
            meanIncQALYs = mean(IncrementalQALYs),
            lbIncQALYs = min(IncrementalQALYs),
            ubIncQALYs = max(IncrementalQALYs),
            meanODDeaths = mean(totalODdeaths),
            lbODDeaths = min(totalODdeaths),
            ubODDeaths = max(totalODdeaths),
            meanTotCosts = mean(totalCosts),
            meanIncCosts = mean(IncrementalCosts),
            lbIncCosts = min(IncrementalCosts),
            ubIncCosts = max(IncrementalCosts),
            meanNMB = mean(NMB),
            lbNMB = min(NMB),
            ubNMB = max(NMB),
            #percentCE = (sum(NMB > 0)/n())*100)
            percentCE = mean(NMB > 0)*100)%>%
            ungroup()%>%
  arrange(match(InterventionName,orderStrategies))


colnames(sensAllSummary)<- c("Strategy", "InterventionName", "Average Total QALYs", 
                             "Average Incremental QALYs compared to the Status Quo", 
                             "Lowerbound-Incremental QALYs", "Upperbound-Incremental QALYs", 
                             "Average Overdose Deaths", "Lowerbound Overdose Deaths","Upperbound Overdose Deaths",
                             "Average Total Costs",
                             "Average Incremental Costs compared to the Status Quo", "Lowerbound-Incremental Costs", 
                             "Upperbound-Incremental Costs", "Average NMB compared to the Status Quo", "Lowerbound-NMB", 
                             "Upperbound-NMB", "Percent of Runs that are Cost-Effective Compared to Status Quo (%)")

### Create CEAC for willingness to pay $0-$200,000/QALY gained
sensAllceac <- sensAll1 %>%
  mutate(IncrementalCosts= totalCosts-statusQuoCosts,
         IncrementalQALYs = totalQALYS - statusQuoQALYs,
         #ODdeathsAverted = totalOverdoseDeaths,
         NMB.0 = IncrementalQALYs*0 - IncrementalCosts,
         NMB.10 = IncrementalQALYs*10000 - IncrementalCosts,
         NMB.20 = IncrementalQALYs*20000 - IncrementalCosts,
         NMB.30 = IncrementalQALYs*30000 - IncrementalCosts,
         NMB.40 = IncrementalQALYs*40000 - IncrementalCosts,
         NMB.50 = IncrementalQALYs*50000 - IncrementalCosts,
         NMB.60 = IncrementalQALYs*60000 - IncrementalCosts,
         NMB.70 = IncrementalQALYs*70000 - IncrementalCosts,
         NMB.80 = IncrementalQALYs*80000 - IncrementalCosts,
         NMB.90 = IncrementalQALYs*90000 - IncrementalCosts,
         NMB.100 = IncrementalQALYs*100000 - IncrementalCosts,
         NMB.110 = IncrementalQALYs*110000 - IncrementalCosts,
         NMB.120 = IncrementalQALYs*120000 - IncrementalCosts,
         NMB.130 = IncrementalQALYs*130000 - IncrementalCosts,
         NMB.140 = IncrementalQALYs*140000 - IncrementalCosts,
         NMB.150 = IncrementalQALYs*150000 - IncrementalCosts,
         NMB.160 = IncrementalQALYs*160000 - IncrementalCosts,
         NMB.170 = IncrementalQALYs*170000 - IncrementalCosts,
         NMB.180 = IncrementalQALYs*180000 - IncrementalCosts,
         NMB.190 = IncrementalQALYs*190000 - IncrementalCosts,
         NMB.200 = IncrementalQALYs*200000 - IncrementalCosts)%>%
  arrange(match(InterventionName,orderStrategies))

#Calculate the number of runs where each intervention is preferred
sensAllceacHighestNMB <- sensAllceac %>%
  group_by(Simulation)%>%
  mutate(pref.NMB.0 = (max(NMB.0) == NMB.0),
         pref.NMB.10 = (max(NMB.10) == NMB.10),
         pref.NMB.20 = (max(NMB.20) == NMB.20),
         pref.NMB.30 = (max(NMB.30) == NMB.30),
         pref.NMB.40 = (max(NMB.40) == NMB.40),
         pref.NMB.50 = (max(NMB.50) == NMB.50),
         pref.NMB.60 = (max(NMB.60) == NMB.60),
         pref.NMB.70 = (max(NMB.70) == NMB.70),
         pref.NMB.80 = (max(NMB.80) == NMB.80),
         pref.NMB.90 = (max(NMB.90) == NMB.90),
         pref.NMB.100 = (max(NMB.100) == NMB.100),
         pref.NMB.110 = (max(NMB.110) == NMB.110),
         pref.NMB.120 = (max(NMB.120) == NMB.120),
         pref.NMB.130 = (max(NMB.130) == NMB.130),
         pref.NMB.140 = (max(NMB.140) == NMB.140),
         pref.NMB.150 = (max(NMB.150) == NMB.150),
         pref.NMB.160 = (max(NMB.160) == NMB.160),
         pref.NMB.170 = (max(NMB.170) == NMB.170),
         pref.NMB.180 = (max(NMB.180) == NMB.180),
         pref.NMB.190 = (max(NMB.190) == NMB.190),
         pref.NMB.200 = (max(NMB.200) == NMB.200))%>%
  ungroup()%>%
  group_by(InterventionName) %>%
  summarise(percentPreferred.NMB0 = mean(pref.NMB.0),
            percentPreferred.NMB10 = mean(pref.NMB.10),
            percentPreferred.NMB20 = mean(pref.NMB.20),
            percentPreferred.NMB30 = mean(pref.NMB.30),
            percentPreferred.NMB40 = mean(pref.NMB.40),
            percentPreferred.NMB50 = mean(pref.NMB.50),
            percentPreferred.NMB60 = mean(pref.NMB.60),
            percentPreferred.NMB70 = mean(pref.NMB.70),
            percentPreferred.NMB80 = mean(pref.NMB.80),
            percentPreferred.NMB90 = mean(pref.NMB.90),
            percentPreferred.NMB100 = mean(pref.NMB.100),
            percentPreferred.NMB110 = mean(pref.NMB.110),
            percentPreferred.NMB120 = mean(pref.NMB.120),
            percentPreferred.NMB130 = mean(pref.NMB.130),
            percentPreferred.NMB140 = mean(pref.NMB.140),
            percentPreferred.NMB150 = mean(pref.NMB.150),
            percentPreferred.NMB160 = mean(pref.NMB.160),
            percentPreferred.NMB170 = mean(pref.NMB.170),
            percentPreferred.NMB180 = mean(pref.NMB.180),
            percentPreferred.NMB190 = mean(pref.NMB.190),
            percentPreferred.NMB200 = mean(pref.NMB.200))%>%
  ungroup()%>%
  mutate(total = rowSums(.[2:22]))%>%
  filter(total > 0)%>%
  group_by(InterventionName)%>%
  pivot_longer(cols = c(percentPreferred.NMB0,
                        percentPreferred.NMB10,
                        percentPreferred.NMB20,
                        percentPreferred.NMB30,
                        percentPreferred.NMB40,
                        percentPreferred.NMB50,
                        percentPreferred.NMB60,
                        percentPreferred.NMB70,
                        percentPreferred.NMB80,
                        percentPreferred.NMB90,
                        percentPreferred.NMB100,
                        percentPreferred.NMB110,
                        percentPreferred.NMB120,
                        percentPreferred.NMB130,
                        percentPreferred.NMB140,
                        percentPreferred.NMB150,
                        percentPreferred.NMB160,
                        percentPreferred.NMB170,
                        percentPreferred.NMB180,
                        percentPreferred.NMB190,
                        percentPreferred.NMB200), 
                                            names_to = "WTP",
                                            names_prefix = "percentPreferred.NMB",
                                            values_to = "percentPreferred")%>%
  mutate(WTP = as.numeric(WTP))%>%
  data.frame()
  

#sensAllceac$Intervention <- gsub("C:/Users/ai219/Dropbox \\(Partners HealthCare\\)/Documents/Opioids/Sensitivity Analysis/clean_export_sensitivity_","",sensAll$Intervention)

sensAllSummaryCEACwtp <- sensAllceac %>%
  mutate(Intervention = as.factor(Intervention))%>%
  group_by(Intervention, InterventionName) %>%
  pivot_longer(cols = c( NMB.0,
                         NMB.10,
                         NMB.20,
                         NMB.30,
                         NMB.40,
                         NMB.50,
                         NMB.60,
                         NMB.70,
                         NMB.80,
                         NMB.90,
                         NMB.100,
                         NMB.110,
                         NMB.120,
                         NMB.130,
                         NMB.140,
                         NMB.150,
                         NMB.160,
                         NMB.170,
                         NMB.180,
                         NMB.190,
                         NMB.200), 
               names_to = "WTP",
               names_prefix = "NMB.",
               values_to = "NMB")%>%
  mutate(WTP = as.numeric(WTP))%>%
  group_by(Intervention, InterventionName, WTP)%>%
  summarise(meanTotQALYs = mean(totalQALYS),
            meanIncQALYs = mean(IncrementalQALYs),
            lbIncQALYs = min(IncrementalQALYs),
            ubIncQALYs = max(IncrementalQALYs),
            meanODDeaths = mean(totalODdeaths),
            lbODDeaths = min(totalODdeaths),
            ubODDeaths = max(totalODdeaths),
            meanTotCosts = mean(totalCosts),
            meanIncCosts = mean(IncrementalCosts),
            lbIncCosts = min(IncrementalCosts),
            ubIncCosts = max(IncrementalCosts),
            meanNMB = mean(NMB),
            lbNMB = min(NMB),
            ubNMB = max(NMB),
            #percentCE = (sum(NMB > 0)/n())*100)
            percentCE = mean(NMB > 0)*100)%>%
  ungroup()

### Run Dampack package to plot PSA

# ### Make PSA object
# psa_sens<- make_psa_obj(
#   cost = data.frame(sensAll$totalCosts),
#   effectiveness = data.frame(sensAll$totalQALYS),
#   parameters = NULL,
#   strategies = data.frame(sensAll$Intervention),
#   currency = "$",
#   other_outcome = NULL
# )


##Save the outputs 
if(SaveOutputs){
  date.run <- Sys.Date()
  ##All
  ##write.csv(sensAll, file = paste0("C:/Users/ai219/Dropbox (Partners HealthCare)/Documents/Opioids/R Studio/Bup_CEA/Results/Sensitivity Analysis/sensAll",date.run,".csv"))
  #Summary
  write.csv(sensAllSummary, file = paste0("C:/Users/ai219/Dropbox (Partners HealthCare)/Documents/Opioids/R Studio/Bup_CEA/Results/Sensitivity Analysis/sensAllSummaryPSA",coveragePercent,"cov",date.run,".csv"))
  write.csv(sensAllSummaryCEACwtp, file = paste0("C:/Users/ai219/Dropbox (Partners HealthCare)/Documents/Opioids/R Studio/Bup_CEA/Results/Sensitivity Analysis/sensAllSummaryPSAceac",coveragePercent,"cov",date.run,".csv"))
  }

if(plotSave){
  ## Plot cost-effectiveness plane
  source('plotSensitivityPSA.R')
  plotSensitivityPSA(sensAll, sensAllSummaryCEACwtp, sensAllceacHighestNMB)
}

