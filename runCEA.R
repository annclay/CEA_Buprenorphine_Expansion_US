### Run CEA for any intervention and get results
### By Anneke Claypool
### Last Updated 7/6/22

runCEA <- function(interventionModelOutput0, interventionCoverage, #interventionCoverage should be in format: [cm, ther, hs, ed, tele]
                   interventionName, abbrevName,
                   discCosts, discCriminalCosts, discQALYs, interventionRampUpYears, iCriminalJusticeOn, startYear){ 
  
### Functions
#Transform data- Status Quo (Make this a function for formatting incoming SOURCE trace data)
formatSOURCEoutput <- function(modelOutput0){
  modelOutput1 <- data.frame(t(data.frame(modelOutput0)))
  modelOutput <- modelOutput1[-1,]
  transform(modelOutput)
  colnames(modelOutput) <- make.names(modelOutput1[1,], unique=TRUE)
  modelOutput <- sapply(modelOutput,as.numeric)
  modelOutput<- as.data.frame(modelOutput)
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
    mutate(Total.nonfatal.overdoses = c(Cumulative.nonfatal.overdoses[1],diff(Cumulative.nonfatal.overdoses)) , #Calculation method for total and cumulative are different. Changing it so it is consistent
           Total.overdose.deaths = c(Cumulative.overdose.deaths[1],diff(Cumulative.overdose.deaths)))%>%
    return(modelOutput)
}

interventionModelOutput <- formatSOURCEoutput(interventionModelOutput0)

#Prepare intervention to run CEA
l_intervention <- list(modelOutput = interventionModelOutput,
                    cmCoverage = interventionCoverage[1],
                    therCoverage = interventionCoverage[2],
                    hsCoverage = interventionCoverage[3],
                    edCoverage = interventionCoverage[4],
                    teleCoverage = interventionCoverage[5],
                    discCosts = discCosts,
                    discCriminalCosts = discCriminalCosts,
                    discQALYs = discQALYs,
                    interventionRampUpYears = interventionRampUpYears,
                    iCriminalJusticeOn = iCriminalJusticeOn,
                    startTime = startYear
) 

#Load and run CEA for intervention
source("interventionCEA.R")
resultsIntervention <- interventionCEA(l_intervention)

#Format outputs
overdosesTimeIntervention <- resultsIntervention[[8]] %>%
  mutate(Intervention = interventionName,
         abbrevName = abbrevName)

CEAtableIntervention <- data.frame(interventionName)
CEAtableIntervention$totalQALY <- resultsIntervention[[9]]$totalQALYs
CEAtableIntervention$totalcost <- resultsIntervention[[9]]$totalCosts
CEAtableIntervention$InterventionAbbrev <- abbrevName
colnames(CEAtableIntervention)[1]<- "Intervention"

returnInterventionResults <- list(fullResults = resultsIntervention,
                                  overdosesTime = overdosesTimeIntervention,
                                  CEAtable = CEAtableIntervention) 
return(returnInterventionResults)
}