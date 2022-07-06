### Script to calculate and aggregate the costs and QALYs for each of the interventions
### By Anneke Claypool
### Last Updated 7/6/22

interventionCEA <- function(l_intervention){#l_intervention<- l_statusQuo #l_intervention <-l_sens_cm
  
  #Extract the model outputs
  statusQuo <- l_intervention$modelOutput #All intervention outputs are referred to as status quo in this script 
  cmCoverage <- l_intervention$cmCoverage
  therCoverage <- l_intervention$therCoverage
  hsCoverage <- l_intervention$hsCoverage
  edCoverage <- l_intervention$edCoverage
  thCoverage <- l_intervention$teleCoverage
  discCosts <- l_intervention$discCosts
  discCriminalCosts<- l_intervention$discCriminalCosts
  discQALYs <- l_intervention$discQALYs
  interventionRampUpYears <- l_intervention$interventionRampUpYears
  iCriminalJusticeOn <- l_intervention$iCriminalJusticeOn
  startTime <- l_intervention$startTime
  
  nStepsPerYear <- nrow(statusQuo[statusQuo$Time >= 1999 & statusQuo$Time < 2000,])
  
  ### Add in costs to dataset
  statusQuoCosts <- left_join(statusQuo, discCosts, by = "Time") %>%
    fill(colnames(discCosts), .direction = "down") %>%
    mutate_at(c("dcAnnualHealthUS","dcOUDNoTxExcess", "dcOUDTxExcess",
                "dcMethadone","dcBuprenorphine","dcNaltrexone",      
                "dcOverdoseNonfatal", "dcOverdoseFatal","dcNetProductivity"),
              ~./nStepsPerYear)
  
  #Criminal Justice Costs
  if(iCriminalJusticeOn){
    statusQuoCriminalCosts <- left_join(statusQuo, discCriminalCosts, by = "Time") %>%
      fill(colnames(discCriminalCosts), .direction = "down") %>%
      mutate_at(c("dcCriminalJusticeHUDTx","dcCriminalJusticeOUDTx", "dcCriminalJusticeHUD",
                  "dcCriminalJusticeOUD","dcCriminalJusticeHUDRemission","dcCriminalJusticeOUDRemission"),
                ~./nStepsPerYear)
  }
  
  ### Add in QALYs to dataset
  statusQuoQALYs <- left_join(statusQuo, discQALYs, by = "Time") %>%
    fill(colnames(discQALYs), .direction = "down") %>%
    mutate_at(c("dqNonDisHerUse","dqRxMisuse","dqRxOUDnoHeroin",
                "dqRxOUDHeroin", "dqHUD", "dqRxOUDBup",                  
                "dqRxOUDMMT", "dqRxOUDViv", "dqRxOUDHeroinBup","dqRxOUDHeroinMMT",            
                "dqRxOUDHeroinViv", "dqHUDBup", "dqHUDMMT", "dqHUDViv",                    
                "dqRxOUDRemission", "dqRxOUDStableRemission",      
                "dqRxOUDHeroinRemission", "dqRxOUDHeroinStableRemission",
                "dqHUDRemission", "dqHUDStableRemission","dqGainedSurvival"), 
              ~./nStepsPerYear)
  
  
  ###Calculate total costs 
  
  ## Costs per stock over time
  df_costs_stock <- statusQuoCosts %>%
    group_by(Time) %>%
    summarise(totCostHealthcarePop = dcAnnualHealthUS*Total.population,
              totCostOUDNoTxExcess = dcOUDNoTxExcess*(Total.with.UD-(Total.by.MOUD.Bup.+ 
                                                                       Total.by.MOUD.MMT. + Total.by.MOUD.Viv.)), 
              totCostOUDTxExess = dcOUDTxExcess*(Total.by.MOUD.Bup.+Total.by.MOUD.MMT. + Total.by.MOUD.Viv.),      
              totCostBuprenorphine = dcBuprenorphine*Total.by.MOUD.Bup.,
              totCostMMT = dcMethadone*Total.by.MOUD.MMT.,
              totCostViv = dcNaltrexone*Total.by.MOUD.Viv.,
              totCostOverdoseNonfatal = dcOverdoseNonfatal*Total.nonfatal.overdoses, 
              totCostOverdoseFatal = dcOverdoseFatal*Total.overdose.deaths,
              totCostLostProductivity = dcNetProductivity*Total.overdose.deaths) 
  
  ## Calculate total costs by stock
  l_total_costs_stock <- colSums(df_costs_stock[,-1], na.rm = T)
  
  n_total_cost_stock<-sum(l_total_costs_stock)
  
  ## Total cost intervention by coverage and time (time of rampup considered in runscript)
  df_costs_intervention <- statusQuoCosts %>%
    group_by(Time) %>%
    summarise(totCostCM = dcCM*Total.by.MOUD.Bup.*cmCoverage,
              totCostTher = dcTherapy*Total.by.MOUD.Bup.*therCoverage,
              #totCostHS = dcHubSpoke*Total.by.MOUD.Bup.*hsCoverage) # Make sure this is per buprenorphine patient costs, change if per provider
              totCostHS = dcHubSpoke*Bup.providers*hsCoverage, # Make sure this is per buprenorphine provider costs.
              totCostED = Total.with.UD*edCoverage*0.26,
              totCostTH = dcTH*Total.by.MOUD.Bup.*thCoverage) #Total.nonfatal.overdoses*edCoverage)
  
  l_total_costs_intervention <- colSums(df_costs_intervention[,-1], na.rm = T)
  
  n_total_cost_intervention <-sum(l_total_costs_intervention)
  
  ## Calculate criminal justice costs
  if(iCriminalJusticeOn){
    df_costs_criminal_justice <- statusQuoCriminalCosts %>%
      group_by(Time) %>%
      summarise(totCriminalJusticeCostHUDTx = dcCriminalJusticeHUDTx*(HUD.by.MOUD.Bup.+ HUD.by.MOUD.MMT.+HUD.by.MOUD.Viv.),
                totCriminalJusticeOUDTx = dcCriminalJusticeOUDTx*(Rx.OUD.by.MOUD.Bup.+ Rx.OUD.by.MOUD.MMT.+ Rx.OUD.by.MOUD.Viv.),
                totCriminalJusticeHUD = dcCriminalJusticeHUD*HUD.no.MOUD,
                totCriminalJusticeOUD = dcCriminalJusticeOUD*(Rx.OUD.no.PY.heroin.no.MOUD+Rx.OUD.with.PY.heroin.no.MOUD),
                totCriminalJusticeHUDRemission = dcCriminalJusticeHUDRemission*(HUD.in.remission + HUD.in.stable.remission),
                dcCriminalJusticeOUDRemission = dcCriminalJusticeOUDRemission*(Rx.OUD.no.heroin.in.remission + Rx.OUD.no.heroin.in.stable.remission+
                                                                                 Rx.OUD.with.heroin.in.remission + Rx.OUD.with.heroin.in.stable.remission) )
    ## Calculate total criminal justice costs by stock
    l_total_costs_stock_criminal_justice <- colSums(df_costs_criminal_justice[,-1], na.rm = T)
    
    n_total_cost_stock_criminal_justice <-sum(l_total_costs_stock_criminal_justice)
  }else{
    df_costs_criminal_justice <- NA
    l_total_costs_stock_criminal_justice <- NA
    n_total_cost_stock_criminal_justice <- NA
  }
  
  ### Calculate Total QALYs
  df_qaly_stock <- statusQuoQALYs %>%
    group_by(Time) %>%
    summarise(totQALYNonDisHerUse = dqNonDisHerUse*Nondisordered.heroin.use,
              totQALYRxMisuse = dqRxMisuse*Rx.misuse.no.PY.heroin,
              totQALYOUDnoHeroin = dqRxOUDnoHeroin*Rx.OUD.no.PY.heroin.no.MOUD,
              totQALYRxOUDHeroin = dqRxOUDHeroin*Rx.OUD.with.PY.heroin.no.MOUD, 
              totQALYHUD = dqHUD*HUD.no.MOUD, 
              totQALYRxOUDBup = dqRxOUDBup*Rx.OUD.no.heroin.by.MOUD.Bup.,                  
              totQALYRxOUDMMT = dqRxOUDMMT*Rx.OUD.no.heroin.by.MOUD.MMT., 
              totQALYRxOUDViv = dqRxOUDViv*Rx.OUD.no.heroin.by.MOUD.Viv., 
              totQALYRxOUDHeroinBup = dqRxOUDHeroinBup*Rx.OUD.with.heroin.by.MOUD.Bup.,
              totQALYRxOUDHeroinMMT = dqRxOUDHeroinMMT*Rx.OUD.with.heroin.by.MOUD.MMT.,            
              totQALYRxOUDHeroinViv = dqRxOUDHeroinViv*Rx.OUD.with.heroin.by.MOUD.Viv., 
              totQALYHUDBup = dqHUDBup*HUD.by.MOUD.Bup., 
              totQALYHUDMMT = dqHUDMMT*HUD.by.MOUD.MMT., 
              totQALYHUDViv = dqHUDViv*HUD.by.MOUD.Viv.,                    
              totQALYRxOUDRemission = dqRxOUDRemission*Rx.OUD.no.heroin.in.remission, 
              totQALYRxOUDStableRemission = dqRxOUDStableRemission*Rx.OUD.no.heroin.in.stable.remission,      
              totQALYRxOUDHeroinRemission = dqRxOUDHeroinRemission*Rx.OUD.with.heroin.in.remission, 
              totQALYRxOUDHeroinStableRemission = dqRxOUDHeroinStableRemission*Rx.OUD.with.heroin.in.stable.remission,
              totQALYHUDRemission = dqHUDRemission*HUD.in.remission, 
              totQALYHUDStableRemission = dqHUDStableRemission*HUD.in.stable.remission)
  
  ## Calculate total qalys  by stock
  l_total_qaly_stock <- colSums(df_qaly_stock[,-1], na.rm = T)
  
  n_total_qualy_stock<-sum(l_total_qaly_stock)
  
  ## QALYs remaining for the surviving cohort at end of time period
  nPopFinal <- statusQuo$Total.population[statusQuo$Time == max(statusQuo$Time)]
  dqSurvival <- statusQuoQALYs$dqGainedSurvival[statusQuoQALYs$Time == max(statusQuo$Time)]
  
  n_qualy_survival <- nPopFinal*dqSurvival
  
  ##Final outputs
  #Costs
  if(iCriminalJusticeOn){
    total_Cost <- n_total_cost_stock + n_total_cost_stock_criminal_justice + n_total_cost_intervention
  }else{total_Cost <- n_total_cost_stock + n_total_cost_intervention}
  
  
  #QALYs
  total_QALYS <- n_total_qualy_stock + n_qualy_survival
  
  #Overdoses
  cumulative_overdose_deaths <- max(statusQuo$Cumulative.overdose.deaths)
  cumulative_nonfatal_overdose <- max(statusQuo$Cumulative.nonfatal.overdoses)
  
  #Total Overdoses time period
  total_overdose_deaths_analyticTime <- max(statusQuo$Cumulative.overdose.deaths)-statusQuo$Cumulative.overdose.deaths[statusQuo$Time == startYear]
  total_nonfatal_overdose_analyticTime <- max(statusQuo$Cumulative.nonfatal.overdoses)-statusQuo$Cumulative.nonfatal.overdoses[statusQuo$Time == startYear]
  
  #All Overdoses during time period (for figure)
  overdosesAnalyticTime <- statusQuo %>%
    select(Time, Cumulative.overdose.deaths, Cumulative.nonfatal.overdoses, Total.overdose.deaths, Total.nonfatal.overdoses)%>%
    filter(Time >= startTime)
  
  ceaOutputs <- data.frame(totalQALYs = total_QALYS, 
                           totalOverdoseDeaths = total_overdose_deaths_analyticTime, 
                           totalCosts = total_Cost) 
  
  #Aggregate all results to send back to runscript
  results <- list(
    total_Costs_Stock <- l_total_costs_stock,
    total_Criminal_Justic_Costs_Stock <- l_total_costs_stock_criminal_justice,
    total_QALY_stock <- l_total_qaly_stock,
    total_cost <- total_Cost,
    totalQALY <- total_QALYS,
    total_overdose_deaths <- total_overdose_deaths_analyticTime,
    total_nonfatal_overdose <- total_nonfatal_overdose_analyticTime,
    overdosesTime <- overdosesAnalyticTime,
    CEAoutputs <- ceaOutputs
  )
  
  return(results)
}