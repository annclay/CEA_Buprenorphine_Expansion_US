#Script to plot overdoses, fatal overdoses, and overdoses averted
### By Anneke Claypool
### Last Updated 7/6/22

plotOverdose <- function(cumulOverdose, coveragePercent){

  library(gridExtra)
  library(lubridate)
  library(RColorBrewer)
  
  runDate <- Sys.Date()
  cbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  interventions <- unique(cumulOverdose$Intervention)
  
  cumulOverdose <- cumulOverdose %>%
    mutate(Time = as.Date(paste(Time, 1, 1, sep = "-")))%>%
    mutate(Intervention = factor(Intervention, levels = interventions))#Make into factors to maintain order in plot
  
  ## Plot all strategies together
  # plotTotalFatalOverdoseAverted <- ggplot(cumulOverdose, aes(x = cumulOverdose$Time,
  #                                                            y = cumulOverdose$Overdose.deaths.Averted,
  #                                                            color = cumulOverdose$Intervention)) +
  #   geom_line()
  
  cumulOverdoseSingleInt <- cumulOverdose %>%
    filter(Intervention == "Status Quo" |
             Intervention == "Contingency Management"|
             Intervention == "Psychotherapy"|
             Intervention == "Hub and Spoke"|
             Intervention == "ED Initiation"|
             Intervention == "Telehealth")
  
  cumulOverdosePortfolios2 <- cumulOverdose %>%
    filter(Intervention == "Status Quo" |
             Intervention == "Contingency Management + Psychotherapy"|
             Intervention == "Contingency Management + Hub and Spoke"|
             Intervention == "Psychotherapy + Hub and Spoke" |
             Intervention == "Contingency Management + ED Initiation"|                                
             Intervention == "Hub and Spoke + ED Initiation"| 
             Intervention == "Psychotherapy + ED Initiation"|  
             Intervention == "Contingency Management + Telehealth"|  
             Intervention == "Psychotherapy + Telehealth"|  
             Intervention == "Hub and Spoke + Telehealth"|  
             Intervention == "ED Initiation + Telehealth")
  
  cumulOverdosePortfolios3 <- cumulOverdose %>%
    filter(Intervention == "Status Quo" |
             Intervention == "Contingency Management + Psychotherapy + Hub and Spoke"|                                        
             Intervention == "ED Initiation + Contingency Management + Psychotherapy"|
             Intervention == "ED Initiation + Contingency Management + Hub and Spoke" |                
             Intervention == "ED Initiation + Psychotherapy + Hub and Spoke" |
             Intervention == "Contingency Management + Psychotherapy + Telehealth"|
             Intervention == "Contingency Management + Hub and Spoke + Telehealth"|
             Intervention == "Psychotherapy + Hub and Spoke + Telehealth"|
             Intervention == "Contingency Management + ED Initiation + Telehealth"|
             Intervention == "Hub and Spoke + ED Initiation + Telehealth"|
             Intervention == "Psychotherapy + ED Initiation +Telehealth")
  
  cumulOverdosePortfolios4.5 <- cumulOverdose %>%
    filter(Intervention == "Status Quo" |
             Intervention == "ED Initiation + Contingency Management + Psychotherapy + Hub and Spoke"|
             Intervention == "Contingency Management + Psychotherapy + ED Initiation + Telehealth"|
             Intervention == "Contingency Management + Hub and Spoke + ED Initiation +Telehealth"|
             Intervention == "Contingency Management + Psychotherapy + Hub and Spoke + Telehealth"|
             Intervention == "Psychotherapy + Hub and Spoke + ED Initiation + Telehealth"|
             Intervention == "Contingency Management + Psychotherapy + Hub and Spoke + ED Initiation + Telehealth")
  
  #Subplots separating the single interventions
  plotTotalFatalOverdoseAvertedSingle <- ggplot(cumulOverdoseSingleInt, aes(x = cumulOverdoseSingleInt$Time,
                                                             y = cumulOverdoseSingleInt$Overdose.deaths.Averted,
                                                             color = cumulOverdoseSingleInt$abbrevName,
                                                             linetype= cumulOverdoseSingleInt$abbrevName)) +
    geom_line(size = 1.2)+
    labs(x = "Year", y = "Number of Overdose Deaths Averted", color = "Strategy", linetype = "Strategy", title = "") +
    scale_y_continuous(limits = c(min(cumulOverdose$Overdose.deaths.Averted), 3000))+#4300 2200 max(cumulOverdose$Overdose.deaths.Averted))) +
    scale_color_manual(values = cbPalette[c(2,3,4,5,1,7)])+
    #scale_linetype_manual(values=c("twodash", "dotted", "dashed","dotdash", "solid", "longdash"))+
    theme_bw() +
    theme(text = element_text(size = 13)) +
    theme(legend.position = "right")
  
  plotTotalFatalOverdoseAvertedSingle
  ggsave(path = "Figures",
         filename =paste0("fig1a",coveragePercent,"cov",runDate,".jpeg"),
         height = 5,
         width = 9,
         units = "in",
         dpi = 700)
  #Subplots separating the single interventions with the portfolios
  plotTotalFatalOverdoseAverted2portfolio <- ggplot(cumulOverdosePortfolios2, aes(x = cumulOverdosePortfolios2$Time,
                                                                            y = cumulOverdosePortfolios2$Overdose.deaths.Averted,
                                                                            color = cumulOverdosePortfolios2$abbrevName)) +
    geom_line()+
    labs(x = "Year", y = "Number of Overdose Deaths Averted", color = "Strategy", title = "") +
    scale_y_continuous(limits = c(min(cumulOverdose$Overdose.deaths.Averted), 3000))+#2200 max(cumulOverdose$Overdose.deaths.Averted))) +
    #scale_color_manual(values = cbPalette[c(1,2,3,4,5,6)])+
    theme_bw() +
    theme(text = element_text(size = 13)) +
    theme(legend.position = "right")
  
  plotTotalFatalOverdoseAverted2portfolio
  ggsave(path = "Figures",
         filename =paste0("fig1b",coveragePercent,"cov",runDate,".jpeg"),
         height = 5,
         width = 9,
         units = "in",
         dpi = 700)
  
  #Subplots separating the single interventions with the portfolios 3
  plotTotalFatalOverdoseAverted3portfolio <- ggplot(cumulOverdosePortfolios3, aes(x = cumulOverdosePortfolios3$Time,
                                                                                  y = cumulOverdosePortfolios3$Overdose.deaths.Averted,
                                                                                  color = cumulOverdosePortfolios3$abbrevName)) +
    geom_line()+
    labs(x = "Year", y = "Number of Overdose Deaths Averted", color = "Strategy", title = "") +
    scale_y_continuous(limits = c(min(cumulOverdose$Overdose.deaths.Averted), 4300))+#2200 max(cumulOverdose$Overdose.deaths.Averted))) +
    #scale_color_manual(values = cbPalette[c(1,2,3,4,5,6)])+
    theme_bw() +
    theme(text = element_text(size = 13)) +
    theme(legend.position = "right")
  
  plotTotalFatalOverdoseAverted3portfolio
  ggsave(path = "Figures",
         filename =paste0("fig1c",coveragePercent,"cov",runDate,".jpeg"),
         height = 5,
         width = 9,
         units = "in",
         dpi = 700)
  
  #Subplots separating the single interventions with the portfolios 4-5
  plotTotalFatalOverdoseAverted45portfolio <- ggplot(cumulOverdosePortfolios4.5, aes(x = cumulOverdosePortfolios4.5$Time,
                                                                                  y = cumulOverdosePortfolios4.5$Overdose.deaths.Averted,
                                                                                  color = cumulOverdosePortfolios4.5$abbrevName,
                                                                                  linetype= cumulOverdosePortfolios4.5$abbrevName)) +
    geom_line(size = 1.2)+
    labs(x = "Year", y = "Number of Overdose Deaths Averted", color = "Strategy", linetype = "Strategy", title = "") +
    scale_y_continuous(limits = c(min(cumulOverdose$Overdose.deaths.Averted), 3000))+#4300 max(cumulOverdose$Overdose.deaths.Averted))) +
    scale_color_manual(values = cbPalette[c(2,3,4,5,7,1)])+
    theme_bw() +
    theme(text = element_text(size = 13)) +
    theme(legend.position = "right")
  
  plotTotalFatalOverdoseAverted45portfolio
  ggsave(path = "Figures",
         filename =paste0("fig1d",coveragePercent,"cov",runDate,".jpeg"),
         height = 5,
         width = 9,
         units = "in",
         dpi = 700)
  
  # ## Get more colors for the portfolios
  # # Classic palette BuPu, with 4 colors
  # coul <- brewer.pal(9, "BuGn") #PuOr
  # # Add more colors to this palette :
  # coul <- colorRampPalette(coul)(15)

}

# ## Portfolios with 2 interventions
# #Select for only portfolios of 2
# cumulOverdosePortfolios2 <- cumulOverdosePortfolios %>%
#   filter(Intervention %in% c("Status Quo",
#          "Contingency Management + Psychotherapy",                
#          "Contingency Management + Hub and Spoke",                
#          "Psychotherapy + Hub and Spoke",                        
#          "Contingency Management + Psychotherapy + Hub and Spoke",
#          "ED Initiation + Contingency Management",                              
#          "ED Initiation + Hub and Spoke",
#          "ED Initiation + Psychotherapy"))
# 
# ## Fix this! 
# plotTotalFatalOverdoseAvertedPortfolios2 <- ggplot(cumulOverdosePortfolios2, aes(x = cumulOverdosePortfolios$Time,
#                                                                                y = cumulOverdosePortfolios$Overdose.deaths.Averted,
#                                                                                color = cumulOverdosePortfolios$Intervention)) +
#   geom_line()+
#   labs(x = "Year", y = "Number of Overdose Deaths Averted", color = "Strategy", title = "") +
#   theme_bw() +
#   theme(text = element_text(size = 13)) +
#   scale_y_continuous(limits = c(min(cumulOverdose$Overdose.deaths.Averted), 2200))+# max(cumulOverdose$Overdose.deaths.Averted))) +
#   scale_color_manual(labels = c("Status Quo","Contingency Management + \nPsychotherapy",                
#                                 "Contingency Management + \nHub and Spoke",                
#                                 "Psychotherapy + \nHub and Spoke",                         
#                                 "Contingency Management + \nPsychotherapy + \nHub and Spoke",
#                                 "ED Initiation + \nContingency Management",                              
#                                 "ED Initiation + \nHub and Spoke", 
#                                 "ED Initiation + \nPsychotherapy"),values = colorRampPalette(coul)(8))+ #,values = cbPalette[c(1,5,6,7,8)])+
#   theme(legend.position = "right")
# 
# plotTotalFatalOverdoseAvertedPortfolios2
# 
# ggsave(path = "Figures",
#        filename =paste0("fig1b",runDate,".jpeg"),
#        height = 5,
#        width = 9,
#        units = "in",
#        dpi = 700)
# 
# ## Portfolios with 3 and 4 interventions
# 
# plotTotalFatalOverdoseAvertedPortfolios3 <- ggplot(cumulOverdosePortfolios, aes(x = cumulOverdosePortfolios$Time,
#                                                                                y = cumulOverdosePortfolios$Overdose.deaths.Averted,
#                                                                                color = cumulOverdosePortfolios$Intervention)) +
#   geom_line()+
#   labs(x = "Year", y = "Number of Overdose Deaths Averted", color = "Strategy", title = "") +
#   theme_bw() +
#   theme(text = element_text(size = 13)) +
#   scale_y_continuous(limits = c(min(cumulOverdose$Overdose.deaths.Averted), 2200))+# max(cumulOverdose$Overdose.deaths.Averted))) +
#   scale_color_manual(labels = c("Status Quo",                                        
#                                 "ED Initiation + \nContingency Management + \nPsychotherapy",
#                                 "ED Initiation + \nContingency Management + \nHub and Spoke" ,                
#                                 "ED Initiation + \nPsychotherapy + \nHub and Spoke" ,
#                                 "ED Initiation + \nContingency Management + \nPsychotherapy + \nHub and Spoke"),values = coul)+ #,values = cbPalette[c(1,5,6,7,8)])+
#   theme(legend.position = "right")
# 
# plotTotalFatalOverdoseAvertedPortfolios3
# ggsave(path = "Figures",
#        filename =paste0("fig1b",runDate,".jpeg"),
#        height = 5,
#        width = 9,
#        units = "in",
#        dpi = 700)

