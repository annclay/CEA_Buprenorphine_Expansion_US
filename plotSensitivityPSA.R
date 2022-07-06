###Plot Sensitivity Analysis
### Code to plot sensitivity analysis
### By Anneke Claypool
### Last Updated 7/6/22

plotSensitivityPSA <- function(sensAll, sensAllSummaryCEACwtp, sensAllceacHighestNMB){
  library(stringr)
  
  date.run = Sys.Date()
  ### Plot Cost_effectiveness frontier
  
  ### See how many interventions are in each strategy
  sensAll$number.of.int <- str_count(sensAll$InterventionName, "\\+")+1
  sensAllSummaryCEACwtp$number.of.int <- str_count(sensAllSummaryCEACwtp$InterventionName, "\\+")+1
  
  plotCESensitivity <- ggplot(sensAll, aes(x = IncrementalCosts,
                                           y = IncrementalQALYs,
                                           color = InterventionName))+#,
    #shape = Strategy)) +
    geom_point()+
    geom_abline(aes(slope = 1/wtp, intercept = 0, linetype = "$100,000/QALY Gained"), col = "gray60")+
    #annotate(geom="text", label="WTP $100,000/QALY Gained", x=1, y=h, vjust=-1)+
    geom_abline(aes(slope = 1/(0.5*wtp), intercept = 0,  linetype = "$50,000/QALY Gained"), col = "gray60")+
    geom_hline(yintercept=0)+ 
    geom_vline(xintercept = 0) + 
    # geom_abline(mapping = aes(slope = plotWTP$slope,
    #                           intercept = c(0,0), 
    #                           linetype = as.factor(WTP_Threshold))) +
    labs(x = "Incremental Cost (2021 USD)", y = "Quality-Adjusted Life Years Gained", linetype = "Willingness-to-Pay Threshold", color = "Strategy", title = "") +
    theme_bw() +
    theme(text = element_text(size = 13)) +
    scale_shape_manual(values = 0:7)+
    scale_x_continuous(labels = function(x){paste0("$",signif(x/1e9),"B")})+ 
    #scale_y_continuous(labels = function(x){sprintf("%.3f", round(abs(x),0))})+ # Keep for reformatting later (QALY gained)
    scale_y_continuous(labels = function(x){paste0(signif(x/1e6),"M")})+
    theme(legend.position = "right")
  
  plotCESensitivity
  ggsave(path = "Figures",
         filename = paste0("CE_plane_psa.",date.run,".jpeg"),
         height = 5,
         width = 9,
         units = "in",
         dpi = 700)
  
  plotCEACSensitivity <- ggplot(sensAllSummaryCEACwtp, aes(x = WTP,
                                           y = percentCE,
                                           color = InterventionName,
                                           linetype = as.factor(number.of.int)))+#,
    #shape = Strategy)) +
    geom_line()+
    labs(x = "Willingness-to-Pay Threshold (thousands USD/QALY gained", 
         y = "Percent of Runs Cost-Effective Compared to SQ", 
         color = "Strategy", 
         linetype = "Number of Interventions",
         title = "") +
    theme_bw() +
    theme(text = element_text(size = 13)) +
    theme(legend.position = "right")
  plotCEACSensitivity
  ggsave(path = "Figures",
         filename = paste0("fig3.ceac.",date.run,".jpeg"),
         height = 6,
         width = 9,
         units = "in",
         dpi = 700)
  
  plotCEACSensitivityPref <- ggplot(sensAllceacHighestNMB, aes(x = WTP,
                                                           y = percentPreferred,
                                                           color = InterventionName))+#,
    #shape = Strategy)) +
    geom_line()+
    labs(x = "Willingness-to-Pay Threshold (thousands USD/QALY gained", y = "Percent of Runs Intervention is Preferred", color = "Strategy", title = "") +
    theme_bw() +
    theme(text = element_text(size = 13)) +
    theme(legend.position = "right")
  
  plotCEACSensitivityPref
  ggsave(path = "Figures",
         filename = paste0("fig3b.ceac.pref.",date.run,".jpeg"),
         height = 5,
         width = 9,
         units = "in",
         dpi = 700)
  
}
