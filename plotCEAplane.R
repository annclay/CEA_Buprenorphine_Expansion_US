### Plotting the Cost-Effectiveness Frontier 
### By Anneke Claypool
### Last Updated 3/10/23

plotCEAplane <-function(icers, coveragePercent){ #icers <-   read.csv("C:/Users/ai219/Dropbox (Partners HealthCare)/Documents/Opioids/R Studio/Bup_CEA/Results/ICERsTable.2022-03-04.csv")
  
  
  library(ggrepel)
  
  runDate <- Sys.Date()
  icers <- as.data.table(icers)
  
  icerLabel <- paste0("$",round(icers$ICER,0),"/QALY gained")

  
  ##Plot version 1-- Include total costs and QALYs on the axes
  
  plotPlane <- ggplot()+

    geom_line(data = icers[Status == "ND"],
              aes(x = Cost, y = Effect))+

    geom_point(data = icers,
               aes(x = Cost, y = Effect), size=2)+ #color = Strategy"
    labs(x = "Total Cost (2021 USD)", y = "Quality-Adjusted Life Years", title = "") +#color = "Strategy",
    theme_bw() +
    scale_x_continuous(labels = function(x){paste0("$",signif(x/1e9),"B")})+ 
    scale_y_continuous(labels = function(x){paste0(signif(x/1e6),"M")})+
    theme(legend.position = "none")#+
  
  plotPlane <- plotPlane +
    geom_text_repel(data = icers,#[icers$Status !="D",],
                    aes_(x = as.name("Cost"),
                         y = as.name("Effect"),
                         label = as.name("abbrevName")),
                    size = 3,
                    show.legend = FALSE,
                    #max.iter = max.iter,
                    direction = "both")
  
  ##Plot version 2-- Include incremental costs and QALYs on the axes
  icers2 <- icers %>%
    mutate(IncCostSQ = Cost-Cost[Strategy == "Status Quo"],
           IncQALYSQ = Effect - Effect[Strategy == "Status Quo"])
  
  plotPlane2 <- ggplot()+
    geom_line(data = icers2[icers2$Status == "ND",],
              aes(x = IncCostSQ, y = IncQALYSQ))+
    geom_point(data = icers2,
               aes(x = IncCostSQ, y = IncQALYSQ), size=2)+
    labs(x = "Incremental Costs (2021 USD)", y = "Incremental Quality-Adjusted Life Years", color = "Strategy", title = "") +
    geom_vline(xintercept=0, linetype = "dotted") +
    theme_bw() +
    scale_x_continuous(labels = function(x){paste0("$",signif(x/1e9),"B")})+
    scale_y_continuous(labels = function(x){paste0(signif(x/1e6),"M")})+
    theme(legend.position = "none")#+

  
  plotPlane2 <- plotPlane2 +
    geom_text_repel(data = icers2,
                    aes_(x = as.name("IncCostSQ"),
                         y = as.name("IncQALYSQ"),
                         label = as.name("abbrevName")),
                    size = 3,
                    show.legend = FALSE,
                    #max.iter = max.iter,
                    direction = "both")
  
  #Save correct version
  #plotPlane
  plotPlane2
  ggsave(path = "Figures",
         filename = paste0("fig2.",coveragePercent,"cov",runDate,".jpeg"),
         height = 5,
         width = 7,
         units = "in",
         dpi = 700)
  
  ### Save submitted figures as .eps files
  setEPS()
  postscript(paste0("Figure2",runDate,".eps"))
  plotPlane2
  dev.off()
  
  return(plotPlane)
  
  
}