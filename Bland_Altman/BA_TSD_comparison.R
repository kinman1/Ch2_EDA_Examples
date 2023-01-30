
library(BlandAltmanLeh)
library(blandr)
library(captioner)
library(cowplot)
library(dplyr)
library(ggdist)
library(ggExtra)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(janitor)
library(kableExtra)
library(knitr)
library(readr)
library(tidyquant)
library(tidyr)
library(tidyverse)
library(zeallot)

source("/Volumes/InmanDrive4/Dropbox/Dropbox/KPI/University_of_Dundee/Dissertation/Lab Notebooks/library.R")
# Using just columns needed for graphing (Sample\#, program, logLR, efficiency, true contributor, and donor input DNA, stutter status)


MT <- read_csv("/Volumes/InmanDrive4/Dropbox/Dropbox/KPI/University_of_Dundee/Dissertation/Data/Output_files/AllResults_Data/AllResults_Table2.csv")
baEff <- MT %>% 
 pivot_wider(id_cols = c("Sample","TC","St","TotDNA", "NC"), names_from = "Pr", values_from = "Eff") %>% 
 rename("Eff.EFM" = EFM, "Eff.likeLTD" = likeLTD)

baEff <- na.omit(baEff)
                                        
balLR <- MT %>% 
 pivot_wider(id_cols = c("Sample","TC","St","TotDNA"), names_from = "Pr", values_from = "lLR") %>% 
 rename("TotlogLR.EFM" = EFM, "TotlogLR.likeLTD" = likeLTD)
 
balLR <- na.omit(balLR)


EffCompTSD <- TukeySumDiffPlotKPI(baEff$Eff.EFM, baEff$Eff.likeLTD, two = 3 )  +
  geom_point(aes(color = baEff$Stutter, shape = baEff$Stutter), size = 0.25) +
  labs(color = "Stutter", shape = "Stutter") +
  annotate("text", x = -0.05, y = .25, label = "EFM higher",
  family = "serif", fontface = "italic", colour = "darkred", size = 3) +
  annotate("text", x = -0.05, y = -0.2, label = "likeLTD higher",
  family = "serif", fontface = "italic", colour = "darkred", size = 3) +
  geom_smooth(method = lm, se = TRUE, color = "red", size = .3, fullrange = TRUE) +
  labs(title = "Tukey Sum-Difference Analysis\nComparing EFM & likeLTD", subtitle = "Efficiency using samples with or without stutter", caption = "Tukey SumDiff calculated using Efficiency") +
  theme_stata(base_family = "serif") +
  panel_border(color = "black") +
  scale_color_stata() +
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
  legend.position = "bottom", 
  legend.text = element_text(size = 8), 
  legend.title = element_text(size = 9), 
  plot.background = element_rect(fill = "lightgray"))
                                        
  EffCompTSD

  
  EffCompBA <- baPlotKPI(baEff$Eff.EFM, baEff$Eff.likeLTD, two = 3 )  +
    geom_point(aes(color = baEff$Stutter, shape = baEff$Stutter), size = 0.25) +
    labs(color = "Stutter", shape = "Stutter") +
    annotate("text", x = -0.05, y = .25, label = "EFM higher",
             family = "serif", fontface = "italic", colour = "darkred", size = 3) +
    annotate("text", x = -0.05, y = -0.2, label = "likeLTD higher",
             family = "serif", fontface = "italic", colour = "darkred", size = 3) +
    geom_smooth(method = lm, se = TRUE, color = "red", size = .3, fullrange = TRUE) +
    labs(title = "Bland-Altman Analysis\nComparing EFM & likeLTD", subtitle = "Efficiency using samples with or without stutter", caption = "Bland-Altman calculated using Efficiency") +
    theme_stata(base_family = "serif") +
    panel_border(color = "black") +
    scale_color_stata() +
    theme(plot.title = element_text(size = 12, hjust = 0.5), 
          legend.position = "bottom", 
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 9), 
          plot.background = element_rect(fill = "lightgray"))
  
  EffCompBA
  
  plot_grid(EffCompBA,EffCompTSD, nrow = 2)
  
  ggsave2("/Volumes/InmanDrive4/Dropbox/Dropbox/KPI/University_of_Dundee/Dissertation/Learning/Matloff_fasteR/BA vs. TukeySD comparison.pdf", width = 8, height = 10, units = "in" )
  