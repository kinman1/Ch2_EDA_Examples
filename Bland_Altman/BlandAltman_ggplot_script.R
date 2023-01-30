
dev.off()
library (tidyverse)
library (blandr)

# Create dataframe and extract specific data for Bland Altman analysis

MT <- read.csv("/Volumes/InmanDrive4/Dropbox (Personal)/KPI/University_of_Dundee/Dissertation/Data/Output_files/AllResults_Data/Master_Table.csv")

MT$TotDNA <- as.factor(MT$TotDNA)

# Subset dataframe with LR from one program (including columns needed for the subsetting task)
baEFM <- MT %>%
  
  
  select(Sample,Program,TotlogLR,StFilt,TCon,TotDNA) %>%
  filter(Program == "EFM",StFilt =="off",TCon == "B")

baEFM <- na.omit(baEFM)

# Subset dataframe with LR from the other program (including columns needed for the subsetting task)
baLTD <- MT %>%
  
  select(Sample,Program,TotlogLR,StFilt,TCon,TotDNA) %>%
  filter(Program == "likeLTD",StFilt == "off", TCon == "B")
baLTD <- na.omit(baLTD)

# Merge subsetted dataframes into one for Bland Altman calculations
ba <- baEFM %>%
  
  inner_join(baLTD, by = "Sample") %>% 
  select(Sample,TotlogLR.x,TotlogLR.y,TotDNA.x,TotDNA.y)


# Check on data to ensure integrity and adherence to proper format
blandr.data.preparation(ba$TotlogLR.x, ba$TotlogLR.y, sig.level = 0.95) 


#Prepares statistics used in calculating the Bland-Altman plots for methods Lab Retriever and likeLTD)
bland.stats <- blandr.statistics(ba$TotlogLR.x, ba$TotlogLR.y) 

####################################################################
# Standard bland altman plot, using the script from the blandr package
blandr.draw(ba$TotlogLR.x, ba$TotlogLR.y, method1name = "EFM",
            method2name = "likeLTD",
            plotTitle = "Bland-Altman plot: EFM/likeLTD stutter off",
            sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
            ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
            lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
            overlapping = FALSE, plotter = "ggplot2", x.plot.mode = "means",
            y.plot.mode = "difference", plotProportionalBias = FALSE,
            plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE)


####################################################################
# Various plot options using ggplot; can use in addition to or separate from the blandr.draw function

# Scatter plot with 4 plots, one for each amount of Total DNA (TotDNA) of the sample
ggplot(data = ba) +
  geom_point(mapping = aes(x = bland.stats$means, y = bland.stats$differences), color = "blue") +
  facet_wrap(~ TotDNA.y) +
  labs(title = "Bland-Altman plot \nEFM/likeLTD stutter off", x = "Differences", y = "Means") +
  theme_bw()

# Scatter plot with single plot, each amount of TotDNA color-coded
ggplot(data = ba) +
  geom_point(mapping = aes(x = bland.stats$means, y = bland.stats$differences, color = TotDNA.y)) +
  labs(title = "Bland-Altman plot \nEFM/likeLTD stutter off", x = "Differences", y = "Means") +
  theme_bw()

# Smoothed plots of 4 DNA amounts (not that informative)
ggplot(data = ba) +
  geom_smooth(mapping = aes(x = bland.stats$means, y = bland.stats$differences, color = TotDNA.y),
              show.legend = TRUE) +
  labs(title = "Bland-Altman plot \nEFM/likeLTD stutter off", x = "Differences", y = "Means") +
  theme_bw()

# All data with a single smoothed line, with TotDNA color-coded
ggplot(data = ba, mapping = aes(x = bland.stats$means, y = bland.stats$differences)) +
  geom_point(mapping = aes(color = TotDNA.y, shape = TotDNA.y)) +
  geom_smooth() +
  labs(title = "Bland-Altman plot \nEFM/likeLTD stutter off", x = "Differences", y = "Means") +
  theme_bw()

# All data with smoothed line for each amt of TotDNA
ggplot(data = ba, mapping = aes(x = bland.stats$means, y = bland.stats$differences)) +
  geom_point(mapping = aes(color = TotDNA.y, shape = TotDNA.y)) +
  geom_smooth(mapping = aes(x = bland.stats$means, y = bland.stats$differences, color = TotDNA.y),
            show.legend = TRUE) +
            labs(title = "Bland-Altman plot \nEFM/likeLTD stutter off", x = "Differences", y = "Means") +
            theme_bw()

# 4 plots, one for each amt of TotDNA, including smooth line, color, and shapes
ggplot(data = ba, mapping = aes(x = bland.stats$means, y = bland.stats$differences)) +
      geom_point(mapping = aes(color = TotDNA.y, shape = TotDNA.y)) +
      geom_smooth(mapping = aes(x = bland.stats$means, y = bland.stats$differences, color = TotDNA.y),
              show.legend = TRUE) +
      facet_wrap(~ TotDNA.y) +
      labs(title = "Bland-Altman plot \nEFM/likeLTD stutter off", x = "Differences", y = "Means") +
      theme_bw()



###################################################################

