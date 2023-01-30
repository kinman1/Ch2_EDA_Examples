dev.off()
library (tidyverse)
library (blandr)
# setwd("//Volumes/InmanDrive4/Dropbox (Personal)/KPI/University of Dundee/Dissertation/Data/Statistical testing/Bland Altman/EFM data")

MT <- read.csv("/Users/KeithMacBookPro/Dropbox (Personal)/KPI/University_of_Dundee/Dissertation/Data/Output_files/AllResults_Data/Master_Table.csv")

baEFM <- MT %>%
      
        select(Sample,Program,TotlogLR,StFilt,TCon) %>%
        filter(Program == "EFM",StFilt =="off",TCon == "B")

baEFM <- na.omit(baEFM)
  
baLTD <- MT %>%
  
        select(Sample,Program,TotlogLR,StFilt,TCon) %>%
        filter(Program == "likeLTD",StFilt == "off", TCon == "B")
baLTD <- na.omit(baLTD)

ba <- baEFM %>%
  
      inner_join(baLTD, by = "Sample") %>% 
      select(Sample,TotlogLR.x,TotlogLR.y)

blandr.data.preparation(ba$TotlogLR.x, ba$TotlogLR.y, sig.level = 0.95) # Check on data to ensure integrity and adherence to proper format

# blandr.method.comparison(LabRet,likeLTD, sig.level = 0.95); a simple comparison method that shows correlation but is misleading. 

bland.stats <- blandr.statistics(ba$TotlogLR.x, ba$TotlogLR.y) #Prepares statistics used in calculating the Bland-Altman plots for methods Lab Retriever and likeLTD)

#dif.methods <- bland.stats$differences #creates an object containing the paired differences between methods; $differences are values created during function
#write.table (dif.methods, file = "Bland-Altman differences.txt") #Save the differences in a txt file
#sum.methods <- sum(dif.methods) #Sum the differences to get the overall differences, indicating whether there's an overall difference

blandr.draw(ba$TotlogLR.x, ba$TotlogLR.y, method1name = "EFM",
            method2name = "likeLTD",
            plotTitle = "Bland-Altman plot: EFM/likeLTD stutter off",
            sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
            ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
            lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
            overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
            y.plot.mode = "difference", plotProportionalBias = FALSE,
            plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE)


bland.stats <- blandr.statistics(ba$TotlogLR.x, ba$TotlogLR.y) #Prepares statistics used in calculating the Bland-Altman plots for methods Lab Retriever and likeLTD)

dif.methods <- bland.stats$differences #creates an object containing the paired differences between methods; $differences are values created during function
sum.methods <- sum(dif.methods) #Sum the differences to get the overall differences, indicating whether there's an overall difference
write.table (sum.methods, file = "Bland-Altman Stutter on_off.txt") #Save the differences in a txt file



baEFM %>%
  count(TCon)
baLTD %>%
  count(TCon)
