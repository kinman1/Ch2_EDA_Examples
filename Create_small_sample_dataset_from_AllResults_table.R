
# Created 2023.01.28 KPI
# This script creates a small data subset for use in testing any new code or as sample data for demonstrating EDA or analysis code used in the dissertation. It is derived from the complete data file containing all of the LRs and parameters from the EFM and likeLTD runs.

# Read in the data from the latest AllResults table.
data <- read_csv("/Volumes/InmanDrive4/Dropbox/Dropbox/KPI/University_of_Dundee/Dissertation/Data/Output_files/AllResults_Data/AllResults_Table_4.csv")

# Subset the data to include only the samples where the total DNA input was 500 or 100 pg
tmp <- data[data$TotDNA == 500 | data$TotDNA == 100, ]

# Further subset the data to include only Contributor B, who was used as a contributor in 2, 3, and 4 person mixtures
tmp <- tmp[tmp$TC %in% "B", ]

# Save the subset to file for use in testing or demonstration code
write_csv(tmp, "/Volumes/InmanDrive4/Dropbox/Dropbox/KPI/University_of_Dundee/Dissertation/Lab Notebooks/Ch2_EDA_Examples/sample_data.csv")
