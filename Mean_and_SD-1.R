#############################################################
#
# Finding the mean and standard deviation
#
#############################################################
## Load the library to read the excel file
library(readxl)


#### You will need to change this value for your own computer
dataDir = "/Users/10975067/Documents/John/UVU/Classes/STAT 2040/2040 Ritems/ncha/Files"

### Read in the Dataset
ncha = read_excel(paste(dataDir, 
                "NCHA-III WEB SPRING 2021 UTAH VALLEY UNIVERSITY  - TIMESTAMP.xlsx", 
                sep = "/"), sheet = "NCHA-III WEB SPRING 2021 UTAH V")

ncha = as.data.frame(ncha)

#### mean
mean(ncha$N3Q18B, na.rm = TRUE)


#### SD
sd(ncha$N3Q18B, na.rm = TRUE)

