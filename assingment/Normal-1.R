#############################################################
#
# Normal distribution values
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
myMean = mean(ncha$N3Q60, na.rm = TRUE)


#### SD
mySD = sd(ncha$N3Q60, na.rm = TRUE)

myVal = myMean + .567*mySD
myVal

pnorm(myVal, mean = myMean, sd = mySD)
pnorm(myVal, mean = myMean, sd = mySD, lower.tail = FALSE)
