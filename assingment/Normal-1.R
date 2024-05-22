#############################################################
#
# Normal distribution values
#
#############################################################
## Load the library to read the excel file
library(readxl)


#### You will need to change this value for your own computer
current_dir <- dirname(normalizePath("."))
dataDir <- file.path(current_dir, "Github", "r-stat-2050", "datas")

### Read in the Dataset
ncha = read_excel(paste(dataDir, 
                        "NCHA-III WEB SPRING 2021 UTAH VALLEY UNIVERSITY  - TIMESTAMP.xlsx", 
                        sep = "/"), sheet = "NCHA-III WEB SPRING 2021 UTAH V")

ncha = as.data.frame(ncha)

#### mean
myMean = mean(ncha$N3Q71, na.rm = TRUE)


#### SD
mySD = sd(ncha$N3Q71, na.rm = TRUE)

myVal = myMean - 1.23 * mySD
myVal

pnorm(myVal, mean = myMean, sd = mySD)
pnorm(myVal, mean = myMean, sd = mySD, lower.tail = FALSE)
