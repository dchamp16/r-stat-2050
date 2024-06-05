#############################################################
#
# Confidence interval for a mean
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

#### mean, SD, and SE
myMean = mean(ncha$N3Q69, na.rm = TRUE)
mySD = sd(ncha$N3Q69, na.rm = TRUE)
n = sum(!is.na(ncha$N3Q69))
mySE = mySD / sqrt(n)

#### t star
tStar = qt( (1 - .85) / 2, df = n - 1, lower.tail = FALSE )

#### confidence interval
myMean - tStar*mySE
myMean + tStar*mySE

cat("Mean:", myMean, "\n")
cat("Standard Deviation:", mySD, "\n")
cat("Standard Error:", mySE, "\n")
cat("t* value:", tStar, "\n")
cat("85% Confidence Interval:", lower_bound, "to", upper_bound, "\n")

