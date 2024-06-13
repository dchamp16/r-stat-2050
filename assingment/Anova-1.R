#############################################################
#
# ANOVA
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

#### ANOVA
myANOVA = aov(BMI ~ N3Q1, data = ncha)
summary(myANOVA)

plot(myANOVA)
# Conclusion: We reject H0 and conclude that the mean BMI is different for
#             at least one of the groups (overall health levels)
# Assumptions: maybe some non-constant variance (first bar of points is a little shorter)
#               but it looks to be within acceptable ranges. Some small deviation from
#               normality, but probably ok as well. 


