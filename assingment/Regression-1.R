#############################################################
#
# Regression
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

#### regression
myReg = lm(BMI ~ N3Q6, data = ncha)
summary(myReg)

# Interpretation of Slope: With a slope of -0.00096, we estimate that BMI decreases 
#                           by 0.00096 for every extra minute of moderate exercise.
# Conclusion: We fail to reject H0 and cannot conclude that there is a significant 
#             linear relationship between minutes of moderate exercise and BMI.



