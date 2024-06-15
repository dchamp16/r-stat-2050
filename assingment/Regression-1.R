#############################################################
#
# Regression
#
#############################################################
## Load the library to read the excel file
library(readxl)


#### You will need to change this value for your own computer
current_dir <- dirname(normalizePath("."))
dataDir <- file.path(current_dir, "GitHub","r-stat-2050", "datas")
### Read in the Dataset
ncha = read_excel(paste(dataDir, 
                        "NCHA-III WEB SPRING 2021 UTAH VALLEY UNIVERSITY  - TIMESTAMP.xlsx", 
                        sep = "/"), sheet = "NCHA-III WEB SPRING 2021 UTAH V")

ncha = as.data.frame(ncha)

#### regression
myReg = lm(N3Q9A ~ N3Q7, data = ncha)
summary(myReg)

# Calculate and print the correlation
correlation <- cor(ncha$N3Q7, ncha$N3Q9A, use = "complete.obs")  # Ensuring no NA values interfere
print(correlation)

# Extracting the t-statistic and p-value from the regression summary
t_statistic <- summary(myReg)$coefficients[2,3]
p_value <- summary(myReg)$coefficients[2,4]

print(paste("t-statistic:", t_statistic))
print(paste("p-value:", p_value))
ss
# ssince the pvalue 0.05, 0.217 to be exact. we dont reject the null hypothesis does not have a big impact on this group when drinking sugary drink and doing intense workout. statistically it says that the relationship between drinking sugary drink and exercising is not significant. so encouraging doing more minutes of workout will not do better redusing sugary consumption. there are more better option trying to lower the sugar drink intake.


