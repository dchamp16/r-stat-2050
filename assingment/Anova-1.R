#############################################################
#
# ANOVA
#
#############################################################
## Load the library to read the excel file
library(readxl)


#### You will need to change this value for your own computer
current_dir <- dirname(normalizePath("."))
dataDir <- file.path(current_dir, "r-stat-2050", "datas")

### Read in the Dataset
ncha = read_excel(paste(dataDir, 
                        "NCHA-III WEB SPRING 2021 UTAH VALLEY UNIVERSITY  - TIMESTAMP.xlsx", 
                        sep = "/"), sheet = "NCHA-III WEB SPRING 2021 UTAH V")

ncha = as.data.frame(ncha)


myANOVA_exercise = aov(N3Q6 ~ N3Q4, data = ncha)
summary(myANOVA_exercise)

# Plot diagnostic plots to check assumptions
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(myANOVA_exercise)



# anova result
# f value: 1.091
# p value: 0.36
# conclusion: pvalue is greater than 0.05. fail to reject the null hypothesis. no big difference in mean on moderate exercise among the different weight classes (
  
# assumption
# q-q plot has a deviation from normality going up trend, residual and leverage plot indicated has influential points upward trend and residual and fitted looking like a downward trend
  
# conclusion:
# according to anova it fail to reject the null hypothesis, no significant diferenc in groups. anova is robust for assumptions, it provided a good result even though it is an assumption, but there is another approach to validate and ensure more reliable judgment
  
