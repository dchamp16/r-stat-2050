#############################################################
#
# Chi square tests
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


goodness_of_fit_test <- chisq.test(table(ncha$N3Q12C), correct = FALSE, p = c(0.7, 0.15, 0.05, 0.1))

print("Goodness of Fit Test for N3Q12C:")
print(goodness_of_fit_test)


association_test <- chisq.test(table(ncha$N3Q12C, ncha$N3Q12D))

print("Test of Association between N3Q12C and N3Q12D:")
print(association_test)

