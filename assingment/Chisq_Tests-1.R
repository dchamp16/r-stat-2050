#############################################################
#
# Chi square tests
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

#### Goodness of fit test
# H0: The proportions are as stated
# Ha: At least one proportion is different
chisq.test(table(ncha$N3Q5), correct = FALSE, 
            p = c(.1, .2, .5, .2))

# Conclusion: We fail to reject H0 and cannot conclude any of the proportions 
#             is different than stated.


#### Chi square test of independence
# H0: The variables are not associated
# Ha: The variables are associated
chisq.test(table(ncha$N3Q12A, ncha$N3Q12B))

# Conclusion: We reject H0 and conclude that there is an association between the variables.

