#############################################################
#
# Creating various plots in R
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

#### Pie chart
pie(table(ncha$N3Q1), main = "Overall Health")


#### Bar Chart
barplot(table(ncha$N3Q1), main = "Overall Health")


#### Histogram
hist(ncha$N3Q6, main = "Hours of Moderate Exercise", xlab = "Hours")


#### Boxplot
boxplot(ncha$N3Q6, main = "Hours of Moderate Exercise", xlab = "Hours")


#### Side-by-Side boxplots
boxplot(ncha$N3Q6 ~ ncha$N3Q1, ylab = "Hours of Moderate Exercise", xlab = "Overall Health")


# Answers:

#### Pie chart for N3Q5 (Categorical Variable)
pie(table(ncha$N3Q5), main = "Are you trying to do any of the following about your weight?")

#### Bar Chart for N3Q5 (Categorical Variable)
barplot(table(ncha$N3Q5), main = "Are you trying to do any of the following about your weight?")

#### Histogram for N3Q9A (Quantitative Variable)
hist(ncha$N3Q9A, main = "Number of Sugar-Sweetened Beverages in a Day", xlab = "Number of Beverages")

#### Boxplot for N3Q9A (Quantitative Variable)
boxplot(ncha$N3Q9A, main = "Number of Sugar-Sweetened Beverages in a Day", ylab = "Number of Beverages")

#### Side-by-Side Boxplots for N3Q9A by N3Q5 (Quantitative vs Categorical Variable)
boxplot(ncha$N3Q9A ~ ncha$N3Q5, ylab = "Number of Sugar-Sweetened Beverages", xlab = "Weight Management Effort")

