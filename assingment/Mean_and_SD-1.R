#############################################################
#
# Finding the mean and standard deviation
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
mean_N3Q18B <-mean(ncha$N3Q18B, na.rm = TRUE)

#### SD
sd_N3Q18B <- sd(ncha$N3Q18B, na.rm = TRUE)


# answer:

# Print the results for N3Q18B
print(paste("Mean of N3Q18B:", mean_N3Q18B))
print(paste("Standard Deviation of N3Q18B:", sd_N3Q18B))

# Mean and standard deviation for N3Q18C
mean_N3Q18C <- mean(ncha$N3Q18C, na.rm = TRUE)
sd_N3Q18C <- sd(ncha$N3Q18C, na.rm = TRUE)

# Print the results for N3Q18C
print(paste("Mean of N3Q18C:", mean_N3Q18C))
print(paste("Standard Deviation of N3Q18C:", sd_N3Q18C))
