library(readxl)

# Reading the Excel file
current_dir <- dirname(normalizePath("."))
dataDir <- file.path(current_dir, "r-stat-2050", "datas")

### Read in the Dataset
ncha = read_excel(paste(dataDir, 
                        "NCHA-III WEB SPRING 2021 UTAH VALLEY UNIVERSITY  - TIMESTAMP.xlsx", 
                        sep = "/"), sheet = "NCHA-III WEB SPRING 2021 UTAH V")
ncha = as.data.frame(ncha)

t_test_exercise = t.test(ncha$N3Q6, mu = 250)
print(t_test_exercise)

t_test_weight_diff = t.test(ncha$N3Q6[ncha$N3Q4 == "About the right weight"], ncha$N3Q6[ncha$N3Q4 == "Slightly overweight"], var.equal = TRUE)
print(t_test_weight_diff)

prop_test_right_weight = prop.test(x = sum(ncha$N3Q4 == "About the right weight"), n = nrow(ncha), p = 0.5)
print(prop_test_right_weight)

data_sub = ncha[ncha$N3Q4 %in% c("About the right weight", "Slightly overweight"),]
prop_test_excellent_weight = prop.test(x = table(data_sub$N3Q1 == "Excellent", data_sub$N3Q4))
print(prop_test_excellent_weight)
