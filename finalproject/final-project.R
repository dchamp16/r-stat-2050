# Install necessary packages
packages <- c("httr", "jsonlite", "dplyr", "ggplot2", "fitdistrplus", 
              "survival", "forecast", "spatstat", "raster", "sf", "caret", "car")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(survival)
library(forecast)
library(spatstat)
library(raster)
library(sf)
library(caret)
library(car)

# Define the API endpoint and parameters
endpoint <- "https://earthquake.usgs.gov/fdsnws/event/1/query"
params <- list(
  format = "geojson",
  starttime = "2000-01-01",
  endtime = "2023-12-31",
  minlatitude = 36.997966,
  maxlatitude = 42.001567,
  minlongitude = -114.052973,
  maxlongitude = -109.041058,
  limit = 20000
)

# Fetch data from USGS API
response <- GET(endpoint, query = params)
stop_for_status(response)

# Parse the JSON response
data <- content(response, "parsed", type = "application/json")
if (length(data$features) == 0) {
  stop("No data returned. Adjust the time range or check the parameters.")
}

# Convert JSON data to a DataFrame
df <- do.call(rbind, lapply(data$features, function(x) {
  if (!is.null(x$properties$time) && !is.null(x$properties$mag) &&
      !is.null(x$geometry$coordinates[1]) && !is.null(x$geometry$coordinates[2])) {
    data.frame(
      time = as.POSIXct(x$properties$time / 1000, origin = "1970-01-01"),
      magnitude = as.numeric(x$properties$mag),
      latitude = as.numeric(x$geometry$coordinates[2]),
      longitude = as.numeric(x$geometry$coordinates[1]),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}))

# Ensure data has no missing values for magnitude and latitude
df <- df %>% filter(!is.na(magnitude) & !is.na(latitude))

# Creating a 'segment' variable based on latitude
df$segment <- cut(df$latitude, 
                  breaks = c(-Inf, 37.5, 40, Inf), 
                  labels = c("South", "Central", "North"))

# Check distribution of segment
table(df$segment)

# Run ANOVA to compare magnitudes across segments
anova_results <- aov(magnitude ~ segment, data = df)
summary_anova <- summary(anova_results)

# Display the ANOVA summary
print(summary_anova)

# Plot the results
ggplot(df, aes(x = segment, y = magnitude)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Earthquake Magnitudes by Segment", x = "Segment", y = "Magnitude")

# Save results to a file for further use
save(df, anova_results, file = "earthquake_analysis_results.RData")

