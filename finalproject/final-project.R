library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Define the endpoint and parameters for the USGS Earthquake API
endpoint <- "https://earthquake.usgs.gov/fdsnws/event/1/query"
params <- list(
  format = "geojson",
  starttime = "2000-01-01",
  endtime = "2023-12-31",
  minlatitude = 36.997966,
  maxlatitude = 42.001567,
  minlongitude = -114.052973,
  maxlongitude = -109.041058,
  limit = 20000  # Adjust limit if needed based on API guidelines
)

# Make the GET request to the API
response <- GET(endpoint, query = params)

# Check if the request was successful
if (response$status_code == 200) {
  data <- content(response, as = "parsed", type = "application/json")
  
  # Check if data has features
  if (length(data$features) == 0) {
    stop("No data returned. Check if the parameters are correct or adjust the time range.")
  }
  
  # Attempt to convert the features into a dataframe
  df <- tryCatch({
    do.call(rbind, lapply(data$features, function(x) {
      data.frame(
        time = if (!is.null(x$properties$time)) as.POSIXct(as.numeric(x$properties$time) / 1000, origin = "1970-01-01") else NA,
        magnitude = if (!is.null(x$properties$mag)) as.numeric(x$properties$mag) else NA,
        place = if (!is.null(x$properties$place)) as.character(x$properties$place) else NA,
        latitude = if (!is.null(x$geometry$coordinates[2])) as.numeric(x$geometry$coordinates[2]) else NA,
        longitude = if (!is.null(x$geometry$coordinates[1])) as.numeric(x$geometry$coordinates[1]) else NA,
        stringsAsFactors = FALSE
      )
    }))
  }, error = function(e) {
    cat("Error in converting features to dataframe: ", e$message, "\n")
    NULL  # Returning NULL if an error occurs
  })
  
  # Check if dataframe is created successfully
  if (is.null(df)) {
    stop("Failed to convert data to dataframe.")
  }
  
  # Display the structure and summary of the dataframe
  print(str(df))
  print(summary(df))
  
} else {
  stop(paste("Failed to fetch data from the USGS Earthquake API. Status code:", response$status_code))
}

# Save the dataframe for further analysis
save(df, file = "earthquake_data_utah.RData")



# Load necessary library
library(ggplot2)

# Plotting the distribution of earthquake magnitudes
ggplot(df, aes(x = magnitude)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Earthquake Magnitudes in Utah", x = "Magnitude", y = "Count")

# Time series plot of earthquakes over time (monthly counts)
df$month <- as.Date(format(df$time, "%Y-%m-01"))  # Creating a new column 'month' for aggregation
monthly_counts <- df %>% 
  group_by(month) %>%
  summarise(count = n())

ggplot(monthly_counts, aes(x = month, y = count)) +
  geom_line(color = "red") +
  labs(title = "Monthly Earthquake Counts in Utah", x = "Month", y = "Number of Earthquakes")

# Map of earthquake occurrences
library(ggmap)
utah_map <- get_map(location = c(lon = mean(c(-114.052973, -109.041058)), lat = mean(c(36.997966, 42.001567))), 
                    zoom = 7, maptype = "terrain")
ggmap(utah_map) +
  geom_point(data = df, aes(x = longitude, y = latitude, color = magnitude), alpha = 0.5, size = 1) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Map of Earthquake Occurrences in Utah", x = "Longitude", y = "Latitude")

