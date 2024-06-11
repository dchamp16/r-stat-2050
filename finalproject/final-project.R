# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(leaflet)

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
  limit = 20000
)

# Make the GET request to the API
response <- GET(endpoint, query = params)

# Handle the API response
if (response$status_code == 200) {
  data <- content(response, "parsed", type = "application/json")
  
  if (length(data$features) == 0) {
    stop("No data returned. Adjust the time range or check the parameters.")
  }
  
  # Convert the features into a dataframe
  df <- tryCatch({
    do.call(rbind, lapply(data$features, function(x) {
      data.frame(
        time = if (!is.null(x$properties$time)) as.POSIXct(as.numeric(x$properties$time) / 1000, origin = "1970-01-01") else NA,
        magnitude = if (!is.null(x$properties$mag)) as.numeric(x$properties$mag) else NA,
        place = if (!is.null(x$properties$place)) as.character(x$properties$place) else NA,
        latitude = if (!is.null(x$geometry$coordinates[2])) as.numeric(x$geometry$coordinates[2]) else NA,
        longitude = if (!is.null(x$geometry$coordinates[1])) as.numeric(x$geometry$coordinates[1]) else NA
      )
    }))
  }, error = function(e) {
    cat("Error in data conversion: ", e$message, "\n")
    NULL  # Return NULL if an error occurs
  })
  
  if (is.null(df)) {
    stop("Failed to convert data to dataframe.")
  }
  
  # Filter out non-finite values in the magnitude column
  df <- df %>% filter(is.finite(magnitude))
  
} else {
  stop(paste("Failed to fetch data: Status code", response$status_code))
}

# Visualization and Summary Statistics
# Histogram of earthquake magnitudes
ggplot(df, aes(x = magnitude)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Earthquake Magnitudes in Utah", x = "Magnitude", y = "Count")

# Time series plot of monthly earthquake counts
df$month <- as.Date(format(df$time, "%Y-%m-01"))
monthly_counts <- df %>%
  group_by(month) %>%
  summarise(count = n())

ggplot(monthly_counts, aes(x = month, y = count)) +
  geom_line(color = "red") +
  labs(title = "Monthly Earthquake Counts in Utah", x = "Month", y = "Number of Earthquakes")

# Assuming 'df' is your data frame with longitude, latitude, magnitude, and place information
# Prepare the Leaflet map
leaflet_map <- leaflet(df) %>%
  addProviderTiles("OpenStreetMap") %>%  # Using OpenStreetMap tiles
  addCircleMarkers(
    ~longitude, 
    ~latitude, 
    radius = ~magnitude,  # Setting the radius proportional to the magnitude
    popup = ~paste("Place:", place, "<br/>Magnitude:", magnitude),  # Popup text
    color = ~ifelse(magnitude > 4, "red", "blue"),  # Color condition
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "bottomright", 
    pal = colorNumeric(palette = c("blue", "red"), domain = df$magnitude),  # Color palette
    values = df$magnitude,
    title = "Magnitude",
    opacity = 0.7
  )

# Print the map to view in RStudio's Viewer pane
print(leaflet_map)

# Additionally, save the map as an HTML file to view in any web browser
library(htmlwidgets)  # Load htmlwidgets to use saveWidget
saveWidget(leaflet_map, "EarthquakeMap.html", selfcontained = TRUE)


# Print the summary statistics
summary_stats <- summary(df$magnitude)
print(summary_stats)
