# Install necessary packages
packages <- c("httr", "jsonlite", "dplyr", "ggplot2", "leaflet", "RColorBrewer", "scales")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(scales)

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

# Check if the 'time' column exists and is in the correct format
if("time" %in% names(df) && inherits(df$time, "POSIXct")) {
  # Create the 'time_year' column
  df$time_year <- format(df$time, "%Y")
} else {
  stop("The 'time' column does not exist or is not in POSIXct format. Please check your data.")
}


# Group by 'time_year' and summarize
yearly_stats <- df %>%
  group_by(time_year) %>%
  summarize(
    average_magnitude = mean(magnitude, na.rm = TRUE),  # Calculate average, handling NA values
    sd_magnitude = sd(magnitude, na.rm = TRUE)  # Calculate standard deviation
  )
print(yearly_stats)

# Plot average magnitude and its variability
ggplot(df, aes(x = longitude, y = latitude)) +
  geom_density_2d_filled(aes(fill = after_stat(level)), adjust = 1.5) +
  scale_fill_brewer(palette = "Spectral") +  # Using a Brewer palette for discrete data
  labs(title = "Density Map of Earthquake Occurrences", x = "Longitude", y = "Latitude") +
  coord_fixed(ratio = 1) +
  theme_minimal()


magnitude_palette <- colorQuantile("YlOrRd", df$magnitude, n = 5)

# Define colors manually
breaks <- quantile(df$magnitude, probs = seq(0, 1, length.out = length(colors) + 1), na.rm = TRUE)

# Colors must match the number of intervals created by the breaks
colors <- brewer.pal(9, "YlOrRd")[c(1, 3, 5, 7, 9)]  # For example, choose 5 colors

# Create labels for the legend
labels <- sprintf("%.2f - %.2f", head(breaks, -1), tail(breaks, -1))


# Create a leaflet map and add earthquake data with colors based on magnitude
map <- leaflet(df) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircles(
    lng = ~longitude, lat = ~latitude,
    weight = 1, radius = ~magnitude * 1000,
    color = ~magnitude_palette(magnitude),
    fillOpacity = 0.5, opacity = 0.5,
    popup = ~paste("Magnitude:", magnitude)
  ) %>%
  addProviderTiles(providers$Stamen.Toner) %>%  # Adds a nicer map aesthetic
  setView(lng = mean(df$longitude, na.rm = TRUE), lat = mean(df$latitude, na.rm = TRUE), zoom = 6)



# Add a legend to the map
# Add the legend to the map using only 'colors'
map %>% addLegend(
  position = "bottomright",
  colors = colors,  # Explicitly provide colors
  labels = labels,  # Use the manually created labels
  title = "Earthquake Magnitude",
  opacity = 1.0
)

# Histogram of earthquake magnitudes
ggplot(df, aes(x = magnitude)) +
  geom_histogram(bins = 30, fill = "tomato", color = "black") +
  labs(title = "Distribution of Earthquake Magnitudes", x = "Magnitude", y = "Frequency") +
  theme_minimal()

# Creating a time series plot of earthquake occurrences
df$date <- as.Date(df$time)  # Ensure 'time' is converted to Date format if needed
df %>% 
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.numeric(year), y = count)) +
  geom_line(group=1) +
  labs(title = "Annual Earthquake Frequency", x = "Year", y = "Number of Earthquakes") +
  theme_minimal()

# Boxplot of magnitudes by year
df %>% 
  mutate(year = format(time, "%Y")) %>%
  ggplot(aes(x = year, y = magnitude)) +
  geom_boxplot() +
  labs(title = "Earthquake Magnitudes by Year", x = "Year", y = "Magnitude") +
  theme_minimal()


