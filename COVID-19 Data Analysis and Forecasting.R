# Install necessary packages if not installed
install.packages(c("tidyverse", "lubridate", "forecast", "ggplot2"))

# Load the required libraries
library(tidyverse)   # For data wrangling & visualization
library(lubridate)   # For handling dates
library(forecast)    # For time series forecasting
library(ggplot2)     # For plotting
library(dplyr)

# Load the COVID-19 dataset from OWID
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
covid_data <- read.csv(url)

# Overview of the dataset
glimpse(covid_data)  # View column names and types
head(covid_data)     # View the first few rows of the dataset

# Select relevant columns for analysis
covid_data <- covid_data %>%
  select(location, date, total_cases, new_cases, total_deaths, new_deaths)

# Convert date column to Date type
covid_data$date <- as.Date(covid_data$date)

# Replace any missing values with 0
covid_data[is.na(covid_data)] <- 0

# Filter data for India as an example
india_data <- covid_data %>% filter(location == "India")

# -------------------- Plot 1: Daily New COVID-19 Cases in India --------------------
ggplot(india_data, aes(x = date, y = new_cases)) +
  geom_line(color = "blue") +
  labs(title = "Daily COVID-19 Cases in India", x = "Date", y = "New Cases") +
  theme_minimal()

# -------------------- Plot 2: Total COVID-19 Cases in India --------------------
ggplot(india_data, aes(x = date, y = total_cases)) +
  geom_line(color = "red") +
  labs(title = "Total COVID-19 Cases in India", x = "Date", y = "Total Cases") +
  theme_minimal()

# -------------------- Plot 3: Top 10 Countries by Total COVID-19 Cases --------------------
# Filter out non-country entries (e.g., World, Europe, etc.)
total_cases_by_country <- covid_data %>%
  group_by(location) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  filter(!(location %in% c("World", "High-income countries", "Asia", 
                           "Europe", "Upper-middle-income countries", 
                           "European Union (27)", "North America", 
                           "United States", "Lower-middle-income countries", 
                           "South America"))) %>%
  top_n(10)

# Plotting top 10 countries by total cases with color gradient
ggplot(total_cases_by_country, aes(x = reorder(location, total_cases), y = total_cases, fill = total_cases)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "red") +  # Color gradient from lightblue to red
  coord_flip() +
  labs(title = "Top 10 Countries by Total COVID-19 Cases", 
       x = "Country", y = "Total Cases") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend for fill


# -------------------- Plot 4: COVID-19 Forecast for Next 30 Days --------------------
# Create a time series object for new cases in India
ts_data <- ts(india_data$new_cases, start = c(2020, 1), frequency = 365)

# Fit an ARIMA model to the data
model <- auto.arima(ts_data)

# Forecasting for the next 30 days
forecast_cases <- forecast(model, h = 30)

# Plotting the forecast
autoplot(forecast_cases) +
  labs(title = "COVID-19 Case Forecast for Next 30 Days", x = "Time", y = "Predicted Cases") +
  theme_minimal()

# -------------------- Additional Plots --------------------
# Check if 'total_recovered' exists before calculating recovery rate
# Check if 'total_recovered' exists before calculating recovery rate
if("total_recovered" %in% colnames(covid_data)) {
  # Add recovery rate column
  covid_data <- covid_data %>%
    mutate(recovery_rate = ifelse(total_cases == 0, NA, (total_recovered / total_cases) * 100))  # Avoid division by zero
  
  # Calculate total cases and recovery rate by country
  country_data <- covid_data %>%
    group_by(location) %>%
    summarise(total_cases = max(total_cases, na.rm = TRUE), 
              average_recovery_rate = mean(recovery_rate, na.rm = TRUE)) %>%
    arrange(desc(total_cases)) %>%
    filter(!(location %in% c("World", "High-income countries", "Asia", 
                             "Europe", "Upper-middle-income countries", 
                             "European Union (27)", "North America", 
                             "United States", 
                             "Lower-middle-income countries", 
                             "South America"))) %>%
    top_n(10)
  
  # Plot the total cases and recovery rate for the top 10 countries
  ggplot(country_data, aes(x = reorder(location, total_cases))) +
    geom_bar(aes(y = total_cases), stat = "identity", fill = "blue", alpha = 0.6) +
    geom_point(aes(y = average_recovery_rate * 1000), color = "green", size = 3) +  # Scaling recovery rate for better visibility
    scale_y_continuous(sec.axis = sec_axis(~ . / 1000, name = "Recovery Rate (%)")) +  # Second y-axis for recovery rate
    labs(title = "Top 10 Countries by Total Cases and Recovery Rate", 
         x = "Country", y = "Total Cases") +
    theme_minimal() +
    theme(legend.position = "none")
  
} else {
  message("The 'total_recovered' column is missing, so recovery rate analysis can't be performed.")
}

# Plot 7: COVID-19 Cases vs Total Deaths (Scatter Plot)
ggplot(covid_data, aes(x = total_cases, y = total_deaths, color = location)) +
  geom_point() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "COVID-19 Cases vs Total Deaths by Country", 
       x = "Total Cases", y = "Total Deaths") +
  theme_minimal() +
  theme(legend.position = "none")

# -------------------- Saving Cleaned Data --------------------
# Save cleaned data to CSV for future use
write.csv(india_data, "cleaned_covid_data.csv", row.names = FALSE)
