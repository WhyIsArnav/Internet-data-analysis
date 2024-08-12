# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(scales)
library(plotly)
library(janitor)


# Set working directory and read data
setwd('~/Documents/ITS Summer')
tests <- read.csv('Post summer stress test copy.csv')

# Data processing and cleaning
all_data <- tests %>%
  clean_names() %>% # Clean column names for consistency
  filter(building == 'Old Main') %>%
  select(1:10) %>%
  mutate(
    timestamp = as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M:%S"),
    date = as.Date(timestamp), # Extract date part
    time = format(timestamp, "%H:%M:%S"), # Extract time part
    download_speed_reported_mbps = as.numeric(gsub("[^0-9.]", "", download_speed_reported_mbps)),
    upload_speed_reported_mbps = as.numeric(gsub("[^0-9.]", "", upload_speed_reported_mbps)),
    season = case_when(
      format(timestamp, "%m") %in% c("06", "07", "08") ~ "Summer",
      format(timestamp, "%m") %in% c("03", "04", "05") ~ "Spring",
      TRUE ~ "Other"
    ),
    type = case_when(
      season == "Spring" ~ "Wireless", # All Spring data is classified as Wireless
      were_you_able_to_run_the_speedtest_from_ac1 == "Yes" ~ "Wireless",
      were_you_able_to_run_the_speedtest_from_ac1 == "Wired speedtest" ~ "Wired",
      TRUE ~ "Other"
    ),
    room_category = case_when(
      standardized_room %in% c(005, 021, 028, 2, 3, 4, 5, 6, 7, 8, 9, 13, 22, 24, 25, 21, 26, 28, 31, 32) ~ "Basement",
      standardized_room %in% c(101, 102, 103, 104, 105, 106, 108, 109, 110, 111, 113, 114, 120, 122, 123, 124, 125, 128, 129, 135, 117, 132, 127) ~ "First Floor",
      standardized_room %in% c(302, 303, 304, 305, 306, 307, 308, 329, 310, 311, 312, 313, 314, 315, 316, 320, 323, 324, 325, 326, 327, 330, 331, 332, 333) ~ "Third Floor",
      TRUE ~ "Other"
    )
  ) %>%
  filter(
    !(season == "Summer" & type == "Wireless" & (download_speed_reported_mbps > 500 | upload_speed_reported_mbps > 500)) &
      !(season == "Spring" & upload_speed_reported_mbps > 2000)
  )

# Filter the data to include only the rows where the date is "2024-07-24" and the time is between "08:26:37" and "11:03:10"
stress_test_df <- all_data %>%
  filter(
    date == as.Date("2024-07-24") &
      time >= "08:27:37" &
      time <= "11:03:10"
  )
  
all_data <- all_data %>%
  filter(
    !(date == as.Date("2024-07-24") &
        time >= "08:26:37" &
        time <= "11:03:10")
  )

# PAGE (2)
# Function to summarize data
summarize_data <- function(data) {
  data %>%
    summarize(
      mean_download = mean(download_speed_reported_mbps, na.rm = TRUE),
      median_download = median(download_speed_reported_mbps, na.rm = TRUE),
      sd_download = sd(download_speed_reported_mbps, na.rm = TRUE),
      mean_upload = mean(upload_speed_reported_mbps, na.rm = TRUE),
      median_upload = median(upload_speed_reported_mbps, na.rm = TRUE),
      sd_upload = sd(upload_speed_reported_mbps, na.rm = TRUE)
    )
}

summer_wireless_summary <- all_data %>% filter(season == "Summer", type == "Wireless") %>% summarize_data()
spring_wireless_summary <- all_data %>% filter(season == "Spring", type == "Wireless") %>% summarize_data()
spring_wired_summary <- all_data %>% filter(season == "Spring", type == "Wired") %>% summarize_data()


# PAGE (3)
# Plotting function for download and upload speeds
plot_speeds <- function(data, season, test_type) {
  # Reshape the data for plotting
  long_data <- data %>%
    pivot_longer(cols = c(download_speed_reported_mbps, upload_speed_reported_mbps),
                 names_to = "SpeedType",
                 values_to = "Speed")
  
  ggplot(long_data, aes(x = SpeedType, y = Speed, fill = SpeedType)) +
    geom_boxplot(width = 0.5) +
    scale_x_discrete(labels = c("download_speed_reported_mbps" = "Download Speed", "upload_speed_reported_mbps" = "Upload Speed")) +
    labs(
      title = paste(season, test_type, "Speeds"),
      x = "",
      y = "Speed (Mbps)"
    ) +
    scale_fill_manual(values = c("download_speed_reported_mbps" = "blue", "upload_speed_reported_mbps" = "green")) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Plotting function for download and upload speeds with error bars
plot_speeds_bar <- function(data, season, test_type) {
  # Calculate mean and standard deviation for download and upload speeds
  summary_data <- data %>%
    pivot_longer(cols = c(download_speed_reported_mbps, upload_speed_reported_mbps),
                 names_to = "SpeedType",
                 values_to = "Speed") %>%
    group_by(SpeedType) %>%
    summarize(
      MeanSpeed = mean(Speed, na.rm = TRUE),
      SDSpeed = sd(Speed, na.rm = TRUE)
    )
  
  ggplot(summary_data, aes(x = SpeedType, y = MeanSpeed, fill = SpeedType)) +
    geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7)) +
    geom_errorbar(aes(ymin = MeanSpeed - SDSpeed, ymax = MeanSpeed + SDSpeed),
                  width = 0.2, position = position_dodge(0.7)) +
    scale_x_discrete(labels = c("download_speed_reported_mbps" = "Download Speed", "upload_speed_reported_mbps" = "Upload Speed")) +
    labs(
      title = paste(season, test_type, "Average Speeds with Error Bars"),
      x = "",
      y = "Speed (Mbps)"
    ) +
    scale_fill_manual(values = c("download_speed_reported_mbps" = "blue", "upload_speed_reported_mbps" = "green")) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# PAGE (4)
all_data <- all_data %>%
  mutate(hour = case_when(
    format(timestamp, "%H:%M:%S") >= "08:00:00" & format(timestamp, "%H:%M:%S") < "10:00:00" ~ "8-10am",
    format(timestamp, "%H:%M:%S") >= "10:00:00" & format(timestamp, "%H:%M:%S") < "12:00:00" ~ "10-12pm",
    format(timestamp, "%H:%M:%S") >= "12:00:00" & format(timestamp, "%H:%M:%S") < "14:00:00" ~ "12-2pm",
    format(timestamp, "%H:%M:%S") >= "14:00:00" & format(timestamp, "%H:%M:%S") < "16:45:00" ~ "2-4:30pm",
    TRUE ~ "Other"
  ))

plot_speeds_by_hour <- function(data, selected_season) {
  # Filter and aggregate the data based on the selected season
  filtered_data <- data %>%
    filter(season == selected_season, type == "Wireless") %>%
    group_by(hour) %>%
    summarize(
      median_download_speed = median(download_speed_reported_mbps, na.rm = TRUE),
      median_upload_speed = median(upload_speed_reported_mbps, na.rm = TRUE),
      sd_download_speed = sd(download_speed_reported_mbps, na.rm = TRUE),
      sd_upload_speed = sd(upload_speed_reported_mbps, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = c(median_download_speed, median_upload_speed), 
                 names_to = "SpeedType", 
                 values_to = "MedianSpeed") %>%
    pivot_longer(cols = c(sd_download_speed, sd_upload_speed),
                 names_to = "ErrorType",
                 values_to = "Error") %>%
    filter((SpeedType == "median_download_speed" & ErrorType == "sd_download_speed") |
             (SpeedType == "median_upload_speed" & ErrorType == "sd_upload_speed")) %>%
    mutate(hour = factor(hour, levels = c("8-10am", "10-12pm", "12-2pm", "2-4:30pm"))) %>%
    ungroup()
  
  # Plot the line graph with error bars
  ggplot(filtered_data, aes(x = hour, y = MedianSpeed, color = SpeedType, group = SpeedType)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = MedianSpeed - Error, ymax = MedianSpeed + Error), width = 0.2) +
    labs(
      title = paste("Median Wireless Download and Upload Speeds by Hour of Day (", selected_season, ") with Error Bars", sep = ""),
      x = "Hour of Day",
      y = "Median Speed (Mbps)",
      color = "Speed Type"
    ) +
    scale_color_manual(values = c("median_download_speed" = "blue", "median_upload_speed" = "green"),
                       labels = c("Download Speed", "Upload Speed")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 1)
    )
}








# PAGE (5)
# Create a dataframe with wireless connections having download or upload speeds lower than 20 Mbps in summer
summer_slow_speeds <- all_data %>%
  filter(season == "Summer" & type == "Wireless" & (download_speed_reported_mbps < 20 | upload_speed_reported_mbps < 20))

# Summarize the count of slow speeds by floor
slow_speeds_by_floor <- summer_slow_speeds %>%
  group_by(room_category) %>%
  summarize(
    count_slow_speeds = n(),
    total_tests = nrow(all_data %>% filter(season == "Summer" & type == "Wireless" & room_category == first(room_category))),
    percentage_slow_speeds = round((count_slow_speeds / total_tests) * 100, 2)
  )

# Create a dataframe with wireless connections having download or upload speeds lower than 20 Mbps in spring
spring_slow_speeds <- all_data %>%
  filter(season == "Spring" & type == "Wireless" & (download_speed_reported_mbps < 20 | upload_speed_reported_mbps < 20))

# Summarize the count of slow speeds by floor
slow_speeds_by_floor_spring <- spring_slow_speeds %>%
  group_by(room_category) %>%
  summarize(
    count_slow_speeds = n(),
    total_tests = nrow(all_data %>% filter(season == "Spring" & type == "Wireless" & room_category == first(room_category))),
    percentage_slow_speeds = round((count_slow_speeds / total_tests) * 100, 2)
  )

# PAGE (6)
# Function to plot download speeds as box plots for basement comparison
plot_download_basement_comparison <- function(data, stress_data) {
  # Filter data for the basement
  stress_basement <- stress_data %>%
    filter(room_category == "Basement") %>%
    filter(download_speed_reported_mbps <= 500) %>%
    mutate(Category = "Stress Test - Basement")
  
  summer_basement <- data %>%
    filter(room_category == "Basement", season == "Summer", type == "Wireless") %>%
    mutate(Category = "Summer Wireless - Basement")
  
  spring_basement <- data %>%
    filter(room_category == "Basement", season == "Spring", type == "Wireless") %>%
    mutate(Category = "Spring Wireless - Basement")
  
  combined_data <- bind_rows(stress_basement, summer_basement, spring_basement)
  
  ggplot(combined_data, aes(x = Category, y = download_speed_reported_mbps, fill = Category)) +
    geom_boxplot() +
    labs(
      title = "Download Speed Basement Comparison",
      x = "Category",
      y = "Download Speed (Mbps)"
    ) +
    scale_fill_manual(values = c("Stress Test - Basement" = "orange", "Summer Wireless - Basement" = "blue", "Spring Wireless - Basement" = "blue")) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to plot upload speeds as box plots for basement comparison
plot_upload_basement_comparison <- function(data, stress_data) {
  # Filter data for the basement
  stress_basement <- stress_data %>%
    filter(room_category == "Basement") %>%
    filter(upload_speed_reported_mbps <= 500) %>%
    mutate(Category = "Stress Test - Basement")
  
  summer_basement <- data %>%
    filter(room_category == "Basement", season == "Summer", type == "Wireless") %>%
    mutate(Category = "Summer Wireless - Basement")
  
  spring_basement <- data %>%
    filter(room_category == "Basement", season == "Spring", type == "Wireless") %>%
    mutate(Category = "Spring Wireless - Basement")
  
  combined_data <- bind_rows(stress_basement, summer_basement, spring_basement)
  
  ggplot(combined_data, aes(x = Category, y = upload_speed_reported_mbps, fill = Category)) +
    geom_boxplot() +
    labs(
      title = "Upload Speed Basement Comparison",
      x = "Category",
      y = "Upload Speed (Mbps)"
    ) +
    scale_fill_manual(values = c("Stress Test - Basement" = "orange", "Summer Wireless - Basement" = "green", "Spring Wireless - Basement" = "green")) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to plot download speeds as box plots for third floor comparison
plot_download_third_floor_comparison <- function(data, stress_data) {
  # Filter data for the third floor
  stress_third_floor <- stress_data %>%
    filter(room_category == "Third Floor") %>%
    filter(download_speed_reported_mbps <= 500) %>%
    mutate(Category = "Stress Test - Third Floor")
  
  summer_third_floor <- data %>%
    filter(room_category == "Third Floor", season == "Summer", type == "Wireless") %>%
    mutate(Category = "Summer Wireless - Third Floor")
  
  spring_third_floor <- data %>%
    filter(room_category == "Third Floor", season == "Spring", type == "Wireless") %>%
    mutate(Category = "Spring Wireless - Third Floor")
  
  combined_data <- bind_rows(stress_third_floor, summer_third_floor, spring_third_floor)
  
  ggplot(combined_data, aes(x = Category, y = download_speed_reported_mbps, fill = Category)) +
    geom_boxplot() +
    labs(
      title = "Download Speed Third Floor Comparison",
      x = "Category",
      y = "Download Speed (Mbps)"
    ) +
    scale_fill_manual(values = c("Stress Test - Third Floor" = "orange", "Summer Wireless - Third Floor" = "blue", "Spring Wireless - Third Floor" = "blue")) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to plot upload speeds as box plots for third floor comparison
plot_upload_third_floor_comparison <- function(data, stress_data) {
  # Filter data for the third floor
  stress_third_floor <- stress_data %>%
    filter(room_category == "Third Floor") %>%
    filter(upload_speed_reported_mbps <= 500) %>%
    mutate(Category = "Stress Test - Third Floor")
  
  summer_third_floor <- data %>%
    filter(room_category == "Third Floor", season == "Summer", type == "Wireless") %>%
    mutate(Category = "Summer Wireless - Third Floor")
  
  spring_third_floor <- data %>%
    filter(room_category == "Third Floor", season == "Spring", type == "Wireless") %>%
    mutate(Category = "Spring Wireless - Third Floor")
  
  combined_data <- bind_rows(stress_third_floor, summer_third_floor, spring_third_floor)
  
  ggplot(combined_data, aes(x = Category, y = upload_speed_reported_mbps, fill = Category)) +
    geom_boxplot() +
    labs(
      title = "Upload Speed Third Floor Comparison",
      x = "Category",
      y = "Upload Speed (Mbps)"
    ) +
    scale_fill_manual(values = c("Stress Test - Third Floor" = "orange", "Summer Wireless - Third Floor" = "green", "Spring Wireless - Third Floor" = "green")) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Generate plots
plot_download_basement <- plot_download_basement_comparison(all_data, stress_test_df)
plot_upload_basement <- plot_upload_basement_comparison(all_data, stress_test_df)
plot_download_third_floor <- plot_download_third_floor_comparison(all_data, stress_test_df)
plot_upload_third_floor <- plot_upload_third_floor_comparison(all_data, stress_test_df)

# Display plots
print(plot_download_basement)
print(plot_upload_basement)
print(plot_download_third_floor)
print(plot_upload_third_floor)

#PAGE (7)
# Filter summer wireless data and remove the first floor
summer_wireless <- all_data %>%
  filter(season == "Summer" & type == "Wireless" & room_category != "First Floor")

# Create two separate data frames based on the date
before_st <- summer_wireless %>%
  filter(date < as.Date("2024-07-24"))

after_st <- summer_wireless %>%
  filter(date >= as.Date("2024-07-24"))

# Calculate average speeds and SD by floor
avg_speed_before_st <- before_st %>%
  group_by(room_category) %>%
  summarise(
    avg_download_speed = mean(download_speed_reported_mbps, na.rm = TRUE),
    sd_download_speed = sd(download_speed_reported_mbps, na.rm = TRUE),
    avg_upload_speed = mean(upload_speed_reported_mbps, na.rm = TRUE),
    sd_upload_speed = sd(upload_speed_reported_mbps, na.rm = TRUE)
  ) %>%
  mutate(test_period = "Before Stress Test")

avg_speed_after_st <- after_st %>%
  group_by(room_category) %>%
  summarise(
    avg_download_speed = mean(download_speed_reported_mbps, na.rm = TRUE),
    sd_download_speed = sd(download_speed_reported_mbps, na.rm = TRUE),
    avg_upload_speed = mean(upload_speed_reported_mbps, na.rm = TRUE),
    sd_upload_speed = sd(upload_speed_reported_mbps, na.rm = TRUE)
  ) %>%
  mutate(test_period = "After Stress Test")

# Combine the two data frames
combined_avg_speed <- bind_rows(avg_speed_before_st, avg_speed_after_st)

# Ensure 'test_period' is a factor with 'Before Stress Test' first
combined_avg_speed <- combined_avg_speed %>%
  mutate(test_period = factor(test_period, levels = c("Before Stress Test", "After Stress Test"))) %>%
  arrange(room_category, test_period)

# Combine the two data frames
combined_avg_speed <- bind_rows(avg_speed_before_st, avg_speed_after_st) %>%
  # Ensure 'test_period' is a factor with 'Before Stress Test' first
  mutate(test_period = factor(test_period, levels = c("Before Stress Test", "After Stress Test"))) %>%
  arrange(room_category, test_period)

# Calculate percentage change for each floor
combined_avg_speed <- combined_avg_speed %>%
  group_by(room_category) %>%
  mutate(
    perc_change_download_speed = 100 * (avg_download_speed - lag(avg_download_speed)) / lag(avg_download_speed),
    perc_change_upload_speed = 100 * (avg_upload_speed - lag(avg_upload_speed)) / lag(avg_upload_speed),
    perc_change_sd_download_speed = 100 * (sd_download_speed - lag(sd_download_speed)) / lag(sd_download_speed),
    perc_change_sd_upload_speed = 100 * (sd_upload_speed - lag(sd_upload_speed)) / lag(sd_upload_speed)
  ) %>%
  ungroup()

# The first row for each room_category will have NA in the percentage change columns (since there's no previous row to compare to)
# You may want to replace those NAs with zeros or keep them as is depending on your analysis needs

# Plot the graph
ggplot(combined_avg_speed, aes(x = room_category, y = avg_download_speed, fill = test_period)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = avg_download_speed - sd_download_speed, ymax = avg_download_speed + sd_download_speed),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Average Download Speed and SD by Floor",
       x = "Floor",
       y = "Average Download Speed (Mbps)") +
  theme_minimal()

















# Define UI for the Shiny app using shinydashboard------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Internet Speed Test Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Time", tabName = "time", icon = icon("clock")),
      menuItem("Slow Speeds", tabName = "slow_speeds", icon = icon("slow-motion-video")),
      menuItem("Stress Test Comparison", tabName = "stress_test_comparison", icon = icon("bar-chart")),
      menuItem("AP Results", tabName = "ap_results", icon = icon("chart-line"))  # New menu item
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-select .selectize-input {
          min-height: 30px;
          font-size: 14px;
        }
        .info-box-custom {
          font-size: 16px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "summary",
              h3("Summary Statistics"),
              p("This page provides an overview of summary statistics for internet speed tests, filtered by season, test type, and room category. 
                The summary metrics include the mean, median, and standard deviation for both download and upload speeds. 
                The bar plot visually represents the median speeds across different room categories, providing a clear comparison."),
              fluidRow(
                box(
                  title = "Select Options",
                  width = 4,
                  div(class = "small-select",
                      selectInput("season", "Select Season:", choices = c("Summer", "Spring"), width = "100%"),
                      selectInput("test_type", "Select Test Type:", choices = c("Wireless", "Wired"), width = "100%"),
                      selectInput("room_category", "Select Room Category:", choices = c("All", unique(all_data$room_category)), width = "100%")
                  )
                ),
                box(
                  title = "Summary Metrics",
                  width = 8,
                  fluidRow(
                    infoBoxOutput("meanDownload", width = 4),
                    infoBoxOutput("medianDownload", width = 4),
                    infoBoxOutput("sdDownload", width = 4)
                  ),
                  fluidRow(
                    infoBoxOutput("meanUpload", width = 4),
                    infoBoxOutput("medianUpload", width = 4),
                    infoBoxOutput("sdUpload", width = 4)
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Median Speed Bar Plot",
                  width = 12,
                  plotOutput("medianSpeedBarPlot")
                )
              )
      ),
      tabItem(tabName = "plots",
              h3("Speed Plots"),
              p("This page displays box plots for download and upload speeds, along with bar plots with error bars, filtered by season, test type, and room category. 
          The plots allow for a visual comparison of the distribution and variability of speeds under different conditions."),
              fluidRow(
                box(
                  title = "Select Options",
                  width = 4,
                  div(class = "small-select",
                      selectInput("seasonPlot", "Select Season:", choices = c("Summer", "Spring"), width = "100%"),
                      selectInput("test_typePlot", "Select Test Type:", choices = c("Wireless", "Wired"), width = "100%"),
                      selectInput("room_categoryPlot", "Select Room Category:", choices = c("All", unique(all_data$room_category)), width = "100%")
                  ),
                  div(style = "font-size: 12px; margin-top: 10px;",
                      p("Note: A box plot is a standardized way of displaying the distribution of data based on a five-number summary: minimum, first quartile (Q1), median, third quartile (Q3), and maximum. 
               The box represents the interquartile range (IQR) where the middle 50% of the data lie, while the line inside the box shows the median. 
               The 'whiskers' extend to the smallest and largest values within 1.5 * IQR from the quartiles. 
               Points outside this range are considered outliers and are plotted individually.")
                  )
                ),
                box(
                  title = "Speed Box Plot",
                  width = 8,
                  plotOutput("speedPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Speed Bar Plot with Error Bars",
                  width = 12,
                  plotOutput("speedBarPlot")
                )
              )
      ),
      tabItem(tabName = "time",
              h3("Median Speeds by Time of Day"),
              p("This page provides insights into how internet speeds fluctuate throughout the day for the selected season. 
          The line plot depicts the median download and upload speeds at different times, helping to identify patterns and peak usage periods."),
              fluidRow(
                box(
                  title = "Select Options",
                  width = 4,
                  div(class = "small-select",
                      selectInput("seasonTime", "Select Season:", choices = c("Summer", "Spring"), width = "100%")
                  )
                ),
                box(
                  title = "Time of Day Speed Trends",
                  width = 12,
                  plotOutput("timePlot")
                )
              )
      ),
      tabItem(tabName = "slow_speeds",
              h3("Slow Speeds Analysis"),
              p("This page focuses on analyzing slow internet speeds, specifically for summer and spring seasons. 
                It includes summary statistics for slow speeds and data tables listing instances of slow speeds across different room categories. 
                This helps in identifying areas and conditions where performance is significantly lower."),
              fluidRow(
                box(
                  title = "Summary Statistics for Slow Speeds",
                  width = 12,
                  fluidRow(
                    infoBoxOutput("slowSpeedsPercentage", width = 4),
                    infoBoxOutput("summerSlowSpeedsPercentage", width = 4),
                    infoBoxOutput("springSlowSpeedsPercentage", width = 4)
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Summer Slow Speeds Data Table",
                  width = 12,
                  dataTableOutput("summerSlowSpeedsTable")
                )
              ),
              fluidRow(
                box(
                  title = "Spring Slow Speeds Data Table",
                  width = 12,
                  dataTableOutput("springSlowSpeedsTable")
                )
              )
      ),
      tabItem(tabName = "stress_test_comparison",
              h3("Stress Test Comparison"),
              p("This page compares download and upload speeds under stress test conditions across different floors. 
          The plots help to understand how internet speeds vary under load, providing insights into network performance."),
              fluidRow(
                box(
                  title = "Select Options",
                  width = 4,
                  div(class = "small-select",
                      selectInput("speed_type", "Select Speed Type:", choices = c("Download", "Upload"), width = "100%"),
                      selectInput("floor", "Select Floor:", choices = c("Basement", "Third Floor"), width = "100%")
                  ),
                  div(style = "font-size: 12px; margin-top: 10px;",
                      p("Note: The orange box plot represents the stress test data, while the blue box plots represent data from Summer and Spring.")
                  )
                ),
                box(
                  title = "Stress Test Comparison Plot",
                  width = 8,
                  plotOutput("stressTestPlot")
                )
              )
      ),
      tabItem(tabName = "ap_results",
              h3("AP Results"),
              p("This page provides a comparison of download and upload speeds before and after AP installation, including the percentage changes and standard deviations."),
              fluidRow(
                box(
                  title = "AP Results Bar Plot",
                  width = 12,
                  plotOutput("apResultsPlot")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h4("Basement Changes"),
                  fluidRow(
                    column(width = 3, strong("Download Speed Change (%)"), uiOutput("basementDownloadChange")),
                    column(width = 3, strong("SD Download Change (%)"), uiOutput("basementSDDownloadChange")),
                    column(width = 3, strong("Upload Speed Change (%)"), uiOutput("basementUploadChange")),
                    column(width = 3, strong("SD Upload Change (%)"), uiOutput("basementSDUploadChange"))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h4("Third Floor Changes"),
                  fluidRow(
                    column(width = 3, strong("Download Speed Change (%)"), uiOutput("thirdFloorDownloadChange")),
                    column(width = 3, strong("SD Download Change (%)"), uiOutput("thirdFloorSDDownloadChange")),
                    column(width = 3, strong("Upload Speed Change (%)"), uiOutput("thirdFloorUploadChange")),
                    column(width = 3, strong("SD Upload Change (%)"), uiOutput("thirdFloorSDUploadChange"))
                  )
                )
              )
      )
      
      
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  summary_stats <- reactive({
    data <- all_data %>%
      filter(season == input$season, type == input$test_type)
    
    if (input$room_category != "All") {
      data <- data %>% filter(room_category == input$room_category)
    }
    
    data %>%
      summarize(
        Mean_Download = mean(download_speed_reported_mbps, na.rm = TRUE),
        Median_Download = median(download_speed_reported_mbps, na.rm = TRUE),
        SD_Download = sd(download_speed_reported_mbps, na.rm = TRUE),
        Mean_Upload = mean(upload_speed_reported_mbps, na.rm = TRUE),
        Median_Upload = median(upload_speed_reported_mbps, na.rm = TRUE),
        SD_Upload = sd(upload_speed_reported_mbps, na.rm = TRUE)
      )
  })
  
  output$meanDownload <- renderInfoBox({
    infoBox("Mean Download", format(summary_stats()$Mean_Download, digits = 2), icon = icon("download"))
  })
  
  output$medianDownload <- renderInfoBox({
    infoBox("Median Download", format(summary_stats()$Median_Download, digits = 2), icon = icon("download"))
  })
  
  output$sdDownload <- renderInfoBox({
    infoBox("SD Download", format(summary_stats()$SD_Download, digits = 2), icon = icon("download"))
  })
  
  output$meanUpload <- renderInfoBox({
    infoBox("Mean Upload", format(summary_stats()$Mean_Upload, digits = 2), icon = icon("upload"))
  })
  
  output$medianUpload <- renderInfoBox({
    infoBox("Median Upload", format(summary_stats()$Median_Upload, digits = 2), icon = icon("upload"))
  })
  
  output$sdUpload <- renderInfoBox({
    infoBox("SD Upload", format(summary_stats()$SD_Upload, digits = 2), icon = icon("upload"))
  })
  
  output$medianSpeedBarPlot <- renderPlot({
    filtered_data <- all_data %>%
      filter(season == input$season, type == input$test_type)
    
    if (input$room_category != "All") {
      filtered_data <- filtered_data %>% filter(room_category == input$room_category)
    }
    
    median_speeds <- filtered_data %>%
      group_by(room_category) %>%
      summarize(
        Median_Download = median(download_speed_reported_mbps, na.rm = TRUE),
        Median_Upload = median(upload_speed_reported_mbps, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(Median_Download, Median_Upload), names_to = "Metric", values_to = "Value")
    
    ggplot(median_speeds, aes(x = room_category, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Median Speeds by Room Category", x = "Room Category", y = "Speed (Mbps)") +
      scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("Download", "Upload")) +
      theme_minimal()
  })
  
  output$speedPlot <- renderPlot({
    plot_data <- all_data %>%
      filter(season == input$seasonPlot, type == input$test_typePlot)
    
    if (input$room_categoryPlot != "All") {
      plot_data <- plot_data %>% filter(room_category == input$room_categoryPlot)
    }
    
    plot_speeds(plot_data, input$seasonPlot, input$test_typePlot)
  })
  
  output$speedBarPlot <- renderPlot({
    plot_data <- all_data %>%
      filter(season == input$seasonPlot, type == input$test_typePlot)
    
    if (input$room_categoryPlot != "All") {
      plot_data <- plot_data %>% filter(room_category == input$room_categoryPlot)
    }
    
    plot_speeds_bar(plot_data, input$seasonPlot, input$test_typePlot)
  })

  output$timePlot <- renderPlot({
    plot_speeds_by_hour(all_data, input$seasonTime)
  })
  
  output$slowSpeedsPercentage <- renderInfoBox({
    total_slow_speeds <- nrow(summer_slow_speeds) + nrow(spring_slow_speeds)
    total_tests <- nrow(all_data %>% filter(type == "Wireless"))
    slow_speed_percentage <- (total_slow_speeds / total_tests) * 100
    
    infoBox("Overall Slow Speeds", paste0(format(slow_speed_percentage, digits = 2), "%"), icon = icon("tachometer-alt"))
  })
  
  output$summerSlowSpeedsPercentage <- renderInfoBox({
    summer_total_tests <- nrow(all_data %>% filter(season == "Summer", type == "Wireless"))
    summer_slow_speed_percentage <- (nrow(summer_slow_speeds) / summer_total_tests) * 100
    
    infoBox("Summer Slow Speeds", paste0(format(summer_slow_speed_percentage, digits = 2), "%"), icon = icon("sun"))
  })
  
  output$springSlowSpeedsPercentage <- renderInfoBox({
    spring_total_tests <- nrow(all_data %>% filter(season == "Spring", type == "Wireless"))
    spring_slow_speed_percentage <- (nrow(spring_slow_speeds) / spring_total_tests) * 100
    
    infoBox("Spring Slow Speeds", paste0(format(spring_slow_speed_percentage, digits = 2), "%"), icon = icon("seedling"))
  })
  
  
  output$summerSlowSpeedsTable <- renderDataTable({
    slow_speeds_by_floor
  })
  
  output$springSlowSpeedsTable <- renderDataTable({
    slow_speeds_by_floor_spring
  })
  
  output$stressTestPlot <- renderPlot({
    if (input$floor == "Basement") {
      if (input$speed_type == "Download") {
        plot_download_basement
      } else {
        plot_upload_basement
      }
    } else {
      if (input$speed_type == "Download") {
        plot_download_third_floor
      } else {
        plot_upload_third_floor
      }
    }
  })
  
  output$apResultsPlot <- renderPlot({
    ggplot(combined_avg_speed, aes(x = room_category, y = avg_download_speed, fill = test_period)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = avg_download_speed - sd_download_speed, ymax = avg_download_speed + sd_download_speed),
                    width = 0.2, position = position_dodge(0.9)) +
      labs(title = "Average Download Speed and SD by Floor",
           x = "Floor",
           y = "Average Download Speed (Mbps)") +
      theme_minimal()
  })

  # Helper function to style percentage changes
  style_percentage_change <- function(change) {
    if (is.na(change)) {
      return(tags$div("N/A"))
    } else if (change > 0) {
      return(tags$div(style = "background-color: green; color: white; padding: 5px; border-radius: 5px;", 
                      paste0(round(change, 2), "%")))
    } else {
      return(tags$div(style = "background-color: red; color: white; padding: 5px; border-radius: 5px;", 
                      paste0(round(change, 2), "%")))
    }
  }
  
  # Helper function to style SD changes with both symbols and numerical values
  style_sd_change <- function(change) {
    if (is.na(change)) {
      return(tags$div("N/A"))
    } else if (change > 0) {
      return(tags$div(
        tags$span(style = "font-size: 18px; margin-right: 5px;", "✘"), 
        paste0(round(change, 2), "%")
      ))
    } else {
      return(tags$div(
        tags$span(style = "font-size: 18px; margin-right: 5px;", "✔"), 
        paste0(round(change, 2), "%")
      ))
    }
  }
  
  # Text outputs for Basement Changes
  output$basementDownloadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Basement" & test_period == "After Stress Test") %>%
      pull(perc_change_download_speed)
    style_percentage_change(change)
  })
  
  output$basementSDDownloadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Basement" & test_period == "After Stress Test") %>%
      pull(perc_change_sd_download_speed)
    style_sd_change(change)
  })
  
  output$basementUploadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Basement" & test_period == "After Stress Test") %>%
      pull(perc_change_upload_speed)
    style_percentage_change(change)
  })
  
  output$basementSDUploadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Basement" & test_period == "After Stress Test") %>%
      pull(perc_change_sd_upload_speed)
    style_sd_change(change)
  })
  
  # Text outputs for Third Floor Changes
  output$thirdFloorDownloadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Third Floor" & test_period == "After Stress Test") %>%
      pull(perc_change_download_speed)
    style_percentage_change(change)
  })
  
  output$thirdFloorSDDownloadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Third Floor" & test_period == "After Stress Test") %>%
      pull(perc_change_sd_download_speed)
    style_sd_change(change)
  })
  
  output$thirdFloorUploadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Third Floor" & test_period == "After Stress Test") %>%
      pull(perc_change_upload_speed)
    style_percentage_change(change)
  })
  
  output$thirdFloorSDUploadChange <- renderUI({
    change <- combined_avg_speed %>%
      filter(room_category == "Third Floor" & test_period == "After Stress Test") %>%
      pull(perc_change_sd_upload_speed)
    style_sd_change(change)
  })
}

shinyApp(ui, server)
