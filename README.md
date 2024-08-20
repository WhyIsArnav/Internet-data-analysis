# INTERNET SPEED ANALYSIS (Shiny App)

### Author: Arnav Shrestha

## Overview
This Shiny dashboard project is part of a summer-long data visualization initiative aimed at analyzing internet speed data across various room categories and seasons (Spring and Summer). The dashboard provides a comprehensive view of median wireless download and upload speeds by hour, along with analyses of slow speeds for both seasons. It also offers summary statistics and visualizations to help identify and understand trends in internet speed performance.

## Directory 📖
The columns that were used are: 
- Room Category
- Date
- Time
- Month
- Day
- Year
- Hour
- Download Speed
- Upload Speed
- Season
- Slow Speed Indicator

## Data Cleaning 🧹

**1. Creating a main data frame**

```r
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
```
**2. Filtering values into categorized data sources**

```r
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

summer_slow_speeds <- all_data %>%
  filter(season == "Summer" & type == "Wireless" & (download_speed_reported_mbps < 20 | upload_speed_reported_mbps < 20))

spring_slow_speeds <- all_data %>%
  filter(season == "Spring" & type == "Wireless" & (download_speed_reported_mbps < 20 | upload_speed_reported_mbps < 20))
```
**3. Changing time format**

```r
all_data <- all_data %>%
  mutate(hour = case_when(
    format(timestamp, "%H:%M:%S") >= "08:00:00" & format(timestamp, "%H:%M:%S") < "10:00:00" ~ "8-10am",
    format(timestamp, "%H:%M:%S") >= "10:00:00" & format(timestamp, "%H:%M:%S") < "12:00:00" ~ "10-12pm",
    format(timestamp, "%H:%M:%S") >= "12:00:00" & format(timestamp, "%H:%M:%S") < "14:00:00" ~ "12-2pm",
    format(timestamp, "%H:%M:%S") >= "14:00:00" & format(timestamp, "%H:%M:%S") < "16:45:00" ~ "2-4:30pm",
    TRUE ~ "Other"
  ))
```
**4. Arranging slow speeds by floor**

```r
slow_speeds_by_floor <- summer_slow_speeds %>%
  group_by(room_category) %>%
  summarize(
    count_slow_speeds = n(),
    total_tests = nrow(all_data %>% filter(season == "Summer" & type == "Wireless" & room_category == first(room_category))),
    percentage_slow_speeds = round((count_slow_speeds / total_tests) * 100, 2)
  )

slow_speeds_by_floor_spring <- spring_slow_speeds %>%
  group_by(room_category) %>%
  summarize(
    count_slow_speeds = n(),
    total_tests = nrow(all_data %>% filter(season == "Spring" & type == "Wireless" & room_category == first(room_category))),
    percentage_slow_speeds = round((count_slow_speeds / total_tests) * 100, 2)
  )
```

**5. Creating Mutated data frames**
```r
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
  ```



