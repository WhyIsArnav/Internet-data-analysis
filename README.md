# INTERNET SPEED ANALYSIS DASHBOARD

### Author: Arnav Shrestha

<div>
<img src="Images/InternetSpeedAnalysis.png" width="450">
</div>

## Overview:
This Shiny dashboard project analyzes internet speed data across various room categories and different seasons (Spring and Summer). The dashboard provides a comprehensive view of median wireless download and upload speeds by hour, along with slow speed analyses for both seasons. It also offers summary statistics and data visualizations to help understand trends in internet speed performance.

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

**1. Combine all CSV files into a single dataframe**

```r
df1 <- read.csv("internet_speed_apr.csv")
df2 <- read.csv("internet_speed_may.csv")
df3 <- read.csv("internet_speed_jun.csv")
df4 <- read.csv("internet_speed_jul.csv")
df5 <- read.csv("internet_speed_aug.csv")
df6 <- read.csv("internet_speed_sep.csv")

internet_speed <- rbind(df1, df2, df3, df4, df5, df6)
