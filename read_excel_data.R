library(tidyverse)
library(here)
library(lubridate)
library(readxl)

file_data_2019 <- here::here('data', 'base_data_2019.xlsx') 
file_data_2022 <- here::here('data', 'base_data_2022.xlsx')

#-------------------------------------------------------------------------------
# Data 2019

## Read pressures in bar for 2019 every 15 minutes
data_2019_pressure <- read_excel(file_data_2019, sheet = "pu_pd") |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

## Read flows in liter per second for 2019 every 15 minutes
data_2019_flow <- read_excel(file_data_2019, sheet = "flow") |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

## Read valve position in % for 2019 every 15 minutes
data_2019_position <- read_excel(file_data_2019, sheet = "vp") |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

## Bind data frames
data_2019 = rbind(data_2019_pressure,data_2019_flow,data_2019_position)
rm(data_2019_pressure, data_2019_flow, data_2019_position)

# Round date_time to 15 minutes 
data_2019$date_time <-  lubridate::round_date(data_2019$date_time, "15 minutes") 

# Calculate the mean of the value every 15 minutes for 2019
data_2019 <- data_2019 |>
  dplyr::group_by(date_time, measurement)  |>
  dplyr::summarise(value = mean(value))

#-------------------------------------------------------------------------------
# Data 2022

## Read upstream pressures in bar for 2022 every 15 minutes

data_2022_pu <- read_excel(file_data_2022, sheet = "pu") 
data_2022_pu$date_time <- lubridate::ymd_hms(data_2022_pu$date_time) 
data_2022_pu <- data_2022_pu |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_pd <- read_excel(file_data_2022, sheet = "pd")
data_2022_pd$date_time <- lubridate::ymd_hms(data_2022_pd$date_time) 
data_2022_pd <- data_2022_pd |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_pc <- read_excel(file_data_2022, sheet = "pc") 
data_2022_pc$date_time <- lubridate::ymd_hms(data_2022_pc$date_time)
data_2022_pc <- data_2022_pc |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_vp <- read_excel(file_data_2022, sheet = "vp") 
data_2022_vp$date_time <- lubridate::ymd_hms(data_2022_vp$date_time)
data_2022_vp <- data_2022_vp |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_flow <- read_excel(file_data_2022, sheet = "flow") 
data_2022_flow$date_time <- lubridate::ymd_hms(data_2022_flow$date_time)
data_2022_flow <- data_2022_flow |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

# Bind data frames 2022
data_2022 <- rbind(data_2022_pu, data_2022_pd, data_2022_pc, data_2022_vp, data_2022_flow)
rm(data_2022_pu, data_2022_pd, data_2022_pc, data_2022_vp, data_2022_flow)

# Round date_time to 15 minutes 
data_2022$date_time <-lubridate::round_date(data_2022$date_time, "15 minutes") 

# Calculate the mean of the value every 15 minutes for 2022
data_2022 <- data_2022 |>
  dplyr::group_by(date_time, measurement)  |>
  dplyr::summarise(value = mean(value))

#-------------------------------------------------------------------------------
# combine data_2019 and data_2022
data_prv_barcelona <- rbind(data_2019, data_2022)
rm(data_2019, data_2022)

## Extract unique elements (identical)
data_prv_barcelona <- data_prv_barcelona |> 
  dplyr::distinct()

# Filter out non-zero values
data_prv_barcelona <- data_prv_barcelona |> 
  filter(value > 0)

## select character columns 'measurement', to factor:
data_prv_barcelona <- mutate_at(data_prv_barcelona, vars(measurement), as.factor)

# Pivot data 
data_prv_barcelona <- data_prv_barcelona |> 
  pivot_wider(names_from = measurement, values_from = value)

# Insert column Year and filter for 2019 and 2022
data_prv_barcelona <- data_prv_barcelona |> 
  mutate(year = year(date_time)) |> 
  mutate_at(vars(year), as.factor) |> 
  filter(year == 2019 | year == 2022)

#-------------------------------------------------------------------------------
# Create calender for 2019 and 2020

# Holidays and Observances in Ecuador 
#https://www.timeanddate.com/holidays/ecuador/

start_date  <- as.POSIXct("2019-01-01 00:00:00", tz = "UTC")
end_date    <- as.POSIXct("2019-12-31 23:59:59", tz = "UTC")
time_series <- seq(from = start_date, to = end_date, by = "15 min")
ts_2019     <- ts(data = rep(0, length(time_series)), start = c(2019, 1), frequency = 4 * 24 * 365)




#-------------------------------------------------------------------------------
# Exploratory Plots

##  Histogram plot for upstream pressure
median_pu <- data_prv_barcelona |> 
  select(year, pu) |> 
  group_by(year) |> 
  summarise(median_pu =median(pu, na.rm=TRUE))

data_prv_barcelona |> 
ggplot(aes(x=pu, color=year)) + 
  geom_histogram(binwidth=0.01, fill="white", position="dodge")+ 
  geom_vline(data=median_pu, aes(xintercept=median_pu, color=year),
             linetype="dashed")+
  theme(legend.position="top")

##  Histogram plot for upstream flows
median_flow <- data_prv_barcelona |> 
  select(year, flow) |> 
  group_by(year) |> 
  summarise(median_flow =median(flow, na.rm=TRUE))

ggplot(data_prv_barcelona, aes(x=flow, color=year)) + 
  geom_histogram(binwidth= 10, fill="white", position="dodge") +
  geom_vline(data=median_flow, aes(xintercept=median_flow, color=year),
             linetype="dashed")+
  theme(legend.position="top")
  
       