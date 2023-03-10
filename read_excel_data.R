library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(bizdays)

file_data_2019 <- here::here('data', 'base_data_2019.xlsx') 
file_data_2022 <- here::here('data', 'base_data_2022.xlsx')

params <- list(
  diameter =900,
  temperature = 20,
  elevation = 0
)

# Functions for Normalization and round the Valve position and round up to the nearest 1%

normalize2 <- function(x, na.rm = T) (x  / max(x, na.rm = T))
roundUp <- function(x,to=10) to*(x%/%to + as.logical(x%%to))

#-------------------------------------------------------------------------------
# Data 2019

## Read pressures in bar for 2019 every 15 minutes
data_2019_pressure <- read_excel(file_data_2019, sheet = "pu_pd") %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

## Read flows in liter per second for 2019 every 15 minutes
data_2019_flow <- read_excel(file_data_2019, sheet = "flow") %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

## Read valve position in % for 2019 every 15 minutes
data_2019_position <- read_excel(file_data_2019, sheet = "vp") %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

### Set negative values to NA
data_2019_position$value[data_2019_position$value<0] <- NA

## Bind data frames
data_2019 <- rbind(data_2019_pressure,data_2019_flow,data_2019_position)
rm(data_2019_pressure, data_2019_flow)


# Round date_time to 15 minutes 
data_2019$date_time <-  lubridate::round_date(data_2019$date_time, "15 minutes") 

# Calculate the mean of the value every 15 minutes for 2019
data_2019 <- data_2019 %>%
  dplyr::group_by(date_time, measurement)  %>%
  dplyr::summarise(value = mean(value)) %>% 
  ungroup() %>% 
  as_tibble()

#-------------------------------------------------------------------------------
# Data 2022

## Read upstream pressures in bar for 2022 every 15 minutes

data_2022_pu <- read_excel(file_data_2022, sheet = "pu") 
data_2022_pu$date_time <- lubridate::ymd_hms(data_2022_pu$date_time) 
data_2022_pu <- data_2022_pu %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_pd <- read_excel(file_data_2022, sheet = "pd")
data_2022_pd$date_time <- lubridate::ymd_hms(data_2022_pd$date_time) 
data_2022_pd <- data_2022_pd %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_pc <- read_excel(file_data_2022, sheet = "pc") 
data_2022_pc$date_time <- lubridate::ymd_hms(data_2022_pc$date_time)
data_2022_pc <- data_2022_pc %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_vp <- read_excel(file_data_2022, sheet = "vp") 
data_2022_vp$date_time <- lubridate::ymd_hms(data_2022_vp$date_time)
data_2022_vp <- data_2022_vp %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_flow <- read_excel(file_data_2022, sheet = "flow") 
data_2022_flow$date_time <- lubridate::ymd_hms(data_2022_flow$date_time)
data_2022_flow <- data_2022_flow %>% 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

# Bind data frames 2022
data_2022 <- rbind(data_2022_pu, data_2022_pd, data_2022_pc, data_2022_vp, data_2022_flow)
rm(data_2022_pu, data_2022_pd, data_2022_pc,data_2022_flow)

# Round date_time to 15 minutes 
data_2022$date_time <-lubridate::round_date(data_2022$date_time, "15 minutes") 

# Calculate the mean of the value every 15 minutes for 2022
data_2022 <- data_2022 %>%
  dplyr::group_by(date_time, measurement)  %>%
  dplyr::summarise(value = mean(value))%>% 
  ungroup() %>% 
  as_tibble()

#-------------------------------------------------------------------------------
# Combine data_2019 and data_2022
data_prv <- rbind(data_2019, data_2022)
rm(data_2019, data_2022)

## Extract unique elements (identical)
data_prv <- data_prv %>% dplyr::distinct()

# Filter out non-zero values
data_prv <- data_prv %>% filter(value > 0)

## select character columns 'measurement', to factor:
data_prv <- mutate_at(data_prv, vars(measurement), as.factor)

# Pivot data 
data_prv <- data_prv %>% 
  pivot_wider(names_from = measurement, values_from = value)

#-------------------------------------------------------------------------------
# Create calender for 2019 and 2020

time_series_2019 <- seq(from = as.POSIXct("2019-01-01 00:00:00", tz = "UTC"), 
                        to   = as.POSIXct("2019-12-31 23:59:59", tz = "UTC"), 
                        by = "15 min")

time_series_2022 <- seq(from = as.POSIXct("2022-01-01 00:00:00", tz = "UTC"), 
                        to   = as.POSIXct("2022-12-12 11:15:00", tz = "UTC"), 
                        by = "15 min")

measurements_time <- as_tibble(c(time_series_2019, time_series_2022))
measurements_time <- rename(measurements_time, date_time = value)

# Holidays Calendar Ecuador 2019 and 2022

holidays <- read_csv(here::here('data','holidays.csv'),
                     show_col_types = FALSE) %>% 
  filter(type == 'National holiday') %>%
  mutate(date = ydm(date))

holidays <- as.Date(holidays$date)

calender <- create.calendar("ANBIMA",
                            holidays = holidays,
                            weekdays = c("saturday", "sunday")
)

# Create columns for Year, Month, Weekday, Week year, and hour

measurements_time <- measurements_time %>%
  mutate(yr = year(date_time),
         mt = month(date_time, label = TRUE, abbr = TRUE),
         dy = wday(date_time, label = TRUE, abbr = TRUE),
         bus_day = is.bizday(date(date_time), calender),
         wk = week(date_time),
         hr = hour(date_time))

measurements_time <- measurements_time %>%
   mutate_at(vars(yr), as.factor)


# Join measurements_time with data_prv

data_prv <- dplyr::left_join(measurements_time, 
                             data_prv,
                             by = 'date_time')

rm(time_series_2019, time_series_2022, measurements_time)


#-------------------------------------------------------------------------------
# Normalization of the Valve position and round up to the nearest 1%

data_prv <- data_prv %>% 
  mutate_at('vp', normalize2) %>% #scale to the max value
  mutate(vp = vp*100)  %>% 
  mutate(vp = roundUp(vp, to=1))


#-------------------------------------------------------------------------------
# Calculation of the pressure difference between Pu and Pc
data_prv <- data_prv %>%
  mutate(dpc = pd-pc)

#-------------------------------------------------------------------------------
# Calculation of the Kv


#-------------------------------------------------------------------------------
# Save data_prv in RDS format

data_prv %>%
  saveRDS(file =  here::here("data", "data_prv.rds"))


#===============================================================================
# VALVE POSITION DATA
#===============================================================================
# Combine Data of Valve position !!!!!!
data_valve_position <- rbind(data_2019_position, data_2022_vp) %>% 
  select(date_time, value)

rm(data_2019_position, data_2022_vp)

## Extract unique elements (identical) from Valve position

# data_valve_position <- data_valve_position %>% dplyr::distinct()
data_valve_position$date_time <- round_date(data_valve_position$date_time, "second")

data_valve_position <- data_valve_position %>%
  dplyr::group_by(date_time)  %>%
  dplyr::summarise(value = mean(value))%>% 
  ungroup() %>% 
  as_tibble()

data_valve_position <- data_valve_position %>% 
  mutate_at('value', normalize2) %>% #scale to the max value
  mutate(value = value*100)  %>% 
  mutate(value = roundUp(value, to=1))

data_valve_position <- data_valve_position %>%
  mutate(delta_time = as.numeric(date_time-lag(date_time)),
         delta_pv = value-lag(value))

data_valve_position$delta_time[data_valve_position$delta_time >= 86400] <- NA

data_valve_position <- data_valve_position %>%
  drop_na() %>% 
  filter(delta_time<=3600 & delta_pv<=20 & delta_pv>= -20) 
#===============================================================================
#=============================================================================== 

data_valve_position %>% 
  saveRDS(file =  here::here("data", "data_valve_position.rds"))

#-------------------------------------------------------------------------------

