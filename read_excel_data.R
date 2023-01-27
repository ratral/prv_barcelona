library(tidyverse)
library(here)
library(lubridate)
library(readxl)

file_data_2019 <- here::here('data', 'base_data_2019.xlsx') 
file_data_2022 <- here::here('data', 'base_data_2022.xlsx')


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

data_2019 = rbind(data_2019_pressure,data_2019_flow,data_2019_position)

rm(data_2019_pressure, data_2019_flow, data_2019_position)


# Data 2022

## Read upstream pressures in bar for 2022 every 15 minutes

data_2022_pu <- read_excel(file_data_2022, sheet = "pu") 
  
data_2022_pu$date_time <- lubridate::ymd_hms(data_2022_pu$date_time) |> 
  lubridate::round_date("15 minutes") 

data_2022_pu <- data_2022_pu |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_pd <- read_excel(file_data_2022, sheet = "pd")

data_2022_pd$date_time <- lubridate::ymd_hms(data_2022_pd$date_time) |> 
  lubridate::round_date("15 minutes") 

data_2022_pd <- data_2022_pd |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_pc <- read_excel(file_data_2022, sheet = "pc") 

data_2022_pc$date_time <- lubridate::ymd_hms(data_2022_pc$date_time) |> 
  lubridate::round_date("15 minutes") 

data_2022_pc <- data_2022_pc |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022_vp <- read_excel(file_data_2022, sheet = "vp") 

data_2022_vp$date_time <- lubridate::ymd_hms(data_2022_vp$date_time) |> 
  lubridate::round_date("15 minutes") 

data_2022_vp <- data_2022_vp |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")


data_2022_flow <- read_excel(file_data_2022, sheet = "flow") 

data_2022_flow$date_time <- lubridate::ymd_hms(data_2022_flow$date_time) |> 
  lubridate::round_date("15 minutes")

data_2022_flow <- data_2022_flow |> 
  pivot_longer(!date_time, names_to = "measurement", values_to = "value")

data_2022 <- rbind(data_2022_pu, data_2022_pd, data_2022_pc, 
                  data_2022_vp, data_2022_flow)

rm(data_2022_pu, data_2022_pd, data_2022_pc, data_2022_vp, data_2022_flow)


## combine data_2019 and data_2022

data_prv_barcelona <- rbind(data_2019, data_2022)
