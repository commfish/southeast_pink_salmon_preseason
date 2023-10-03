# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov)
# October 2023
# Data was originally downloaded from https://climatedataguide.ucar.edu/sites/default/files/2023-04/npindex_monthly.txt

# load libraries----
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(httr)
library(tidyverse)
library(PBSmapping)
library(extrafont)
library(ggrepel)
library(lubridate)
library(ggpubr)
# extrafont::font_import() # only needs to be run once for extra fonts for figures
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# create a folder for temperature_data
out.path <- paste0("2024_forecast/results/temperature_data/") # update year
if(!exists(out.path)){dir.create(out.path)}

# set up directories----
year.forecast <- "2024_forecast" # update year
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,  'results/temperature_data', '/')

# download data directly from site 
read_delim('https://climatedataguide.ucar.edu/sites/default/files/2023-04/npindex_monthly.txt', delim = " ", col_names=F)-> raw_data 
raw_data %>%
  slice(-1) %>%
  select(-X2, -X4, -X5, -X6, -X7, -X8) %>%
  rename(date = X1,
         NPI = X3) %>%
  mutate(date= ym(date)) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  dplyr::select(year, month, NPI) %>%
  filter(month >= 6 & month <= 8) %>%
  group_by(year) %>% 
  mutate(NPI = as.numeric(NPI)) %>%
  summarise(NPI = mean(NPI), .groups = 'drop') %>%
  as.data.frame() %>%
  write.csv(., paste0(data.directory, 'NPI.csv'))
