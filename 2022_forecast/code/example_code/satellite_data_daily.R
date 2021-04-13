# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov) with help from Jordan Watson (jordan.watson@noaa.gov)
# April 8, 2021

# load libraries----
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(ncdf4)
library(tidync)
library(httr)
library(tidyverse)
library(PBSmapping)
library(extrafont)
library(ggrepel)
library(lubridate)
#extrafont::font_import() # only needs to be run once for extra fonts for figures
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# set up directories----
year.forecast <- "2022_forecast" 
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,  'results', '/')
#------------------------------------------------------------------------------------------------------------------------------------------
# DOWNLOAD SST DATA FROM THE NOAA SITE AND CREATE CSV FILE FOR YEARS 1997 to 2020 (May - July data)
#------------------------------------------------------------------------------------------------------------------------------------------
# 1997-1999 data----
# download the data for a fixed spatial and temporal period	(SST data, NOAA Global Coral Bleaching Monitoring, 5km, V.3.1, daily, 1985-Present)
# to do this, go to the site: https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html, set the latitude and longitude and time period, and file type as .nc,
# and then paste the URL in the section below
# The site will only download 2 years at a time for this big an area (or maybe due to a slow internet connection)

# https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.graph; this site is helpful to map the area of interest

# depending on the dates and the latitude and longitude chosen, the site will timeout after 60 seconds. Therefore, multiple files were downloaded and then merged later on in the code
download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(1997-05-01T12:00:00Z):1:(1998-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_97_98.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(1999-05-01T12:00:00Z):1:(2000-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_99_00.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2001-05-01T12:00:00Z):1:(2002-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_01_02.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2003-05-01T12:00:00Z):1:(2004-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_03_04.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2005-05-01T12:00:00Z):1:(2006-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_05_06.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2007-05-01T12:00:00Z):1:(2008-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_07_08.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2009-05-01T12:00:00Z):1:(2010-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_09_10.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2011-05-01T12:00:00Z):1:(2012-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_11_12.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2013-05-01T12:00:00Z):1:(2014-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_13_14.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2015-05-01T12:00:00Z):1:(2016-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_15_16.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2017-05-01T12:00:00Z):1:(2018-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_17_18.nc')))

download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2019-05-01T12:00:00Z):1:(2020-07-31T12:00:00Z)][(60):1:(56.025)][(-137.2):1:(-131.5)]"),
              method = "libcurl", mode="wb",destfile = (paste0(data.directory,'NOAA_DHW_daily_19_20.nc')))

# extract netCDF data, convert date (seconds since 1970) to date time, and spatially average daily data to a single daily point for each set of years
tidync(paste0(data.directory,'NOAA_DHW_daily_97_98.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_97_98

tidync(paste0(data.directory,'NOAA_DHW_daily_99_00.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_99_00

tidync(paste0(data.directory,'NOAA_DHW_daily_01_02.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_01_02

tidync(paste0(data.directory,'NOAA_DHW_daily_03_04.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_03_04

tidync(paste0(data.directory,'NOAA_DHW_daily_05_06.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_05_06

tidync(paste0(data.directory,'NOAA_DHW_daily_07_08.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_07_08

tidync(paste0(data.directory,'NOAA_DHW_daily_09_10.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_09_10

tidync(paste0(data.directory,'NOAA_DHW_daily_11_12.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_11_12

tidync(paste0(data.directory,'NOAA_DHW_daily_13_14.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_13_14

tidync(paste0(data.directory,'NOAA_DHW_daily_15_16.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_15_16

tidync(paste0(data.directory,'NOAA_DHW_daily_17_18.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_17_18

tidync(paste0(data.directory,'NOAA_DHW_daily_19_20.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(CRW_SST),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 5 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_19_20

# create plot of data for a certain date 
#sst_regions_oisst_97_99 %>% 
#  ggplot(aes(longitude,latitude,color=SST)) + 
#  geom_point()

# combine all the data sets into one csv file
rbind(sst_regions_oisst_97_98, sst_regions_oisst_99_00) %>%
  rbind(., sst_regions_oisst_01_02) %>%
  rbind(., sst_regions_oisst_03_04) %>%
  rbind(., sst_regions_oisst_05_06) %>%
  rbind(., sst_regions_oisst_07_08) %>%
  rbind(., sst_regions_oisst_09_10) %>%
  rbind(., sst_regions_oisst_11_12) %>%
  rbind(., sst_regions_oisst_13_14) %>%
  rbind(., sst_regions_oisst_15_16) %>%
  rbind(., sst_regions_oisst_17_18) %>%
  rbind(., sst_regions_oisst_19_20) %>%
  dplyr::select(latitude, longitude, SST, year, month, day) %>%
  mutate(longitude = round(longitude, 3), # merge does not work correctly unless the latitude and longitude are exactly the same so round the lat/longs to 3 digits
         latitude = round(latitude, 3))  %>%
  group_by(latitude, longitude, year, month) %>% # average across the month for each lat/long combination
  summarise(SST = mean(SST), .groups = 'drop') %>%
  as.data.frame() -> SST_satellite

SST_satellite %>%
  write.csv(., paste0(data.directory, 'sst_oisst_97_20_daily_data.csv'))

# create plot of entire data set  
SST_satellite %>% 
  ggplot(aes(longitude,latitude,color = SST)) + 
  geom_point()
#-------------------------------------------------------------------------------------------------------------
# MERGE MAP DATA COORDINATES (FOUR REGIONS) WITH THE SST DATA
#-------------------------------------------------------------------------------------------------------------
read.csv(paste0(data.directory, 'sst_oisst_97_20_daily_data.csv')) -> SST_satellite

# filter regions (lat/longs) from the map_data file
read.csv(paste0(data.directory, 'sst_data_map.csv')) %>% 
  rename(latitude = Y,
         longitude= X) %>% 
  dplyr::select(latitude, longitude, region, variable) %>%
  mutate(longitude = round(longitude, 3),# merge does not work correctly unless the latitude and longitude are exactly the same so round the lat/longs to 3 digits
       latitude = round(latitude, 3)) -> map_data

map_data %>%
  filter(region == 'Icy_Strait') -> map_data_Icy_Strait

map_data %>%
  filter(region == 'NSEAK') -> map_data_NSEAK

map_data %>%
  filter(region == 'Chatham_Strait') -> map_data_Chatham_Strait

map_data %>%
  filter(region == 'SST_Jordan') -> map_data_SST_Jordan

# merge coordinates from the map_data with the SST data to create SST data by region of interest
merge(map_data_Icy_Strait, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'Icy_Strait')-> Icy_Strait_SST

merge(map_data_NSEAK, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'NSEAK') -> NSEAK_SST

merge(map_data_Chatham_Strait, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'Chatham_Strait') -> Chatham_Strait_SST

merge(map_data_SST_Jordan, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'SST_Jordan') -> SST_Jordan

# combine region data sets into one csv file (data by lat, long, region, year, month, SST)
rbind(Icy_Strait_SST, NSEAK_SST) %>%
rbind(., Chatham_Strait_SST) %>% 
rbind(., SST_Jordan) %>% 
  write.csv(., paste0(data.directory, 'sst_regions_oisst_97_20_daily_data.csv'))
