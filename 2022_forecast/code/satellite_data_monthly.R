# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov) with help from Jordan Watson (jordan.watson@noaa.gov)
# April 12, 2021

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
results.directory <- file.path(year.forecast,  'results/temperature_data', '/')
#------------------------------------------------------------------------------------------------------------------------------------------
# DOWNLOAD SST DATA FROM THE NOAA SITE AND CREATE CSV FILE FOR YEARS 1997 to 2020 (May - July data)
#------------------------------------------------------------------------------------------------------------------------------------------
# 1997-1999 data----
# download the data for a fixed spatial and temporal period	(SST data, NOAA Global Coral Bleaching Monitoring, 5km, V.3.1, daily, 1985-Present)
# to do this, go to the site: https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_monthly.html, set the latitude and longitude and time period, and filt type as .nc,
# and then paste the URL in the section below

# https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_monthly.graph; this site is helpful to map the area of interest

# depending on the dates and the latitude and longitude chosen, the site will timeout after 60 seconds. Therefore, multiple files were downloaded and then merged later on in the code
download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_monthly.nc?sea_surface_temperature%5B(1997-04-01T00:00:00Z):1:(2020-07-31T00:00:00Z)%5D%5B(60):1:(54)%5D%5B(-137.2):1:(-130)%5D,mask%5B(1997-04-01T00:00:00Z):1:(2020-07-31T00:00:00Z)%5D%5B(60):1:(54)%5D%5B(-137.2):1:(-130)%5D,sea_surface_temperature_anomaly%5B(1997-04-01T00:00:00Z):1:(2020-07-31T00:00:00Z)%5D%5B(60):1:(54)%5D%5B(-137.2):1:(-130)%5D"),
              method = "libcurl", mode="wb", destfile = (paste0(data.directory,'NOAA_DHW_monthly_97_20.nc')))

# if the site times out, you can also go to the site, and manually download the data and save the file to the data folder
# extract netCDF data, convert date (seconds since 1970) to date time, and spatially average daily data to a single daily point for each set of years
tidync(paste0(data.directory,'NOAA_DHW_monthly_97_20.nc')) %>% 
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(sea_surface_temperature),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 4 & month <= 7) %>% 
  filter(!is.na(SST)) -> sst_regions_oisst_97_20

# combine all the data sets into one csv file
sst_regions_oisst_97_20 %>%
  dplyr::select(latitude, longitude, SST, year, month, day) %>%
  mutate(longitude = round(longitude, 3), # merge does not work correctly unless the latitude and longitude are exactly the same so round the lat/longs to 3 digits
         latitude = round(latitude, 3))  %>%
  group_by(latitude, longitude, year, month) %>% # average across the month for each lat/long combination
  summarise(SST = mean(SST), .groups = 'drop') %>%
  as.data.frame() -> SST_satellite

SST_satellite %>%
  write.csv(., paste0(data.directory, 'sst_oisst_97_20_monthly_data.csv'))

# create plot of entire data set  
SST_satellite %>% 
  ggplot(aes(longitude,latitude,color = SST)) + 
  geom_point()
#--------------------------------------------------------------------------------------------------------------------------------------------
# MERGE MAP DATA COORDINATES (FOUR REGIONS) WITH THE SST DATA
#--------------------------------------------------------------------------------------------------------------------------------------------
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
  filter(region == 'Icy_Strait') -> Icy_Strait_SST

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
  write.csv(., paste0(data.directory, 'sst_regions_oisst_97_20_monthly_data.csv'))
#--------------------------------------------------------------------------------------------------------------------------------------------
# SUMMARIZE SST DATA BY REGION, YEAR, MONTH FOR Icy_Strait, Chatham_Strait, AND NSEAK
#--------------------------------------------------------------------------------------------------------------------------------------------
# summarize SST data
read.csv(paste0(data.directory, 'sst_regions_oisst_97_20_monthly_data.csv')) %>%
  filter(region %in% c('Icy_Strait', 'Chatham_Strait', 'NSEAK')) %>% 
  group_by(region, year, month) %>%
  summarise(msst = mean(SST), .groups = 'drop') %>%
  as.data.frame() -> SST_satellite_grouped

# average SST for May, June, July by region
SST_satellite_grouped %>%
  filter(month %in% c(5,6,7)) %>% 
  group_by(region, year) %>%
  summarise(SST_MJJ = mean(msst), .groups = 'drop') -> SST_MJJ

# average SST for May by region
SST_satellite_grouped %>%
  filter(month %in% c(5)) %>% 
  group_by(region, year) %>%
  summarise(SST_May = mean(msst), .groups = 'drop') -> SST_May

# average SST for April-July by region
SST_satellite_grouped %>%
  filter(month %in% c(4,5,6,7)) %>% 
  group_by(region, year) %>%
  summarise(SST_AMJJ = mean(msst), .groups = 'drop') -> SST_AMJJ

# combine data sets into one csv file
merge(SST_MJJ, SST_May, by = c("region", "year")) %>%
  merge(., SST_AMJJ, by = c("region", "year")) %>%
  write.csv(., paste0(results.directory, 'sst_regions_oisst_97_20_monthly_data_summary.csv'))

# create tables by region for the report
read.csv(paste0(results.directory, 'sst_regions_oisst_97_20_monthly_data_summary.csv')) %>%
  dplyr::select(region, year, SST_MJJ, SST_May, SST_AMJJ) -> tempdata

tempdata %>%
  filter(region == "Icy_Strait") %>%
  mutate(Icy_Strait_SST_MJJ = round(SST_MJJ, 2),
         Icy_Strait_SST_May = round(SST_May, 2),
         Icy_Strait_SST_AMJJ = round(SST_AMJJ, 2)) %>%
  dplyr::select(year, Icy_Strait_SST_MJJ, Icy_Strait_SST_May, Icy_Strait_SST_AMJJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_20_Icy_Strait_monthly_summary.csv'), row.names = FALSE)

tempdata %>%
  filter(region == "Chatham_Strait") %>%
  mutate(Chatham_Strait_SST_MJJ = round(SST_MJJ, 2),
         Chatham_Strait_SST_May = round(SST_May, 2),
         Chatham_Strait_SST_AMJJ = round(SST_AMJJ, 2)) %>%
  dplyr::select(year, Chatham_Strait_SST_MJJ, Chatham_Strait_SST_May, Chatham_Strait_SST_AMJJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_20_Chatham_monthly_summary.csv'), row.names = FALSE)

tempdata %>%
  filter(region == "NSEAK") %>%
  mutate(NSEAK_SST_MJJ = round(SST_MJJ, 2),
         NSEAK_SST_May = round(SST_May, 2),
         NSEAK_SST_AMJJ = round(SST_AMJJ, 2)) %>%
  dplyr::select(year, NSEAK_SST_MJJ, NSEAK_SST_May, NSEAK_SST_AMJJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_20_NSEAK_monthly_summary.csv'), row.names = FALSE) # row.name = F gets rid of the first column X1 that is not needed
#------------------------------------------------------------------------------------------------------------------------------------------
# SUMMARIZE SST DATA BY REGION, YEAR, MONTH FOR SST_JORDAN REGION
#------------------------------------------------------------------------------------------------------------------------------------------
# summarize SST data
read.csv(paste0(data.directory, 'sst_regions_oisst_97_20_monthly_data.csv')) %>%
  filter(region %in% c('SST_Jordan')) %>% 
  group_by(year, month, variable) %>%
  summarise(mean_msst = mean(SST), .groups = 'drop') %>%
  as.data.frame() %>%
  group_by(year, month) %>%
  summarise(msst = mean(mean_msst), .groups = 'drop') -> SST_satellite_grouped

# average SST for May, June, July by region
SST_satellite_grouped %>%
  filter(month %in% c(5,6,7)) %>% 
  group_by(year) %>%
  summarise(SST_MJJ = mean(msst), .groups = 'drop') -> SST_MJJ

# average SST for May by region
SST_satellite_grouped %>%
  filter(month %in% c(5)) %>% 
  group_by(year) %>%
  summarise(SST_May = mean(msst), .groups = 'drop') -> SST_May

# average SST for April-July by region
SST_satellite_grouped %>%
  filter(month %in% c(4,5,6,7)) %>% 
  group_by(year) %>%
  summarise(SST_AMJJ = mean(msst), .groups = 'drop') -> SST_AMJJ

# combine data sets into one csv file
merge(SST_MJJ, SST_May, by = c("year")) %>%
  merge(., SST_AMJJ, by = c("year")) %>%
  mutate(region = 'SST_Jordan',
        SST_Jordan_MJJ = round(SST_MJJ, 2),
        SST_Jordan_May = round(SST_May, 2),
        SST_Jordan_AMJJ = round(SST_AMJJ, 2)) %>%
  dplyr::select(year, SST_Jordan_MJJ, SST_Jordan_May, SST_Jordan_AMJJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_20_SST_Jordan_monthly_summary.csv'), row.names = FALSE)
#---------------------------------------------------------------------------------------------------------------------------------------------
# COMBINE DATA SETS FOR THE FOUR REGIONS FOR FIG
#---------------------------------------------------------------------------------------------------------------------------------------------
read.csv(paste0(results.directory, 'sst_oisst_97_20_Icy_Strait_monthly_summary.csv')) -> Icy_Strait
read.csv(paste0(results.directory, 'sst_oisst_97_20_Chatham_monthly_summary.csv')) -> Chatham_Strait
read.csv(paste0(results.directory, 'sst_oisst_97_20_NSEAK_monthly_summary.csv')) -> NSEAK
read.csv(paste0(results.directory, 'sst_oisst_97_20_SST_Jordan_monthly_summary.csv')) %>%
  merge (., Icy_Strait) %>%
  merge (., Chatham_Strait) %>% 
  merge (., NSEAK) -> fig_data

read.csv(paste0(data.directory, 'SECMvar2021.csv')) %>%
  dplyr::select(year, year, ISTI3_May, ISTI10_May, ISTI15_May, ISTI20_May, IS3_May) %>%
  write.csv(., paste0(results.directory, 'SECMvar2021_May.csv'), row.names = FALSE)

read.csv(paste0(data.directory, 'SECMvar2021.csv')) %>%
  dplyr::select(year, ISTI3_MJJ, ISTI10_MJJ, ISTI15_MJJ, ISTI20_MJJ, IS3_MJJ) %>%
  write.csv(., paste0(results.directory, 'SECMvar2021_MJJ.csv'), row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------
# CREATE FIGURES OF TEMPERATURE DATA BY REGION
#-------------------------------------------------------------------------------------------------------------
# create a figure of SST_MJJ and SST_May by region and for the SECM survey
read.csv(paste0(data.directory, 'SECMvar2021.csv')) %>%
merge(.,fig_data) %>%
  dplyr::select(year, ISTI3_MJJ, ISTI3_May, Icy_Strait_SST_MJJ, Icy_Strait_SST_May) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group =var)) +
    geom_point(aes(shape = var, color = var, size=var)) +
    geom_line(aes(linetype = var, color = var)) +
    scale_linetype_manual(values=c("solid", "solid", "dotted", "dotted" ))+
    scale_shape_manual(values=c(1, 16, 1, 16)) +
    scale_color_manual(values=c('black','black', 'grey70', 'grey70'))+
    scale_size_manual(values=c(2,2,2,2)) +
    theme(legend.title=element_blank(),
                     panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
                     legend.text=element_text(size=12), 
                     axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
                     axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
          legend.position=c(0.85,0.18))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(4,5,6,7, 8, 9,10,11,12,13), limits = c(4,13))+
  geom_text(aes(x = 1997, y = 13, label="A)"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot1

read.csv(paste0(data.directory, 'SECMvar2021.csv')) %>%
  merge(.,fig_data) %>%
  dplyr::select(year, IS3_MJJ, IS3_May, SST_Jordan_MJJ, SST_Jordan_May) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group =var)) +
  geom_point(aes(shape = var, color = var, size=var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "solid", "dotted", "dotted" ))+
  scale_shape_manual(values=c(1, 16, 1, 16)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70'))+
  scale_size_manual(values=c(2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=12), 
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position=c(0.85,0.18))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(4,5,6,7, 8, 9,10,11,12,13), limits = c(4,13))+
  geom_text(aes(x = 1997, y = 13, label="B)"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 2, ncol=1)
ggsave(paste0(results.directory, "monthly_temp.png"), dpi = 500, height = 6, width = 6.5, units = "in")

# create a figure of SST_MJJ, SST_May, SST_AMJJ by region
fig_data %>%
  dplyr::select(year, Icy_Strait_SST_May, Chatham_Strait_SST_May, NSEAK_SST_May, SST_Jordan_May) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70'))+
  scale_size_manual(values=c(2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=12), 
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position=c(0.8,0.8))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(5,6,7, 8, 9,10,11,12,13), limits = c(5,13))+
  geom_text(aes(x = 1999, y = 13, label="A) May temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot1

fig_data %>%
  dplyr::select(year, Icy_Strait_SST_MJJ, Chatham_Strait_SST_MJJ, NSEAK_SST_MJJ, SST_Jordan_MJJ) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70'))+
  scale_size_manual(values=c(2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=12), 
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position=c(0.8,0.2))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(5,6,7, 8, 9,10, 11, 12,13), limits = c(5,13))+
  geom_text(aes(x = 2000.5, y = 13, label="B) May, June, July temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot2

fig_data %>%
  dplyr::select(year, Icy_Strait_SST_AMJJ, Chatham_Strait_SST_AMJJ, NSEAK_SST_AMJJ, SST_Jordan_AMJJ) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70'))+
  scale_size_manual(values=c(2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=12), 
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position=c(0.8,0.2))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(5,6,7, 8, 9,10, 11, 12,13), limits = c(5,13))+
  geom_text(aes(x = 2001.5, y = 13, label="C) April, May, June, July temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot3
cowplot::plot_grid(plot1,plot2,plot3, align = "v", nrow = 3, ncol=1)
ggsave(paste0(results.directory, "monthly_temp_regions.png"), dpi = 500, height = 8, width = 7, units = "in")

# create a figure of ISTI_May and ISTI_MJJ for the SECM survey
read.csv(paste0(data.directory, 'SECMvar2021.csv')) %>%
  dplyr::select(year, ISTI3_May, ISTI10_May, ISTI15_May, ISTI20_May, IS3_May) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size=var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2, 8)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70', 'black'))+
  scale_size_manual(values=c(2,2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=12), 
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position=c(0.9,0.2))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(5,6,7, 8, 9,10,11,12,13), limits = c(5,13))+
  geom_text(aes(x = 1999, y = 13, label="A) May temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot1

read.csv(paste0(data.directory, 'SECMvar2021.csv')) %>%
  dplyr::select(year, ISTI3_MJJ, ISTI10_MJJ, ISTI15_MJJ, ISTI20_MJJ, IS3_MJJ) %>%
  gather("var", "value", -c(year)) %>% 
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size=var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2,8)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70','black'))+
  scale_size_manual(values=c(2,2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=12), 
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position=c(0.9,0.2))  +
  scale_x_continuous(breaks = 1997:2020, labels = 1997:2020) +
  scale_y_continuous(breaks = c(5,6,7, 8, 9,10,11,12,13), limits = c(5,13))+
  geom_text(aes(x = 2000.5, y = 13, label="B) May, June, July temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot2
cowplot::plot_grid(plot1,plot2, align = "vh", nrow = 2, ncol=1)
ggsave(paste0(results.directory, "monthly_SECM_temp_regions.png"), dpi = 500, height = 7, width = 7, units = "in")
#-------------------------------------------------------------------------------------------------------------------------------------------
## CREATE A MAP OF THE SATELLITE DATA STATIONS
#-------------------------------------------------------------------------------------------------------------------------------------------
data(nepacLLhigh)   #import land data
# color code
.PBSdot <- 3; .PBSdash <- 2 
.PBSclr <- function(){ 
  PBSclr = list( black=c(0,0,0), 
                 sea=c(224,253,254), land=c(255,255,195), red=c(255,0,0), 
                 green=c(0,255,0), blue=c(0,0,255), yellow=c(255,255,0), 
                 cyan=c(0,255,255), magenta=c(255,0,255), purple=c(150,0,150), 
                 lettuce=c(205,241,203), moss=c(132,221,124), irish=c(54,182,48), 
                 forest=c(29,98,27), white=c(255,255,255), fog=c(223,223,223) ) 
  PBSclr <- lapply(PBSclr,function(v) {rgb(v[1],v[2],v[3],maxColorValue=255) }) 
  return(PBSclr) } 
clr <- .PBSclr() 

# https://www.nhc.noaa.gov/gccalc.shtml
# https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles#:~:text=Degrees%20of%20latitude%20are%20parallel%20so%20the%20distance,latitude%20is%20approximately%2069%20miles%20%28111%20kilometers%29%20apart.
# determine scale for map
d = 59 #convert latitude into decimal degrees ~ 59.0
d * 0.01745329-> radians # convert degrees to radians using function
cos(radians) -> cosine # take the cosine of the value in radians
cosine * 69.172 -> miles  # 1 degree of Longitude = ~ 0.79585 * 69.172 = ~ 55.051 miles
miles * 0.86897624 -> nm # -131 to -132 = ~ 31 nm 

read.csv(file.path(data.directory,'SST_data_map.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> SST_data
SST_data %>%
  filter(variable == "scale") -> scale

SST_data %>%
  filter(region == "Icy_Strait") -> Icy_Strait

SST_data %>%
  filter(region == "Chatham_Strait") -> Chatham_Strait

SST_data %>%
  filter(region == "NSEAK") -> NSEAK

SST_data %>%
  filter(region == "SSEAK") -> SSEAK

SST_data %>%
  filter(region == "Upper_Chatham_Strait_SECM") -> Upper_Chatham_Strait_SECM

SST_data %>%
  filter(region == "Icy_Strait_SECM") -> Icy_Strait_SECM

SST_data %>%
  filter(region == "SST_Jordan") -> SST_Jordan

scale <- as.PolySet(scale, projection="LL") # nautical mile scale for map
Icy_Strait <- as.PolyData(Icy_Strait, projection="LL") # general SECM survey area (Icy Strait region)
Chatham_Strait <- as.PolyData(Chatham_Strait, projection="LL") # Chatham Strait data
NSEAK <- as.PolyData(NSEAK, projection="LL") # NSEAK data
SSEAK <- as.PolyData(SSEAK, projection="LL") # SSEAK data
Upper_Chatham_Strait_SECM <- as.PolyData(Upper_Chatham_Strait_SECM, projection="LL") #  Icy Strait SECM stations
Icy_Strait_SECM <- as.PolyData(Icy_Strait_SECM, projection="LL") # Icy Strait SECM stations
SST_Jordan <- as.PolyData(SST_Jordan, projection="LL") # Icy Strait SECM stations

x<-c(-138, -131.5)   #coordinates of land data
y<-c(56,59.5)

# Icy Strait region
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "Icy_Strait.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(Icy_Strait, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
addPoints(Upper_Chatham_Strait_SECM, xlim=x,ylim=y,col=2,pch=16, cex=0.75) # station points
addPoints(Icy_Strait_SECM, xlim=x,ylim=y,col=2,pch=16, cex=0.75) # station points
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-137, 56.5, rot="trueN", cex=1)
legend (-134.2, 58.9, legend=c("Icy Strait SST locations", "SECM stations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-136,57.1,"Pacific Ocean", cex=1.25, adj=1, font=1)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()

# Chatham_Strait region
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "Chatham.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(Chatham_Strait, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-137, 56.5, rot="trueN", cex=1)
legend (-134.8, 58.95, legend=c("Chatham Strait SST locations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-136,57.1,"Pacific Ocean", cex=1.25, adj=1, font=1)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()

# NSEAK region
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "NSEAK.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(NSEAK, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-137, 56.5, rot="trueN", cex=1)
legend (-134.2, 58.9, legend=c("NSEAK SST locations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-136,57.1,"Pacific Ocean", cex=1.25, adj=1, font=1)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()

# SST_Jordan region
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "SST_Jordan.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(Icy_Strait_SECM, xlim=x,ylim=y,col=2,pch=16, cex=0.75) # station points
addPoints(SST_Jordan, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-137, 56.5, rot="trueN", cex=1)
legend (-134.5, 58.9, legend=c("SST_Jordan locations", "SECM stations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-136,57.1,"Pacific Ocean", cex=1.25, adj=1, font=1)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()

x<-c(-138, -129)   #coordinates of land data
y<-c(54,59.5)

# SEAK region
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "SEAK.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(SSEAK, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
addPoints(NSEAK, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-136.5, 55, rot="trueN", cex=1)
legend (-133, 58.8, legend=c("SEAK SST locations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-135,56,"Pacific Ocean", cex=1.25, adj=1, font=0.75)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()
