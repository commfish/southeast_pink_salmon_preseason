# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov) with assistance from Jordan Watson (jordan.watson@noaa.gov)
# June 2025

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
library(ggpubr)
# extrafont::font_import() # only needs to be run once for extra fonts for figures
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# create a folder for temperature_data
out.path <- paste0("2026_forecast/results/temperature_data/") # update year
if(!exists(out.path)){dir.create(out.path)}

# set up directories----
year.forecast <- "2026_forecast" # update year
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,  'results/temperature_data', '/')
#------------------------------------------------------------------------------------------------------------------------------------------
# DOWNLOAD SST DATA FROM THE NOAA SITE AND CREATE CSV FILE FOR YEARS 1997 to present (April - July data)
#------------------------------------------------------------------------------------------------------------------------------------------
# April 1997 to July yyyy data----
# download the data for a fixed spatial and temporal period	(SST data, NOAA Global Coral Bleaching Monitoring, 5km, V.3.1, daily, 1985-present)
# to do this, go to the site: https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_monthly.html;
# set the latitude (54, 60) and longitude (-137.2, -130) and time period (April 1997 - July yyyy; yyyy is the current year), and fill type as .nc,
# save the file as NOAA_DHW_monthly_97_yy.nc in the data folder

# https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_monthly.graph; this site is helpful to map the area of interest

tidync(paste0(data.directory,'NOAA_DHW_monthly_97_25.nc')) %>% # update file name
  hyper_tibble() %>% 
  mutate(date=lubridate::as_datetime(time)) %>%
  group_by(date, latitude, longitude) %>% 
  summarise(SST=round(mean(sea_surface_temperature),2)) %>%
  as.data.frame() %>% 
  mutate(year = as.numeric(format(date,'%Y')),
         month = as.numeric(format(date,'%m')),
         day = as.numeric(format(date,'%d'))) %>% 
  filter(month >= 4 & month <= 7) %>% 
  filter(!is.na(SST)) %>%
  dplyr::select(latitude, longitude, SST, year, month, day) %>%
  mutate(longitude = as.numeric(longitude), # lat/long need to be changed to numeric
         latitude = as.numeric(latitude))  %>%
  mutate(longitude = round(longitude, 3), # merge does not work correctly unless the latitude and longitude are exactly the same so round the lat/longs to 3 digits
         latitude = round(latitude, 3))  %>%
  group_by(latitude, longitude, year, month) %>% # average across the month for each lat/long combination
  summarise(SST = mean(SST), .groups = 'drop') %>%
  as.data.frame() -> SST_satellite

SST_satellite %>%
  write.csv(., paste0(data.directory, 'sst_oisst_97_25_monthly_data.csv')) # update file name

# create plot of entire data set  
# SST_satellite %>% 
#   ggplot(aes(longitude,latitude, color = SST)) + 
#   geom_point()-> fig
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
  filter(region == 'SEAK') -> map_data_SEAK

map_data %>%
  filter(region == 'Chatham') -> map_data_Chatham

# merge coordinates from the map_data with the SST data to create SST data by region of interest
merge(map_data_Icy_Strait, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'Icy_Strait') -> Icy_Strait_SST

merge(map_data_NSEAK, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'NSEAK') -> NSEAK_SST

merge(map_data_SEAK, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'SEAK') -> SEAK_SST

merge(map_data_Chatham, SST_satellite, by = c("latitude", "longitude"), all.x = TRUE, all.y = TRUE) %>% 
  dplyr::select(latitude, longitude, region, variable, year, month, SST) %>%
  filter(region == 'Chatham') -> Chatham_SST

# combine region data sets into one csv file (data by lat, long, region, year, month, SST)
rbind(Icy_Strait_SST, NSEAK_SST) %>%
rbind(., SEAK_SST) %>%   
rbind(., Chatham_SST) %>% 
  write.csv(., paste0(data.directory, 'sst_regions_oisst_97_25_monthly_data.csv')) # update file name
#--------------------------------------------------------------------------------------------------------------------------------------------
# SUMMARIZE SST DATA BY REGION, YEAR, MONTH
#--------------------------------------------------------------------------------------------------------------------------------------------
# summarize SST data
read.csv(paste0(data.directory, 'sst_regions_oisst_97_25_monthly_data.csv')) %>% # update file name
  filter(region %in% c('Icy_Strait', 'Chatham', 'NSEAK', 'SEAK')) %>% 
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

# average SST for April-June by region
SST_satellite_grouped %>%
  filter(month %in% c(4,5,6)) %>% 
  group_by(region, year) %>%
  summarise(SST_AMJ = mean(msst), .groups = 'drop') -> SST_AMJ

# combine data sets into one csv file
merge(SST_MJJ, SST_May, by = c("region", "year")) %>%
  merge(., SST_AMJJ, by = c("region", "year")) %>%
  merge(., SST_AMJ, by = c("region", "year")) %>%
  write.csv(., paste0(results.directory, 'sst_regions_oisst_97_25_monthly_data_summary.csv')) # update file name

# create tables by region for the report
read.csv(paste0(results.directory, 'sst_regions_oisst_97_25_monthly_data_summary.csv')) %>% # update file name
  dplyr::select(region, year, SST_MJJ, SST_May, SST_AMJJ, SST_AMJ) -> tempdata

tempdata %>%
  filter(region == "Icy_Strait") %>%
  mutate(Icy_Strait_SST_MJJ = round(SST_MJJ, 2),
         Icy_Strait_SST_May = round(SST_May, 2),
         Icy_Strait_SST_AMJJ = round(SST_AMJJ, 2),
         Icy_Strait_SST_AMJ = round(SST_AMJ, 2)) %>%
  dplyr::select(year, Icy_Strait_SST_MJJ, Icy_Strait_SST_May, Icy_Strait_SST_AMJJ, Icy_Strait_SST_AMJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_25_Icy_Strait_monthly_summary.csv'), row.names = FALSE) # update file name

tempdata %>%
  filter(region == "Chatham") %>%
  mutate(Chatham_SST_MJJ = round(SST_MJJ, 2),
         Chatham_SST_May = round(SST_May, 2),
         Chatham_SST_AMJJ = round(SST_AMJJ, 2),
         Chatham_SST_AMJ = round(SST_AMJ, 2)) %>%
  dplyr::select(year, Chatham_SST_MJJ, Chatham_SST_May, Chatham_SST_AMJJ, Chatham_SST_AMJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_25_Chatham_monthly_summary.csv'), row.names = FALSE) # update file name

tempdata %>%
  filter(region == "NSEAK") %>%
  mutate(NSEAK_SST_MJJ = round(SST_MJJ, 2),
         NSEAK_SST_May = round(SST_May, 2),
         NSEAK_SST_AMJJ = round(SST_AMJJ, 2),
         NSEAK_SST_AMJ = round(SST_AMJ, 2)) %>%
  dplyr::select(year, NSEAK_SST_MJJ, NSEAK_SST_May, NSEAK_SST_AMJJ, NSEAK_SST_AMJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_25_NSEAK_monthly_summary.csv'), row.names = FALSE) # row.name = F gets rid of the first column X1 that is not needed; update file name

tempdata %>%
  filter(region == "SEAK") %>%
  mutate(SEAK_SST_MJJ = round(SST_MJJ, 2),
         SEAK_SST_May = round(SST_May, 2),
         SEAK_SST_AMJJ = round(SST_AMJJ, 2),
         SEAK_SST_AMJ = round(SST_AMJ, 2)) %>%
  dplyr::select(year, SEAK_SST_MJJ, SEAK_SST_May, SEAK_SST_AMJJ, SEAK_SST_AMJ) %>%
  write.csv(., paste0(results.directory, 'sst_oisst_97_25_SEAK_monthly_summary.csv'), row.names = FALSE) # final satellite data by month (copy and paste to var*yyyy*_final.csv file); # update file name
#---------------------------------------------------------------------------------------------------------------------------------------------
# COMBINE DATA SETS FOR FIG
#---------------------------------------------------------------------------------------------------------------------------------------------
read.csv(paste0(results.directory, 'sst_oisst_97_25_Icy_Strait_monthly_summary.csv')) -> Icy_Strait # update file name
read.csv(paste0(results.directory, 'sst_oisst_97_25_Chatham_monthly_summary.csv')) -> Chatham # update file name 
read.csv(paste0(results.directory, 'sst_oisst_97_25_NSEAK_monthly_summary.csv')) -> NSEAK # update file name
read.csv(paste0(results.directory, 'sst_oisst_97_25_SEAK_monthly_summary.csv')) %>% # SEAK region # update file name
  merge (., Icy_Strait) %>%
  merge (., Chatham) %>% 
  merge (., NSEAK) -> fig_data
#-------------------------------------------------------------------------------------------------------------
# CREATE FIGURES OF TEMPERATURE DATA BY REGION
#-------------------------------------------------------------------------------------------------------------
# create a figure of SST_MJJ, SST_May, SST_AMJJ, SST_AMJ by region
fig_data %>%
  dplyr::select(year, Icy_Strait_SST_May, Chatham_SST_May, NSEAK_SST_May, SEAK_SST_May) %>%
  rename(Icy_Strait = Icy_Strait_SST_May,
         Chatham = Chatham_SST_May,
         NSEAK =  NSEAK_SST_May,
         SEAK= SEAK_SST_May) %>% 
  gather("var", "value", -c(year)) %>%
  mutate(var = factor(var,
                      ordered = TRUE,
                      levels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK"),
                      labels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK"))) %>%
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 1, 24)) +
  scale_color_manual(values=c('black','black', 'grey70', 'black')) +
  scale_size_manual(values=c(0.02,2,0.02,2)) +
  theme(legend.title=element_blank(),legend.position = "bottom",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        legend.text=element_text(size=10), 
        axis.title.y = element_text(size=10, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"))+
  scale_x_continuous(breaks = 1997:2025, labels = 1997:2025) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10,11,12), limits = c(6,12))+
  geom_text(aes(x = 1997.4, y = 12, label="D) May"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot1

fig_data %>%
  dplyr::select(year, Icy_Strait_SST_MJJ, Chatham_SST_MJJ, NSEAK_SST_MJJ, SEAK_SST_MJJ) %>%
  rename(Icy_Strait = Icy_Strait_SST_MJJ,
         Chatham = Chatham_SST_MJJ,
         NSEAK =  NSEAK_SST_MJJ,
         SEAK= SEAK_SST_MJJ) %>% 
  gather("var", "value", -c(year)) %>%
  mutate(var = factor(var,
                      ordered = TRUE,
                      levels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK"),
                      labels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK")))%>%
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 1, 24)) +
  scale_color_manual(values=c('black','black', 'grey70', 'black')) +
  scale_size_manual(values=c(0.02,2,0.02,2)) +
  theme(legend.title=element_blank(),legend.position = "none",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        axis.title.y = element_text(size=10, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman")) +
  scale_x_continuous(breaks = 1997:2025, labels = 1997:2025) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10, 11, 12), limits = c(6,12))+
  geom_text(aes(x = 1999.2, y = 12, label="C) May, June, July"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot2

fig_data %>%
  dplyr::select(year, Icy_Strait_SST_AMJJ, Chatham_SST_AMJJ, NSEAK_SST_AMJJ, SEAK_SST_AMJJ) %>%
  rename(Icy_Strait = Icy_Strait_SST_AMJJ,
         Chatham = Chatham_SST_AMJJ,
         NSEAK =  NSEAK_SST_AMJJ,
         SEAK= SEAK_SST_AMJJ) %>% 
  gather("var", "value", -c(year)) %>%
  mutate(var = factor(var,
                      ordered = TRUE,
                      levels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK"),
                      labels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK")))%>% 
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 1, 24)) +
  scale_color_manual(values=c('black','black', 'grey70', 'black')) +
  scale_size_manual(values=c(0.02,2,0.02,2)) +
  theme(legend.title=element_blank(),legend.position = "none",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        axis.title.y = element_text(size=10, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"))  +
  scale_x_continuous(breaks = 1997:2025, labels = 1997:2025) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10, 11,12), limits = c(6,12))+
  geom_text(aes(x = 2000.4, y = 12, label="A) April, May, June, July"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot3

fig_data %>%
  dplyr::select(year, Icy_Strait_SST_AMJ, Chatham_SST_AMJ, NSEAK_SST_AMJ, SEAK_SST_AMJ) %>%
  rename(Icy_Strait = Icy_Strait_SST_AMJ,
         Chatham = Chatham_SST_AMJ,
         NSEAK =  NSEAK_SST_AMJ,
         SEAK= SEAK_SST_AMJ) %>% 
  gather("var", "value", -c(year)) %>%
  mutate(var = factor(var,
                      ordered = TRUE,
                      levels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK"),
                      labels = c("Icy_Strait", "Chatham", "NSEAK", "SEAK")))%>%
  ggplot(., aes(y = value, x = year, group = var)) +
  geom_point(aes(shape = var, color = var, size = var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_shape_manual(values=c(1, 16, 1, 24)) +
  scale_color_manual(values=c('black','black', 'grey70', 'black')) +
  scale_size_manual(values=c(0.02,2,0.02,2)) +
  theme(legend.title=element_blank(),legend.position = "none",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        axis.title.y = element_text(size=10, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman")) +
  scale_x_continuous(breaks = 1997:2025, labels = 1997:2025) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10, 11,12), limits = c(6,12))+
  geom_text(aes(x = 1999.5, y = 12, label="B) April, May, June"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot4
# https://newbedev.com/one-shared-legend-for-a-cowplot-grid-in-r
ggpubr::ggarrange(plot3, plot4, plot2, plot1,  # list of plots
                  #labels = "AUTO", # labels
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom", # legend position
                  align = "v", nrow = 4) # Align them both, horizontal and vertical
ggsave(paste0(results.directory, "monthly_temp_regions.png"), dpi = 500, height = 8, width = 6, units = "in")

# create a figure of ISTI_MJJ for the SECM survey
read.csv(paste0(data.directory, 'var2025_final.csv')) %>% # update file name
  dplyr::select(JYear, ISTI20_MJJ) %>%
  rename(Year = JYear) %>%
write.csv(., paste0(results.directory, 'SECMvar2025_MJJ.csv'), row.names = FALSE) # update file name

read.csv(paste0(data.directory, 'var2025_final.csv')) %>% # update file name 
  dplyr::select(JYear, ISTI20_MJJ) %>%
  gather("var", "value", -c(JYear)) %>% 
  ggplot(., aes(y = value, x = JYear, group = var)) +
  geom_point(aes(shape = var, color = var, size=var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2,8)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70','black'))+
  scale_size_manual(values=c(2,2,2,2,2)) +
  theme(legend.title=element_blank(),
        #panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position="none") +
  scale_x_continuous(breaks = 1997:2025, labels = 1997:2025) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10,11,12,13), limits = c(6,12))+
  #geom_text(aes(x = 2000.5, y = 13, label="May, June, July temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot1
cowplot::plot_grid(plot1, align = "vh", nrow = 1, ncol=1)
ggsave(paste0(results.directory, "monthly_SECM_temp_regions.png"), dpi = 500, height = 5, width = 7, units = "in")
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
  filter(region == "Chatham") -> Chatham

SST_data %>%
  filter(region == "NSEAK") -> NSEAK

SST_data %>%
  filter(region == "SSEAK") -> SSEAK

SST_data %>%
  filter(region == "Upper_Chatham_Strait_SECM") -> Upper_Chatham_Strait_SECM

SST_data %>%
  filter(region == "Icy_Strait_SECM") -> Icy_Strait_SECM

SST_data %>%
  filter(region == "SEAK") -> SEAK


scale <- as.PolySet(scale, projection="LL") # nautical mile scale for map
Icy_Strait <- as.PolyData(Icy_Strait, projection="LL") # general SECM survey area (Icy Strait region)
Chatham <- as.PolyData(Chatham, projection="LL") # Chatham Strait data
NSEAK <- as.PolyData(NSEAK, projection="LL") # NSEAK data
SSEAK <- as.PolyData(SSEAK, projection="LL") # SSEAK data
SEAK <- as.PolyData(SEAK, projection="LL") # SSEAK data
Upper_Chatham_Strait_SECM <- as.PolyData(Upper_Chatham_Strait_SECM, projection="LL") #  Icy Strait SECM stations
Icy_Strait_SECM <- as.PolyData(Icy_Strait_SECM, projection="LL") # Icy Strait SECM stations

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

# Chatham region
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "Chatham.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(Chatham, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
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
addPoints(NSEAK, xlim=x,ylim=y,col=1,pch=16, lwd=0.5, cex=0.5) # satellite SST data
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-137, 56.5, rot="trueN", cex=1)
#legend (-134.2, 58.9, legend=c("NSEAK SST locations"),
#        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
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
addPoints(SSEAK, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.5) # satellite SST data
addPoints(NSEAK, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.5) # satellite SST data
addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-136.5, 55, rot="trueN", cex=1)
legend (-133, 58.8, legend=c("SEAK SST locations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-135,56,"Pacific Ocean", cex=1.25, adj=1, font=0.75)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()

