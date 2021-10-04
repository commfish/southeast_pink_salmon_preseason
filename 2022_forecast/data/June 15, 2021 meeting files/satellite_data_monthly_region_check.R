# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov) with help from Jordan Watson (jordan.watson@noaa.gov)
# April 12, 2021
# Download one day of data from the site https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_monthly.graph as a csv file
# and put the data in the file SST_data_map_test similar to the file sst_data_map.csv (POS should be 1, PID is 1 through n, X
# is longitude and should be negative and X is latitude); include a variable and region name. This code will allow you to see the
# data points that make up the region of interest. Then the points can be deleted as needed to represent a certain area.

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

read.csv(file.path(data.directory,'SST_data_map_test.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> SST_data

SST_data %>%
  filter(region == "Icy_Strait") -> Icy_Strait

SST_data %>%
  filter(region == "Chatham") -> Chatham

Icy_Strait <- as.PolyData(Icy_Strait, projection="LL") # SSEAK data

Chatham <- as.PolyData(Chatham, projection="LL") # SSEAK data

x<-c(-138, -129)   #coordinates of land data
y<-c(54,59.5)

# test map
region<-clipPolys(nepacLLhigh,xlim=x,ylim=y)      
par(mfrow=c(1,1),omi=c(0,0,0,0))  
png(paste0(results.directory, "test_map.png"),width=6,height=8,units="in", res=600)                                                        
plotMap(region,xlim=x, ylim=y, tck=-0.02, plt=c(.13,.98,.13,.98),projection="LL", cex=1.2,
        xlab="Longitude (°W)", ylab="Latitude (°N)", cex.lab=1.5, font.lab=6, col=clr$land, bg=clr$sea)
addPoints(Icy_Strait, xlim=x,ylim=y,col=2,pch=16, lwd=1, cex=0.25) # satellite SST data
addPoints(Chatham, xlim=x,ylim=y,col=1,pch=1, lwd=1, cex=0.75) # satellite SST data
#addLines(scale, xlim=x,ylim=y,col=1,lty=1, lwd=2, cex=1)
addCompass(-136.5, 55, rot="trueN", cex=1)
legend (-133, 58.8, legend=c("test SST locations"),
        col=c(1,2), bty="n", cex=1, pch=c(1, 16))
text(-135,56,"Pacific Ocean", cex=1.25, adj=1, font=0.75)
text(-131.7, 59.2,"25 nm at 59°N", cex = 0.75, adj = 1, font = 1)
dev.off()