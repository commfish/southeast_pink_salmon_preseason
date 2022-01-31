library(tidyverse)
library(tidync)
library(lubridate)

#  Download a netcdf            
download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST[(2021-03-01T12:00:00Z):1:(2021-03-01T12:00:00Z)][(59.025):1:(56.025)][(-135.975):1:(-133.975)]"),
              method = "libcurl", mode="wb",destfile = "test.nc")

#  Convert the netcdf to a data frame - the land mask removes the NA automatically.
tidync(paste0("test.nc")) %>% #  Read in the negative longitude sst.
  hyper_tibble() %>% 
  mutate(date=as_datetime(time))


#  Download via csv instead
download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?CRW_SST%5B(2021-03-01T12:00:00Z):1:(2021-03-01T12:00:00Z)%5D%5B(59.025):1:(56.025)%5D%5B(-135.975):1:(-133.975)%5D"),
              "test.csv")

#  The format is a little weird here it seems. The header reads in as two lines so I skipped one line.
#  The csv also seems to include the NaN from the land mask, so I've deleted those and then plotted to make sure it looks okay.
read.csv("test.csv",skip=1,header=TRUE) %>% 
  filter(!is.nan(Celsius)) %>% 
  ggplot(aes(degrees_east,degrees_north,color=Celsius)) + 
  geom_point()
  


