# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov) with assistance from Jordan Watson (jordan.watson@noaa.gov)
# September 2026

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
out.path <- paste0("2027_forecast/results/temperature_data/") # update year
if(!exists(out.path)){dir.create(out.path)}

# set up directories----
year.forecast <- "2027_forecast" # update year
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,  'results/temperature_data', '/')

# create a figure of ISTI_JJ for the SECM survey
# the varyyyy_final.csv file needs to be updated with the current year's data prior to running the code below 
read.csv(paste0(data.directory, 'var2026_final.csv')) %>% # update file name
  dplyr::select(JYear, ISTI20_JJ) %>%
  rename(Year = JYear) %>%
  write.csv(., paste0(results.directory, 'SECMvar2026_JJ.csv'), row.names = FALSE) # update file name

read.csv(paste0(data.directory, 'var2026_final.csv')) %>% # update file name 
  dplyr::select(JYear, ISTI20_JJ) %>%
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
  scale_x_continuous(breaks = 1997:2026, labels = 1997:2026) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10,11,12,13), limits = c(6,12))+
  #geom_text(aes(x = 2000.5, y = 13, label="May, June, July temperature"),family="Times New Roman", colour="black", size=4) +
  labs(y = "Temperature (Celsius)", x ="") -> plot1
cowplot::plot_grid(plot1, align = "vh", nrow = 1, ncol=1)
ggsave(paste0(results.directory, "annual_ISTI20_JJ.png"), dpi = 500, height = 5, width = 7, units = "in")
