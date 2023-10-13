# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/10/2022
# last update: Sept 2023

# load libraries
# load libraries
library("devtools")
library(tidyverse)
library(ggplot2)
library(readr)
library(psych)
library(PerformanceAnalytics)
#extrafont::font_import() # only need to run this once, then comment out
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_report(base_size = 14))


# inputs
year.forecast <- "2024_forecast" # forecast year 
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')

# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'explore_variables.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables

cor.test(variables$SEAK_catch, variables$CPUE, method="pearson")
cor.test(variables$SEAK_catch, variables$ISTI20_MJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$condition_June, method="pearson")
cor.test(variables$SEAK_catch, variables$condition_July, method="pearson")

cor.test(variables$SEAK_catch, variables$energy_density_June, method="pearson")
cor.test(variables$SEAK_catch, variables$energy_density_July, method="pearson")
cor.test(variables$SEAK_catch, variables$May_DV, method="pearson")
cor.test(variables$SEAK_catch, variables$June_DV, method="pearson")
cor.test(variables$SEAK_catch, variables$July_DV, method="pearson")
cor.test(variables$SEAK_catch, variables$zoo_density_May, method="pearson")
cor.test(variables$SEAK_catch, variables$zoo_density_June, method="pearson")
cor.test(variables$SEAK_catch, variables$zoo_density_July, method="pearson")
cor.test(variables$SEAK_catch, variables$NPI, method="pearson")

cor.test(variables$SEAK_catch, variables$Chatham_SST_MJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$Chatham_SST_May, method="pearson")
cor.test(variables$SEAK_catch, variables$Chatham_SST_AMJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$Chatham_SST_AMJ, method="pearson")

cor.test(variables$SEAK_catch, variables$Icy_Strait_SST_MJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$Icy_Strait_SST_May, method="pearson")
cor.test(variables$SEAK_catch, variables$Icy_Strait_SST_AMJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$Icy_Strait_SST_AMJ, method="pearson")

cor.test(variables$SEAK_catch, variables$NSEAK_SST_MJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$NSEAK_SST_May, method="pearson")
cor.test(variables$SEAK_catch, variables$NSEAK_SST_AMJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$NSEAK_SST_AMJ, method="pearson")

cor.test(variables$SEAK_catch, variables$SEAK_SST_MJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$SEAK_SST_May, method="pearson")
cor.test(variables$SEAK_catch, variables$SEAK_SST_AMJJ, method="pearson")
cor.test(variables$SEAK_catch, variables$SEAK_SST_AMJ, method="pearson")

variables %>%
  select(-Year, -JYear, -SEAK_catch, -energy_density_June, -condition_June) %>%
  chart.Correlation(.)

variables %>%
  select(-Year, -JYear, -SEAK_catch,  -energy_density_June, -condition_June) %>%
corr.test(.)

variables %>%
  select(-Year, -JYear, -SEAK_catch,  -energy_density_June, -condition_June) %>% # delete variables that don't have a value for every year
  cor(.) %>%
  write.csv(., paste0(results.directory, 'correlations.csv'))



