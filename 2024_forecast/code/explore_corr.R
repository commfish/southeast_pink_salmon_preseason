# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/10/2022
# last update: Sept 2023

# load libraries
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(gam)
library(MASS)
library(MuMIn)
library(AICcmodavg)
library(broom)
library(rpart)
library(mda)
library(tidyverse)
library(dLagM) # MASE calc
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(dplyr)
library(extrafont)
library(ggrepel)
library(Metrics) # MASE calc
library(MetricsWeighted)
library(stats)
library("RColorBrewer") 
#extrafont::font_import() # only need to run this once, then comment out
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2024_forecast" # forecast year 
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')

# read in data from the csv file  (make sure this is up to date)
variables<- read.csv(file.path(data.directory,'explore_variables'), header=TRUE, as.is=TRUE, strip.white=TRUE) 
cor.test(variables$SEAKCatch, variables$ISTI20_MJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$Chatham_SST_MJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$Chatham_SST_May, method="pearson")
cor.test(variables$SEAKCatch, variables$Chatham_SST_AMJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$Chatham_SST_AMJ, method="pearson")

cor.test(variables$SEAKCatch, variables$Icy_Strait_SST_MJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$Icy_Strait_SST_May, method="pearson")
cor.test(variables$SEAKCatch, variables$Icy_Strait_SST_AMJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$Icy_Strait_SST_AMJ, method="pearson")

cor.test(variables$SEAKCatch, variables$NSEAK_SST_MJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$NSEAK_May, method="pearson")
cor.test(variables$SEAKCatch, variables$NSEAK_AMJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$NSEAK_AMJ, method="pearson")

cor.test(variables$SEAKCatch, variables$SSEAK_SST_MJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$SSEAK_May, method="pearson")
cor.test(variables$SEAKCatch, variables$SSEAK_AMJJ, method="pearson")
cor.test(variables$SEAKCatch, variables$SSEAK_AMJ, method="pearson")

cor.test(variables$SEAKCatch, variables$NPI, method="pearson")
cor.test(variables$SEAKCatch, variables$CPUE, method="pearson")

cor.test(variables$SEAKCatch, variables$energy_density_June, method="pearson")
cor.test(variables$SEAKCatch, variables$energy_density_July, method="pearson")
cor.test(variables$SEAKCatch, variables$May_DV, method="pearson")
cor.test(variables$SEAKCatch, variables$June_DV, method="pearson")
cor.test(variables$SEAKCatch, variables$July_DV, method="pearson")
cor.test(variables$SEAKCatch, variables$zoo_density_May, method="pearson")
cor.test(variables$SEAKCatch, variables$zoo_density_June, method="pearson")
cor.test(variables$SEAKCatch, variables$zoo_density_July, method="pearson")


