# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/10/2022
# last update: October 2024
# pink_cal_pooled_species
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
# update all data files varyyyy_final.csv
# Note: summary table 5 needs to be updated manually from the excel sheet model_summary_table_month_year.xlsx
# https://www.r-bloggers.com/2021/10/multiple-linear-regression-made-simple/
# load libraries
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(stats)
library(gam)
library(MASS)
library(MuMIn)
library(AICcmodavg)
library(broom)
library(rpart)
library(mda)
library(tidyverse)
#library(dLagM) # MASE calc
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(extrafont)
library(ggrepel)
library(Metrics) # MASE calc
library(MetricsWeighted)
library("RColorBrewer") 
library(StepReg)
library(ggh4x)
#extrafont::font_import() # only need to run this once, then comment out
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2025_forecast" # forecast year 
year.data <- 2024 # last year of data
year.data.one <- year.data - 1
sample_size <-  (year.data-1998)+1 # number of data points in model (this is used for Cook's distance)
# forecast2023 <- 15.6 # input last year's forecast for the forecast plot
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')
results.directory.MAPE <- file.path(year.forecast,  'results/MAPE', '/')
results.directory.retro <- file.path(year.forecast,  'results/retro', '/')
source('2025_forecast/code/functions.r') # source the function file for functions used below

# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_temp # update file names
read.csv(file.path(data.directory,'adj_raw_pink.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_adj_raw_pink # update file names

variables_adj_raw_pink %>%
  mutate (adj_raw_pink_log = log(adj_raw_pink +1)) %>% # log CPUE variable
          group_by(JYear, Year, vessel) %>% 
          summarise(adj_raw_pink_log = max(adj_raw_pink_log)) %>% 
  merge(., variables_temp, by.x = c("JYear", "Year"), by.y = c("JYear", "Year")) %>%
  dplyr::filter(adj_raw_pink_log > 0) %>%
  dplyr::filter(vessel!= 'Steller') %>%
  dplyr::filter(vessel!= 'Chellissa') %>%
  mutate (odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),
          SEAKCatch_log = log(SEAKCatch)) %>%
  dplyr::select(-c(CPUE, SEAKCatch)) -> log_data

# data check only  
 log_data %>%
    write.csv(., paste0(data.directory, "/var2024_merge_check2.csv"), row.names = F)

 #https://www.jmp.com/en_us/statistics-knowledge-portal/what-is-multiple-regression/mlr-with-interactions.html
 #https://statsandr.com/blog/multiple-linear-regression-made-simple/
 #https://stackoverflow.com/questions/71166010/multiple-regression-with-interaction
 #https://faculty.nps.edu/rbassett/_book/multiple-regression-models.html#multiple-regression-with-categorical-variables-including-the-neighborhood
# STEP #2: MODELS
# allow each vessel to have own slope and y-intercept (multiplicative model; interaction)
mod1 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor), data = log_data)
mod2 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + ISTI20_MJJ, data = log_data)
mod3 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data)
mod4 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Chatham_SST_May, data = log_data)
mod5 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data)
mod6 <- lm(SEAKCatch_log ~  as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data)
mod7 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data)
mod8 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data)
mod9 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data)
mod10 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data)
mod11 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data)
mod12 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data)
mod13<-  lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data)
mod14<-  lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data)
mod15 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data)
mod16 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + SEAK_SST_May, data = log_data)
mod17 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data)
mod18 <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log + adj_raw_pink_log*as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data)
# y-intercept to change with the vessel and odd/even year (additive model); one slope
mod19 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor), data = log_data)
mod20 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + ISTI20_MJJ, data = log_data)
mod21 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data)
mod22 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Chatham_SST_May, data = log_data)
mod23 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data)
mod24 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data)
mod25 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data)
mod26 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data)
mod27 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data)
mod28 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data)
mod29 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data)
mod30 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data)
mod31<-  lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data)
mod32<-  lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data)
mod33 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data)
mod34 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + SEAK_SST_May, data = log_data)
mod35<-  lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data)
mod36 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log:as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data)
# potential models
model.names <- c(m1='vessel x adj_raw_pink_log + odd_factor x adj_raw_pink_log',
                 m2='vessel x adj_raw_pink_log + ISTI20_MJJ + odd_factor x adj_raw_pink_log',
                 m3='vessel x adj_raw_pink_log + Chatham_SST_May + odd_factor x adj_raw_pink_log',
                 m4='vessel x adj_raw_pink_log + Chatham_SST_MJJ + odd_factor x adj_raw_pink_log',
                 m5='vessel x adj_raw_pink_log + Chatham_SST_AMJ + odd_factor x adj_raw_pink_log',
                 m6='vessel x adj_raw_pink_log + Chatham_SST_AMJJ + odd_factor x adj_raw_pink_log',
                 m7='vessel x adj_raw_pink_log + Icy_Strait_SST_May + odd_factor x adj_raw_pink_log',
                 m8='vessel x adj_raw_pink_log + Icy_Strait_SST_MJJ + odd_factor x adj_raw_pink_log',
                 m9='vessel x adj_raw_pink_log + Icy_Strait_SST_AMJ + odd_factor x adj_raw_pink_log',
                 m10='vessel x adj_raw_pink_log + Icy_Strait_SST_AMJJ + odd_factor x adj_raw_pink_log',
                 m11='vessel x adj_raw_pink_log + NSEAK_SST_May + odd_factor x adj_raw_pink_log',
                 m12='vessel x adj_raw_pink_log + NSEAK_SST_MJJ + odd_factor x adj_raw_pink_log',
                 m13='vessel x adj_raw_pink_log+  NSEAK_SST_AMJ + odd_factor x adj_raw_pink_log',
                 m14='vessel x adj_raw_pink_log + NSEAK_SST_AMJJ + odd_factor x adj_raw_pink_log',
                 m15='vessel x adj_raw_pink_log + SEAK_SST_May + odd_factor x adj_raw_pink_log',
                 m16='vessel x adj_raw_pink_log + SEAK_SST_MJJ + odd_factor x adj_raw_pink_log',
                 m17='vessel x adj_raw_pink_log + SEAK_SST_AMJ + odd_factor x adj_raw_pink_log',
                 m18='vessel x adj_raw_pink_log + SEAK_SST_AMJJ + odd_factor x adj_raw_pink_log',
                 m19='vessel : adj_raw_pink_log + odd_factor : adj_raw_pink_log',
                 m20='vessel : adj_raw_pink_log + ISTI20_MJJ + odd_factor : adj_raw_pink_log',
                 m21='vessel : adj_raw_pink_log + Chatham_SST_May + odd_factor: adj_raw_pink_log',
                 m22='vessel : adj_raw_pink_log + Chatham_SST_MJJ + odd_factor: adj_raw_pink_log',
                 m23='vessel : adj_raw_pink_log + Chatham_SST_AMJ + odd_factor: adj_raw_pink_log',
                 m24='vessel : adj_raw_pink_log + Chatham_SST_AMJJ + odd_factor: adj_raw_pink_log',
                 m25='vessel : adj_raw_pink_log + Icy_Strait_SST_May + odd_factor: adj_raw_pink_log',
                 m26='vessel : adj_raw_pink_log + Icy_Strait_SST_MJJ + odd_factor: adj_raw_pink_log',
                 m27='vessel : adj_raw_pink_log + Icy_Strait_SST_AMJ + odd_factor: adj_raw_pink_log',
                 m28='vessel : adj_raw_pink_log + Icy_Strait_SST_AMJJ + odd_factor: adj_raw_pink_log',
                 m29='vessel : adj_raw_pink_log + NSEAK_SST_May + odd_factor: adj_raw_pink_log',
                 m30='vessel : adj_raw_pink_log + NSEAK_SST_MJJ + odd_factor: adj_raw_pink_log',
                 m31='vessel : adj_raw_pink_log+  NSEAK_SST_AMJ + odd_factor: adj_raw_pink_log',
                 m32='vessel : adj_raw_pink_log + NSEAK_SST_AMJJ + odd_factor: adj_raw_pink_log',
                 m33='vessel : adj_raw_pink_log + SEAK_SST_May + odd_factor: adj_raw_pink_log',
                 m34='vessel : adj_raw_pink_log + SEAK_SST_MJJ + odd_factor: adj_raw_pink_log',
                 m35='vessel : adj_raw_pink_log + SEAK_SST_AMJ + odd_factor: adj_raw_pink_log',
                 m36='vessel : adj_raw_pink_log + SEAK_SST_AMJJ + odd_factor: adj_raw_pink_log')

model.formulas <- c(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + ISTI20_MJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_May + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_MJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_AMJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_AMJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_May + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_MJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_AMJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_AMJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_May + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_MJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log+  NSEAK_SST_AMJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_AMJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_May + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_MJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_AMJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_AMJJ + as.factor(odd_even_factor)* adj_raw_pink_log,
                    
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + ISTI20_MJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Chatham_SST_May + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Chatham_SST_MJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Chatham_SST_AMJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Chatham_SST_AMJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Icy_Strait_SST_May + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Icy_Strait_SST_MJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Icy_Strait_SST_AMJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + Icy_Strait_SST_AMJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + NSEAK_SST_May + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + NSEAK_SST_MJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log+  NSEAK_SST_AMJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + NSEAK_SST_AMJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + SEAK_SST_May + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + SEAK_SST_MJJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + SEAK_SST_AMJ + as.factor(odd_even_factor):adj_raw_pink_log,
                    SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + SEAK_SST_AMJJ + as.factor(odd_even_factor):adj_raw_pink_log)

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values, models = "_multi")

# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data %>%
  dplyr::filter(JYear < year.data) -> log_data_subset

lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + ISTI20_MJJ, data = log_data_subset) -> m2
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Chatham_SST_May, data = log_data_subset) -> m3
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Chatham_SST_MJJ, data = log_data_subset) -> m4
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Chatham_SST_AMJ, data = log_data_subset) -> m5
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Chatham_SST_AMJJ, data = log_data_subset) -> m6
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Icy_Strait_SST_May, data = log_data_subset) -> m7
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + NSEAK_SST_May, data = log_data_subset) -> m11
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + NSEAK_SST_MJJ, data = log_data_subset) -> m12
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + NSEAK_SST_AMJ, data = log_data_subset) -> m13
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + NSEAK_SST_AMJJ, data = log_data_subset) -> m14
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + SEAK_SST_May, data = log_data_subset) -> m15
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + SEAK_SST_MJJ, data = log_data_subset) -> m16
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + SEAK_SST_AMJ, data = log_data_subset) -> m17
lm(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)*adj_raw_pink_log + SEAK_SST_AMJJ, data = log_data_subset) -> m18

lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log, data = log_data_subset) -> m19
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + ISTI20_MJJ, data = log_data_subset) -> m20
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Chatham_SST_May, data = log_data_subset) -> m21
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Chatham_SST_MJJ, data = log_data_subset) -> m22
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Chatham_SST_AMJ, data = log_data_subset) -> m23
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Chatham_SST_AMJJ, data = log_data_subset) -> m24
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Icy_Strait_SST_May, data = log_data_subset) -> m25
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Icy_Strait_SST_MJJ, data = log_data_subset) -> m26
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Icy_Strait_SST_AMJ, data = log_data_subset) -> m27
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m28
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + NSEAK_SST_May, data = log_data_subset) -> m29
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + NSEAK_SST_MJJ, data = log_data_subset) -> m30
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + NSEAK_SST_AMJ, data = log_data_subset) -> m31
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + NSEAK_SST_AMJJ, data = log_data_subset) -> m32
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + SEAK_SST_May, data = log_data_subset) -> m33
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + SEAK_SST_MJJ, data = log_data_subset) -> m34
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + SEAK_SST_AMJ, data = log_data_subset) -> m35
lm(SEAKCatch_log ~ as.factor(vessel) : adj_raw_pink_log + as.factor(odd_even_factor):adj_raw_pink_log + SEAK_SST_AMJJ, data = log_data_subset) -> m36

tidy(m1) -> model1
tidy(m2) -> model2
tidy(m3) -> model3
tidy(m4) -> model4
tidy(m5) -> model5
tidy(m6) -> model6
tidy(m7) -> model7
tidy(m8) -> model8
tidy(m9) -> model9
tidy(m10) -> model10
tidy(m11) -> model11
tidy(m12) -> model12
tidy(m13) -> model13
tidy(m14) -> model14
tidy(m15) -> model15
tidy(m16) -> model16
tidy(m17) -> model17
tidy(m18) -> model18
tidy(m19) -> model19
tidy(m20) -> model20
tidy(m21) -> model21
tidy(m22) -> model22
tidy(m23) -> model23
tidy(m24) -> model24
tidy(m25) -> model25
tidy(m26) -> model26
tidy(m27) -> model27
tidy(m28) -> model28
tidy(m29) -> model29
tidy(m30) -> model30
tidy(m31) -> model31
tidy(m32) -> model32
tidy(m33) -> model33
tidy(m34) -> model34
tidy(m35) -> model35
tidy(m36) -> model36

rbind(model1, model2) %>%
rbind(., model3) %>%
rbind(., model4) %>%
rbind(., model5) %>%
rbind(., model6) %>%
rbind(., model7) %>%
rbind(., model8) %>%
rbind(., model9) %>%
rbind(., model10) %>%
rbind(., model11) %>%
rbind(., model12) %>%
rbind(., model13) %>%
rbind(., model14) %>%
rbind(., model15) %>%
rbind(., model16) %>%
rbind(., model17) %>%
rbind(., model18) %>%
rbind(., model19) %>%
rbind(., model20) %>%
rbind(., model21) %>%
rbind(., model22) %>%
rbind(., model23) %>%
rbind(., model24) %>%
rbind(., model25) %>%
rbind(., model26) %>%
rbind(., model27) %>%
rbind(., model28) %>%
rbind(., model29) %>%
rbind(., model30) %>%
rbind(., model31) %>%
rbind(., model32) %>%
rbind(., model33) %>%
rbind(., model34) %>%
rbind(., model35) %>%
rbind(., model36) -> models 
nyear <- 8
model <- c(rep('m1',nyear),rep('m2',nyear+1),rep('m3',nyear+1),rep('m4',nyear+1),
             rep('m5',nyear+1),rep('m6',nyear+1),rep('m7',nyear+1),rep('m8',nyear+1),
             rep('m9',nyear+1),rep('m10',nyear+1),rep('m11',nyear+1),rep('m12',nyear+1),
             rep('m13',nyear+1),rep('m14',nyear+1),rep('m15',nyear+1),rep('m16',nyear+1),
             rep('m17',nyear+1),rep('m18',nyear+1),rep('m19',nyear-3),rep('m20',nyear-2),
             rep('m21',nyear-2),rep('m22',nyear-2),rep('m23',nyear-2),rep('m24',nyear-2),
             rep('m25',nyear-2),rep('m26',nyear-2),rep('m27',nyear-2),rep('m28',nyear-2),
             rep('m29',nyear-2),rep('m30',nyear-2),rep('m31',nyear-2),rep('m32',nyear-2),
             rep('m33',nyear-2),rep('m34',nyear-2),rep('m35',nyear-2),rep('m36',nyear-2))
model<-as.data.frame(model)
cbind(models, model)%>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(Model = model,
         Term =term,
          Estimate = round(estimate,8),
         'Standard Error' = round(std.error,3),
         Statistic = round(statistic,3),
         'p value' = round(p.value,3)) %>%
  dplyr::select(Model, Term, Estimate, 'Standard Error', Statistic, 'p value') %>%
write.csv(., paste0(results.directory, "/model_summary_table1_multi.csv"), row.names = F) # detailed model summaries

# calculate one step ahead MAPE
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
# forecasted in the one step ahead process)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html

# STEP #3: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2018, models="_multi")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)

# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
# function is correct for the base CPUE model

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5_multi.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  mutate(MAPE5 = round(MAPE5,3)) %>%
  dplyr::select(terms, MAPE5) -> MAPE5

# format the file seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr # mase3<-dLagM::MASE(m18)
read.csv(file.path(results.directory,'seak_model_summary_multi.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit, fit_UPI, fit_LPI,AdjR2, sigma, MAPE) %>%
  mutate(AdjR2 = round(AdjR2,3)) %>%
  mutate(Model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                   'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                   'm18','m19','m20'	,'m21'	,'m22'	,'m23'	,'m24'	,'m25'	,
                   'm26'	,'m27'	,'m28'	,'m29'	,'m30'	,'m31'	,'m32'	,'m33'	,
                   'm34'	,'m35'	, 'm36')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  mutate(Fit = round(fit_log,3),
         Fit_LPI = round(fit_log_LPI,3),
         Fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE) %>%
  merge(., MAPE5, by="terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2_multi.csv"), row.names = F)

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary_multi.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit, fit_LPI, fit_UPI, sigma) %>%
  mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                   'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                   'm18','m19','m20'	,'m21'	,'m22'	,'m23'	,'m24'	,'m25'	,
                   'm26'	,'m27'	,'m28'	,'m29'	,'m30'	,'m31'	,'m32'	,'m33'	,
                   'm34'	,'m35'	, 'm36')) %>%
  mutate(order = c('1','2','3','4','5','6','7','8',
                   '9','10','11','12','13','14','15','16','17',
                   '18','19','20'	,'21'	,'22'	,'23'	,'24'	,'25'	,
                   '26'	,'27'	,'28'	,'29'	,'30'	,'31'	,'32'	,'33'	,
                   '34'	,'35'	, '36')) %>%
  mutate(order = as.numeric(order)) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log, fit_log_LPI, fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
ggplot(., aes(x=factor(model, level=c('m1','m2','m3','m4','m5','m6','m7','m8',
                                      'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                                      'm18','m19','m20'	,'m21'	,'m22'	,'m23'	,'m24'	,'m25'	,
                                      'm26'	,'m27'	,'m28'	,'m29'	,'m30'	,'m31'	,'m32'	,'m33'	,
                                      'm34'	,'m35'	, 'm36')), y=fit_log)) +
  geom_col(aes(y = fit_log, fill = "SEAK pink catch"), colour ="grey70",
           width = 1, position = position_dodge(width = 0.1)) +
  scale_colour_manual("", values=c("SEAK pink catch" = "grey90", "fit" = "black")) +
  scale_fill_manual("",values="lightgrey")+ geom_hline(yintercept=20, linetype='dashed', color=c('grey30'))+
  theme_bw() + theme(legend.key=element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_text(size = 7, family="Times New Roman"),
                     legend.title=element_blank(),
                     legend.position = "none") +
  geom_errorbar(mapping=aes(x=model, ymin=fit_log_UPI, ymax=fit_log_LPI), width=0.2, linewidth=1, color="grey30")+
  scale_y_continuous(breaks = c(0,10, 20, 30, 40, 50, 60, 70, 80,100), limits = c(0,100))+
  labs(x = "Models", y = "2025 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "figs/forecast_models_multi.png"), dpi = 500, height = 4, width = 10, units = "in")

# # different intercept, same slope
# mod1a <- lm(SEAKCatch_log ~ as.factor(vessel)+adj_raw_pink_log, data = log_data)
# 
# # different intercept, differnt slope
# mod1b <- lm(SEAKCatch_log ~ as.factor(vessel)*adj_raw_pink_log, data = log_data)
# 
# # same intercept, different slope
# mod1c <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + adj_raw_pink_log, data = log_data)
# 
# summary(mod1c)
# best_model<-m3a
# model<-'m3a'
# year.forecast <- "2025_forecast" # forecast year
# year.data <- 2024 # last year of data
# year.data.one <- year.data - 1
# 
# # best model based on performance metrics
# lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log +adj_raw_pink_log, data = log_data_subset) -> m3a
# 
# # MODEL DIAGNOSTICS TABLES
# as.numeric(sigma(best_model))-> sigma
# augment(best_model) %>% 
#   mutate(Harvest = round((exp(SEAKCatch_log)),2),
#          Residuals = round((.resid),2),
#          'Hat values' = round((.hat),2),
#          'Cooks distance' = round((.cooksd),2),
#          'Std. residuals' = round((.std.resid),2),
#          fitted = round((.fitted),5)) %>%
#   dplyr::select(Harvest, Residuals, 'Hat values', 'Cooks distance', 'Std. residuals', fitted) %>%
#   cbind(.,log_data_subset)%>%
#   write.csv(file =paste0(results.directory, "model_summary_table4_1a", model, ".csv"), row.names = F)
