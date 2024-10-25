# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/10/2022
# last update: October 2024
# pink_cal_pooled_species
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
# update all data files varyyyy_final.csv
# Note: summary table 5 needs to be updated manually from the excel sheet model_summary_table_month_year.xlsx

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
library(dLagM) # MASE calc
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(dplyr)
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

# automatically create output folder MAPE, retro, and figs (by model)
if(!dir.exists(file.path(year.forecast,  'results/MAPE', '/'))){dir.create(file.path(year.forecast,  'results/MAPE', '/'))}
if(!dir.exists(file.path(year.forecast,  'results/retro', '/'))){dir.create(file.path(year.forecast,  'results/retro', '/'))}

# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_temp # update file names
read.csv(file.path(data.directory,'adj_raw_pink.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_adj_raw_pink # update file names

merge(variables_temp, variables_adj_raw_pink, by.x = c("JYear", "Year"), by.y = c("JYear", "Year")) -> variables
variables %>%
  write.csv(., paste0(data.directory, "/var2024_merge.csv"), row.names = F)

# restructure the data for modeling
n <- dim(variables)[1] # number of years including forecast year
variables %>%
  mutate (SEAKCatch_log = log(SEAKCatch),
          adj_raw_pink_log = log(adj_raw_pink +1)) %>% # log catch variable
  dplyr::select(c(JYear,	Year,	ISTI20_MJJ, CPUE, NSEAK_SST_May,	region,	haul_number,	locality,
                  month,	date,	station,	vessel, adj_raw_pink_log)) %>% 
  mutate (odd_even_factor = ifelse(Year %% 2 == 0, "even", "odd"))-> log_data # determine if an add or even number (is it divisible by 2?)

MODELS




# restructure the data (for write-up)
variables %>%
  mutate(Harvest = round(SEAKCatch, 2),
         'Juvenile year' = Year-1,
         CPUE = round(CPUE, 2)) %>%
  dplyr::select(c(Year, 'Juvenile year', Harvest, CPUE)) %>%
  write.csv(., paste0(results.directory, "/data_used_harvest.csv"), row.names = F)

# STEP #2: STEPWISE REGRESSION
# take out any variables that aren't a full time series from 1997-2024
nullmod <- lm(SEAKCatch_log ~ 1, data = log_data)
fullmod <- lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ	+ Chatham_SST_MJJ	+ Chatham_SST_May	+ Chatham_SST_AMJJ + Chatham_SST_AMJ + 
                Icy_Strait_SST_MJJ + Icy_Strait_SST_May + Icy_Strait_SST_AMJJ	+ Icy_Strait_SST_AMJ + NSEAK_SST_MJJ + NSEAK_SST_May +
                NSEAK_SST_AMJJ + NSEAK_SST_AMJ + SEAK_SST_MJJ + SEAK_SST_May + SEAK_SST_AMJJ + SEAK_SST_AMJ, data = log_data)
model <- SEAKCatch_log ~ CPUE + ISTI20_MJJ	+ Chatham_SST_MJJ	+ Chatham_SST_May	+ Chatham_SST_AMJJ + Chatham_SST_AMJ + 
                Icy_Strait_SST_MJJ + Icy_Strait_SST_May + Icy_Strait_SST_AMJJ	+ Icy_Strait_SST_AMJ + NSEAK_SST_MJJ + NSEAK_SST_May +
                NSEAK_SST_AMJJ + NSEAK_SST_AMJ + SEAK_SST_MJJ + SEAK_SST_May + SEAK_SST_AMJJ + SEAK_SST_AMJ
regF <- step(nullmod, scope = list(lower = nullmod, upper = fullmod),
              direction="forward")
regB <- step(nullmod, scope = list(lower = nullmod, upper = fullmod),
              direction="backward")
regS <- step(nullmod, scope = list(lower = nullmod, upper = fullmod),
             direction="both")
summary(regF)
summary(regB)
summary(regS)
StepReg::stepwise(formula = model,
              data=log_data,
              include=NULL,
              strategy="bidirection",
              metric="AIC",
              sle=0.15,
              sls=0.15,
              weight=NULL)

StepReg::stepwise(formula = model,
                  data=log_data,
                  include=NULL,
                  strategy="bidirection",
                  metric="AIC",
                  type = "linear",
                  sle=0.15,
                  sls=0.15,
                  weight=NULL)

# the best model based on the stepwise regression becomes model 19 below

# potential models
model.names <- c(m1='CPUE',
               m2='CPUE + ISTI20_MJJ',
               m3='CPUE + Chatham_SST_May',
               m4='CPUE + Chatham_SST_MJJ',
               m5='CPUE + Chatham_SST_AMJ',
               m6='CPUE + Chatham_SST_AMJJ',
               m7='CPUE + Icy_Strait_SST_May',
               m8='CPUE + Icy_Strait_SST_MJJ',
               m9='CPUE + Icy_Strait_SST_AMJ',
               m10='CPUE + Icy_Strait_SST_AMJJ',
               m11='CPUE + NSEAK_SST_May',
               m12='CPUE + NSEAK_SST_MJJ',
               m13='CPUE + NSEAK_SST_AMJ',
               m14='CPUE + NSEAK_SST_AMJJ',
               m15='CPUE + SEAK_SST_May',
               m16='CPUE + SEAK_SST_MJJ',
               m17='CPUE + SEAK_SST_AMJ',
               m18='CPUE + SEAK_SST_AMJJ')
# model formulas (maintain the same order as above)
model.formulas <- c(SEAKCatch_log ~ CPUE,
                 SEAKCatch_log ~ CPUE + ISTI20_MJJ,
                 SEAKCatch_log ~ CPUE + Chatham_SST_May,
                 SEAKCatch_log ~ CPUE + Chatham_SST_MJJ,
                 SEAKCatch_log ~ CPUE + Chatham_SST_AMJ,
                 SEAKCatch_log ~ CPUE + Chatham_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_May,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJ,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_May,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + SEAK_SST_May,
                 SEAKCatch_log ~ CPUE + SEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE + SEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE + SEAK_SST_AMJJ) # temp. data

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values)
# if there is an error about MASE, run the MASE part of the function file and then rerun line 204

# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data %>%
  dplyr::filter(JYear < year.data) -> log_data_subset

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_subset) -> m2
lm(SEAKCatch_log ~ CPUE + Chatham_SST_May, data = log_data_subset) -> m3
lm(SEAKCatch_log ~ CPUE + Chatham_SST_MJJ, data = log_data_subset) -> m4
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJ, data = log_data_subset) -> m5
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJJ, data = log_data_subset) -> m6
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset) -> m7
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m11
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ, data = log_data_subset) -> m12
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ, data = log_data_subset) -> m13
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset) -> m14
lm(SEAKCatch_log ~ CPUE + SEAK_SST_May, data = log_data_subset) -> m15
lm(SEAKCatch_log ~ CPUE + SEAK_SST_MJJ, data = log_data_subset) -> m16
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJ, data = log_data_subset) -> m17
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJJ, data = log_data_subset) -> m18

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

mutate(model = c('m1','m1','m2','m2','m2','m3','m3','m3',
                 'm4','m4','m4','m5','m5','m5','m6','m6',' m6',
                 'm7','m7','m7','m8','m8','m8','m9','m9',' m9',
                 'm10','m10','m10','m11','m11','m11','m12','m12',' m12',
                 'm13','m13','m13','m14','m14','m14','m15','m15',' m15',
                 'm16','m16','m16','m17','m17','m17','m18','m18',' m18')) %>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(Model = model,
         Term =term,
          Estimate = round(estimate,8),
         'Standard Error' = round(std.error,3),
         Statistic = round(statistic,3),
         'p value' = round(p.value,3)) %>%
  dplyr::select(Model, Term, Estimate, 'Standard Error', Statistic, 'p value') %>%
write.csv(., paste0(results.directory, "/model_summary_table1.csv"), row.names = F) # detailed model summaries

# calculate one step ahead MAPE
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
# forecasted in the one step ahead process)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html

# STEP #3: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2018)  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)
f_model_one_step_ahead_multiple10(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2013)  # start = 1997, end = 2011 means Jyear 2012-2021 used for MAPE calc. (10-year)
# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
# function is correct for the base CPUE model

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  mutate(MAPE5 = round(MAPE5,3)) %>%
  dplyr::select(terms, MAPE5) -> MAPE5

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead10.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  mutate(MAPE10 = round(MAPE10,3)) %>%
  dplyr::select(terms, MAPE10) -> MAPE10

# format the file seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr # mase3<-dLagM::MASE(m5)
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, sigma) %>%
  mutate(AdjR2 = round(AdjR2,3)) %>%
  mutate(Model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                   'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                   'm18')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(Fit = round(fit_log,3),
         Fit_LPI = round(fit_log_LPI,3),
         Fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2) %>%
  merge(., MAPE5, by="terms") %>%
  merge(., MAPE10, by="terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2.csv"), row.names = F)

# run function f_model_one_step_ahead for each model;
# these return results_modelxx.csv files for the one step ahead forecast for each model;
# comment out the "return(data)" of the function in the functions.R file if you want the one-step-ahead-MAPE;
# these results are used in the model_summary_table_month_year.xlsx file; they need to be input by hand into the excel sheet 
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE, start = 1997, end = 2013, model_num = "m1") # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc.(5-year MAPE)
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + ISTI20_MJJ, start = 1997, end = 2013, model_num = "m2")# start = 1997, end = 2011 means Jyear 2012-2021 used for MAPE calc. (10-year MAPE)
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_May, start = 1997, end = 2013, model_num = "m3")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_MJJ, start = 1997, end = 2013, model_num = "m4")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_AMJ, start = 1997, end = 2013, model_num = "m5")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_AMJJ, start = 1997, end = 2013, model_num = "m6")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_May, start = 1997, end = 2013, model_num = "m7")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_MJJ, start = 1997, end = 2013, model_num = "m8")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_AMJ, start = 1997, end = 2013, model_num = "m9")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_AMJJ, start = 1997, end = 2013, model_num = "m10")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_May, start = 1997, end = 2013, model_num = "m11")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_MJJ, start = 1997, end = 2013, model_num = "m12")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_AMJ, start = 1997, end = 2013, model_num = "m13")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_AMJJ, start = 1997, end = 2013, model_num = "m14")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_May, start = 1997, end = 2013, model_num = "m15")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_MJJ, start = 1997, end = 2013, model_num = "m16")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_AMJ, start = 1997, end = 2013, model_num = "m17")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_AMJJ, start = 1997, end = 2013, model_num = "m18")
# the results of the models need to be manually entered into the model_summary_table_month_year.xlsx sheet in the results/summary_tables folder
# summary tables 3a, 3b, and 3c are made from the automated tables

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('1','2','3','4','5','6','7','8',
                   '9','10','11','12','13','14','15','16','17',
                   '18')) %>%
  mutate(order = c('1','2','3','4','5','6','7','8',
                   '9','10','11','12','13','14','15','16','17',
                   '18')) %>%
  mutate(order = as.numeric(order)) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
  ggplot(., aes(x=factor(model, level=c('1','2','3','4','5','6','7','8',
                                        '9','10','11','12','13','14','15','16','17',
                                        '18')), y=fit_log)) +
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
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40, 45, 50), limits = c(0,50))+
  #scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), limits = c(0,20))+
  labs(x = "Models", y = "2025 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "figs/forecast_models.png"), dpi = 500, height = 4, width = 7, units = "in")

 # # test of predict results
 # model.m11 = lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset)
 # best.model <- m11 # this can be added after steps 1 and 2 after the best model is determined
 # last_year_data_cpue <- 1.45
 # sigma<- sigma(best.model) # best model
 # CPUE <- last_year_data_cpue # last year of data
 # NSEAK_SST_May <- 7.62
 # # last year of data
 # newdata <- data.frame(CPUE)
 # preds<-predict(model.m11, newdata, interval="prediction", level = 0.80, se.fit=T)
 # preds
 # 
 # z <- predict(model.m11, newdata, se.fit = TRUE)
 # alpha <- 0.80  ## 90%
 # Qt <- c(-1, 1) * qt((1 - alpha) / 2, z$df, lower.tail = FALSE)
 # CI <- z$fit + outer(z$se.fit, Qt)
 # colnames(CI) <- c("lwr", "upr")
 # CI
 
 # read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
 # results %>%
 #   dplyr::rename(terms = 'X') %>%
 #   dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, AICc, BIC,	p.value,	sigma,	MAPE,	MAPE_LOOCV,	MASE, wMAPE) %>%
 #   mutate(AdjR2 = round(AdjR2,3),
 #          sigma = round(sigma,3),
 #          AICc = round(AICc,2),
 #          MAPE = round(MAPE,3),
 #          MASE = round(MASE,3),
 #          MAPE_LOOCV = round(MAPE_LOOCV,3),
 #          wMAPE = round(wMAPE, 3)) %>%
 #   mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
 #                    'm9','m10','m11','m12','m13','m14','m15','m16',' m17',
 #                    'm18')) %>%
 #   mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
 #          fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
 #          fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
 #   mutate(fit = round(fit_log,3),
 #          fit_LPI = round(fit_log_LPI,3),
 #          fit_UPI = round(fit_log_UPI,3)) %>%
 #   dplyr::select(model, terms, fit,	fit_LPI, fit_UPI) %>%
 #   write.csv(paste0(results.directory, "/model_summary_table3.csv"), row.names = F)