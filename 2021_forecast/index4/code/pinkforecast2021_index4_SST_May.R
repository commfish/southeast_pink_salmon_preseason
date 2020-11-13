# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 9/24/2020
# pink_cal_pooled_species

# load libraries
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(gam)
library(MASS)
library(MuMIn)
library(AICcmodavg)
library(forecast)
library(broom)
library(caret)
library(rpart)
library(mda)
library(tidyverse)
library(dLagM) #MASE calc
library(ggplot2)
library(car)
library(ggfortify)
library(Hmisc)
library(dplyr)
library(extrafont)
library(ggrepel)
#extrafont::font_import()
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2021_forecast" 
year.data <- 2020  
year.data.one <- year.data - 1
sample_size <-  23 # number of data points in model
index <- "index4"
# need to change lines 58-59****
data.directory <- file.path(year.forecast, index, 'data', '/')
results.directory <- file.path(year.forecast, index, 'results', '/')
best.model <- m3 # this can be added after steps 1 and 2 after the best model is determined
last_year_data_cpue <- 2.147502256 # last year of data
last_year_data_SST_May <- 8.085
 # last year of data
source('2021_forecast/functions.r')

# STEP 1: DATA
# read in data
readRDS(file.path(data.directory,'SECM_Sampling_location_sst.RDS')) -> satellite_SST # SST temp data from satellites
read.csv(file.path(data.directory,'SECMcatch2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> catch # update file names
read.csv(file.path(data.directory,'SECMvar2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables #update file names

# summarize SST data
satellite_SST %>%
  group_by(year, month, region) %>%
  summarise(mean_SST = mean(msst), .groups = 'drop') %>%
  as.data.frame() %>%
  group_by(year, month) %>%
  summarise(mean_SST = mean(mean_SST), .groups = 'drop') -> SST_satellite_grouped

# average SST for May, June, July
SST_satellite_grouped %>%
  filter(month %in% c(5,6,7)) %>% 
  group_by(year) %>%
  summarise(SST_MJJ = mean(mean_SST), .groups = 'drop') -> SST_MJJ

# average SST for May
SST_satellite_grouped %>%
  filter(month %in% c(5)) %>% 
  group_by(year) %>%
  summarise(SST_May = mean(mean_SST), .groups = 'drop') -> SST_May

merge(SST_MJJ, SST_May, by=("year")) -> SST_data

# merge satellite data with variables file
left_join(variables, SST_data,  by = c("JYear" = "year")) -> variables
write.csv(variables, paste0(data.directory, "/SST_dataset.csv"), row.names = F)

# restructure the data 
variables$CPUE <- variables$CPUEcal # Use CPUEcal as CPUE index
n <- dim(variables)[1] # number of years including forecast year
variables %>% 
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch, CPUEcal, Pink_Peak)) -> log_data 

catch %>% 
  mutate(Pink = LN_Pink_Cal_pool) -> catch # variable 'Pink' is logged calibrated pink catch (same variable as in SECMvaryyyy.csv file)

# normal data check
eda.norm(log_data$SEAKCatch)# data is normal if the p-value is above 0.05.
eda.norm(log_data$SEAKCatch_log)
eda.norm(log_data$CPUE)# already ln(CPUE+1)
eda.norm(log_data$ISTI)
eda.norm(log_data$ISTI_May)
eda.norm(log_data$SST_May)
eda.norm(log_data$SST_MJJ)

# subset data by peak month (using left_join) and generate list of catch by year (this is used for the bootstrap)
left_join(catch, variables, by = c("Year" = "JYear")) %>% 
  dplyr::select(-c(SEAKCatch, CPUEcal, ISTI, CPUE)) %>% 
  dplyr::filter(Month == Pink_Peak) -> cal.data

cal.data <- split(cal.data$Pink,cal.data$Year)
cal.data

# STEP #2: HARVEST MODELS AND SUMMARY STATS
# define model names and formulas
model.names<-c(m1='CPUE',
               m2='CPUE+ISTI', 
               m3 = 'CPUE + SST_May')
model.formulas<-c(SEAKCatch_log ~ CPUE,
                 SEAKCatch_log ~ CPUE+ISTI,
                 SEAKCatch_log ~ CPUE+SST_May) # temp. data 

# summary statistics and bootstrap of SEAK pink salmon harvest forecast models
seak.model.summary <- model.summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names)
#seak.boot.summary <- boot.summary(cpuedata=cal.data,variables=log_data,model.formulas=model.formulas,model.names=model.names)

# summary of model fits (i.e., coefficients, p-value)
log_data %>% 
  dplyr::filter(JYear < year.data) -> log_data_subset 

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ CPUE + ISTI, data = log_data_subset) -> m2
lm(SEAKCatch_log ~ CPUE + SST_May, data = log_data_subset) -> m3

tidy(m1) -> m11
tidy(m2) -> m22
tidy(m3) -> m33

rbind(m11, m22) %>% 
rbind(., m33) %>% 
mutate(model = c('m1','m1','m2','m2','m2','m3','m3','m3')) %>% 
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(estimate = round(estimate,3),
         std.error = round(std.error,3),
         statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
write.csv(., paste0(results.directory, "/model_summary_table1.csv"), row.names = F)

# leave one out cross validation (verify seak.model.summary)
# https://stats.stackexchange.com/questions/27351/compare-models-loccv-implementation-in-r
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

# MAPE calc. check (check that these match summary file) 3 leave one out cross validation method
log_data %>% 
  filter(JYear < year.data) -> log_data_subset
model_m1 <- train(SEAKCatch_log ~ CPUE, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m2 <- train(SEAKCatch_log ~ CPUE + ISTI, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))

model_m3 <- train(SEAKCatch_log ~ CPUE + SST_May, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
# calculate MASE 
log_data %>% 
  filter(JYear < year.data) -> log_data_subset
model.m1 = lm(SEAKCatch_log ~ CPUE, data = log_data_subset)
model.m2 = lm(SEAKCatch_log ~ CPUE + ISTI, data = log_data_subset)
model.m3 = lm(SEAKCatch_log ~ CPUE + SST_May, data = log_data_subset)

MASE(model.m1, model.m2, model.m3) %>%
  dplyr::select(MASE)-> MASE

# add MASE to summary file
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>% 
  dplyr::select(X, AdjR2, AICc, MAPE, MEAPE) %>%
  dplyr::rename(terms = 'X') %>% 
  mutate(model = ifelse(terms =="CPUE", 'm1',
         ifelse(terms =="CPUE +ISTI", 'm2', 'm3'))) %>% 
  cbind(., MASE) %>%
  dplyr::select(model, terms, AdjR2, AICc, MAPE, MEAPE, MASE) %>%
  mutate(AdjR2 = round(AdjR2,3),
         AICc = round(AICc,0),
         MAPE = round(MAPE,3),
         MEAPE = round(MEAPE,3),
         MASE = round(MASE,3)) %>%
  write.csv(paste0(results.directory, "/model_summary_table2.csv"), row.names = F)

# STEP #5: PREDICT NEXT YEAR'S CATCH BASED ON BEST MODEL
# bootstrap
# http://rstudio-pubs-static.s3.amazonaws.com/24365_2803ab8299934e888a60e7b16113f619.html
# prediction m2
sigma<- sigma(best.model) # best model
CPUE <- last_year_data_cpue # last year of data
SST_May <- last_year_data_SST_May # last year of data
newdata <- data.frame(CPUE, SST_May)
predicted<-predict(model.m3, newdata, interval="prediction", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)
fit_value <- exp(predicted$fit)*exp(0.5*sigma*sigma) #adjustment for exp
lwr_pi <-  exp(predicted$lwr)*exp(0.5*sigma*sigma)
upr_pi <-  exp(predicted$upr)*exp(0.5*sigma*sigma) # https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr
fit_value
lwr_pi
upr_pi

