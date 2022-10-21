# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/10/2022
# pink_cal_pooled_species
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
# update all data files varyyyy_final.csv

# load libraries
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(gam)
library(MASS)
library(MuMIn)
library(AICcmodavg)
#library(forecast)
library(broom)
#library(caret)
library(rpart)
library(mda)
library(tidyverse)
library(dLagM) # MASE calc
library(ggplot2)
#library(car)
library(ggfortify)
library(Hmisc)
library(dplyr)
library(extrafont)
library(ggrepel)
library(Metrics) # MASE calc
library(MetricsWeighted)
#extrafont::font_import()
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2023_forecast"
year.data <- 2022
year.data.one <- year.data - 1
sample_size <-  25 # number of data points in model (this is used for Cook's distance)
forecast2022 <- 15.6 # input last year's forecast for the forecast plot
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')
source('2023_forecast/code/functions.r')

# STEP 1: DATA
# read in data (make sure this is up to date)
read.csv(file.path(data.directory,'var2022_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data
variables$CPUE <- variables$CPUEcal # Use CPUEcal as CPUE index
n <- dim(variables)[1] # number of years including forecast year
variables %>%
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch,	CPUEcal)) -> log_data

# STEP #2: HARVEST MODELS AND SUMMARY STATS
# define model names and formulas
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

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values)

# summary of model fits (i.e., coefficients, p-value)
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
  mutate(estimate = round(estimate,8),
         std.error = round(std.error,3),
         statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
write.csv(., paste0(results.directory, "/model_summary_table1.csv"), row.names = F)
# mase3<-dLagM::MASE(m5)
# format seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, AICc, BIC,	p.value,	sigma,	MAPE,	MAPE_LOOCV,	MASE, wMAPE) %>%
  mutate(AdjR2 = round(AdjR2,3),
         sigma = round(sigma,3),
         AICc = round(AICc,2),
         MAPE = round(MAPE,3),
         MASE = round(MASE,3),
         MAPE_LOOCV = round(MAPE_LOOCV,3),
         wMAPE = round(wMAPE, 3)) %>%
  mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                   'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                   'm18')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(fit = round(fit_log,3),
         fit_LPI = round(fit_log_LPI,3),
         fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(model, terms, AdjR2, AICc,MASE ,wMAPE, MAPE_LOOCV) %>%
  write.csv(paste0(results.directory, "/model_summary_table2.csv"), row.names = F)

read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, AICc, BIC,	p.value,	sigma,	MAPE,	MAPE_LOOCV,	MASE, wMAPE) %>%
  mutate(AdjR2 = round(AdjR2,3),
         sigma = round(sigma,3),
         AICc = round(AICc,2),
         MAPE = round(MAPE,3),
         MASE = round(MASE,3),
         MAPE_LOOCV = round(MAPE_LOOCV,3),
         wMAPE = round(wMAPE, 3)) %>%
  mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                   'm9','m10','m11','m12','m13','m14','m15','m16',' m17',
                   'm18')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(fit = round(fit_log,3),
         fit_LPI = round(fit_log_LPI,3),
         fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(model, terms, fit,	fit_LPI, fit_UPI) %>%
  write.csv(paste0(results.directory, "/model_summary_table3.csv"), row.names = F)

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
 results %>%
   dplyr::rename(terms = 'X') %>%
   dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
   mutate(model = c('1','2','3','4','5','6','7','8',
                    '9','10','11','12','13','14','15','16','17',
                    '18')) %>%
   mutate(model= as.numeric(model),
          fit_log = exp(fit)*exp(0.5*sigma*sigma),
          fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
          fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
   dplyr::select(model, terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
   as.data.frame() %>%
   ggplot(., aes(x=model)) +
   geom_bar(aes(y = fit_log, fill = "SEAK pink catch"),
            stat = "identity", colour ="black",
            width = 1, position = position_dodge(width = 0.1)) +
   scale_colour_manual("", values=c("SEAK pink catch" = "lightgrey", "fit" = "black")) +
   scale_fill_manual("",values="lightgrey")+
   theme_bw() + theme(legend.key=element_blank(),
                      legend.title=element_blank(),
                      legend.position = "none") +
   geom_errorbar(mapping=aes(x=model, ymin=fit_log_UPI, ymax=fit_log_LPI), width=0.2, size=1, color="blue")+
   scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40), limits = c(0,40))+
   scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), limits = c(0,19))+
   labs(x = "Models", y = "2023 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
 ggsave(paste0(results.directory, "figs/forecast_models.png"), dpi = 500, height = 4, width = 6, units = "in")

# one step ahead MAPE
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
# forecasted in the one step ahead process)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html
# f_model_one_step_ahead_multiple_inv_var(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2016)  # num should be final year of data - end (e.g. 2021-2015) years
f_model_one_step_ahead_multiple(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2016)  # num should be final year of data - end (e.g. 2021-2015) years
# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead fucntion check.xlsx to make sure the
# function is correct for the basec CPUE model

read.csv(file.path(results.directory,'model_summary_table2.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> model_summary_table2
read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  mutate(MAPE_one_step_ahead = round(MAPE,3)) %>%
  cbind(., results) %>%
  dplyr::select(MAPE_one_step_ahead) %>%
  cbind(., model_summary_table2) %>%
  dplyr::select(model, AdjR2,  AICc,  MASE, wMAPE, MAPE_LOOCV, MAPE_one_step_ahead) %>%
  write.csv(paste0(results.directory, "/model_summary_table4.csv"), row.names = F)

# run function f_model_one_step_ahead for each model
# these return results_model.csv files for the one step ahead forecast
# comment out the "return(data)" of the function in the functions.R file if you want the one-step-ahead-MAPE
# these results are used in the model_summary_table_month_year.xlsx file
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE, start = 1997, end = 2011, model_num = "m1")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + ISTI20_MJJ, start = 1997, end = 2011, model_num = "m2")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_May, start = 1997, end = 2011, model_num = "m3")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_MJJ, start = 1997, end = 2011, model_num = "m4")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_AMJ, start = 1997, end = 2011, model_num = "m5")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_AMJJ, start = 1997, end = 2011, model_num = "m6")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_May, start = 1997, end = 2011, model_num = "m7")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_MJJ, start = 1997, end = 2011, model_num = "m8")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_AMJ, start = 1997, end = 2011, model_num = "m9")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_AMJJ, start = 1997, end = 2011, model_num = "m10")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_May, start = 1997, end = 2011, model_num = "m11")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_MJJ, start = 1997, end = 2011, model_num = "m12")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_AMJ, start = 1997, end = 2011, model_num = "m13")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_AMJJ, start = 1997, end = 2011, model_num = "m14")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_May, start = 1997, end = 2011, model_num = "m15")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_MJJ, start = 1997, end = 2011, model_num = "m16")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_AMJ, start = 1997, end = 2011, model_num = "m17")
seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_AMJJ, start = 1997, end = 2011, model_num = "m18")
# the results of the models need to be manually entered into the model_summary_table_month_year.xlsx sheet
# # test of predict results
 model.m11 = lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset)
 best.model <- m11 # this can be added after steps 1 and 2 after the best model is determined
 last_year_data_cpue <- 1.45
 sigma<- sigma(best.model) # best model
 CPUE <- last_year_data_cpue # last year of data
 NSEAK_SST_May <- 7.62
 # last year of data
 newdata <- data.frame(CPUE)
 preds<-predict(model.m11, newdata, interval="prediction", level = 0.80, se.fit=T)
 preds
 
 z <- predict(model.m11, newdata, se.fit = TRUE)
 alpha <- 0.80  ## 90%
 Qt <- c(-1, 1) * qt((1 - alpha) / 2, z$df, lower.tail = FALSE)
 CI <- z$fit + outer(z$se.fit, Qt)
 colnames(CI) <- c("lwr", "upr")
 CI

# restructure the data (for write-up)
 variables %>%
   mutate(Harvest = round(SEAKCatch, 2)) %>%
   filter(Year<=year.data)%>%
   dplyr::select(c(Year, Harvest)) %>%
   write.csv(., paste0(results.directory, "/data_used_a.csv"), row.names = F)
 
 variables %>%
   mutate('Juvenile year' = Year-1,
          CPUE = round(CPUEcal, 2),
          NSEAK_SST_May = round(NSEAK_SST_May,2),
          ISTI20_MJJ = round(ISTI20_MJJ,2)) %>%
   dplyr::select(c('Juvenile year', CPUE, NSEAK_SST_May, ISTI20_MJJ)) %>%
   write.csv(., paste0(results.directory, "/data_used_b.csv"), row.names = F)
 