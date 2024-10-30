# run code 2_diagnostics.R first
# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data for modeling
n <- dim(variables)[1] # number of years including forecast year
variables %>%
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch)) -> log_data

# restructure the data (for write-up)
# variables %>%
#   mutate(Harvest = round(SEAKCatch, 2),
#          'Juvenile year' = Year-1,
#          CPUE = round(CPUE, 2)) %>%
#   dplyr::select(c(Year, 'Juvenile year', Harvest, CPUE)) %>%
#   write.csv(., paste0(results.directory, "/data_used_harvest.csv"), row.names = F)

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
model.names <- c(m1a='CPUE',
               m2a='CPUE + ISTI20_MJJ',
               m3a='CPUE + Chatham_SST_May',
               m4a='CPUE + Chatham_SST_MJJ',
               m5a='CPUE + Chatham_SST_AMJ',
               m6a='CPUE + Chatham_SST_AMJJ',
               m7a='CPUE + Icy_Strait_SST_May',
               m8a='CPUE + Icy_Strait_SST_MJJ',
               m9a='CPUE + Icy_Strait_SST_AMJ',
               m10a='CPUE + Icy_Strait_SST_AMJJ',
               m11a='CPUE + NSEAK_SST_May',
               m12a='CPUE + NSEAK_SST_MJJ',
               m13a='CPUE + NSEAK_SST_AMJ',
               m14a='CPUE + NSEAK_SST_AMJJ',
               m15a='CPUE + SEAK_SST_May',
               m16a='CPUE + SEAK_SST_MJJ',
               m17a='CPUE + SEAK_SST_AMJ',
               m18a='CPUE + SEAK_SST_AMJJ')
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

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1a
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_subset) -> m2a
lm(SEAKCatch_log ~ CPUE + Chatham_SST_May, data = log_data_subset) -> m3a
lm(SEAKCatch_log ~ CPUE + Chatham_SST_MJJ, data = log_data_subset) -> m4a
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJ, data = log_data_subset) -> m5a
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJJ, data = log_data_subset) -> m6a
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset) -> m7a
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8a
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9a
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10a
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m11a
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ, data = log_data_subset) -> m12a
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ, data = log_data_subset) -> m13a
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset) -> m14a
lm(SEAKCatch_log ~ CPUE + SEAK_SST_May, data = log_data_subset) -> m15a
lm(SEAKCatch_log ~ CPUE + SEAK_SST_MJJ, data = log_data_subset) -> m16a
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJ, data = log_data_subset) -> m17a
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJJ, data = log_data_subset) -> m18a

tidy(m1a) -> model1a
tidy(m2a) -> model2a
tidy(m3a) -> model3a
tidy(m4a) -> model4a
tidy(m5a) -> model5a
tidy(m6a) -> model6a
tidy(m7a) -> model7a
tidy(m8a) -> model8a
tidy(m9a) -> model9a
tidy(m10a) -> model10a
tidy(m11a) -> model11a
tidy(m12a) -> model12a
tidy(m13a) -> model13a
tidy(m14a) -> model14a
tidy(m15a) -> model15a
tidy(m16a) -> model16a
tidy(m17a) -> model17a
tidy(m18a) -> model18a

rbind(model1a, model2a) %>%
rbind(., model3a) %>%
rbind(., model4a) %>%
rbind(., model5a) %>%
rbind(., model6a) %>%
rbind(., model7a) %>%
rbind(., model8a) %>%
rbind(., model9a) %>%
rbind(., model10a) %>%
rbind(., model11a) %>%
rbind(., model12a) %>%
rbind(., model13a) %>%
rbind(., model14a) %>%
rbind(., model15a) %>%
rbind(., model16a) %>%
rbind(., model17a) %>%
rbind(., model18a) %>%

mutate(model = c('m1a','m1a','m2a','m2a','m2a','m3a','m3a','m3a',
                 'm4a','m4a','m4a','m5a','m5a','m5a','m6a','m6a',' m6a',
                 'm7a','m7a','m7a','m8a','m8a','m8a','m9a','m9a',' m9a',
                 'm10a','m10a','m10a','m11a','m11a','m11a','m12a','m12a',' m12a',
                 'm13a','m13a','m13a','m14a','m14a','m14a','m15a','m15a',' m15a',
                 'm16a','m16a','m16a','m17a','m17a','m17a','m18a','m18a',' m18a')) %>%
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
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, sigma, MAPE) %>%
  mutate(AdjR2 = round(AdjR2,3)) %>%
  mutate(Model = c('m1a','m2a','m3a','m4a','m5a','m6a','m7a','m8a',
                   'm9a','m10a','m11a','m12a','m13a','m14a','m15a','m16a','m17a',
                   'm18a')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(Fit = round(fit_log,3),
         Fit_LPI = round(fit_log_LPI,3),
         Fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE) %>%
  merge(., MAPE5, by="terms") %>%
  merge(., MAPE10, by="terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2.csv"), row.names = F)

# run function f_model_one_step_ahead for each model;
# these return results_modelxx.csv files for the one step ahead forecast for each model;
# comment out the "return(data)" of the function in the functions.R file if you want the one-step-ahead-MAPE;
# these results are used in the model_summary_table_month_year.xlsx file; they need to be input by hand into the excel sheet 
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE, start = 1997, end = 2013, model_num = "m1") # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc.(5-year MAPE)
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + ISTI20_MJJ, start = 1997, end = 2013, model_num = "m2")# start = 1997, end = 2011 means Jyear 2012-2021 used for MAPE calc. (10-year MAPE)
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_May, start = 1997, end = 2013, model_num = "m3")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_MJJ, start = 1997, end = 2013, model_num = "m4")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_AMJ, start = 1997, end = 2013, model_num = "m5")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Chatham_SST_AMJJ, start = 1997, end = 2013, model_num = "m6")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_May, start = 1997, end = 2013, model_num = "m7")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_MJJ, start = 1997, end = 2013, model_num = "m8")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_AMJ, start = 1997, end = 2013, model_num = "m9")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + Icy_Strait_SST_AMJJ, start = 1997, end = 2013, model_num = "m10")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_May, start = 1997, end = 2013, model_num = "m11")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_MJJ, start = 1997, end = 2013, model_num = "m12")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_AMJ, start = 1997, end = 2013, model_num = "m13")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + NSEAK_SST_AMJJ, start = 1997, end = 2013, model_num = "m14")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_May, start = 1997, end = 2013, model_num = "m15")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_MJJ, start = 1997, end = 2013, model_num = "m16")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_AMJ, start = 1997, end = 2013, model_num = "m17")
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE + SEAK_SST_AMJJ, start = 1997, end = 2013, model_num = "m18")
# the results of the models need to be manually entered into the model_summary_table_month_year.xlsx sheet in the results/summary_tables folder
# summary tables 3a, 3b, and 3c are made from the automated tables

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('1a','2a','3a','4a','5a','6a','7a','8a',
                   '9a','10a','11a','12a','13a','14a','15a','16a','17a',
                   '18a')) %>%
  mutate(order = c('1a','2a','3a','4a','5a','6a','7a','8a',
                   '9a','10a','11a','12a','13a','14a','15a','16a','17a',
                   '18a')) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
  ggplot(., aes(x=factor(model, level=c('1a','2a','3a','4a','5a','6a','7a','8a',
                                        '9a','10a','11a','12a','13a','14a','15a','16a','17a',
                                        '18a')), y=fit_log)) +
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
  scale_y_continuous(breaks = c(0,10, 20, 30, 40, 50, 60, 70, 80), limits = c(0,80))+
  labs(x = "Models", y = "2025 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "figs/forecast_models.png"), dpi = 500, height = 4, width = 7, units = "in")

