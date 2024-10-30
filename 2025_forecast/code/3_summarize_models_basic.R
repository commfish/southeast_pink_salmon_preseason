# run code 2_diagnostics.R first
# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data for modeling
n <- dim(variables)[1] # number of years including forecast year
variables %>%
  mutate (SEAKCatch_log = log(SEAKCatch),
          odd_even_factor = ifelse(JYear %% 2 == 0, "even", "odd")) %>% # log catch variable
  dplyr::select(-c(SEAKCatch)) -> log_data

# STEP #2: MODELS
model.names <- c(m1a='CPUE*as.factor(odd_even_factor)',
               m2a='CPUE*as.factor(odd_even_factor) + ISTI20_MJJ',
               m3a='CPUE*as.factor(odd_even_factor) + Chatham_SST_May',
               m4a='CPUE*as.factor(odd_even_factor) + Chatham_SST_MJJ',
               m5a='CPUE*as.factor(odd_even_factor) + Chatham_SST_AMJ',
               m6a='CPUE*as.factor(odd_even_factor) + Chatham_SST_AMJJ',
               m7a='CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_May',
               m8a='CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_MJJ',
               m9a='CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_AMJ',
               m10a='CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ',
               m11a='CPUE*as.factor(odd_even_factor) + NSEAK_SST_May',
               m12a='CPUE*as.factor(odd_even_factor) + NSEAK_SST_MJJ',
               m13a='CPUE*as.factor(odd_even_factor) + NSEAK_SST_AMJ',
               m14a='CPUE*as.factor(odd_even_factor) + NSEAK_SST_AMJJ',
               m15a='CPUE*as.factor(odd_even_factor) + SEAK_SST_May',
               m16a='CPUE*as.factor(odd_even_factor) + SEAK_SST_MJJ',
               m17a='CPUE*as.factor(odd_even_factor) + SEAK_SST_AMJ',
               m18a='CPUE*as.factor(odd_even_factor) + SEAK_SST_AMJJ',
               m19a='CPUE:as.factor(odd_even_factor)',
               m20a='ISTI20_MJJ+ CPUE:as.factor(odd_even_factor)',
               m21a='Chatham_SST_May+ CPUE:as.factor(odd_even_factor)',
               m22a='Chatham_SST_MJJ+ CPUE:as.factor(odd_even_factor)',
               m23a='Chatham_SST_AMJ+ CPUE:as.factor(odd_even_factor)',
               m24a='Chatham_SST_AMJJ+ CPUE:as.factor(odd_even_factor)',
               m25a='Icy_Strait_SST_May+ CPUE:as.factor(odd_even_factor)',
               m26a='Icy_Strait_SST_MJJ+ CPUE:as.factor(odd_even_factor)',
               m27a='Icy_Strait_SST_AMJ+ CPUE:as.factor(odd_even_factor)',
               m28a='Icy_Strait_SST_AMJJ+ CPUE:as.factor(odd_even_factor)',
               m29a='NSEAK_SST_May+ CPUE:as.factor(odd_even_factor)',
               m30a='NSEAK_SST_MJJ+ CPUE:as.factor(odd_even_factor)',
               m31a='NSEAK_SST_AMJ+ CPUE:as.factor(odd_even_factor)',
               m32a='NSEAK_SST_AMJJ+ CPUE:as.factor(odd_even_factor)',
               m33a='SEAK_SST_May+ CPUE:as.factor(odd_even_factor)',
               m34a='SEAK_SST_MJJ+ CPUE:as.factor(odd_even_factor)',
               m35a='SEAK_SST_AMJ+ CPUE:as.factor(odd_even_factor)',
               m36a='SEAK_SST_AMJJ+ CPUE:as.factor(odd_even_factor)')
# model formulas (maintain the same order as above)
model.formulas <- c(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor),
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + ISTI20_MJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_May,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_MJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_AMJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_AMJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_May,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_MJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_AMJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_May,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_AMJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_May,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_AMJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor),
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + ISTI20_MJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_May,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_MJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_AMJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_AMJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_May,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_MJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_AMJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_May,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_AMJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_May,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_AMJJ) # temp. data

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values)
# if there is an error about MASE, run the MASE part of the function file and then rerun line 204

# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data %>%
  dplyr::filter(JYear < year.data) -> log_data_subset

lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor), data = log_data_subset) -> m1a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + ISTI20_MJJ, data = log_data_subset) -> m2a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_May, data = log_data_subset) -> m3a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data_subset) -> m4a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data_subset) -> m5a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data_subset) -> m6a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data_subset) -> m7a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data_subset) -> m11a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data_subset) -> m12a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data_subset) -> m13a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data_subset) -> m14a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_May, data = log_data_subset) -> m15a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data_subset) -> m16a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data_subset) -> m17a
lm(SEAKCatch_log ~ CPUE*as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data_subset) -> m18a

lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor), data = log_data_subset) -> m19a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + ISTI20_MJJ, data = log_data_subset) -> m20a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_May, data = log_data_subset) -> m21a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data_subset) -> m22a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data_subset) -> m23a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data_subset) -> m24a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data_subset) -> m25a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data_subset) -> m26a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data_subset) -> m27a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m28a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data_subset) -> m29a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data_subset) -> m30a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data_subset) -> m31a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data_subset) -> m32a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_May, data = log_data_subset) -> m33a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data_subset) -> m34a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data_subset) -> m35a
lm(SEAKCatch_log ~ CPUE:as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data_subset) -> m36a

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

tidy(m19a) -> model19a
tidy(m20a) -> model20a
tidy(m21a) -> model21a
tidy(m22a) -> model22a
tidy(m23a) -> model23a
tidy(m24a) -> model24a
tidy(m25a) -> model25a
tidy(m26a) -> model26a
tidy(m27a) -> model27a
tidy(m28a) -> model28a
tidy(m29a) -> model29a
tidy(m30a) -> model30a
tidy(m31a) -> model31a
tidy(m32a) -> model32a
tidy(m33a) -> model33a
tidy(m34a) -> model34a
tidy(m35a) -> model35a
tidy(m36a) -> model36a

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
  

rbind(., model19a) %>%
rbind(., model20a) %>%
rbind(., model21a) %>%
rbind(., model22a) %>%
rbind(., model23a) %>%
rbind(., model24a) %>%
rbind(., model25a) %>%
rbind(., model26a) %>%
rbind(., model27a) %>%
rbind(., model28a) %>%
rbind(., model29a) %>%
rbind(., model30a) %>%
rbind(., model31a) %>%
rbind(., model32a) %>%
rbind(., model33a) %>%
rbind(., model34a) %>%
rbind(., model35a) %>%
rbind(., model36a) -> models 

nyear <- 4
model <- c(rep('m1a',nyear),rep('m2a',nyear+1),rep('m3a',nyear+1),rep('m4a',nyear+1),
           rep('m5a',nyear+1),rep('m6a',nyear+1),rep('m7a',nyear+1),rep('m8a',nyear+1),
           rep('m9a',nyear+1),rep('m10a',nyear+1),rep('m11a',nyear+1),rep('m12a',nyear+1),
           rep('m13a',nyear+1),rep('m14a',nyear+1),rep('m15a',nyear+1),rep('m16a',nyear+1),
           rep('m17a',nyear+1),rep('m18a',nyear+1),rep('m19a',nyear-1),rep('m20a',nyear),
           rep('m21a',nyear),rep('m22a',nyear),rep('m23a',nyear),rep('m24a',nyear),
           rep('m25a',nyear),rep('m26a',nyear),rep('m27a',nyear),rep('m28a',nyear),
           rep('m29a',nyear),rep('m30a',nyear),rep('m31a',nyear),rep('m32a',nyear),
           rep('m33a',nyear),rep('m34a',nyear),rep('m35a',nyear),rep('m36a',nyear))
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
                   'm18a','m19a','m20a'	,'m21a'	,'m22a'	,'m23a'	,'m24a'	,'m25a'	,
                   'm26a'	,'m27a'	,'m28a'	,'m29a'	,'m30a'	,'m31a'	,'m32a'	,'m33a'	,
                   'm34a'	,'m35a'	, 'm36a')) %>%
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

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('m1a','m2a','m3a','m4a','m5a','m6a','m7a','m8a',
                   'm9a','m10a','m11a','m12a','m13a','m14a','m15a','m16a','m17a',
                   'm18a','m19a','m20a'	,'m21a'	,'m22a'	,'m23a'	,'m24a'	,'m25a'	,
                   'm26a'	,'m27a'	,'m28a'	,'m29a'	,'m30a'	,'m31a'	,'m32a'	,'m33a'	,
                   'm34a'	,'m35a'	, 'm36a')) %>%
  mutate(order = c('1a','2a','3a','4a','5a','6a','7a','8a',
                   '9a','10a','11a','12a','13a','14a','15a','16a','17a',
                   '18a','19a','20a'	,'21a'	,'22a'	,'23a'	,'24a'	,'25a'	,
                   '26a'	,'27a'	,'28a'	,'29a'	,'30a'	,'31a'	,'32a'	,'33a'	,
                   '34a'	,'35a'	, '36a')) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
  ggplot(., aes(x=factor(model, level=c('m1a','m2a','m3a','m4a','m5a','m6a','m7a','m8a',
                                        'm9a','m10a','m11a','m12a','m13a','m14a','m15a','m16a','m17a',
                                        'm18a','m19a','m20a'	,'m21a'	,'m22a'	,'m23a'	,'m24a'	,'m25a'	,
                                        'm26a'	,'m27a'	,'m28a'	,'m29a'	,'m30a'	,'m31a'	,'m32a'	,'m33a'	,
                                        'm34a'	,'m35a'	, 'm36a')), y=fit_log)) +
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
  scale_y_continuous(breaks = c(0,10, 20, 30, 40, 50, 60, 70, 80, 100), limits = c(0,100))+
  labs(x = "Models", y = "2025 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "figs/forecast_models.png"), dpi = 500, height = 4, width = 10, units = "in")

