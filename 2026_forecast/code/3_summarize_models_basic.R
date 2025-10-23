# run code 2_diagnostics.R first
# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2025_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data for modeling
n <- dim(variables)[1] # number of years including forecast year
variables %>%
  mutate (odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),
          SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch)) -> log_data

# STEP #2: MODELS
model.names <- c(m1a='no temperature index included',
               m2a='ISTI20_JJ',
               m3a='Chatham_SST_May',
               m4a='Chatham_SST_MJJ',
               m5a='Chatham_SST_AMJ',
               m6a='Chatham_SST_AMJJ',
               m7a='Icy_Strait_SST_May',
               m8a='Icy_Strait_SST_MJJ',
               m9a='Icy_Strait_SST_AMJ',
               m10a='Icy_Strait_SST_AMJJ',
               m11a='NSEAK_SST_May',
               m12a='NSEAK_SST_MJJ',
               m13a='NSEAK_SST_AMJ',
               m14a='NSEAK_SST_AMJJ',
               m15a='SEAK_SST_May',
               m16a='SEAK_SST_MJJ',
               m17a='SEAK_SST_AMJ',
               m18a='SEAK_SST_AMJJ')
               
# model formulas (maintain the same order as above)
model.formulas <- c(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor),
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+ISTI20_JJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Chatham_SST_May,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Chatham_SST_MJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Chatham_SST_AMJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Chatham_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Icy_Strait_SST_May,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Icy_Strait_SST_MJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Icy_Strait_SST_AMJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+Icy_Strait_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+NSEAK_SST_May,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+NSEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+NSEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+NSEAK_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+SEAK_SST_May,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+SEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+SEAK_SST_AMJ,
                 SEAKCatch_log ~ CPUE + as.factor(odd_even_factor)+SEAK_SST_AMJJ)

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values, models = "")

# STEP #3: SUMMARY OF MODEL FITS
# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data %>%
  dplyr::filter(JYear < year.data) -> log_data_subset

lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor), data = log_data_subset) -> m1a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + ISTI20_JJ, data = log_data_subset) -> m2a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Chatham_SST_May, data = log_data_subset) -> m3a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data_subset) -> m4a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data_subset) -> m5a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data_subset) -> m6a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data_subset) -> m7a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data_subset) -> m11a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data_subset) -> m12a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data_subset) -> m13a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data_subset) -> m14a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + SEAK_SST_May, data = log_data_subset) -> m15a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data_subset) -> m16a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data_subset) -> m17a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data_subset) -> m18a

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
rbind(., model18a) -> models 

nyear <- 3
model <- c(rep('m1a',nyear),rep('m2a',nyear+1),rep('m3a',nyear+1),rep('m4a',nyear+1),
           rep('m5a',nyear+1),rep('m6a',nyear+1),rep('m7a',nyear+1),rep('m8a',nyear+1),
           rep('m9a',nyear+1),rep('m10a',nyear+1),rep('m11a',nyear+1),rep('m12a',nyear+1),
           rep('m13a',nyear+1),rep('m14a',nyear+1),rep('m15a',nyear+1),rep('m16a',nyear+1),
           rep('m17a',nyear+1),rep('m18a',nyear+1))
model<-as.data.frame(model)
cbind(models, model)%>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(Model = model,
         Term =term,
          Estimate = round(estimate,3),
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

# STEP #4: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2018, models = "")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)
#f_model_one_step_ahead_multiple10(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2013)  # start = 1997, end = 2011 means Jyear 2012-2021 used for MAPE calc. (10-year)
# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
# function is correct for the base CPUE model

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(Terms = 'X') %>%
  mutate(MAPE5 = round(MAPE5,3)) %>%
  dplyr::select(Terms, MAPE5) -> MAPE5

# format the file seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr # mase3<-dLagM::MASE(m5)
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(Terms = 'X') %>%
  dplyr::select(Terms, fit,	fit_LPI,	fit_UPI, AdjR2, sigma, AICc) %>%
  mutate(AdjR2 = round(AdjR2,2)) %>%
  mutate(Model = c('m1a','m2a','m3a','m4a','m5a','m6a','m7a','m8a',
                   'm9a','m10a','m11a','m12a','m13a','m14a','m15a','m16a','m17a',
                   'm18a')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(Fit = round(fit_log,1),
         Fit_LPI = round(fit_log_LPI,1),
         Fit_UPI = round(fit_log_UPI,1),
         AICc = round(AICc, 1)) %>%
  dplyr::select(Model, Terms, Fit, Fit_LPI, Fit_UPI, AdjR2, AICc) %>%
  merge(., MAPE5, by="Terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2.csv"), row.names = F)

# STEP #5: CREATE FORECAST FIGURE
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(Terms = 'X') %>%
  dplyr::select(Terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('m1a','m2a','m3a','m4a','m5a','m6a','m7a','m8a',
                   'm9a','m10a','m11a','m12a','m13a','m14a','m15a','m16a','m17a',
                   'm18a')) %>%
  mutate(order = c('1a','2a','3a','4a','5a','6a','7a','8a',
                   '9a','10a','11a','12a','13a','14a','15a','16a','17a',
                   '18a')) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, Terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
  ggplot(., aes(x=factor(model, level=c('m1a','m2a','m3a','m4a','m5a','m6a','m7a','m8a',
                                        'm9a','m10a','m11a','m12a','m13a','m14a','m15a','m16a','m17a',
                                        'm18a')), y=fit_log)) +
  geom_col(aes(y = fit_log, fill = "SEAK pink catch"), colour ="grey70",
           width = 1, position = position_dodge(width = 0.1)) +
  scale_colour_manual("", values=c("SEAK pink catch" = "grey90", "fit" = "black")) +
  scale_fill_manual("",values="lightgrey")+ geom_hline(yintercept=20, linetype='dashed', color=c('grey30'))+
  theme_bw() + theme(legend.key=element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_text(size = 9, family="Times New Roman"),
                     legend.title=element_blank(),
                     legend.position = "none") +
  geom_errorbar(mapping=aes(x=model, ymin=fit_log_UPI, ymax=fit_log_LPI), width=0.2, linewidth=1, color="grey30")+
  scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25, 30, 35, 40, 45), limits = c(0,45))+
  labs(x="", y = "2026 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "figs/forecast_models.png"), dpi = 500, height = 4, width = 10, units = "in")

# STEP 6: CREATE DATASET FOR WRITE-UP
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2025_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_temp # update file names
read.csv(file.path(data.directory,'adj_raw_pink.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_adj_raw_pink # update file names

variables_adj_raw_pink %>%
  mutate (adj_raw_pink_log = log(adj_raw_pink +1)) %>% # log CPUE variable
  group_by(JYear, Year, vessel) %>% 
  summarise(adj_raw_pink_log = max(adj_raw_pink_log)) %>% 
  merge(., variables_temp, by.x = c("JYear", "Year"), by.y = c("JYear", "Year")) %>%
  dplyr::filter(adj_raw_pink_log > 0) %>%
  #dplyr::filter(vessel!= 'Steller') %>%
  #dplyr::filter(vessel!= 'Chellissa') %>%
  dplyr::select(JYear, Year, SEAKCatch, vessel, adj_raw_pink_log) -> multi

variables_temp %>%
  mutate(odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even")) %>% # log catch variable
  merge(., multi, all.x =T)%>%
  dplyr::select(JYear, Year, SEAKCatch, odd_even_factor, vessel, adj_raw_pink_log, CPUE) %>%
  mutate(Harvest = round(SEAKCatch,1),
         Factor = odd_even_factor,
         adj_raw_pink_log = round(adj_raw_pink_log,2),
         CPUE = round(CPUE,2))%>% 
  dplyr::select(JYear, Year, Harvest, odd_even_factor, vessel, adj_raw_pink_log, CPUE) %>%
  write.csv(., paste0(data.directory, "/data.csv"), row.names = F)   


# STEP 7: CREATE PERFORMANCE SUMMARY
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(results.directory,'model_summary_table2.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE)  %>%
  arrange(MAPE5) %>%
  mutate(MAPE5 = round(MAPE5,1)) %>%
  mutate(change = AICc- min(AICc)) %>%
  dplyr::select(Terms, Model, Fit, Fit_LPI, Fit_UPI,AdjR2, MAPE5, change) %>%
  rename(AICc = change) %>%
  write.csv(., paste0(results.directory, "/model_summary_final.csv"), row.names = F) 

