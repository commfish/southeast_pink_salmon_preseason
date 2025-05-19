# run code 6_diagnostics_models_basic_by_odd_only.R first
# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data for modeling
variables %>%
  mutate (odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),
          SEAKCatch_log = log(SEAKCatch)) %>%
  dplyr::select(-c(SEAKCatch))-> log_data_odd_even
variables<-log_data_odd_even
n <- dim(variables)[1] # number of years including forecast year


# STEP #2: MODELS
model.names <- c(m1c='CPUE +odd_even_factor',
               m2c='CPUE + ISTI20_MJJ+odd_even_factor',
               m3c='CPUE + Chatham_SST_May+odd_even_factor',
               m4c='CPUE + Chatham_SST_MJJ+odd_even_factor',
               m5c='CPUE + Chatham_SST_AMJ+odd_even_factor',
               m6c='CPUE + Chatham_SST_AMJJ+odd_even_factor',
               m7c='CPUE + Icy_Strait_SST_May+odd_even_factor',
               m8c='CPUE + Icy_Strait_SST_MJJ+odd_even_factor',
               m9c='CPUE + Icy_Strait_SST_AMJ+odd_even_factor',
               m10c='CPUE + Icy_Strait_SST_AMJJ+odd_even_factor',
               m11c='CPUE + NSEAK_SST_May+odd_even_factor',
               m12c='CPUE + NSEAK_SST_MJJ+odd_even_factor',
               m13c='CPUE + NSEAK_SST_AMJ+odd_even_factor',
               m14c='CPUE + NSEAK_SST_AMJJ+odd_even_factor',
               m15c='CPUE + SEAK_SST_May+odd_even_factor',
               m16c='CPUE + SEAK_SST_MJJ+odd_even_factor',
               m17c='CPUE + SEAK_SST_AMJ+odd_even_factor',
               m18c='CPUE + SEAK_SST_AMJJ+odd_even_factor')
# model formulas (maintain the same order as above)
model.formulas <- c(SEAKCatch_log ~ CPUE,
                 SEAKCatch_log ~ CPUE + ISTI20_MJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Chatham_SST_May+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Chatham_SST_MJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Chatham_SST_AMJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Chatham_SST_AMJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_May+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_May+odd_even_factor,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + SEAK_SST_May+odd_even_factor,
                 SEAKCatch_log ~ CPUE + SEAK_SST_MJJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + SEAK_SST_AMJ+odd_even_factor,
                 SEAKCatch_log ~ CPUE + SEAK_SST_AMJJ+odd_even_factor) # temp. data

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary_odd(harvest=log_data_odd_even$SEAKCatch_log, variables=log_data_odd_even, model.formulas=model.formulas,model.names=model.names, w = log_data_odd_even$weight_values, models = "_odd_even")

# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data_odd_even %>%
  dplyr::filter(JYear < year.data) -> log_data_odd_even_subset

lm(SEAKCatch_log ~ CPUE+odd_even_factor, data = log_data_odd_even_subset) -> m1c
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ+odd_even_factor, data = log_data_odd_even_subset) -> m2c
lm(SEAKCatch_log ~ CPUE + Chatham_SST_May+odd_even_factor, data = log_data_odd_even_subset) -> m3c
lm(SEAKCatch_log ~ CPUE + Chatham_SST_MJJ+odd_even_factor, data = log_data_odd_even_subset) -> m4c
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJ+odd_even_factor, data = log_data_odd_even_subset) -> m5c
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJJ+odd_even_factor, data = log_data_odd_even_subset) -> m6c
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May+odd_even_factor, data = log_data_odd_even_subset) -> m7c
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ+odd_even_factor, data = log_data_odd_even_subset) -> m8c
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJ+odd_even_factor, data = log_data_odd_even_subset) -> m9c
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ+odd_even_factor, data = log_data_odd_even_subset) -> m10c
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May+odd_even_factor, data = log_data_odd_even_subset) -> m11c
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ+odd_even_factor, data = log_data_odd_even_subset) -> m12c
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ+odd_even_factor, data = log_data_odd_even_subset) -> m13c
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ+odd_even_factor, data = log_data_odd_even_subset) -> m14c
lm(SEAKCatch_log ~ CPUE + SEAK_SST_May+odd_even_factor, data = log_data_odd_even_subset) -> m15c
lm(SEAKCatch_log ~ CPUE + SEAK_SST_MJJ+odd_even_factor, data = log_data_odd_even_subset) -> m16c
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJ+odd_even_factor, data = log_data_odd_even_subset) -> m17c
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJJ+odd_even_factor, data = log_data_odd_even_subset) -> m18c

tidy(m1c) -> model1c
tidy(m2c) -> model2c
tidy(m3c) -> model3c
tidy(m4c) -> model4c
tidy(m5c) -> model5c
tidy(m6c) -> model6c
tidy(m7c) -> model7c
tidy(m8c) -> model8c
tidy(m9c) -> model9c
tidy(m10c) -> model10c
tidy(m11c) -> model11c
tidy(m12c) -> model12c
tidy(m13c) -> model13c
tidy(m14c) -> model14c
tidy(m15c) -> model15c
tidy(m16c) -> model16c
tidy(m17c) -> model17c
tidy(m18c) -> model18c

rbind(model1c, model2c) %>%
rbind(., model3c) %>%
rbind(., model4c) %>%
rbind(., model5c) %>%
rbind(., model6c) %>%
rbind(., model7c) %>%
rbind(., model8c) %>%
rbind(., model9c) %>%
rbind(., model10c) %>%
rbind(., model11c) %>%
rbind(., model12c) %>%
rbind(., model13c) %>%
rbind(., model14c) %>%
rbind(., model15c) %>%
rbind(., model16c) %>%
rbind(., model17c) %>%
rbind(., model18c) -> models 

nyear <- 3
model <- c(rep('m1c',nyear),rep('m2c',nyear+1),rep('m3c',nyear+1),rep('m4c',nyear+1),
           rep('m5c',nyear+1),rep('m6c',nyear+1),rep('m7c',nyear+1),rep('m8c',nyear+1),
           rep('m9c',nyear+1),rep('m10c',nyear+1),rep('m11c',nyear+1),rep('m12c',nyear+1),
           rep('m13c',nyear+1),rep('m14c',nyear+1),rep('m15c',nyear+1),rep('m16c',nyear+1),
           rep('m17c',nyear+1),rep('m18c',nyear+1))
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
write.csv(., paste0(results.directory, "/model_summary_table1_odd_even.csv"), row.names = F) # detailed model summaries

# calculate one step ahead MAPE
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
# forecasted in the one step ahead process)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html

# STEP #3: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data_odd_even$SEAKCatch_log, variables=log_data_odd_even, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2018, models = "_odd_even")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)
# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
# function is correct for the base CPUE model

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5_odd_even.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  mutate(MAPE5 = round(MAPE5,3)) %>%
  dplyr::select(terms, MAPE5) -> MAPE5

# format the file seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr # mase3<-dLagM::MASE(m5)
read.csv(file.path(results.directory,'seak_model_summary_odd_even.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, sigma, MAPE) %>%
  mutate(AdjR2 = round(AdjR2,3)) %>%
  mutate(Model = c('m1c','m2c','m3c','m4c','m5c','m6c','m7c','m8c',
                   'm9c','m10c','m11c','m12c','m13c','m14c','m15c','m16c','m17c',
                   'm18c')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(Fit = round(fit_log,3),
         Fit_LPI = round(fit_log_LPI,3),
         Fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE) %>%
  merge(., MAPE5, by="terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2_odd_even.csv"), row.names = F)

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary_odd_even.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('m1c','m2c','m3c','m4c','m5c','m6c','m7c','m8c',
                   'm9c','m10c','m11c','m12c','m13c','m14c','m15c','m16c','m17c',
                   'm18c')) %>%
  mutate(order = c('1c','2c','3c','4c','5c','6c','7c','8c',
                   '9c','10c','11c','12c','13c','14c','15c','16c','17c',
                   '18c')) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
  ggplot(., aes(x=factor(model, level=c('m1c','m2c','m3c','m4c','m5c','m6c','m7c','m8c',
                                        'm9c','m10c','m11c','m12c','m13c','m14c','m15c','m16c','m17c',
                                        'm18c')), y=fit_log)) +
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
ggsave(paste0(results.directory, "figs/forecast_models_odd_even.png"), dpi = 500, height = 4, width = 10, units = "in")

