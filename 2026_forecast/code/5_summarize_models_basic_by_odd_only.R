# run code 4_diagnostics_models_basic.R first
# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data for modeling
variables %>%
  mutate (odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),
          SEAKCatch_log = log(SEAKCatch)) %>%
  dplyr::filter(odd_even_factor == "odd") %>%
  dplyr::select(-c(SEAKCatch))%>%   
  mutate(year_num = 1:14)-> log_data_odd
variables<-log_data_odd
n <- dim(variables)[1] # number of years including forecast year


# STEP #2: MODELS
model.names <- c(m1b='CPUE',
               m2b='CPUE + ISTI20_MJJ',
               m3b='CPUE + Chatham_SST_May',
               m4b='CPUE + Chatham_SST_MJJ',
               m5b='CPUE + Chatham_SST_AMJ',
               m6b='CPUE + Chatham_SST_AMJJ',
               m7b='CPUE + Icy_Strait_SST_May',
               m8b='CPUE + Icy_Strait_SST_MJJ',
               m9b='CPUE + Icy_Strait_SST_AMJ',
               m10b='CPUE + Icy_Strait_SST_AMJJ',
               m11b='CPUE + NSEAK_SST_May',
               m12b='CPUE + NSEAK_SST_MJJ',
               m13b='CPUE + NSEAK_SST_AMJ',
               m14b='CPUE + NSEAK_SST_AMJJ',
               m15b='CPUE + SEAK_SST_May',
               m16b='CPUE + SEAK_SST_MJJ',
               m17b='CPUE + SEAK_SST_AMJ',
               m18b='CPUE + SEAK_SST_AMJJ')
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
seak_model_summary <- f_model_summary_odd(harvest=log_data_odd$SEAKCatch_log, variables=log_data_odd, model.formulas=model.formulas,model.names=model.names, w = log_data_odd$weight_values, models = "_odd")

# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data_odd %>%
  dplyr::filter(JYear < year.data) -> log_data_odd_subset

lm(SEAKCatch_log ~ CPUE, data = log_data_odd_subset) -> m1b
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_odd_subset) -> m2b
lm(SEAKCatch_log ~ CPUE + Chatham_SST_May, data = log_data_odd_subset) -> m3b
lm(SEAKCatch_log ~ CPUE + Chatham_SST_MJJ, data = log_data_odd_subset) -> m4b
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJ, data = log_data_odd_subset) -> m5b
lm(SEAKCatch_log ~ CPUE + Chatham_SST_AMJJ, data = log_data_odd_subset) -> m6b
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_odd_subset) -> m7b
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ, data = log_data_odd_subset) -> m8b
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJ, data = log_data_odd_subset) -> m9b
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ, data = log_data_odd_subset) -> m10b
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_odd_subset) -> m11b
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ, data = log_data_odd_subset) -> m12b
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ, data = log_data_odd_subset) -> m13b
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_odd_subset) -> m14b
lm(SEAKCatch_log ~ CPUE + SEAK_SST_May, data = log_data_odd_subset) -> m15b
lm(SEAKCatch_log ~ CPUE + SEAK_SST_MJJ, data = log_data_odd_subset) -> m16b
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJ, data = log_data_odd_subset) -> m17b
lm(SEAKCatch_log ~ CPUE + SEAK_SST_AMJJ, data = log_data_odd_subset) -> m18b

tidy(m1b) -> model1b
tidy(m2b) -> model2b
tidy(m3b) -> model3b
tidy(m4b) -> model4b
tidy(m5b) -> model5b
tidy(m6b) -> model6b
tidy(m7b) -> model7b
tidy(m8b) -> model8b
tidy(m9b) -> model9b
tidy(m10b) -> model10b
tidy(m11b) -> model11b
tidy(m12b) -> model12b
tidy(m13b) -> model13b
tidy(m14b) -> model14b
tidy(m15b) -> model15b
tidy(m16b) -> model16b
tidy(m17b) -> model17b
tidy(m18b) -> model18b

rbind(model1b, model2b) %>%
rbind(., model3b) %>%
rbind(., model4b) %>%
rbind(., model5b) %>%
rbind(., model6b) %>%
rbind(., model7b) %>%
rbind(., model8b) %>%
rbind(., model9b) %>%
rbind(., model10b) %>%
rbind(., model11b) %>%
rbind(., model12b) %>%
rbind(., model13b) %>%
rbind(., model14b) %>%
rbind(., model15b) %>%
rbind(., model16b) %>%
rbind(., model17b) %>%
rbind(., model18b) -> models 

nyear <- 2
model <- c(rep('m1b',nyear),rep('m2b',nyear+1),rep('m3b',nyear+1),rep('m4b',nyear+1),
           rep('m5b',nyear+1),rep('m6b',nyear+1),rep('m7b',nyear+1),rep('m8b',nyear+1),
           rep('m9b',nyear+1),rep('m10b',nyear+1),rep('m11b',nyear+1),rep('m12b',nyear+1),
           rep('m13b',nyear+1),rep('m14b',nyear+1),rep('m15b',nyear+1),rep('m16b',nyear+1),
           rep('m17b',nyear+1),rep('m18b',nyear+1))
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
write.csv(., paste0(results.directory, "/model_summary_table1_odd.csv"), row.names = F) # detailed model summaries

# calculate one step ahead MAPE
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
# forecasted in the one step ahead process)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html

# STEP #3: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5_odd(harvest=log_data_odd$SEAKCatch_log, variables=log_data_odd, model.formulas=model.formulas,model.names=model.names, start = 1, end = 8, models = "_odd")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)
# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
# function is correct for the base CPUE model

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5_odd.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  mutate(MAPE5 = round(MAPE5,3)) %>%
  dplyr::select(terms, MAPE5) -> MAPE5

# format the file seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr # mase3<-dLagM::MASE(m5)
read.csv(file.path(results.directory,'seak_model_summary_odd.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, AdjR2, sigma, MAPE) %>%
  mutate(AdjR2 = round(AdjR2,3)) %>%
  mutate(Model = c('m1b','m2b','m3b','m4b','m5b','m6b','m7b','m8b',
                   'm9b','m10b','m11b','m12b','m13b','m14b','m15b','m16b','m17b',
                   'm18b')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(Fit = round(fit_log,3),
         Fit_LPI = round(fit_log_LPI,3),
         Fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE) %>%
  merge(., MAPE5, by="terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2_odd.csv"), row.names = F)

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary_odd.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('m1b','m2b','m3b','m4b','m5b','m6b','m7b','m8b',
                   'm9b','m10b','m11b','m12b','m13b','m14b','m15b','m16b','m17b',
                   'm18b')) %>%
  mutate(order = c('1b','2b','3b','4b','5b','6b','7b','8b',
                   '9b','10b','11b','12b','13b','14b','15b','16b','17b',
                   '18b')) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma),
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log,fit_log_LPI, 	fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
  ggplot(., aes(x=factor(model, level=c('m1b','m2b','m3b','m4b','m5b','m6b','m7b','m8b',
                                        'm9b','m10b','m11b','m12b','m13b','m14b','m15b','m16b','m17b',
                                        'm18b')), y=fit_log)) +
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
ggsave(paste0(results.directory, "figs/forecast_models_odd.png"), dpi = 500, height = 4, width = 10, units = "in")

