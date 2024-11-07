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

# STEP #2: MODELS
mod1 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor), data = log_data)
mod2 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + ISTI20_MJJ, data = log_data)
mod3 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data)
mod4 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_May, data = log_data)
mod5 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data)
mod6 <- lm(SEAKCatch_log ~  as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data)
mod7 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data)
mod8 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data)
mod9 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data)
mod10 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data)
mod11 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data)
mod12 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data)
mod13<-  lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data)
mod14<-  lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data)
mod15 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data)
mod16 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_May, data = log_data)
mod17 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data)
mod18 <- lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data)
# potential models
model.names <- c(m1='vessel x adj_raw_pink_log + odd_factor ',
                 m2='vessel x adj_raw_pink_log + ISTI20_MJJ + odd_factor ',
                 m3='vessel x adj_raw_pink_log + Chatham_SST_May + odd_factor',
                 m4='vessel x adj_raw_pink_log + Chatham_SST_MJJ + odd_factor',
                 m5='vessel x adj_raw_pink_log + Chatham_SST_AMJ + odd_factor',
                 m6='vessel x adj_raw_pink_log + Chatham_SST_AMJJ + odd_factor',
                 m7='vessel x adj_raw_pink_log + Icy_Strait_SST_May + odd_factor',
                 m8='vessel x adj_raw_pink_log + Icy_Strait_SST_MJJ + odd_factor',
                 m9='vessel x adj_raw_pink_log + Icy_Strait_SST_AMJ + odd_factor',
                 m10='vessel x adj_raw_pink_log + Icy_Strait_SST_AMJJ + odd_factor',
                 m11='vessel x adj_raw_pink_log + NSEAK_SST_May + odd_factor',
                 m12='vessel x adj_raw_pink_log + NSEAK_SST_MJJ + odd_factor',
                 m13='vessel x adj_raw_pink_log+  NSEAK_SST_AMJ + odd_factor',
                 m14='vessel x adj_raw_pink_log + NSEAK_SST_AMJJ + odd_factor',
                 m15='vessel x adj_raw_pink_log + SEAK_SST_May + odd_factor',
                 m16='vessel x adj_raw_pink_log + SEAK_SST_MJJ + odd_factor',
                 m17='vessel x adj_raw_pink_log + SEAK_SST_AMJ + odd_factor',
                 m18='vessel x adj_raw_pink_log + SEAK_SST_AMJJ + odd_factor')

model.formulas <- c(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + ISTI20_MJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Chatham_SST_May + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Chatham_SST_MJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Chatham_SST_AMJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Chatham_SST_AMJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Icy_Strait_SST_May + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Icy_Strait_SST_MJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Icy_Strait_SST_AMJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + Icy_Strait_SST_AMJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + NSEAK_SST_May + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + NSEAK_SST_MJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log+  NSEAK_SST_AMJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + NSEAK_SST_AMJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + SEAK_SST_May + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + SEAK_SST_MJJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + SEAK_SST_AMJ + as.factor(odd_even_factor),
                    SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + SEAK_SST_AMJJ + as.factor(odd_even_factor))
# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values, models = "_vessel_inter")

# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data %>%
  dplyr::filter(JYear < year.data) -> log_data_subset

lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor), data = log_data_subset) -> m1d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + ISTI20_MJJ, data = log_data_subset) -> m2d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_May, data = log_data_subset) -> m3d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_MJJ, data = log_data_subset) -> m4d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_AMJ, data = log_data_subset) -> m5d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Chatham_SST_AMJJ, data = log_data_subset) -> m6d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_May, data = log_data_subset) -> m7d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_May, data = log_data_subset) -> m11d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_MJJ, data = log_data_subset) -> m12d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data_subset) -> m13d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + NSEAK_SST_AMJJ, data = log_data_subset) -> m14d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_May, data = log_data_subset) -> m15d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_MJJ, data = log_data_subset) -> m16d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_AMJ, data = log_data_subset) -> m17d
lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + SEAK_SST_AMJJ, data = log_data_subset) -> m18d


tidy(m1d) -> model1
tidy(m2d) -> model2
tidy(m3d) -> model3
tidy(m4d) -> model4
tidy(m5d) -> model5
tidy(m6d) -> model6
tidy(m7d) -> model7
tidy(m8d) -> model8
tidy(m9d) -> model9
tidy(m10d) -> model10
tidy(m11d) -> model11
tidy(m12d) -> model12
tidy(m13d) -> model13
tidy(m14d) -> model14
tidy(m15d) -> model15
tidy(m16d) -> model16
tidy(m17d) -> model17
tidy(m18d) -> model18

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
rbind(., model18) -> models 
nyear <- 5
model <- c(rep('m1d',nyear),rep('m2d',nyear+1),rep('m3d',nyear+1),rep('m4d',nyear+1),
             rep('m5d',nyear+1),rep('m6d',nyear+1),rep('m7d',nyear+1),rep('m8d',nyear+1),
             rep('m9d',nyear+1),rep('m10d',nyear+1),rep('m11d',nyear+1),rep('m12d',nyear+1),
             rep('m13d',nyear+1),rep('m14d',nyear+1),rep('m15d',nyear+1),rep('m16d',nyear+1),
             rep('m17d',nyear+1),rep('m18d',nyear+1))
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
write.csv(., paste0(results.directory, "/model_summary_table1_vessel_inter.csv"), row.names = F) # detailed model summaries

# calculate one step ahead MAPE
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
# forecasted in the one step ahead process)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html

# STEP #3: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2018, models="_vessel_inter")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)

# if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
# then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
# function is correct for the base CPUE model

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5_vessel_inter.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  mutate(MAPE5 = round(MAPE5,3)) %>%
  dplyr::select(terms, MAPE5) -> MAPE5

# format the file seak_model_summary.csv file
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model; Correcting log-transformation bias in a linear model
# https://stackoverflow.com/questions/40324963/when-predicting-using-model-with-logtarget-do-i-have-to-make-any-changes-to-pr # mase3<-dLagM::MASE(m18)
read.csv(file.path(results.directory,'seak_model_summary_vessel_inter.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit, fit_UPI, fit_LPI,AdjR2, sigma, MAPE) %>%
  mutate(AdjR2 = round(AdjR2,3)) %>%
  mutate(Model = c('m1d','m2d','m3d','m4d','m5d','m6d','m7d','m8d',
                   'm9d','m10d','m11d','m12d','m13d','m14d','m15d','m16d','m17d',
                   'm18d')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  mutate(Fit = round(fit_log,3),
         Fit_LPI = round(fit_log_LPI,3),
         Fit_UPI = round(fit_log_UPI,3)) %>%
  dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE) %>%
  merge(., MAPE5, by="terms") %>%
  write.csv(., paste0(results.directory, "/model_summary_table2_vessel_inter.csv"), row.names = F)

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary_vessel_inter.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit, fit_LPI, fit_UPI, sigma) %>%
  mutate(model = c('m1d','m2d','m3d','m4d','m5d','m6d','m7d','m8d',
                   'm9d','m10d','m11d','m12d','m13d','m14d','m15d','m16d','m17d',
                   'm18d')) %>%
  mutate(order = c('1','2','3','4','5','6','7','8',
                   '9','10','11','12','13','14','15','16','17',
                   '18')) %>%
  mutate(order = as.numeric(order)) %>%
  mutate(model= as.factor(model),
         fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(model, order, terms, fit_log, fit_log_LPI, fit_log_UPI) %>%
  as.data.frame() %>%
  dplyr::arrange(order) %>%
ggplot(., aes(x=factor(model, level=c('m1d','m2d','m3d','m4d','m5d','m6d','m7d','m8d',
                                      'm9d','m10d','m11d','m12d','m13d','m14d','m15d','m16d','m17d',
                                      'm18d')), y=fit_log)) +
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
  scale_y_continuous(breaks = c(0,10, 20, 30, 40, 50, 60, 70, 90, 80,100), limits = c(0,100))+
  labs(x = "Models", y = "2025 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "figs/forecast_models_vessel_inter.png"), dpi = 500, height = 4, width = 10, units = "in")

