# source code and functions
source('2022_forecast/code/1_summarize_models.r')
source('2022_forecast/code/functions.r')

# STEP 1: DATA
# read in data
read.csv(file.path(data.directory,'var2021_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables 

# restructure the data (exclude influential years based on leverage and Cook's distance plots)
variables$CPUE <- variables$CPUEcal # Use CPUEcal as CPUE index
n <- dim(variables)[1] # number of years including forecast year
variables %>% 
  filter(JYear != 1998) %>% # exclude juvenile year 1998
  filter(JYear != 2016) %>% # exclude juvenile year 2016
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch,	CPUEcal)) -> log_data_sensitivity

# STEP #2: HARVEST MODELS AND SUMMARY STATS
# define model names and formulas
model.names.sensitivity <- c(m1s='CPUE',
               m2s='CPUE + ISTI3_May',
               m3s='CPUE + ISTI10_May',
               m4s='CPUE + ISTI15_May',
               m5s='CPUE + ISTI20_May',
               m6s='CPUE + ISTI3_MJJ',
               m7s='CPUE + ISTI10_MJJ',
               m8s='CPUE + ISTI15_MJJ',
               m9s='CPUE + ISTI20_MJJ',
               m10s='CPUE + IS3_May',
               m11s='CPUE + IS3_MJJ',
               m12s='CPUE + Chatham_Strait_SST_MJJ',
               m13s='CPUE + Chatham_Strait_SST_May',
               m14s='CPUE + Chatham_Strait_SST_AMJJ',
               m15s='CPUE + Icy_Strait_SST_MJJ',
               m16s='CPUE + Icy_Strait_SST_May',
               m17s='CPUE + Icy_Strait_SST_AMJJ',
               m18s='CPUE + NSEAK_SST_MJJ',
               m19s='CPUE + NSEAK_SST_May',
               m20s='CPUE + NSEAK_SST_AMJJ',
               m21s='CPUE + SST_Jordan_MJJ',
               m22s='CPUE + SST_Jordan_May',
               m23s='CPUE + SST_Jordan_AMJJ')
model.formulas.sensitivity <- c(SEAKCatch_log ~ CPUE,
                 SEAKCatch_log ~ CPUE + ISTI3_May,
                 SEAKCatch_log ~ CPUE + ISTI10_May,
                 SEAKCatch_log ~ CPUE + ISTI15_May,
                 SEAKCatch_log ~ CPUE + ISTI20_May,
                 SEAKCatch_log ~ CPUE + ISTI3_MJJ,
                 SEAKCatch_log ~ CPUE + ISTI10_MJJ,
                 SEAKCatch_log ~ CPUE + ISTI15_MJJ,
                 SEAKCatch_log ~ CPUE + ISTI20_MJJ,
                 SEAKCatch_log ~ CPUE + IS3_May,
                 SEAKCatch_log ~ CPUE + IS3_MJJ,
                 SEAKCatch_log ~ CPUE + Chatham_Strait_SST_MJJ,
                 SEAKCatch_log ~ CPUE + Chatham_Strait_SST_May,
                 SEAKCatch_log ~ CPUE + Chatham_Strait_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_May,
                 SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_May,
                 SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ,
                 SEAKCatch_log ~ CPUE + SST_Jordan_MJJ,
                 SEAKCatch_log ~ CPUE + SST_Jordan_May,
                 SEAKCatch_log ~ CPUE + SST_Jordan_AMJJ) # temp. data 

# summary statistics and bootstrap of SEAK pink salmon harvest forecast models
seak_model_summary <- f_model_sensitivity(harvest=log_data_sensitivity$SEAKCatch_log, variables=log_data_sensitivity, model.formulas=model.formulas.sensitivity,model.names=model.names.sensitivity, w = log_data_sensitivity$weight_values)

# summary of model fits (i.e., coefficients, p-value)
log_data_sensitivity %>% 
  dplyr::filter(JYear < year.data) -> log_data_subset_sensitivity 

lm(SEAKCatch_log ~ CPUE, data = log_data_subset_sensitivity) -> m1s
lm(SEAKCatch_log ~ CPUE + ISTI3_May, data = log_data_subset_sensitivity) -> m2s
lm(SEAKCatch_log ~ CPUE + ISTI10_May, data = log_data_subset_sensitivity) -> m3s
lm(SEAKCatch_log ~ CPUE + ISTI15_May, data = log_data_subset_sensitivity) -> m4s
lm(SEAKCatch_log ~ CPUE + ISTI20_May, data = log_data_subset_sensitivity) -> m5s
lm(SEAKCatch_log ~ CPUE + ISTI3_MJJ, data = log_data_subset_sensitivity) -> m6s
lm(SEAKCatch_log ~ CPUE + ISTI10_MJJ, data = log_data_subset_sensitivity) -> m7s
lm(SEAKCatch_log ~ CPUE + ISTI15_MJJ, data = log_data_subset_sensitivity) -> m8s
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_subset_sensitivity) -> m9s
lm(SEAKCatch_log ~ CPUE + IS3_May, data = log_data_subset_sensitivity) -> m10s
lm(SEAKCatch_log ~ CPUE + IS3_MJJ, data = log_data_subset_sensitivity) -> m11s
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_MJJ, data = log_data_subset_sensitivity) -> m12s
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_May, data = log_data_subset_sensitivity) -> m13s
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_AMJJ, data = log_data_subset_sensitivity) -> m14s
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ, data = log_data_subset_sensitivity) -> m15s
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset_sensitivity) -> m16s
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ, data = log_data_subset_sensitivity) -> m17s
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ, data = log_data_subset_sensitivity) -> m18s
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset_sensitivity) -> m19s
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset_sensitivity) -> m20s
lm(SEAKCatch_log ~ CPUE + SST_Jordan_MJJ, data = log_data_subset_sensitivity) -> m21s
lm(SEAKCatch_log ~ CPUE + SST_Jordan_May, data = log_data_subset_sensitivity) -> m22s
lm(SEAKCatch_log ~ CPUE + SST_Jordan_AMJJ, data = log_data_subset_sensitivity) -> m23s

tidy(m1s) -> model1s
tidy(m2s) -> model2s
tidy(m3s) -> model3s
tidy(m4s) -> model4s
tidy(m5s) -> model5s
tidy(m6s) -> model6s
tidy(m7s) -> model7s
tidy(m8s) -> model8s
tidy(m9s) -> model9s
tidy(m10s) -> model10s
tidy(m11s) -> model11s
tidy(m12s) -> model12s
tidy(m13s) -> model13s
tidy(m14s) -> model14s
tidy(m15s) -> model15s
tidy(m16s) -> model16s
tidy(m17s) -> model17s
tidy(m18s) -> model18s
tidy(m19s) -> model19s
tidy(m20s) -> model20s
tidy(m21s) -> model21s
tidy(m22s) -> model22s
tidy(m23s) -> model23s

rbind(model1s, model2s) %>% 
rbind(., model3s) %>% 
rbind(., model4s) %>% 
rbind(., model5s) %>% 
rbind(., model6s) %>% 
rbind(., model7s) %>% 
rbind(., model8s) %>%   
rbind(., model9s) %>% 
rbind(., model10s) %>% 
rbind(., model11s) %>% 
rbind(., model12s) %>% 
rbind(., model13s) %>% 
rbind(., model14s) %>%   
rbind(., model15s) %>% 
rbind(., model16s) %>% 
rbind(., model17s) %>% 
rbind(., model18s) %>% 
rbind(., model19s) %>% 
rbind(., model20s) %>% 
rbind(., model21s) %>% 
rbind(., model22s) %>% 
rbind(., model23s) %>%   
mutate(model = c('m1s','m1s','m2s','m2s','m2s','m3s','m3s','m3s',
                 'm4s','m4s','m4s','m5s','m5s','m5s','m6s','m6s',' m6s',
                 'm7s','m7s','m7s','m8s','m8s','m8s','m9s','m9s',' m9s',
                 'm10s','m10s','m10s','m11s','m11s','m11s','m12s','m12s',' m12s',
                 'm13s','m13s','m13s','m14s','m14s','m14s','m15s','m15s',' m15s',
                 'm16s','m16s','m16s','m17s','m17s','m17s','m18s','m18s',' m18s',
                 'm19s','m19s','m19s','m20s','m20s','m20s','m21s','m21s',' m21s',
                 'm22s','m22s','m22s','m23s','m23s','m23s')) %>% 
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(estimate = round(estimate,8),
         std.error = round(std.error,3),
         statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
write.csv(., paste0(results.directory, "/model_summary_table_sensitivity1.csv"), row.names = F)

read.csv(file.path(results.directory,'seak_model_summary_sensitivity.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
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
  mutate(model = c('m1s','m2s','m3s','m4s','m5s','m6s','m7s','m8s',
                   'm9s','m10s','m11s','m12s','m13s','m14s','m15s','m16s',' m17s',
                   'm18s','m19s','m20s','m21s','m22s','m23s')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(fit = round(fit_log,3), 
         fit_LPI = round(fit_log_LPI,3),
         fit_UPI = round(fit_log_UPI,3)) %>% 
  dplyr::select(model, terms, fit,	AdjR2, AICc,MASE ,wMAPE, MAPE_LOOCV) %>%
  write.csv(paste0(results.directory, "/model_summary_table_sensitivity2.csv"), row.names = F)

read.csv(file.path(results.directory,'seak_model_summary_sensitivity.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('m1s','m2s','m3s','m4s','m5s','m6s','m7s','m8s',
                   'm9s','m10s','m11s','m12s','m13s','m14s','m15s','m16s',' m17s',
                   'm18s','m19s','m20s','m21s','m22s','m23s')) %>%
  mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
         fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
         fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>% # exponentiate the forecast
  mutate(fit = round(fit_log,3), 
         fit_LPI = round(fit_log_LPI,3),
         fit_UPI = round(fit_log_UPI,3)) %>% 
  dplyr::select(model, terms, fit,	fit_LPI, fit_UPI) %>%
  write.csv(paste0(results.directory, "/model_summary_table_sensitivity3.csv"), row.names = F)

# forecast figure
read.csv(file.path(results.directory,'seak_model_summary_sensitivity.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(terms, fit,	fit_LPI,	fit_UPI, sigma) %>%
  mutate(model = c('1','2','3','4','5','6','7','8',
                   '9','10','11','12','13','14','15','16',' 17',
                   '18','19','20','21','22','23')) %>%
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
  geom_hline(aes(yintercept=mean(fit_log)), color="grey50", lty = 2) +
  geom_errorbar(mapping=aes(x=model, ymin=fit_log_UPI, ymax=fit_log_LPI), width=0.2, size=1, color="blue")+
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55), limits = c(0,55))+ 
  labs(x = "Models", y = "2021 SEAK Pink Salmon Forecast (millions)")  -> plot1
ggsave(paste0(results.directory, "forecast_models_sensitivity.png"), dpi = 500, height = 4, width = 6, units = "in")

seak_model_summary <- f_model_one_step_ahead_multiple_sensitive(harvest=log_data_sensitivity$SEAKCatch_log, variables=log_data_sensitivity, model.formulas=model.formulas.sensitivity,model.names=model.names.sensitivity, start = 1997, end = 2014)

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead_sensitive.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
read.csv(file.path(results.directory,'model_summary_table_sensitivity2.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> model_summary_table2
results %>% 
  mutate(MAPE_one_step_ahead = round(MAPE,3)) %>%
  dplyr::select(MAPE_one_step_ahead) %>%
  cbind(., model_summary_table2) %>%
  dplyr::select(model, AdjR2,  AICc,  MASE, wMAPE, MAPE_LOOCV, MAPE_one_step_ahead) %>%
  write.csv(paste0(results.directory, "/model_summary_table_sensitivity4.csv"), row.names = F)