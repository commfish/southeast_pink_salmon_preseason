# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 5/12/2021
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
library(dLagM) # MASE calc
library(ggplot2)
library(car)
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
year.forecast <- "2022_forecast" 
year.data <- 2020  
year.data.one <- year.data - 1
sample_size <-  23 # number of data points in model

data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')
source('2022_forecast/code/functions.r')

# STEP 1: DATA
# read in data
read.csv(file.path(data.directory,'var2021_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names

# restructure the data 
variables$CPUE <- variables$CPUEcal # Use CPUEcal as CPUE index
n <- dim(variables)[1] # number of years including forecast year
variables %>% 
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch,	CPUEcal)) -> log_data 

# STEP #2: HARVEST MODELS AND SUMMARY STATS
# define model names and formulas
model.names <- c(m1='CPUE',
               m2='CPUE + ISTI3_May',
               m3='CPUE + ISTI10_May',
               m4='CPUE + ISTI15_May',
               m5='CPUE + ISTI20_May',
               m6='CPUE + ISTI3_MJJ',
               m7='CPUE + ISTI10_MJJ',
               m8='CPUE + ISTI15_MJJ',
               m9='CPUE + ISTI20_MJJ',
               m10='CPUE + IS3_May',
               m11='CPUE + IS3_MJJ',
               m12='CPUE + Chatham_Strait_SST_MJJ',
               m13='CPUE + Chatham_Strait_SST_May',
               m14='CPUE + Chatham_Strait_SST_AMJJ',
               m15='CPUE + Icy_Strait_SST_MJJ',
               m16='CPUE + Icy_Strait_SST_May',
               m17='CPUE + Icy_Strait_SST_AMJJ',
               m18='CPUE + NSEAK_SST_MJJ',
               m19='CPUE + NSEAK_SST_May',
               m20='CPUE + NSEAK_SST_AMJJ',
               m21='CPUE + SST_Jordan_MJJ',
               m22='CPUE + SST_Jordan_May',
               m23='CPUE + SST_Jordan_AMJJ')
model.formulas <- c(SEAKCatch_log ~ CPUE,
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
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values)

# summary of model fits (i.e., coefficients, p-value)
log_data %>% 
  dplyr::filter(JYear < year.data) -> log_data_subset 

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ CPUE + ISTI3_May, data = log_data_subset) -> m2
lm(SEAKCatch_log ~ CPUE + ISTI10_May, data = log_data_subset) -> m3
lm(SEAKCatch_log ~ CPUE + ISTI15_May, data = log_data_subset) -> m4
lm(SEAKCatch_log ~ CPUE + ISTI20_May, data = log_data_subset) -> m5
lm(SEAKCatch_log ~ CPUE + ISTI3_MJJ, data = log_data_subset) -> m6
lm(SEAKCatch_log ~ CPUE + ISTI10_MJJ, data = log_data_subset) -> m7
lm(SEAKCatch_log ~ CPUE + ISTI15_MJJ, data = log_data_subset) -> m8
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_subset) -> m9
lm(SEAKCatch_log ~ CPUE + IS3_May, data = log_data_subset) -> m10
lm(SEAKCatch_log ~ CPUE + IS3_MJJ, data = log_data_subset) -> m11
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_MJJ, data = log_data_subset) -> m12
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_May, data = log_data_subset) -> m13
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_AMJJ, data = log_data_subset) -> m14
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_MJJ, data = log_data_subset) -> m15
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset) -> m16
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m17
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_MJJ, data = log_data_subset) -> m18
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m19
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset) -> m20
lm(SEAKCatch_log ~ CPUE + SST_Jordan_MJJ, data = log_data_subset) -> m21
lm(SEAKCatch_log ~ CPUE + SST_Jordan_May, data = log_data_subset) -> m22
lm(SEAKCatch_log ~ CPUE + SST_Jordan_AMJJ, data = log_data_subset) -> m23


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
tidy(m19) -> model19
tidy(m20) -> model20
tidy(m21) -> model21
tidy(m22) -> model22
tidy(m23) -> model23

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
rbind(., model19) %>% 
rbind(., model20) %>% 
rbind(., model21) %>% 
rbind(., model22) %>% 
rbind(., model23) %>%   
mutate(model = c('m1','m1','m2','m2','m2','m3','m3','m3',
                 'm4','m4','m4','m5','m5','m5','m6','m6',' m6',
                 'm7','m7','m7','m8','m8','m8','m9','m9',' m9',
                 'm10','m10','m10','m11','m11','m11','m12','m12',' m12',
                 'm13','m13','m13','m14','m14','m14','m15','m15',' m15',
                 'm16','m16','m16','m17','m17','m17','m18','m18',' m18',
                 'm19','m19','m19','m20','m20','m20','m21','m21',' m21',
                 'm22','m22','m22','m23','m23','m23')) %>% 
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
                   'm9','m10','m11','m12','m13','m14','m15','m16',' m17',
                   'm18','m19','m20','m21','m22','m23')) %>%
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
                   'm18','m19','m20','m21','m22','m23')) %>%
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
ggsave(paste0(results.directory, "forecast_models.png"), dpi = 500, height = 4, width = 6, units = "in")

# summary of interaction model fits (i.e., coefficients, p-value)
log_data %>% 
  dplyr::filter(JYear < year.data) -> log_data_subset 

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ CPUE * ISTI3_May, data = log_data_subset) -> m2i
lm(SEAKCatch_log ~ CPUE * ISTI10_May, data = log_data_subset) -> m3i
lm(SEAKCatch_log ~ CPUE * ISTI15_May, data = log_data_subset) -> m4i
lm(SEAKCatch_log ~ CPUE * ISTI20_May, data = log_data_subset) -> m5i
lm(SEAKCatch_log ~ CPUE * ISTI3_MJJ, data = log_data_subset) -> m6i
lm(SEAKCatch_log ~ CPUE * ISTI10_MJJ, data = log_data_subset) -> m7i
lm(SEAKCatch_log ~ CPUE * ISTI15_MJJ, data = log_data_subset) -> m8i
lm(SEAKCatch_log ~ CPUE * ISTI20_MJJ, data = log_data_subset) -> m9i
lm(SEAKCatch_log ~ CPUE * IS3_May, data = log_data_subset) -> m10i
lm(SEAKCatch_log ~ CPUE * IS3_MJJ, data = log_data_subset) -> m11i
lm(SEAKCatch_log ~ CPUE * Chatham_Strait_SST_MJJ, data = log_data_subset) -> m12i
lm(SEAKCatch_log ~ CPUE * Chatham_Strait_SST_May, data = log_data_subset) -> m13i
lm(SEAKCatch_log ~ CPUE * Chatham_Strait_SST_AMJJ, data = log_data_subset) -> m14i
lm(SEAKCatch_log ~ CPUE * Icy_Strait_SST_MJJ, data = log_data_subset) -> m15i
lm(SEAKCatch_log ~ CPUE * Icy_Strait_SST_May, data = log_data_subset) -> m16i
lm(SEAKCatch_log ~ CPUE * Icy_Strait_SST_AMJJ, data = log_data_subset) -> m17i
lm(SEAKCatch_log ~ CPUE * NSEAK_SST_MJJ, data = log_data_subset) -> m18i
lm(SEAKCatch_log ~ CPUE * NSEAK_SST_May, data = log_data_subset) -> m19i
lm(SEAKCatch_log ~ CPUE * NSEAK_SST_AMJJ, data = log_data_subset) -> m20i
lm(SEAKCatch_log ~ CPUE * SST_Jordan_MJJ, data = log_data_subset) -> m21i
lm(SEAKCatch_log ~ CPUE * SST_Jordan_May, data = log_data_subset) -> m22i
lm(SEAKCatch_log ~ CPUE * SST_Jordan_AMJJ, data = log_data_subset) -> m23i


tidy(m1) -> model1
tidy(m2i) -> model2
tidy(m3i) -> model3
tidy(m4i) -> model4
tidy(m5i) -> model5
tidy(m6i) -> model6
tidy(m7i) -> model7
tidy(m8i) -> model8
tidy(m9i) -> model9
tidy(m10i) -> model10
tidy(m11i) -> model11
tidy(m12i) -> model12
tidy(m13i) -> model13
tidy(m14i) -> model14
tidy(m15i) -> model15
tidy(m16i) -> model16
tidy(m17i) -> model17
tidy(m18i) -> model18
tidy(m19i) -> model19
tidy(m20i) -> model20
tidy(m21i) -> model21
tidy(m22i) -> model22
tidy(m23i) -> model23

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
  rbind(., model19) %>% 
  rbind(., model20) %>% 
  rbind(., model21) %>% 
  rbind(., model22) %>% 
  rbind(., model23) %>%   
  mutate(model = c('m1','m1','m2i','m2i','m2i','m2i','m3i','m3i','m3i','m3i',
                   'm4i','m4i','m4i','m4i','m5i','m5i','m5i','m5i','m6i','m6i','m6i','m6i',
                   'm7i','m7i','m7i','m7i', 'm8i','m8i','m8i','m8i','m9i','m9i',' m9i','m9i',
                   'm10i','m10i','m10i','m10i','m11i','m11i','m11i','m11i', 'm12i','m12i',' m12i','m12i',
                   'm13i','m13i','m13i','m13i','m14i','m14i','m14i','m14i', 'm15i','m15i','m15i','m15i',
                   'm16i','m16i','m16i','m16i','m17i','m17i','m17i','m17i',
                   'm18i','m18i','m18i','m18i',
                   'm19i','m19i','m19i','m19i',
                   'm20i','m20i','m20i','m20i', 'm21i','m21i','m21i','m21i',
                   'm22i','m22i','m22i','m22i','m23i','m23i','m23i','m23i')) %>% 
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(estimate = round(estimate,8),
         std.error = round(std.error,3),
         statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  write.csv(., paste0(results.directory, "/model_summary_table4.csv"), row.names = F)
