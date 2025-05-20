# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/10/2022
# last update: May 2025
# pink_cal_pooled_species
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
# update all data files varyyyy_final.csv
# Note: summary table 5 needs to be updated manually from the excel sheet model_summary_table_month_year.xlsx
# https://www.r-bloggers.com/2021/10/multiple-linear-regression-made-simple/
# load libraries
library("devtools")
devtools::install_github("commfish/fngr")
library("fngr")
library(stats)
library(gam)
library(MASS)
library(MuMIn)
library(AICcmodavg)
library(broom)
library(rpart)
library(mda)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(extrafont)
library(ggrepel)
library(Metrics) # MASE calc
library(MetricsWeighted)
library("RColorBrewer") 
library(StepReg)
library(ggh4x)
#extrafont::font_import() # only need to run this once, then comment out
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2026_forecast" # forecast year 
year.data <- 2025 # last year of data
year.data.one <- year.data - 1
sample_size <-  (year.data-1998)+1 # number of data points in model (this is used for Cook's distance)
# forecast2023 <- 15.6 # input last year's forecast for the forecast plot
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')
results.directory.MAPE <- file.path(year.forecast,  'results/MAPE', '/')
results.directory.retro <- file.path(year.forecast,  'results/retro', '/')
source('2026_forecast/code/functions.r') # source the function file for functions used below

# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2025_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables_temp # update file names
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
  dplyr::select(-c(SEAKCatch)) -> log_data

# data check only  
 log_data %>%
    write.csv(., paste0(data.directory, "/var2025_merge.csv"), row.names = F)
# STEP 2: MODELS
 model.names <- c(m1='vessel x adj_raw_pink_log + odd_factor +vessel',
                  m2='vessel x adj_raw_pink_log + ISTI20_MJJ + odd_factor x adj_raw_pink_log + vessel',
                  m3='vessel x adj_raw_pink_log + Chatham_SST_May + odd_factor x adj_raw_pink_log + vessel',
                  m4='vessel x adj_raw_pink_log + Chatham_SST_MJJ + odd_factor x adj_raw_pink_log + vessel',
                  m5='vessel x adj_raw_pink_log + Chatham_SST_AMJ + odd_factor x adj_raw_pink_log + vessel',
                  m6='vessel x adj_raw_pink_log + Chatham_SST_AMJJ + odd_factor x adj_raw_pink_log + vessel',
                  m7='vessel x adj_raw_pink_log + Icy_Strait_SST_May + odd_factor x adj_raw_pink_log + vessel',
                  m8='vessel x adj_raw_pink_log + Icy_Strait_SST_MJJ + odd_factor x adj_raw_pink_log + vessel',
                  m9='vessel x adj_raw_pink_log + Icy_Strait_SST_AMJ + odd_factor x adj_raw_pink_log + vessel',
                  m10='vessel x adj_raw_pink_log + Icy_Strait_SST_AMJJ + odd_factor x adj_raw_pink_log + vessel',
                  m11='vessel x adj_raw_pink_log + NSEAK_SST_May + odd_factor x adj_raw_pink_log + vessel',
                  m12='vessel x adj_raw_pink_log + NSEAK_SST_MJJ + odd_factor x adj_raw_pink_log + vessel',
                  m13='vessel x adj_raw_pink_log+  NSEAK_SST_AMJ + odd_factor x adj_raw_pink_log + vessel',
                  m14='vessel x adj_raw_pink_log + NSEAK_SST_AMJJ + odd_factor x adj_raw_pink_log + vessel',
                  m15='vessel x adj_raw_pink_log + SEAK_SST_May + odd_factor x adj_raw_pink_log + vessel',
                  m16='vessel x adj_raw_pink_log + SEAK_SST_MJJ + odd_factor x adj_raw_pink_log + vessel',
                  m17='vessel x adj_raw_pink_log + SEAK_SST_AMJ + odd_factor x adj_raw_pink_log + vessel',
                  m18='vessel x adj_raw_pink_log + SEAK_SST_AMJJ + odd_factor x adj_raw_pink_log + vessel')
 
 model.formulas <- c(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + ISTI20_MJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_May + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_May + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_May + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log+  NSEAK_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_May + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel),
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel))
 
 # summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
 seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values, models = "_multi")

 # STEP #3: SUMMARY OF MODEL FITS
# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
 log_data %>%
   dplyr::filter(JYear < year.data) -> log_data_subset
 
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel), data = log_data_subset) -> m1
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + ISTI20_MJJ, data = log_data_subset) -> m2
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_May, data = log_data_subset) -> m3
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_MJJ, data = log_data_subset) -> m4
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_AMJ, data = log_data_subset) -> m5
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_AMJJ, data = log_data_subset) -> m6
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_May, data = log_data_subset) -> m7
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_MJJ, data = log_data_subset) -> m8
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_AMJ, data = log_data_subset) -> m9
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_AMJJ, data = log_data_subset) -> m10
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_May, data = log_data_subset) -> m11
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_MJJ, data = log_data_subset) -> m12
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_AMJ, data = log_data_subset) -> m13
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_AMJJ, data = log_data_subset) -> m14
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_May, data = log_data_subset) -> m15
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_MJJ, data = log_data_subset) -> m16
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_AMJ, data = log_data_subset) -> m17
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_AMJJ, data = log_data_subset) -> m18
 
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
   rbind(., model18) -> models 
 nyear <- 7
 model <- c(rep('m1',nyear),rep('m2',nyear+1),rep('m3',nyear+1),rep('m4',nyear+1),
            rep('m5',nyear+1),rep('m6',nyear+1),rep('m7',nyear+1),rep('m8',nyear+1),
            rep('m9',nyear+1),rep('m10',nyear+1),rep('m11',nyear+1),rep('m12',nyear+1),
            rep('m13',nyear+1),rep('m14',nyear+1),rep('m15',nyear+1),rep('m16',nyear+1),
            rep('m17',nyear+1),rep('m18',nyear+1))
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
   write.csv(., paste0(results.directory, "/model_summary_table1_multi.csv"), row.names = F) # detailed model summaries
 
 # calculate one step ahead MAPE
 # https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
 # end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
 # forecasted in the one step ahead process)
 # https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html
 
 # STEP #4: CALCULATE ONE_STEP_AHEAD MAPE
 f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2019, models="_multi")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)
 
 # if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
 # then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
 # function is correct for the base CPUE model
 
 read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead5_multi.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
   dplyr::rename(terms = 'X') %>%
   mutate(MAPE5 = round(MAPE5,3)) %>%
   dplyr::select(terms, MAPE5) -> MAPE5
 
 read.csv(file.path(results.directory,'seak_model_summary_multi.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
   dplyr::rename(terms = 'X') %>%
   dplyr::select(terms, fit, fit_UPI, fit_LPI,AdjR2, sigma, AICc) %>%
   mutate(AdjR2 = round(AdjR2,3)) %>%
   mutate(Model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                    'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                    'm18')) %>%
   mutate(fit_log = exp(fit)*exp(0.5*sigma*sigma),
          fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
          fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
   mutate(Fit = round(fit_log,3),
          Fit_LPI = round(fit_log_LPI,3),
          Fit_UPI = round(fit_log_UPI,3)) %>%
   dplyr::select(Model, terms, Fit, Fit_LPI, Fit_UPI, AdjR2, AICc) %>%
   merge(., MAPE5, by="terms") %>%
   arrange(MAPE5) %>%
   write.csv(., paste0(results.directory, "/model_summary_table2_multi.csv"), row.names = F)
 
  # STEP #5: FORECAST FIGURE
 read.csv(file.path(results.directory,'seak_model_summary_multi.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
 results %>%
   dplyr::rename(terms = 'X') %>%
   dplyr::select(terms, fit, fit_LPI, fit_UPI, sigma) %>%
   mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                    'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                    'm18')) %>%
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
   ggplot(., aes(x=factor(model, level=c('m1','m2','m3','m4','m5','m6','m7','m8',
                                         'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                                         'm18')), y=fit_log)) +
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
   scale_y_continuous(breaks = c(0,50, 100, 150, 200), limits = c(0,200))+
   labs(x = "Models", y = "2026 SEAK Pink Salmon Harvest Forecast (millions)")  -> plot1
 ggsave(paste0(results.directory, "figs/forecast_models_multi.png"), dpi = 500, height = 4, width = 7, units = "in")
 
 