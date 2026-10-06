# run code 3_summarize_models.R first/

# inputs
year.forecast <- "2027_forecast" # forecast year 
year.data <- 2026 # last year of data
year.data.one <- year.data - 1
sample_size <-  (year.data-1998)+1 # number of data points in model (this is used for Cook's distance)
# forecast2023 <- 15.6 # input last year's forecast for the forecast plot
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')
source('2027_forecast/code/functions.r') # source the function file for functions used below

# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2026_final.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) -> variables_temp # update file names
read.csv(file.path(data.directory,'adj_raw_pink.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) -> variables_adj_raw_pink # update file names

# STEP 1: DATA
log_data <- variables_adj_raw_pink %>%
  mutate(adj_raw_pink_log = log(adj_raw_pink + 1)) %>%  # log CPUE variable
  group_by(JYear, Year, vessel) %>% 
  summarise(adj_raw_pink_log = max(adj_raw_pink_log), .groups = "drop") %>% 
  inner_join(variables_temp, by = c("JYear", "Year")) %>%  
  filter(adj_raw_pink_log > 0) %>%
  filter(!vessel %in% c("Steller", "Chellissa")) %>%
  mutate(odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),  
    SEAKCatch_log = log(SEAKCatch)) %>% 
  dplyr::select(-SEAKCatch)

 
# STEP 2: MODELS
 model.names <- c(m1i='no temperature index included',
                  m2i='ISTI20_JJ',
                  m3i='Chatham_SST_May',
                  m4i='Chatham_SST_MJJ',
                  m5i='Chatham_SST_AMJ',
                  m6i='Chatham_SST_AMJJ',
                  m7i='Icy_Strait_SST_May',
                  m8i='Icy_Strait_SST_MJJ',
                  m9i='Icy_Strait_SST_AMJ',
                  m10i='Icy_Strait_SST_AMJJ',
                  m11i='NSEAK_SST_May',
                  m12i='NSEAK_SST_MJJ',
                  m13i='NSEAK_SST_AMJ',
                  m14i='NSEAK_SST_AMJJ',
                  m15i='SEAK_SST_May',
                  m16i='SEAK_SST_MJJ',
                  m17i='SEAK_SST_AMJ',
                  m18i='SEAK_SST_AMJJ')
 
 model.formulas <- c(SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + ISTI20_JJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_May + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Chatham_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_May + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + Icy_Strait_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_May + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log+  NSEAK_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + NSEAK_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_May + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_MJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_AMJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log,
                     SEAKCatch_log ~ as.factor(vessel) * adj_raw_pink_log + SEAK_SST_AMJJ + as.factor(odd_even_factor)+ as.factor(vessel) + adj_raw_pink_log)
 
 # summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
 seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values, models = "_inter")

# STEP #3: SUMMARY OF MODEL FITS
# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
 log_data %>%
   dplyr::filter(JYear < year.data) -> log_data_subset
 
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + adj_raw_pink_log, data = log_data_subset) -> m1i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + ISTI20_JJ + adj_raw_pink_log, data = log_data_subset) -> m2i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_May + adj_raw_pink_log, data = log_data_subset) -> m3i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_MJJ + adj_raw_pink_log, data = log_data_subset) -> m4i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_AMJ + adj_raw_pink_log, data = log_data_subset) -> m5i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Chatham_SST_AMJJ + adj_raw_pink_log, data = log_data_subset) -> m6i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_May + adj_raw_pink_log, data = log_data_subset) -> m7i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_MJJ + adj_raw_pink_log, data = log_data_subset) -> m8i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_AMJ + adj_raw_pink_log, data = log_data_subset) -> m9i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + Icy_Strait_SST_AMJJ + adj_raw_pink_log, data = log_data_subset) -> m10i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_May + adj_raw_pink_log, data = log_data_subset) -> m11i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_MJJ + adj_raw_pink_log, data = log_data_subset) -> m12i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_AMJ + adj_raw_pink_log, data = log_data_subset) -> m13i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + NSEAK_SST_AMJJ + adj_raw_pink_log, data = log_data_subset) -> m14i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_May + adj_raw_pink_log, data = log_data_subset) -> m15i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_MJJ + adj_raw_pink_log, data = log_data_subset) -> m16i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_AMJ + adj_raw_pink_log, data = log_data_subset) -> m17i
 lm(SEAKCatch_log ~ as.factor(vessel):adj_raw_pink_log + as.factor(odd_even_factor) + as.factor(vessel) + SEAK_SST_AMJJ + adj_raw_pink_log, data = log_data_subset) -> m18i
 
 tidy(m1i) -> model1
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
 model <- c(rep('m1i',nyear),rep('m2i',nyear+1),rep('m3i',nyear+1),rep('m4i',nyear+1),
            rep('m5i',nyear+1),rep('m6i',nyear+1),rep('m7i',nyear+1),rep('m8i',nyear+1),
            rep('m9i',nyear+1),rep('m10i',nyear+1),rep('m11i',nyear+1),rep('m12i',nyear+1),
            rep('m13i',nyear+1),rep('m14i',nyear+1),rep('m15i',nyear+1),rep('m16i',nyear+1),
            rep('m17i',nyear+1),rep('m18i',nyear+1))
 model<-as.data.frame(model)
 cbind(models, model) %>%
   dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
   mutate(Model = model,
          Term =term,
          Estimate = round(estimate,3),
          'Standard Error' = round(std.error,3),
          Statistic = round(statistic,3),
          'p value' = round(p.value,3)) %>%
   dplyr::select(Model, Term, Estimate, 'Standard Error', Statistic, 'p value') %>%
   write.csv(., paste0(results.directory, "/model_summary_table1_inter.csv"), row.names = F) # detailed model summaries
 
 # calculate one step ahead MAPE
 # https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
 # end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
 # forecasted in the one step ahead process)
 # https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html
 
 # STEP #4: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2020, models="_inter")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)

 # if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
 # then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
 # function is correct for the base CPUE model
 
 read.csv(file.path(results.directory,'model_summary_one_step_ahead5_inter.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) %>%
   dplyr::rename(Terms = 'X') %>%
   mutate(MAPE5 = round(MAPE5,3)*100) %>%
   dplyr::select(Terms, MAPE5) -> MAPE5
 
 read.csv(file.path(results.directory,'model_summary_inter.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) %>%
   dplyr::rename(Terms = 'X') %>%
   dplyr::select(Terms, fit, fit_UPI, fit_LPI,AdjR2, sigma, AICc) %>%
   mutate(AdjR2 = round(AdjR2,2),
   Model = paste0("m", seq_len(nrow(results)), "i"),  # dynamic model labels
   fit_log = exp(fit)*exp(0.5*sigma*sigma),
   fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
   fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
   Fit = round(fit_log,1),
   Fit_LPI = round(fit_log_LPI,1),
   Fit_UPI = round(fit_log_UPI,1),
   AICc = round(AICc, 1)) %>%
   dplyr::select(Model, Terms, Fit, Fit_LPI, Fit_UPI, AdjR2, AICc) %>%
   inner_join(MAPE5, by = "Terms") %>%  # tidyverse join
   mutate(MAPE5 = round(MAPE5,1)) %>%
   write.csv(., paste0(results.directory, "/model_summary_table2_inter.csv"), row.names = F)
 
# STEP #5: CREATE FORECAST FIGURE
 read.csv(file.path(results.directory,'model_summary_inter.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) -> results
 results %>%
   dplyr::rename(Terms = 'X') %>%
   dplyr::select(Terms, fit, fit_LPI, fit_UPI, sigma) %>%
   mutate(model = paste0("m", seq_len(nrow(results)), "i"),
          order = factor(seq_len(nrow(results)), levels = seq_len(nrow(results)), ordered = TRUE),
   fit_log = exp(fit)*exp(0.5*sigma*sigma),
          fit_log_LPI = exp(fit_LPI)*exp(0.5*sigma*sigma), # exponentiate the forecast
          fit_log_UPI = exp(fit_UPI)*exp(0.5*sigma*sigma)) %>%
   dplyr::select(model, order, Terms, fit_log, fit_log_LPI, fit_log_UPI) %>%
   as.data.frame() -> results
 
 results %>%
   ggplot(aes(
     x = factor(model, levels = c(
       'm1i','m2i','m3i','m4i','m5i','m6i','m7i','m8i',
       'm9i','m10i','m11i','m12i','m13i','m14i','m15i','m16i','m17i','m18i')), y = fit_log)) +
   geom_col(aes(fill = "SEAK pink catch"),
     colour = "grey70",width = 1,
     position = position_dodge(width = 0.1)) +
   scale_fill_manual("", values = c("SEAK pink catch" = "lightgrey")) +
   geom_hline(aes(yintercept=mean(fit_log)), linetype='dashed', color=c('grey30'))+
   theme_bw() +
   theme(
     legend.key = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     axis.text.x = element_text(size = 9, family = "Times New Roman"),
     legend.title = element_blank(),
     legend.position = "none") +
   geom_errorbar(
     aes(x = model,
       ymin = fit_log_LPI,  # lower bound
       ymax = fit_log_UPI), # upper bound
     width = 0.2,
     linewidth = 1,
     colour = "grey30") +
   scale_y_continuous(
     breaks = seq(0, 140, 10),
     limits = c(0, 140)) +
   labs(x = "",y = "2027 SEAK Pink Salmon Harvest Forecast (millions)") -> plot1
 ggsave(paste0(results.directory, "forecast_models_inter.png"), dpi = 500, height = 4, width = 10, units = "in")
 
 # create final table for report
 read.csv(file.path(results.directory,'model_summary_table2_inter.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE)  %>%
   arrange(MAPE5) %>%
   mutate(MAPE5 = round(MAPE5,1)) %>%
   mutate(change = AICc- min(AICc)) %>%
   dplyr::select(Terms, Model, Fit, Fit_LPI, Fit_UPI,AdjR2, MAPE5, change) %>%
   rename(AICc_change = change) %>%
   write.csv(., paste0(results.directory, "/model_summary_final_inter.csv"), row.names = F) 
 
 