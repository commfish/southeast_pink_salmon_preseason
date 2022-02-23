# source code and functions
source('FMS_writeup/code/1_summarize_models.r')
source('FMS_writeup/code/functions.r')
# https://www.mm218.dev/posts/2021/01/model-averaging/
# Mahoney (2021, Jan. 18). Mike Mahoney: Model averaging methods: how and why to build ensemble models. Retrieved from https://www.mm218.dev/posts/2021/01/model-averaging/
# https://rdrr.io/github/padpadpadpad/rTPC/f/vignettes/model_averaging_selection.Rmd
# https://stats.stackexchange.com/questions/155305/how-does-r-calculate-prediction-intervals-in-the-forecast-package
# https://www.investopedia.com/ask/answers/042415/what-difference-between-standard-error-means-and-standard-deviation.asp


# inverse variance (MAPE) weighting (all models included)
 read.csv(file.path(results.directory,'model_summary_table5.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
   dplyr::select(model, AdjR2, inv_var) %>% 
   mutate(weight = inv_var/sum(inv_var)) -> summary_table # scale inv_var to one

read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(terms, fit,	se.fit, fit_LPI,	fit_UPI, sigma, df) %>%
  cbind(., summary_table) %>%
  mutate(sigma = round(sigma,3),
         stdev = (se.fit * sqrt(df +1))) %>%
  mutate(var_yhat = stdev * stdev)  %>% 
  mutate(fit_bias_corrected = fit+((sigma*sigma)/2)) %>%
  mutate(weight_pred = sum(fit_bias_corrected*weight)) %>%
  mutate(step1 = ((fit_bias_corrected - weight_pred)^2) + var_yhat) %>%
  mutate(step2 = sqrt(step1) * weight) %>%
  mutate(se_y_pred = sum(step2)) %>%
  mutate(fit_LPI_80 = (weight_pred)-(1.28*se_y_pred),
         fit_UPI_80 = (weight_pred)+(1.28*se_y_pred)) %>%
  mutate(exp_fit = exp(weight_pred),
         exp_fit_LPI_80 = exp(fit_LPI_80),
         exp_fit_UPI_80 = exp(fit_UPI_80)) %>%
  write.csv(paste0(results.directory, "/inverse_variance.csv"), row.names = F)

# equal weighting top models by leave one out MAPE (only include models where MAPE <0.14)
read.csv(file.path(results.directory,'model_summary_table5.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
  dplyr::select(model, AdjR2, MAPE_one_step_ahead) -> summary_table

read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(terms, fit,	se.fit, fit_LPI,	fit_UPI, AICc,	sigma, df) %>%
  cbind(., summary_table) %>%
  filter(MAPE_one_step_ahead < 0.14) %>%
  mutate(sigma = round(sigma,3),
         stdev = (se.fit * sqrt(df +1))) %>%
  mutate(var_yhat = stdev * stdev)  %>% 
  mutate(fit_bias_corrected = fit+((sigma*sigma)/2)) %>%
  mutate(weight = 1/length(sigma)) %>%
  mutate(weight_pred = sum(fit_bias_corrected*weight)) %>%
  mutate(step1 = ((fit_bias_corrected - weight_pred)^2) + var_yhat) %>%
  mutate(step2 = sqrt(step1) * weight) %>%
  mutate(se_y_pred = sum(step2)) %>%
  mutate(fit_LPI_80 = (weight_pred)-(1.28*se_y_pred),
         fit_UPI_80 = (weight_pred)+(1.28*se_y_pred)) %>%
  mutate(exp_fit = exp(weight_pred),
         exp_fit_LPI_80 = exp(fit_LPI_80),
         exp_fit_UPI_80 = exp(fit_UPI_80)) %>%
  write.csv(paste0(results.directory, "/MAPE_equal_weighting.csv"), row.names = F)

# AICc weights
read.csv(file.path(results.directory,'model_summary_table5.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
  dplyr::select(model, AdjR2, MAPE_one_step_ahead) -> summary_table

read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(terms, fit,	se.fit, fit_LPI,	fit_UPI, AICc,	sigma, df) %>%
  cbind(., summary_table) %>%
  mutate(min_AICc=min(AICc))%>%
  mutate(delta_AICc = AICc-min_AICc)%>%
  filter(delta_AICc <= 4) %>%
  mutate(sigma = round(sigma,3),
         stdev = (se.fit * sqrt(df +1))) %>%
  mutate(var_yhat = stdev * stdev)  %>% 
  mutate(fit_bias_corrected = fit+((sigma*sigma)/2)) %>%
  mutate(weight = 1/length(sigma)) %>%
  mutate(weight_pred = sum(fit_bias_corrected*weight)) %>%
  mutate(step1 = ((fit_bias_corrected - weight_pred)^2) + var_yhat) %>%
  mutate(step2 = sqrt(step1) * weight) %>%
  mutate(se_y_pred = sum(step2)) %>%
  mutate(fit_LPI_80 = (weight_pred)-(1.28*se_y_pred),
         fit_UPI_80 = (weight_pred)+(1.28*se_y_pred)) %>%
  mutate(exp_fit = exp(weight_pred),
         exp_fit_LPI_80 = exp(fit_LPI_80),
         exp_fit_UPI_80 = exp(fit_UPI_80)) %>%
  write.csv(paste0(results.directory, "/AICc_weighting.csv"), row.names = F)