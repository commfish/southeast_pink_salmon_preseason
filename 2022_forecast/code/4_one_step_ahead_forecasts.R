# source code and functions
source('2022_forecast/code/1_summarize_models.r')
source('2022_forecast/code/functions.r')
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# end year is the year the data is used through (e.g., end = 2004 means that the regression is runs through JYear 2014 and years 2015-2019 are
# forecasted in the one step ahead process)
seak_model_summary <- f_model_one_step_ahead_multiple(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = 2014)

read.csv(file.path(results.directory,'seak_model_summary_one_step_ahead.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
read.csv(file.path(results.directory,'model_summary_table2.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> model_summary_table2
results %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(MAPE) %>%
  mutate(MAPE_one_step_ahead = round(MAPE,3)) %>%
  dplyr::select(MAPE_one_step_ahead) %>%
  cbind(., model_summary_table2) %>%
  dplyr::select(model, AdjR2,  AICc,  MASE, wMAPE, MAPE_LOOCV, MAPE_one_step_ahead) %>%
  write.csv(paste0(results.directory, "/model_summary_table5.csv"), row.names = F)