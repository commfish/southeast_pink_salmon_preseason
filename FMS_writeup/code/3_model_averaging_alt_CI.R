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
               m2='CPUE + ISTI20_MJJ',
               m3='CPUE + Chatham_SST_May',
               m4='CPUE + Chatham_SST_MJJ',
               m5='CPUE + Chatham_SST_AMJ',
               m6='CPUE + Chatham_SST_AMJJ',
               m7='CPUE + Icy_Strait_SST_May',
               m8='CPUE + Icy_Strait_SST_MJJ',
               m9='CPUE + Icy_Strait_SST_AMJ',
               m10='CPUE + Icy_Strait_SST_AMJJ',
               m11='CPUE + NSEAK_SST_May',
               m12='CPUE + NSEAK_SST_MJJ',
               m13='CPUE + NSEAK_SST_AMJ',
               m14='CPUE + NSEAK_SST_AMJJ',
               m15='CPUE + SEAK_SST_May',
               m16='CPUE + SEAK_SST_MJJ',
               m17='CPUE + SEAK_SST_AMJ',
               m18='CPUE + SEAK_SST_AMJJ')
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

# summary statistics and bootstrap of SEAK pink salmon harvest forecast models
seak_model_summary <-f_model_summary_model_average(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, w = log_data$weight_values)

read.csv(file.path(results.directory,'seak_model_summary_hindcasts.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(-c(AdjR2, AICc, p.value, X1:X19, R2, sigma, terms)) %>% 
  rename('Y2021'=X24) %>% 
  rename('Y2020'=X23) %>% 
  rename('Y2019'=X22) %>% 
  rename('Y2018'=X21) %>% 
  rename('Y2017'=X20) %>% 
  mutate(weight = 1/length(model))%>% 
  mutate(F2017 = sum(Y2017*weight),
         F2018 = sum(Y2018*weight),
         F2019 = sum(Y2019*weight),
         F2020 = sum(Y2020*weight),
         F2021 = sum(Y2021*weight))%>%  #https://www.statology.org/gather-function-in-r/
  dplyr::select(-c(Y2017, Y2018, Y2019, Y2020, Y2021, weight)) %>% 
  mutate(se_y_pred = (log(34.7)-F2017)^2+(log(8.1)-F2018)^2+(log(21.1)-F2019)^2+(log(8.0679)-F2020)^2+(log(48.4)-F2021)^2) %>% # sample standard deviation
  dplyr::select(-c(F2017, F2018, F2019, F2020, F2021, model)) ->df3


# model averaging process
read.csv(file.path(results.directory,'model_summary_table4.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::select(model, AdjR2) %>%
  mutate(weight = 1/length(model)) -> summary_table # scale inv_var to one

read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) %>%
  dplyr::rename(terms = 'X') %>%
  dplyr::select(terms, fit,	se.fit, fit_LPI,	fit_UPI, sigma, df) %>%
  cbind(., summary_table) %>%
  mutate(sigma = round(sigma,3),
         stdev = (se.fit * sqrt(df +1))) %>%
  mutate(fit_bias_corrected = fit+((sigma*sigma)/2)) %>%
  mutate(weight_pred = sum(fit_bias_corrected*weight)) %>%
  cbind(., df3) %>%
  mutate(fit_LPI_80 = (weight_pred)-(1.28*se_y_pred),
         fit_UPI_80 = (weight_pred)+(1.28*se_y_pred)) %>%
  mutate(exp_fit = exp(weight_pred),
         exp_fit_LPI_80 = exp(fit_LPI_80),
         exp_fit_UPI_80 = exp(fit_UPI_80)) %>%
  write.csv(paste0(results.directory, "/equal_weighting_2008_CI.csv"), row.names = F)