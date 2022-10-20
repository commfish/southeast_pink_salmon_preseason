data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,'results', '/')

# best model based on performance metrics
# model one check
read.csv(file.path(data.directory,'var2022_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names
variables$CPUE <- variables$CPUEcal # Use CPUEcal as CPUE index
model.names <- c(m1='CPUE')
model.formulas <- c(SEAKCatch_log ~ CPUE) # temp. data


f_model_summary_retro <- function(model.formulas,model.names, year_num, best_model){
variables %>%
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch,	CPUEcal)) %>%
  dplyr::filter(JYear < year_num) -> log_data_subset

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
best_model<-m1
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(Harvest = round((exp(SEAKCatch_log)),2),
         Residuals = round((.resid),2),
         'Hat values' = round((.hat),2),
         'Cooks distance' = round((.cooksd),2),
         'Std. residuals' = round((.std.resid),2),
         fitted = round((.fitted),5),
         Year=1998:year_num,
         fit = exp(.fitted) * exp(0.5* sigma*sigma),
         'Fitted values' = round(fit,2),
         juvenile_year = 1997:(year_num-1), 
         model=model.names) %>%
  dplyr::select(Year,'Fitted values') %>%
  write.csv(file =paste0(results.directory, "year_", model,"_", year_num, ".csv"), row.names = F)}

f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2012, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2013, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2014, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2015, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2016, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2017, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2018, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2019, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2020, best_model = m1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2021, best_model = m1)

read.csv(file.path(results.directory,'year_m2_2012.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results1
read.csv(file.path(results.directory,'year_m2_2013.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results2
read.csv(file.path(results.directory,'year_m2_2014.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results3
read.csv(file.path(results.directory,'year_m2_2015.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results4
read.csv(file.path(results.directory,'year_m2_2016.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results5
read.csv(file.path(results.directory,'year_m2_2017.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results6
read.csv(file.path(results.directory,'year_m2_2018.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results7
read.csv(file.path(results.directory,'year_m2_2019.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results8
read.csv(file.path(results.directory,'year_m2_2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results9
read.csv(file.path(results.directory,'year_m2_2021.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results10
merge(results1, results2, by="Year", all=T) %>%
  merge(., results3, by="Year", all=T) %>% 
  merge(., results4, by="Year", all=T) %>%
  merge(., results5, by="Year", all=T) %>% 
  merge(., results6, by="Year", all=T) %>%
  merge(., results7, by="Year", all=T) %>% 
  merge(., results8, by="Year", all=T) %>%
  merge(., results9, by="Year", all=T) %>% 
  merge(., results10, by="Year", all=T)->x
