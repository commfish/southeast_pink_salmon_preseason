# the forecasts.csv file is created from the spreadsheet model_summary_table_month_year.xlsx in the folder results/summary_tables and the model_summary_table2 for the forecast value
# the forecasts.csv file needs to be manually created from the excel spreadsheet 
read.csv(file.path(data.directory,'forecasts.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> forecasts

# model m1 retro
read.csv(file.path(data.directory,'var2023_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names
model.names <- c(m1='CPUE')
model.formulas <- c(SEAKCatch_log ~ CPUE) # temp. data
model<-'m1'

# function to create hindcasts from each model
f_model_summary_retro <- function(model.formulas,model.names, year_num, best_model, yearminus){
  variables %>%
    mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
    dplyr::select(-c(SEAKCatch)) %>%
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
           sigma=sigma,
           Year=1998:year_num,
           fit = exp(.fitted) * exp(0.5* sigma*sigma),
           fitted_values = round(fit,2),
           juvenile_year = 1997:(year_num-1), 
           type = 'estimate',
           year_minus = yearminus,
           model_name=model) %>%
    dplyr::select(Year,fitted_values, year_minus, type, model_name) %>% 
    write.csv(file =paste0(results.directory.retro, "year_", model,"_", year_num, ".csv"), row.names = F)}

f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2013, best_model = m1, yearminus = 10)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2014, best_model = m1, yearminus = 9)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2015, best_model = m1, yearminus = 8)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2016, best_model = m1, yearminus = 7)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2017, best_model = m1, yearminus = 6)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2018, best_model = m1, yearminus = 5)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2019, best_model = m1, yearminus = 4)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2020, best_model = m1, yearminus = 3)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2021, best_model = m1, yearminus = 2)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2022, best_model = m1, yearminus = 1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2023, best_model = m1, yearminus = 0)

read.csv(file.path(results.directory.retro,'year_m1_2013.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results1
read.csv(file.path(results.directory.retro,'year_m1_2014.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results2
read.csv(file.path(results.directory.retro,'year_m1_2015.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results3
read.csv(file.path(results.directory.retro,'year_m1_2016.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results4
read.csv(file.path(results.directory.retro,'year_m1_2017.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results5
read.csv(file.path(results.directory.retro,'year_m1_2018.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results6
read.csv(file.path(results.directory.retro,'year_m1_2019.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results7
read.csv(file.path(results.directory.retro,'year_m1_2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results8
read.csv(file.path(results.directory.retro,'year_m1_2021.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results9
read.csv(file.path(results.directory.retro,'year_m1_2022.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results10
read.csv(file.path(results.directory.retro,'year_m1_2023.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results11
rbind(results1, results2, results3, results4, results5,results6, results7, results8, results9, results10, results11) -> df1

# model m2 retro
# create hindcasts from m2 
read.csv(file.path(data.directory,'var2023_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names
model.names <- c(m2='CPUE + ISTI20_MJJ')
model.formulas <- c(SEAKCatch_log ~ CPUE+ISTI20_MJJ) # temp. data
model<-'m2'

f_model_summary_retro <- function(model.formulas,model.names, year_num, best_model, yearminus){
variables %>%
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch)) %>%
  dplyr::filter(JYear < year_num) -> log_data_subset

lm(SEAKCatch_log ~ CPUE+ISTI20_MJJ, data = log_data_subset) -> m2
best_model<-m2
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(Harvest = round((exp(SEAKCatch_log)),2),
         Residuals = round((.resid),2),
         'Hat values' = round((.hat),2),
         'Cooks distance' = round((.cooksd),2),
         'Std. residuals' = round((.std.resid),2),
         fitted = round((.fitted),5),
         sigma=sigma,
         Year=1998:year_num,
         fit = exp(.fitted) * exp(0.5* sigma*sigma),
         fitted_values = round(fit,2),
         juvenile_year = 1997:(year_num-1), 
         type = 'estimate',
         year_minus = yearminus,
         model_name=model) %>%
  dplyr::select(Year,fitted_values, year_minus, type, model_name) %>% 
  write.csv(file =paste0(results.directory.retro, "year_", model,"_", year_num, ".csv"), row.names = F)}

f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2013, best_model = m2, yearminus = 10)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2014, best_model = m2, yearminus = 9)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2015, best_model = m2, yearminus = 8)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2016, best_model = m2, yearminus = 7)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2017, best_model = m2, yearminus = 6)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2018, best_model = m2, yearminus = 5)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2019, best_model = m2, yearminus = 4)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2020, best_model = m2, yearminus = 3)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2021, best_model = m2, yearminus = 2)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2022, best_model = m2, yearminus = 1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2023, best_model = m2, yearminus = 0)

read.csv(file.path(results.directory.retro,'year_m2_2013.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results1
read.csv(file.path(results.directory.retro,'year_m2_2014.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results2
read.csv(file.path(results.directory.retro,'year_m2_2015.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results3
read.csv(file.path(results.directory.retro,'year_m2_2016.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results4
read.csv(file.path(results.directory.retro,'year_m2_2017.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results5
read.csv(file.path(results.directory.retro,'year_m2_2018.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results6
read.csv(file.path(results.directory.retro,'year_m2_2019.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results7
read.csv(file.path(results.directory.retro,'year_m2_2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results8
read.csv(file.path(results.directory.retro,'year_m2_2021.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results9
read.csv(file.path(results.directory.retro,'year_m2_2022.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results10
read.csv(file.path(results.directory.retro,'year_m2_2023.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results11
rbind(results1, results2, results3, results4, results5,results6, results7, results8, results9, results10, results11) -> df2

# model m19 retro
# create hindcasts from m19 
read.csv(file.path(data.directory,'var2023_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names
model.names <- c(m19 = 'CPUE + ISTI20_MJJ + zoo_density_June + condition_July')
model.formulas <- c(SEAKCatch_log ~ CPUE + ISTI20_MJJ + zoo_density_June + condition_July) # temp. data
model<-'m19'

f_model_summary_retro <- function(model.formulas,model.names, year_num, best_model, yearminus){
  variables %>%
    mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
    dplyr::select(-c(SEAKCatch)) %>%
    dplyr::filter(JYear < year_num) -> log_data_subset
  
  lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ + zoo_density_June + condition_July, data = log_data_subset) -> m19
  best_model<-m19
  as.numeric(sigma(best_model))-> sigma
  augment(best_model) %>% 
    mutate(Harvest = round((exp(SEAKCatch_log)),2),
           Residuals = round((.resid),2),
           'Hat values' = round((.hat),2),
           'Cooks distance' = round((.cooksd),2),
           'Std. residuals' = round((.std.resid),2),
           fitted = round((.fitted),5),
           sigma=sigma,
           Year=1998:year_num,
           fit = exp(.fitted) * exp(0.5* sigma*sigma),
           fitted_values = round(fit,2),
           juvenile_year = 1997:(year_num-1),
           type = 'estimate',
           year_minus = yearminus,
           model_name=model) %>%
    dplyr::select(Year,fitted_values, year_minus, type, model_name) %>% 
    write.csv(file =paste0(results.directory.retro, "year_", model,"_", year_num, ".csv"), row.names = F)}

f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2013, best_model = m19, yearminus = 10)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2014, best_model = m19, yearminus = 9)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2015, best_model = m19, yearminus = 8)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2016, best_model = m19, yearminus = 7)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2017, best_model = m19, yearminus = 6)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2018, best_model = m19, yearminus = 5)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2019, best_model = m19, yearminus = 4)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2020, best_model = m19, yearminus = 3)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2021, best_model = m19, yearminus = 2)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2022, best_model = m19, yearminus = 1)
f_model_summary_retro(model.formulas=model.formulas,model.names=model.names, year_num=2023, best_model = m19, yearminus = 0)

# combine the results of model m1, m2, and m19 for comparison
read.csv(file.path(results.directory.retro,'year_m19_2013.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results1
read.csv(file.path(results.directory.retro,'year_m19_2014.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results2
read.csv(file.path(results.directory.retro,'year_m19_2015.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results3
read.csv(file.path(results.directory.retro,'year_m19_2016.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results4
read.csv(file.path(results.directory.retro,'year_m19_2017.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results5
read.csv(file.path(results.directory.retro,'year_m19_2018.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results6
read.csv(file.path(results.directory.retro,'year_m19_2019.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results7
read.csv(file.path(results.directory.retro,'year_m19_2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results8
read.csv(file.path(results.directory.retro,'year_m19_2021.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results9
read.csv(file.path(results.directory.retro,'year_m19_2022.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results10
read.csv(file.path(results.directory.retro,'year_m19_2023.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) ->results11
rbind(results1, results2, results3, results4, results5,results6, results7, results8, results9, results10, results11, df1, df2) %>%
  group_by(year_minus) %>% 
  mutate(`Forecast model` = as.character(max(Year))) %>% 
  ungroup() %>% 
  mutate(`Forecast model` = fct_rev(factor(`Forecast model`)),
         label = ifelse(type == "forecast", 
                        prettyNum(matbio_tons, big.mark = ",", digits = 1), NA)) -> df # data set for figures


# plot of model hindcasts (m1, m2, m11) with the predictions based on one-step-ahead forecasts
augment(m19) %>% 
  mutate(year = as.numeric(1998:year.data), 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 10), aes(x = (Year), y = fitted_values, 
                           colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus == 10), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  #geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_10.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-8, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 9), aes(x = Year, y = fitted_values, 
                                                        colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus == 9), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_9.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-7, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 8), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==8), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_8.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-6, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 7), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==7), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_7.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-5, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 6), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==6), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_6.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-4, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 5), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==5), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_5.png"), dpi = 500, height = 3, width = 6, units = "in")  


augment(m19) %>% 
  mutate(year = 1998:year.data-3, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 4), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==4), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  #geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_4.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-2, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 3), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==3), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_3.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data-1, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 2), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==2), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8))  +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_2.png"), dpi = 500, height = 3, width = 6, units = "in")  

augment(m19) %>% 
  mutate(year = 1998:year.data, 
         harvest = exp(SEAKCatch_log)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(data = df %>% filter(year_minus == 1), aes(x = Year, y = fitted_values, 
                                                       colour = model_name, linetype = model_name), size= 0.75) +
  geom_point(data = forecasts %>% filter(year_minus ==1), 
             aes(x = Year, y = fitted_values, colour = model_name, shape =model_name), size=2) +
  scale_shape_manual(values =c(15,8,16)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times"),
                     axis.title.x = element_text(size=9, colour="black",family="Times"),
                     legend.position=c(0.5,0.8)) +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/year_minus_1.png"), dpi = 500, height = 3, width = 6, units = "in")  

# MAPE figure
augment(m19) %>% 
  mutate(year = 1998:year.data, 
         harvest = exp(SEAKCatch_log)) %>%
  filter(year>2013)%>%
  #mutate(year = as.factor(year))%>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
   geom_point(data = forecasts, 
             aes(x = Year, y = fitted_values, colour = forecast_model_name, shape =forecast_model_name), size=3) +
  geom_line(data = forecasts, 
             aes(x = Year, y = fitted_values, colour = forecast_model_name, linetype = forecast_model_name), size=0.75) +
  scale_shape_manual(values =c(16,8,2)) +
  scale_colour_manual(values =c("black", "darkgrey", "black")) +
  scale_linetype_manual(values=c("dotted", "solid", "dashed")) + 
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal", panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=11),
                     axis.title.y = element_text(size=11, colour="black",family="Times"),
                     axis.title.x = element_text(size=11, colour="black",family="Times"),
                     legend.position=c(0.5,0.80)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank()) +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024), limits = c(2013, 2024))+ 
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") 
ggsave(paste0(results.directory.retro, "figs/MAPE_forecasts.png"), dpi = 500, height = 3, width = 7, units = "in")  
