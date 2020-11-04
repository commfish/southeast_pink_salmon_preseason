# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 9/24/2020
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
library(dLagM) #MASE calc
library(ggplot2)
library(car)
library(ggfortify)
library(Hmisc)
library(dplyr)
library(extrafont)
library(ggrepel)
#extrafont::font_import()
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2021_forecast" 
year.data <- 2020  
year.data.one <- year.data - 1
sample_size <-  23 # number of data points in model
index <- "index4"
# need to change lines 58-59****
data.directory <- file.path(year.forecast, index, 'data', '/')
results.directory <- file.path(year.forecast, index, 'results', '/')
best.model <- m2 # this can be added after steps 1 and 2 after the best model is determined
last_year_data_cpue <- 2.147502256 # last year of data
last_year_data_ISTI <- 8.888254 # last year of data
source('2021_forecast/functions.r')

# STEP 1: DATA
# read in data
read.csv(file.path(data.directory,'SECMcatch2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> catch # update file names
read.csv(file.path(data.directory,'SECMvar2020.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables #update file names

# restructure the data 
variables$CPUE <- variables$CPUEcal # Use CPUEcal as CPUE index
n <- dim(variables)[1] # number of years including forecast year
variables %>% 
  mutate (SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch, CPUEcal, Pink_Peak)) -> log_data 

catch %>% 
  mutate(Pink = LN_Pink_Cal_pool) -> catch # variable 'Pink' is logged calibrated pink catch (same variable as in SECMvaryyyy.csv file)

# normal data check
eda.norm(log_data$SEAKCatch)# data is normal if the p-value is above 0.05.
eda.norm(log_data$SEAKCatch_log)
eda.norm(log_data$CPUE)# already ln(CPUE+1)
eda.norm(log_data$ISTI)

# subset data by peak month (using left_join) and generate list of catch by year (this is used for the bootstrap)
left_join(catch, variables, by = c("Year" = "JYear")) %>% 
  dplyr::select(-c(SEAKCatch, CPUEcal, ISTI, CPUE)) %>% 
  dplyr::filter(Month == Pink_Peak) -> cal.data

cal.data <- split(cal.data$Pink,cal.data$Year)
cal.data

# STEP #2: HARVEST MODELS AND SUMMARY STATS
# define model names and formulas
model.names<-c(m1='CPUE',
          m2='CPUE+ISTI')
model.formulas<-c(SEAKCatch_log ~ CPUE,
                 SEAKCatch_log ~ CPUE+ISTI) # temp. data 

# summary statistics and bootstrap of SEAK pink salmon harvest forecast models
seak.model.summary <- model.summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names)
#seak.boot.summary <- boot.summary(cpuedata=cal.data,variables=log_data,model.formulas=model.formulas,model.names=model.names)

# summary of model fits (i.e., coefficients, p-value)
log_data %>% 
  dplyr::filter(JYear < year.data) -> log_data_subset 

lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ CPUE + ISTI, data = log_data_subset) -> m2
lm(SEAKCatch_log ~ CPUE*ISTI, data = log_data_subset) -> m3

tidy(m1) -> m11
tidy(m2) -> m22
tidy(m3) -> m33
rbind(m11, m22) %>% 
rbind(., m33) %>% 
mutate(model = c('m1','m1','m2','m2','m2','m3','m3','m3',' m3')) %>% 
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(estimate = round(estimate,3),
         std.error = round(std.error,3),
         statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
write.csv(., paste0(results.directory, "/model_summary_table1.csv"), row.names = F)

augment(best.model) %>% 
  mutate(SEAKCatch = round((exp(SEAKCatch_log)),1),
        resid = round((.resid),3),
         hat_values = round((.hat),3),
         Cooks_distance = round((.cooksd),3),
         std_resid = round((.std.resid),3),
         fitted = round((.fitted),3),
         CPUE = round((CPUE),3),
         ISTI = round((ISTI),3),
         year=1998:year.data, 
        juvenile_year = 1997:year.data.one) %>%
  dplyr::select(year, juvenile_year,  SEAKCatch, CPUE, ISTI, resid, hat_values, Cooks_distance, std_resid, fitted) %>%
  write.csv(paste0(results.directory, "/model_summary_table3.csv"), row.names = F)
# leave one out cross validation (verify seak.model.summary)
# https://stats.stackexchange.com/questions/27351/compare-models-loccv-implementation-in-r
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

# MAPE calc. check (check that these match summary file) 3 leave one out cross validation method
log_data %>% 
  filter(JYear < year.data) -> log_data_subset
model_m1 <- train(SEAKCatch_log ~ CPUE, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m2 <- train(SEAKCatch_log ~ CPUE + ISTI, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))

# calculate MASE 
log_data %>% 
  filter(JYear < year.data) -> log_data_subset
model.m1 = lm(SEAKCatch_log ~ CPUE, data = log_data_subset)
model.m2 = lm(SEAKCatch_log ~ CPUE + ISTI, data = log_data_subset)
MASE(model.m1, model.m2) %>%
  dplyr::select(MASE)-> MASE

# add MASE to summary file
read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>% 
  dplyr::select(X, AdjR2, AICc, MAPE, MEAPE) %>%
  dplyr::rename(terms = 'X') %>% 
  mutate(model = ifelse(terms =="CPUE", 'm1', 'm2')) %>% 
  cbind(., MASE) %>%
  dplyr::select(model, terms, AdjR2, AICc, MAPE, MEAPE, MASE) %>%
  mutate(AdjR2 = round(AdjR2,3),
         AICc = round(AICc,0),
         MAPE = round(MAPE,3),
         MEAPE = round(MEAPE,3),
         MASE = round(MASE,3)) %>%
  write.csv(paste0(results.directory, "/model_summary_table2.csv"), row.names = F)

# STEP #3: PREDICT NEXT YEAR'S CATCH BASED ON BEST MODEL
# bootstrap
# http://rstudio-pubs-static.s3.amazonaws.com/24365_2803ab8299934e888a60e7b16113f619.html
# prediction m2
sigma<- sigma(best.model) # best model
CPUE <- last_year_data_cpue # last year of data
ISTI <- last_year_data_ISTI # last year of data
newdata <- data.frame(CPUE, ISTI)
predicted<-predict(model.m2, newdata, interval="prediction", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)
fit_value <- exp(predicted$fit)*exp(0.5*sigma*sigma) #adjustment for exp
lwr_pi <-  exp(predicted$lwr)*exp(0.5*sigma*sigma)
upr_pi <-  exp(predicted$upr)*exp(0.5*sigma*sigma)
fit_value
lwr_pi
upr_pi

# STEP #4: DIAGNOSTIC PLOTS OF BEST MODEL
# Diagnostics: test model assumptions (normality, linearity, residuals)
png(paste0(results.directory, "figs/general_diagnostics.png"))
autoplot(best.model)
dev.off()

outlierTest(best.model) #Bonferroni p-values (term # 16)
residualPlots(best.model) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
car::residualPlots(best.model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)

# cpue and catch
augment(best.model)%>% 
  ggplot(aes(x = CPUE, y = SEAKCatch_log)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  labs(y = "ln(Harvest)", x =  "ln(CPUE+1)") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 5, label="a)"),family="Times New Roman", colour="black", size=5) -> plot1

# temp and catch
augment(best.model)  %>% 
  ggplot(aes(x = ISTI, y = SEAKCatch_log)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = ISTI, fill = ISTI), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12), limits = c(7,12)) +
  labs(y = "ln(Harvest)", x =  "Temperature") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 7, y = 5, label="b)"),family="Times New Roman", colour="black", size=5)-> plot2
cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/cpue_temp.png"), dpi = 500, height = 3, width = 6, units = "in")

# residuals against covariate
augment(best.model) %>% 
  mutate(resid = (.std.resid)) %>% 
  ggplot(aes(x = CPUE, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  labs(y = "Standardized residuals", x =  "ln(CPUE+1)") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 4, label="a)"),family="Times New Roman", colour="black", size=5)-> plot1

# residuals against covariate
augment(best.model) %>% 
  mutate(resid = (.std.resid))%>% 
  ggplot(aes(x = ISTI, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = ISTI, fill = ISTI), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12), limits = c(7,12)) +
  labs(y = "Standardized residuals", x =  "Temperature") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(aes(x = 7, y = 4, label="b)"),family="Times New Roman", colour="black", size=5) -> plot2
cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/predicted.png"), dpi = 500, height = 3, width = 6, units = "in")

# residuals by year
augment(best.model) %>% 
  mutate(resid = (.std.resid),
         count = 1997:year.data.one)%>% 
  ggplot(aes(x = count, y = resid)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0, 1,2,3,4), limits = c(-4,4))+
  labs(y = "Standardized residuals", x =  "Juvenile year") + theme_bw () +theme(text = element_text(size=10),
                                                                   axis.text.x = element_text(angle=90, hjust=1),
                                                                   panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 1997, y = 4, label="a)"),family="Times New Roman", colour="black", size=5)-> plot2

# residuals against fitted
augment(best.model) %>% 
  mutate(resid = (.resid),
         fit = (.fitted)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = fit, fill = fit),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1))+
  scale_x_continuous(breaks = c(2,3,4,5), limits = c(2,5))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Residuals", x =  "Fitted values") +
  geom_text(aes(x = 2.1, y = 1, label="b)"),family="Times New Roman", colour="black", size=5)-> plot3
cowplot::plot_grid(plot2, plot3, align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/fitted.png"), dpi = 500, height = 3, width = 6, units = "in")

# Cook's distance plot
k <- 2 # predictors in model
level <- 4/(sample_size-k-1) # source: Ren et al. 2016

augment(best.model) %>% 
  mutate(cooksd = (.cooksd),
         count = (1997:year.data.one),
         name= ifelse(cooksd >level, count, "")) %>% 
  ggplot(aes(x = count, y = cooksd, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  geom_hline(yintercept = level, lty=2) +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1))+
  labs(y = "Cook's distance", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                      axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="a)"),family="Times New Roman", colour="black", size=5) -> plot4

# Leverage plot
p <- 3 # the number of parameters in the model including intercept
level <- 2*p/sample_size # source: Ren et al. 2016
# leverage plot
augment(best.model) %>% 
  mutate(hat= (.hat),
         count = 1997:year.data.one,
         name= ifelse(hat >0.27, count, "")) %>% # may need to adjust valeu; see hat value equation above
  ggplot(aes(x = count, y = hat, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = 0.26, lty=2) +
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  labs(y = "Hat-values", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                      axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="b)"),family="Times New Roman", colour="black", size=5)-> plot5
cowplot::plot_grid(plot4, plot5,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/influential.png"), dpi = 500, height = 3, width = 6, units = "in")

# plot of harvest by year with prediction error 
augment(best.model) %>% 
  mutate(year = 1998:year.data, 
         catch = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma))  %>%
  as.data.frame() %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = catch, fill = "SEAK pink catch"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = fit, colour = "fit"), linetype = 1, size = 0.75) +
  scale_colour_manual("", values=c("SEAK pink catch" = "lightgrey", "fit" = "black")) +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal",
                     panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                         text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                                         axis.title.y = element_text(size=9, colour="black",family="Times New Roman"),
                                         axis.title.x = element_text(size=9, colour="black",family="Times New Roman"),
                     legend.position=c(0.9,0.9)) +
  geom_point(x=year.data +1, y=fit_value, pch=21, size=3, colour = "black", fill="grey") +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  #geom_text(aes(x = 1998, y = 140, label="a)"),family="Times New Roman", colour="black", size=5)+
  geom_segment(aes(x = year.data + 1, y = lwr_pi, yend = upr_pi, xend = year.data + 1), size=1, colour="black", lty=1)  -> plot1

# plot of observed harvest by fitted values (with one to one line)
augment(best.model) %>% 
  mutate(year = 1998:2020, 
         catch = exp(SEAKCatch_log), 
         sigma = .sigma,
         fit = exp(.fitted) * exp(0.5*sigma*sigma)) %>%
  ggplot(aes(x=fit, y=catch)) +
  geom_point(aes(y = catch), colour = "black", size = 1) +
  scale_color_grey() +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                         axis.title.y = element_text(size=9, colour="black",family="Times New Roman"),
                                         axis.title.x = element_text(size=9, colour="black",family="Times New Roman")) +
  theme(legend.position="none") + theme(legend.title=element_blank())+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  geom_abline(intercept = 0, lty=3) +  
  #geom_text_repel( aes(x = fit, y = catch, label = year),
   #                                                    nudge_x = 1, size = 4, show.legend = FALSE) + 
  labs(y = "Observed SEAK Pink Salmon Harvest (millions)", x = "Predicted SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  #geom_text(aes(x = 2, y = 140, label="b)"),family="Times New Roman", colour="black", size=5) +
  geom_text(aes(y = 101, x = 57, label="2013"),family="Times New Roman", colour="black", size=4) +
  geom_text(aes(y = 46, x = 23, label="1998"),family="Times New Roman", colour="black", size=4) +
  geom_text(aes(y = 8, x = 19, label="2018"),family="Times New Roman", colour="black", size=4) +
  geom_text(aes(y = 30, x = 62, label="2015"),family="Times New Roman", colour="black", size=4)-> plot2
cowplot::plot_grid(plot1, align = "vh", nrow = 1, ncol=1)
ggsave(paste0(results.directory, "figs/catch_plot_pred_a.png"), dpi = 500, height = 3, width = 6, units = "in")
cowplot::plot_grid(plot2, align = "vh", nrow = 1, ncol=1)
ggsave(paste0(results.directory, "figs/catch_plot_pred_b.png"), dpi = 500, height = 3, width = 6, units = "in")
# model average (not sure how to do prediction interval on model averaged linear regressions)**
# not currently used 
#fit.avg <- model.avg(model.m1, model.m2)
#predicted<-predict(fit.avg, variables[24,], se.fit = TRUE)
#lower_CI <- predicted$fit - 1.96*predicted$se.fit
#upper_CI <- predicted$fit + 1.96*predicted$se.fit

# Cook's Distance cut-offs
#https://stats.stackexchange.com/questions/22161/how-to-read-cooks-distance-plots
#https://www.stat.berkeley.edu/~spector/s133/Lr1.html
#http://rstudio-pubs-static.s3.amazonaws.com/477250_8b19e334ad1245c9b9259e9c5db36089.html

#Here are the guidelines commonly used:https://online.stat.psu.edu/stat462/node/173/
#If Di is greater than 0.5, then the ith data point is worthy of further investigation as it may be influential. 
#If Di is greater than 1, then the ith data point is quite likely to be influential. 
#Or, if Di sticks out like a sore thumb from the other Di values, it is almost certainly influential.
#level <- qf(.10, df1=3, df2=20) # F distribution of p (p=3 for two predictors in the model) and n-p; Cook's distance cut-off; pg.382 in Neter et al. 1996
#level <- 4/n
# Rough rule of thumb: Cook’s distance is large if Di > 4/(n−p−1); https://stat.ethz.ch/education/semesters/FS_2008/regression/7-Diagnostics.pdf
#level <- 4/(n-p) # source: Bollen et al (1990) as cited in  Jayakumar and A.Sulthan 2015 
# Bollen KA, Jackman RW. Regression diagnostics: An expository treatment of outliers and influential cases. Sociological Methods & Research. 1985 May;13(4):510-42.
#level <- 0.5



