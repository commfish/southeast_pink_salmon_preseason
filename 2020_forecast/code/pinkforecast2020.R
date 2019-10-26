# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19
# adapted by Sara Miller 10/25/19

# load----
devtools::install_github("ben-williams/FNGr")
library("FNGr")
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
source('2020_forecast/code/functions.r')

# data----
SECM2019<-read.csv("2020_forecast/data/SECMcatch2019.csv")
variables<-read.csv("2020_forecast/data/SECMvar2019.csv")

# analysis----
variables$CPUE<-variables$CPUEcal #Use CPUEcal as CPUE index
n <- dim(variables)[1] #number of years including forecast year
variables %>% 
  mutate_at(vars(-JYear), funs(log = log(.)))-> log_data

SECM2019 %>% 
  mutate(Pink=log(Pink))-> SECM2019

# subset data by peak month and generate list of catch by year
cal.data <- SECM2019[SECM2019$Pink_Peak,]
cal.data <- split(cal.data$Pink,cal.data$Year)

# 2020 SE Pink salmon harvest models
# define model names and formulas
model.names<-c(m1='CPUE',
          m2='CPUE+ISTI_MJJ',
          m3='CPUE+ISTI_MJJ+CPUE:ISTI_MJJ')
model.formulas<-c(SEAKCatch_log ~ CPUE_log,
                 SEAKCatch_log ~ CPUE_log+ISTI_MJJ_log,
                 SEAKCatch_log ~ CPUE_log*ISTI_MJJ_log)

# summary statistics SEAK pink salmon harvest forecast models
seak.model.summary <- model.summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names)
#seak.boot.summary <- boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas,model.names=model.names)

# summary of model fits (i.e., coefficients, p-value)
log_data %>% 
  dplyr::filter(JYear<2019) %>% 
  do(m1 = lm(SEAKCatch_log ~ CPUE_log, data = .),
     m2 = lm(SEAKCatch_log ~ CPUE_log + ISTI_MJJ_log, data = .),
     m3 = lm(SEAKCatch_log ~ CPUE_log*ISTI_MJJ_log, data = .)) -> lm_out_seak
lm_out_seak %>% 
  tidy(m1) -> m1
lm_out_seak %>% 
  tidy(m2) -> m2
lm_out_seak %>% 
  tidy(m3) -> m3
rbind(m1, m2) %>% 
rbind(., m3) %>% 
write.csv(., "2020_forecast/results/model_summary_table1.csv")

# leave one out cross validation (verify seak.model.summary)
# https://stats.stackexchange.com/questions/27351/compare-models-loccv-implementation-in-r
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
log_data %>% 
  filter(JYear<2019) -> log_data_subset
model_m1 <- train(SEAKCatch_log ~ CPUE_log, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m2 <- train(SEAKCatch_log ~ CPUE_log + ISTI_MJJ_log, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m3 <- train(SEAKCatch_log ~ CPUE_log * ISTI_MJJ_log, data = log_data_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))

# MASE calculation
log_data %>% 
  filter(JYear<2019) -> log_data_subset
model.m1 = lm(SEAKCatch_log ~ CPUE_log, data = log_data_subset)
model.m2 = lm(SEAKCatch_log ~ CPUE_log + ISTI_MJJ_log, data = log_data_subset)
model.m3 = lm(SEAKCatch_log ~ CPUE_log*ISTI_MJJ_log, data = log_data_subset)
MASE(model.m1, model.m2) %>%
  dplyr::select(MASE)-> MASE

results<-read.csv("2020_forecast/results/seak_model_summary.csv")
results %>% 
  dplyr::select(X, AdjR2, AICc, MAPE, MEAPE) %>%
  dplyr::rename(model = 'X') %>% 
  filter(model!='CPUE+ISTI_MJJ+CPUE:ISTI_MJJ' ) %>%
  cbind(., MASE) %>%
  dplyr::select(model, AdjR2, AICc, MAPE, MEAPE, MASE) %>%
  write.csv(., "2020_forecast/results/model_summary_table2.csv")

# bootstrap
# http://rstudio-pubs-static.s3.amazonaws.com/24365_2803ab8299934e888a60e7b16113f619.html
sigma<- sigma(model.m2)
CPUE_log <- log(1.202607)
ISTI_MJJ_log <- log(9.91121125)
newdata <- data.frame(CPUE_log, ISTI_MJJ_log)
#bootfit1 <- Boot(model.m2, function(SEAKCatch_log)predict(SEAKCatch_log, newdata), R=10000)
predicted<-predict(model.m2, newdata, interval="prediction", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)
fit_value <- exp(predicted$fit)*exp(0.5*sigma*sigma)
lwr_pi <-  exp(predicted$lwr)*exp(0.5*sigma*sigma)
upr_pi <-  exp(predicted$upr)*exp(0.5*sigma*sigma)

# Diagnostics: test model assumptions (normality, linearity, residuals)
# diagnostic plots
png("2020_forecast/results/figs/general_diagnostics.png")
autoplot(model.m2)
dev.off()

outlierTest(model.m2) #Bonferroni p-values (term # 16)
residualPlots(model.m2) #lack-of fit curvature test; terms that are non-significant suggest a properly speciified model
car::residualPlots(model.m2, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)

# residuals against covariate
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(resid = (.std.resid)) %>% 
  ggplot(aes(x = CPUE_log, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = CPUE_log, fill = CPUE_log), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4,5), limits = c(-4,5)) +
  scale_x_continuous(breaks = c(-2, -1, 0,1,2), limits = c(-2,2)) +
  labs(y = "Standardized residuals", x =  "ln(CPUE)") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = -2, y = 5, label="a)"),family="Times New Roman", colour="black", size=5)-> plot1

# residuals against covariate
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(resid = (.std.resid))%>% 
  ggplot(aes(x = ISTI_MJJ_log, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = ISTI_MJJ_log, fill = ISTI_MJJ_log), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4,5), limits = c(-4,5)) +
  labs(y = "Standardized residuals", x =  "ln(ISTI_MJJ)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(aes(x = 2, y = 4, label="b)"),family="Times New Roman", colour="black", size=5) -> plot2
cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
ggsave("2020_forecast/results/figs/predicted.png", dpi = 500, height = 3, width = 6, units = "in")

# residuals by year
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(resid = (.std.resid),
         count = 1997:2018)%>% 
  ggplot(aes(x = count, y = resid)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  scale_x_continuous(breaks = 1997:2018, labels = 1997:2018) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0, 1,2,3,4, 5), limits = c(-4,5))+
  labs(y = "Standardized residuals", x =  "Juvenile year") + theme_bw () +theme(text = element_text(size=10),
                                                                   axis.text.x = element_text(angle=90, hjust=1),
                                                                   panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 1997, y = 5, label="a)"),family="Times New Roman", colour="black", size=5)-> plot2

# residuals against fitted
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(resid = (.resid),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = fit, fill = fit),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-1, -0.50, 0, 0.5, 1), limits = c(-1,1))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Residuals", x =  "Fitted values (millions)") +
  geom_text(aes(x = 0, y = 1, label="b)"),family="Times New Roman", colour="black", size=5)-> plot3
cowplot::plot_grid(plot2, plot3, align = "vh", nrow = 1, ncol=2)
ggsave("2020_forecast/results/figs/fitted.png", dpi = 500, height = 3, width = 6, units = "in")

qf(.50, df1=4, df2=18) # F distribution of p+1 (4) and n-p-1 (22-3-1); Cook's distance cut-off
# Cook's distance plot
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(cooksd = (.cooksd),
         count = 1997:2018,
         name= ifelse(cooksd >0.87, count, ""))%>% 
  ggplot(aes(x = count, y = cooksd, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  geom_hline(yintercept = 0.87, lty=2) +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1997:2018, labels = 1997:2018) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1))+
  labs(y = "Cook's distance", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                      axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="a)"),family="Times New Roman", colour="black", size=5)-> plot4

# hat value 2*p/n = 2*(3/22); cut-off value
# leverage plot
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(hat= (.hat),
         count = 1997:2018,
         name= ifelse(hat >0.27, count, "")) %>% 
  ggplot(aes(x = count, y = hat, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = 0.27, lty=2) +
  scale_x_continuous(breaks = axisb$breaks, labels = axisb$labels)  +
  labs(y = "Hat-values", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                      axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="b)"),family="Times New Roman", colour="black", size=5)-> plot5

cowplot::plot_grid(plot4, plot5,  align = "vh", nrow = 1, ncol=2)
ggsave("2020_forecast/results/figs/influential.png", dpi = 500, height = 3, width = 6, units = "in")

# plot of harvest by year with prediction error 
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(year = 1998:2019, 
         catch = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) -> m2
m2 %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = catch, colour = " SEAK pink catch"),
           stat = "identity",  
           fill = "lightgrey",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(aes(x=year, y = fit), linetype = 2, colour = "black", size = 1) +
  scale_color_grey() +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                         text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1)) +
  theme(legend.position="none") +geom_point(x=2020, y=fit_value, pch=8, size=2) +
  scale_x_continuous(breaks = seq(1998, 2020, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100), limits = c(0,100))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "Harvest (millions)\n", linetype = NULL, fill = NULL) +
  geom_segment(aes(x = 2020, y = lwr_pi, yend = upr_pi, xend = 2020), size=1, colour="black", lty=1) +
  geom_text(aes(x = 1998, y = 100, label="a)"),family="Times New Roman", colour="black", size=5)-> plot1

# plot of observed harvest by fitted values (with one to one line)
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(year = 1997:2018, 
         catch = exp(SEAKCatch_log), 
         sigma = .sigma,
         fit = exp(.fitted) * exp(0.5*sigma*sigma))  -> m2
axisb <- tickr(m2, year, 2)
m2 %>%
  ggplot(aes(x=fit, y=catch)) +
  geom_point() +
  geom_point(aes(y = catch), colour = "black", size = 1) +
  scale_color_grey() +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") + theme(legend.title=element_blank())+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100))+
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100))+geom_abline(intercept = 0) +
  labs(y = "Harvest (millions)\n", x = "Fitted (millions)", linetype = NULL, fill = NULL) +
  geom_text(aes(x = 2, y = 100, label="b)"),family="Times New Roman", colour="black", size=5)-> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave('2020_forecast/results/figs/catch_plot_pred.png', dpi=500, height=3, width=7, units="in")

# model average (not sure how to do prediction interval on model averaged linear regressions)**
fit.avg <- model.avg(model.m1, model.m2)
predicted<-predict(fit.avg, variables[23,], se.fit = TRUE)
lower_CI <- predicted$fit - 1.96*predicted$se.fit
upper_CI <- predicted$fit + 1.96*predicted$se.fit

# summary of model fits (i.e., coefficients, p-value); not-logged
eda.norm(log_data$SEAKCatch)# data is normal if the p-value is above 0.05.
eda.norm(log_data$SEAKCatch_log)
eda.norm(log_data$ISTI_MJJ)# data is normal if the p-value is above 0.05.
eda.norm(log_data$ISTI_MJJ_log)
eda.norm(log_data$CPUE)# data is normal if the p-value is above 0.05.
eda.norm(log_data$CPUE_log)

log_data %>% 
  dplyr::filter(JYear<2019) %>% 
  do(m1 = lm(SEAKCatch ~ CPUE, data = .),
     m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = .),
     m3 = lm(SEAKCatch~ CPUE*ISTI_MJJ, data = .)) -> lm_out_seak
lm_out_seak %>% 
  tidy(m1) -> m1
lm_out_seak %>% 
  tidy(m2) -> m2
lm_out_seak %>% 
  tidy(m3) -> m3
rbind(m1, m2) %>% 
  rbind(., m3) %>% 
  write.csv(., "2020_forecast/results/model_summary_table1_non.csv")

# models
log_data %>% 
  filter(JYear<2019) -> log_data_subset
model.m1 = lm(SEAKCatch ~ CPUE, data = log_data_subset)
model.m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = log_data_subset)
model.m3 = lm(SEAKCatch~ CPUE*ISTI_MJJ, data = log_data_subset)

# prediction
CPUE <- 1.202607
ISTI_MJJ <- 9.91121125
newdata <- data.frame(CPUE, ISTI_MJJ)
predicted<-predict(model.m2, newdata, interval="prediction", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)

