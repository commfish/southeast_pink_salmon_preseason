# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19

#need to add MASE metric from Hundman paper***
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

# subset data by peak month and generate list of catch by year
cal.data <- SECM2019[SECM2019$Pink_Peak,]
cal.data <- split(cal.data$Pink,cal.data$Year)

# 2020 SE Pink salmon harvest models
# define model names and formulas
model.names<-c(m1='CPUE',
          m2='CPUE+ISTI_MJJ',
          m3='CPUE+ISTI_MJJ+CPUE:ISTI_MJJ')
model.formulas<-c(SEAKCatch ~ CPUE,
                 SEAKCatch ~ CPUE+ISTI_MJJ,
                 SEAKCatch ~ CPUE*ISTI_MJJ)

# summary statistics SEAK pink salmon harvest forecast models
seak.model.summary <- model.summary(harvest=variables$SEAKCatch,variables=variables, model.formulas=model.formulas,model.names=model.names)
seak.boot.summary <- boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas,model.names=model.names)
# http://rstudio-pubs-static.s3.amazonaws.com/24365_2803ab8299934e888a60e7b16113f619.html
CPUE <- 1.202606515
ISTI_MJJ <- 9.91121125
newdata <- data.frame(x1,x2)
bootfit1 <- Boot(model.m2, function(SEAKCatch)predict(SEAKCatch, newdata), R=10000)
predict(model.m2, newdata, interval="predict", level = 0.80) 

# results as tables
results<-read.csv("2020_forecast/results/seak_model_summary.csv")
results %>% 
  dplyr::select(X, Fit, LCI, UCI, se.fit, df, residual.scale) %>%
  dplyr::rename(model = 'X')%>% 
write.csv(., "2020_forecast/results/model_summary_table1.csv")
results %>% 
  dplyr::select(X, AdjR2, AIC, AICc, BIC) %>%
  dplyr::rename(model = 'X') %>% 
write.csv(., "2020_forecast/results/model_summary_table2.csv")

# leave one out cross validation (verify seak.model.summary)
# https://stats.stackexchange.com/questions/27351/compare-models-loccv-implementation-in-r
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
variables %>% 
  filter(JYear<2019) -> variables_subset
model_m1 <- train(SEAKCatch ~ CPUE, data = variables_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m2 <- train(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m3 <- train(SEAKCatch ~ CPUE * ISTI_MJJ, data = variables_subset, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))

# MASE calculation
model.m1 = lm(SEAKCatch ~ CPUE, data = variables_subset)
model.m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables_subset)
model.m3 = lm(SEAKCatch ~ CPUE*ISTI_MJJ, data = variables_subset)
MASE(model.m1, model.m2, model.m3) %>%
  dplyr::select(MASE)-> MASE

plot(model.m1)
results<-read.csv("2020_forecast/results/seak_model_summary.csv")
results %>% 
  dplyr::select(X, MAPE, MEAPE) %>%
  dplyr::rename(model = 'X') %>%
  cbind(., MASE)%>%
  dplyr::select(model, MAPE, MEAPE, MASE) %>%
  write.csv(., "2020_forecast/results/model_summary_table3.csv")

# summary of model fits (i.e., coefficients, p-value)
variables %>% 
  dplyr::filter(JYear<2019) %>% 
  do(m1 = lm(SEAKCatch ~ CPUE, data = variables_subset),
     m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables_subset),
     m3 = lm(SEAKCatch ~ CPUE*ISTI_MJJ, data = variables_subset)) -> lm_out_seak
lm_out_seak %>% 
  tidy(m1) -> m1
lm_out_seak %>% 
  tidy(m2) -> m2
lm_out_seak %>% 
  tidy(m3) -> m3

x <- rbind(m1, m2) #combine data for all zones
x <- rbind(x, m3)
write.csv(x, "2020_forecast/results/model_summary_table4.csv")

# Diagnostics: test model assumptions (normality, linearity, residuals)
# diagnostic plots
axisb <- tickr(m2, year, 2)
png("2020_forecast/results/figs/general_diagnostics.png")
autoplot(model.m2)
dev.off()

outlierTest(model.m2) #Bonferroni p-values (term # 16)
residualPlots(model.m2) #lack-of fit curvature test; terms that are non-significant suggest a properly speciified model
lm_out_seak %>% #  residuals against covariate
  augment(m2) %>% 
  mutate(resid = (.resid))%>% 
  ggplot(aes(x = CPUE, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
  labs(y = "Residuals", x =  "CPUE") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 45, label="a)"),family="Times New Roman", colour="black", size=5)-> plot1

lm_out_seak %>% #  residuals against covariate
  augment(m2) %>% 
  mutate(resid = (.resid))%>% 
  ggplot(aes(x = ISTI_MJJ, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = ISTI_MJJ, fill = ISTI_MJJ), colour="black") +
  labs(y = "Residuals", x =  "ISTI_MJJ") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(aes(x = 7, y = 45, label="b)"),family="Times New Roman", colour="black", size=5) -> plot2
cowplot::plot_grid(plot1, plot2
                   ,  align = "vh", nrow = 1, ncol=2)
ggsave("2020_forecast/results/figs/predicted.png", dpi = 500, height = 3, width = 6, units = "in")

lm_out_seak %>% #Pearson by index
  augment(m2) %>% 
  mutate(resid = (.resid),
         count = 1997:2018) %>% 
  ggplot(aes(x = count, y = resid)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  scale_x_continuous(breaks = axisb$breaks, labels = axisb$labels) +
  labs(y = "Residuals", x =  "Juvenile year") + theme_bw () +theme(text = element_text(size=10),
                                                                   axis.text.x = element_text(angle=90, hjust=1),
                                                                   panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 1997, y = 45, label="a)"),family="Times New Roman", colour="black", size=5)-> plot2

lm_out_seak %>% #residuals against fitted
  augment(m2) %>% 
  mutate(resid = (.resid),
         fit = (.fitted)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = fit, fill = fit),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Residuals", x =  "Fitted values") +
  geom_text(aes(x = 1, y = 45, label="b)"),family="Times New Roman", colour="black", size=5)-> plot3
cowplot::plot_grid(plot2, plot3, align = "vh", nrow = 1, ncol=2)
ggsave("2020_forecast/results/figs/fitted.png", dpi = 500, height = 3, width = 6, units = "in")

qf(.50, df1=4, df2=18) # F distribution of p+1 (4) and n-p-1 (22-3-1)
lm_out_seak %>% #Cook's distance plot
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
  scale_x_continuous(breaks = axisb$breaks, labels = axisb$labels) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1))+
  labs(y = "Cook's distance", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                      axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="a)"),family="Times New Roman", colour="black", size=5)-> plot4

#hat value 2*p/n = 2*(3/22)
lm_out_seak %>% #leverage plot
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
  geom_text(aes(x = 1997, y = 0.45, label="b)"),family="Times New Roman", colour="black", size=5)-> plot5

cowplot::plot_grid(plot4, plot5,  align = "vh", nrow = 1, ncol=2)
ggsave("2020_forecast/results/figs/influential.png", dpi = 500, height = 3, width = 6, units = "in")

# plot with prediction error 
lm_out_seak %>% 
  augment(m2) %>% 
  mutate(year = 1998:2019, 
         catch = SEAKCatch, 
         fit = (.fitted)) -> m2
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
  theme(legend.position="none") +geom_point(x=2020, y=2.06113749767909, pch=8, size=2) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100), limits = c(0,100))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "Harvest (millions)\n", linetype = NULL, fill = NULL) +
  geom_segment(aes(x = 2020, y = 12.19, yend = 0, xend = 2020), size=1, colour="black", lty=1) -> plot1

#test new model to match ouput in SFMM
#nd<-data.frame(CPUE=1.202606515,ISTI_MJJ=9.91121125) 
#prediction<-predict(model.m2, newdata=nd, interval="prediction")

#lm_out_seak %>% 
#  augment(m2) %>% 
#  mutate(year = 1997:2018, 
#         catch = SEAKCatch, 
#         fit = (.fitted)) -> m2
#axisb <- tickr(m2, year, 2)
#m2 %>%
#  ggplot(aes(x=fit, y=catch)) +
#  geom_point() +
#  geom_point(x=1.202607, y=2.06113749767909, pch=8, size=2) +
#  geom_smooth(method="lm", colour="black") +
#  geom_point(aes(y = catch), colour = "black", size = 1) +
#  scale_color_grey() +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#  theme(legend.position="none") + theme(legend.title=element_blank())+
#  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100))+
#  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100))+
#  labs(y = "Harvest (millions)\n", x = "Fitted", linetype = NULL, fill = NULL) +
#  geom_segment(aes(x = 1.202607, y = 12.19, yend = 0, xend = 1.202607), size=1, colour="black", lty=1)+  
#  geom_text(aes(x = 3, y = 100, label="b)"),family="Times New Roman", colour="black", size=5)-> plot2

cowplot::plot_grid(plot1, plot1,  align = "vh", nrow = 1, ncol=1)
ggsave('2020_forecast/results/figs/catch_plot_pred.png', dpi=500, height=3, width=7, units="in")

# model average (not sure how to do prediction interval on model averaged linear regressions)**
fit.avg <- model.avg(model.m1, model.m2)
predicted<-predict(fit.avg, variables[23,], se.fit = TRUE, level= 0.8, interval ="predict")
lower_CI <- predicted$fit - 1.96*predicted$se.fit
upper_CI <- predicted$fit + 1.96*predicted$se.fit
