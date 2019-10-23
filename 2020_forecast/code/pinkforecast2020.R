# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19

#need to add MASE metric from Hundman paper***
# load----
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
model.names<-c(m1='CPUE')
model.formulas<-c(SEAKCatch ~ CPUE)

model.names<-c(m1='CPUE',
          m2='CPUE+ISTI_MJJ',
          m3='CPUE+ISTI_MJJ+CPUE:ISTI_MJJ')
model.formulas<-c(SEAKCatch ~ CPUE,
                 SEAKCatch ~ CPUE+ISTI_MJJ,
                 SEAKCatch ~ CPUE*ISTI_MJJ)

# summary statistics SEAK pink salmon harvest forecast models
seak.model.summary <- model.summary(harvest=variables$SEAKCatch,variables=variables, model.formulas=model.formulas,model.names=model.names)
seak.boot.summary <- boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas,model.names=model.names)

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
  filter(JYear<2019) -> variables
model_m1 <- train(SEAKCatch ~ CPUE, data = variables, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m2 <- train(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))
model_m3 <- train(SEAKCatch ~ CPUE * ISTI_MJJ, data = variables, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))

# MASE calculation
model.m1 = lm(SEAKCatch ~ CPUE, data = variables)
model.m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables)
model.m3 = lm(SEAKCatch ~ CPUE*ISTI_MJJ, data = variables)
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
  do(m1 = lm(SEAKCatch ~ CPUE, data = variables),
     m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables),
     m3 = lm(SEAKCatch ~ CPUE*ISTI_MJJ, data = variables)) -> lm_out_seak
lm_out_seak %>% 
  tidy(m1) -> m1
lm_out_seak %>% 
  tidy(m2) -> m2
lm_out_seak %>% 
  tidy(m3) -> m3

x <- rbind(m1, m2) #combine data for all zones
x <- rbind(x, m3)
write.csv(x, "2020_forecast/results/model_summary_table4.csv")

# model average
fit.avg<-model.avg(seak.model.summary[[1]])
predict(fit.avg,variables[23,])

#plot with prediction error

# Diagnostics: test model assumptions (normality, linearity, residuals)
variables %>% #outer verus maturity
  ggplot(aes(x=CPUE, y=SEAKCatch)) +
  geom_point() + 
  geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax), alpha=0.5) + 
  geom_line(data=new.data, aes(y=fit)) + 
  labs(x="CPUE", y="SEAKCatch") + 
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.5), limits = c(0, 0.5)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8,1.0), limits = c(0, 1.0)) +
  geom_text(aes(x = 0, y = 0.98, label="c)"),family="Times New Roman", colour="black", size=5)-> plot3

lm_out_seak %>% #  residuals against covariate
  augment(m2) %>% 
  mutate(resid = (.resid))%>% 
  ggplot(aes(x = CPUE, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  #scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.5), limits = c(0, 0.5))+
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45), limits = c(0,45)) +
  geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
  labs(y = "Residuals", x =  "CPUE") +
  geom_text(aes(x = 0, y = 45, label="a)"),family="Times New Roman", colour="black", size=5)-> plot1

lm_out_seak %>% #  residuals against covariate
  augment(m2) %>% 
  mutate(resid = (.resid))%>% 
  ggplot(aes(x = ISTI_MJJ, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  #scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.5), limits = c(0, 0.5))+
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45), limits = c(0,45)) +
  geom_smooth(aes(colour = ISTI_MJJ, fill = ISTI_MJJ), colour="black") +
  labs(y = "Residuals", x =  "ISTI_MJJ") +
  geom_text(aes(x = 7, y = 45, label="b)"),family="Times New Roman", colour="black", size=5) -> plot2

lm_out_seak %>% #residuals against fitted
  augment(m2) %>% 
  mutate(resid = (.resid),
         fit = (.fitted)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50") + 
  #scale_x_continuous(breaks = c(-1.0, -0.5, 0, 0.5), limits = c(-1, 0.5))+
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45), limits = c(0,45)) +
  geom_smooth(aes(colour = fit, fill = fit),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  labs(y = "Residuals", x =  "Fitted values") +
  geom_text(aes(x = 0, y = 45, label="c)", hjust = 1),family="Times New Roman", colour="black", size=5)-> plot3

lm_out_seak %>% #Cook's distance plot
  augment(m2) %>% 
  mutate(cooksd = (.cooksd),
         count = 1:22,
         name= ifelse(cooksd >0.05, count, ""))%>% 
  ggplot(aes(x = count, y = cooksd, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15), limits = c(0,0.15)) +
  labs(y = "Cook's distance", x =  "Index") +
  geom_text(aes(x = 0, y = 0.15, label="d)"),family="Times New Roman", colour="black", size=5)-> plot4

lm_out_seak %>% #leverage plot
  augment(m2) %>% 
  mutate(hat= (.hat),
         count = 1:22,
         name= ifelse(hat >0.20, count, "")) %>% 
  ggplot(aes(x = count, y = hat, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  #scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, .2, .25, .3), limits = c(0,0.3)) +
  labs(y = "hat-values", x =  "Index") +
  geom_text(aes(x = 1, y = 0.45, label="e)"),family="Times New Roman", colour="black", size=5)-> plot5

lm_out_seak %>% #Pearson by index
  augment(m2) %>% 
  mutate(resid = (.resid),
         count = 1:22) %>% 
  ggplot(aes(x = count, y = resid)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  #scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2), limits = c(-2,2)) +
  labs(y = "Pearson residuals", x =  "Index") +
  geom_text(aes(x = 0, y = 1.9, label="f)"),family="Times New Roman", colour="black", size=5)-> plot6

cowplot::plot_grid(plot1, plot2, plot3, plot4, plot5, plot6,  align = "vh", nrow = 3, ncol=2)
ggsave("figs/glm_diagnostics.png", dpi = 500, height = 6, width = 8, units = "in")
