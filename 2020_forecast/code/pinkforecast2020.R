# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19

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

# summary of model fits
variables %>% 
  filter(JYear<2019) %>% 
  do(m1 = lm(SEAKCatch ~ CPUE, data = variables),
     m2 = lm(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables),
     m3 = lm(SEAKCatch ~ CPUE+ISTI_MJJ+CPUE*ISTI_MJJ, data = variables)) -> lm_out_seak
lm_out_seak %>% 
  tidy(m1) -> m1
lm_out_seak %>% 
  tidy(m2) -> m2
lm_out_seak %>% 
  tidy(m3) -> m3

x <- rbind(m1, m2) #combine data for all zones
x <- rbind(x, m3)
write_csv(x, "2020_forecast/results/lm_out_seak.csv")

# leave one out cross validation
# https://stats.stackexchange.com/questions/27351/compare-models-loccv-implementation-in-r
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
variables %>% 
  filter(JYear<2019) -> variables
model_m1 <- train(SEAKCatch ~ CPUE, data = variables, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("MAPE"))

model_m2 <- train(SEAKCatch ~ CPUE + ISTI_MJJ, data = variables, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("RMSE","MAPE"))

model_m3 <- train(SEAKCatch ~ CPUE * ISTI_MJJ, data = variables, method='lm', 
                  trControl=trainControl(method = "LOOCV", summaryFunction = mape_summary),
                  metric = c("RMSE","MAPE"))

# model average
fit.avg<-model.avg(seak.model.summary[[1]])
predict(fit.avg,variables[23,])



