# SECM Pink salmon forecast models
# Script written by Jim Murphy updated: 10/18/19

# load----
library(gam)
library(MASS)
library(MuMIn)
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
          m3='CPUE+CPUE:ISTI_MJJ',
          m4='CPUE+ISTI_MJJ+CPUE:ISTI_MJJ')
model.formulas<-c(SEAKCatch ~ CPUE,
                 SEAKCatch ~ CPUE+ISTI_MJJ,
                 SEAKCatch ~ CPUE+CPUE:ISTI_MJJ,
                 SEAKCatch ~ CPUE+ISTI_MJJ+CPUE:ISTI_MJJ)

# summary statistics SEAK pink salmon harvest forecast models
seak.model.summary<-model.summary(harvest=variables$SEAKCatch,variables=variables, model.formulas=model.formulas,model.names=model.names)
seak.boot.summary<-boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas,model.names=model.names)

# model average
fit.avg<-model.avg(seak.model.summary[[1]])
predict(fit.avg,variables[23,])




