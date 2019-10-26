#####################################
# SECM Pink salmon forecast models
#  
#  Jim Murphy updated: 10/18/19
########################################

#################
# Libraries
################
library(gam)
library(MASS)
library(MuMIn)
#################
#  Data
################
# Read data
SECM2019<-read.csv("2020_forecast/data/SECMcatch2019.csv")
variables<-read.csv("2020_forecast/data/SECMvar2019.csv")
variables$CPUE<-variables$CPUEcal#Use CPUEcal as CPUE index
n<-dim(variables)[1]#number of years including forecast year

# subset data by Peak month and generate list of catch by year
cal.data<-SECM2019[SECM2019$Pink_Peak,]
cal.data<-split(cal.data$Pink,cal.data$Year)

#####################################################
#  Analysis: 2020 SE Pink salmon harvest models
#####################################################
#Define model names and formulas
model.names<-c(m1='CPUE',
          m2='CPUE+ISTI_MJJ',
          m3='CPUE+CPUE:ISTI_MJJ',
          m4='CPUE+ISTI_MJJ+CPUE:ISTI_MJJ')
model.formulas<-c(SEAKCatch~CPUE,
                 SEAKCatch~CPUE+ISTI_MJJ,
                 SEAKCatch~CPUE+CPUE:ISTI_MJJ,
                 SEAKCatch~CPUE+ISTI_MJJ+CPUE:ISTI_MJJ)

#####################################################
#  Analysis: Summary statistics SEAK Pink salmon harvest forecast models
######################################################
seak.model.summary<-model.summary(harvest=variables$SEAKCatch,variables=variables, model.formulas=model.formulas,model.names=model.names)
seak.boot.summary<-boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas,model.names=model.names)

#Model average
fit.avg<-model.avg(seak.model.summary[[1]])
predict(fit.avg,variables[23,])


######################
#  Functions
######################

#######################
# linear regression jacknife prediction function
#           data: table of variables for forecast model
#  model.formula: linear regression model formulas included in the summary
# jacknife.index: index year for jacknife regression 
######################
jacklm.reg<-function(data,model.formula,jacknife.index=0){
  if(jacknife.index>0) {
    var.fit<-data[-jacknife.index,]
    var.pred<-data[jacknife.index,]
  }
  jack.lm<-lm(model.formula,data=var.fit)
  predict(jack.lm,newdata=var.pred)
}
######################################################
# model summary function
#       harvest: vector of harvest data to forecast
#       variables: table of variables for forecast model
#  model.formulas: selected model formulas included in the summary
#     model.names: names of selected models
#
# this function needs to be edited to select harvest data based on the model.formula
######################################################
model.summary<-function(harvest,variables,model.formulas,model.names){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  fit.out<-list()
  for(i in 1:length(model.formulas)) {
    fit<-lm(model.formulas[[i]],data=data)
    fit.out[[i]]<-fit
    model.sum<-summary(fit)
    vector.jack<-numeric()
    for(j in 1:(n-1)){
      vector.jack[j]<-jacklm.reg(data=data,model.formula=model.formulas[[i]],jacknife.index=j)
    }
    mape<-mean(abs(vector.jack-obs)/obs)
    meape<-median(abs(vector.jack-obs)/obs)
    model.pred<-unlist(predict(fit,newdata=variables[n,],se=T,interval='confidence',level=.8))
    model.results<-rbind(model.results,c(model.pred,R2=model.sum$r.squared,AdjR2=model.sum$adj.r.squared,AIC=AIC(fit),AICc=AICcmodavg::AICc(fit),BIC=BIC(fit),MAPE=mape,MEAPE=meape))
  }
  
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1:3]<-c('Fit','LCI','UCI')
  list(fit.out,model.results)}

#####################
# Bootstrap Functions
######################
#  boostraplm function
#        cpuedata: a list of individual catch observations by year
#       variables: a dataframe of harvest, catch indices and forecast variables
#   model.formula: linear model formula
#
###################################
bootstraplm<-function(cpuedata,variables,model.formula){
  logplus1<-function(x) log(x+1)
  catch1<-lapply(cpuedata,sample,replace=T)
  catch2<-lapply(catch1,logplus1)
  variables$CPUE<-unlist(lapply(catch2,mean))
  d<-dim(variables)[1]
  var.fit<-variables[-d,]
  var.pred<-variables[d,]
  boot.lm<-lm(model.formula,data=var.fit)
  predict(boot.lm,newdata=var.pred)
}

########################################################
#  bootstrap summary function: generates quantiles of boostrap distribution
#   cpuedata: list of individual cpue data to bootstrap
#   variables:  table of variables used in forcast models
#   model.formulas: vector of model formulas to boostrap
#   model.names: vector of model names to bootstrap
#   quantiles: quantiles of the boostrap distribution used for confidence intervals
#
#
# this function will need to be edited once the bootstraplm function is edited to support weighted transect data
########################################################
boot.summary<-function(cpuedata,variables,model.formulas,model.names,quantiles=c(.1,.9)){
  boot.summary<-numeric()
  for(i in 1:length(model.formulas))
    boot.summary<-rbind(boot.summary,
                        quantile(
                          replicate(10000,
                                    bootstraplm(cpuedata=cpuedata,variables=variables,model.formula=model.formulas[[i]])),
                          probs=quantiles))
  row.names(boot.summary)<-model.names
  boot.summary
}