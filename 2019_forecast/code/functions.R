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
######################################################
model.summary<-function(harvest,variables,model.formulas,model.names){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  for(i in 1:length(model.formulas)) {
    fit<-lm(model.formulas[[i]],data=data)
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
  model.results}


#####################
# Bootstrap Functions
######################
#  boostraplm function
#        cpuedata: a list of individual catch observations by year
#       variables: a dataframe of harvest, catch indices and forecast variables
#   model.formula: linear model formula
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
#       this function only uses the bootstraplm function and does not support weighted transect data
#   cpuedata: list of individual cpue data to bootstrap
#   variables:  table of variables used in forcast models
#   model.formulas: vector of model formulas to boostrap
#   model.names: vector of model names to bootstrap
#   quantiles: quantiles of the boostrap distribution used for confidence intervals
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


#to get rid of scientific notation
value_formatter <- function (val) {
  format(val, scientific =FALSE)
}

# Depends on dplyr
tickr <- function(
  data, # dataframe
  var, # column of interest
  to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}

#####################
#Unused Functions
######################
######################################################
#  boostraplm.mon function: boostrap model that allows peak month to vary
#        cpuedata: a list of individual catch observations by month and year
#       variables: a dataframe of harvest, catch indices and forecast variables
#   model.formula: linear model formula
#########################################################
boostraplm.mon<-function(cpuedata,variables,model.formula,nyears=22){
  logplus1<-function(x) log(x+1)
  catch1<-lapply(cpuedata,sample,replace=T)
  catch2<-lapply(catch1,logplus1)
  catch3<-lapply(catch2,mean)
  catch4<-matrix(unlist(catch3),nrow=nyears)
  catch4<-catch4[,c(2,3)]#Extract June and July
  catch4[is.na(catch4)]<-0
  julysub<-catch4[,2]>catch4[,1]#Identify years when July catches are larger
  catch5<-catch4[,1]#Start with June catch
  catch5[julysub]<-catch4[julysub,2]#Replace June catch with July when larger
  variables$CPUE<-catch5
  d<-dim(variables)[1]
  var.fit<-variables[-d,]
  var.pred<-variables[d,]
  boot.lm<-lm(model.formula,data=var.fit)
  predict(boot.lm,newdata=var.pred)
}
########################################################
#  boostraplm.loc function: a boostrap model that balances sampling at Upper Chatham and Icy Strait
#        cpuedata: a list of individual catch observations by locality and year
#       variables: a dataframe of harvest, catch indices and forecast variables with forecast (final) year included
#   model.formula: linear model formula
##########################################################
bootstraplm.loc<-function(cpuedata,variables,model.formula,nyears=22){
  logplus1<-function(x) log(x+1)
  catch1<-lapply(cpuedata,sample,replace=T)
  catch2<-lapply(catch1,logplus1)
  catch3<-lapply(catch2,mean)
  catch4<-matrix(unlist(catch3),nrow=nyears)
  variables$CPUE<-apply(catch4,1,mean)
  d<-dim(variables)[1]
  var.fit<-variables[-d,]
  var.pred<-variables[d,]
  boot.lm<-lm(model.formula,data=var.fit)
  predict(boot.lm,newdata=var.pred)
}

