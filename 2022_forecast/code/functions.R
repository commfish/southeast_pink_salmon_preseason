# make sure functions refer to correct forecast folder
# MASE function #https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments
MASE <- function(f,y) { # f = vector with forecasts, y = vector with actuals
  if(length(f)!=length(y)){ stop("Vector length is not equal") }
  n <- length(f)
  return(mean(abs((y - f) / ((1/(n-1)) * sum(abs(y[2:n]-y[1:n-1]))))))
}

# linear regression jacknife prediction function
     # data: table of variables for forecast model
     # model.formula: linear regression model formulas included in the summary
     # jacknife.index: index year for jacknife regression 
jacklm.reg<-function(data,model.formula,jacknife.index=0){
  if(jacknife.index>0) {
    var.fit<-data[-jacknife.index,]
    var.pred<-data[jacknife.index,]
  }
  jack.lm<-lm(model.formula,data=var.fit)
  predict(jack.lm,newdata=var.pred)
}

# model summary function
      # harvest: vector of harvest data to forecast
      # variables: table of variables for forecast model
      # model.formulas: selected model formulas included in the summary
      # model.names: names of selected models
  
      # this function needs to be edited to select harvest data based on the model.formula
model.summary <- function(harvest,variables,model.formulas,model.names){
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
    mape_LOOCV<-mean(abs(obs-vector.jack)/obs)
    mape<-mean(abs(obs-fit$fitted.values)/obs)
    mase<-MASE(f = (fit$fitted.values), y = obs) # function
    mase2<-Metrics::mase(obs,fit$fitted.values,1 )
    model.pred<-unlist(predict(fit,newdata=variables[n,],se=T,interval='prediction',level=.8))
    sigma <- sigma(fit)
    model.results<-rbind(model.results,c(model.pred,R2=model.sum$r.squared,AdjR2=model.sum$adj.r.squared, AIC=AIC(fit),AICc=AICcmodavg::AICc(fit),BIC=BIC(fit),
                                         p = pf(model.sum$fstatistic[1], model.sum$fstatistic[2],model.sum$fstatistic[3],lower.tail = FALSE), sigma = sigma,
                                         MAPE=mape,MAPE_LOOCV=mape_LOOCV,
                                         MASE = mase, MASE2=mase2))
  }
  
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1:3]<-c('fit','fit_LPI','fit_UPI')
  as.data.frame(model.results)-> x
  write.csv(x, file=paste0(results.directory, "/seak_model_summary.csv"))}
# Bootstrap Functions
   # boostraplm function
       # cpuedata: a list of individual catch observations by year
       # variables: a dataframe of harvest, catch indices and forecast variables
       # model.formula: linear model formula
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

# bootstrap summary function: generates quantiles of boostrap distribution
      # cpuedata: list of individual cpue data to bootstrap
      # variables:  table of variables used in forcast models
      # model.formulas: vector of model formulas to boostrap
      # model.names: vector of model names to bootstrap
      # quantiles: quantiles of the boostrap distribution used for confidence intervals
# this function will need to be edited once the bootstraplm function is edited to support weighted transect data
boot.summary <- function(cpuedata,variables,model.formulas,model.names,quantiles=c(.1,.9)){
  boot.summary<-numeric()
  for(i in 1:length(model.formulas))
    boot.summary<-rbind(boot.summary,quantile(replicate(10000,
                      bootstraplm(cpuedata=cpuedata,variables=variables,model.formula=model.formulas[[i]])),
                      probs=quantiles))
  row.names(boot.summary)<-model.names
  boot.summary
  write.csv(boot.summary, file.path(results.directory, "seak_model_bootsummary.csv")) }


mape <- function(actual, predicted){
  mean(abs((actual - predicted)/actual))}

mape_summary <- function (data,lev = NULL, model = NULL) {
  out <- mape((data$obs),(data$pred))  
  names(out) <- "MAPE"
  out
}

#normality test
eda.norm <- function(x, ...)
{
  par(mfrow=c(2,2))
  if(sum(is.na(x)) > 0)
    warning("NA's were removed before plotting")
  x <- x[!is.na(x)]
  hist(x, main = "Histogram and non-\nparametric density estimate", prob = T)
  iqd <- summary(x)[5] - summary(x)[2]
  lines(density(x, width = 2 * iqd))
  boxplot(x, main = "Boxplot", ...)
  qqnorm(x)
  qqline(x)
  plot.ecdf(x, main="Empirical and normal cdf")
  LIM <- par("usr")
  y <- seq(LIM[1],LIM[2],length=100)
  lines(y, pnorm(y, mean(x), sqrt(var(x))))
  shapiro.test(x)
}


