# make sure functions refer to correct forecast folder
# MASE function #https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments
# add MAPE the last 5 years
# https://stackoverflow.com/questions/12994929/whats-the-gaps-for-the-forecast-error-metrics-mape-and-wmape
# https://www.statology.org/leave-one-out-cross-validation/
# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition 
)


# MASE <- function(f,y) { # f = vector with forecasts, y = vector with actuals
#   if(length(f)!=length(y)){ stop("Vector length is not equal") }
#   n <- length(f)
#   return(mean(abs((y - f) / ((1/(n-1)) * sum(abs(y[2:n]-y[1:n-1]))))))
# }

mape <- function(actual, predicted){
  mean(abs((actual - predicted)/actual))}

inv_var <- function(actual, predicted){
1/((sum(abs((actual - predicted)^2)))/(length(actual)-1))}

mape_summary <- function (data,lev = NULL, model = NULL) {
  out <- mape((data$obs),(data$pred))  
  names(out) <- "MAPE"
  out
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
f_model_summary <- function(harvest,variables,model.formulas,model.names,w, models){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  weights<-w[-n]
  data<-variables[-n,]
  fit.out<-list()
  sum_w<-sum(weights)
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
    wmape1<-((abs(obs-fit$fitted.values)/obs)*weights)
    wmape2<-sum(wmape1)
    wmape<-wmape2/sum_w
    #mase<-MASE(f = (fit$fitted.values), y = obs) # function
    #mase2<-Metrics::mase(obs,fit$fitted.values,1 )
    model.pred<-unlist(predict(fit,newdata=variables[n,],se=T,interval='prediction',level=.80))
    sigma <- sigma(fit)
    model.results<-rbind(model.results,c(model.pred,R2=model.sum$r.squared,AdjR2=model.sum$adj.r.squared, AIC=AIC(fit),AICc=AICcmodavg::AICc(fit),BIC=BIC(fit),
                                         p = pf(model.sum$fstatistic[1], model.sum$fstatistic[2],model.sum$fstatistic[3],lower.tail = FALSE), sigma = sigma,
                                         MAPE=mape,MAPE_LOOCV=mape_LOOCV,
                                         #MASE = mase, MASE2=mase2, 
                                         wMAPE= wmape))
  }
  
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1:3]<-c('fit','fit_LPI','fit_UPI')
  as.data.frame(model.results)%>%
write.csv(., paste0(results.directory, "/seak_model_summary", models, ".csv"))}

f_model_summary_odd <- function(harvest,variables,model.formulas,model.names,w, models){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  weights<-w[-n]
  data<-variables[-n,]
  fit.out<-list()
  sum_w<-sum(weights)
  for(i in 1:length(model.formulas)) {
    fit<-lm(model.formulas[[i]],data=data)
    fit.out[[i]]<-fit
    model.sum<-summary(fit)
    # vector.jack<-numeric()
    # for(j in 1:(n-1)){
    #   vector.jack[j]<-jacklm.reg(data=data,model.formula=model.formulas[[i]],jacknife.index=j)
    # }
    # mape_LOOCV<-mean(abs(obs-vector.jack)/obs)
    mape<-mean(abs(obs-fit$fitted.values)/obs)
    wmape1<-((abs(obs-fit$fitted.values)/obs)*weights)
    wmape2<-sum(wmape1)
    wmape<-wmape2/sum_w
    #mase<-MASE(f = (fit$fitted.values), y = obs) # function
    #mase2<-Metrics::mase(obs,fit$fitted.values,1 )
    model.pred<-unlist(predict(fit,newdata=variables[n,],se=T,interval='prediction',level=.80))
    sigma <- sigma(fit)
    model.results<-rbind(model.results,c(model.pred,R2=model.sum$r.squared,AdjR2=model.sum$adj.r.squared, AIC=AIC(fit),AICc=AICcmodavg::AICc(fit),BIC=BIC(fit),
                                         p = pf(model.sum$fstatistic[1], model.sum$fstatistic[2],model.sum$fstatistic[3],lower.tail = FALSE), sigma = sigma,
                                         MAPE=mape,
                                         #MASE = mase, MASE2=mase2, 
                                         wMAPE= wmape))
  }
  
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1:3]<-c('fit','fit_LPI','fit_UPI')
  as.data.frame(model.results)%>%
    write.csv(., paste0(results.directory, "/seak_model_summary", models, ".csv"))}

f_model_summary_model_average <- function(harvest,variables,model.formulas,model.names,w){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  weights<-w[-n]
  data<-variables[-n,]
  fit.out<-list()
  sum_w<-sum(weights)
  for(i in 1:length(model.formulas)) {
    fit<-lm(model.formulas[[i]],data=data)
    fit.out[[i]]<-fit
    model.sum<-summary(fit)
    model.pred<-fitted(fit)
    sigma <- sigma(fit)
    model.results<-rbind(model.results,c(model.pred,R2=model.sum$r.squared,AdjR2=model.sum$adj.r.squared, AICc=AICcmodavg::AICc(fit),
                                         p = pf(model.sum$fstatistic[1], model.sum$fstatistic[2],model.sum$fstatistic[3],lower.tail = FALSE), sigma = sigma))
  }
  
  row.names(model.results)<-model.names
  as.data.frame(model.results)-> x
  x %>% 
    mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                     'm9','m10','m11','m12','m13','m14','m15','m16','m17',
                     'm18')) ->x
    
  write.csv(x, file = paste0(results.directory, "/seak_model_summary_hindcasts.csv"))}


# output diagnostics function
f_model_diagnostics <- function(best_model, model_name){
  augment(best_model) %>% 
  mutate(resid = round((.resid),3),
         hat_values = round((.hat),3),
         Cooks_distance = round((.cooksd),3),
         std_resid = round((.std.resid),3),
         fitted = round((.fitted),3),
         year=1998:year.data, 
         juvenile_year = 1997:year.data.one,
         model = model_name) %>%
    dplyr::select(17, 15,16,10:14) %>%
  #dplyr::select(model, year, juvenile_year,resid, hat_values, Cooks_distance, std_resid, fitted) %>%
  write.csv(paste0(results.directory, "/model_summary_table_", model_name, ".csv"), row.names = F)}

# output diagnostic plots
f_resid_year_diagnostics_plot<-function(best_model, model_name){
  augment(best_model) %>% 
    mutate(resid = (.std.resid)) %>% 
    ggplot(., aes(x = CPUE, y = resid)) +
    geom_hline(yintercept = 0, lty=2) + 
    geom_point(color ="grey50") + ggtitle(model_name) +
    geom_smooth(aes(colour = CPUE), colour="black") +
    scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
    labs(y = "Standardized residuals", x =  "ln(CPUE+1)") + theme(legend.position="none") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    geom_text(aes(x = 0.2, y = 4, label="A."),family="Times", colour="black", size=5) -> plot1

  augment(best_model) %>% 
  mutate(resid = (.std.resid),
         count = 1997:year.data.one)%>% 
  ggplot(aes(x = count, y = resid)) + ggtitle(model_name) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0, 1,2,3,4), limits = c(-4,4))+
  labs(y = "Standardized residuals", x =  "Juvenile year") + theme_bw () +theme(text = element_text(size=10),
                                                                                axis.text.x = element_text(angle=90, hjust=1, size=6),
                                                                                panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 1997, y = 4, label="C."),family="Times", colour="black", size=5) -> plot2

# residuals against fitted
augment(best_model) %>% 
  mutate(resid = (.resid),
         fit = (.fitted)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50")  + ggtitle(model_name) +
  geom_smooth(aes(colour = fit,),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1))+
  scale_x_continuous(breaks = c(2,3,4,5,5), limits = c(2,5))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Residuals", x =  "Fitted values") +
  geom_text(aes(x = 2.1, y = 1, label="D."),family="Times", colour="black", size=5)-> plot3

# residuals against temp
augment(best_model) %>% 
  mutate(resid = (.std.resid),
         temp = .[[3]]) %>% # third column should be temperature variable
  ggplot(aes(x = temp, y = resid)) +
  geom_point(color ="grey50")  + ggtitle(model_name) +
  geom_smooth(aes(colour = temp),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(5,6,7,8,9,10,11,12), limits = c(5,12)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Standardized residuals", x =  "Temperature") +
  geom_text(aes(x = 5.2, y = 4, label="B."),family="Times", colour="black", size=5) -> plot4

augment(best_model) %>% 
  mutate(temp = .[[3]]) %>% 
  ggplot(aes(x = temp, y = SEAKCatch_log)) +
  geom_point(color ="grey50") +  ggtitle(model_name) +
  geom_smooth(aes(colour = temp), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  scale_x_continuous(breaks = c(5,6,7,8,9,10,11,12), limits = c(5,12)) +
  labs(y = "ln(Harvest)", x =  "Temperature") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 5, y = 6, label="E."),family="Times", colour="black", size=5) -> plot5

augment(best_model) %>%  
  ggplot(aes(x = CPUE, y = SEAKCatch_log)) +
  geom_point(color ="grey50") +   ggtitle(model_name) +
  geom_smooth(aes(colour = CPUE), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  labs(y = "ln(Harvest)", x =  "ln(CPUE+1)") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 6, label="F."),family="Times", colour="black", size=5) -> plot6

cowplot::plot_grid(plot1, plot4, plot2, plot3, align = "vh", nrow = 2, ncol=2)
ggsave(paste0(results.directory, "model_figs/fitted_", model_name,".png"), dpi = 500, height = 5, width = 5, units = "in")}

# Cook's distance and leverage plot
f_resid_leverage_diagnostics_plot<-function(best_model, model_name, k, p){
  level <- 4/(sample_size-k-1) # source: Ren et al. 2016# k = # of predictors in model (not including intercept)
augment(best_model) %>% 
  mutate(cooksd = (.cooksd),
         count = (1997:year.data.one),
         name= ifelse(cooksd >level, count, "")) %>% 
  ggplot(aes(x = count, y = cooksd, label=name)) +ggtitle(model_name) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 2, position = position_stack(vjust = 1.1)) + 
  geom_hline(yintercept = level, lty=2) +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0, 1.25, 1.50), limits = c(0,1.5))+
  labs(y = "Cook's distance", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                            axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1.5, label="A."),family="Times", colour="black", size=5) -> plot1

# leverage plot
#  p = number of parameters in the model including intercept
level <- 2*p/sample_size # source: Ren et al. 2016
# leverage plot
augment(best_model) %>% 
  mutate(hat= (.hat),
         count = 1997:year.data.one,
         name= ifelse(hat > level, count, "")) %>% # may need to adjust valeu; see hat value equation above
  ggplot(aes(x = count, y = hat, label=name)) +ggtitle(model_name) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 2, position = position_stack(vjust =1.1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = level, lty=2) +
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  labs(y = "Hat-values", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                       axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="B."),family="Times", colour="black", size=5)-> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "model_figs/influential_", model_name,".png"), dpi = 500, height = 3, width = 6, units = "in")}

# Bootstrap Functions
   # boostraplm function
       # cpuedata: a list of individual catch observations by year
       # variables: a dataframe of harvest, catch indices and forecast variables
       # model.formula: linear model formula
# bootstraplm<-function(cpuedata,variables,model.formula){
#   logplus1<-function(x) log(x+1)
#   catch1<-lapply(cpuedata,sample,replace=T)
#   catch2<-lapply(catch1,logplus1)
#   variables$CPUE<-unlist(lapply(catch2,mean))
#   d<-dim(variables)[1]
#   var.fit<-variables[-d,]
#   var.pred<-variables[d,]
#   boot.lm<-lm(model.formula,data=var.fit)
#   predict(boot.lm,newdata=var.pred)
# }

# bootstrap summary function: generates quantiles of boostrap distribution
      # cpuedata: list of individual cpue data to bootstrap
      # variables:  table of variables used in forcast models
      # model.formulas: vector of model formulas to boostrap
      # model.names: vector of model names to bootstrap
      # quantiles: quantiles of the boostrap distribution used for confidence intervals
# this function will need to be edited once the bootstraplm function is edited to support weighted transect data
# boot.summary <- function(cpuedata,variables,model.formulas,model.names,quantiles=c(.1,.9)){
#   boot.summary<-numeric()
#   for(i in 1:length(model.formulas))
#     boot.summary<-rbind(boot.summary,quantile(replicate(10000,
#                       bootstraplm(cpuedata=cpuedata,variables=variables,model.formula=model.formulas[[i]])),
#                       probs=quantiles))
#   row.names(boot.summary)<-model.names
#   boot.summary
#   write.csv(boot.summary, file.path(results.directory, "seak_model_bootsummary.csv")) }

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
# function check for one model (one step ahead MAPE)
f_model_one_step_ahead <- function(harvest,variables,model, start, end, model_num){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  fit.out<-list()
  for (i in (end+1):tail(data$JYear)[6])
  {
    fit<-lm(model,data = data[data$JYear >= start & data$JYear < i,])
    fit.out<-list()
    data$model1_sim[data$JYear == i] <- predict(fit, newdata = data[data$JYear == i,])
    data$sigma[data$JYear == i] <- sigma(fit, newdata = data[data$JYear == i,])
  }
  return(data)
  data %>% 
    dplyr::filter(JYear > end) %>% 
    as.data.frame() %>% 
    write.csv(., file = paste0(results.directory.MAPE, "results_",model_num,".csv"))
  # mape(exp(output$SEAKCatch_log),exp(output$model1_sim))
} 
# function check for one model (one step ahead MAPE)
# seak_model_summary1 <- f_model_one_step_ahead(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE, start = 1998, end = 2012, model_num = "m1b")

f_model_one_step_ahead_odd <- function(harvest,variables,model, start, end, model_num){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  fit.out<-list()
  for (i in (end+1):tail(data$year_num)[6])
  {
    fit<-lm(model,data = data[data$year_num >= start & data$year_num < i,])
    fit.out<-list()
    data$model1_sim[data$year_num== i] <- predict(fit, newdata = data[data$year_num == i,])
    data$sigma[data$year_num == i] <- sigma(fit, newdata = data[data$year_num == i,])
  }
  return(data)
  data %>% 
    dplyr::filter(year_num > end) %>% 
    as.data.frame() %>% 
    write.csv(., file = paste0(results.directory.MAPE, "results_",model_num,".csv"))
  # mape(exp(output$SEAKCatch_log),exp(output$model1_sim))
} 
# function check for one model (one step ahead MAPE)
# seak_model_summary1 <- f_model_one_step_ahead_odd(harvest=log_data_odd$SEAKCatch_log, variables=log_data_odd, model = SEAKCatch_log ~CPUE, start = 1, end = 8, model_num = "m1b")


f_model_one_step_ahead_multiple5 <- function(harvest,variables,model.formulas,model.names, start, end, models){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  fit.out<-list()
  for(i in 1:length(model.formulas)) 
  {
    for (j in (end+1):tail(data$JYear)[6])
    {
      fit<-lm(model.formulas[[i]],data = data[data$JYear >= start & data$JYear < j,])
      fit.out[[i]]<-fit
      data$model1_sim[data$JYear == j] <- predict(fit, newdata = data[data$JYear == j,])
    }
    #return(data)
    data %>% 
      dplyr::filter(JYear > end) -> output
    MAPE<-mape(exp(output$SEAKCatch_log),exp(output$model1_sim))
    #MAPE<-mape(output$SEAKCatch_log,output$model1_sim)
    model.results<-rbind(model.results, MAPE= MAPE)
  } 
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1]<-c('MAPE5')
  as.data.frame(model.results) %>%
  write.csv(., paste0(results.directory, "/seak_model_summary_one_step_ahead5", models, ".csv"))}

f_model_one_step_ahead_multiple5_odd <- function(harvest,variables,model.formulas,model.names, start, end, models){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  fit.out<-list()
  for(i in 1:length(model.formulas)) 
  {
    for (j in (end+1):tail(data$JYear)[6])
    {
      fit<-lm(model.formulas[[i]],data = data[data$year_num >= start & data$year_num  < j,])
      fit.out[[i]]<-fit
      data$model1_sim[data$year_num  == j] <- predict(fit, newdata = data[data$year_num  == j,])
    }
    #return(data)
    data %>% 
      dplyr::filter(year_num > end) -> output
    MAPE<-mape(exp(output$SEAKCatch_log),exp(output$model1_sim))
    #MAPE<-mape(output$SEAKCatch_log,output$model1_sim)
    model.results<-rbind(model.results, MAPE= MAPE)
  } 
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1]<-c('MAPE5')
  as.data.frame(model.results) %>%
    write.csv(., paste0(results.directory, "/seak_model_summary_one_step_ahead5", models, ".csv"))}

f_model_one_step_ahead_multiple10 <- function(harvest,variables,model.formulas,model.names, start, end){
  n<-dim(variables)[1]
  model.results<-numeric()
  obs<-harvest[-n]
  data<-variables[-n,]
  fit.out<-list()
  for(i in 1:length(model.formulas)) 
  {
    for (j in (end+1):tail(data$JYear)[6])
    {
      fit<-lm(model.formulas[[i]],data = data[data$JYear >= start & data$JYear < j,])
      fit.out[[i]]<-fit
      data$model1_sim[data$JYear == j] <- predict(fit, newdata = data[data$JYear == j,])
    }
    #return(data)
    data %>% 
      dplyr::filter(JYear > end) -> output
    MAPE<-mape(exp(output$SEAKCatch_log),exp(output$model1_sim))
    #MAPE<-mape(output$SEAKCatch_log,output$model1_sim)
    model.results<-rbind(model.results, MAPE= MAPE)
  } 
  row.names(model.results)<-model.names
  dimnames(model.results)[[2]][1]<-c('MAPE10')
  as.data.frame(model.results)-> x
  write.csv(x, file=paste0(results.directory, "/seak_model_summary_one_step_ahead10.csv"))
  
}
# f_model_one_step_ahead_inv_var <- function(harvest,variables,model, start, end){
#   n<-dim(variables)[1]
#   model.results<-numeric()
#   obs<-harvest[-n]
#   data<-variables[-n,]
#   fit.out<-list()
#   for (i in (end+1):tail(data$JYear)[6])
#   {
#     fit<-lm(model,data = data[data$JYear >= start & data$JYear < i,])
#     data$model1_sim[data$JYear == i] <- predict(fit, newdata = data[data$JYear == i,])
#   }
#   #return(data)
#   data %>% 
#     dplyr::filter(JYear > end) -> output
#   inv_var(output$SEAKCatch_log,output$model1_sim)
#   
# } 
# # function check for one model (one step ahead MAPE)
# seak_model_summary2 <- f_model_one_step_ahead_inv_var(harvest=log_data$SEAKCatch_log, variables=log_data, model = SEAKCatch_log ~CPUE, start = 1997, end = 2015)
# 
# 
# # function for multiple models (one step ahead inverse_variance)
# f_model_one_step_ahead_multiple_inv_var <- function(harvest,variables,model.formulas,model.names, start, end){
#   n<-dim(variables)[1]
#   model.results<-numeric()
#   obs<-harvest[-n]
#   data<-variables[-n,]
#   fit.out<-list()
#   for(i in 1:length(model.formulas)) 
#   {
#     for (j in (end+1):tail(data$JYear)[6])
#     {
#       fit<-lm(model.formulas[[i]],data = data[data$JYear >= start & data$JYear < j,])
#       fit.out[[i]]<-fit
#       data$model1_sim[data$JYear == j] <- predict(fit, newdata = data[data$JYear == j,])
#     }
#     #return(data)
#     data %>% 
#       dplyr::filter(JYear > end) -> output
#     inverse_variance<-inv_var(output$SEAKCatch_log,output$model1_sim)
#     model.results<-rbind(model.results, inverse_variance= inverse_variance)
#   } 
#   row.names(model.results)<-model.names
#   dimnames(model.results)[[2]][1]<-c('inv_var')
#   as.data.frame(model.results)-> x
#   write.csv(x, file=paste0(results.directory, "/seak_model_summary_one_step_ahead_inv_var.csv"))}




