##########
# inverse variance weighting of Yukon River Chinook salmon forecast models
# Jim Murphy 3/4/21
##############
library(dplyr)
library(tidyr)
library(ggplot2)

# read data
setwd('C:/Users/jim.murphy/Work/Projects/2021/JTC spring/juvenile forecast')
forecasts<-read.csv('ChinCanForecast.csv')

current_year<-2021
CI<-c(.8,.9,.95)

#Forecasts for current year, select only the forecast variables
forecast_pred<-filter(forecasts,Year==current_year)
forecast_pred<-select(forecast_pred,Ricker,Sibling,Juvenile)

#select forecasts prior to current year and years without juvenile forecasts
forecasts<-filter(forecasts,Year<current_year)
forecasts<-filter(forecasts,!is.na(Juvenile))
n<-dim(forecasts)[1]

#Estimate Inverse variance weighted forecast
ricker<-sum((forecasts$Ricker-forecasts$Observed)^2)/(n-1)
sibling<-sum((forecasts$Sibling-forecasts$Observed)^2)/(n-1)
juvenile<-sum((forecasts$Juvenile-forecasts$Observed)^2)/(n-1)
forecast_wt<-c(1/ricker,1/sibling,1/juvenile)
forecast_wt<-forecast_wt/sum(forecast_wt)
combined<-apply(t(select(forecasts,Ricker,Sibling,Juvenile))*forecast_wt,2,sum)
forecasts<-cbind(forecasts,Combined=combined)
forecast_pred<-unlist(c(forecast_pred,Combined=sum(forecast_pred*forecast_wt)))
lnforecast_pred<-log(forecast_pred)

#Estimate error in log space
lnricker_sd<-sqrt(sum((log(forecasts$Ricker)-log(forecasts$Observed))^2)/(n-1))
lnsibling_sd<-sqrt(sum((log(forecasts$Sibling)-log(forecasts$Observed))^2)/(n-1))
lnjuvenile_sd<-sqrt(sum((log(forecasts$Juvenile)-log(forecasts$Observed))^2)/(n-1))
lncombined_sd<-sqrt(sum((log(forecasts$Combined)-log(forecasts$Observed))^2)/(n-1))
lnforecast_sd<-c(Ricker=lnricker_sd,Sibling=lnsibling_sd,Juvenile=lnjuvenile_sd,Combined=lncombined_sd)

forecast_pars<-data.frame(Mean=lnforecast_pred,SD=lnforecast_sd,
                      Method=names(lnforecast_pred),N=n)

#Estimate CIs
CI_fun<-function(pars,CI){
  pars<-filter(pars,Method=='Combined')
  m<-pars$Mean
  sd<-pars$SD
  n<-pars$N
  zcrit <- qnorm((1 + CI) / 2)
  c(exp((m-zcrit*sd)-(sd^2)/2),exp((m+zcrit*sd)-(sd^2)/2))
}

forecasts_CI<-as.data.frame(rbind(
          CI_fun(forecast_pars,CI=CI[1]),
          CI_fun(forecast_pars,CI=CI[2]),
          CI_fun(forecast_pars,CI=CI[3])))

names(forecasts_CI)<-c('Lower','Upper')


#################
# Build pdfs
#################

pdfp<-data.frame(Probability=seq(.99,.01,by=-.01),
                 RunSize_in_logspace=lnforecast_pred['Combined']+
                   lnforecast_sd['Combined']*qnorm(1:99/100))
pdfp<-data.frame(pdfp,
                 RunSize=exp(pdfp$RunSize_in_logspace-
                               lnforecast_sd['Combined']^2/2))
pdfp<-data.frame(pdfp,
                 Upper_IMEG=pdfp$RunSize-55000,
                 Middle_IMEG=pdfp$RunSize-48750,
                 Lower_IMEG=pdfp$RunSize-42500)
pdfp[pdfp<0]<-0 # change negative harvests to zero

x <- seq(10000, 200000, by=1000)
pdfd <- mapply(dlnorm, mean = forecast_pars$Mean, sd = forecast_pars$SD, MoreArgs = list(x = x),
                 SIMPLIFY = FALSE)

# scale pdfs_d to sum to 1
scale_to_one<-function(x) x/sum(x)
pdfd<-lapply(pdfd,scale_to_one)

# add names
names(pdfd) <- forecast_pars$Method

# convert to dataframe
pdfd <- do.call(cbind.data.frame, pdfd)

# scale run size to 1,000's
pdfd$x <- x/1000

# convert dataframe to tall format
forecast_pdfd <- gather(pdfd, Method, Density, -x)

############
#output
############
forecast
write.csv(x = forecast_pdfd, file = 'forecast_pdf.csv')
write.csv(x = pdfp,file = 'forecast_harvest_prob.csv')
write.csv(x= forecast_pars, file = 'forecast_pars.csv')

#############
# Plotting
############
# plot three forecast model pdfs
filter(forecast_pdfd,Method!='Combined') %>% 
ggplot(aes(color = Method, x = x, y = Density))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=c('blue','grey','red'))+
  xlab('Run Size (1,000s)')+
  ylab('Density')+
  theme_classic()+
  theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top"))+
  scale_x_continuous(breaks=seq(10,130,10),limits=c(10,120))+
  scale_y_continuous(limits=c(0,.033))

ggsave('forecast models.jpg',device='jpeg')

#plot combined pdf with CIs
filter(forecast_pdfd,Method=='Combined') %>% 
ggplot(aes(x = x, y = Density)) +
  geom_area(fill = "lightblue")+
 geom_vline(aes(xintercept=forecasts_CI$Lower[3]/1000),
             color="grey", linetype="dashed", size=.5)+
  geom_vline(aes(xintercept=forecasts_CI$Upper[3]/1000),
             color="grey", linetype="dashed", size=.5)+
  geom_vline(aes(xintercept=forecasts_CI$Lower[2]/1000),
             color="blue", linetype="dashed", size=.5)+
  geom_vline(aes(xintercept=forecasts_CI$Upper[2]/1000),
             color="blue", linetype="dashed", size=.5)+
  geom_vline(aes(xintercept=forecasts_CI$Lower[1]/1000),
            color="blue", size=1)+
  geom_vline(aes(xintercept=forecasts_CI$Upper[1]/1000),
             color="blue", size=1)+
    xlab('Run Size (1,000s)')+
  ylab('Density')+
  theme_classic()+
  scale_x_continuous(breaks=seq(10,130,10),limits=c(10,120))+
  scale_y_continuous(limits=c(0,.033))+
  annotate(x=forecasts_CI$Lower[3]/1000,y=.03,label=paste(round(forecasts_CI$Lower[3]/1000,0),'  ',CI[3]*100,'% CI',sep=''),vjust=0,geom='text',angle=90,size=4)+
  annotate(x=forecasts_CI$Lower[1]/1000,y=.03,label=paste(round(forecasts_CI$Lower[1]/1000,0),'  ',CI[1]*100,'% CI',sep=''),vjust=1,geom="text",angle=90,size=4)+
  annotate(x=forecasts_CI$Upper[3]/1000,y=.03,label=paste(round(forecasts_CI$Upper[3]/1000,0),'  ',CI[3]*100,'% CI',sep=''),vjust=1,geom='text',angle=90,size=4)+
  annotate(x=forecasts_CI$Upper[1]/1000,y=.03,label=paste(round(forecasts_CI$Upper[1]/1000,0)-1,'  ',CI[1]*100,'% CI',sep=''),vjust=-.3,geom="text",angle=90,size=4)

ggsave('combined forecast.jpg',device='jpeg')

# convert dataframe to tall format
forecast_pdfp <- select(pdfp, Probability, RunSize, Lower_IMEG, Middle_IMEG,Upper_IMEG)
forecast_pdfp<-gather(forecast_pdfp, Goal, Harvest, -Probability, -RunSize)
filter(forecast_pdfp,Probability>=.5) %>% 
  ggplot(aes(x=Probability,y=Harvest,color=Goal))+
  geom_line(size=1)+
  theme_classic()+
  scale_color_manual(values=c('red','black','green'))+
  xlab('Probability of Meeting or Exceeding Goal')+
  ylab('Harvest Guidance')+
  theme_classic()+
  theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top"))+
  scale_x_continuous(breaks=seq(.5,1,.05),limits=c(.5,1))+
  scale_y_continuous(breaks=seq(0,16000,2000),limits=c(0,16000))+
  theme(panel.grid.major = element_line(colour="grey", size = (.5)),
        panel.grid.minor = element_line(size = (0.2), linetype='dashed',colour="grey"))

ggsave('harvest guidance.jpg',device='jpeg')


