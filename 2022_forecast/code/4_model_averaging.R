# source code and functions
source('2022_forecast/code/1_summarize_models.r')
source('2022_forecast/code/functions.r')
# https://www.mm218.dev/posts/2021/01/model-averaging/
# Mahoney (2021, Jan. 18). Mike Mahoney: Model averaging methods: how and why to build ensemble models. Retrieved from https://www.mm218.dev/posts/2021/01/model-averaging/
# https://rdrr.io/github/padpadpadpad/rTPC/f/vignettes/model_averaging_selection.Rmd
# https://stats.stackexchange.com/questions/155305/how-does-r-calculate-prediction-intervals-in-the-forecast-package
# https://www.investopedia.com/ask/answers/042415/what-difference-between-standard-error-means-and-standard-deviation.asp

read.csv(file.path(results.directory,'seak_model_summary.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> results
results %>% 
  dplyr::rename(terms = 'X') %>% 
  dplyr::select(terms, fit,	se.fit, fit_LPI,	fit_UPI, AICc,	sigma, df) %>%
  mutate(sigma = round(sigma,3),
         stdev = (se.fit * sqrt(df +1)))%>%
  mutate(var_yhat = stdev * stdev) %>%
  mutate(model = c('m1','m2','m3','m4','m5','m6','m7','m8',
                   'm9','m10','m11','m12','m13','m14','m15','m16',' m17',
                   'm18','m19','m20','m21','m22','m23','m24','m25')) %>% 
  mutate(fit_bias_corrected = fit+((sigma*sigma)/2)) %>%         
  mutate(delta = AICc-min(AICc)) %>%
  mutate(relLik = exp(-0.5 * delta)) %>%
  mutate(aicc_weight = relLik/sum(relLik)) %>%
  mutate(aicc_weights_pred = sum(fit_bias_corrected*aicc_weight)) %>%
  mutate(step1 = ((fit_bias_corrected - aicc_weights_pred)^2) + var_yhat) %>%
  mutate(step2 = sqrt(step1) *aicc_weight) %>%
  mutate(var_y_pred = (sum(step2))^2) %>%
  mutate(fit_LPI_80 = (aicc_weights_pred)-(1.28*sqrt(var_y_pred)),
         fit_UPI_80 = (aicc_weights_pred)+(1.28*sqrt(var_y_pred))) %>%
  mutate(exp_fit = exp(aicc_weights_pred),
         exp_fit_LPI_80 = exp(fit_LPI_80),
         exp_fit_UPI_80 = exp(fit_UPI_80)) %>%
  write.csv(paste0(results.directory, "/model_summary_table4.csv"), row.names = F)

# STOP HERE*******************************************************************************
#****************************************************************************************
# MuMIN package
# use the model.sel function to conduct model selection
# and put output into object out.put
out.put<-model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,
                   m17,m18,m19,m20,m21,m22,m23,m24,m25)
out.put

# select models with delta AICc less than 5
sub <- subset(out.put, delta <5)

# select models using Royall's 1/8 rule for strength of evidence
# IMPORTANT: Weights have been renormalized!!
subset(out.put, 1/8 < weight/max(out.put$weight))

# select models 95% cumulative weight criteria
# IMPORTANT: Weights have been renormalized!!
subset(out.put, cumsum(out.put$weight) <= .95)

# coerce the object out.put into a data frame
sel.table<-as.data.frame(out.put)
sel.table<-as.data.frame(out.put)[29:33]
sel.table
# number of parameters (df) should be K
names(sel.table)[1] = "K"
## lets be sure to put the model names in a column
sel.table$model<-rownames(sel.table)
sel.table<-sel.table[,c(6,1,2,3,4,5)]
sel.table
AICc5 <- get.models(out.put, subset =  delta < 5)
MA.ests<-model.avg(AICc5, revised.var = TRUE)

# https://nwfsc-timeseries.github.io/atsa-labs/sec-uss-comparing-models-with-aic-and-model-weights.html

# Predictions from each of the models in a set, and with averaged coefficients
pred <- data.frame(
  model = sapply(AICc5, predict, newdata = new_data),
  averaged.subset = predict(MA.ests, new_data, full = FALSE),
  averaged.full = predict(MA.ests, new_data, full = TRUE))
screenreg(MA.ests)
summary(MA.ests)$coefmat.full
summary(MA.ests)$coefmat.subset
summary(MA.ests) # Note there are two sets of estimates: the “full” coefficients set terms to 0 if 
# they are not included in the model while averaging, whereas the “conditional” coefficients ignores 
# the predictors entirely. The “full” coefficients are thus more conservative and it is best practice 
# to interpret these. Finally, the last part of the output tells us in how many models each of the terms was included.
# Model averaging is a mean to incorporate model selection uncertainty. 
# Here, the parameter estimates for each candidate model are weighted using their corresponding model weights 
# and summed. There are two methods for model-averaging defined by Burnham and Anderson as , where parameter 
# estimates are averaged over all models in which predictor xj occurs and  where parameter estimates are 
# averaged over all models not just those in which predictor xj occurs. MuMIn function model.avg conducts both types of model averaging and reports the first type of model averaging as “subset” and the second type as “full.

# For comparison, prediction obtained by averaging predictions of the component
# models
pred.se <- predict(MA.ests, new_data, se.fit = TRUE)
y <- pred.se$fit
ci <- pred.se$se.fit  * 2

pred.parms<-get.models(out.put, subset= delta < 5)
# predict values using each model, here were just using the
# the example dataset, you could use a new dataset
model.preds = sapply(pred.parms, predict, newdata = new_data)


