# functions

mape <- function(actual, predicted){
  mean(abs((actual - predicted)/actual))}

jacklm.reg<-function(data,model.formula,jacknife.index=0){
  if(jacknife.index>0) {
    var.fit<-data[-jacknife.index,]
    var.pred<-data[jacknife.index,]
  }
  jack.lm<-lm(model.formula,data=var.fit)
  predict(jack.lm,newdata=var.pred)
}


f_model_one_step_ahead_multiple5 <- function(harvest, variables, model.formulas, model.names,
                                             start, end, models, bias_correct = TRUE,
                                             results.directory = get("results.directory", envir = .GlobalEnv)) {
  data <- variables[!is.na(harvest), ]                # drop the forecast row, wherever it is
  fc_years <- (end + 1):max(data$JYear)               # e.g. end = 2020 -> JYear 2021-2025
  
  mape5 <- vapply(model.formulas, function(f) {
    pred <- vapply(fc_years, function(j) {
      fit <- lm(f, data = data[data$JYear >= start & data$JYear < j, ])
      mu  <- predict(fit, newdata = data[data$JYear == j, ])
      exp(mu + if (bias_correct) sigma(fit)^2 / 2 else 0)
    }, numeric(1))
    obs <- exp(data$SEAKCatch_log[match(fc_years, data$JYear)])
    Metrics::mape(obs, pred)
  }, numeric(1))
  
  out <- data.frame(MAPE5 = mape5, row.names = model.names)
  write.csv(out, paste0(results.directory, "model_summary_one_step_ahead5", models, ".csv"))
  invisible(out)
}



f_model_summary <- function(harvest, variables, model.formulas, model.names, models,
                            w = NULL, results.directory = get("results.directory", envir = .GlobalEnv),
                            level = 0.80) {
  
  if (is.null(w)) w <- rep(1, nrow(variables))
  variables$.w <- w
  
  fc_row <- which(is.na(harvest))
  stopifnot(length(fc_row) == 1)                 # exactly one forecast row
  data     <- variables[-fc_row, ]
  newdata  <- variables[fc_row, ]
  obs_log  <- harvest[-fc_row]
  obs      <- exp(obs_log)                       # observed catch (millions)
  n_fit    <- nrow(data)
  
  fit.out <- vector("list", length(model.formulas))
  names(fit.out) <- names(model.formulas)
  model.results <- vector("list", length(model.formulas))
  
  for (i in seq_along(model.formulas)) {
    f   <- model.formulas[[i]]
    fit <- lm(f, data = data, weights = .w)
    fit.out[[i]] <- fit
    s   <- summary(fit)
    s2  <- sigma(fit)^2
    
    # leave-one-out predictions, bias-corrected, on the catch scale
    loo <- vapply(seq_len(n_fit), function(j) {
      fj <- lm(f, data = data[-j, ], weights = .w)
      exp(predict(fj, newdata = data[j, ]) + sigma(fj)^2 / 2)
    }, numeric(1))
    
    fitted_catch <- exp(fitted(fit) + s2 / 2)
    
    p_obj <- predict(fit, newdata = newdata, se.fit = TRUE,
                     interval = "prediction", level = level)
    
    f_stat <- s$fstatistic
    p_val  <- if (!is.null(f_stat)) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else NA
    
    model.results[[i]] <- data.frame(
      fit        = p_obj$fit[1, "fit"],          # log scale
      fit_LPI    = p_obj$fit[1, "lwr"],          # log scale
      fit_UPI    = p_obj$fit[1, "upr"],          # log scale
      se_fit     = p_obj$se.fit[1],
      R2         = s$r.squared,
      AdjR2      = s$adj.r.squared,
      AIC        = AIC(fit),
      AICc       = AICcmodavg::AICc(fit),
      BIC        = BIC(fit),
      p          = unname(p_val),
      sigma      = sigma(fit),
      MAPE       = mean(abs(obs - fitted_catch) / obs),   # catch scale, in-sample
      MAPE_LOOCV = mean(abs(obs - loo) / obs)             # catch scale, leave-one-out
    )
  }
  
  model.results <- do.call(rbind, model.results)
  rownames(model.results) <- model.names
  
  write.csv(model.results, paste0(results.directory, "model_summary", models, ".csv"),
            row.names = TRUE)
  
  invisible(fit.out)
}
