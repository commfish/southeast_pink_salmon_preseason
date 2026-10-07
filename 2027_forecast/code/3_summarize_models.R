# SECM Pink salmon forecast models
# last update: September 2026
# pink_cal_pooled_species
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
# https://www.r-bloggers.com/2021/10/multiple-linear-regression-made-simple/

# load libraries
if (!requireNamespace("fngr", quietly = TRUE)) devtools::install_github("commfish/fngr")
library("fngr") # theme report
library(broom)
library(tidyverse)
library(extrafont)
library(Metrics) # MASE calc
library(MetricsWeighted)
library("RColorBrewer") 
#extrafont::font_import() # only need to run this once, then comment out
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2027_forecast" # forecast year 
year.data <- 2026 # last year of data
year.data.one <- year.data - 1
data.directory    <- paste0(file.path(year.forecast, "data"), "/")
results.directory <- paste0(file.path(year.forecast, "results"), "/")
source(file.path(year.forecast, "code", "functions.r"))

# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory,'var2026_final.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) -> variables # update file names

variables %>%
  mutate (odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"), # coded to reflect Year NOT Year
          SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
    filter(!vessel %in% c("Steller", "Chellissa")) %>% # exclude these years as only one data point
    dplyr::select(-c(SEAKCatch)) -> log_data

stopifnot(sum(is.na(log_data$SEAKCatch_log)) == 1,           # only the forecast row lacks catch
         is.na(log_data$SEAKCatch_log[log_data$JYear == year.data]))

# STEP 2: MODELS
temp_vars <- c("ISTI20_JJ",
               paste0(rep(c("Chatham_SST", "Icy_Strait_SST", "NSEAK_SST", "SEAK_SST"), each = 4),
                      "_", c("May", "MJJ", "AMJ", "AMJJ")))
stopifnot(all(temp_vars %in% names(log_data)))

model.names <- setNames(c("no temperature index included", temp_vars),
                        paste0("m", seq_len(length(temp_vars) + 1)))

model_key <- tibble::tibble(Model = names(model.names), Terms = unname(model.names))

base_terms <- "as.factor(odd_even_factor) + as.factor(vessel) + adj_raw_pink_log"

model.formulas <- c(as.formula(paste("SEAKCatch_log ~", base_terms)),
                    lapply(temp_vars, \(v) as.formula(paste("SEAKCatch_log ~", v, "+", base_terms))))
names(model.formulas) <- names(model.names)

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, models = "_multi")

# STEP #3: SUMMARY OF MODEL FITS
# summary of model fits (i.e., coefficients, p-value); creates the file model_summary_table1.csv.
log_data %>%
  dplyr::filter(JYear < year.data) -> log_data_subset

coef_table <- function(formulas, data) {
  fits <- lapply(formulas, lm, data = data)
  purrr::map_dfr(fits, broom::tidy, .id = "Model") %>%
    dplyr::transmute(Model,
                     Term               = term,
                     Estimate           = round(estimate, 3),
                     `Standard Error`   = round(std.error, 3),
                     Statistic          = round(statistic, 3),
                     `p value`          = round(p.value, 3))}

coef_table(model.formulas, log_data_subset) %>%
  write.csv(file.path(results.directory, "model_summary_table1_multi.csv"), row.names = FALSE)


 # calculate one step ahead MAPE
 # https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
 # end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
 # forecasted in the one step ahead process)
 # https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html
 
# STEP #4: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas,model.names=model.names, start = 1997, end = year.data-6, models="_multi")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)

 # if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
 # then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
 # function is correct for the base CPUE model
 
read.csv(file.path(results.directory, 'model_summary_one_step_ahead5_multi.csv'),
        header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>%
  dplyr::rename(Terms = X) %>%
  dplyr::mutate(MAPE5 = round(MAPE5 * 100, 1)) %>%
  dplyr::select(Terms, MAPE5) -> MAPE5
 
# read model summary; Fit is the bias-corrected mean, Fit_LPI/Fit_UPI are lognormal quantiles
read.csv(file.path(results.directory, 'model_summary_multi.csv'),
         header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>%
  dplyr::rename(Terms = X) %>%
  dplyr::left_join(model_key, by = "Terms") -> model_summary

stopifnot(nrow(model_summary) == length(model.names),
          !anyNA(model_summary$Model))                  # every row matched to a model label

summary_tbl <- model_summary %>%
  dplyr::mutate(Fit        = round(exp(fit + 0.5 * sigma^2), 1),   # bias-corrected point forecast
                Fit_LPI    = round(exp(fit_LPI), 1),               # no bias correction on bounds
                Fit_UPI    = round(exp(fit_UPI), 1),
                AdjR2      = round(AdjR2, 2),
                dAICc      = round(AICc - min(AICc), 1),
                AICc       = round(AICc, 1),
                LOOCV = round(MAPE_LOOCV * 100, 1)) %>%
  dplyr::select(Model, Terms, Fit, Fit_LPI, Fit_UPI, AdjR2, AICc, dAICc, LOOCV) %>%
  dplyr::left_join(MAPE5, by = "Terms") %>%
  dplyr::arrange(dAICc)

stopifnot(nrow(summary_tbl) == length(model.names),   # no rows lost or duplicated
          !anyNA(summary_tbl$MAPE5))                  # every model matched in MAPE5

write.csv(summary_tbl, file.path(results.directory, "model_summary_table2_multi.csv"), row.names = FALSE)
 
# STEP #5: CREATE FORECAST FIGURE
model_summary %>%
  dplyr::mutate(model  = factor(Model, levels = names(model.names)),
                fit_bc = exp(fit + 0.5 * sigma^2),
                LPI    = exp(fit_LPI),
                UPI    = exp(fit_UPI)) %>%
  dplyr::select(model, Terms, fit_bc, LPI, UPI) -> results

y_max <- ceiling(max(results$UPI) / 20) * 22     # e.g. 188 -> 200

ggplot(results, aes(x = model, y = fit_bc)) +
  geom_col(fill = "lightgrey", colour = "grey70", width = 1) +
  geom_errorbar(aes(ymin = LPI, ymax = UPI),
                width = 0.2, linewidth = 1, colour = "grey30") +
  geom_hline(yintercept = mean(results$fit_bc), linetype = "dashed", colour = "grey30") +
  scale_y_continuous(breaks = seq(0, y_max, 20), limits = c(0, y_max), expand = c(0, 0)) +
  labs(x = "", y = "2027 SEAK Pink Salmon Harvest Forecast (millions)") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(size = 9),
        legend.position  = "none") -> plot1

ggsave(paste0(results.directory, "forecast_models_multi.png"), plot = plot1,
       dpi = 500, height = 4, width = 10, units = "in")

# create final table for report
summary_tbl %>%
  dplyr::arrange(MAPE5, dAICc) %>%
  dplyr::select(Terms, Model, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE5, LOOCV, AICc_change = dAICc) %>%
  write.csv(paste0(results.directory, "model_summary_final_multi.csv"), row.names = FALSE)

 