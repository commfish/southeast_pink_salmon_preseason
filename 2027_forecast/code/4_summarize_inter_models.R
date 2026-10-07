# run code 3_summarize_models.R first/
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
 
# STEP 2: INTERACTION MODELS (vessel-specific CPUE slopes)
temp_vars <- c("ISTI20_JJ",
               paste0(rep(c("Chatham_SST", "Icy_Strait_SST", "NSEAK_SST", "SEAK_SST"), each = 4),
                      "_", c("May", "MJJ", "AMJ", "AMJJ")))
stopifnot(all(temp_vars %in% names(log_data)))

model.names_inter <- setNames(c("no temperature index included", temp_vars),
                              paste0("m", seq_len(length(temp_vars) + 1), "i"))
model_key_inter <- tibble::tibble(Model = names(model.names_inter), Terms = unname(model.names_inter))

inter_terms <- "as.factor(vessel) * adj_raw_pink_log"
other_terms <- "as.factor(odd_even_factor)"

model.formulas_inter <- c(as.formula(paste("SEAKCatch_log ~", inter_terms, "+", other_terms)),
                          lapply(temp_vars, \(v) as.formula(paste("SEAKCatch_log ~", inter_terms,
                                                                  "+", v, "+", other_terms))))
names(model.formulas_inter) <- names(model.names_inter)

# summary statistics of SEAK pink salmon harvest forecast models (seak_model_summary.csv file created)
seak_model_summary_inter <- f_model_summary(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas_inter,model.names=model.names_inter,  models = "_inter")

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

coef_table(model.formulas_inter, log_data_subset) %>%
  write.csv(paste0(results.directory, "model_summary_table1_inter.csv"), row.names = FALSE)

 # calculate one step ahead MAPE
 # https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
 # end year is the year the data is used through (e.g., end = 2014 means that the regression is runs through JYear 2014 and Jyears 2015-2019 are
 # forecasted in the one step ahead process)
 # https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html
 
 # STEP #4: CALCULATE ONE_STEP_AHEAD MAPE
f_model_one_step_ahead_multiple5(harvest=log_data$SEAKCatch_log, variables=log_data, model.formulas=model.formulas_inter,model.names=model.names_inter, start = 1997, end = year.data-6, models="_inter")  # start = 1997, end = 2016 means Jyear 2017-2021 used for MAPE calc. (5-year)

 # if you run the function f_model_one_step_ahead, and do not comment out return(data), you can see how many years of data are used in the MAPE,
 # then you can use the f_model_one_step_ahead function check.xlsx (in the data folder) to make sure the
 # function is correct for the base CPUE model

read.csv(paste0(results.directory, "model_summary_one_step_ahead5_inter.csv"),
         header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>%
  dplyr::rename(Terms = X) %>%
  dplyr::transmute(Terms, MAPE5 = round(MAPE5 * 100, 1)) -> MAPE5_inter

# extrapolation check: leverage of the forecast row, h0 = (se_fit / sigma)^2, compared with the
# largest leverage among fitted years. h0 well above max_hat means the forecast is an extrapolation.
max_hat_inter <- tibble::tibble(Model   = names(seak_model_summary_inter),
                                max_hat = vapply(seak_model_summary_inter, \(m) max(hatvalues(m)), numeric(1)))

read.csv(paste0(results.directory, "model_summary_inter.csv"),
         header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>%
  dplyr::rename(Terms = X) %>%
  dplyr::left_join(model_key_inter, by = "Terms") %>%
  dplyr::left_join(max_hat_inter, by = "Model") -> model_summary_inter

stopifnot(nrow(model_summary_inter) == length(model.names_inter),
          !anyNA(model_summary_inter$Model))            # every row matched to a model label

summary_inter <- model_summary_inter %>%
  dplyr::mutate(Fit        = round(exp(fit + 0.5 * sigma^2), 1),   # bias-corrected point forecast
                Fit_LPI    = round(exp(fit_LPI), 1),               # no bias correction on bounds
                Fit_UPI    = round(exp(fit_UPI), 1),
                AdjR2      = round(AdjR2, 2),
                dAICc      = round(AICc - min(AICc), 1),
                AICc       = round(AICc, 1),
                LOOCV = round(MAPE_LOOCV * 100, 1),
                Lev_fc     = round((se_fit / sigma)^2, 2),
                Lev_max    = round(max_hat, 2)) %>%
  dplyr::select(Model, Terms, Fit, Fit_LPI, Fit_UPI, AdjR2, AICc, dAICc, LOOCV, Lev_fc, Lev_max) %>%
  dplyr::left_join(MAPE5_inter, by = "Terms") %>%
  dplyr::arrange(dAICc)

stopifnot(nrow(summary_inter) == length(model.names_inter),   # no rows lost or duplicated
          !anyNA(summary_inter$MAPE5))                        # every model matched in MAPE5

write.csv(summary_inter, paste0(results.directory, "model_summary_table2_inter.csv"), row.names = FALSE)

# STEP #5: CREATE FORECAST FIGURE
model_summary_inter %>%
  dplyr::mutate(model  = factor(Model, levels = names(model.names_inter)),
                fit_bc = exp(fit + 0.5 * sigma^2),
                LPI    = exp(fit_LPI),
                UPI    = exp(fit_UPI)) %>%
  dplyr::select(model, Terms, fit_bc, LPI, UPI) -> results_inter

y_max <- ceiling(max(results_inter$UPI) / 100) * 120

ggplot(results_inter, aes(x = model, y = fit_bc)) +
  geom_col(fill = "lightgrey", colour = "grey70", width = 1) +
  geom_errorbar(aes(ymin = LPI, ymax = UPI),
                width = 0.2, linewidth = 1, colour = "grey30") +
  geom_hline(yintercept = mean(results_inter$fit_bc), linetype = "dashed", colour = "grey30") +
  scale_y_continuous(breaks = seq(0, y_max, 100), limits = c(0, y_max), expand = c(0, 0)) +
  labs(x = "", y = "2027 SEAK Pink Salmon Harvest Forecast (millions)") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(size = 9),
        legend.position  = "none") -> plot_inter

ggsave(paste0(results.directory, "forecast_models_inter.png"), plot = plot_inter, dpi = 500, height = 4, width = 10, units = "in")

# create final table for report
summary_inter %>%
  dplyr::arrange(MAPE5, dAICc) %>%
  dplyr::select(Terms, Model, Fit, Fit_LPI, Fit_UPI, AdjR2, MAPE5, LOOCV, AICc_change = dAICc,
                Lev_fc, Lev_max) %>%
  write.csv(paste0(results.directory, "model_summary_final_inter.csv"), row.names = FALSE)

# STEP #6: ADDITIVE vs INTERACTION COMPARISON
# Both model sets use the same years and response, so AICc is comparable across them.
# dAICc here is relative to the best model across all 36. Requires the additive script's output.
additive_file <- paste0(results.directory, "model_summary_table2_multi.csv")
if (file.exists(additive_file)) {
  additive <- read.csv(additive_file, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  
  dplyr::bind_rows(additive, summary_inter) %>%
    dplyr::mutate(dAICc = round(AICc - min(AICc), 1),
                  PI    = sprintf("%.1f-%.1f", Fit_LPI, Fit_UPI),
                  Terms = Terms %>%
                    gsub("no temperature index included", "None", .) %>%
                    gsub("_SST_", " ", .) %>%
                    gsub("Icy_Strait", "Icy Strait", .)) %>%
    dplyr::arrange(MAPE5, dAICc) %>%
    dplyr::select(Model, Temp = Terms, Fit, `80% PI` = PI, AdjR2,
                  dAICc, MAPE5, LOOCV) -> comparison
  
  write.csv(comparison, paste0(results.directory, "model_summary_comparison.csv"), row.names = FALSE)
} else {
  message("Skipping comparison: run 2027_pink_forecast_models.R first to create ", additive_file)
}