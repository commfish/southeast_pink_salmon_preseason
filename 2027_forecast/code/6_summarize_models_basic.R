# run code 5_diagnostics_models.R first
# STEP 1: DATA
# SECM Pink salmon forecast models: calibrated CPUE (m1a-m18a)
# last update: October 2026

# load libraries
if (!requireNamespace("fngr", quietly = TRUE)) devtools::install_github("commfish/fngr")
library(fngr)       # theme_report()
library(tidyverse)  # dplyr, ggplot2, purrr
library(broom)      # tidy() for the coefficient table

if (.Platform$OS.type == "windows") windowsFonts(Times = windowsFont("Times New Roman"))
theme_set(theme_report(base_size = 14))

# inputs
year.forecast <- "2027_forecast" # forecast year
year.data <- 2026 # last year of data
data.directory    <- paste0(file.path(year.forecast, "data"), "/")
results.directory <- paste0(file.path(year.forecast, "results"), "/")
source(file.path(year.forecast, "code", "functions.r"))

# STEP 1: DATA
# read in data from the csv file  (make sure this is up to date)
read.csv(file.path(data.directory, 'var2026_final.csv'), header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) -> variables # update file names

# odd_even_factor labels the RETURN year (Year = JYear + 1): an even JYear returns in an odd year
variables %>%
  mutate(odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),
         SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(-c(SEAKCatch)) -> log_data

stopifnot(sum(is.na(log_data$SEAKCatch_log)) == 1,           # only the forecast row lacks catch
          is.na(log_data$SEAKCatch_log[log_data$JYear == year.data]))

# STEP 2: MODELS
temp_vars <- c("ISTI20_JJ",
               paste0(rep(c("Chatham_SST", "Icy_Strait_SST", "NSEAK_SST", "SEAK_SST"), each = 4),
                      "_", c("May", "MJJ", "AMJ", "AMJJ")))
stopifnot(all(temp_vars %in% names(log_data)))

model.names <- setNames(c("no temperature index included", temp_vars),
                        paste0("m", seq_len(length(temp_vars) + 1), "a"))
model_key <- tibble::tibble(Model = names(model.names), Terms = unname(model.names))

base_terms <- "CPUE + as.factor(odd_even_factor)"

model.formulas <- c(as.formula(paste("SEAKCatch_log ~", base_terms)),
                    lapply(temp_vars, \(v) as.formula(paste("SEAKCatch_log ~", base_terms, "+", v))))
names(model.formulas) <- names(model.names)

# summary statistics (model_summary.csv file created); returns the fitted models
seak_model_summary <- f_model_summary(harvest = log_data$SEAKCatch_log, variables = log_data,
                                      model.formulas = model.formulas, model.names = model.names,
                                      models = "")

# STEP #3: SUMMARY OF MODEL FITS
# coefficients and p-values; creates model_summary_table1.csv
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
  write.csv(paste0(results.directory, "model_summary_table1.csv"), row.names = FALSE)

# STEP #4: CALCULATE ONE-STEP-AHEAD MAPE
# end = year.data - 6 = 2020: models are refit through JYear 2020, ..., 2024 and used to
# forecast JYear 2021-2025 (5 years).
# https://stackoverflow.com/questions/37661829/r-multivariate-one-step-ahead-forecasts-and-accuracy
# https://nwfsc-timeseries.github.io/atsa-labs/sec-dlm-forecasting-with-a-univariate-dlm.html
f_model_one_step_ahead_multiple5(harvest = log_data$SEAKCatch_log, variables = log_data,
                                 model.formulas = model.formulas, model.names = model.names,
                                 start = 1997, end = year.data - 6, models = "")

read.csv(paste0(results.directory, "model_summary_one_step_ahead5.csv"),
         header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>%
  dplyr::rename(Terms = X) %>%
  dplyr::transmute(Terms, MAPE5 = round(MAPE5 * 100, 1)) -> MAPE5

# extrapolation check: forecast-row leverage h0 = (se_fit / sigma)^2 vs largest in-sample leverage
# https://stats.stackexchange.com/questions/359088/correcting-log-transformation-bias-in-a-linear-model
max_hat <- tibble::tibble(Model   = names(seak_model_summary),
                          max_hat = vapply(seak_model_summary, \(m) max(hatvalues(m)), numeric(1)))

read.csv(paste0(results.directory, "model_summary.csv"),
         header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>%
  dplyr::rename(Terms = X) %>%
  dplyr::left_join(model_key, by = "Terms") %>%
  dplyr::left_join(max_hat, by = "Model") -> model_summary

stopifnot(nrow(model_summary) == length(model.names),
          !anyNA(model_summary$Model))                  # every row matched to a model label

summary_tbl <- model_summary %>%
  dplyr::mutate(Fit        = round(exp(fit + 0.5 * sigma^2), 1),   # bias-corrected point forecast
                Fit_LPI    = round(exp(fit_LPI), 1),               # no bias correction on bounds
                Fit_UPI    = round(exp(fit_UPI), 1),
                AdjR2      = round(AdjR2, 2),
                dAICc      = round(AICc - min(AICc), 1),
                AICc       = round(AICc, 1),
                MAPE_LOOCV = round(MAPE_LOOCV * 100, 1),
                Lev_fc     = round((se_fit / sigma)^2, 2),
                Lev_max    = round(max_hat, 2)) %>%
  dplyr::select(Model, Terms, Fit, Fit_LPI, Fit_UPI, AdjR2, AICc, dAICc, MAPE_LOOCV, Lev_fc, Lev_max) %>%
  dplyr::left_join(MAPE5, by = "Terms") %>%
  dplyr::arrange(dAICc)

stopifnot(nrow(summary_tbl) == length(model.names),   # no rows lost or duplicated
          !anyNA(summary_tbl$MAPE5))                  # every model matched in MAPE5

write.csv(summary_tbl, paste0(results.directory, "model_summary_table2.csv"), row.names = FALSE)

# STEP #5: CREATE FORECAST FIGURE
model_summary %>%
  dplyr::mutate(model  = factor(Model, levels = names(model.names)),
                fit_bc = exp(fit + 0.5 * sigma^2),
                LPI    = exp(fit_LPI),
                UPI    = exp(fit_UPI)) %>%
  dplyr::select(model, Terms, fit_bc, LPI, UPI) -> results

y_max <- ceiling(max(results$UPI) / 20) * 20      # e.g. 121 -> 140
plot_family <- if (.Platform$OS.type == "windows") "Times" else "serif"

ggplot(results, aes(x = model, y = fit_bc)) +
  geom_col(fill = "lightgrey", colour = "grey70", width = 1) +
  geom_errorbar(aes(ymin = LPI, ymax = UPI),
                width = 0.2, linewidth = 1, colour = "grey30") +
  geom_hline(yintercept = mean(results$fit_bc), linetype = "dashed", colour = "grey30") +
  scale_y_continuous(breaks = seq(0, y_max, 20), limits = c(0, y_max), expand = c(0, 0)) +
  labs(x = "", y = "2027 SEAK Pink Salmon Harvest Forecast (millions)") + # update forecast year
  theme_bw(base_family = plot_family) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(size = 9),
        legend.position  = "none") -> plot1

ggsave(paste0(results.directory, "forecast_models.png"), plot = plot1,
       dpi = 500, height = 4, width = 10, units = "in")

# create final table for report (compact layout so it fits a portrait page)
summary_tbl %>%
  dplyr::arrange(MAPE5, dAICc) %>%
  dplyr::mutate(PI    = paste0(Fit_LPI, "-", Fit_UPI),
                Terms = Terms %>%
                  gsub("no temperature index included", "None", .) %>%
                  gsub("_SST_", " ", .) %>%
                  gsub("Icy_Strait", "Icy Strait", .)) %>%
  dplyr::select(Model, Temp = Terms, Fit, `80% PI` = PI, AdjR2,
                dAICc, MAPE5, LOOCV = MAPE_LOOCV) %>%
  write.csv(paste0(results.directory, "model_summary_final.csv"), row.names = FALSE)

# STEP 6: CREATE DATASET FOR WRITE-UP
variables %>%
  dplyr::transmute(JYear, Year,
                   Harvest          = round(SEAKCatch, 1),
                   odd_even_factor  = ifelse(JYear %% 2 == 0, "odd", "even"),
                   vessel,
                   adj_raw_pink_log = round(adj_raw_pink_log, 2),
                   CPUE             = round(CPUE, 2)) %>%
  write.csv(paste0(data.directory, "data.csv"), row.names = FALSE)