# run code 6_summarize_models_basic.R first
# SECM Pink salmon forecast: diagnostics for the selected model
# last update: October 2026

# load libraries
library(tidyverse)  # dplyr, ggplot2
library(broom)      # augment()
library(ggfortify)  # autoplot() for lm diagnostics
library(car)        # outlierTest(), residualPlots()
library(cowplot)    # plot_grid()

# SELECTED MODEL (update each year) 
model    <- "m13a"            # label as in model_summary_table2.csv
temp_var <- "NSEAK_SST_AMJ"   # temperature variable in that model (NULL if none)

# inputs
year.forecast <- "2027_forecast" # forecast year
year.data <- 2026 # last year of data
data.directory    <- paste0(file.path(year.forecast, "data"), "/")
results.directory <- paste0(file.path(year.forecast, "results"), "/")
dir.create(results.directory, showWarnings = FALSE, recursive = TRUE)
plot_family <- if (.Platform$OS.type == "windows") "Times" else "serif"
if (.Platform$OS.type == "windows") windowsFonts(Times = windowsFont("Times New Roman"))

# axis breaks with labels every `to` units (from fngr-style tickr)
tickr <- function(data, var, to) {
  VAR <- enquo(var)
  data %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = !!VAR, labels)
}

# shared plot theme
theme_diag <- function(base_size = 10) {
  theme_bw(base_size = base_size, base_family = plot_family) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border     = element_blank(),
          axis.line        = element_line(colour = "black"),
          legend.position  = "none")
}

# panel letter in the top-left corner
panel_label <- function(letter) {
  annotate("text", x = -Inf, y = Inf, label = letter, hjust = -0.5, vjust = 1.5,
           family = plot_family, size = 5)
}

# round an axis limit up/down to a multiple of `to`
up_to   <- function(x, to) ceiling(max(x, na.rm = TRUE) / to) * to
down_to <- function(x, to) floor(min(x, na.rm = TRUE) / to) * to

# input data
read.csv(file.path(data.directory, 'var2026_final.csv'), header = TRUE,
         stringsAsFactors = FALSE, strip.white = TRUE) %>%
  mutate(odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),  # labels the return year
         SEAKCatch_log   = log(SEAKCatch)) -> log_data

log_data %>% dplyr::filter(JYear < year.data) -> log_data_subset

# best model
base_terms   <- "CPUE + as.factor(odd_even_factor)"
full_formula <- as.formula(paste("SEAKCatch_log ~", base_terms,
                                 if (!is.null(temp_var)) paste("+", temp_var)))
best_model   <- lm(full_formula, data = log_data_subset)
reduced      <- lm(as.formula(paste("SEAKCatch_log ~", base_terms)), data = log_data_subset)

# forecast and 80% PI for this model, from the summary table
tbl2 <- read.csv(paste0(results.directory, "model_summary_table2.csv"),
                 header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
sel  <- tbl2[tbl2$Model == model, ]
stopifnot(nrow(sel) == 1,
          identical(sel$Terms, if (is.null(temp_var)) "no temperature index included" else temp_var))
fit_value_model <- sel$Fit
lwr_pi_80       <- sel$Fit_LPI
upr_pi_80       <- sel$Fit_UPI

sigma_hat   <- sigma(best_model)
sample_size <- nobs(best_model)
p <- length(coef(best_model))   # parameters including intercept
k <- p - 1                      # predictors excluding intercept

# fitted values, residuals and influence measures, with Year from the data
diag_df <- augment(best_model, data = log_data_subset) %>%
  mutate(harvest = exp(SEAKCatch_log),
         fit     = exp(.fitted + 0.5 * sigma_hat^2))     # bias-corrected

axisf <- tickr(data.frame(Year = seq(min(diag_df$Year), year.data + 1)), Year, 2)
x_years <- scale_x_continuous(limits = c(min(diag_df$Year) - 0.5, year.data + 1.5),
                              breaks = axisf$breaks, labels = axisf$labels)

# model diagnostics table
diag_df %>%
  transmute(Year,
            Harvest          = round(harvest, 2),
            Residuals        = round(.resid, 2),
            `Hat values`     = round(.hat, 2),
            `Cooks distance` = round(.cooksd, 2),
            `Std. residuals` = round(.std.resid, 2),
            `Fitted values`  = round(fit, 2)) %>%
  write.csv(paste0(results.directory, "model_summary_table4_", model, ".csv"), row.names = FALSE)

# tests (printed in console only)
print(car::outlierTest(best_model))   # Bonferroni-adjusted test for the largest studentized residual
print(anova(reduced, best_model))     # does the temperature term improve on CPUE + odd/even?

png(paste0(results.directory, "residual_plots_", model, ".png"), width = 7, height = 5, units = "in", res = 300)
car::residualPlots(best_model, terms = ~ 1, fitted = TRUE, id.n = 5, smoother = TRUE)  # lack-of-fit curvature test
dev.off()

png(paste0(results.directory, "general_diagnostics_", model, ".png"), width = 7, height = 7, units = "in", res = 300)
print(autoplot(best_model))
dev.off()

# catch figure
y_max_catch <- up_to(c(diag_df$harvest, diag_df$fit, upr_pi_80), 20)

ggplot(diag_df, aes(x = Year)) +
  geom_col(aes(y = harvest), fill = "lightgrey", colour = "black", width = 1) +
  geom_line(aes(y = fit), linewidth = 0.75) +
  annotate("errorbar", x = year.data + 1, ymin = lwr_pi_80, ymax = upr_pi_80, width = 0.4, linewidth = 0.5) +
  annotate("point", x = year.data + 1, y = fit_value_model, shape = 21, size = 2.5, fill = "grey") +
  x_years +
  scale_y_continuous(breaks = seq(0, y_max_catch, 20), limits = c(0, y_max_catch), expand = c(0, 0)) +
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)") +
  theme_diag(11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  panel_label("A.") -> plot_catch

y_max_obs <- up_to(c(diag_df$harvest, diag_df$fit), 20)

ggplot(diag_df, aes(x = fit, y = harvest)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, lty = 3) +
  # geom_text(aes(label = Year), size = 2.5, nudge_x = 2) +   # uncomment to label years
  scale_x_continuous(breaks = seq(0, y_max_obs, 20), limits = c(0, y_max_obs)) +
  scale_y_continuous(breaks = seq(0, y_max_obs, 20), limits = c(0, y_max_obs)) +
  coord_fixed() +
  labs(x = "Predicted SEAK Pink Salmon Harvest (millions)",
       y = "Observed SEAK Pink Salmon Harvest (millions)") +
  theme_diag(9) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  panel_label("B.") -> plot_obs

ggsave(paste0(results.directory, "catch_plot_pred_", model, ".png"),
       plot = plot_grid(plot_catch, plot_obs, align = "h", nrow = 1),
       dpi = 500, height = 4, width = 7, units = "in")

# residual: vs CPUE (A), vs temperature (B), by year (C), vs fitted (D) 
std_lim   <- max(4, up_to(abs(diag_df$.std.resid), 1))
std_scale <- scale_y_continuous(breaks = seq(-std_lim, std_lim, 1), limits = c(-std_lim, std_lim))

ggplot(diag_df, aes(x = CPUE, y = .std.resid)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(colour = "grey50") +
  geom_smooth(colour = "black", method = "loess", formula = y ~ x) +
  std_scale +
  scale_x_continuous(limits = c(0, up_to(diag_df$CPUE, 1))) +
  labs(x = "CPUE", y = "Standardized residuals", title = model) +
  theme_diag() + panel_label("A.") -> plot_cpue

if (!is.null(temp_var)) {
  ggplot(diag_df, aes(x = .data[[temp_var]], y = .std.resid)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_point(colour = "grey50") +
    geom_smooth(colour = "black", method = "loess", formula = y ~ x) +
    std_scale +
    scale_x_continuous(limits = c(down_to(diag_df[[temp_var]], 1), up_to(diag_df[[temp_var]], 1))) +
    labs(x = paste0("Temperature (", temp_var, ")"), y = "Standardized residuals", title = model) +
    theme_diag() + panel_label("B.") -> plot_temp
} else {
  plot_temp <- ggplot() + theme_void()
}

ggplot(diag_df, aes(x = Year, y = .std.resid)) +
  geom_col(colour = "grey50", fill = "lightgrey", alpha = 0.7, width = 0.8) +
  std_scale + x_years +
  labs(x = "Year", y = "Standardized residuals", title = model) +
  theme_diag() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  panel_label("C.") -> plot_year

res_lim <- up_to(abs(diag_df$.resid), 0.5)
ggplot(diag_df, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(colour = "grey50") +
  geom_smooth(colour = "black", method = "loess", formula = y ~ x) +
  scale_y_continuous(breaks = seq(-res_lim, res_lim, 0.5), limits = c(-res_lim, res_lim)) +
  scale_x_continuous(limits = c(down_to(diag_df$.fitted, 1), up_to(diag_df$.fitted, 1))) +
  labs(x = "Fitted values (log scale)", y = "Residuals", title = model) +
  theme_diag() + panel_label("D.") -> plot_fitted

ggsave(paste0(results.directory, "fitted_", model, ".png"),
       plot = plot_grid(plot_cpue, plot_temp, plot_year, plot_fitted, align = "hv", nrow = 2),
       dpi = 500, height = 5, width = 5, units = "in")

# Iinfluence figure: Cook's distance (A) and leverage (B) ----------------------------
cook_level <- 4 / (sample_size - k - 1)   # Ren et al. 2016; k = predictors excluding intercept
hat_level  <- 2 * p / sample_size         # p = parameters including intercept

diag_df %>%
  mutate(name = ifelse(.cooksd > cook_level, Year, "")) %>%
  ggplot(aes(x = Year, y = .cooksd, label = name)) +
  geom_col(colour = "grey50", fill = "lightgrey", alpha = 0.7, width = 0.8) +
  geom_text(size = 2, vjust = -0.5) +
  geom_hline(yintercept = cook_level, lty = 2) +
  x_years +
  scale_y_continuous(limits = c(0, max(1, up_to(diag_df$.cooksd, 0.25)))) +
  labs(x = "Year", y = "Cook's distance", title = model) +
  theme_diag() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6)) +
  panel_label("A.") -> plot_cook

diag_df %>%
  mutate(name = ifelse(.hat > hat_level, Year, "")) %>%
  ggplot(aes(x = Year, y = .hat, label = name)) +
  geom_col(colour = "grey50", fill = "lightgrey", alpha = 0.7, width = 0.8) +
  geom_text(size = 2, vjust = -0.5) +
  geom_hline(yintercept = hat_level, lty = 2) +
  x_years +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Year", y = "Hat values", title = model) +
  theme_diag() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6)) +
  panel_label("B.") -> plot_hat

ggsave(paste0(results.directory, "influential_", model, ".png"),
       plot = plot_grid(plot_cook, plot_hat, align = "h", nrow = 1),
       dpi = 500, height = 3, width = 6, units = "in")