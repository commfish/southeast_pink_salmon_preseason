# source code and functions
source('2022_forecast/code/1_summarize_models.r')
source('2022_forecast/code/functions.r')

# best models based on performance metrics
lm(SEAKCatch_log ~ CPUE, data = log_data_subset) -> m1
lm(SEAKCatch_log ~ CPUE + Chatham_Strait_SST_May, data = log_data_subset) -> m13
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset) -> m16
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m19
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset) -> m20
lm(SEAKCatch_log ~ CPUE + SST_Jordan_May, data = log_data_subset) -> m22

# STEP #1: MODEL DIAGNOSTICS TABLES
f_model_diagnostics(m13, 'm13')
f_model_diagnostics(m16, 'm16')
f_model_diagnostics(m19, 'm19')
f_model_diagnostics(m20, 'm20')
f_model_diagnostics(m22, 'm22')

# STEP #2: DIAGNOSTIC PLOTS
# Diagnostics: test model assumptions (normality, linearity, residuals)

# residuals
f_resid_year_diagnostics_plot(m13, 'm13')
f_resid_year_diagnostics_plot(m16, 'm16')
f_resid_year_diagnostics_plot(m19, 'm19')
f_resid_year_diagnostics_plot(m20, 'm20')
f_resid_year_diagnostics_plot(m22, 'm22')

# leverage and Cook's distance plots
f_resid_leverage_diagnostics_plot(m13, 'm13', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m16, 'm16', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m19, 'm19', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m20, 'm20', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m22, 'm22', k = 2, p = 3)
# additional tests
# png(paste0(results.directory, "general_diagnostics.png"))
# autoplot(best.model)
# dev.off()

# outlierTest(m22) #Bonferroni p-values (term # 16)
# residualPlots(m22) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
# car::residualPlots(best.model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)



