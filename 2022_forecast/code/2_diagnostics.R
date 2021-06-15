# source code and functions
source('2022_forecast/code/1_summarize_models.r')
source('2022_forecast/code/functions.r')

# best models based on performance metrics
lm(SEAKCatch_log ~ CPUE + Chatham_SST_May, data = log_data_subset) -> m10
lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset) -> m14
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m18
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ, data = log_data_subset) -> m20
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset) -> m21

# STEP #1: MODEL DIAGNOSTICS TABLES
f_model_diagnostics(m10, 'm10')
f_model_diagnostics(m14, 'm14')
f_model_diagnostics(m18, 'm18')
f_model_diagnostics(m20, 'm20')
f_model_diagnostics(m21, 'm21')

# STEP #2: DIAGNOSTIC PLOTS
# Diagnostics: test model assumptions (normality, linearity, residuals)

# residuals
f_resid_year_diagnostics_plot(m10, 'm10')
f_resid_year_diagnostics_plot(m14, 'm14')
f_resid_year_diagnostics_plot(m18, 'm18')
f_resid_year_diagnostics_plot(m20, 'm20')
f_resid_year_diagnostics_plot(m21, 'm21')

# leverage and Cook's distance plots
f_resid_leverage_diagnostics_plot(m10, 'm10', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m14, 'm14', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m18, 'm18', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m20, 'm20', k = 2, p = 3)
f_resid_leverage_diagnostics_plot(m21, 'm21', k = 2, p = 3)
# additional tests
# png(paste0(results.directory, "general_diagnostics.png"))
# autoplot(best.model)
# dev.off()

# outlierTest(m22) #Bonferroni p-values (term # 16)
# residualPlots(m22) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
# car::residualPlots(best.model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)



