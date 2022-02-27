# # source code and functions
# source('2022_forecast/code/1_summarize_models.r')
# source('2022_forecast/code/functions.r')
# 
# # best models based on performance metrics
# lm(SEAKCatch_log ~ CPUE + Chatham_SST_May, data = log_data_subset) -> m3
# lm(SEAKCatch_log ~ CPUE + Icy_Strait_SST_May, data = log_data_subset) -> m7
# lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m11
# lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJ, data = log_data_subset) -> m13
# lm(SEAKCatch_log ~ CPUE + NSEAK_SST_AMJJ, data = log_data_subset) -> m14
# 
# # STEP #1: MODEL DIAGNOSTICS TABLES
# f_model_diagnostics(m3, 'm3')
# f_model_diagnostics(m7, 'm7')
# f_model_diagnostics(m11, 'm11')
# f_model_diagnostics(m13, 'm13')
# f_model_diagnostics(m14, 'm14')
# 
# # STEP #2: DIAGNOSTIC PLOTS
# # Diagnostics: test model assumptions (normality, linearity, residuals)
# 
# # residuals
# f_resid_year_diagnostics_plot(m3, 'm3')
# f_resid_year_diagnostics_plot(m7, 'm7')
# f_resid_year_diagnostics_plot(m11, 'm11')
# f_resid_year_diagnostics_plot(m13, 'm13')
# f_resid_year_diagnostics_plot(m14, 'm14')
# 
# # leverage and Cook's distance plots
# f_resid_leverage_diagnostics_plot(m3, 'm3', k = 2, p = 3)
# f_resid_leverage_diagnostics_plot(m7, 'm7', k = 2, p = 3)
# f_resid_leverage_diagnostics_plot(m11, 'm11', k = 2, p = 3)
# f_resid_leverage_diagnostics_plot(m13, 'm13', k = 2, p = 3)
# f_resid_leverage_diagnostics_plot(m14, 'm14', k = 2, p = 3)
# # additional tests
# # png(paste0(results.directory, "general_diagnostics.png"))
# # autoplot(best.model)
# # dev.off()
# 
# # outlierTest(m22) #Bonferroni p-values (term # 16)
# # residualPlots(m22) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
# # car::residualPlots(best.model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)



