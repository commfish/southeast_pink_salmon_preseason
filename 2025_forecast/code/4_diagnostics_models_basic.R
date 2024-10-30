# run code 3_summarize_models_basic.R first
# inputs
fit_value_model<-19.516 #best model outputs (bias-corrected); value of forecast (from model_summary_table2)
lwr_pi_80<-12.39 # 80% PI from model_summary_table2 in the results folder
upr_pi_80<-30.741 # 80% PI from model_summary_table2 in the results folder
best_model<-m2a
model<-'m2a'
year.forecast <- "2025_forecast" # forecast year
year.data <- 2024 # last year of data
year.data.one <- year.data - 1

# best model based on performance metrics
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_subset) -> m2a

# MODEL DIAGNOSTICS TABLES
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(Harvest = round((exp(SEAKCatch_log)),2),
         Residuals = round((.resid),2),
         'Hat values' = round((.hat),2),
         'Cooks distance' = round((.cooksd),2),
         'Std. residuals' = round((.std.resid),2),
         fitted = round((.fitted),5),
         Year=1998:year.data,
         fit = exp(.fitted) * exp(0.5* sigma*sigma),
         'Fitted values' = round(fit,2),
         juvenile_year = 1997:year.data.one) %>%
  dplyr::select(Year, Harvest, Residuals, 'Hat values', 'Cooks distance', 'Std. residuals', 'Fitted values') %>%
  write.csv(file =paste0(results.directory, "model_summary_table4_", model, ".csv"), row.names = F)

# # DIAGNOSTIC PLOTS
# # Diagnostics: test model assumptions (normality, linearity, residuals)
# # residuals
# f_resid_year_diagnostics_plot(m11, 'm11')
# 
# # leverage and Cook's distance plots
# f_resid_leverage_diagnostics_plot(m11, 'm11', k = 2, p = 3) # the k and p need to be updated in the CPUE model is chosen or a more complicated model than 
# # the basic CPUE +temp models
# 
# # DIAGNOSTIC PLOTS OF BEST MODEL
# # Diagnostics: test model assumptions (normality, linearity, residuals)
# png(paste0(results.directory, "model_figs/general_diagnostics_", model, ".png"))
# autoplot(best_model)
# dev.off()
# 
# car::outlierTest(best_model) #Bonferroni p-values (term # 24); lack of fit test; https://stats.stackexchange.com/questions/288910/outlier-detection-using-outliertest-function
# car::residualPlots(best_model) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
# car::residualPlots(best_model, terms = ~ 1, fitted = T, id.n = 5, smoother = T)

# plot of harvest by year with prediction error 
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(year = 1998:year.data, 
         harvest = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1) +
  geom_line(aes(y = fit, colour = "fit"), linetype = 1, linewidth = 0.75) +
  scale_colour_manual("", values=c("fit" = "black")) +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal",
                     #panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.text.x = element_text(size =10, family="Times New Roman"),
                     axis.title.y = element_text(size=11, colour="black",family="Times New Roman"),
                     axis.title.x = element_text(size=11, colour="black",family="Times New Roman"),
                     panel.border = element_rect(colour = "black", size=1),
                     legend.position=c(0.51,0.87)) +
  geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
  scale_x_continuous(
    minor_breaks = seq(1998, year.data +1, by = 1),
    breaks = seq(1998, year.data +1, by = 4), limits = c(1998, year.data+1),
    guide = "axis_minor") + # this is added to the original code)
  
  #scale_x_continuous(breaks = seq(1998, year.data +1, 4)) +theme(legend.title=element_blank())+
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_text(aes(x = 1998, y = 140, label="A."),family="Times New Roman", colour="black", size=5) +
  geom_segment(aes(x = year.data + 1, y = lwr_pi_80, yend = upr_pi_80, xend = year.data + 1), size=1, colour="black", lty=1)-> plot1

# plot of observed harvest by fitted values (with one to one line)
# the year labels are manually put in, so uncomment the geom_text_repel to make sure the correct
# labels are there
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(year = 1998:year.data, 
         harvest = exp(SEAKCatch_log), 
         fit = as.numeric(exp(.fitted) * exp(0.5*sigma*sigma))) %>%
  ggplot(aes(x=fit, y=harvest)) +
  geom_point() +
  geom_point(aes(y = harvest), colour = "black", size = 1) +
  scale_color_grey() +theme_bw() + theme(panel.grid.minor = element_blank(),
                                         panel.grid.major = element_blank(), axis.line = element_line(colour = "black"),
                                         axis.title.y = element_text(size=9, colour="black",family="Times New Roman"),
                                         panel.border = element_rect(colour = "black", fill=NA, size=1),
                                         axis.title.x = element_text(size=9, colour="black",family="Times New Roman")) +
  theme(legend.position="none") + theme(legend.title=element_blank())+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  geom_abline(intercept = 0, lty=3) +
  # geom_text_repel(aes(y = harvest, label = year),
  #                nudge_x = 1, size = 3, show.legend = FALSE) +
  labs(y = "Observed SEAK Pink Salmon Harvest (millions)", x = "Predicted SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL)+
  geom_text(aes(x = 2, y = 140, label="B."),family="Times New Roman", colour="black", size=5)-> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "model_figs/catch_plot_pred_", model, ".png"), dpi = 500, height = 4, width = 7, units = "in")
dev.off()

