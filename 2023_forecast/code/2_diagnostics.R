# run diagnostics on the best model
# inputs
fit_value_model<-18.841 #best model outputs (bias-corrected); value of forecast (from model_summary_table3)
lwr_pi_80<-12.273 # 80% PI from model_summary_table3
upr_pi_80<-28.922 # 80% PI from model_summary_table3
best_model<-m11
model<-'m11'
year.forecast <- "2023_forecast"
year.data <- 2022
year.data.one <- year.data - 1

# source code and functions
source('2023_forecast/code/1_summarize_models.r')
source('2023_forecast/code/functions.r')

# best model based on performance metrics
lm(SEAKCatch_log ~ CPUE + NSEAK_SST_May, data = log_data_subset) -> m11

# MODEL DIAGNOSTICS TABLES
# f_model_diagnostics(m11, 'm11')

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
  write.csv(file =paste0(results.directory, "model_summary_table6_", model, ".csv"), row.names = F)

# DIAGNOSTIC PLOTS
# Diagnostics: test model assumptions (normality, linearity, residuals)
# residuals
f_resid_year_diagnostics_plot(m11, 'm11')

# # leverage and Cook's distance plots
f_resid_leverage_diagnostics_plot(m11, 'm11', k = 2, p = 3)

# DIAGNOSTIC PLOTS OF BEST MODEL
# Diagnostics: test model assumptions (normality, linearity, residuals)
png(paste0(results.directory, "figs/general_diagnostics_", model, ".png"))
autoplot(best_model)
dev.off()

car::outlierTest(best_model) #Bonferroni p-values (term # 24); lack of fit test; https://stats.stackexchange.com/questions/288910/outlier-detection-using-outliertest-function
car::residualPlots(best_model) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
car::residualPlots(best_model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)

# plot of harvest by year with prediction error 
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(year = 1998:year.data, 
         harvest = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = fit, colour = "fit"), linetype = 1, size = 0.75) +
  scale_colour_manual("", values=c("fit" = "black")) +
  scale_fill_manual("",values="lightgrey")+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal",
                     panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1),
                     axis.title.y = element_text(size=9, colour="black",family="Times New Roman"),
                     axis.title.x = element_text(size=9, colour="black",family="Times New Roman"),
                     legend.position=c(0.6,0.9)) +
  geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_segment(aes(x = year.data + 1, y = lwr_pi_80, yend = upr_pi_80, xend = year.data + 1), size=1, colour="black", lty=1) +
  geom_text(aes(x = 1998, y = 140, label="A."),family="Times New Roman", colour="black", size=5) -> plot1
dev.off()
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
  scale_color_grey() +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                         axis.title.y = element_text(size=9, colour="black",family="Times New Roman"),
                                         axis.title.x = element_text(size=9, colour="black",family="Times New Roman")) +
  theme(legend.position="none") + theme(legend.title=element_blank())+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  geom_abline(intercept = 0, lty=3) +
  # geom_text_repel(aes(y = harvest, label = year),
  #                nudge_x = 1, size = 3, show.legend = FALSE) +
  labs(y = "Observed SEAK Pink Salmon Harvest (millions)", x = "Predicted SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL)+
  geom_text(aes(x = 2, y = 140, label="B."),family="Times New Roman", colour="black", size=5)+
geom_text(aes(y = 55, x = 20, label="2021"),family="Times New Roman", colour="black", size=4) +
geom_text(aes(y = 103, x = 62, label="2013"),family="Times New Roman", colour="black", size=4) +
geom_text(aes(y = 85, x = 125, label="1999"),family="Times New Roman", colour="black", size=4) -> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/catch_plot_pred_", model, ".png"), dpi = 500, height = 3, width = 6, units = "in")
dev.off()

# these figures are made from the functions f_resid_year_diagnostics_plot(m11, 'm11'); and
# f_resid_leverage_diagnostics_plot(m11, 'm11', k = 2, p = 3); the individual code for the plots are below

## cpue and catch
# augment(best_model)%>% 
#   ggplot(aes(x = CPUE, y = SEAKCatch_log)) +
#   geom_point(color ="grey50") + 
#   geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
#   scale_y_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
#   scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
#   labs(y = "ln(Harvest)", x =  "ln(CPUE+1)") + theme(legend.position="none") +
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_text(aes(x = 0, y = 5, label="A."),family="Times New Roman", colour="black", size=5) -> plot1
# 
# # temp and catch
# augment(best_model)  %>% 
#   ggplot(aes(x = NSEAK_SST_May, y = SEAKCatch_log)) +
#   geom_point(color ="grey50") + 
#   geom_smooth(aes(colour = NSEAK_SST_May, fill = NSEAK_SST_May), colour="black") +
#   scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
#   scale_x_continuous(breaks = c(5,6,7,8,9,10,11,12), limits = c(5,12)) +
#   labs(y = "ln(Harvest)", x =  "Temperature") + theme(legend.position="none") +
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_text(aes(x = 5, y = 6, label="B."),family="Times New Roman", colour="black", size=5)-> plot2
# cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
# ggsave(paste0(results.directory, "figs/cpue_temp.png"), dpi = 500, height = 3, width = 6, units = "in")
# 
# # residuals against covariate
# augment(best_model) %>% 
#   mutate(resid = (.std.resid)) %>% 
#   ggplot(aes(x = CPUE, y = resid)) +
#   geom_hline(yintercept = 0, lty=2) + 
#   geom_point(color ="grey50") + 
#   geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
#   scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
#   scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
#   labs(y = "Standardized residuals", x =  "ln(CPUE+1)") + theme(legend.position="none") +
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_text(aes(x = 0, y = 4, label="A."),family="Times New Roman", colour="black", size=5)-> plot1
# 
# # residuals against covariate
# augment(best_model) %>% 
#   mutate(resid = (.std.resid))%>% 
#   ggplot(aes(x = NSEAK_SST_May, y = resid)) +
#   geom_hline(yintercept = 0, lty=2) + 
#   geom_point(color ="grey50") + 
#   geom_smooth(aes(colour = NSEAK_SST_May, fill = NSEAK_SST_May), colour="black") +
#   scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
#   scale_x_continuous(breaks = c(5,6,7,8,9,10,11,12), limits = c(5,12)) +
#   labs(y = "Standardized residuals", x =  "Temperature") +
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   geom_text(aes(x = 5, y = 4, label="B."),family="Times New Roman", colour="black", size=5) -> plot2
# cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
# ggsave(paste0(results.directory, "figs/predicted.png"), dpi = 500, height = 3, width = 6, units = "in")
# 
# # residuals by year
# augment(best_model) %>% 
#   mutate(resid = (.std.resid),
#          count = 1997:year.data.one)%>% 
#   ggplot(aes(x = count, y = resid)) +
#   geom_bar(stat = "identity", colour = "grey50", 
#            fill = "lightgrey",alpha=.7,
#            width = 0.8, position = position_dodge(width = 0.2)) + 
#   scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
#   scale_y_continuous(breaks = c(-4,-3,-2,-1,0, 1,2,3,4), limits = c(-4,4))+
#   labs(y = "Standardized residuals", x =  "Juvenile year") + theme_bw () +theme(text = element_text(size=10),
#                                                                                 axis.text.x = element_text(angle=90, hjust=1),
#                                                                                 panel.border = element_blank(), panel.grid.major = element_blank(),
#                                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_text(aes(x = 1997, y = 4, label="A."),family="Times New Roman", colour="black", size=5)-> plot2
# 
# # residuals against fitted
# augment(best_model) %>% 
#   mutate(resid = (.resid),
#          fit = (.fitted)) %>% 
#   ggplot(aes(x = fit, y = resid)) +
#   geom_point(color ="grey50") + 
#   geom_smooth(aes(colour = fit, fill = fit),colour="black") +
#   geom_hline(yintercept = 0, lty=2) + 
#   scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1))+
#   scale_x_continuous(breaks = c(2,3,4,5), limits = c(2,5))+
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   labs(y = "Residuals", x =  "Fitted values") +
#   geom_text(aes(x = 2, y = 1, label="B."),family="Times New Roman", colour="black", size=5)-> plot3
# cowplot::plot_grid(plot2, plot3, align = "vh", nrow = 1, ncol=2)
# ggsave(paste0(results.directory, "figs/fitted.png"), dpi = 500, height = 3, width = 6, units = "in")
# 
# # Cook's distance plot
# k <- 2 # predictors in model
# level <- 4/(sample_size-k-1) # source: Ren et al. 2016 (Cook's distance value)
# 
# augment(best_model) %>% 
#   mutate(cooksd = (.cooksd),
#          count = (1997:year.data.one),
#          name= ifelse(cooksd >level, count, "")) %>% 
#   ggplot(aes(x = count, y = cooksd, label=name)) +
#   geom_bar(stat = "identity", colour = "grey50", 
#            fill = "lightgrey",alpha=.7,
#            width = 0.8, position = position_dodge(width = 0.2)) + 
#   geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
#   geom_hline(yintercept = level, lty=2) +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                                                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
#   scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1))+
#   labs(y = "Cook's distance", x =  "Juvenile year") + theme(text = element_text(size=10),
#                                                             axis.text.x = element_text(angle=90, hjust=1))+
#   geom_text(aes(x = 1997, y = 1, label="A."),family="Times New Roman", colour="black", size=5) -> plot4
# 
# # Leverage plot
# p <- 3 # the number of parameters in the model including intercept
# level <- 2*(p/sample_size) # source: Ren et al. 2016 (leverage value)
# # http://home.iitk.ac.in/~shalab/regression/Chapter6-Regression-Diagnostic%20for%20Leverage%20and%20Influence.pdf
# 
# augment(best_model) %>% 
#   mutate(hat= (.hat),
#          count = 1997:year.data.one,
#          name= ifelse(hat >level, count, "")) %>% # may need to adjust valeu; see hat value equation above
#   ggplot(aes(x = count, y = hat, label=name)) +
#   geom_bar(stat = "identity", colour = "grey50", 
#            fill = "lightgrey",alpha=.7,
#            width = 0.8, position = position_dodge(width = 0.2)) + 
#   geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_hline(yintercept = level, lty=2) +
#   scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
#   labs(y = "Hat-values", x =  "Juvenile year") + theme(text = element_text(size=10),
#                                                        axis.text.x = element_text(angle=90, hjust=1))+
#   geom_text(aes(x = 1997, y = 1, label="B."),family="Times New Roman", colour="black", size=5)-> plot5
# cowplot::plot_grid(plot4, plot5,  align = "vh", nrow = 1, ncol=2)
# ggsave(paste0(results.directory, "figs/influential.png"), dpi = 500, height = 3, width = 6, units = "in") 