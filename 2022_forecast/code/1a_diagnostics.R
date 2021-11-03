# # source code and functions
source('2022_forecast/code/1_summarize_models.r')
source('2022_forecast/code/functions.r')

# # best models based on performance metrics
lm(SEAKCatch_log ~ CPUE + ISTI20_MJJ, data = log_data_subset) -> m2

# # STEP #1: MODEL DIAGNOSTICS TABLES
f_model_diagnostics(m2, 'm2')

augment(best.model) %>% 
  mutate(SEAKCatch = round((exp(SEAKCatch_log)),1),
         resid = round((.resid),2),
         hat_values = round((.hat),2),
         Cooks_distance = round((.cooksd),2),
         std_resid = round((.std.resid),2),
         fitted = round((.fitted),5),
         CPUE = round((CPUE),2),
         ISTI20_MJJ = round((ISTI20_MJJ),2),
         year=1998:year.data,
         fit = exp(.fitted) * exp(0.5* sigma*sigma),
         fit_bias_corrected = round(fit,2),
         juvenile_year = 1997:year.data.one) %>%
  dplyr::select(year, SEAKCatch, CPUE, ISTI20_MJJ, resid, hat_values, Cooks_distance, std_resid, fit_bias_corrected) %>%
  write.csv(paste0(results.directory, "/model_summary_table4.csv"), row.names = F)

# # STEP #2: DIAGNOSTIC PLOTS
# # Diagnostics: test model assumptions (normality, linearity, residuals)
# # residuals
f_resid_year_diagnostics_plot(m2, 'm2')

# # leverage and Cook's distance plots
f_resid_leverage_diagnostics_plot(m2, 'm2', k = 2, p = 3)

# # additional tests
# # png(paste0(results.directory, "general_diagnostics.png"))
# # autoplot(best.model)
# # dev.off()
# 
# # outlierTest(m22) #Bonferroni p-values (term # 16)
# # residualPlots(m22) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
# # car::residualPlots(best.model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)

# STEP #4: DIAGNOSTIC PLOTS OF BEST MODEL
# Diagnostics: test model assumptions (normality, linearity, residuals)
png(paste0(results.directory, "figs/general_diagnostics.png"))
autoplot(best.model)
dev.off()

outlierTest(best.model) #Bonferroni p-values (term # 16)
residualPlots(best.model) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
car::residualPlots(best.model, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)

# cpue and catch
augment(best.model)%>% 
  ggplot(aes(x = CPUE, y = SEAKCatch_log)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  labs(y = "ln(Harvest)", x =  "ln(CPUE+1)") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 5, label="a)"),family="Times New Roman", colour="black", size=5) -> plot1

# temp and catch
augment(best.model)  %>% 
  ggplot(aes(x = ISTI20_MJJ, y = SEAKCatch_log)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = ISTI20_MJJ, fill = ISTI20_MJJ), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12), limits = c(7,12)) +
  labs(y = "ln(Harvest)", x =  "Temperature") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 7, y = 5, label="b)"),family="Times New Roman", colour="black", size=5)-> plot2
cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/cpue_temp.png"), dpi = 500, height = 3, width = 6, units = "in")

# residuals against covariate
augment(best.model) %>% 
  mutate(resid = (.std.resid)) %>% 
  ggplot(aes(x = CPUE, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = CPUE, fill = CPUE), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  labs(y = "Standardized residuals", x =  "ln(CPUE+1)") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 4, label="a)"),family="Times New Roman", colour="black", size=5)-> plot1

# residuals against covariate
augment(best.model) %>% 
  mutate(resid = (.std.resid))%>% 
  ggplot(aes(x = ISTI20_MJJ, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = ISTI20_MJJ, fill = ISTI20_MJJ), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12), limits = c(7,12)) +
  labs(y = "Standardized residuals", x =  "Temperature") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(aes(x = 7, y = 4, label="b)"),family="Times New Roman", colour="black", size=5) -> plot2
cowplot::plot_grid(plot1, plot2, align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/predicted.png"), dpi = 500, height = 3, width = 6, units = "in")

# residuals by year
augment(best.model) %>% 
  mutate(resid = (.std.resid),
         count = 1997:year.data.one)%>% 
  ggplot(aes(x = count, y = resid)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0, 1,2,3,4), limits = c(-4,4))+
  labs(y = "Standardized residuals", x =  "Juvenile year") + theme_bw () +theme(text = element_text(size=10),
                                                                                axis.text.x = element_text(angle=90, hjust=1),
                                                                                panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 1997, y = 4, label="a)"),family="Times New Roman", colour="black", size=5)-> plot2

# residuals against fitted
augment(best.model) %>% 
  mutate(resid = (.resid),
         fit = (.fitted)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50") + 
  geom_smooth(aes(colour = fit, fill = fit),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1))+
  scale_x_continuous(breaks = c(2,3,4,5), limits = c(2,5))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Residuals", x =  "Fitted values") +
  geom_text(aes(x = 2, y = 1, label="b)"),family="Times New Roman", colour="black", size=5)-> plot3
cowplot::plot_grid(plot2, plot3, align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/fitted.png"), dpi = 500, height = 3, width = 6, units = "in")

# Cook's distance plot
k <- 2 # predictors in model
level <- 4/(sample_size-k-1) # source: Ren et al. 2016

augment(best.model) %>% 
  mutate(cooksd = (.cooksd),
         count = (1997:year.data.one),
         name= ifelse(cooksd >level, count, "")) %>% 
  ggplot(aes(x = count, y = cooksd, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  geom_hline(yintercept = level, lty=2) +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1))+
  labs(y = "Cook's distance", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                            axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="a)"),family="Times New Roman", colour="black", size=5) -> plot4

# Leverage plot
p <- 3 # the number of parameters in the model including intercept
level <- 2*p/sample_size # source: Ren et al. 2016
# leverage plot
augment(best.model) %>% 
  mutate(hat= (.hat),
         count = 1997:year.data.one,
         name= ifelse(hat >0.27, count, "")) %>% # may need to adjust valeu; see hat value equation above
  ggplot(aes(x = count, y = hat, label=name)) +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 3, position = position_stack(vjust = 1.1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = 0.26, lty=2) +
  scale_x_continuous(breaks = 1997:year.data.one, labels = 1997:year.data.one) +
  labs(y = "Hat-values", x =  "Juvenile year") + theme(text = element_text(size=10),
                                                       axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(x = 1997, y = 1, label="b)"),family="Times New Roman", colour="black", size=5)-> plot5
cowplot::plot_grid(plot4, plot5,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/influential.png"), dpi = 500, height = 3, width = 6, units = "in")

# plot of harvest by year with prediction error 
augment(best.model) %>% 
  mutate(year = 1998:year.data, 
         catch = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = catch, fill = "SEAK pink catch"),
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
  geom_point(x=year.data +1, y=fit_value, pch=21, size=3, colour = "black", fill="grey") +
  scale_x_continuous(breaks = seq(1998, year.data+1, 1)) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_segment(aes(x = year.data + 1, y = lwr_pi, yend = upr_pi, xend = year.data + 1), size=1, colour="black", lty=1) +
  geom_text(aes(x = 1998, y = 140, label="a)"),family="Times New Roman", colour="black", size=5) -> plot1

# plot of observed harvest by fitted values (with one to one line)
augment(m2) %>% 
  mutate(year = 1997:year.data.one, 
         catch = exp(SEAKCatch_log), 
         sigma = .sigma,
         fit = exp(.fitted) * exp(0.5*sigma*sigma))  %>%
  ggplot(aes(x=fit, y=catch)) +
  geom_point() +
  geom_point(aes(y = catch), colour = "black", size = 1) +
  scale_color_grey() +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                         axis.title.y = element_text(size=9, colour="black",family="Times New Roman"),
                                         axis.title.x = element_text(size=9, colour="black",family="Times New Roman")) +
  theme(legend.position="none") + theme(legend.title=element_blank())+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140), limits = c(0,140)) +
  geom_abline(intercept = 0, lty=3) +
  labs(y = "Observed SEAK Pink Salmon Harvest (millions)", x = "Predicted SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_text(aes(x = 2, y = 140, label="b)"),family="Times New Roman", colour="black", size=5) +
  geom_text(aes(y = 103, x = 57, label="2013"),family="Times New Roman", colour="black", size=4) +
  geom_text(aes(y = 85, x = 134, label="1999"),family="Times New Roman", colour="black", size=4) -> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "figs/catch_plot_pred.png"), dpi = 500, height = 3, width = 6, units = "in")


