# run code 3_summarize_models_basic.R first
# inputs
fit_value_model <- 19.3 #best model outputs (bias-corrected); value of forecast (from model_summary_table2)
lwr_pi_80 <- 12.6 # 80% PI from model_summary_table2 in the results folder
upr_pi_80 <- 29.8 # 80% PI from model_summary_table2 in the results folder
best_model <- m13a
model <- 'm13a'
year.forecast <- "2026_forecast" # forecast year
year.data <- 2025 # last year of data
year.data.one <- year.data - 1
sample_size <- 28 # number of data points in model (this is used for Cook's distance)

# best model based on performance metrics (need to update each year)
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor) + NSEAK_SST_AMJ, data = log_data_subset) -> m13a
lm(SEAKCatch_log ~ CPUE + as.factor(odd_even_factor), data = log_data_subset) -> m13a_reduced

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    #    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}

tickryr <- data.frame(Year = 1997:2026)
axisf <- tickr(tickryr, Year, 2)

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
                     axis.text.x = element_text(size =9, family="Times New Roman"),
                     axis.title.y = element_text(size=11, colour="black",family="Times New Roman"),
                     axis.title.x = element_text(size=11, colour="black",family="Times New Roman"),
                     panel.border = element_rect(colour = "black", size=1),
                     legend.position=c(0.50,0.87)) +
  geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
  scale_x_continuous(limits = c(min(tickryr$Year), max(tickryr$Year)),
                     breaks = axisf$breaks, labels = axisf$labels) +
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

# DIAGNOSTIC PLOTS
# Diagnostics: test model assumptions (normality, linearity, residuals)
png(paste0(results.directory, "model_figs/general_diagnostics_m13a.png"))
autoplot(best_model)
dev.off()

car::outlierTest(best_model) #Bonferroni p-values (term # 24); lack of fit test; https://stats.stackexchange.com/questions/288910/outlier-detection-using-outliertest-function
#car::residualPlots(best_model) #lack-of fit curvature test; terms that are non-significant suggest a properly specified model
car::residualPlots(best_model, terms = ~ 1, fitted = T, id.n = 5, smoother = T)
anova(m13a, m13a_reduced) #Since this p-value is less than .05, we can reject the null hypothesis of the test and conclude that the full model offers a statistically significantly better fit than the reduced model.

#https://www.statology.org/lack-of-fit-test-in-r/
# output diagnostic plots
augment(best_model) %>% 
  mutate(resid = (.std.resid)) %>% 
  ggplot(., aes(x = CPUE, y = resid)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(color ="grey50") + ggtitle("m13a") +
  geom_smooth(aes(colour = CPUE), colour="black") +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) +
  labs(y = "Standardized residuals", x =  "CPUE") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0.2, y = 4, label="A."),family="Times", colour="black", size=5) -> plot1

tickryr <- data.frame(Year = 1997:2026)
axisf <- tickr(tickryr, Year, 2)
log_data_subset %>%
  dplyr::select(JYear, Year) -> log_data_subset  

augment(best_model) %>% 
  cbind(.,log_data_subset) %>%
  mutate(resid = .std.resid)%>% 
  ggplot(aes(x = Year, y = resid)) + ggtitle("m13a") +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  scale_x_continuous(limits = c(min(tickryr$Year), max(tickryr$Year)),
                     breaks = axisf$breaks, labels = axisf$labels) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0, 1,2,3,4), limits = c(-4,4))+
  labs(y = "Standardized residuals", x =  "Year") + theme_bw () +theme(text = element_text(size=10),
                                                                                axis.text.x = element_text(angle=90, hjust=1, size=6,vjust=0.5 ),
                                                                                panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 1998, y = 4, label="C."),family="Times", colour="black", size=5) -> plot2

# residuals against fitted
augment(best_model) %>% 
  mutate(resid = (.resid),
         fit = (.fitted)) %>% 
  ggplot(aes(x = fit, y = resid)) +
  geom_point(color ="grey50")  + ggtitle("m13a") +
  geom_smooth(aes(colour = fit,),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1))+
  scale_x_continuous(breaks = c(2,3,4,5,5), limits = c(2,5))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Residuals", x =  "Fitted values") +
  geom_text(aes(x = 2.1, y = 1, label="D."),family="Times", colour="black", size=5)-> plot3

# residuals against temp
augment(best_model) %>% 
  mutate(resid = (.std.resid),
         temp = .[[4]]) %>% # fourth column should be temperature variable
  ggplot(aes(x = temp, y = resid)) +
  geom_point(color ="grey50")  + ggtitle("m13a") +
  geom_smooth(aes(colour = temp),colour="black") +
  geom_hline(yintercept = 0, lty=2) + 
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0,1,2,3,4), limits = c(-4,4)) +
  scale_x_continuous(breaks = c(5,6,7,8,9,10,11,12), limits = c(5,12)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Standardized residuals", x =  "Temperature") +
  geom_text(aes(x = 5.2, y = 4, label="B."),family="Times", colour="black", size=5) -> plot4

augment(best_model) %>%  
  ggplot(aes(x = CPUE, y = SEAKCatch_log)) +
  geom_point(color ="grey50") +   ggtitle("m13a") +
  geom_smooth(aes(colour = CPUE), colour="black") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
  labs(y = "ln(Harvest)", x =  "CPUE") + theme(legend.position="none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(x = 0, y = 6, label="F."),family="Times", colour="black", size=5) -> plot6  
cowplot::plot_grid(plot1, plot4, plot2, plot3, align = "vh", nrow = 2, ncol=2)
ggsave(paste0(results.directory, "model_figs/fitted_m13a.png"), dpi = 500, height = 5, width = 5, units = "in") 

# Cook's distance and leverage plot
k = 3
p = 4
level <- 4/(sample_size-k-1) # source: Ren et al. 2016# k = # of predictors in model (not including intercept); p = # of predictors including intercept
augment(best_model) %>% 
  cbind(.,log_data_subset) %>% 
  mutate(cooksd = (.cooksd),
         name= ifelse(cooksd >level, Year, "")) %>% 
  ggplot(aes(x = Year, y = cooksd, label=name)) +ggtitle("m13a") +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 2, position = position_stack(vjust = 1), vjust=-2) + 
  geom_hline(yintercept = level, lty=2) +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(limits = c(min(tickryr$Year), max(tickryr$Year)),
                     breaks = axisf$breaks, labels = axisf$labels) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1.0))+
  labs(y = "Cook's distance", x =  "Year") + theme(text = element_text(size=10),
                                                            axis.text.x = element_text(angle=90, vjust=0.5))+
  geom_text(aes(x = 1998, y = 1, label="A."),family="Times", colour="black", size=5) -> plot1

# leverage plot
#  p = number of parameters in the model including intercept
level <- 2*(p/sample_size)
level # leverage value
augment(best_model) %>% 
  cbind(.,log_data_subset) %>% 
  mutate(hat= (.hat),
         name= ifelse(hat > level, Year, "")) %>% # may need to adjust value; see hat value equation above
  ggplot(aes(x = Year, y = hat, label=name)) +ggtitle("m13a") +
  geom_bar(stat = "identity", colour = "grey50", 
           fill = "lightgrey",alpha=.7,
           width = 0.8, position = position_dodge(width = 0.2)) + 
  geom_text(size = 2, position = position_stack(vjust =1.1), vjust=-2) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = level, lty=2) +
  scale_x_continuous(limits = c(min(tickryr$Year), max(tickryr$Year)),
                     breaks = axisf$breaks, labels = axisf$labels) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0), limits = c(0,1.0)) +
  labs(y = "Hat-values", x =  "Year") + theme(text = element_text(size=10),
                                                       axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))+
  geom_text(aes(x = 1998, y = 1, label="B."),family="Times", colour="black", size=5)-> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "model_figs/influential_m13a.png"), dpi = 500, height = 3, width = 6, units = "in")


# read.csv(file.path(data.directory,'var2025_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names
# variables %>%
#   dplyr::filter(Year== 2009|Year== 2010) %>%
#   mutate(harvest = SEAKCatch)%>%
#   mutate(terms = "SEAK pink harvest (not fit)")%>%
#   dplyr::select(c(harvest, Year, JYear, terms)) -> fig_data

# Extract sigma from the model
tickryr <- data.frame(Year = 1997:2026)
axisf <- tickr(tickryr, Year, 2)

sigma <- as.numeric(sigma(best_model))

 augment(best_model) %>% 
   cbind(.,log_data_subset)%>%
   mutate(harvest = exp(SEAKCatch_log),
          fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>%
   ggplot(aes(x=Year)) +
   geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
            stat = "identity", colour ="black",
            width = 1) +
  geom_line(aes(y = fit, colour = "fit"), linetype = 1, linewidth = 0.75) +
  scale_colour_manual("terms", values=c("fit" = "black")) +
  scale_fill_manual("terms",values=c("#e7e7e7", "darkgrey"))+
  theme_bw() + theme(legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.box="horizontal",
                     panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.text.x = element_text(angle=90, hjust=1, size=7,vjust=0.5),
                     axis.title.y = element_text(size=11, colour="black",family="Times New Roman"),
                     axis.title.x = element_text(size=11, colour="black",family="Times New Roman"),
                     panel.border = element_rect(colour = "black", size=1),
                     legend.position=c(0.4,0.87)) +
  geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
   scale_x_continuous(limits = c(min(tickryr$Year), max(tickryr$Year)),
                      breaks = axisf$breaks, labels = axisf$labels) +
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_text(aes(x = 1998, y = 140, label="A."),family="Times New Roman", colour="black", size=5) +
  geom_segment(aes(x = year.data + 1, y = lwr_pi_80, yend = upr_pi_80, xend = year.data + 1), size=1, colour="black", lty=1)-> plot1

# plot of observed harvest by fitted values (with one to one line)
# the year labels are manually put in, so uncomment the geom_text_repel to make sure the correct
# labels are there
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  cbind(.,log_data_subset)%>%
  mutate(harvest = exp(SEAKCatch_log), 
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
  labs(y = "Observed SEAK Pink Salmon Harvest (millions)", x = "Predicted SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL)+
  geom_text(aes(x = 2, y = 140, label="B."),family="Times New Roman", colour="black", size=5) -> plot2
cowplot::plot_grid(plot1, plot2,  align = "vh", nrow = 1, ncol=2)
ggsave(paste0(results.directory, "model_figs/catch_plot_pred_", model, ".png"), dpi = 500, height = 4, width = 7, units = "in")
dev.off()

