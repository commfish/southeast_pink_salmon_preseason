# run code 5_summarize_models_basic_by_odd_only.R first
# inputs
fit_value_model<-34.564 #best model outputs (bias-corrected); value of forecast (from model_summary_table2)
lwr_pi_80<-21.61 # 80% PI from model_summary_table2 in the results folder
upr_pi_80<-55.282 # 80% PI from model_summary_table2 in the results folder
best_model<-m15b
model<-'m15b'
year.forecast <- "2025_forecast" # forecast year
year.data <- 2024 # last year of data
year.data.one <- year.data - 1

read.csv(file.path(data.directory,'var2024_final.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> variables # update file names
n <- dim(variables)[1] # number of years including forecast year
variables %>%
  mutate (odd_even_factor = ifelse(JYear %% 2 == 0, "odd", "even"),
          SEAKCatch_log = log(SEAKCatch)) %>% # log catch variable
  dplyr::select(c(Year,JYear, SEAKCatch_log, odd_even_factor)) %>%
  dplyr::filter(JYear < year.data) -> log_data_subset


# best model based on performance metrics
lm(SEAKCatch_log ~ CPUE + SEAK_SST_May, data = log_data_odd_subset) -> m15b
log_data_odd_subset %>% # log catch variable
  dplyr::select(c(Year,JYear)) -> log_data_odd_subset

# MODEL DIAGNOSTICS TABLES
as.numeric(sigma(best_model))-> sigma
augment(best_model) %>% 
  mutate(Harvest = round((exp(SEAKCatch_log)),2),
         Residuals = round((.resid),2),
         'Hat values' = round((.hat),2),
         'Cooks distance' = round((.cooksd),2),
         'Std. residuals' = round((.std.resid),2),
         fitted = round((.fitted),5),
         fit = exp(.fitted) * exp(0.5* sigma*sigma),
         'Fitted values' = round(fit,2)) %>%
  cbind(.,log_data_odd_subset)%>%
  dplyr::select(Year, Harvest, Residuals, 'Hat values', 'Cooks distance', 'Std. residuals', 'Fitted values') %>%
  write.csv(file =paste0(results.directory, "model_summary_table4_", model, ".csv"), row.names = F)

# plot of harvest by year with prediction error 
as.numeric(sigma(best_model))-> sigma  
augment(best_model) %>%
  cbind(.,log_data_odd_subset) %>%
  mutate(fit = exp(.fitted) * exp(0.5* sigma*sigma))-> fit_data

fit_data %>%
merge(., log_data_subset, by=c("Year","JYear", "SEAKCatch_log"), all.y = T) %>% 
  mutate(harvest = exp(SEAKCatch_log))->fig_data 
  ggplot(aes(x=Year), data = fig_data) +
  geom_bar(aes(y = harvest, fill = odd_even_factor),
           stat = "identity", colour ="black",
           width = 1, data = fig_data) +
  geom_line(aes(x=Year, y = fit, colour = "fit"), data = fit_data, linetype = 1, linewidth = 0.75) +
  scale_colour_manual("odd_even_factor", values=c("fit" = "black")) +
  scale_fill_manual("odd_even_factor",values=c("#e7e7e7", "darkgrey"))+
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
                     legend.position=c(0.50,0.87)) +
  geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
    scale_x_continuous(
      minor_breaks = seq(1998, year.data +1, by = 1),
      breaks = seq(1997, year.data +1, by = 4), limits = c(1997, year.data+1),
      guide = "axis_minor")   + # this is added to the original code)
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_text(aes(x = 1999, y = 140, label="A."),family="Times New Roman", colour="black", size=5) +
  geom_segment(aes(x = year.data + 1, y = lwr_pi_80, yend = upr_pi_80, xend = year.data + 1), size=1, colour="black", lty=1)-> plot1

# plot of observed harvest by fitted values (with one to one line)
# the year labels are manually put in, so uncomment the geom_text_repel to make sure the correct
# labels are there
as.numeric(sigma(best_model))-> sigma  
augment(best_model) %>%
  cbind(.,log_data_odd_subset)%>%
  merge(., log_data_subset, by=c("Year","JYear", "SEAKCatch_log"), all.y = T) %>% 
  mutate(harvest = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>%
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

# plot of harvest by year with prediction error (Figure 2 just odd year) 
as.numeric(sigma(best_model))-> sigma  
augment(best_model) %>%
  cbind(.,log_data_odd_subset) %>%
  mutate(fit = exp(.fitted) * exp(0.5* sigma*sigma))%>%
  mutate(harvest = exp(SEAKCatch_log))%>% 
ggplot(aes(x=Year)) +
  geom_bar(aes(y = harvest, fill = "SEAK pink harvest"),
           stat = "identity", colour ="black",
           width = 1) +
  geom_line(aes(x=Year, y = fit, colour = "fit"),linetype = 1, linewidth = 0.75) +
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
                     legend.position=c(0.50,0.87)) +
  geom_point(x=year.data +1, y=fit_value_model, pch=21, size=2.5, colour = "black", fill="grey") +
  scale_x_continuous(
    minor_breaks = seq(1998, year.data +1, by = 1),
    breaks = seq(1997, year.data +1, by = 4), limits = c(1997, year.data+1),
    guide = "axis_minor")   + # this is added to the original code)
  scale_y_continuous(breaks = c(0,20, 40, 60, 80, 100,120,140), limits = c(0,140))+ theme(legend.title=element_blank())+
  labs(x = "Year", y = "SEAK Pink Salmon Harvest (millions)", linetype = NULL, fill = NULL) +
  geom_text(aes(x = 1999, y = 140, label="A."),family="Times New Roman", colour="black", size=5) +
  geom_segment(aes(x = year.data + 1, y = lwr_pi_80, yend = upr_pi_80, xend = year.data + 1), size=1, colour="black", lty=1)-> plot1

# plot of observed harvest by fitted values (with one to one line)
# the year labels are manually put in, so uncomment the geom_text_repel to make sure the correct
# labels are there
as.numeric(sigma(best_model))-> sigma  
augment(best_model) %>%
  cbind(.,log_data_odd_subset)%>%
  merge(., log_data_subset, by=c("Year","JYear", "SEAKCatch_log"), all.y = T) %>% 
  mutate(harvest = exp(SEAKCatch_log),
         fit = exp(.fitted) * exp(0.5* sigma*sigma)) %>%
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
ggsave(paste0(results.directory, "model_figs/catch_plot_pred2_", model, ".png"), dpi = 500, height = 4, width = 7, units = "in")
dev.off()

