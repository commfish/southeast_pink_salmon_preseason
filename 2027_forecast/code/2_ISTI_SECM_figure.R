# Environmental Variables for SEAK Pink Salmon Forecast Models
# Script written by Sara Miller (sara.miller@alaska.gov) with assistance from Jordan Watson (jordan.watson@noaa.gov)
# September 2026

# load libraries----
library(tidyverse)
library(extrafont)
library(lubridate)
library(ggpubr)
# extrafont::font_import() # only needs to be run once for extra fonts for figures
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_report(base_size = 14))

# create a folder for temperature_data
out.path <- paste0("2027_forecast/results/temperature_data/") # update year
if(!exists(out.path)){dir.create(out.path)}

# set up directories----
year.forecast <- "2027_forecast" # update year
data.directory <- file.path(year.forecast, 'data', '/')
results.directory <- file.path(year.forecast,  'results/temperature_data', '/')

# create the ISTI dataset for the report
# the varyyyy_final.csv file needs to be updated with the current year's data prior to running the code below 
read.csv(paste0(data.directory, 'var2026_final.csv')) %>% # update file name
  dplyr::select(JYear, ISTI20_JJ) %>%
  rename(Year = JYear) %>%
  write.csv(., paste0(results.directory, 'SECMvar2026_JJ.csv'), row.names = FALSE) # update file name

# create a figure of ISTI_JJ for the SECM survey
read.csv(paste0(data.directory, 'var2026_final.csv')) %>% # update file name 
  dplyr::select(JYear, ISTI20_JJ) %>%
  gather("var", "value", -c(JYear)) %>% 
  ggplot(., aes(y = value, x = JYear, group = var)) +
  geom_point(aes(shape = var, color = var, size=var)) +
  geom_line(aes(linetype = var, color = var)) +
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "dotted"))+
  scale_shape_manual(values=c(1, 16, 15, 2,8)) +
  scale_color_manual(values=c('black','black', 'grey70', 'grey70','black'))+
  scale_size_manual(values=c(2,2,2,2,2)) +
  theme(legend.title=element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),
        axis.title.y = element_text(size=12, colour="black",family="Times New Roman"),
        axis.title.x = element_text(size=12, colour="black",family="Times New Roman"),
        legend.position="none") +
  scale_x_continuous(breaks = 1997:2026, labels = 1997:2026) + # update final year
  scale_y_continuous(breaks = c(6,7, 8, 9,10,11,12,13), limits = c(6,12))+
  labs(y = "Temperature (Celsius)", x ="") -> plot1
cowplot::plot_grid(plot1, align = "vh", nrow = 1, ncol=1)
ggsave(paste0(results.directory, "annual_ISTI20_JJ.png"), dpi = 500, height = 5, width = 7, units = "in")

# Correlation among the 17 temperature indices used in the 2027 SEAK pink salmon forecast
# Uses the years with an observed harvest (the years the models are fit to).
read.csv(file.path(data.directory,'var2026_final.csv'), header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) -> dat # update file names

# order: ISTI20_JJ, then region x window (same order as model numbering m2-m18)
regions   <- c("Chatham", "Icy_Strait", "NSEAK", "SEAK")
windows   <- c("May", "MJJ", "AMJ", "AMJJ")
var_order <- c("ISTI20_JJ", paste0(rep(regions, each = 4), "_SST_", rep(windows, times = 4)))

temp_dat <- dat %>%
  dplyr::select(dplyr::all_of(var_order))

# correlation matrix 
cor_mat <- cor(temp_dat, method = "pearson")
write.csv(round(cor_mat, 2), here::here(results.directory, "temperature_correlation.csv"))

r_all  <- cor_mat[upper.tri(cor_mat)]
sst    <- var_order[-1]
r_sst  <- cor_mat[sst, sst][upper.tri(cor_mat[sst, sst])]
r_isti <- cor_mat["ISTI20_JJ", sst]

cat(sprintf("All 17 indices:        r = %.2f to %.2f (median %.2f)\n", min(r_all), max(r_all), median(r_all)))
cat(sprintf("16 satellite SST:      r = %.2f to %.2f\n", min(r_sst), max(r_sst)))
cat(sprintf("ISTI20_JJ vs SST:      r = %.2f to %.2f\n", min(r_isti), max(r_isti)))

# principal components: how much is one shared signal?
pca <- prcomp(temp_dat, scale. = TRUE)
pve <- 100 * pca$sdev^2 / sum(pca$sdev^2)
cat(sprintf("Variance explained by PC1-PC3: %.1f%%, %.1f%%, %.1f%%\n", pve[1], pve[2], pve[3]))

# heatmap (lower triangle)
cor_long <- as.data.frame(as.table(cor_mat)) %>%
  setNames(c("x", "y", "r")) %>%
  dplyr::mutate(i = match(x, var_order),
                j = match(y, var_order)) %>%
  dplyr::filter(i <= j) %>%
  dplyr::mutate(x = factor(x, levels = var_order),
                y = factor(y, levels = rev(var_order)))

p_cor <- ggplot(cor_long, aes(x = x, y = y, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.2) +
  scale_fill_gradient(low = "white", high = "firebrick", limits = c(0.6, 1),
                      oob = scales::squish, name = "Pearson r") +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank(),
        legend.position = "right")

print(p_cor)
ggsave(here::here(results.directory, "temperature_correlation.png"), p_cor,
       width = 7.5, height = 7, dpi = 300, bg = "white")

