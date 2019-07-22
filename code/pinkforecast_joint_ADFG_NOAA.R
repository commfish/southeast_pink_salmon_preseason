#notes----
#author: Jim Murphy updated: 9/25/18
# modified by Rich Brenner and Sara Miller after 28 Sept. 2018
#contact: jim.murphy@noaa.gov; richard.brenner@alaska.gov; sara.miller@alaska.gov
#SECM Pink salmon forecast models

#set ggplot themes
loadfonts(device="win")
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_sleek())

#source functions for bootstrap and model summaries
source("code/functions.R")

#read in data----
SECM2019<-read.csv("southeast/preseason_joint_pink_salmon/data/SECMcatch2019.csv") # catch data
variables<-read.csv("southeast/preseason_joint_pink_salmon/data/SECMvar2019.csv") # dataset with harvest, catch indicies, and environmental variables
historical<-read.csv("southeast/preseason_joint_pink_salmon/data/historical/pink_salmon_historical_esc_harvest.csv") #historical catch data

#FLAG: Rich Brenner is checking to makle sure the historical dataset matches the variables and SECM2019 catch data

# subset data by peak month and generate list of catch by year
SECM2019 %>% filter(Pink_Peak == TRUE) %>%
  mutate(ttd_pink = Raw_Pink/TTD)-> ttd.data

SECM2019 %>% filter(Pink_Peak == TRUE) -> cal.data
cal.data<-split(cal.data$Pink,cal.data$Year)

SECM2019 %>% filter(Pink_Peak == TRUE) -> cal.data.loc
cal.data.loc<-split(cal.data.loc$Pink,list(cal.data.loc$Year,cal.data.loc$Locality))

#analysis----
#stepwise model selection for SEAK and NSEI Pink salmon harvest

#number of years including forecast year
n<-dim(variables)[1]

# stepwise selection of SEAK harvest forecast models 
# using calibrated CPUE and including ISTI as the only temperature variable
fitcal<-step(lm(SEAKCatch~CPUEcal+ISTI+July24Size+Condition+MEI_win+PDO_sum+PDO_win,data=variables[-n,]))

# stepwise selection of SEAK harvest forecast models 
# using calibrated CPUE and transect weighting
# transect weights: Icy Strait=3, Upper Chatham=2
# ISTI is the only temperature variable
fitcal.loc1<-step(lm(SEAKCatch~CPUEcal_loc1+ISTI+July24Size+Condition+MEI_win+PDO_sum+PDO_win,data=variables[-n,]))

# stepwise selection of SEAK harvest forecast models 
# using calibrated CPUE
# GOA temperature variables are included
# ERSSTMAMJJ: Mar-Jul GOA temperatures from ERSSTv5
# Temp: average of ISTI and ERSSTMAMJJ
fitcal.temp<-step(lm(SEAKCatch~CPUEcal+ISTI+Temp+ERSSTMAMJJ+July24Size+Condition+MEI_win+PDO_win,data=variables[-n,]))

# stepwise selection of SEAK harvest forecast models 
# using calibrated CPUE and transect weighting
# transect weights: Icy Strait=3, Upper Chatham=2
# GOA temperature variables are included
# ERSSTMAMJJ: Mar-Jul GOA temperatures from ERSSTv5
# Temp: average of ISTI and ERSSTMAMJJ
fitcal.loc1.temp<-step(lm(SEAKCatch~CPUEcal_loc1+ISTI+Temp+ERSSTMAMJJ+July24Size+Condition+MEI_win+PDO_win,data=variables[-n,]))

# stepwise selection of NSEI harvest forecast models 
# using calibrated CPUE
# and including ISTI as the only temperature variable
fitcal.nsei<-step(lm(NSEICatch~CPUEcal+ISTI+July24Size+Condition+MEI_win+PDO_sum+PDO_win,data=variables[-n,]))

# stepwise selection of NSEI harvest forecast models 
# using calibrated CPUE and transect weighting
# transect weights: Icy Strait=3, Upper Chatham=2
# ISTI is the only temperature variable
fitcal.loc1.nsei<-step(lm(NSEICatch~CPUEcal_loc1+ISTI+July24Size+Condition+MEI_win+PDO_sum+PDO_win,data=variables[-n,]))

# stepwise selection of SEAK harvest forecast models 
# using calibrated CPUE
# GOA temperature variables are included
# ERSSTMAMJJ: Mar-Jul GOA temperatures from ERSSTv5
# Temp: average of ISTI and ERSSTMAMJJ
fitcal.nsei.temp<-step(lm(NSEICatch~CPUEcal+ISTI+Temp+ERSSTMAMJJ+July24Size+Condition+MEI_win+PDO_sum+PDO_win,data=variables[-n,]))

# stepwise selection of SEAK harvest forecast models 
# using calibrated CPUE and transect weighting
# transect weights: Icy Strait=3, Upper Chatham=2
# GOA temperature variables are included
# ERSSTMAMJJ: Mar-Jul GOA temperatures from ERSSTv5
# Temp: average of ISTI and ERSSTMAMJJ
fitcal.loc1.nsei.temp<-step(lm(NSEICatch~CPUEcal_loc1+ISTI+Temp+ERSSTMAMJJ+July24Size+Condition+MEI_win+PDO_sum+PDO_win,data=variables[-n,]))

#selected models for 2019 SE Pink salmon harvest
#define model names and formulas
model.names<-c(m1='CPUE',
               m2='CPUE+ISTI',
               m3='CPUE+ISTI+Condition',
               m4='CPUE+ISTI+PDO_sum',
               m5='CPUE+ISTI+Condition+PDO_sum',
               m6='CPUE+Temp',
               m7='CPUE+Temp+Condition')
model.formulas<-c(SEAKCatch~CPUE,
                  SEAKCatch~CPUE+ISTI,
                  SEAKCatch~CPUE+ISTI+Condition,
                  SEAKCatch~CPUE+ISTI+PDO_sum,
                  SEAKCatch~CPUE+ISTI+Condition+PDO_sum,
                  SEAKCatch~CPUE+Temp,
                  SEAKCatch~CPUE+Temp+Condition)
model.names.nsei<-c(m1='CPUE',
                    m2='CPUE+July24Size',
                    m3='CPUE+MEI_win',
                    m4='CPUE+July24Size+MEI_win')

model.formulas.nsei<-c(NSEICatch~CPUE,
                       NSEICatch~CPUE+July24Size,
                       NSEICatch~CPUE+MEI_win,
                       NSEICatch~CPUE+July24Size+MEI_win)

#summary statistics SEAK Pink salmon harvest forecast models
# FLAG: bootstrap function needs to be generalized to accept strata weighting
# only model comparisons are run
variables$CPUE<-variables$CPUEcal
seak.model.summary<-model.summary(harvest=variables$SEAKCatch,variables=variables, model.formulas=model.formulas,model.names=model.names)
seak.boot.summary<-boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas,model.names=model.names)

variables$CPUE<-variables$CPUEcal_loc1
seak.model.loc1.summary<-model.summary(harvest=variables$SEAKCatch,variables=variables, model.formulas=model.formulas,model.names=model.names)

#summary statistics NSEI Pink salmon harvest forecast models
variables$CPUE<-variables$CPUEcal
nsei.model.summary<-model.summary(harvest=variables$NSEICatch,variables=variables,model.formulas=model.formulas.nsei,model.names=model.names.nsei)
nsei.boot.summary<-boot.summary(cpuedata=cal.data,variables=variables,model.formulas=model.formulas.nsei,model.names=model.names.nsei)

variables$CPUE<-variables$CPUEcal_loc1
nsei.model.loc1.summary<-model.summary(harvest=variables$NSEICatch,variables=variables, model.formulas=model.formulas.nsei,model.names=model.names.nsei)

#figures
#annual harvest 
historical %>%
  filter(data=='harvest')%>%
  dplyr::select(year, `NSE inside` = NSE_inside, `NSE outside` = NSE_outside, SSE = SSE) %>% 
  gather("var", "value", -c(year)) %>% 
  mutate(var = factor(var, ordered = TRUE, 
                      levels = c("NSE inside", "NSE outside", 
                                 "SSE"))) -> df

axisb <- tickr(df, year, 5)
ggplot(df, aes(x = year)) +
  geom_bar(aes(y = value, fill = var), colour = "black", size = 0.2, stat = "identity") +
  scale_fill_manual(values = c("white", "grey85", "grey50", "grey20")) +
  theme(legend.position = c(0.2, 0.8),
        legend.spacing.y = unit(0, "cm")) +
  scale_x_continuous(breaks = axisb$breaks, labels = axisb$labels) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 100000000, 10000000)) +
  labs(x = "", y = "Harvest\n", linetype = NULL, fill = NULL) -> historical_catch_plot
ggsave('figures/historical_catch_plot.png', dpi=300, height=4, width=6, units="in")

#annual harvest with predictions (just an example using model m2)
#FLAG: need to add 2019 forecast with CI
n<-dim(variables)[1]
data<-variables[-n,]
data %>% 
  do(m1 = lm(SEAKCatch ~ CPUEcal, data = .),
     m2 = lm(SEAKCatch ~ CPUEcal + ISTI, data = .),
     m3 = lm(SEAKCatch ~ CPUEcal + ISTI + Condition, data = .),
     m4 = lm(SEAKCatch ~ CPUEcal + ISTI + PDO_sum, data = .),
     m5 = lm(SEAKCatch ~ CPUEcal + ISTI + Condition + PDO_sum, data = .),
     m6 = lm(SEAKCatch ~ CPUEcal + Temp, data = .),
     m7 = lm(SEAKCatch ~ CPUEcal + Temp + Condition, data = .))-> run_seakmodels

run_seakmodels %>% 
  augment(m2) %>% 
  mutate(year = data$Year_Juv, catch = SEAKCatch, fit = (.fitted)) -> m2
axisb <- tickr(m2, year, 2)
m2 %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y = catch, colour = " SEAK pink catch"),
           stat = "identity",  
           fill = "lightgrey",
           width = 1, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = fit, linetype = "forecast model m2"), colour = "black", size = 1) +
  scale_color_grey() +
  theme(legend.position = c(0.85, 0.85),
        legend.spacing.y = unit(0, "cm")) +
  scale_x_continuous(breaks = axisb$breaks, labels = axisb$labels) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 100, 10)) + theme(legend.title=element_blank())+
  labs(x = "", y = "Harvest (millions)\n", linetype = NULL, fill = NULL) -> catch_plot_pred
ggsave('figures/catch_plot_pred.png', dpi=300, height=6, width=8, units="in")

