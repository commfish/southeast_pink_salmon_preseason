# load ----
install.packages("devtools")
devtools::install_github("ben-williams/FNGr")
library("FNGr")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(extrafont)
library(broom)
library("FNGr")

loadfonts(device="win")
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_sleek())


# Depends on dplyr
tickr <- function(
  data, # dataframe
  var, # column of interest
  to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    select(breaks = UQ(VAR), labels)
}
