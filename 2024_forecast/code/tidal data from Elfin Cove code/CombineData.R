library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

###Below will set working directory to wherever you opened this file from...

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

### List of CSV files in directory, do not run if you have saved a csv output of
###combined data or remove the old combined data before running, ask me how I 
###know...All of the monthly tidal prediction data need to be in the directory
###with this script file. 

###I included an rds file bypassing all of these steps...

#csv_files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

### Empty data frame for combined data
#combined_data <- data.frame()

### Loop through each file and combine data
#for (csv_file in csv_files) {
#
 # data <- read_csv(csv_file, show_col_types = FALSE)
  #
  #combined_data <- bind_rows(combined_data, data)
#}

### Order the combined data by Date Time
#combined_data <- combined_data %>% arrange(`Date Time`)

###Pull year, month, and absolute difference in height for each successive tide
###and add each as a new column to the data set
#combined_data <- combined_data %>%
 # mutate(Year = lubridate::year(`Date Time`)) %>% 
  #mutate(Month = lubridate::month(`Date Time`)) %>% 
  #mutate(Abs_Diff = c(NA, abs(diff(Prediction))))

#write_rds(combined_data, "combined_tide_data.rds")

###Start here
combined_data <- read_rds("combined_tide_data.rds")

### Calculate the average "Prediction" and "Abs_Diff" by year and month
avg_prediction_month <- combined_data %>%
  group_by(Year, Month) %>%
  summarize(avg_Prediction = mean(Prediction, na.rm = TRUE)) %>%
  ungroup()

avg_abs_diff_month <- combined_data %>%
  group_by(Year, Month) %>%
  summarize(avg_Abs_Diff = mean(Abs_Diff, na.rm = TRUE)) %>%
  ungroup()

#### Identify the month within each year with the largest average Prediction
largest_avg_prediction_year <- avg_prediction_month %>%
  group_by(Year) %>%
  top_n(1, avg_Prediction) %>%
  ungroup()

### Identify the month within each year with the largest average "Abs_Diff"
largest_avg_abs_diff_year <- avg_abs_diff_month %>%
  group_by(Year) %>%
  top_n(1, avg_Abs_Diff) %>%
  ungroup()

###Taking the average seems to really wash out any signal that might be 
###available, there are differences but they are really small...

### Identify the month within each year with the largest value of "Prediction"
largest_prediction_year <- combined_data %>%
  group_by(Year, Month) %>%
  summarize(max_Prediction = max(Prediction, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Year) %>%
  top_n(1, max_Prediction) %>%
  ungroup()

### Identify the month within each year with the largest value of "Abs_Diff"
largest_abs_diff_year <- combined_data %>%
  group_by(Year, Month) %>%
  summarize(max_abs_diff = max(Abs_Diff, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Year) %>%
  top_n(1, max_abs_diff) %>%
  ungroup()

### Maybe months with smallest tidal movement?
min_abs_diff_year <- combined_data %>% 
  group_by(Year, Month) %>% 
  summarize(min_abs_diff = min(Abs_Diff, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  top_n(1, min_abs_diff) %>% 
  ungroup()

###write_csv(combined_data, "combined_tide_data.csv")

###1997 only to visualize tidal patterns...
data_1997 <- combined_data %>%
  filter(Year == 1997)

data_1997 <- data_1997 %>%
  mutate(Julian_Day = yday(`Date Time`)) %>% 
  mutate(Order = seq_len(n()))


###Plot Prediction values, separate months with vert line

ggplot(data_1997, aes(x = Order, y = Prediction)) +
  geom_line() +
  geom_vline(aes(xintercept = Order),
             data = data_1997 %>%
               filter((Month == 6 & lag(Month) == 5) |
                        (Month == 7 & lag(Month) == 6) |
                        (Month == 8 & lag(Month) == 7)),
             color = "blue", linetype = "dashed")+
  labs(x = "Order", y = "Prediction", title = "Prediction Values by Order") +
  theme_minimal()
