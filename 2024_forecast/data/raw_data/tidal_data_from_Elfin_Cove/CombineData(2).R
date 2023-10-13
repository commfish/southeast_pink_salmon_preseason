library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(data.table)

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
#combined_data <- read_rds("combined_tide_data.rds")
#combined_data$DateTime <- combined_data$`Date Time`
#combined_data$`Date Time` <- NULL 
#JPink <- read.csv("JPink Systematic with Zeros.csv")


### Calculate the average "Prediction" and "Abs_Diff" by year and month
#avg_prediction_month <- combined_data %>%
 # group_by(Year, Month) %>%
  #summarize(avg_Prediction = mean(Prediction, na.rm = TRUE)) %>%
  #ungroup()

#avg_abs_diff_month <- combined_data %>%
 # group_by(Year, Month) %>%
  #summarize(avg_Abs_Diff = mean(Abs_Diff, na.rm = TRUE)) %>%
  #ungroup()

#### Identify the month within each year with the largest average Prediction
#largest_avg_prediction_year <- avg_prediction_month %>%
 # group_by(Year) %>%
  #top_n(1, avg_Prediction) %>%
  #ungroup()

### Identify the month within each year with the largest average "Abs_Diff"
#largest_avg_abs_diff_year <- avg_abs_diff_month %>%
 # group_by(Year) %>%
  #top_n(1, avg_Abs_Diff) %>%
  #ungroup()

###Taking the average seems to really wash out any signal that might be 
###available, there are differences but they are really small...

### Identify the month within each year with the largest value of "Prediction"
#largest_prediction_year <- combined_data %>%
 # group_by(Year, Month) %>%
  #summarize(max_Prediction = max(Prediction, na.rm = TRUE)) %>%
  #ungroup() %>%
  #group_by(Year) %>%
  #top_n(1, max_Prediction) %>%
  #ungroup()

### Identify the month within each year with the largest value of "Abs_Diff"
#largest_abs_diff_year <- combined_data %>%
 # group_by(Year, Month) %>%
  #summarize(max_abs_diff = max(Abs_Diff, na.rm = TRUE)) %>%
  #ungroup() %>%
  #group_by(Year) %>%
  #top_n(1, max_abs_diff) %>%
  #ungroup()

### Maybe months with smallest tidal movement?
#min_abs_diff_year <- combined_data %>% 
 # group_by(Year, Month) %>% 
  #summarize(min_abs_diff = min(Abs_Diff, na.rm = TRUE)) %>% 
  #ungroup() %>% 
  #group_by(Year) %>% 
  #top_n(1, min_abs_diff) %>% 
  #ungroup()

###write_csv(combined_data, "combined_tide_data.csv")

###1997 only to visualize tidal patterns...
#data_1997 <- combined_data %>%
 # filter(Year == 1997)

#data_1997 <- data_1997 %>%
 # mutate(Julian_Day = yday(`Date Time`)) %>% 
  #mutate(Order = seq_len(n()))


###Plot Prediction values, separate months with vert line

#ggplot(data_1997, aes(x = Order, y = Prediction)) +
 # geom_line() +
  #geom_vline(aes(xintercept = Order),
   #          data = data_1997 %>%
    #           filter((Month == 6 & lag(Month) == 5) |
     #                   (Month == 7 & lag(Month) == 6) |
      #                  (Month == 8 & lag(Month) == 7)),
       #      color = "blue", linetype = "dashed")+
  #labs(x = "Order", y = "Prediction", title = "Prediction Values by Order") +
  #theme_minimal()

###Format DT

combined_data$DateTime <- ymd_hms(combined_data$DateTime)

### Pink catch data had date in one column, time in another, format and combine
JPink$DateTime <- paste(JPink$Date, JPink$Time)
JPink$DateTime <- mdy_hm(JPink$DateTime)

### Create empty vectors for Type values and time differences
type_values <- character(nrow(JPink))
time_differences <- numeric(nrow(JPink))

### Magic loops for Pink data
for (i in 1:nrow(JPink)) {
  # Find the closest previous tidal record for each catch record
  previous_records <- combined_data$DateTime[combined_data$DateTime < JPink$DateTime[i]]
  
  # double check no previous records
  if(length(previous_records) > 0) {
    # Find the nearest previous record's index
    nearest_index <- which.max(previous_records)
    
    # Grab the tide "Type" and stick it in the list of types to be appended to 
    # the pink data
    type_values[i] <- combined_data$Type[nearest_index]
    
    # tdiff in hours
    time_differences[i] <- as.numeric(difftime(JPink$DateTime[i], combined_data$DateTime[nearest_index], units = "hours"))
  } else {
    # If no previous, set Type and time difference to NA
    type_values[i] <- NA
    time_differences[i] <- NA
  }
}

###Append new schtuff to the pink data
JPink$Type <- type_values
JPink$TimeDiffHours <- time_differences

###Same play as line 136, new variable
state_values <- character(nrow(JPink))

###Magic loop through all records in pink catch data to assign a state.
### H=High SL = slack low, SH = slack high... 1 hour on each side of the 6 hour
### tidal cycle...
for (i in 1:nrow(JPink)) {
  # Check the Type and TimeDiffHours for each row
  if (JPink$Type[i] == "H" && JPink$TimeDiffHours[i] > 5) {
    state_values[i] <- "SL"
  } else if (JPink$Type[i] == "H" && JPink$TimeDiffHours[i] < 1) {
    state_values[i] <- "SH"
  } else if (JPink$Type[i] == "H" && JPink$TimeDiffHours[i] <= 5 && JPink$TimeDiffHours[i] >= 1) {
    state_values[i] <- "Ebb"
  } else if (JPink$Type[i] == "L" && JPink$TimeDiffHours[i] > 5) {
    state_values[i] <- "SH"
  } else if (JPink$Type[i] == "L" && JPink$TimeDiffHours[i] < 1) {
    state_values[i] <- "SL"
  } else if (JPink$Type[i] == "L" && JPink$TimeDiffHours[i] <= 5 && JPink$TimeDiffHours[i] >= 1) {
    state_values[i] <- "Flood"
  } else {
    state_values[i] <- NA
  }
}

###Stick State column to catch data
JPink$State <- state_values

###Check for pesky NAs, seeing none, moving on...
na_count <- sum(is.na(JPink$State))

#write_rds(JPink, "TidalPinks.rds")

###Read in and start from here...
combined_data <- read_rds("2024_forecast/data/raw_data/tidal_data_from_Elfin_Cove/combined_tide_data.rds")
JPink  <- read_rds("2024_forecast/data/raw_data/tidal_data_from_Elfin_Cove/TidalPinks.rds")

###Visuals please
ggplot(JPink, aes(x = Catch, fill = State)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  facet_wrap(~State, scales = "free") +
  labs(title = "Catch Distribution by State", x = "Catch", y = "Frequency")

###Kolmogorov Smirnov, see if the distributions are the same...
ks_test_results <- list()

###Unique States, SL, SH, Ebb, Flood
unique_states <- unique(JPink$State)

###Prep for KS test for catch for each pair of State
for (i in 1:(length(unique_states) - 1)) {
  for (j in (i + 1):length(unique_states)) {
    state1 <- unique_states[i]
    state2 <- unique_states[j]
    
    # Subset catch for two states
    data_state1 <- JPink$Catch[JPink$State == state1]
    data_state2 <- JPink$Catch[JPink$State == state2]
    
    # KS test set of states
    ks_result <- ks.test(data_state1, data_state2)
    
    # Result into list
    result_name <- paste("KS_Test_", state1, "_vs_", state2, sep = "")
    ks_test_results[[result_name]] <- ks_result
  }
}

### Print KS test results, can not reject the null in any case, all from the 
###same distribution, zero inflated Poisson?
for (result_name in names(ks_test_results)) {
  print(paste(result_name, ":", ks_test_results[[result_name]]$statistic, "p-value:", ks_test_results[[result_name]]$p.value))
}

###Fit as zero inflated Poisson, does support using State as a significant 
###improvement of model fit?? Now what!?
###install.packages("pscl")
library(pscl)
library(lmtest)

###Fit a zero-inflated Poisson
inflated_zipper <- zeroinfl(Catch ~ State, dist = "poisson", data = JPink)

# Print model summary
summary(inflated_zipper)

# Perform likelihood ratio test (LRT) to compare the models with and without State as a predictor
lrt <- lrtest(inflated_zipper, update(inflated_zipper, . ~ 1))
print(lrt)


summary(lrt)