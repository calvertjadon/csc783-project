##########################################
# stat_analysis.R - Statistical Analysis
# CSC 783 Data Visualization Project
##########################################

library(dplyr)
library(ggplot2)
library(reshape2)

# Source pre-cleaned accidents dataset
source("data_prep.R")

# 1. Correlation Heatmap 
numeric_data <- accidents %>%
  select(Severity, visibility, temperature, wind_chill, precipitation) %>%
  na.omit()

cor_matrix <- cor(numeric_data)
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Numerical Features", x = "", y = "")


# 2. ANOVA: Severity by Weather
anova_data <- accidents %>%
  filter(!is.na(weather) & !is.na(Severity)) %>%
  filter(weather %in% c("Clear", "Cloudy", "Rain", "Snow", "Fog"))

# Run ANOVA
anova_result <- aov(Severity ~ weather, data = anova_data)

# Output ANOVA table
summary(anova_result)


# 3. Impact of Holidays on Accident Patterns

library(lubridate)
library(dplyr)
library(ggplot2)

# fixed date holidays
custom_holidays <- c("01-01", # New Year's Day
                     "07-04", # Independence Day
                     "12-25") # Christmas

# floating holidays
get_floating_holidays <- function(years) {
  holidays <- c()
  
  for (y in years) {
    # Thanksgiving: 4th Thursday in November
    thanksgiving <- as.Date(cut(as.Date(paste0(y, "-11-01")) + weeks(3), "week")) + 4
    while (weekdays(thanksgiving) != "Thursday") {
      thanksgiving <- thanksgiving + 1
    }
    
    # Memorial Day: last Monday in May
    memorial_day <- as.Date(paste0(y, "-05-31"))
    while (weekdays(memorial_day) != "Monday") {
      memorial_day <- memorial_day - 1
    }
    
    # Labor Day: first Monday in September
    labor_day <- as.Date(paste0(y, "-09-01"))
    while (weekdays(labor_day) != "Monday") {
      labor_day <- labor_day + 1
    }
    
    holidays <- c(holidays, thanksgiving, memorial_day, labor_day)
  }
  
  as.Date(holidays)
}

# full holiday list
years <- 2016:2023
floating_days <- get_floating_holidays(years)

fixed_days <- do.call(c, lapply(years, function(y) {
  as.Date(paste0(y, "-", custom_holidays))
}))

# Combine all holidays
specific_holidays <- sort(c(fixed_days, floating_days))

# Flag holidays in accident data
acc$holiday_specific <- acc$date_ %in% specific_holidays


# T-Test: Severity
t_test_severity <- t.test(Severity ~ holiday_specific, data = acc)
print("T-test on Severity (Specific Holidays):")
print(t_test_severity)

# Frequency per day
acc_day <- acc %>%
  group_by(date_) %>%
  summarise(n_acc = n(), holiday_specific = any(holiday_specific)) %>%
  ungroup()

t_test_freq <- t.test(n_acc ~ holiday_specific, data = acc_day)
print("T-test on Frequency (Specific Holidays):")
print(t_test_freq)


# Visualization: Severity
acc %>%
  group_by(holiday_specific) %>%
  summarise(mean_severity = mean(Severity)) %>%
  ggplot(aes(x = holiday_specific, y = mean_severity, fill = holiday_specific)) +
  geom_col() +
  labs(title = "Average Severity on Specific Holidays vs. Other Days", y = "Avg Severity", x = "Is Specific Holiday")

# Visualization: Frequency
acc_day %>%
  group_by(holiday_specific) %>%
  summarise(mean_accidents = mean(n_acc)) %>%
  ggplot(aes(x = holiday_specific, y = mean_accidents, fill = holiday_specific)) +
  geom_col() +
  labs(title = "Average Daily Accidents: Specific Holidays vs. Other Days", y = "Avg Accidents/Day", x = "Is Specific Holiday")