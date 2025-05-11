##########################################
# dominiques_visualizations.R - Dominique's Visualizations
# CSC 783 Data Visualization Project
##########################################

library(dplyr)
library(ggplot2)
library(xts)
library(dygraphs)

source("data_prep.R")

# ----------- Weather vs. Severity ------------
# Filter and group by weather
weather_severity <- accidents %>%
  filter(!is.na(weather)) %>%
  group_by(weather) %>%
  summarise(
    avg_severity = mean(Severity),
    count = n()
  ) %>%
  arrange(desc(count)) %>%
  slice(1:10) 

# Bar plot
ggplot(weather_severity, aes(x = reorder(weather, -avg_severity), y = avg_severity)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Severity by Weather Condition (Top 10)",
       x = "Weather Condition", y = "Average Severity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ----------- Accidents by Hour of Day -----------

ggplot(accidents, aes(x = hour)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Accidents by Hour of Day",
       x = "Hour (24-Hour Format)", y = "Number of Accidents") +
  theme_minimal()


# ----------- Accidents by Day of Week -----------

accidents$day_of_week <- factor(accidents$day_of_week,
                                levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(accidents, aes(x = day_of_week)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Accidents by Day of the Week",
       x = "Day", y = "Number of Accidents") +
  theme_minimal()


# ----------- Accidents by Month -----------

ggplot(accidents, aes(x = month)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Accidents by Month",
       x = "Month", y = "Number of Accidents") +
  theme_minimal()


# ----------- Interactive Time Series -----------

accident_counts <- accidents %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(count = n())

accident_xts <- xts(accident_counts$count, order.by = accident_counts$date)

dygraph(accident_xts, main = "Daily Accident Counts (2016–2023)") %>%
  dySeries("V1", label = "Accidents") %>%
  dyRangeSelector()
