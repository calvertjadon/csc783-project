library(ggplot2)
library(plotly)
library(readr)
library(here)
library(dplyr)
library(corrplot)
library(stringr)

if (!exists("acc")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  acc <- read_rds(here("data", "processed_data", "US_Accidents_March23.rds"))
}

# ACCIDENT SEVERITY COUNTS
sev_count <- acc %>%
  count(sevg)
sev_count

p <- ggplot(sev_count, aes(x = sevg, y = n)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Accident Counts by Severity (2016 - 2023)", x = "Severity", y = "Number of Accidents")
sev_count_p <- ggplotly(p)
sev_count_p

# ACCIDENTS PER MONTH
acc_month <- acc %>%
  count(year_, month_) %>%
  mutate(
    # convert each date to the first of the respective month
    month_year = str_c(year_, "-", str_pad(
      month_, 2, side = "left", pad = "0"
    ), "-01"),
    d = as.Date(month_year)
  )
acc_month

p <- ggplot(acc_month, aes(x = d, y = n)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Accidents / month")
acc_month_p <- ggplotly(p)
acc_month_p

# ACCIDENTS PER HOUR
acc_hour <- acc %>%
  count(hour_)
acc_hour

p <- ggplot(acc_hour, aes(x = hour_, y = n)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 23, by = 1))
acc_hour_p <- ggplotly(p)
acc_hour_p
