library(ggplot2)
library(plotly)
library(dplyr)
library(here)
library(readr)
library(lubridate)
library(corrplot)

if (!exists("acc")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  acc <- read_rds(here("data", "processed_data", "US_Accidents_March23.rds"))
}

# accidents per day assumptions
x <- acc %>%
  group_by(State, City, date_) %>%
  summarize(
    n = n(),
    mean_severity = mean(Severity),
    mean_precip = mean(`Precipitation(in)`),
    mean_temp = mean(`Temperature(F)`),
    precip_day = mean_precip > 0,
    hot_day = mean_temp > 80,
    cold_day = mean_temp < 50,
    .groups = "drop"
  ) %>%
  na.omit()

daily <- acc %>%
  group_by(date_) %>%
  summarise(
    mean_sev = mean(Severity),
    n_acc = n(),
    mean_temp = mean(`Temperature(F)`),
    mean_precip = mean(`Precipitation(in)`),
    .groups = "drop"
  )

temp_ranges <- daily %>%
  mutate(temp_range = cut(mean_temp, breaks = seq(-20, 120, 10))) %>%
  group_by(temp_range) %>%
  summarise(
    mean_n = mean(n_acc),
    mean_sev = mean(mean_sev),
    .groups = "drop"
  ) %>%
  filter(!is.na(temp_range))
temp_ranges

# mean accidents per day by temp
p <- ggplot(temp_ranges, aes(temp_range, mean_n, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Temperature range (F)", y = "Accidents per day")
ggplotly(p)

# mean severity per day by temp
p <- ggplot(temp_ranges, aes(temp_range, mean_sev, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Temperature range (F)", y = "Mean severity / day")
ggplotly(p)

str(x)

x_cor <- cor(x %>% select(n, mean_severity, mean_precip, mean_temp))
corrplot(
  x_cor,
  type = "lower",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

t.test(data = x, mean_severity ~ precip_day, var.eq = T)
t.test(data = x, mean_severity ~ hot_day, var.eq = T)
t.test(data = x, mean_severity ~ cold_day, var.eq = T)

t.test(data = x, n ~ precip_day, var.eq = T)
t.test(data = x, n ~ hot_day, var.eq = T)
t.test(data = z, n ~ cold_day, var.eq = T)