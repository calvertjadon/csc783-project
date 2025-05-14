library(readr)
library(here)
library(dplyr)
library(stringr)
library(lubridate)

if (!exists("accidents_raw")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  accidents_raw <- read_csv(here("data", "raw_data", "US_Accidents_March23.csv"), lazy = FALSE)
}

acc <- accidents_raw

# inspection
head(acc)
dim(acc)
str(acc)
summary(acc)
colSums(is.na(acc))

acc %>%
  filter(`Temperature(F)` > 130 | `Temperature(F)` < -60) %>%
  group_by(year(Start_Time)) %>%
  summarize(n = n())

# cleanup
acc <- acc %>%
  filter(between(`Temperature(F)`, -60, 130)) %>%
  mutate(
    date_ = date(Start_Time),
    year = year(Start_Time),
    month = month(Start_Time),
    hour = hour(Start_Time),
    precipitation = coalesce(`Precipitation(in)`, 0),
    any_precip = precipitation > 0,
    weather = coalesce(Weather_Condition, "Unknown"),
    temperature = coalesce(`Temperature(F)`, mean(`Temperature(F)`, na.rm = TRUE)),
    visibility = `Visibility(mi)`,
    wind_chill = `Wind_Chill(F)`
  )
str(acc)
summary(acc)

table(is.na(acc$`Precipitation(in)`))

# augmentation
state_lookup <- tibble(State  = state.abb, state_name  = str_to_title(state.name))

acc <- acc %>%
  mutate(
    sevg = case_when(
      Severity == 1 ~ "least severe",
      Severity == 2 ~ "less severe",
      Severity == 3 ~ "more severe",
      TRUE ~ "most severe"
    )
  ) %>%
  inner_join(state_lookup, by = "State")

str(acc %>% select(State, state_name))

# write_rds(acc, here("data", "processed_data", "US_Accidents_March23.rds"))

# Make available as `accidents` for scripts
accidents <- acc