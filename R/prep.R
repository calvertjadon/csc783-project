library(readr)
library(here)
library(dplyr)
library(stringr)
library(lubridate)

if(!exists("accidents_raw")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  accidents_raw <- read_csv(
    here("data", "raw_data", "US_Accidents_March23.csv"),
    lazy = FALSE
  )
}

acc <- accidents_raw

# inspection
head(acc)
dim(acc)
str(acc)
summary(acc)
colSums(is.na(acc))


# cleanup
acc <- acc %>% 
  mutate(
    date_ = date(Start_Time),
    year_ = year(Start_Time),
    month_ = month(Start_Time),
    hour_ = hour(Start_Time),
    any_precip= !is.na(`Precipitation(in)`) & `Precipitation(in)` > 0,
  )
str(acc)

# augmentation
state_lookup <- tibble(
  State  = state.abb,
  state_name  = str_to_title(state.name)
)

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
