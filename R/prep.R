library(readr)
library(here)
library(dplyr)
library(stringr)

if(!exists("accidents_raw")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  accidents_raw <- read_csv(
    here("data", "raw_data", "US_Accidents_March23.csv"),
    lazy = FALSE
  )
}

head(accidents_raw)
dim(accidents_raw)
str(accidents_raw)
summary(accidents_raw)

# make sure no na in severity
table(is.na(accidents_raw$Severity))

# lookup table to map state codes to state names (for choropleth)
state_lookup <- tibble(
  State  = state.abb,
  state_name  = str_to_title(state.name)
)

accidents <- accidents_raw %>% 
  mutate(sevg = ifelse(Severity == 1, "least severe",
                       ifelse(Severity == 2, "less severe",
                              ifelse(Severity == 3, "more severe", "most severe")))) %>% 
  inner_join(state_lookup, by = "State")
str(accidents %>% select(State, state_name))

write_rds(accidents, here("data", "processed_data", "US_Accidents_March23.rds"))
