library(readr)
library(here)
library(dplyr)

if(!exists("accidents")) {
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

table(is.na(accidents_raw$Severity))

accidents <- accidents_raw %>% 
  mutate(sevg = ifelse(Severity == 1, "least severe",
                       ifelse(Severity == 2, "less severe",
                              ifelse(Severity == 3, "more severe", "most severe"))))

write_rds(accidents, here("data", "processed_data", "US_Accidents_March23.rds"))
