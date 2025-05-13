# Geographic trends: If you’re comfortable, you could focus on the heat map by
#    state and the environmental factors analysis (weather vs. road conditions).

library(ggplot2)
library(plotly)
library(dplyr)
library(here)
library(readr)
library(ggiraphExtra)
library(maps)
library(mapproj)
library(stringr)

if (!exists("acc")) {
  # accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  acc <- read_rds(here("data", "processed_data", "US_Accidents_March23.rds"))
}

# SEVERITY BY STATE
meanseverity_state <- acc %>%
  group_by(State, state_name) %>%
  summarise(
    mean_severity = mean(Severity),
    mean_temp = mean(`Temperature(F)`),
    n_acc = n()
  ) %>%
  arrange(desc(mean_severity))
meanseverity_state

p <- ggplot(
  data = meanseverity_state,
  aes(
    x = reorder(State, -mean_severity),
    y = mean_severity
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Average Accident Severity by State (2016 – 2023)",
    x = NULL,
    y = "Mean Severity (1 = least, 4 = most)"
  )

meanseverity_state_p <- ggplotly(p, tooltip = c("x", "y"))
meanseverity_state_p

states_map <- map_data("state")
str(states_map)
states_map$region <- str_to_title(states_map$region)
str(states_map)

meanseverity_state_p2 <- ggChoropleth(
  data = meanseverity_state,
  aes(fill = mean_severity, map_id = state_name),
  map = states_map,
  interactive = T
)
meanseverity_state_p2

meanseverity_state_p3 <- ggChoropleth(
  data = meanseverity_state,
  aes(fill = mean_temp, map_id = state_name),
  map = states_map,
  interactive = T
)
meanseverity_state_p3

est_pop_state_2023 <- data.frame(
  state_name = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  pop = c(
    5108468, 733406, 7431344, 3067732, 38965193, 5877610, 3617176, 1031890,
    678972, 22610726, 11029227, 1435138, 1964726, 12549689, 6862199, 3207004,
    2940546, 4526154, 4573749, 1395722, 6180253, 7001399, 10037261, 5737915,
    2939690, 6196156, 1132812, 1978379, 3194176, 1402054, 9290841, 2114371,
    19571216, 10835491, 783926, 11785935, 4053824, 4233358, 12961683, 1095962,
    5373555, 919318, 7126489, 30503301, 3417734, 647464, 8715698, 7812880,
    1770071, 5910955, 584057
  )
)
str(est_pop_state_2023)


n_acc_state <- acc %>%
  group_by(State, state_name, year_) %>%
  summarise(n_acc = n(), groups = "drop") %>%
  left_join(est_pop_state_2023, by = "state_name") %>%
  group_by(State, state_name) %>%
  mutate(
    acc_per_100k = 100000 * mean(n_acc) / pop
  )
n_acc_state

n_acc_state_p <- ggChoropleth(
  data = n_acc_state,
  aes(fill = acc_per_100k, map_id = state_name),
  map = states_map,
  interactive = T,
  title = "Total Accidents Per 100K Residents Per Year",
)
n_acc_state_p

state_day <- acc %>%
  mutate(date = as.Date(Start_Time)) %>%
  group_by(State, date) %>%
  summarise(
    n_acc = n(),
    mean_temp = mean(`Temperature(F)`, na.rm = TRUE),
    .groups = "drop"
  )

state_cor <- state_day %>%
  group_by(State) %>%
  summarise(
    r = cor(mean_temp, n_acc, use = "pair"),
    .groups = "drop"
  ) %>%
  mutate(state_name = state.name[match(State, state.abb)], map_id = state_name) %>%
  filter(!is.na(r))

ggChoropleth(
  data = state_cor,
  aes(fill = r, map_id = map_id),
  map = states_map,
  interactive = TRUE
)
