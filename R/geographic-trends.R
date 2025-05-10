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

if(!exists("accidents")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  accidents <- read_rds(
    here("data", "processed_data", "US_Accidents_March23.rds")
  )
}

meanseverity_state <- accidents %>% 
  group_by(State, state_name) %>% 
  summarise(mean_severity = mean(Severity)) %>% 
  arrange(desc(mean_severity))
meanseverity_state

meanseverity_state_p <- ggplotly(ggplot(
  data = meanseverity_state,
  aes(x = factor(State, levels = State),
      y = mean_severity,
      fill = mean_severity)
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  labs(
    title = "Average Accident Severity by State (2016 – 2023)",
    x = NULL,
    y = "Mean Severity (1 = least, 4 = most)"
  ),
  tooltip = c("x", "y")
)
meanseverity_state_p

states_map <- map_data("state")
str(states_map)
states_map$region <- str_to_title(states_map$region)

ggChoropleth(data = meanseverity_state,
             aes(fill = mean_severity,
                 map_id = state_name),
             map = states_map,
             interactive = T)
