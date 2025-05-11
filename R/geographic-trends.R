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

if(!exists("acc")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  acc <- read_rds(
    here("data", "processed_data", "US_Accidents_March23.rds")
  )
}

# SEVERITY BY STATE
meanseverity_state <- acc %>% 
  group_by(State, state_name) %>% 
  summarise(mean_severity = mean(Severity)) %>% 
  arrange(desc(mean_severity))
meanseverity_state

p <- ggplot(data = meanseverity_state,
            aes(x = reorder(State, -mean_severity),
                y = mean_severity)) +
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

meanseverity_state_p2 <- ggChoropleth(data = meanseverity_state,
             aes(fill = mean_severity,
                 map_id = state_name),
             map = states_map,
             interactive = T)
meanseverity_state_p2
