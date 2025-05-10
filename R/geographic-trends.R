# Geographic trends: If you’re comfortable, you could focus on the heat map by
#    state and the environmental factors analysis (weather vs. road conditions).

meanseverity_state <- accidents %>% 
  group_by(State) %>% 
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
