library(dplyr)
library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)

mpg_mean <- mpg %>% 
  group_by(cyl) %>% 
  summarize(mean_hwy = mean(hwy))

plot1 <- ggplot(mpg_mean, aes(x = cyl, y = mean_hwy)) +
  geom_col()
  