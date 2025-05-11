##########################################
# stat_analysis.R - Statistical Analysis
# CSC 783 Data Visualization Project
##########################################

library(dplyr)
library(ggplot2)
library(reshape2)

# Source pre-cleaned accidents dataset
source("data_prep.R")

# Correlation Heatmap 
numeric_data <- accidents %>%
  select(Severity, visibility, temperature, wind_chill, precipitation) %>%
  na.omit()

cor_matrix <- cor(numeric_data)
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Numerical Features", x = "", y = "")


# ANOVA: Severity by Weather
anova_data <- accidents %>%
  filter(!is.na(weather) & !is.na(Severity)) %>%
  filter(weather %in% c("Clear", "Cloudy", "Rain", "Snow", "Fog"))

# Run ANOVA
anova_result <- aov(Severity ~ weather, data = anova_data)

# Output ANOVA table
summary(anova_result)
