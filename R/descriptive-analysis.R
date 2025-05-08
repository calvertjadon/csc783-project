library(ggplot)
library(plotly)
library(readr)

if(!exists("accidents")) {
  #accidents <- read.csv("data/raw_data/US_Accidents_March23.csv")
  accidents <- read_csv("data/raw_data/US_Accidents_March23.csv",
                        lazy = TRUE)
}


head(accidents)
dim(accidents)
str(accidents)
summary(accidents)

table(is.na(accidents$Severity))

accidents <- accidents %>% 
  mutate(sevg = ifelse(Severity == 1, "least severe",
                       ifelse(Severity == 2, "less severe",
                              ifelse(Severity == 3, "more severe", "most severe"))))

severity_counts <- accidents %>% 
  count(sevg)

severity_counts

severity_counts_p <- ggplotly(
  ggplot(data = severity_counts, aes(x = sevg, y = n, fill = sevg)) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    geom_text(aes(label = scales::comma(n))) +
    labs(
      title = "Accident Counts by Severity (2016 - 2023)",
      x = "Severity",
      y = "Number of Accidents"
    ),
  tooltip = c("x", "y")
)

severity_counts_p
