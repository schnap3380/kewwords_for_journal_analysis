library(tidyverse)
library(dplyr)

Data <- read.csv("journal_ranking_data.CSV")

sapply(Data, function(x) sum(is.na(x)))

Data$Category_number <- sum(Data[30:56])

filtered_data <- Data %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("X")), na.rm = T))

filtered_data <- filtered_data[filtered_data$sum==1 , ]

# Find the top 5 cited journals for each category
top_journals <- filtered_data %>%
  pivot_longer(cols = colnames(Data[30:56]), names_to = "Category", values_to = "Value") %>%
  filter(Value > 0) %>%
  group_by(Category) %>%
  arrange(desc(CiteScore)) %>%
  slice_head(n = 5) %>%
  ungroup()




#if coverage < 2005:

filtered_data <- Data %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("X")), na.rm = T))

filtered_data <- filtered_data[filtered_data$sum==1 & filtered_data$Coverage < 2005, ]

# Find the top 5 cited journals for each category
top_journals_coverage <- filtered_data %>%
  pivot_longer(cols = colnames(Data[30:56]), names_to = "Category", values_to = "Value") %>%
  filter(Value > 0) %>%
  group_by(Category) %>%
  arrange(desc(CiteScore)) %>%
  slice_head(n = 5) %>%
  ungroup()

sum(top_journals$Total.Refs.)
sum(top_journals_coverage$Total.Refs.)

