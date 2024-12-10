library(tidyverse)
library(dplyr)
install.packages('writexl')
library(writexl)
install.packages('fedmatch')
library(fedmatch)

Data <- read.csv("journal_ranking_data.CSV")

sapply(Data, function(x) sum(is.na(x)))

Data$Category_number <- sum(Data[30:56])


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



cleaned_data <- top_journals_coverage %>%
  select(Title, Best.Categories)  %>%
  separate_rows(Best.Categories, sep = ",") %>%
  rename(Category = Best.Categories)

cleaned_data$Category <- gsub("\\(miscellaneous\\)", "", cleaned_data$Category)
cleaned_data$Category <- gsub("\\[", "", cleaned_data$Category)
cleaned_data$Category <- gsub("\\]", "", cleaned_data$Category)
cleaned_data$Category <- gsub("\\'", "", cleaned_data$Category)

unique(cleaned_data$Category)

unique(cleaned_data[order(cleaned_data$Category),]$Category)





