library(tidyverse)
library(dplyr)
install.packages('writexl')
library(writexl)

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
cleaned_data$Category <- stringr::str_trim(cleaned_data$Category)

unique(cleaned_data$Category)

unique(cleaned_data[order(cleaned_data$Category),]$Category)


#making queries:


#I am a business executive who is interested in learning more about what science can teach us about [Insert field]
#I am a PHD student  who is interested in learning more about what science can teach us about [Insert field]

#what are some things that the field of social cognition has come to a consensus on?
promts = c('I am a business executive who is interested in learning more about what science can teach us about',
           'I am a PHD student  who is interested in learning more about what science can teach us about')
categories <- unique(cleaned_data[order(cleaned_data$Category),]$Category)


form <- expand.grid(
  promt = promts,
  field = categories
)

#randomize the rows:
seed(42)
form <- form[sample(1:nrow(form),nrow(form), replace=FALSE),]

#making the promts:

queries <- c()
sen_part_1 <- 'What are some things that the field of'
sen_part_2 <- 'has come to a consensus on?'
for (i in 1:nrow(form)) {
  queries[i] <- paste(form[i,1],form[i,2],". ", sen_part_1, form[i,2],sen_part_2, sep = " ")
  
  }
















