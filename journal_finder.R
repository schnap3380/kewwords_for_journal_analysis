library(tidyverse)
library(dplyr)

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
  select(Title, Best.Categories , Best.Subject.Area)  %>%
  separate_rows(Best.Categories, sep = ",") %>%
  rename(Category = Best.Categories)
# write_csv(cleaned_data, "journals_categories.csv")


cleaned_data$Category <- gsub("\\(miscellaneous\\)", "", cleaned_data$Category)
cleaned_data$Category <- gsub("\\[", "", cleaned_data$Category)
cleaned_data$Category <- gsub("\\]", "", cleaned_data$Category)
cleaned_data$Category <- gsub("\\'", "", cleaned_data$Category)
cleaned_data$Category <- stringr::str_trim(cleaned_data$Category, side = "both")

unique(cleaned_data$Category)

unique(cleaned_data[order(cleaned_data$Category),]$Category)
unique(cleaned_data[order(cleaned_data$Best.Subject.Area),]$Best.Subject.Area)

write_csv(cleaned_data, "data/category_by_subject.csv")
#making queries:


#I am a business executive who is interested in learning more about what science can teach us about [Insert field]
#I am a PHD student  who is interested in learning more about what science can teach us about [Insert field]

#what are some things that the field of social cognition has come to a consensus on?
personas = c('I am a PhD student',
             'I am a university lecturer',
             'I am a business executive',
             'I am a high school teacher')

categories <- unique(cleaned_data[order(cleaned_data$Category),]$Category)

requests <- c('key ideas',
              'consensus conclusions',
              'unresolved questions')

form <- expand.grid(
  persona = personas,
  request = requests,
  field = categories
)

#randomize the rows:
set.seed(42)
form <- form[sample(1:nrow(form),nrow(form), replace=FALSE),]

#making the promts:

queries <- c()
for (i in 1:nrow(form)) {
  queries[i] <- paste0(
    form[i,"persona"], " who is interested in learning more about ", form[i,"field"],". ", 
    "What are some ", form[i,"request"], " in the field of ", form[i,"field"], "?"
    )
  }
















