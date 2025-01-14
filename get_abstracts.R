library(tidyverse)
library(reticulate)
use_condaenv("textrpp_condaenv")

source_python("scripts/fetch_articles_from_journal.py")
source("api_key.R")

journals <- read_csv("data/journals_categories.csv")$Title |> unique()

articles <- lapply(journals, function(j){
  message("Fetching articles from ", j)
  out <- fetch_articles_from_journal(api_key, j)
  out <- try(out |> filter(!is.na(Abstract)))
  out
})

names(articles) <- journals
articles <- articles[!sapply(articles, inherits, what = "try-error")]
articles_df <- bind_rows(articles, .id = "journal")
articles_df <- articles_df |> 
  rename_with(str_to_lower)

write_csv(articles_df, "data/articles.csv")

