library(tidyverse)
library(embedplyr)

# load data
gemini_responses <- read_csv("data/gemini_responses.csv") |> 
  mutate(
    persona_name = case_match(persona, 
                              "I am a PhD student" ~ "phd",
                              "I am a university lecturer" ~ "lecturer",
                              "I am a high school teacher" ~ "teacher",
                              "I am a business executive" ~ "executive"),
    request_name = case_match(request,
                              "unresolved questions" ~ "questions",
                              "key ideas" ~ "ideas",
                              "consensus conclusions" ~ "consensus"),
    condition = paste(persona_name, request_name, sep = "_")
  )

journals <- read_csv("data/journals_categories.csv") |> 
  rename(journal = Title, field = Category) |> 
  group_by(journal) |> 
  summarise(field = head(field, 1L)) # first category associated with journal

articles <- read_csv("data/articles.csv") |> 
  left_join(journals) |> 
  mutate(article_id = as.character(seq_along(abstract)))

# load embeddings model
glove_mod <- load_embeddings("glove.6B.200d")

# embed gemini responses (persona X request X field = 12 embeddings per field)
conditions <- unique(gemini_responses$condition)
fields <- unique(gemini_responses$field)

condition_embeddings_list <- lapply(conditions, function(c){
  condition_responses <- gemini_responses |> 
    filter(condition == c)
  condition_embeddings <- condition_responses |> 
    embed_docs("gemini_response", glove_mod, id_col = "field", output_embeddings = TRUE)
  condition_embeddings
})
names(condition_embeddings_list) <- conditions

# embed abstracts
articles_embeddings <- articles |> 
  embed_docs("abstract", glove_mod, .keep_all = TRUE, id_col = "article_id")

articles <- bind_rows(lapply(fields, function(f){
  field_embeddings <- articles_embeddings |> 
    filter(field == f)
  if (nrow(field_embeddings) == 0) return(tibble())
  field_embeddings |> 
    get_sims(
      dim_1:dim_200,
      list(
        sim_phd_questions = condition_embeddings_list$phd_questions[f,],
        sim_phd_ideas = condition_embeddings_list$phd_ideas[f,],
        sim_lecturer_consensus = condition_embeddings_list$lecturer_consensus[f,],
        sim_teacher_ideas = condition_embeddings_list$teacher_ideas[f,],
        sim_lecturer_questions = condition_embeddings_list$lecturer_questions[f,],
        sim_teacher_questions = condition_embeddings_list$teacher_questions[f,],
        sim_executive_consensus = condition_embeddings_list$executive_consensus[f,],
        sim_teacher_consensus = condition_embeddings_list$teacher_consensus[f,],
        sim_lecturer_ideas = condition_embeddings_list$lecturer_ideas[f,],
        sim_executive_questions = condition_embeddings_list$executive_questions[f,],
        sim_phd_consensus = condition_embeddings_list$phd_consensus[f,],
        sim_executive_ideas = condition_embeddings_list$executive_ideas[f,]
      ),
      method = "cosine_squished"
    )
}))

# pivot similarity scores to one column
articles <- articles |> 
  pivot_longer(
    sim_phd_questions:sim_executive_ideas, 
    names_to = "condition", names_prefix = "sim_", 
    values_to = "sim"
    )

# save dataset
articles <- articles |> 
  arrange(article_id) |> 
  select(-abstract)

write_csv(articles, "data/articles_sims.csv")

# I can't help but take a peak at the data...
articles <- read_csv("data/articles_sims.csv")
articles |> 
  separate_wider_delim(condition, "_", names = c("persona", "request")) |> 
  ggplot(aes(year, sim, color = persona)) +
    geom_smooth() +
    facet_wrap(~request)
