library(tidyverse)

subjects <- read_csv("data/category_by_subject.csv")
articles <- read_csv("data/articles_sims.csv.gz") |> 
  separate_wider_delim(condition, "_", names = c("persona", "request")) |> 
  drop_na() |> 
  left_join(subjects, by = c("journal" = "Title" , "field" = "Category")) |> 
  rename(subject = Best.Subject.Area)

# {
#   articles |> 
#     mutate(
#       persona = case_match(
#         persona,
#         "phd" ~ "I am a PhD student",
#         "lecturer" ~ "I am a university lecturer",
#         "teacher" ~ "I am a high school teacher",
#         "executive" ~ "I am a business executive"
#       ),
#       request = case_match(
#         request,
#         "consensus" ~ "consensus conclusions",
#         "ideas" ~ "key ideas",
#         "questions" ~ "unresolved questions"
#       )) 
# }

#subject mapping : (maybe find a better way):
{
  articles <- articles %>%
    mutate(
      category = field,
      field = case_when(
        subject == "Multidisciplinary" ~ "General",
        subject == "Arts and Humanities" ~ "Social Sciences and Humanities",
        subject == "Agricultural and Biological Sciences" ~ "Life Sciences",
        subject == "Biochemistry, Genetics and Molecular Biology" ~ "Life Sciences",
        subject == "Business, Management and Accounting" ~ "Business and Economics",
        subject == "Chemical Engineering" ~ "Engineering and Technology",
        subject == "Chemistry" ~ "Physical Sciences",
        subject == "Computer Science" ~ "Engineering and Technology",
        subject == "Decision Sciences" ~ "Business and Economics",
        subject == "Earth and Planetary Sciences" ~ "Physical Sciences",
        subject == "Economics, Econometrics and Finance" ~ "Business and Economics",
        subject == "Energy" ~ "Engineering and Technology",
        subject == "Engineering" ~ "Engineering and Technology",
        subject == "Environmental Science" ~ "Environmental Studies",
        subject == "Immunology and Microbiology" ~ "Life Sciences",
        subject == "Health Professions" ~ "Health Sciences",
        subject == "Materials Science" ~ "Engineering and Technology",
        subject == "Mathematics" ~ "Physical Sciences",
        subject == "Medicine" ~ "Health Sciences",
        subject == "Neuroscience" ~ "Life Sciences",
        subject == "Nursing" ~ "Health Sciences",
        subject == "Pharmacology, Toxicology and Pharmaceutics" ~ "Life Sciences",
        subject == "Physics and Astronomy" ~ "Physical Sciences",
        subject == "Psychology" ~ "Social Sciences and Humanities",
        subject == "Social Sciences" ~ "Social Sciences and Humanities",
        subject == "Veterinary" ~ "Life Sciences",
        subject == "Dentistry" ~ "Health Sciences",
        TRUE ~ as.character(subject)  # Default case to handle any unmatched conditions
      )
    )
  }

articles$persona <- factor(articles$persona)
articles$request <- factor(articles$request)
articles$subject <- factor(articles$subject)
articles$field <- factor(articles$field)
