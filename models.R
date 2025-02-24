library(mgcv)
library(tidyverse)


subjects <- read_csv("data/category_by_subject.csv")
articles <- read_csv("data/articles_sims.csv.gz") |> 
  separate_wider_delim(condition, "_", names = c("persona", "request"))
articles <- na.omit(articles)

#merge articles and subject:
articles <- articles %>%
  left_join(subjects, by = c("journal" = "Title" , "field" = "Category"))
colnames(articles)
colnames(articles)[5] <- "Category"
colnames(articles)[9] <- "Subject"

{
articles |> 
  mutate(
    persona = case_match(
      persona,
      "phd" ~ "I am a PhD student",
      "lecturer" ~ "I am a university lecturer",
      "teacher" ~ "I am a high school teacher",
      "executive" ~ "I am a business executive"
    ),
    request = case_match(
      request,
      "consensus" ~ "consensus conclusions",
      "ideas" ~ "key ideas",
      "questions" ~ "unresolved questions"
    )) 
}

#subject mapping : (maybe find a better way):
{
articles <- articles %>%
  mutate(
    field = case_when(
      Subject == "Multidisciplinary" ~ "General",
      Subject == "Arts and Humanities" ~ "Social Sciences and Humanities",
      Subject == "Agricultural and Biological Sciences" ~ "Life Sciences",
      Subject == "Biochemistry, Genetics and Molecular Biology" ~ "Life Sciences",
      Subject == "Business, Management and Accounting" ~ "Business and Economics",
      Subject == "Chemical Engineering" ~ "Engineering and Technology",
      Subject == "Chemistry" ~ "Physical Sciences",
      Subject == "Computer Science" ~ "Engineering and Technology",
      Subject == "Decision Sciences" ~ "Business and Economics",
      Subject == "Earth and Planetary Sciences" ~ "Physical Sciences",
      Subject == "Economics, Econometrics and Finance" ~ "Business and Economics",
      Subject == "Energy" ~ "Engineering and Technology",
      Subject == "Engineering" ~ "Engineering and Technology",
      Subject == "Environmental Science" ~ "Environmental Studies",
      Subject == "Immunology and Microbiology" ~ "Life Sciences",
      Subject == "Health Professions" ~ "Health Sciences",
      Subject == "Materials Science" ~ "Engineering and Technology",
      Subject == "Mathematics" ~ "Physical Sciences",
      Subject == "Medicine" ~ "Health Sciences",
      Subject == "Neuroscience" ~ "Life Sciences",
      Subject == "Nursing" ~ "Health Sciences",
      Subject == "Pharmacology, Toxicology and Pharmaceutics" ~ "Life Sciences",
      Subject == "Physics and Astronomy" ~ "Physical Sciences",
      Subject == "Psychology" ~ "Social Sciences and Humanities",
      Subject == "Social Sciences" ~ "Social Sciences and Humanities",
      Subject == "Veterinary" ~ "Life Sciences",
      Subject == "Dentistry" ~ "Health Sciences",
      TRUE ~ as.character(Subject)  # Default case to handle any unmatched conditions
    )
  )
}

articles$persona <- factor(articles$persona)
articles$request <- factor(articles$request)
articles$Subject <- factor(articles$Subject)
articles$field <- factor(articles$field)


#We will use bam model:
{


  
  data_sample <-  articles[sample(nrow(articles), size = floor(0.1 * nrow(articles))), ]
  
  mod <- bam(
    #sim ~ s(year, by = interaction(Subject, persona, request)) + Subject * persona * request,
    sim ~ s(year, by = interaction(field, persona, request)) + field * persona * request,
    #sim ~ s(year, by = field) + field,
    data = data_sample)
  summary(mod)
  

  # trying to use slopes: (but it takes too long)
  {
  library(marginaleffects)
  library(future)
  plan(multisession, workers = 4)  # Use 4 CPU cores
    
  # gratia
  #avg_slopes(mod, variables = "year",  by = c("year", "field"))
  #plot_slopes(mod, variables = "year", by = c("year", "field")) + geom_hline(yintercept = 0)
  
  # Compute slopes over time
  slopes_data <- avg_slopes(mod, variables = "year",  by = c("year", "field"))
  
  }
  
  

  #Question 0:
  #What is the expected curve of similarity with real research?
  {
  data_sample <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]
  
  mod_partial <- bam(
    sim ~ s(year, by = field) + field * persona * request,
    data = data_sample
  )
  summary(mod_partial)
  
  data_for_slopes <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]
  slopes_data <- avg_slopes(mod_partial,newdata = data_for_slopes, variables = "year",  by = c("year", "field"))
  
  #trying to use slope: work better then before but still not working
  {
  field_slopes <- slopes_data %>% arrange(desc(estimate))
  ggplot(field_slopes, aes(x = reorder(field, estimate), y = estimate, ymin = conf.low, ymax = conf.high, fill = field)) +
    geom_col() +
    geom_errorbar(width = 0.2) +
    coord_flip() +
    labs(title = "How Up to Date LLMs Are Across Fields",
         x = "Field",
         y = "Marginal Effect of Year on Similarity") +
    theme_minimal()
  }
  # Create a prediction grid for all combinations of year, field, persona, and request
  year_range <- range(data_sample$year)
  field_levels <- unique(data_sample$field)
  persona_levels <- unique(data_sample$persona)
  request_levels <- unique(data_sample$request)
  pred_data <- expand.grid(
    year = seq(from = year_range[1], to = year_range[2], by = 1),
    field = field_levels,
    persona = persona_levels,
    request = request_levels
  )
  
  predictions <- predictions(mod_partial, newdata = pred_data, type = "response", vcov = TRUE) #confidence interval estimation only by year
  pred_data$sim_pred <- predictions$estimate
  pred_data$conf_low <- predictions$conf.low
  pred_data$conf_high <- predictions$conf.high
  
  agg_pred_data <- pred_data %>%
    group_by(year, field) %>%
    summarise(
      mean_sim_pred = mean(sim_pred), 
      conf_low = mean(conf_low),  # Approximate CI  | maybe use min?
      conf_high = mean(conf_high),  # Approximate CI | maybe use max?
      .groups = "drop"
    )
  
  ggplot(agg_pred_data, aes(x = year, y = mean_sim_pred, group = field, color = field)) +
    geom_line(size = 1) +  # Thicker lines for clarity
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = field), alpha = 0.2) +
    labs(title = "Predicted SIM by Year for Each Field",
         x = "Year",
         y = "Predicted SIM",
         color = "Field") +
    scale_color_brewer(palette = "Set1") +  # Add color palette
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  
  
  
  #Question 2:
  #Which Questions Get the Most Scientific Answers?
  request_slopes <- avg_slopes(mod_partial, variables = "request")
  
  # Sort by scientific accuracy
  request_slopes <- request_slopes %>% arrange(desc(estimate))
  
  print(request_slopes)
  
  # Plot results
  ggplot(request_slopes, aes(x = reorder(request, estimate), y = estimate, ymin = conf.low, ymax = conf.high, fill = request)) +
    geom_col() +
    geom_errorbar(width = 0.2) +
    coord_flip() +
    labs(title = "Which Questions Get the Most Scientific Answers?",
         x = "Question Type",
         y = "Effect on Similarity") +
    theme_minimal()
  
  
  
  
  
  
  #old Question 0:
  {
    
    #What is the expected curve of similarity with real research?
    # using Predictions:
    year_range <- range(data_sample$year)
    field_levels <- unique(data_sample$field)
    persona_levels <- unique(data_sample$persona)
    request_levels <- unique(data_sample$request)
    
    # Create a prediction grid for all combinations of year, field, persona, and request
    pred_data <- expand.grid(
      year = seq(from = year_range[1], to = year_range[2], by = 1),
      field = field_levels,
      persona = persona_levels,
      request = request_levels
    )
    
    # Generate predictions
    pred_data$sim_pred <- predict(mod, newdata = pred_data, type = "response")
    
    
    
    
    
    pred_summary <- pred_data %>%
      group_by(year, field) %>%
      summarise(mean_sim_pred = mean(sim_pred), .groups = "drop")
    
    
    
    # Find the year of maximum predicted SIM for each field
    peak_years <- pred_summary %>%
      group_by(field) %>%
      summarise(YearOfMaxSim = year[which.max(mean_sim_pred)],
                MaxSim = max(mean_sim_pred), .groups = "drop")
    
    print(peak_years)
    
    # Plot
    ggplot(pred_summary, aes(x = year, y = mean_sim_pred, group = field, color = field)) +
      geom_line(size = 1) +  # Thicker lines for clarity
      labs(title = "Predicted SIM by Year for Each Field",
           x = "Year",
           y = "Predicted SIM",
           color = "Field") +
      scale_color_brewer(palette = "Set1") +  # Add color palette
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

  
}  











