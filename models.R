library(mgcv)
library(tidyverse)
library(marginaleffects) #for predictions and slopes

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

    
  # gratia
  #avg_slopes(mod, variables = "year",  by = c("year", "field"))
  #plot_slopes(mod, variables = "year", by = c("year", "field")) + geom_hline(yintercept = 0)
  
  # Compute slopes over time
  slopes_data <- avg_slopes(mod, variables = "year",  by = c("year", "field"))
  
  }
  
  #the used model is in Q0:

  #Question 0:
  #What is the expected curve of similarity with real research?
  {
  data_sample <-  articles[sample(nrow(articles), size = floor(1 * nrow(articles))), ]
  
  mod_partial <- bam(
    sim ~ s(year, by = field) + field * persona * request,
    data = data_sample
  )
  summary(mod_partial)
  

  #trying to use slope: work better then before but still not working
  {
  data_for_slopes <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]
  slopes_data <- avg_slopes(mod_partial,newdata = data_for_slopes, variables = "year",  by = c("year", "field"))
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
      .groups = "drop")
  
  #final option:
  {
    library(viridis)
    
    ggplot(agg_pred_data, aes(x = year, y = mean_sim_pred, group = field, color = field)) +
      geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +  # LOESS smoothing for a smooth curve
      geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = field), alpha = 0.15, color = NA) +  # Soft confidence bands
      scale_color_viridis_d(option = "C", end = 0.85) +  # Elegant colors
      scale_fill_viridis_d(option = "C", end = 0.85) +
      labs(
        title = "Smoothed Predicted Similarity Over Time by Field",
        x = "Year",
        y = "Predicted SIM"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank()
      )
    
    }
  #another option for plot:
  {
  ggplot(agg_pred_data, aes(x = year, y = mean_sim_pred, group = field, color = field)) +
    geom_line(size = 1) +  # Thicker lines for clarity
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.2 , fill = NA ) +
    scale_color_viridis_d(option = "D", end = 0.85) +  # Modern color palette for lines
    scale_fill_viridis_d(option = "D", end = 0.85) +  # Same palette for confidence intervals
    labs(title = "Predicted SIM by Year for Each Field",
         x = "Year",
         y = "Predicted SIM",
         color = "Field") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center title
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "bottom",  # Move legend for better space management
      legend.title = element_blank(),  # Remove redundant "Field" title
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(),  # Remove excessive grid lines
      panel.grid.minor = element_blank()
    )
    }
  #or:
  {
    library(ggplot2)
    library(viridis)
    
    ggplot(agg_pred_data, aes(x = year, y = mean_sim_pred, group = field, color = field)) +
      geom_line(size = 1.2) +  # Thicker lines for clarity
      geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = field), alpha = 0.15, color = NA) +  # Confidence bands with no border
      scale_color_viridis_d(option = "C", end = 0.85) +  # Modern color palette for lines
      scale_fill_viridis_d(option = "C", end = 0.85) +  # Same palette for confidence intervals
      labs(
        title = "Predicted Similarity Over Time by Field",
        x = "Year",
        y = "Predicted SIM"
      ) +
      theme_minimal(base_size = 14) +  # Set clean base theme with readable text
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered bold title
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",  # Move legend for better space usage
        legend.title = element_blank(),  # Remove redundant "Field" title
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),  # Remove unnecessary major grid lines
        panel.grid.minor = element_blank()  # Remove unnecessary minor grid lines
      )
    
  }
  }
  #Question 1:
  #For which fields are LLMs most up to date?
  {
  #scatter for the MaxSim:
  {
  peak_years <- agg_pred_data %>%
    group_by(field) %>%
    summarise(YearOfMaxSim = year[which.max(mean_sim_pred)],
              Sim = max(mean_sim_pred),
              conf_low = conf_low[which.max(mean_sim_pred)],  # Approximate CI  | maybe use min?
              conf_high = conf_high[which.max(mean_sim_pred)],  # Approximate CI | maybe use max?
              .groups = "drop")
  
  
  # Create the scatter plot with switched axes
  ggplot(peak_years, aes(x = field, y = Sim, color = YearOfMaxSim)) +
    geom_point(size = 4) +  # Scatter plot points
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2, color = "black") +  # Vertical error bars for CI
    scale_color_viridis_c(option = "C" , direction = -1) +  # Color scale for YearOfMaxSim
    labs(
      title = "Peak Similarity Across Fields",
      x = "Field",
      y = "Similarity (Sim)",
      color = "Year of Maximum Similarity"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate field labels for readability
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold")
    )
    }
  #also option: (but no CI)
  {
    library(ggplot2)
    
    # Create the scatter plot with correct x-axis range
    ggplot(peak_years, aes(x = YearOfMaxSim, y = field, color = Sim)) +
      geom_point(size = 4) +  # Scatter plot points
      geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.2, color = "black") +  # Horizontal error bars for CI
      scale_x_continuous(limits = c(2005, 2024), breaks = seq(2005, 2024, by = 2)) +  # Fix x-axis range
      scale_color_viridis_c(option = "C") +  # Color scale for similarity
      labs(
        title = "Peak Year of Similarity by Field",
        x = "Year of Maximum Similarity",
        y = "Field",
        color = "Similarity (Sim)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")
      )
    
    }
  }
  #Question 2:
  #Which Questions Get the Most Scientific Answers?
  {
  #trying to use slope:
  {
  #this take too long:
  request_slopes <- avg_slopes(mod_partial, variables = "request")
  
  #so I try this:
  request_effects <- slopes(mod_partial, variables = "request", newdata = sample_n(data_sample, 100000))
  
  # Sort by scientific accuracy
  request_slopes <- request_effects %>% arrange(desc(estimate))
  print(request_slopes)
  

  request_summary <- request_effects %>%
    group_by(request) %>%
    summarise(
      mean_effect = mean(estimate),
      conf_low = mean(conf.low),
      conf_high = mean(conf.high)
    )

  ggplot(request_summary, aes(x = reorder(request, mean_effect), y = mean_effect, fill = request)) +
    geom_col(show.legend = FALSE) +  
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2, color = "black") +  
    coord_flip() +  
    labs(
      title = "Which Question Types Get the Most Scientific Answers?",
      x = "Question Type",
      y = "Effect on Similarity"
    ) +
    theme_minimal()
  }
  
  #using the predictions like before:
  #Which persona Get the Most Scientific Answers?
  {
  agg_pred_data_request <- pred_data %>%
    group_by(request) %>%
    summarise(
      mean_sim_pred = mean(sim_pred), 
      conf_low = mean(conf_low),  # Approximate CI  | maybe use min?
      conf_high = mean(conf_high),  # Approximate CI | maybe use max?
      .groups = "drop")

  ggplot(agg_pred_data_request, aes(x = reorder(request, mean_sim_pred), y = mean_sim_pred, color = request)) +
    geom_point(size = 4) +  # Bar plot without legend
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2, color = "black") +  # Confidence intervals
    labs(
      title = "Predicted Similarity by Question Type",
      x = "Question Type",
      y = "Mean Predicted Similarity"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold")
    )
  }
  
  #Which persona Get the Most Scientific Answers?
  {
  agg_pred_data_persona <- pred_data %>%
    group_by(persona) %>%
    summarise(
      mean_sim_pred = mean(sim_pred), 
      conf_low = mean(conf_low),  # Approximate CI  | maybe use min?
      conf_high = mean(conf_high),  # Approximate CI | maybe use max?
      .groups = "drop")
  
  ggplot(agg_pred_data_persona, aes(x = reorder(persona, mean_sim_pred), y = mean_sim_pred, color = persona)) +
    geom_point(size = 4) +  # Bar plot without legend
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2, color = "black") +  # Confidence intervals
    labs(
      title = "Predicted Similarity by Persona Type",
      x = "Persone Type",
      y = "Mean Predicted Similarity"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold")
    )
  }
}
  
  
  
  
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











