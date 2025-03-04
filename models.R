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
articles$year <- factor(articles$year)

#bam model:
full_mod <- readRDS("full_mod.rds")
summary(full_mod)

#Question 0:
#What is the expected curve of similarity with real research?
{
#not working:  
year_field_emmeans <- emmeans::emmeans(full_mod, c("year", "field"),
                                       at = list(year = unique(articles$year)),
                                       nuisance = c("persona", "request")) |> as.data.frame()
    
# Create a prediction grid for all combinations of year, field, persona, and request
year_range <- range(articles$year)
field_levels <- unique(articles$field)
persona_levels <- unique(articles$persona)
request_levels <- unique(articles$request)
pred_data <- expand.grid(
  year = seq(from = year_range[1], to = year_range[2], by = 1),
  field = field_levels,
  persona = persona_levels,
  request = request_levels
)

predictions <- predictions(full_mod, newdata = pred_data, type = "response", vcov = TRUE) #confidence interval estimation only by year
pred_data$sim_pred <- predictions$estimate
pred_data$conf_low <- predictions$conf.low
pred_data$conf_high <- predictions$conf.high



#plot:
{
  library(viridis)
  agg_pred_data <- pred_data %>%
    group_by(year , field) %>%
    summarise(
      mean_sim_pred = mean(sim_pred), 
      conf_low = mean(conf_low),  # Approximate CI  | maybe use min?
      conf_high = mean(conf_high),  # Approximate CI | maybe use max?
      .groups = "drop")
  
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
}
#Question 1:
#For which fields are LLMs most up to date?
{
#scatter for the MaxSim:

peak_years <- agg_pred_data %>%
  group_by(field) %>%
  summarise(YearOfMaxSim = year[which.max(mean_sim_pred)],
            Sim = max(mean_sim_pred),
            conf_low = conf_low[which.max(mean_sim_pred)],  # Approximate CI  | maybe use min?
            conf_high = conf_high[which.max(mean_sim_pred)],  # Approximate CI | maybe use max?
            .groups = "drop")


#plot
ggplot(peak_years, aes(x = reorder(field, Sim), y = Sim, color = YearOfMaxSim)) +
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

#Question 2:
#Which Questions Get the Most Scientific Answers?
{
#using the predictions like before:
#Which request Get the Most Scientific Answers?
{
request_emmeans <- emmeans::emmeans(full_mod, c("request")) |> as.data.frame()

ggplot(request_emmeans, aes(x = reorder(request, emmean), y = emmean, color = request)) +
  geom_point(size = 4) +  # Bar plot without legend
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, color = "black") +  # Confidence intervals
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
persona_emmeans <- emmeans::emmeans(full_mod, c("persona")) |> as.data.frame()
  
ggplot(persona_emmeans, aes(x = reorder(persona, emmean), y = emmean, color = persona)) +
  geom_point(size = 4) +  # Bar plot without legend
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, color = "black") +  # Confidence intervals
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













