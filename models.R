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


#subject mapping : (maybe find a better way):
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


articles$persona <- factor(articles$persona)
articles$request <- factor(articles$request)
articles$Subject <- factor(articles$Subject)
articles$field <- factor(articles$field)


#We will use bam model:
{
  data_sample <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]
  
  mod <- bam(
    #sim ~ s(year, by = interaction(Subject, persona, request)) + Subject * persona * request,
    #sim ~ s(year, by = interaction(field, persona, request)) + field * persona * request,
    #sim ~ s(year, by = persona) + persona,
    sim ~ s(year, by = field) + field,
    data = data_sample
  )
  
  
  summary(mod)
  
  
  #instead of using it:
  library(marginaleffects)
  # gratia
  avg_slopes(mod, variables = "year", by = c("year", "field"))
  plot_slopes(mod, variables = "year", by = c("year", "field")) + 
    geom_hline(yintercept = 0)
  
  #I used it:
  
  #predictions:
  year_range <- range(data_sample$year)
  field_levels <- unique(data_sample$field)
  pred_data <- expand.grid(year = seq(from = year_range[1], to = year_range[2], by = 1),
                           field = field_levels)
  pred_data$sim_pred <- predict(mod, newdata = pred_data, type = "response")
  
  # Find the year of the maximum sim for each field
  peak_years <- pred_data %>%
    group_by(field) %>%
    summarise(YearOfMaxSim = year[which.max(sim_pred)],
              MaxSim = max(sim_pred))
  print(peak_years)
  
  ggplot(pred_data, aes(x = year, y = sim_pred, group = field, color = field)) +
    geom_line() +  # Draw lines
    labs(title = "Predicted SIM by Year for Each Field",
         x = "Year",
         y = "Predicted SIM",
         color = "Field") +
    #facet_wrap(~field, scales = "free_y") +  # Create a separate plot for each field
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
}  



#gam model:
{
  
data_sample <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]
mod <- bam(
  #sim ~ s(year, by = interaction(Subject, persona, request)) + Subject * persona * request,
  
  #sim ~ s(year, by = persona) + persona,
  data = data_sample
)

summary(mod)
plot(mod,pages=1,rug=FALSE)
plot(mod,pages=1,rug=FALSE,seWithMean=TRUE)


library(marginaleffects)
# gratia
avg_slopes(mod, variables = "year", by = c("year", "persona"))
plot_slopes(mod, variables = "year", by = c("year", "persona")) + 
  geom_hline(yintercept = 0)



}

#gam model:
{

model1 <- gamm(
  sim ~ s(year) + field*persona, 
  data= articles, 
  random = list(persona = ~1, field = ~1)
)

ggplot(data = articles , aes(year, sim, color = persona)) +
  geom_smooth() +
  facet_wrap(~request)

ggplot(data = articles , aes(year, sim, color = request)) +
  geom_smooth() +
  facet_wrap(~persona)


#the model that we want:
model_gamm <- gamm(sim ~ s(year) + field * persona, 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = articles)

#but first i try this one and got error:
model_gamm <- gamm(sim ~ s(year), 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = articles)
#Error: cannot allocate vector of size 4261.1 Gb

#so i try to use smaller data:

data_sample <-  articles[sample(nrow(articles), size = floor(0.001 * nrow(articles))), ]

model_gamm <- gamm(sim ~ s(year) + field * persona, 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = data_sample)

#Error in MEestimate(lmeSt, grps) :   
#Singularity in backsolve at level 0, block 1

model_gamm <- gamm(sim ~ s(year), 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = data_sample)
summary(model_gamm)




data_sample <-  articles[sample(nrow(articles), size = floor(0.1 * nrow(articles))), ]

model_gamm <- gamm(sim ~ s(year), 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = data_sample)
#Error: cannot allocate vector of size 42.4 Gb

#0.01 working:
data_sample <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]

model_gamm <- gamm(sim ~ s(year), 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = data_sample)


summary(model_gamm$gam)
plot(model_gamm$gam, select = 1)

plot(ggeffects::ggpredict(model_gamm) , facet = TRUE)

library('stats')
predictions <-  predict.gam(model_gamm , data_sample)







summary(model_gamm4$gam)

vis.gam(model_gamm$gam)


prediction <- predict(model_gamm$gam , data_sample)

data_sample$pred <- prediction

ggplot(data = data_sample , aes(year, pred, color = persona)) +
  geom_smooth() +
  facet_wrap(~request)


}
#polynomial:
{
library(lme4)
data_sample <-  articles[sample(nrow(articles), size = floor(0.01 * nrow(articles))), ]

poly_model <- lmer(sim ~ poly(year , degree = 4) + (1 | persona) + (1 + poly(year , degree = 4) | request)
                 , data = data_sample)

summary(poly_model)

prediction <- poly_model %>% predict(data_sample)

data_sample$pred <- prediction

ggplot(data = data_sample , aes(year, pred, color = persona)) +
  geom_smooth() +
  facet_wrap(~request)

ggplot(data = data_sample , aes(year, pred, color = request)) +
  geom_smooth() +
  facet_wrap(~persona)

}

#another option is gamm4:
{
install.packages('gamm4')
library(gamm4)

model_gamm4 <- gamm4(sim ~ s(year) + field * persona, 
                     random = ~(1 | persona) + (1 + year | request), 
                     data = data_sample)



model_gamm4 <- gamm4(sim ~ s(year), 
                     random = ~(1 | persona) + (1 + year | request), 
                     data = data_sample)

}


