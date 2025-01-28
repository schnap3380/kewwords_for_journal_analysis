library(mgcv)
library(tidyverse)



articles <- read_csv("data/articles_sims.csv.gz") |> 
  separate_wider_delim(condition, "_", names = c("persona", "request"))
articles <- na.omit(articles)

model1 <- gamm(
  sim ~ s(year) + field*persona, 
  data= articles, 
  random = list(persona = ~1, field = ~1)
  )

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



ggplot(data = articles , aes(year, sim, color = persona)) +
  geom_smooth() +
  facet_wrap(~request)

ggplot(data = articles , aes(year, sim, color = request)) +
  geom_smooth() +
  facet_wrap(~persona)

#gam model:
{
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

#0.01 working...


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


