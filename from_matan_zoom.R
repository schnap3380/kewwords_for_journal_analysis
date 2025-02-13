data_sample$persona <- factor(data_sample$persona)

mod <- bam(
  #sim ~ s(year, by = interaction(field, persona, request)) + field * persona * request,
  sim ~ s(year, by = persona) + persona,
  data = articles
)

library(marginaleffects)
# gratia
avg_slopes(mod, variables = "year", by = c("year", "persona"))
plot_slopes(mod, variables = "year", by = c("year", "persona")) + 
  geom_hline(yintercept = 0)



model1 <- gamm(
  sim ~ s(year) + field*persona, 
  data= articles, 
  random = list(persona = ~1, field = ~1)
)

model_gamm <- gamm(sim ~ s(year) + field * persona, 
                   random = list(persona = ~1, request = ~1 + year), 
                   data = articles)