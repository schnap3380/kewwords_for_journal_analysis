library(mgcv)

articles <- read_csv("data/articles_sims.csv.gz") |> 
  separate_wider_delim(condition, "_", names = c("persona", "request"))

model1 <- gamm(
  sim ~ s(year) + field*persona, 
  data= articles, 
  random = list(persona = ~1, field = ~1)
  )
