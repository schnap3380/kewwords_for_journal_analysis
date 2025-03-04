library(mgcv)
library(marginaleffects)
source("load_data.R")

full_mod <- bam(
  sim ~ s(year, by = interaction(field, persona, request)) + field * persona * request,
  data = articles
  )
saveRDS(full_mod, "full_mod.rds")
full_mod <- readRDS("full_mod.rds")
summary(full_mod)

# slopes over time
full_mod_slopes <- slopes(
  full_mod,
  newdata = datagrid(
    field = unique(articles$field),
    persona = unique(articles$persona),
    request = unique(articles$request),
    year = seq(2005, 2024, by = .01)
  ),
  variables = "year"
) |> select(estimate:year)
saveRDS(full_mod_slopes, "full_mod_slopes.rds")