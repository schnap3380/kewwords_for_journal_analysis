
# Visualize Curves (model-free) -------------------------------------------

source("load_data.R")

plot_curves_modelfree <- articles |> 
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
    )) |> 
  ggplot(aes(year, sim)) +
  geom_smooth(aes(color = field, fill = field)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # facet_grid(persona ~ request) +
  labs(x = "Year", y = "Similarity between Gemini answers and scientific literature.",
       color = "Field", fill = "Field", 
       title = "Gemini's Fidelity to Scientific Literature Varies by Field and Prompt",
       caption = "Error ribbons represent standard errors.") +
  theme_bw()

# Visualize Curves (model-based) ------------------------------------------

full_mod <- readRDS("full_mod.rds")

var_grid <- expand_grid(
  field = c("Business and Economics", "Engineering and Technology", "Environmental Studies", "General", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences and Humanities"),
  persona = c("executive", "lecturer", "phd", "teacher"),
  request = c("consensus", "ideas", "questions"),
  year = seq(2005, 2024, by = .05)
)

fit <- gratia::fitted_values(full_mod, data = var_grid) |> 
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

plot_curves_modelbased <- fit |> 
  ggplot(aes(year, .fitted, ymin = .lower_ci, ymax = .upper_ci)) +
    geom_ribbon(aes(fill = field), alpha = .2) +
    geom_line(aes(color = field)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    facet_grid(persona ~ request) +
    labs(x = "Year", y = "Similarity between Gemini answers and scientific literature.",
         color = "Field", fill = "Field", 
         title = "Gemini's Fidelity to Scientific Literature Varies by Field and Prompt",
         caption = "Error ribbons represent 95% confidence intervals.") +
    theme_bw()

# How Up to Date Are LLMs Across Fields? (Model-free) ---------------------

plot_peak_sim_modelfree <- articles |> 
  group_by(persona, request, field, year) |> 
  summarise(sim_ci = 1.96*sd(sim)/sqrt(n()), sim = mean(sim)) |> 
  group_by(persona, request, field) |> 
  slice_max(sim) |> ungroup() |> 
  filter(persona == "phd", request == "consensus") |>
  ggplot(aes(sim, xmin = sim-sim_ci, xmax = sim+sim_ci, fct_reorder(field, sim), color = year)) +
  geom_pointrange() +
  scale_color_gradient(low = "blue4", high = "green") +
  labs(
    title = "I am a PhD student who is interested in learning more about [field].\nWhat are some consensus conclusions in the field of [field]?",
    x = "Maximum similarity between Gemini answers and scientific literature",
    y = "",
    color = "Year of Maximum Similarity",
    caption = "Error bars represent 95% confidence intervals."
  ) +
  # facet_grid(persona ~ request) +
  theme_bw()

ggsave("figures/plot_peak_sim_modelfree.png", plot_peak_sim_modelfree, width = 8, height = 6)

plot_peak_year_modelfree <- articles |> 
  group_by(persona, request, field, year) |> 
  summarise(sim = mean(sim)) |> 
  group_by(persona, request, field) |> 
  slice_max(sim) |> ungroup() |> 
  filter(persona == "phd", request == "consensus") |>
  ggplot(aes(year, fct_reorder(field, year))) +
    geom_point(size = 2) +
    labs(
      title = "I am a PhD student who is interested in learning more about [field].\nWhat are some consensus conclusions in the field of [field]?",
      x = "Year of maximum similarity between Gemini answers and scientific literature",
      y = ""
    ) +
    # facet_grid(persona ~ request) +
    theme_bw()

ggsave("figures/plot_peak_year_modelfree.png", plot_peak_year_modelfree, width = 8, height = 6)

# How Up to Date Are LLMs Across Fields? (Model-based) --------------------

full_mod_slopes <- readRDS("full_mod_slopes.rds")

max_slopes <- full_mod_slopes |> 
  group_by(field, request, persona) |> 
  arrange(desc(year)) |> # start from most recent
  mutate(
    is_convex = lag(estimate) > estimate & lead(estimate) < estimate, # is curve convex?
    is_local_minmax = conf.low <= 0 & conf.high >= 0, # is slope not significantly different from 0?
    peak_id = rep.int(seq_len(length(rle(is_convex & is_local_minmax)$lengths)), rle(is_convex & is_local_minmax)$lengths) # before first peak = 1, first peak = 2, etc.
  ) |> 
  filter(is_convex, is_local_minmax, peak_id < 3) |> 
  mutate(
    year_high = max(year), # confidence interval on year
    year_low = min(year)
  ) |> 
  slice_max(predicted, with_ties = FALSE) # take only year of global maximum

plot_peak_sim_modelbased <- gratia::fitted_values(
  full_mod, 
  data = max_slopes |> filter(persona == "phd", request == "consensus")
  ) |> 
  ggplot(aes(.fitted, fct_reorder(field, .fitted), xmax = .upper_ci, xmin = .lower_ci, color = year)) +
    geom_pointrange() +
    scale_color_gradient(low = "blue4", high = "green") +
    labs(
      title = "I am a PhD student who is interested in learning more about [field].\nWhat are some consensus conclusions in the field of [field]?",
      x = "Maximum similarity between Gemini answers and scientific literature",
      y = "",
      color = "Year of Maximum Similarity",
      caption = "Error bars represent 95% confidence intervals."
    ) +
    theme_bw()

ggsave("figures/plot_peak_sim_modelbased.png", plot_peak_sim_modelbased, width = 8, height = 6)

plot_peak_year_modelbased <- max_slopes |> 
  filter(persona == "phd", request == "consensus") |> 
  ggplot(aes(year, fct_reorder(field, year), xmax = year_high, xmin = year_low)) +
  geom_pointrange() +
  labs(
    title = "I am a PhD student who is interested in learning more about [field].\nWhat are some consensus conclusions in the field of [field]?",
    x = "Year of maximum similarity between Gemini answers and scientific literature",
    y = "",
    caption = "Error bars represent dates at which the change in similarity was not significantly different from zero."
  ) +
  theme_bw()

ggsave("figures/plot_peak_year_modelbased.png", plot_peak_year_modelbased, width = 8, height = 6)


# Which Questions Get the Most Scientific Answers? ------------------------

request_slopes <- avg_slopes(full_mod, variables = "request")

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
