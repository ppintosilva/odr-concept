data_weights <- read_rds("data/04-data-scores-weights.rds")

total_scores <-
  data_weights %>%
  group_by(id,instance,domain) %>%
  summarise(
    score = sum(weighted_score, na.rm = T),
    .groups = "drop"
  ) %>%
  # average guesses for multiple assessments of the same organisation
  group_by(id,domain) %>%
  summarise(
    score = mean(score),
    .groups = "drop"
  ) %>%
  # Score of 0 is due to no assessment, so replace with NA
  mutate(score = na_if(score, 0))

write_rds(total_scores, "data/05-total-scores.rds")
