data_scores <- read_rds("data/03-data-scores.rds")

#
# Some questions were answered with less confidence than others, and should be weighted accordingly.
#

data_scores %>%
  filter(question == "agility_notes")

data_scores %>%
  filter(question == "int_notes")

answers_tally <-
  data_scores %>%
  filter(!question %in% c("agility_notes", "int_notes"))  %>%
  group_by(question, answer) %>%
  summarise(
    count = n(),
    total_score = sum(score),
    .groups = "drop"
  )

# # uncomment to generate the table below, then modified by hand
# data_scores %>%
#   distinct(domain,question) %>%
#   mutate(confidence = 0.0) %>%
#   datapasta::tribble_paste()

# to assign confidence, we look at the answers tally for each question and notes taken during the
# interview
# weights are then computed using the softmax function

question_confidence <-
tibble::tribble(
         ~domain,               ~question, ~confidence,
       "Agility",               "storage",           1.00,
       "Agility",              "metadata",           0.90,
       "Agility",         "interop_model",           0.80,
       "Agility",        "interop_fields",           0.80,
       "Agility",          "interop_docs",           0.80,
       "Agility",      "interop_reusable",           0.80,
       "Agility",             "skills_io",           0.80,
       "Agility",         "skills_transf",           0.85,
       "Agility",         "skills_devops",           0.85,
       "Agility",               "testing",           0.70,
       "Agility",             "readiness",           0.70,
#
  "Intelligence",           "methodology",           0.90,
  "Intelligence",             "vailidity",           1.00,
  "Intelligence",                 "tools",           1.00,
  "Intelligence",           "doc_problem",           0.80,
  "Intelligence",            "doc_errors",           0.80,
  "Intelligence",         "doc_decisions",           0.80,
  "Intelligence",           "doc_results",           0.80,
  "Intelligence",       "skills_cleaning",           0.95,
  "Intelligence",    "skills_exploratory",           0.95,
  "Intelligence",      "skills_modelling",           0.95,
  "Intelligence",             "skills_ml",           0.95,
  "Intelligence", "skills_interpretation",           0.95,
  "Intelligence",        "skills_visuals",           0.95,
  )

softmax <- function(x) exp(x)/sum(exp(x), na.rm = T)


data_weights <-
  data_scores %>%
    left_join(
      question_confidence %>% select(question,confidence),
      by = "question"
  ) %>%
  # set confidence to NA if answer is NA
  mutate(confidence = ifelse(is.na(score), NA, confidence)) %>%
  group_by(id,instance,domain) %>%
  mutate(weight = softmax(confidence)) %>%
  mutate(weighted_score = score * weight)

# check that non-na scores have not been included in softmax computation
data_weights %>%
  filter(id == "B") %>%
  print(n=50)

# sanity check: is the sum of all weights, for non-NA scores, per org per domain, equal to 1?
data_weights %>%
  group_by(id,instance,domain) %>%
  filter(!is.na(score)) %>%
  summarise(sum_weights = sum(weight, na.rm = T), .groups = "drop") %>%
  pull(sum_weights) %>%
  purrr::map_lgl(.f = ~ .x == 1) %>%
  all() %>%
  assertthat::assert_that(msg = "Not true")

write_rds(data_weights, "data/04-data-scores-weights.rds")
