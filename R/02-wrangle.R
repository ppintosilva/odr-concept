raw_data_anonymised <- read_rds("data/raw_data_anonymised.rds")

size_levels <- 1:4
size_labels <- c("0-10", "10-50", "50-250", "250+")

org_info <- tribble(
  ~id, ~sector, ~size,
  "A", "Finance", 3,
  "B", "Consumer electronics", 4,
  "C", "Energy provider", 4,
  "D", "Tech", 1,
  "E", "Banking", 4,
  "F", "Healthcare", 4,
  "G", "Housing", 4,
  "H", "Consumer electronics", 4,
  "I", "Energy storage", 2,
  "J", "Manufacturing", 4
) %>%
  mutate(size = factor(size, levels = size_levels, labels = size_labels)) %>%
  mutate(sector = factor(sector))

# Pivot from wide to long data and assign questions to each axis

data_pre_wrangled <-
  bind_rows(
    raw_data_anonymised %>%
      select(id:agility_notes) %>%
      pivot_longer(
        cols = c(storage:agility_notes),
        names_to = "question",
        values_to = "answer"
      ) %>%
      mutate(domain = "Agility"),
    raw_data_anonymised %>%
      select(id,instance,methodology:int_notes) %>%
      pivot_longer(
        cols = c(methodology:int_notes),
        names_to = "question",
        values_to = "answer"
      ) %>%
      mutate(domain = "Intelligence")
  ) %>%
  inner_join(
    org_info, by = "id"
  ) %>%
  mutate(id = factor(id))


# Some answers need to be tweaked manually (e.g. some NAs correspond to "missing"-like responses,
# resulting in a score of 0 instead of score of NA - which does not count towards the total)

data_wrangled <-
  data_pre_wrangled %>%
  mutate(answer = case_when(
    # Assign zero to healthcare interop questions instead of NA
    id == "F" & str_detect(question, "^interop_") ~ "Missing",

    # Adjust startup-tech org answers to match other assessments
    id == "D" & str_detect(question, "^interop_") ~ "Missing", # missing interop
    id == "D" & question == "readiness" ~ "No record/assessment",
    id == "D" & str_detect(question, "^doc_") ~ "Lacking",

    # Assign zero to "energy storage" NA intelligence scores
    id == "I" & question == "methodology" ~ "Skills barrier (know what they want but not how to do it)",
    id == "I" & domain == "Intelligence" &  is.na(answer) ~ "Missing",

    # Assign zero to "healthcare" in testing agility
    id == "F" & question == "testing" ~ "Lacking",

    TRUE ~ answer
))

write_rds(data_wrangled, "data/02-wrangled-data.rds")
