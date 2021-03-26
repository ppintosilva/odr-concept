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
  "I", "Energy storage", 2
) %>%
  mutate(size = factor(size, levels = size_levels, labels = size_labels)) %>%
  mutate(sector = factor(sector))

data_wrangled <-
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

write_rds(data_wrangled, "data/02-wrangled-data.rds")
