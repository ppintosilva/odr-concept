col_names <- c(
  # cover
  "t",
  "interviewee",
  "organisation",
  "sector",
  "size",
  # agility
  "storage",
  "metadata",
  "interop_model",
  "interop_fields",
  "interop_docs",
  "interop_reusable",
  "skills_io",
  "skills_transf",
  "skills_devops",
  "testing",
  "readiness",
  "agility_notes",
  # intelligence
  "methodology",
  "vailidity",
  "tools",
  "doc_problem",
  "doc_errors",
  "doc_decisions",
  "doc_results",
  "skills_cleaning",
  "skills_exploratory",
  "skills_modelling",
  "skills_ml",
  "skills_interpretation",
  "skills_visuals",
  "int_notes"
)

raw_data <- read_csv(
  file = "data/00-raw-data.csv",
  col_names = col_names,
  col_types = cols(
    .default = col_character()
  ),
  skip = 1
)

anonymisation_table <-
  read_csv(
    file = "data/00-anonymisation-table.csv",
    col_types = cols(
      .default = col_character()
    )
  )

raw_data_anonymised <-
  raw_data %>%
  inner_join(anonymisation_table, by = "organisation") %>%
  select(-c(t,interviewee,organisation,sector,size)) %>%
  select(id, instance, everything())

write_rds(raw_data_anonymised, "data/01-raw-data-anonymised.rds")
