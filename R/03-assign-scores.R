data_wrangled <- write_rds("data/02-wrangled-data.rds")

# create a datapasta for manual annotation of weights

generate_annotation_table <- function() {
  data_wrangled %>%
    filter(!question %in% c("agility_notes", "int_notes")) %>%
    group_by(domain) %>%
    mutate(i = cur_group_id()) %>%
    group_by(domain, question) %>%
    mutate(j = cur_group_id()) %>%
    ungroup() %>%
    distinct(domain, question, answer, .keep_all = T) %>%
    arrange(domain, question, answer) %>%
    select(i,j,question,answer)  %>%
    mutate(score = 0.0) %>%
    filter(!is.na(answer)) %>%
    datapasta::tribble_paste()
}

# executed once (uncomment to generate the table below, then modify by hand)
# generate_annotation_table()


# columns manually modified: k and weight
# some answers might be missing because no one selected them: copy paste accordingly

answer_scores <-  tibble::tribble(
  ~i, ~j,               ~question,                                                                                ~answer, ~score,
  #
  # Agility
  #
  1L, 1L,          "interop_docs",                                                                              "Missing",      0/5,
  1L, 1L,          "interop_docs",                                                                              "Lacking",      2/5,
  1L, 1L,          "interop_docs",                                                                                   "OK",      5/5,
  #
  1L, 2L,        "interop_fields",                                                                              "Missing",      0/5,
  1L, 2L,        "interop_fields",                                                                              "Lacking",      2/5,
  1L, 2L,        "interop_fields",                                                                                   "OK",      5/5,
  #
  1L, 3L,         "interop_model",                                                                              "Missing",      0/5,
  1L, 3L,         "interop_model",                                                                              "Lacking",      2/5,
  1L, 3L,         "interop_model",                                                                                   "OK",      5/5,
  #
  1L, 4L,      "interop_reusable",                                                                              "Missing",      0/5,
  1L, 4L,      "interop_reusable",                                                                              "Lacking",      2/5,
  1L, 4L,      "interop_reusable",                                                                                   "OK",      5/5,
  #
  1L, 5L,              "metadata",                                                                        "Word of mouth",      0/5,
  1L, 5L,              "metadata",                                                                    "Word/Unstructured",      1/5,
  1L, 5L,              "metadata",                                      "Unstructured: (e.g. text file, webpage listing)",      2/5,
  1L, 5L,              "metadata",                                                                  "Possibly Structured",      3/5,
  1L, 5L,              "metadata",                                               "Structured but hard to consume via API",      4/5,
  1L, 5L,              "metadata",                                           "Data dictionary but no API + Word of Mouth",      4/5,
  1L, 5L,              "metadata",                                         "Structured (e.g. data dictionary, catalogue)",      5/5,
  #
  1L, 6L,             "readiness",                                                                 "No record/assessment",      0/8,
  1L, 6L,             "readiness",                                                            "Part of the project goals",      1/8,
  1L, 6L,             "readiness",                                                      "Concentrated in one/two persons",      2/8,
  1L, 6L,             "readiness",                                                               "Folder-based readiness",      2/8,
  1L, 6L,             "readiness",                                                                 "Within certain teams",      4/8,
  1L, 6L,             "readiness",                                                    "Whole organisation (but manually)",      6/8,
  1L, 6L,             "readiness",                                                        "Across the whole organisation",      8/8,
  #
  1L, 7L,         "skills_devops",                                                                                  "Low",      1/5,
  1L, 7L,         "skills_devops",                                                                                   "OK",      4/5,
  1L, 7L,         "skills_devops",                                                                                 "High",      5/5,
  #
  1L, 8L,             "skills_io",                                                                                 "Low",       1/5,
  1L, 8L,             "skills_io",                                                                                   "OK",      4/5,
  1L, 8L,             "skills_io",                                                                                 "High",      5/5,
  #
  1L, 9L,         "skills_transf",                                                                                  "Low",      1/5,
  1L, 9L,         "skills_transf",                                                                                   "OK",      4/5,
  1L, 9L,         "skills_transf",                                                                                 "High",      5/5,
  #
  1L, 10L,               "storage",                                                                             "Personal",      1/10,
  1L, 10L,               "storage",                                                           "Personal/Shared filesystem",      2/10,
  1L, 10L,               "storage",                                                                    "Shared filesystem",      3/10,
  1L, 10L,               "storage",                                                      "Silo-ed data (privacy concerns)",      4/10,
  1L, 10L,               "storage",                                                                     "Silo-ed database",      4/10,
  1L, 10L,               "storage",                                        "Data warehouse (still silo-ed to some degree)",      7/10,
  1L, 10L,               "storage",                                       "Data warehouse (mainly used for transactional)",      7/10,
  1L, 10L,               "storage",                                                                       "Data warehouse",      10/10,
  #
  1L, 11L,               "testing",                                            "Do not build software (hence do not test)",      NA,
  1L, 11L,               "testing",                                                                              "Testing",      NA,
  1L, 11L,               "testing",                                                                              "Lacking",      0/5,
  1L, 11L,               "testing",                        "No checking for expected data properties (amount of data/day)",      1/5,
  1L, 11L,               "testing",                                                                    "Integration tests",      4/5,
  #
  # Intelligence
  #
  2L, 13L,         "doc_decisions",                                                                              "Lacking",      0/5,
  2L, 12L,         "doc_decisions",                                                                              "Missing",      2/5,
  2L, 12L,         "doc_decisions",                                                                                   "OK",      5/5,

  2L, 13L,            "doc_errors",                                                                              "Lacking",      0/5,
  2L, 13L,            "doc_errors",                                                                              "Missing",      2/5,
  2L, 13L,            "doc_errors",                                                                                   "OK",      5/5,

  2L, 14L,           "doc_problem",                                                                              "Lacking",      0/5,
  2L, 14L,           "doc_problem",                                                                              "Missing",      2/5,
  2L, 14L,           "doc_problem",                                                                                   "OK",      5/5,

  2L, 15L,           "doc_results",                                                                              "Missing",      0/5,
  2L, 15L,           "doc_results",                                                                              "Lacking",      2/5,
  2L, 15L,           "doc_results",                                                                                   "OK",      5/5,

  2L, 16L,           "methodology",                                          "Dont know what a model is or what they want",      0/8,
  2L, 16L,           "methodology",                            "Skills barrier (know what they want but not how to do it)",      1/8,
  2L, 16L,           "methodology",                                      "Try to build complicated models from the get go",      2/8,
  2L, 16L,           "methodology",                                                              "Descriptive/Explorative",      3/8,
  2L, 16L,           "methodology",                     "Start with simple models and progress to more complicated models",      7/8,

  2L, 17L,       "skills_cleaning",                                                                              "Missing",      0/5,
  2L, 17L,       "skills_cleaning",                                                                              "Lacking",      2/5,
  2L, 17L,       "skills_cleaning",                                                                                   "OK",      5/5,

  2L, 18L,    "skills_exploratory",                                                                              "Missing",      0/5,
  2L, 18L,    "skills_exploratory",                                                                              "Lacking",      2/5,
  2L, 18L,    "skills_exploratory",                                                                                   "OK",      5/5,

  2L, 19L, "skills_interpretation",                                                                              "Missing",      0/5,
  2L, 19L, "skills_interpretation",                                                                              "Lacking",      2/5,
  2L, 19L, "skills_interpretation",                                                                                   "OK",      5/5,

  2L, 20L,             "skills_ml",                                                                              "Missing",      0/5,
  2L, 20L,             "skills_ml",                                                                              "Lacking",      2/5,
  2L, 20L,             "skills_ml",                                                                                   "OK",      5/5,

  2L, 21L,      "skills_modelling",                                                                              "Missing",      0/5,
  2L, 21L,      "skills_modelling",                                                                              "Lacking",      2/5,
  2L, 21L,      "skills_modelling",                                                                                   "OK",      5/5,

  2L, 22L,        "skills_visuals",                                                                              "Missing",      0/5,
  2L, 22L,        "skills_visuals",                                                                              "Lacking",      2/5,
  2L, 22L,        "skills_visuals",                                                                                   "OK",      5/5,

  2L, 23L,                 "tools",                                                                                "Excel",      1/6,
  2L, 23L,                 "tools",                                                                       "Excel/Power BI",      2/6,
  2L, 23L,                 "tools",                                                                              "Excel/R",      2/6,
  2L, 23L,                 "tools",                                                                     "Own software/AWS",      5/6,
  2L, 23L,                 "tools",                                                                             "R/Python",      5/6,

  2L, 24L,             "vailidity",                                                                         "Invalid data",      0/6,
  2L, 24L,             "vailidity", "Unaware of existing/potential errors (because they were not actively using the data)",      1/6,
  2L, 24L,             "vailidity",                                                                        "Too much data",      2/6,
  2L, 24L,             "vailidity",                                                                               "Middle",      3/6,
  2L, 24L,             "vailidity",                                                                 "Optimisation problem",      NA,
  2L, 24L,             "vailidity",                                 "The most relevant independent variables are measured",      5/6,
  2L, 24L,             "vailidity",                                                                            "Maps well",      6/6,
)

#

data_scores <-
  data_wrangled %>%
  group_by(domain) %>%
  mutate(i = cur_group_id()) %>%
  group_by(domain, question) %>%
  mutate(j = cur_group_id()) %>%
  left_join(
    answer_scores %>% select(question, answer, score),
    by = c("question", "answer")
  ) %>%
  select(id, instance, sector, size, i, domain, j, question, answer, score)

write_rds(data_scores, "data/03-data-scores.rds")
