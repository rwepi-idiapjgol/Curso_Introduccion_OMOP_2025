info(logger, "  - Denominator cohort")
# General denominator ----
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = denominator_table_name,
  cohortDateRange = c(as.Date("2020-03-01"), as.Date("2023-06-30")),
  ageGroup = list(c(0,150), c(0,16), c(17,39), c(40,64), c(65,150)), 
  sex = c("Both", "Female", "Male"),
  daysPriorObservation  = 365
)

# Guardamos attrition
cdm[[denominator_table_name]] %>%
  settings() %>%
  inner_join(cdm[[denominator_table_name]] %>%
               attrition(),
             by = "cohort_definition_id") %>%
  write_csv(file = here(output_folder, "denominator_attrition.csv"))
