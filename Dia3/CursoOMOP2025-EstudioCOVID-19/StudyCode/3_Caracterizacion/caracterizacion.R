cohort <- cdm[[denominator_table_name]] %>% 
  filter(cohort_definition_id == 1) %>% 
  # si han tenido COVID-19 o no
  addCohortIntersectFlag(
    targetCohortTable = covid_table_name,
    window = c(0, Inf),
    nameStyle = "{cohort_name}"
  )

# table one:
table_one <- summariseCharacteristics(
  cohort = cohort,
  ageGroup = list(c(0,16), c(17,39), c(40,64), c(65,150)),
  strata = list("covid19_diagnosis", 
                "covid19_positive_test", 
                "covid19_diagnosis_positive_test"),
  tableIntersect = list(
    "Number visits prior year" = list(
      tableName = "visit_occurrence", value = "count", window = c(-365, -1)
    )
  ),
  cohortIntersect = list(
    "Conditions any time prior" = list(
      targetCohortTable = conditions_table_name, value = "flag", window = c(-Inf, -1)
    )
  )
)

# Export
write_csv(table_one, here(output_folder, "table_one.csv"))


