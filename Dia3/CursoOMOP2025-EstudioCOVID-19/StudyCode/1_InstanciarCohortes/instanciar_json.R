info(logger, "  - JSON cohorts")
# Instanciar cohorts COVID-19 ----
covid_json <- readCohortSet(here("1_InstanciarCohortes", "Cohorts", "Covid"))
cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = covid_json,
                         name = covid_table_name,
                         overwrite = TRUE)

# Instanciar general condition cohorts ----
conditions_json <- readCohortSet(here("1_InstanciarCohortes", "Cohorts", "GeneralConditions"))
cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = conditions_json,
                         name = conditions_table_name,
                         overwrite = TRUE)


# Export cohort counts ----
counts <- cohortCount(cdm[[covid_table_name]]) %>%
  left_join(settings(cdm[[covid_table_name]]) %>%
              select("cohort_definition_id", "cohort_name")) %>% 
  mutate(group = "covid") %>%
  union_all(
    cohortCount(cdm[[conditions_table_name]]) %>%
      left_join(settings(cdm[[conditions_table_name]]) %>%
                  select("cohort_definition_id", "cohort_name")) %>%
      mutate(group = "conditions") 
  ) 

write_csv(
  counts, 
  file = here(output_folder, "cohort_count.csv")
)
