# outcome id
covid_dx      <- 1
covid_dx_test <- 2
covid_test    <- 3

# add strata to table
cdm[[denominator_table_name]] <- cdm[[denominator_table_name]] %>%
  addSex() %>%
  addAge(ageGroup = list(c(0,16), c(17,39), c(40,64), c(65,150)))

# survival - covid diagnostics
if (cohortCount(cdm$curs_omop_covid)$number_records[covid_dx] > 0) {
  survival_covid_dx <- estimateSingleEventSurvival(
    cdm = cdm,
    targetCohortTable = denominator_table_name,
    targetCohortId = 1,
    outcomeCohortTable = covid_table_name,
    outcomeCohortId = covid_dx,
    outcomeDateVariable = "cohort_start_date",
    censorOnCohortExit = TRUE,
    followUpDays = Inf,
    strata = list("sex", "age_group", c("sex", "age_group"))
  )
  # guardamos resultados
  survival <- survival_covid_dx %>%
    mutate(outcome = "covid19_diagnosis")
}

# survival - covid positive test
if (cohortCount(cdm$curs_omop_covid)$number_records[covid_test] > 0) {
  survival_covid_test <- estimateSingleEventSurvival(
    cdm = cdm,
    targetCohortTable = denominator_table_name,
    targetCohortId = 1,
    outcomeCohortTable = covid_table_name,
    outcomeCohortId = covid_test,
    outcomeDateVariable = "cohort_start_date",
    censorOnCohortExit = TRUE,
    followUpDays = Inf,
    strata = list("sex", "age_group", c("sex", "age_group"))
  )
  # guardamos resultados
  survival <- survival %>% 
    union_all(survival_covid_test %>%
                mutate(outcome = "covid19_positive_test"))
}


# survival - covid positive test
if (cohortCount(cdm$curs_omop_covid)$number_records[covid_dx_test] > 0) {
  survival_covid_dx_test <- estimateSingleEventSurvival(
    cdm = cdm,
    targetCohortTable = denominator_table_name,
    targetCohortId = 1,
    outcomeCohortTable = covid_table_name,
    outcomeCohortId = covid_dx_test,
    outcomeDateVariable = "cohort_start_date",
    censorOnCohortExit = TRUE,
    followUpDays = Inf,
    strata = list("sex", "age_group", c("sex", "age_group"))
  )
  # guardamos resultados
  survival <- survival %>% 
    union_all(survival_covid_dx_test %>%
                mutate(outcome = "covid19_diagnosis_positive_test"))
}

write_csv(
  survival, 
  file = here(output_folder, "survival.csv")
)
