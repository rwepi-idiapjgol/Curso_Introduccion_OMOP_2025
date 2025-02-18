# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here(paste0("Results_", db_name))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# create logger ----
log_file <- here(output_folder, "log.txt")
if (file.exists(log_file)) {
  unlink(log_file)
}

logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0 INITIAL SETTINGS ----")

# Variables ----
# Names for cohort tables
covid_table_name       <- paste0(stem_table, "covid")
conditions_table_name  <- paste0(stem_table, "conditions")
denominator_table_name <- paste0(stem_table, "denominator")


# Snapshot base de dades ----
write_csv(snapshot(cdm), here(output_folder, "cdm_snapshot.csv"))

# Inicia operacions a executar
if (executarInstanciarCohorts) {
  info(logger, "STEP 1 INSTANTIATE COHORTS ----")
  source(here("1_InstanciarCohortes", "instanciar_json.R"))
  source(here("1_InstanciarCohortes", "instanciar_denominador.R"))
  
} else {
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = cdm_database_schema,
    write_schema = results_database_schema,
    cohort_tables = c(covid_table_name, conditions_table_name, denominator_table_name)
  )
}

if (executarIncidencia) {
  info(logger, "STEP 2 CALCULATE INCIDENCE ----")
  source(here("2_Incidencia", "incidencia.R"))
}

if (executarCaracteritzacio) {
  info(logger, "STEP 3 CHARACTERISATION ----")
  source(here("3_Caracterizacion", "caracterizacion.R"))
}

if (executarSupervivencia) {
  info(logger, "STEP 4 SURVIVAL ----")
  source(here("4_Supervivencia", "survival.R"))
}

info(logger, "STEP 5 ZIP RESULTS ----")
output_folder <- basename(output_folder)
zip(
  zipfile = paste0(output_folder, ".zip"),
  files = list.files(output_folder, full.names = TRUE)
)

info(logger, " -- DONE! --")