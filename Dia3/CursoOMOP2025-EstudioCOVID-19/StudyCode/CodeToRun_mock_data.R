# Paquets
library(DBI)
library(CDMConnector)
library(dplyr)
library(dbplyr)
library(tidyr)
library(here)
library(readr)
library(stringr)
library(log4r)
library(CirceR)
library(PatientProfiles)
library(CohortSurvival)
library(IncidencePrevalence)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "SYNTHEA"

# Database connection details -----
db <- DBI::dbConnect(duckdb::duckdb(), eunomiaDir("synthea-covid19-10k"))

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "main"

# The name of the schema where results tables will be created 
results_database_schema <- "main"

# Name of stem outcome table in the result schema where the outcome cohorts will
# be stored. 
# Notes: 
# - if there is an existing table in your results schema with the same names it
#   will be overwritten
# - more than one cohort will be created
# - name must be lower case
stem_table <- "curs_omop_"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema,
  cdm_name = db_name
)

# check database connection
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()


# Study parameters:
# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# Jobs to Run
executarInstanciarCohorts <- TRUE
executarCaracteritzacio   <- TRUE
executarIncidencia        <- TRUE
executarSupervivencia     <- TRUE

source(here("RunStudy.R"))

print("Done! Thanks for running the study :D")