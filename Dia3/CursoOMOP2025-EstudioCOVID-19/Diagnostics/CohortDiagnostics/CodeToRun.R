# ============================================================================ #
#                          INTRODUCTION TO OMOP CDM                            #
#                           IDIAP Jordi Gol, 2023                              #
# ============================================================================ #

# PACKAGES ----
# We will work with a specific version of CohortDiagnostics. This R project
# contains a file renv.lock which stores the packages needed with the required
# version. We can use the renv file to set up our environment by using the 
# package "renv":
# install.packages("renv") # if not already installed, install renv from CRAN
renv::activate()
renv::restore() # this should prompt you to install the various packages required for the study

# Restart R session and load packages:
.rs.restartR()

# Load packages
library(DatabaseConnector)
library(CohortDiagnostics) # git: remotes::install_github("darwin-eu/CohortDiagnostics")
library(CirceR) # git: remotes::install_github("OHDSI/CirceR")          
library(CohortGenerator) # git: remotes::install_github("OHDSI/CohortGenerator") 
library(here)
library(stringr)
library(tibble)
library(dplyr)

## CONNECT TO DATABASE (!! OHDSI way !!) ----
# Database connection details 
server <- Sys.getenv("DB_SERVER") # Server = host/server_dbi
user <- Sys.getenv("DB_USER_nmercade")
password <- Sys.getenv("DB_PASSWORD_nmercade")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

# sql dialect used with the OHDSI SqlRender package
targetDialect <-"postgresql"

# driver for DatabaseConnector
downloadJdbcDrivers(targetDialect, here()) # if you already have this you can omit and change pathToDriver below

# Create connection details
connectionDetails <- createConnectionDetails(dbms = targetDialect, 
                                             server = server, 
                                             user = user, 
                                             password = password, 
                                             port = port, 
                                             pathToDriver = here())

# schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "omop_test"
# schema that contains the vocabulariea
vocabulary_database_schema <- "omop_test"
# schema where a results table will be created 
results_database_schema <- "results_test"

# stem for tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
cohortTableStem <- "nmb_"


# GET COHORT DEFINITION SET -----
# list of json files
cohortJsonFiles <- list.files(here("cohorts"))[grepl(".json", list.files(here("cohorts")))]

# Create a table containing atlas id, cohort id, cohort name, json code, sql code,
# 
cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json <- here("cohorts",
                     cohortJsonFiles[i])
  cohortJson <- readChar(working.json, file.info(working.json)$size) # read json
  cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
  sql <- buildCohortQuery(cohortExpression, # sql code
                          options = CirceR::createGenerateOptions(generateStats = TRUE))
  
  cohortDefinitionSet[[i]]<-tibble(atlasId = 1,
                                   cohortId = i,
                                   cohortName = str_replace(cohortJsonFiles[i],".json",""),
                                   json = cohortJson,
                                   sql = sql,
                                   logicDescription = NA,
                                   generateStats = TRUE)
}
cohortDefinitionSet <- bind_rows(cohortDefinitionSet)
cohortDefinitionSet

# INSTANTIATE COHORTS (with CohortGenerator) ----
cohort_table_name <- paste0(cohortTableStem, "curs_omop")

# Names of tables to be created during study run
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohort_table_name)
cohortTableNames

# Create the tables in the database (empty)
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

# Generate the cohort set
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# get stats
# this will generate .csv files saved into the folder specified (cohortStatisticsFolder)
CohortGenerator::exportCohortStatsTables(
  connectionDetails = connectionDetails,
  connection = NULL,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  cohortStatisticsFolder = here("Results"),
  incremental = FALSE)


# COHORT DIGANOSTICS ----
# Tap into console ?executeDiagnostics to get information about the input arguments
executeDiagnostics(cohortDefinitionSet,
                   connectionDetails = connectionDetails,
                   cohortTable = cohort_table_name,
                   cohortDatabaseSchema = results_database_schema,
                   cdmDatabaseSchema = cdm_database_schema,
                   exportFolder = here("Results"), 
                   databaseId = "SIDIAP",
                   minCellCount = 5,
                   runInclusionStatistics = TRUE, #
                   runOrphanConcepts = TRUE,
                   runTimeDistributions = TRUE, #
                   runVisitContext = TRUE, #
                   runBreakdownIndexEvents = TRUE, 
                   runIncidenceRate = TRUE, 
                   runTimeSeries = TRUE, #
                   runCohortOverlap = TRUE, 
                   runCohortCharacterization = TRUE,
                   runTemporalCohortCharacterization = TRUE)

# drop cohort stats table
CohortGenerator::dropCohortStatsTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  connection = NULL)

# Review results -----
CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder = here::here("Results"))
