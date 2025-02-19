# Curso OMOP - Estudio COVID-19

## Executing diagnostics
1. Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can clone it using GitHub Desktop).
2. Open the folder "Diagnostics/CohortDiagnostics" and execute the script CodeToRun.R which will execute the diagnostics of the .json files in "cohorts".
3. When finished, copy the PreMerged.RData file to the subfolder "data" in CohortDiagnosticsShiny. Open the global.R script and execute the shiny.

## Running the analysis
1. Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can clone it using GitHub Desktop).
2. Open the folder StudyCode and the project StudyCode.Rproj in RStudio (when inside the project, you will see its name on the top-right of your RStudio session).
3. Open and work through the CodeToRun.R file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study (source(here("RunStudy.R")).
4. After running you should then have folder with your results.
5. To visualise the results in the shiny, open the project 5_ShinyApp.RProj inside the folder 5_ShinyApp. Copy the .csv files in the folder "data", and then execute the app.R script to interact with the shiny.

