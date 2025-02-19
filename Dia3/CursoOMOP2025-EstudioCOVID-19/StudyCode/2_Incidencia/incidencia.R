# Calculamos incidencia ----
incidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = denominator_table_name,
  outcomeTable = covid_table_name,
  interval = "months",
  outcomeWashout = 42,
  repeatedEvents = TRUE,
  minCellCount = minimum_counts
)

# Guardamos resultados incidencia
write_csv(incidence, file = here(output_folder, "incidence.csv"))


