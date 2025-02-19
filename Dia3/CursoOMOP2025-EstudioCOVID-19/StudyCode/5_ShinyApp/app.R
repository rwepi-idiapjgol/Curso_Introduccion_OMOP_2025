library(shiny)
library(shinydashboard)
library(here)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(scales)
library(tidyr)
library(stringr)
library(CohortSurvival)
library(visOmopResults)
library(PatientProfiles)
library(gt)


resultsFolder <- "data"

source(here("functions.R"))

elements <- readFiles(here::here(resultsFolder))
settings <- attr(elements, "settings")

denominatorAttrition <- getElementType(elements, "denominator_attrition") %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    excluded_records = dplyr::if_else(
      is.na(.data$excluded_records), "0", .data$excluded_records
    ),
    excluded_subjects = dplyr::if_else(
      is.na(.data$excluded_subjects), "0", .data$excluded_subjects
    )
  ) %>%
  dplyr::mutate(
    number_records = niceNum(.data$number_records),
    number_subjects = niceNum(.data$number_subjects),
    excluded_records = niceNum(.data$excluded_records),
    excluded_subjects = niceNum(.data$excluded_subjects)
  ) %>%
  dplyr::select(-cohort_name, - cohort_definition_id, -start_date, - end_date)

incidence_estimates <- getElementType(elements, "incidence_estimates") %>%
  bind_rows() %>%
  mutate(
    incidence_100000_pys = as.numeric(incidence_100000_pys), 
    incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
    incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper),
    incidence_start_date = as.Date(incidence_start_date, format = "%Y-%m-%d"),
    incidence_end_date = as.Date(incidence_end_date, format = "%Y-%m-%d")
  ) %>%
  select(
    "incidence_start_date", "incidence_end_date", "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
    "outcome_cohort_name", "denominator_age_group", "denominator_sex", 
    "n_persons", "n_events", "analysis_interval", "cdm_name"
  )

# cohort count
cohortCount <- getElementType(elements, "cohort_count") %>%
  dplyr::bind_rows()  %>%
  dplyr::mutate(
    number_records = niceNum(.data$number_records),
    number_subjects = niceNum(.data$number_subjects),
  ) 

# survival
survival_tab <- getElementType(elements, "survival") %>%
  bind_rows() 

survival_estimates <- survival_tab %>%
  filter(strata_level == "overall") %>%
  mutate(age_group = "0 to 150",
         sex = "Both") %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Female") %>%
      mutate(age_group = "0 to 150",
             sex = "Female")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Male") %>%
      mutate(age_group = "0 to 150",
             sex = "Male")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "0 to 16") %>%
      mutate(age_group = "0 to 16",
             sex = "Both")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "17 to 39") %>%
      mutate(age_group = "17 to 39",
             sex = "Both")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "40 to 64") %>%
      mutate(age_group = "40 to 64",
             sex = "Both")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "65 to 150") %>%
      mutate(age_group = "65 to 150",
             sex = "Both")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Female and 0 to 16") %>%
      mutate(age_group = "0 to 16",
             sex = "Female")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Female and 17 to 39") %>%
      mutate(age_group = "17 to 39",
             sex = "Female")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Female and 40 to 64") %>%
      mutate(age_group = "40 to 64",
             sex = "Female")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Female and 65 to 150") %>%
      mutate(age_group = "65 to 150",
             sex = "Female")
  )  %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Male and 0 to 16") %>%
      mutate(age_group = "0 to 16",
             sex = "Male")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Male and 17 to 39") %>%
      mutate(age_group = "17 to 39",
             sex = "Male")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Male and 40 to 64") %>%
      mutate(age_group = "40 to 64",
             sex = "Male")
  ) %>%
  union_all(
    survival_tab %>%
      filter(strata_level == "Male and 65 to 150") %>%
      mutate(age_group = "65 to 150",
             sex = "Male")
  ) |>
  splitAdditional() |>
  mutate(time = as.numeric(time))

# table one
table_one <- getElementType(elements, "table_characteristics") %>%
  dplyr::bind_rows() %>% 
  filter(strata_level == "overall" | strata_level == 1)

# ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Curs OMOP - Estudi COVID-19"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Databases details", tabName = "database_details",
        menuSubItem("CDM snapshot", tabName = "cdm_snapshot")
      ),
      menuItem(
        "Cohort details", tabName = "cohort_details",
        menuSubItem("Cohort counts", tabName = "cohort_counts"),
        menuSubItem("Denominator attrition", tabName = "den_attrition")
      ),
      menuItem(
        "Results", tabName = "characterization",
        menuSubItem("Table characteristics", tabName = "table_characteristics"),
        menuSubItem("Incidence estimates", tabName = "incidence_estimates"),
        menuSubItem("Survival", tabName = "kaplan_meier")
      )
    )
  ),
  ## body ----
  dashboardBody(
    tabItems(
      ### cdm_snapshot ----
      tabItem(
        tabName = "cdm_snapshot", 
        h3("Details of the databases that participated in the study"),
        p("Identifier 'cdm_name' is the one used in the multiple selection panels of the shiny"),
        DTOutput("cdm_snapshot")
      ),
      ### cohort_count ----
      tabItem(
        tabName = "cohort_counts", 
        h3("Counts for the cohorts instantiated in the study"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cohort_count_cdm_name",
            label = "Database",
            choices = unique(cohortCount$group),
            selected = unique(cohortCount$group)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        DTOutput("cohort_count")
      ),
      ### cohort_attrition -----
      tabItem(
        tabName = "den_attrition",
        h3("Attrition for the different cohorts in the study"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_age_group",
            label = "Age group",
            choices = unique(denominatorAttrition$age_group),
            selected = unique(denominatorAttrition$age_group)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_sex",
            label = "Sex",
            choices = unique(denominatorAttrition$sex),
            selected = unique(denominatorAttrition$sex)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
        )
      ),
      DTOutput("cohort_attrition")
    ),
    ### incidence_estimates ----
    tabItem(
      tabName = "incidence_estimates", 
      h3("Incidence estimates and plots"),
      hr(),
      h3("Outcome"),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "incidence_estimates_outcome_cohort_name",
          label = "Outcome",
          choices = unique(incidence_estimates$outcome_cohort_name),
          selected = unique(incidence_estimates$outcome_cohort_name)[1],
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
        ),
        h3("Denominator population"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_denominator_age_group",
            label = "Age group",
            choices = unique(incidence_estimates$denominator_age_group),
            selected = "0 to 150",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_denominator_sex",
            label = "Sex",
            choices = unique(incidence_estimates$denominator_sex),
            selected = "Both",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        hr(),
        h3("Dates estimates"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_incidence_start_date",
            label = "Incidence start date",
            choices = as.character(
              unique(
                sort(
                  incidence_estimates$incidence_start_date))),
            selected = as.character(min(incidence_estimates$incidence_start_date)),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_incidence_end_date",
            label = "Incidence end date",
            choices = as.character(
              unique(
                sort(
                  incidence_estimates$incidence_end_date))),
            selected = as.character(max(incidence_estimates$incidence_end_date)),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table of estimates", 
            DTOutput('tbl_incidence_estimates') %>% withSpinner()
          ), 
          tabPanel(
            "Plot of estimates",
            tags$h5("Plotting options"),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "incidence_plot_facet",
                label = "Facet by",
                choices = c(
                  "outcome_cohort_name", 
                  "denominator_age_group",
                  "denominator_sex"
                ),
                selected = c("outcome_cohort_name"),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              )
            ),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "incidence_plot_group",
                label = "Colour by",
                choices = c(
                  "outcome_cohort_name", 
                  "denominator_age_group",
                  "denominator_sex"
                ),
                selected="outcome_cohort_name",
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              )
            ),
            plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner() 
          )
        )
      ),
    ### table_one ----
    tabItem(
      tabName = "table_characteristics", 
      h3("Baseline characteristics of the study patients."),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "table_one_covid",
          label = "Outcome",
          choices = table_one$strata_name |> unique(),
          selected = "overall",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = FALSE
        )
      ),
      gt_output('table_one') %>% withSpinner()
    ),
    
    ### survival ----
    tabItem(
      tabName = "kaplan_meier", 
      h3("Survival plots"),
      hr(),
      h3("Outcome"),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "survival_estimates_outcome_cohort_name",
          label = "Outcome",
          choices = c("covid19_diagnosis", "covid19_positive_test", "covid19_diagnosis_positive_test"),
          selected = "covid19_diagnosis",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
      ),
      h3("Denominator population"),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "survival_estimates_denominator_age_group",
          label = "Age group",
          choices = unique(survival_estimates$age_group),
          selected = "0 to 150",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "survival_estimates_denominator_sex",
          label = "Sex",
          choices = unique(survival_estimates$sex),
          selected = "Both",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "survival_estimates_incidence_start_date",
          label = "Survival time",
          choices = as.character(
            sort(
              unique(
                survival_estimates$time))),
          selected = as.character(unique(max(survival_estimates$time, na.rm = TRUE))),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = FALSE
        )
      ),
      div(
        style="display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "survival_plot_facet",
          label = "Facet by",
          choices = c(
            "outcome", 
            "age_group",
            "sex"
          ),
          selected = c("outcome"),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
      ),
      div(
        style="display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "survival_plot_group",
          label = "Colour by",
          choices = c(
            "outcome", 
            "age_group",
            "sex"
          ),
          selected = "outcome",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
      ),
      plotlyOutput('plot_survival', height = "800px") %>% withSpinner()
    )
  )
  ),
  ## parameters ----
  "Curs OMOP - Case example study: COVID-19 in Catalonia"
)

# server ----
server <- function(input, output, session) {
  output$cdm_snapshot <- renderDataTable({
    DT::datatable(
      getElementType(elements, "cdm_snapshot") %>%
        dplyr::bind_rows() %>%
        select(
          cdm_name, cdm_description, cdm_version, cdm_holder, cdm_release_date, vocabulary_version,
          person_count, earliest_observation_period_start_date, latest_observation_period_end_date
        ), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$cohort_count <- renderDataTable({
    DT::datatable(
      cohortCount %>%
        filter(.data$group %in% input$cohort_count_cdm_name), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$cohort_attrition <- renderDataTable({
    DT::datatable(
      denominatorAttrition %>%
        filter(.data$age_group == input$denominator_attrition_age_group) %>%
        filter(.data$sex == input$denominator_attrition_sex) %>%
        select(reason_id, reason, number_records, number_subjects),
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })

  # incidence ----
  output$tbl_incidence_estimates <- renderDataTable({
    DT::datatable(
      displayIncidence(
        incidence = incidence_estimates, 
        ageGroup = input$incidence_estimates_denominator_age_group,
        sex = input$incidence_estimates_denominator_sex,
        incidenceStartDate = input$incidence_estimates_incidence_start_date,
        incidenceEndDate = input$incidence_estimates_incidence_end_date,
        outcome = input$incidence_estimates_outcome_cohort_name
      ), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$plot_incidence_estimates <- renderPlotly({ 
    
    table <- incidence_estimates %>%
      dplyr::filter(.data$denominator_age_group %in% input$incidence_estimates_denominator_age_group) %>%
      dplyr::filter(.data$denominator_sex %in% input$incidence_estimates_denominator_sex) %>%
      dplyr::filter(.data$incidence_start_date >= as.Date(input$incidence_estimates_incidence_start_date)) %>%
      dplyr::filter(.data$incidence_end_date <= as.Date(input$incidence_estimates_incidence_end_date)) %>%
      dplyr::filter(.data$outcome_cohort_name %in% input$incidence_estimates_outcome_cohort_name) %>%
      dplyr::mutate(
        incidence_100000_pys = as.numeric(incidence_100000_pys),
        incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
        incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper)
      )
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_line()+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),nrow = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          geom_line()+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group) ){ 
      
      if(is.null(input$incidence_plot_facet) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_line()+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            geom_line()+
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  # table one -----
  output$table_one <- render_gt({
    table_one  %>%
      filter(strata_name == input$table_one_covid) %>% 
      tableCharacteristics(
        header = character(),
        split = character(),
        excludeColumns = c("result_id", "result_type", "package_name", "package_version",
                           "estimate_type", "additional_name", "additional_level", 
                           "group_name", "group_level", "strata_name", "strata_level")) %>% 
    tab_options(column_labels.padding = px(5)) 
  })
  # survival ----
  output$plot_survival <- renderPlotly({ 
    
    table <- survival_estimates %>%
      dplyr::filter(.data$age_group %in% input$survival_estimates_denominator_age_group) %>%
      dplyr::filter(.data$sex %in% input$survival_estimates_denominator_sex) %>%
      dplyr::filter(.data$time <= input$survival_estimates_incidence_start_date) %>%
      dplyr::filter(.data$variable_level %in% input$survival_estimates_outcome_cohort_name) |>
      visOmopResults::uniteAdditional(cols = c("time", "analysis_type", "outcome","eventgap"  )) |>
      select(-age_group, -sex)
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$survival_plot_group)){
      if(!is.null(input$survival_plot_facet)){
        p<-table %>% 
          # unite("facet_var", 
          #       c(all_of(input$survival_plot_facet)), remove = FALSE, sep = "; ") %>% 
          plotSurvival(facet = c(input$survival_plot_facet)) +
          scale_y_continuous(
            limits = c(NA, 1)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          plotSurvival() +
          scale_y_continuous(
            limits = c(0, NA)
          )        
      }
    } 
    
    
    if(!is.null(input$survival_plot_group) ){ 
      
      if(is.null(input$survival_plot_facet) ){ 
        p<-table %>% 
          # unite("Group", 
          #       c(all_of(input$survival_plot_group)), remove = FALSE, sep = "; ") %>% 
          plotSurvival(colour = c(input$survival_plot_group)) +
          scale_y_continuous(
            limits = c(0, NA)
          ) 
      }
      
      if(!is.null(input$survival_plot_facet) ){
        if(!is.null(input$survival_plot_group) ){ 
          p<-table %>% 
            # unite("Group", 
            #       c(all_of(input$survival_plot_group)), remove = FALSE, sep = "; ") %>% 
            # unite("facet_var", 
            #       c(all_of(input$survival_plot_facet)), remove = FALSE, sep = "; ") %>% 
            plotSurvival(facet = c(input$survival_plot_facet), colour = c(input$survival_plot_group)) +  
            scale_y_continuous(
              limits = c(0, NA)
            )  
        }
      }
      
    }
    
    p
    
  })
}

# app ----
shinyApp(ui, server)
