#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#test

#library(shiny)

fluidPage(
  titlePanel("Analyze Hospital Admissions in relation to COVID-19)"),
  sidebarLayout(
    sidebarPanel(
      
      # date selector goes here 
      dateRangeInput("Date range", inputId = "date_range",
                     start = min(Covid19$Date_of_statistics),
                     end = max(Covid19$Date_of_statistics),
                     format = "yyyy-mm-dd"),
      
      numericInput("vLag", 
                   "Lag Hospital Admissions", 
                   value = 5),
      
      numericInput("vMtry", 
                   "mtry-parameter Random Forest", 
                   value = 2),
      
      numericInput("vNtree", 
                   "ntree-parameter Random Forest", 
                   value = 500),
      
      selectInput("predictor1", label = "Predictor 1",
                  choices = list("Tested_positive" = "Tested_positive",
                                 "RNA_flow_per_measurement"="RNA_flow_per_measurement",
                                 "Coverage_primary_partly"="Coverage_primary_partly",
                                 "Coverage_primary_completed"="Coverage_primary_completed",
                                 "MA_perc_covid_symptoms"="MA_perc_covid_symptoms"), selected = 1),
      
      selectInput("predictor2", label = "Predictor 2",
                  choices = list("Tested_positive" = "Tested_positive",
                                 "RNA_flow_per_measurement"="RNA_flow_per_measurement",
                                 "Coverage_primary_partly"="Coverage_primary_partly",
                                 "Coverage_primary_completed"="Coverage_primary_completed",
                                 "MA_perc_covid_symptoms"="MA_perc_covid_symptoms"), selected = 2),
      
      selectInput("predictor3", label = "Predictor 3",
                  choices = list("Tested_positive" = "Tested_positive",
                                 "RNA_flow_per_measurement"="RNA_flow_per_measurement",
                                 "Coverage_primary_partly"="Coverage_primary_partly",
                                 "Coverage_primary_completed"="Coverage_primary_completed",
                                 "MA_perc_covid_symptoms"="MA_perc_covid_symptoms"), selected = 3),
      numericInput("valuePredictor1", 
                   "Value Predictor 1", 
                   value = 0),
      
      numericInput("valuePredictor2", 
                   "Value Predictor 2", 
                   value = 0),
      
      numericInput("valuePredictor3", 
                   "Value Predictor 3", 
                   value = 0),
      width = 2
      
    ),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                  tabPanel("Summary selected data",verbatimTextOutput("summaryData")),
                  tabPanel("Scatterplot", plotOutput("scatterplot1"), plotOutput("scatterplot2"), plotOutput("scatterplot3")), # Plot
                  tabPanel("Predictors - Timeserie", plotlyOutput("HospitalAdmissionsPlot"), plotlyOutput("Predictor1Plot"),plotlyOutput("Predictor2Plot"),plotlyOutput("Predictor3Plot")),
                  tabPanel("Random Forest - All Predictors", verbatimTextOutput("summaryAll")),
                  tabPanel("Predictor Importance", plotOutput("varImportance")),
                  tabPanel("Random Forest - Selected Predictors", verbatimTextOutput("summarySelected")),
                  tabPanel("Random Forest - Actual vs Predicted", plotOutput("plotSelected")),
                  tabPanel("Random Forest - Prediction",verbatimTextOutput("prediction"))
                  
                  
      ),
      width = 10
    )
  ))


