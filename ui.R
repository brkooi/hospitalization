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
                   h3("Lag Hospital Admissions"), 
                   value = 5),
      
      selectInput("indepvar", label = h3("Explanatory variable 1"),
                  choices = list("Tested_positive" = "Tested_positive",
                                 "RNA_flow_per_measurement"="RNA_flow_per_measurement",
                                 "Coverage_primary_partly"="Coverage_primary_partly",
                                 "Coverage_primary_completed"="Coverage_primary_completed",
                                 "MA_perc_covid_symptoms"="MA_perc_covid_symptoms"), selected = 1),
      
      selectInput("indepvar2", label = h3("Explanatory variable 2"),
                  choices = list("Tested_positive" = "Tested_positive",
                                 "RNA_flow_per_measurement"="RNA_flow_per_measurement",
                                 "Coverage_primary_partly"="Coverage_primary_partly",
                                 "Coverage_primary_completed"="Coverage_primary_completed",
                                 "MA_perc_covid_symptoms"="MA_perc_covid_symptoms"), selected = 2),
      
      selectInput("indepvar3", label = h3("Explanatory variable 3"),
                  choices = list("Tested_positive" = "Tested_positive",
                                 "RNA_flow_per_measurement"="RNA_flow_per_measurement",
                                 "Coverage_primary_partly"="Coverage_primary_partly",
                                 "Coverage_primary_completed"="Coverage_primary_completed",
                                 "MA_perc_covid_symptoms"="MA_perc_covid_symptoms"), selected = 3),
      
      width = 2
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                  tabPanel("Summary selected data",verbatimTextOutput("summaryData")),
                  tabPanel("Scatterplot", plotOutput("scatterplot1"), plotOutput("scatterplot2"), plotOutput("scatterplot4")), # Plot
                  tabPanel("Distribution", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")),
                             column(6, plotOutput("distribution3")),
                             column(6, plotOutput("distribution4")))
                  ),
                  tabPanel("Variables - Timeserie", plotOutput("VarPlot1"), plotOutput("VarPlot2"),plotOutput("VarPlot3"),plotOutput("VarPlot4")),
                  tabPanel("Multiple Lineair Regression - Hospital Admissions", verbatimTextOutput("summaryMLR")),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Model Collinearity", verbatimTextOutput("modelVIF")),
                  
      ),
      width = 10
    )
  ))


