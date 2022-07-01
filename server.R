#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#

function(input, output, session) {
  
  # create the dataset based on the period-filter and lag-variable
  filtered_data <- reactive({
    Covid19$Tested_positive <- lag(Covid19$Tested_positive, input$vLag)
    Covid19$RNA_flow_per_measurement <- lag(Covid19$RNA_flow_per_measurement, input$vLag)
    Covid19$MA_perc_covid_symptoms <- lag(Covid19$MA_perc_covid_symptoms, input$vLag)
    Covid19$Coverage_primary_partly <- lag(Covid19$Coverage_primary_partly, input$vLag)
    Covid19$Coverage_primary_completed <- lag(Covid19$Coverage_primary_completed, input$vLag)
    Covid19 %>% filter(Date_of_statistics %in% seq(input$date_range[1], input$date_range[2], by = "day"))
  })
  
  
  # clean the dataset as input for the Random Forest-models
  inputModel <- reactive({
    
    dataModel <- subset(filtered_data(), select = -c(Date_of_statistics, IC_admission, Tested_with_result, RNA_flow_per_100000))

    dataModel <- as.data.frame(dataModel)
    
    dataModel <- dataModel %>% mutate_all(~ifelse(is.nan(.), 0, .))
    
  })
  
  
  # Random Forest-model based on all the predictors
  fit_all <-  reactive({
    set.seed(2345)
    fit_all <- randomForest(formula=Hospital_admission ~ ., data=inputModel(), mtry=input$vMtry, ntree=input$vNtree, importance=TRUE, na.action=na.omit)
    fit_all
  })
  
  
  # Random Forest-model based on the selected predictors
  fit_selected <- reactive({
    set.seed(1234)
    formula_selected <- paste("Hospital_admission", "~",input$predictor1,"+",input$predictor2,"+",input$predictor3)
    fit <- randomForest(formula=as.formula(formula_selected), data=inputModel(), mtry=input$vMtry, ntree=input$vNtree, na.action=na.omit)
    fit
  })
  
  # Determine the importance of the predictors
  output$varImportance <- renderPlot({
    varImpPlot(fit_all(), main = "Predictor importance" )
  }, height=480)
  
  # Summary data output
  output$summaryData <- renderPrint({
    summary(filtered_data())
  })
  
  # Display model based on selected variables
  output$summarySelected <- renderPrint({
    fit <- fit_selected()
    fit
  })
  
  # Display model based on all variables
  output$summaryAll <- renderPrint({
    fit <- fit_all()
    fit
  })
  
  
  output$plotSelected <- renderPlot({
    data <- filtered_data()
    data_model <- data.frame(Predicted = predict(fit_selected()), 
                           Actual = data$Hospital_admission)
    ggplot(data_model,                                     
           aes(x = Predicted,
               y = Actual)) +
      geom_point() +
      geom_abline(intercept = 0,
                  slope = 1,
                  color = "red",
                  size = 2) 
    
  })
  
  
  # Display the input for the model of the selected period
  output$tbl = DT::renderDataTable({
    DT::datatable(subset(filtered_data(), select = -c(IC_admission, Tested_with_result, RNA_flow_per_100000)), options = list(lengthChange = FALSE))
  })
  
  
  
  
  # Scatterplot output
  output$scatterplot1 <- renderPlotly({
    data <- filtered_data()
    predictor1 <- data[,input$predictor1]
    plot <- ggplot(data = data, aes(x = predictor1 , y = Hospital_admission)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color="red") +
      xlab(input$predictor1)
    ggplotly(plot)
  })
  
  output$scatterplot2 <- renderPlotly({
    data <- filtered_data()
    predictor2 <- data[,input$predictor2]
    plot <- ggplot(data = data, aes(x = predictor2 , y = Hospital_admission)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color="red") +
      xlab(input$predictor2)
    ggplotly(plot)
  })
  
  output$scatterplot3 <- renderPlotly({
    data <- filtered_data()
    predictor3 <- data[,input$predictor3]
    plot <- ggplot(data = data, aes(x = predictor3 , y = Hospital_admission)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color="red") +
      xlab(input$predictor3)
    ggplotly(plot)
  })
  
  # Timeseries plot Hospital admissions
  output$HospitalAdmissionsPlot <- renderPlotly({
    data <- filtered_data()
    plot <- ggplot(data, aes(x=Date_of_statistics, y=Hospital_admission)) + 
            geom_line(color = "blue") +
            ggtitle("Hospital admissions in time")
    ggplotly(plot)
  })
  
  # Timeseries plot predictor 1
  output$Predictor1Plot <- renderPlotly({
    data <- filtered_data()
    predictor1 <- filtered_data()[,input$predictor1]
    plot <-ggplot(data, aes(x=Date_of_statistics, y=predictor1)) + 
      geom_line(color = "red") +
      geom_smooth() +
      ylab(input$predictor1) +
      ggtitle(paste(input$predictor1,"in time"))
    ggplotly(plot)
  })
  
  
  # Timeseries plot predictor 2
  output$Predictor2Plot <- renderPlotly({
    data <- filtered_data()
    predictor2 <- filtered_data()[,input$predictor2]
    plot <- ggplot(data, aes(x=Date_of_statistics, y=predictor2)) + 
            geom_line(color = "red") +
            geom_smooth() +
            ylab(input$predictor2) +
            ggtitle(paste(input$predictor2,"in time"))
    ggplotly(plot)
  })
  
  # Timeseries plot predictor 3
  output$Predictor3Plot <- renderPlotly({
    data <- filtered_data()
    predictor3 <- filtered_data()[,input$predictor3]
    plot <- ggplot(data, aes(x=Date_of_statistics, y=predictor3)) + 
            geom_line(color = "red") +
            geom_smooth() +
            ylab(input$predictor3) +
            ggtitle(paste(input$predictor1,"in time"))
    ggplotly(plot)
  })
  

  # Prediction
  output$prediction <- renderPrint({
    model <- fit_selected()
    predictor1 <- as.numeric(input$valuePredictor1)
    predictor2 <- as.numeric(input$valuePredictor2)
    predictor3 <- as.numeric(input$valuePredictor3)
    inputdata <<- data.frame(predictor1,predictor2,predictor3)
    inputdata <- setNames(inputdata, c(input$predictor1,input$predictor2,input$predictor3))
    prediction <- predict(model, newdata=inputdata,type="response")
    prediction
  
  })
  
}