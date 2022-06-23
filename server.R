#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#test
#library(shiny)

function(input, output, session) {
  
  
  filtered_data <- reactive({
    Covid19$Tested_positive <- lag(Covid19$Tested_positive, input$vLag)
    Covid19$RNA_flow_per_measurement <- lag(Covid19$RNA_flow_per_measurement, input$vLag)
    Covid19$MA_perc_covid_symptoms <- lag(Covid19$MA_perc_covid_symptoms, input$vLag)
    Covid19$Coverage_primary_partly <- lag(Covid19$Coverage_primary_partly, input$vLag)
    Covid19$Coverage_primary_completed <- lag(Covid19$Coverage_primary_completed, input$vLag)
    Covid19 %>% filter(Date_of_statistics %in% seq(input$date_range[1], input$date_range[2], by = "day"))
  })
  
  inputModel <- reactive({
    
    dataModel <- subset(filtered_data(), select = -c(Date_of_statistics, IC_admission, Tested_with_result, RNA_flow_per_100000))
    dataModel <- scale(dataModel)
    
    dataModel <- as.data.frame(dataModel)
    
    dataModel <- dataModel %>% mutate_all(~ifelse(is.nan(.), 0, .))
    
  })
  
  fitRG <- reactive({
    set.seed(1234)
    data <- inputModel()
    fit <- lm(data$Hospital_admission ~ data[,input$indepvar] + data[,input$indepvar2] + data[,input$indepvar3])
    names(fit$coefficients) <- c("Intercept", input$indepvar, input$indepvar2, input$indepvar3)
    fit
  })
  
  
  
  varImportance <- reactive({
    as.data.frame(importance(fitRF()))
  })
  
  # Summary data output
  output$summaryData <- renderPrint({
    summary(filtered_data())
  })
  
  # Regression output
  output$summary <- renderPrint({
    summary(fitRG())
  })
  
  # MLRegression summary
  output$summaryMLR <- renderPrint({
    fitMLR <- lm(Hospital_admission ~ ., data=inputModel())
    summary(fitMLR)
  })
  
  # Model Collinearity
  output$modelVIF <- renderPrint({
    vif(fitRG())
  })
  
  
  
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot1 <- renderPlot({
    data <- filtered_data()
    plot(data[,input$indepvar], data$Hospital_admission, main=paste("Scatterplot", input$indepvar, "x Hospital_admission"),
         xlab=input$indepvar, ylab="Hospital_admission", pch=19)
    abline(lm(data$Hospital_admission ~ filtered_data()[,input$indepvar]), col="red")
    lines(lowess(filtered_data()[,input$indepvar],data$Hospital_admission), col="blue")
  }, height=400)
  
  output$scatterplot2 <- renderPlot({
    data <- filtered_data()
    plot(data[,input$indepvar2], data$Hospital_admission, main=paste("Scatterplot", input$indepvar2, "x Hospital_admission"),
         xlab=input$indepvar2, ylab="Hospital_admission", pch=19)
    abline(lm(data$Hospital_admission ~ filtered_data()[,input$indepvar2]), col="red")
    lines(lowess(filtered_data()[,input$indepvar2],data$Hospital_admission), col="blue")
  }, height=400)
  
  output$scatterplot3 <- renderPlot({
    data <- filtered_data()
    plot(filtered_data()[,input$indepvar3], data$Hospital_admission, main=paste("Scatterplot", input$indepvar3, "x Hospital_admission"),
         xlab=input$indepvar3, ylab="Hospital_admission", pch=19)
    abline(lm(data$Hospital_admission ~ filtered_data()[,input$indepvar3]), col="red")
    lines(lowess(filtered_data()[,input$indepvar3],data$Hospital_admission), col="blue")
  }, height=400)
  
  # VarPlot output
  output$VarPlot1 <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x=Date_of_statistics)) + 
      geom_line(aes(y=data$Hospital_admission,color = data$Hospital_admission)) +
      geom_smooth(aes(y=data$Hospital_admission,method='loess')) +
      xlab("")
  }, height=400)
  
  # VarPlot output
  output$VarPlot2 <- renderPlot({
    ggplot(filtered_data(), aes(x=Date_of_statistics)) + 
      geom_line(aes(y=filtered_data()[,input$indepvar],color = input$indepvar)) +
      geom_smooth(aes(y=filtered_data()[,input$indepvar],method='loess')) +
      xlab("")
  }, height=400)
  
  # VarPlot output
  output$VarPlot3 <- renderPlot({
    ggplot(filtered_data(), aes(x=Date_of_statistics)) + 
      geom_line(aes(y=filtered_data()[,input$indepvar2],color = input$indepvar2)) +
      geom_smooth(aes(y=filtered_data()[,input$indepvar2],method='loess')) +
      xlab("")
  }, height=400)
  
  # VarPlot output
  output$VarPlot4 <- renderPlot({
    ggplot(filtered_data(), aes(x=Date_of_statistics)) + 
      geom_line(aes(y=filtered_data()[,input$indepvar3],color = input$indepvar3)) +
      geom_smooth(aes(y=filtered_data()[,input$indepvar3],method='loess')) +
      xlab("")
  }, height=400)
  
  # VarPlot output
  output$scatterplot4 <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x=filtered_data()[,input$indepvar3])) + 
      geom_point(aes(y=data$Hospital_admission,color = data$Hospital_admission)) +
      geom_smooth(aes(y=data$Hospital_admission,method='lm')) +
      xlab("")
  }, height=400)
  
  
  
  # Histogram output outcome
  output$distribution1 <- renderPlot({
    data <- filtered_data()
    hist(data$Hospital_admission, main="Frequency Hospital Admissions", xlab="Hospital_admission")
  }, height=300, width=300)
  
  # Histogram output var 1
  output$distribution2 <- renderPlot({
    hist(filtered_data()[,input$indepvar], main=paste("Frequency ",input$indepvar), xlab=input$indepvar)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution3 <- renderPlot({
    hist(filtered_data()[,input$indepvar2], main=paste("Frequency ",input$indepvar2), xlab=input$indepvar2)
  }, height=300, width=300)
  
  # Histogram output var 3
  output$distribution4 <- renderPlot({
    hist(filtered_data()[,input$indepvar3], main=paste("Frequency ",input$indepvar3), xlab=input$indepvar3)
  }, height=300, width=300)
  
}