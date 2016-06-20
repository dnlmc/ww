library(shiny)

source("winwinWeeklySalesReportshiny.R")

shinyServer(function(input, output) {
  
    
    filedata <- reactive({
      infile <- input$file
      if (is.null(infile)){
        return(NULL)      
      }
      read.csv(infile$datapath)
    })
    
    report <- eventReactive(input$tabGo, {
      df <- filedata()
      if (is.null(df)) return(NULL)
      
      processfile(df)
      
    })
    
    output$table  <- renderTable({
      report()
      
    
    })
    
    
    observeEvent(input$dpGo, {
      output$dayplot  <- renderPlot({
        dayplot()
        
      })
    })
    
    observeEvent(input$wpGo, {
      output$weeklyplot  <- renderPlot({
        weeklyplot()
        
      }, width = 750, height = 450)
    })
    
    
    observeEvent(input$gpGo, {
      output$goalplot  <- renderPlot({
        goalplot()
        
      })
    })
    
    
    observeEvent(input$ipGo, {
      output$itemplots  <- renderPlot({
        itemplots()
        
      }, height = 1100, width = 680, res = 100)
    })
    
  }
)