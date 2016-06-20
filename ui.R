library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("W/N W/N Weekly Sales Report"),
  
  # Sidebar 

    
    # Show a plot of the generated distribution
    mainPanel(
      #h1("Simulation"),
      h4("Automated Weekly Sales Report"),
      p("To run report:"),
      p(em("Export at least 4 week's of Square sales data by going to the Sales tab in the 
           left menu of the dashboard -> Transactions tab on the top right -> select a date 
           range *ENDING ON THE MONDAY AFTER THE LAST SUNDAY IN THE DESIRED REPORTING PERIOD* 
           and beginning at least 4 full weeks prior to that Monday -> other options should 
           be 'All Day' & 'All Payment Methods' -> select 'Export' & 'Item Detail CSV' -> 
           save .csv file -> upload file via box below -> click each button in sequence to 
           produce table & plots (must wait for each to complete before puching next one)")),
      
      
      # Copy the line below to make a file upload manager
      fileInput("file", label = h3("File input")),
      
      
      actionButton("tabGo", "Produce Table"),
      
      tableOutput("table"),
      
      
      actionButton("dpGo", "Produce Day Plot"),
      
      plotOutput("dayplot", width = "90%"),
      
      
      actionButton("wpGo", "Produce Weekly Plot"),
      
      plotOutput("weeklyplot", width = "110%", height = "110%"),
      
      
      actionButton("gpGo", "Produce Goal Plot"),
      
      plotOutput("goalplot", width = "90%"),
      
      
      actionButton("ipGo", "Produce Item Plots"),
      
      plotOutput("itemplots")
      

    
    )      
))