library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("W/N W/N Automated Weekly Sales Report"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3(strong("To run:")),
      tags$ul(
        tags$li("Export 5 week's of sales data from Square:"),
        tags$ul(
          tags$li("Click", strong("Sales"),"tab on left -> "),
          tags$li(strong("Transactions")," tab on the top right ->"), 
          tags$li("select date range ", 
                  strong("ENDING ON THE MONDAY AFTER THE LAST SUNDAY IN THE DESIRED REPORTING PERIOD"),
                  " and beginning at least 5 full weeks prior to that Monday ->"),
          tags$li("other options should be ", 
                  strong("'All Day'"), " & ", strong("'All Payment Methods'"), " ->"),
          tags$li("select ", strong("'Export'"), " & ", strong("'Item Detail CSV'"), " ->"),
          tags$li("save .csv file")
        ),
        tags$li("upload .csv file via button below"),
        tags$li("click each button to the right in sequence to produce table & plots 
                (must wait for ", strong("'Produce Table'")," to complete before clicking others)"),
        tags$li("tables & charts can be copy/pasted or dragged into google doc. 
                (use ", em("ctrl+shift+v")," or ", em("cmd+shift+v"), " to paste table without cells)")
      ),
      
      fileInput("file", label = h3("File input"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      
      # Copy the line below to make a file upload manager
      
      actionButton("tabGo", "Produce Table"),
      
      tableOutput("table"),
    
      
      actionButton("wpGo", "Produce Weekly Plot"),
      
      plotOutput("weeklyplot", width = "110%", height = "110%"),
      
      
      actionButton("dpGo", "Produce Day Plot"),
      
      plotOutput("dayplot", width = "90%"),
      
      
      actionButton("gpGo", "Produce Goal Plot"),
      
      plotOutput("goalplot", width = "90%"),
      
      
      actionButton("ipGo", "Produce Item Plots"),
      
      plotOutput("itemplots")
      
    
    )
  )
))