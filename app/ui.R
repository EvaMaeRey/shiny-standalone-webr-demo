# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("global clocks webr"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
      # 
      
      shiny::textInput(inputId = "time",
                         label = "Local Time",
                         value = "2024-03-12 13:00:00"),
      
      shiny::selectInput(inputId = "tz",
                         label = "Local Time Zone",
                         choices = OlsonNames(),
                         selected = "US/Mountain"
                         )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      # shiny::plotOutput(outputId = "distPlot"),
      shiny::dataTableOutput(outputId = "tzTable")
      
    )
  )
)