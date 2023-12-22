ui <- fluidPage(
  
  # Application title
  titlePanel("When your heart has some rough edges..." ),
  
  titlePanel("... add vertices!" ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num_vertices",
                  label = "vertices",
                  step = 1,
                  min = 10,
                  max = 200,
                  value = 16
      ),
      selectInput(inputId = "char_color",
                  label = "color",
                  selected = "magenta",
                  choices = colors()
      ),
      selectInput(inputId = "char_fill",
                  label = "fill",
                  selected = "darkred",
                  choices = colors(),
      ),
      radioButtons(inputId = "char_linetype",
                   label = "linetype",
                   selected = "dashed",
                   choices = c("dashed", "dotted", "solid")),
      sliderInput(inputId = "num_alpha",
                  label = "alpha",
                  value = .8,
                  min = 0,
                  max = 1,
                  step = .02
      ),
      sliderInput(inputId = "num_linewidth",
                  label = "linewidth",
                  value = 4,
                  min = 1,
                  max = 5,
                  step = 1
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("distText"),
      plotOutput("distPlot")
    )
    
  )
  
  
  # titlePanel(x_mod)
  
  
)