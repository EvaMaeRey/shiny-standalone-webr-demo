webr::install("tidyverse")


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$tzTable <- shiny::renderDataTable({
    
    lubridate::ymd_hms(input$time, 
            tz = input$tz) ->
      meeting
    
    
    time_to_local <- function(x, tz){
      lubridate::with_tz(x, tz = tz) |>
        as.character()
    }
    
    library(tidyverse)
    OlsonNames() %>% 
      tibble(tz = .) |>
      filter(tz != "US/Pacific-New") |>
      filter(tz %>% str_detect("Amsterdam|Adelaide|Melbourne|Stockholm|US/Eastern|British|Europe/London|US/Pacific|Mountain|US/Central|Sydney|Europe/Vienna")) |>
      mutate(meeting_ny = meeting) |>
      mutate(local_time = purrr::map2(meeting_ny, tz, time_to_local))  |>
      unnest(local_time) |>
      group_by(local_time) |>
      summarise(locations = paste(tz, collapse = "\n")) |> 
      mutate(day = as_date(local_time) %>% wday(label = T)) |>
      arrange(local_time) ->
      df; df  
    
    
    
  })
  
}