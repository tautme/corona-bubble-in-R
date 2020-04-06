library(tidyverse)
library(shiny)

ui <- fluidPage("Hello Rona, How many are you?",
                sliderInput(inputId = "buffer", 
                            label = "Choose Size of Box", 
                            value = 1, 
                            min = 0, 
                            max = 5),
                plotOutput("myPeople")
)

server <- function(input, output) {
  
  output$myPeople <- renderPlot({
    data <- read_csv("/Users/adamhughes/Documents/corona-bubble-in-R/boxApp/202004042339_my_people_cds_snapshot.csv")
    plot(data$deaths, data$cases, asp = input$buffer)
  })
  
  
}

shinyApp(ui = ui, server = server)
