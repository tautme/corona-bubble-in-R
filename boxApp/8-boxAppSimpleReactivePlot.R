library(tidyverse)
library(shiny)
dataP <- read_csv("/Users/adamhughes/Documents/corona-bubble-in-R/boxApp/202004042339_my_people_cds_snapshot.csv")
dataP$name

ui = fluidPage("<h2>Hello Rona, How many are you?</h2>",
    selectInput(inputId = "variable", 
                label = "Sort by:", 
                selected = "tested",
                choices = c("Death" = "deaths",
                            "Cases" = "cases",
                            "Tested" = "tested")),
    plotOutput("myPeople")
)

server = function(input, output) {
  output$myPeople <- renderPlot({
      plot(dataP$deaths, input$variable)
  })
  
}

shinyApp(ui = ui, server = server)
