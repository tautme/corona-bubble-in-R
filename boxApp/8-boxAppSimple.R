library(tidyverse)
library(shiny)

ui <- fluidPage("Hello Rona, How many are you?",
                plotOutput("myPeople")
)

server <- function(input, output) {
  
  output$myPeople <- renderPlot({
    dataP <- read_csv("/Users/adamhughes/Documents/corona-bubble-in-R/boxApp/202004042339_my_people_cds_snapshot.csv")
    ggplot(dataP, aes(x = name, y = cases)) +
      geom_point()
  })
  
  
}

shinyApp(ui = ui, server = server)
