dataP <- read_csv("/Users/adamhughes/Documents/corona-bubble-in-R/boxApp/202004042339_my_people_cds_snapshot.csv")
## Only run examples in interactive R sessions
if (interactive()) {
  
  # basic example
  shinyApp(
    ui = fluidPage(
      selectInput("variable", "Variable:",
                  c("Death" = "deaths",
                    "Cases" = "cases",
                    "Tested" = "tested")),
      tableOutput("data")
    ),
    server = function(input, output) {
      output$data <- renderTable({
        dataP[, c("name", input$variable), drop = FALSE]
      }, rownames = TRUE)
    }
  )
  
  # # demoing group support in the `choices` arg
  # shinyApp(
  #   ui = fluidPage(
  #     selectInput("state", "Choose a state:",
  #                 list(`East Coast` = list("NY", "NJ", "CT"),
  #                      `West Coast` = list("WA", "OR", "CA"),
  #                      `Midwest` = list("MN", "WI", "IA"))
  #     ),
  #     textOutput("result")
  #   ),
  #   server = function(input, output) {
  #     output$result <- renderText({
  #       paste("You chose", input$state)
  #     })
  #   }
  # )
}
