library(shiny)
library(ggplot2)
library(reactable)


datasets <- c("economics", "faithfuld", "seals")

ui <- fluidPage(
  reactableOutput("table")
)
server <- function(input, output, session) {
  output$table <- renderReactable(reactable(mtcars))
}

shinyApp(ui, server)
