library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(plotOutput("plot1", height = 250),
                
                box(
                    title = "Controls,"
                    sliderInput("slider", "Number of observations:", 1, 100, 50)
                ))
        )
    )
)

server <- function(input, output) { }

shinyApp(ui, server)