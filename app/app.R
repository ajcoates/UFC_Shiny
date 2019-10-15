#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
ufc_bind_top20 <- read.csv("ufc_bind_top20.csv")

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- pageWithSidebar(

    # Application title
    headerPanel("UFC Bettors' Reference"),
    
    sidebarPanel(
        
        # Input: Selector for variable to plot against mpg ----
        selectInput("variable", "Variable:", 
                    c("Win by" = "win_by",
                      "Stoppage or decision" = "stoppage_decision",
                      "Gears" = "gear")),
        
        # Input: Checkbox for whether outliers should be included ----
        checkboxInput("outliers", "Show outliers", TRUE)
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        # Output: Formatted text for caption ----
        h3(textOutput("caption")),
        
        # Output: Plot of the requested variable against mpg ----
        plotOutput("barPlot")
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    formulaText <- reactive({
        paste("Outcome ~", input$variable)
    })
    
    output$caption <- renderText({
        formulaText()
    })

        output$barPlot <- renderPlot({

            ggplot(
                data = ufc_bind_top20,
                aes(x = weight_class)) +
                geom_bar(aes(fill = win_by), position = 'fill') +
                labs(title='Weight class by win-type 100% STACK',
                     x='weight class',
                     y='distribution') +
                scale_fill_brewer(palette='Set1') +
                coord_flip() +
                theme_bw() +
                theme(legend.key=element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
