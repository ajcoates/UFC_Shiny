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
        selectInput("variable", "Result type: ", 
                    c("Win by" = "win_by",
                      "Stoppage or decision" = "stoppage_decision")),
        
        checkboxGroupInput("refnames", "Select Referees: ",
                           unique(ufc_bind_top20$referee),
        )
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        # # Output: Formatted text for caption ----
        # h3(textOutput("caption")),
        # 
        # # barplot for weightclass by win_by/stoppage_decision
        # plotOutput("barPlot"),
        # 
        # # barplot for referee by last_round (3 selections)
        # plotOutput("refereePlot")
        
        tabsetPanel(
          tabPanel("Win Type", plotOutput("barPlot")),
          tabPanel("Referee", plotOutput("refereePlot"))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barPlot <- renderPlot({

      g <- ggplot(
        data = ufc_bind_top20,
        aes(x = weight_class))
      
      g + geom_bar(aes_string(fill = input$variable), position = 'fill') +
        labs(title='Weight class by win-type 100% STACK',
             x='weight class',
             y='distribution') +
        scale_fill_brewer(palette='Set1') +
        coord_flip() +
        theme_bw() +
        theme(legend.key=element_blank())
   
    })
    
    output$refereePlot <- renderPlot({

      p <- ggplot(
          data=ufc_bind_top20)

      p + geom_bar(aes(x=ufc_bind_top20$last_round,fill=input$variable),alpha = 0.8,position='dodge') +
          labs(title='Referees By last_round',
            x='last_round',
            y='count') +
          scale_fill_brewer(palette='Set1') +
          theme_bw() +
          theme(legend.key=element_blank())

        })
}

# Run the application 
shinyApp(ui = ui, server = server)
