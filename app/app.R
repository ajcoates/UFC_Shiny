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
          tabPanel("Win Type", plotOutput("barPlot"), plotOutput("weightclasstotalPlot")),
          tabPanel("Referee", plotOutput("refereePlot"), plotOutput("refereetotalPlot"), plotOutput("refereetotalSTACKPlot"))
                   
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
        labs(title='Normalized Interactive - Weight Class by Outcome',
             x='weight class',
             y='') +
        scale_fill_brewer(palette='Set1') +
        coord_flip() +
        theme_bw() +
        theme(legend.key=element_blank())
   
    })
    
    output$refereePlot <- renderPlot({
      
      y <- ufc_bind_top20[which(ufc_bind_top20$referee %in% input$refnames),]
      
      ggplot(data=y) +
        geom_bar(aes(x=y$last_round,fill=y$referee),alpha = 0.8,position='dodge') +
        labs(title='Interactive - Referees by Last Round',
             x='Last Round',
             y='Number of Matches') +
        scale_fill_brewer(palette='Set1') +
        theme_bw() +
        theme(legend.key=element_blank())

      })
    
    output$weightclasstotalPlot <- renderPlot({
      
      ggplot(data=ufc_bind_top20) +
        geom_bar(aes(x=ufc_bind_top20$weight_class,fill=ufc_bind_top20$win_by,position='fill')) +
        labs(title='Weight Classes by Outcome',
             x='Weight Class',
             y='Number of Matches') +
        scale_fill_brewer(palette='Set1') +
        coord_flip() + 
        theme_bw() +
        theme(legend.key=element_blank())
    
      
    })
    
    output$refereetotalPlot <- renderPlot({
    
      ggplot(data=ufc_bind_top20) +
        geom_bar(aes(x=referee,fill=stoppage_decision,position="dodge2")) +
        labs(title='Referee Experience by Outcome Type',
             x='Referee',
             y='Number of Matches') +
        scale_fill_brewer(palette='Set1') +
        coord_flip() + 
        theme_bw() +
        theme(legend.key=element_blank())
      
    })
    
    output$refereetotalSTACKPlot <- renderPlot({
      
      ggplot(data = ufc_bind_top20, aes(x = referee)) +
        geom_bar(aes(fill = stoppage_decision), position = 'fill') +
        labs(title='Normalized - Referee by Outcome Type',
             x='Referee',
             y='') +
        scale_fill_brewer(palette='Set1') +
        coord_flip() +
        theme_bw() +
        theme(legend.key=element_blank())
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
