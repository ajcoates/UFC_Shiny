library(DT)
library(shiny)
library(googleVis)

server <- function(input, output) {
    output$countryOutput <- renderUI({
        selectInput("countryInput", "Country",
                    sort(unique(bcl$Country)),
                    selected = "CANADA")
    })
    print("hello 1")
    
    filtered <- reactive({
        if (FALSE) {
            return(NULL)
        }    
        
        ufc_bind_top20 %>%
            filter(TRUE
            )
    })
    
    output$weightclass <- renderPlot({
        if (is.null(filtered())) {
            return()
        }
        ggplot(filtered(), aes(x = weight_class)) +
            geom_bar(aes(fill = win_by), position = 'fill') +
            labs(title='Weight class by win-type 100% STACK',
                 x='weight class',
                 y='') +
            scale_fill_brewer(palette='Set1') +
            coord_flip() +
            theme_bw() +
            theme(legend.key=element_blank())
    })
    
    output$results <- renderTable({
        filtered()
    })

}

shinyApp(ui = ui, server = server)
