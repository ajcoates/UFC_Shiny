library(DT)
library(shiny)
library(shinydashboard)

ufc_bind_top20 <- read.csv(file = "ufc_bind_top20", stringsAsFactors = FALSE)
print(ufc_bind_top20)
ui <- fluidPage(
    titlePanel("BC Liquor Store prices"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
            radioButtons("typeInput", "Product type",
                         choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                         selected = "WINE"),
            uiOutput("countryOutput")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("weightclass")
        )
    ) 
)
