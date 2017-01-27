library(shiny)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
ui <- fluidPage(
  titlePanel("Store prices", windowTitle = "Store Prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100,
                  value = c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      selectInput("countryInput", "Country",
                  choices = c("CANADA", "FRANCE", "ITALY"))
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

library(ggplot2)
library(dplyr)
server <- function(input, output) {
  filtered <- reactive({
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  output$coolplot <- renderPlot({
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)

# library(shiny)
# bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
# #ui <- fluidPage()
# ui <- fluidPage("Store", "prices")
# server <- function(input, output) {}
# shinyApp(ui = ui, server = server)