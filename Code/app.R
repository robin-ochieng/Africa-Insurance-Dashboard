# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(bs4Dash)
library(DT)

#Loading the Data
life_insurers <- read_excel("./data/data.xlsx") 

# Define the UI
ui <- fluidPage(
  titlePanel("Life Insurers Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country", choices = unique(life_insurers$Country)),
      selectInput("category", "Select Category Class", choices = unique(life_insurers$Category_Class)),
      hr(),
      DTOutput("table")
    ),
    mainPanel(
      plotlyOutput("barPlot"),
      plotlyOutput("scatterPlot")
    )
  )
)


# Define the server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    life_insurers %>%
      filter(Country == input$country & Category_Class == input$category)
  })
  
  output$table <- renderDT({
    datatable(filteredData())
  })
  
  output$barPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Life_Insurers, y = Gross_written_premium_in_USD)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Gross Written Premium by Life Insurers", x = "Life Insurers", y = "Gross Written Premium (USD)")
    
    ggplotly(p)
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(life_insurers, aes(x = Gross_written_premium_in_USD, y = Life_Insurers, color = Country)) +
      geom_point(size = 3) +
      labs(title = "Gross Written Premium Distribution", x = "Gross Written Premium (USD)", y = "Life Insurers")
    
    ggplotly(p)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
