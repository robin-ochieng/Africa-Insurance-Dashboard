# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(scales)
library(bs4Dash)
library(DT)
library(readxl)  # For read_excel function
library(dplyr)   # For data manipulation
library(fresh)
library(maps)




theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD",
    light = "#f8f9fa"
  ),
  bs4dash_status(
    primary = "#007bff",
    secondary = "#6c757d",
    success = "#198754",
    info = "#0dcaf0",
    warning = "#ffc107",
    danger = "#dc3545",
    light = "#f8f9fa",
    dark = "#343a40"
  ),
  bs4dash_vars(
    navbar_light_color = "#ffffff",
    navbar_light_active_color = "#52A1A5",
    navbar_light_hover_color = "#6c757d"
  )
)


# Loading the Data
life_insurers <- read_excel("./data/data.xlsx") 

# Define the UI with bs4Dash
ui <- bs4DashPage(
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Life Insurers In Africa",
      color = "primary"
    )
  ),
  sidebar = dashboardSidebar(
    title = "Options",
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("home")
      )
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            fluidRow(selectInput("country", "Select Country", choices = unique(life_insurers$Country)),
                     hr(),
                     selectInput("category", "Select Category Class", choices = unique(life_insurers$Category_Class)),
                     hr(),),
            solidHeader = TRUE,
            status = "primary",
            title = "Life Insurers Table",
            DTOutput("table") %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(
          box(width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "Gross Written Premium by Life Insurers",
              plotlyOutput("barPlot") %>% withSpinner())
        )
      )
    )
  ),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter()
)

# Define the server logic
server <- function(input, output, session) {
  
  filteredData <- reactive({
    life_insurers %>%
      filter(Country == input$country & Category_Class == input$category)
  })
  
  output$table <- renderDT({
    datatable(filteredData(),
              options = list(
                dom = 't', # This option is to show only the table without the default DataTables controls
                paging = FALSE, # Disable pagination
                ordering = TRUE, # Enable column ordering
                info = FALSE, # Disable showing table information
                searching = FALSE, # Disable search box
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-left', targets = '_all') # Center text in all columns
                ),
                initComplete = JS(
                  "function(settings, json) {", 
                  "$(this.api().table().header()).css({'background-color': '#4A9094', 'color': 'white', 'text-align': 'center'});", 
                  "}"
                )
              ))
  })
  
  output$barPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Life_Insurers, y = Gross_written_premium_in_USD)) +
      geom_bar(stat = "identity", fill = "#3498db") + # A more appealing shade of blue
      geom_text(aes(label = paste0(sprintf("%.2f", Gross_written_premium_in_USD / 1e6), "M")), vjust = -0.3, color = "black") +
      theme_minimal() + # Cleaner theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "", x = "Life Insurers", y = "Gross Written Premium (USD)")
    
    ggplotly(p)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
