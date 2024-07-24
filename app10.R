# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(scales)
library(bs4Dash)
library(DT)
library(readxl)  # For read_excel function
library(dplyr)   # For data manipulation
library(bslib)
library(shinycssloaders)
library(leaflet)
library(sf)      # For spatial data manipulation
library(rnaturalearth) # For natural earth data

# Loading the Data
life_insurers <- read_excel("./data/data.xlsx")

# Ensure latitude and longitude are numeric
life_insurers <- life_insurers %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Gross_written_premium_in_USD = as.numeric(gsub("USD ", "", Gross_written_premium_in_USD))
  )

# Define the UI with bs4Dash
ui <- bs4DashPage(
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
        "Map",
        tabName = "map",
        icon = icon("map")
      ),
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
      ),
      bs4TabItem(
        tabName = "map",
        fluidRow(
          box(width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "Map of Gross Written Premium",
              leafletOutput("map") %>% withSpinner())
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
  
  output$map <- renderLeaflet({
    # Aggregate GWP data by country
    aggregated_data <- life_insurers %>%
      group_by(Country, Latitude, Longitude) %>%
      summarise(Total_GWP = sum(Gross_written_premium_in_USD, na.rm = TRUE), .groups = 'drop')
    
    # Create a color palette for the heatmap
    pal <- colorNumeric("YlOrRd", domain = aggregated_data$Total_GWP)
    
    # Get African countries geometry
    africa <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")
    
    # Merge aggregated data with Africa geometry
    map_data <- merge(africa, aggregated_data, by.x = "name", by.y = "Country", all.x = TRUE)
    
    # Create leaflet map
    leaflet(data = map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        color = ~pal(Total_GWP),
        radius = ~sqrt(Total_GWP) / 50000,
        popup = ~paste("<b>Country:</b>", name, "<br/>",
                       "<b>Total GWP:</b>", scales::dollar(Total_GWP))
      ) %>%
      addPolygons(
        fillColor = ~pal(Total_GWP),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste(name, "- Total GWP:", scales::dollar(Total_GWP)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          opacity = 0.8
        )
      ) %>%
      setView(lng = 20, lat = 0, zoom = 3) %>%
      addLegend(
        "bottomright", pal = pal, values = aggregated_data$Total_GWP,
        title = "Total GWP (USD)", opacity = 1
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
