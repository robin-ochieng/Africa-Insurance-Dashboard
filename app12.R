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
library(viridis) 

# Loading the Data
life_insurers <- read_excel("./data/data.xlsx", sheet = 'Insurers')

ifrs17 <- read_excel("./data/data.xlsx",sheet = 'IFRS17')


# Ensure latitude and longitude are numeric
life_insurers <- life_insurers %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Gross_written_premium_in_USD = as.numeric(gsub("USD ", "", Gross_written_premium_in_USD))
  )


# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1",
  olive = "#4A9094",
  purple = "#8965CD",
  lime = "#52A1A5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)

# Define the UI with bs4Dash
ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = dashboardHeader(
    status = "primary",
    title = dashboardBrand(
      title = HTML("<b>LIFE INSURANCE IN AFRICA</b>"),
      color = "primary"
    ),
    controlbarIcon = NULL
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
    tags$head(
      tags$style(
        type = 'text/css',
        '.custom-select-input { width: 20%; }', # Custom CSS class for the select input width
        '.dashboardBrand-title { font-weight: bold; color: #ff9800; }'
      )
    ),
    bs4TabItems(
      bs4TabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("totalGWPBox", width = 6),
          valueBoxOutput("averageGWPBox", width = 6)
        ),
        fluidRow(
          valueBoxOutput("numImplementedBox", width = 6),
          valueBoxOutput("numNotImplementedBox", width = 6)
        ),
        fluidRow(
          box(
            fluidRow(hr(),
                     div(class = "custom-select-input",
                         selectInput("country", "Select Country", choices = unique(life_insurers$Country), 
                                     selectize = TRUE)
                     ),
                     hr(),
                     selectInput("category", "Select GWP Category Class", choices = unique(life_insurers$Category_Class)),
                     hr(),),
            solidHeader = TRUE,
            status = "primary",
            title = "Life Insurers Interactive Table",
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
              maximizable = TRUE,
              status = "primary",
              title = "Gross Written Premium Distribution Across Africa",
              leafletOutput("map" , height = "480px") %>% withSpinner())
        )
      )
    )
  ),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(
    div(style = "background-color: #007bff; color: white; text-align: center; padding: 8px;", 
        "© 2024 Life Insurers in Africa Dashbord | Powered by Technology and Research Department | Kenbright")
  )
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
                  list(className = 'dt-center', targets = '_all'), # Center text in all columns
                  list(targets = 5, # Target the 5th column (zero-indexed)
                       render = JS(
                         "function(data, type, row) {",
                         "  if(type === 'display' || type === 'filter'){",
                         "    return parseFloat(data).toLocaleString('en-US', {",
                         "      minimumFractionDigits: 0,",
                         "      maximumFractionDigits: 0",
                         "    });",
                         "  }",
                         "  return data;",
                         "}"
                       )),
                  list(visible = FALSE, targets = c(2, 3))
                ),
                initComplete = JS(
                  "function(settings, json) {", 
                  "$(this.api().table().header()).css({'background-color': '#007bff', 'color': 'white', 'text-align': 'center'});", 
                  "}"
                )
              ))
  })
  
  output$barPlot <- renderPlotly({
    # Calculate dynamic vjust values based on Gross_written_premium_in_USD
    dynamic_vjust <- function(premium) {
      ifelse(premium > 1e8, 0, -3)
    }
    p <- ggplot(filteredData(), aes(x = Insurer, y = Gross_written_premium_in_USD)) +
      geom_bar(stat = "identity", fill = "#3498db") + # A more appealing shade of blue
      geom_text(aes(label = paste0(format(round(Gross_written_premium_in_USD / 1e6), big.mark = ",", trim = TRUE), "M"),
                    vjust = dynamic_vjust(Gross_written_premium_in_USD)), color = "black", size = 3) +
      theme_minimal() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")
      ) +
      labs(title = "", x = "Life Insurers", y = "Gross Written Premium (USD)")+
      scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6))
    ggplotly(p)
  })
  
  output$map <- renderLeaflet({
    # Aggregate GWP data by country
    aggregated_data <- life_insurers %>%
      group_by(Country, Latitude, Longitude) %>%
      summarise(Total_GWP = sum(Gross_written_premium_in_USD, na.rm = TRUE), .groups = 'drop') %>%
      # Convert Total_GWP to millions with 2 decimal s and add "M" suffix
      mutate(Total_GWP_Millions = paste0(comma(Total_GWP / 1000000, accuracy = 0.01), "M"))
    # Create a color palette for the heatmap
    pal <- colorNumeric(palette = "viridis", domain = aggregated_data$Total_GWP)
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
                       "<b>Total GWP:</b>", Total_GWP_Millions) # Use formatted Total_GWP
      ) %>%
      addPolygons(
        fillColor = ~pal(Total_GWP),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste(name, "- Total GWP:", Total_GWP_Millions), # Use formatted Total_GWP
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
  
  
  
  # Reactive expressions to calculate the number of countries that implemented/not implemented IFRS17
  numImplemented <- reactive({
    sum(ifrs17$Ifrs17_implementation == "Yes")
  })
  
  numNotImplemented <- reactive({
    sum(ifrs17$Ifrs17_implementation == "No")
  })
  
  totalCountries <- reactive({
    numImplemented() + numNotImplemented()
  })
  
  implementedPercentage <- reactive({
    if (totalCountries() == 0) return(0)
    numImplemented() / totalCountries() * 100
  })
  
  notImplementedPercentage <- reactive({
    if (totalCountries() == 0) return(0)
    numNotImplemented() / totalCountries() * 100
  })
  
  # Output the values in the value boxes
  output$numImplementedBox <- renderValueBox({
    valueBox(
      value = paste0(sprintf("%.2f", implementedPercentage()), "%"),
      subtitle = "Percentage of African Countries with IFRS17 Implementation",
      color = "success",
      icon = icon("check-circle")
    )
  })
  
  output$numNotImplementedBox <- renderValueBox({
    valueBox(
      value = paste0(sprintf("%.2f", notImplementedPercentage()), "%"),
      subtitle = "Percentage of Countries Not Implemented IFRS17",
      color = "secondary",
      icon = icon("times-circle")
    )
  })
  
  # Reactive expression to calculate total Gross Written Premium in millions
  totalGWP <- reactive({
    sum(life_insurers$Gross_written_premium_in_USD, na.rm = TRUE) / 1e6
  })
  
  # Reactive expression to calculate the average Gross Written Premium per insurer in millions
  averageGWP <- reactive({
    mean(life_insurers$Gross_written_premium_in_USD, na.rm = TRUE) / 1e6
  })
  
  
  # Output the values in the value boxes
  output$totalGWPBox <- renderValueBox({
    valueBox(
      value = paste0("$", scales::comma(totalGWP()), " M"), # Format with comma and add "M" suffix
      subtitle = "Total Gross Written Premium",
      color = "purple",
      icon = icon("dollar-sign")
    )
  })
  
  output$averageGWPBox <- renderValueBox({
    valueBox(
      value = paste0("$", scales::comma(averageGWP()), " M"), # Format with comma and add "M" suffix
      subtitle = "Average Gross Written Premium per Insurer",
      color = "warning",
      icon = icon("calculator")
    )
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
