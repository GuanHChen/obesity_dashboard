# --- Libraries ---
library(shiny)
library(tidyverse)
library(plotly)
library(scales)

# --- Load data ---
bmi_data <- read_csv("merged_data_with_gdp.csv")[, -c(1,2,4,15)]
unique_years <- sort(unique(bmi_data$Year), decreasing=TRUE)
all_countries <- unique(bmi_data$Location)

# --- UI ---
ui <- fluidPage(
  titlePanel("Global Overweight, GDP, and Alcohol Consumption Visualization"),
  
  # Space under title
  div(style = "margin-top: 30px;"),
  
  # Main display area
  fluidRow(
    column(4, # Year Dropdown
      selectInput(inputId = "select_year", 
                  label = "Select Year:",
                  choices = unique_years, 
                  selected = unique_years[1])
    ),
    column(4, # Variable Dropdown
      selectInput(inputId = "select_variable", 
                  label = "Select Variable:",
                  choices = c("Overweight Rate" = "OverweightRate", 
                              "Alcohol Consumption" = "Alcohol_Consumption", 
                              "GDP per Capita" = "GDP_per_capita"),
                  selected = "OverweightRate")
    ),
    column(4, # Country Checklist
           checkboxGroupInput(
             inputId = "select_countries",
             label = "Select Countries",
             choices = all_countries,
             selected = all_countries,
             inline = TRUE 
           )
    )
  ),
  
  # Spacer
  hr(), 
  
  # First row of plots
  fluidRow(
    column(6, plotlyOutput("bubble_plot", height = "500px")), # Plot 1: Bubble Plot
    column(6, plotOutput("timeseries_plot", height = "500px")) # Plot 2: Timeseries
  ),
  
  # 3. Second Row for Plots
  fluidRow(
    column(6, plotlyOutput("bubble_plot", height = "500px")), # Plot 1: Bubble Plot
    column(6, plotOutput("timeseries_plot", height = "500px")) # Plot 2: Timeseries
  ),
)

# --- Server Logic Definition ---
server <- function(input, output, session) {
  
  # Country Colors
  country_color_map <- c(
    "United States of America" = "#E63946",
    "Germany" = "#457B9D",
    "Japan" = "#2A9D8F"
  )
  scaled_data <- reactive({
      
  })
  
  output$bubble_plot <- renderPlotly({

    plot_ly(
      data = bmi_data,
      x = ~Alcohol_Consumption,
      y = ~GDP_per_capita,
      size = ~OverweightRate,
      color = ~Location,
      frame = ~Year,
      text = ~paste0(Location, "<br>Overweight: ", round(OverweightRate, 1)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      marker = list(
        opacity = 0.75,
        sizemode = 'diameter'
      )
    ) %>%
      layout(
        title = "Alcohol vs GDP (Size: Overweight Rate)",
        xaxis = list(title = "Alcohol Consumption (%)"),
        yaxis = list(title = "GDP per Capita (USD)"),
        legend = list(title = list(text = "Country"))
      ) %>%
      animation_opts(
        frame = 1000,
        transition = 200,
        redraw = FALSE
      )
  })
  
}

shinyApp(ui = ui, server = server)