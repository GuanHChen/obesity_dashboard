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
  # tags$style(HTML("
  #   body, html {
  #     background-color: #F2F2F2 !important;
  #   }
  # ")),
  
  div(
    style = "background-color: #F2F2F2; padding: 20px;",
    tags$h1(
      HTML("<b>Global Overweight, GDP, and Alcohol Consumption Visualization</b>"),
      style = "font-size: 40px; margin-top: 20px;"
    ),
    div(style = "margin-top: 20px;"),
    
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
  ),
  # Divider Bar
  div(style = "height: 6px; background-color: #CCCCCC; width: 100%; margin-bottom: 30px;"),
  # First row of plots
  fluidRow(
    column(6, plotlyOutput("bubble_plot", height = "500px")),
    column(6, plotlyOutput("timeseries_plot", height = "500px"))
  ),
  
  # Second row of plots
  fluidRow(
    column(6, plotlyOutput("bubbasdfle_plot", height = "500px")),
    column(6, plotlyOutput("timeasdfseries_plot", height = "500px"))
  )
  
)

# --- Server Logic Definition ---
server <- function(input, output, session) {
  
  # Country Colors
  country_color_map <- c(
    "United States of America" = "#E63946",
    "Germany" = "#457B9D",
    "Japan" = "#2A9D8F"
  )
  # Reactive filtered data based on filter selection
  filtered_data <- reactive({
    req(input$select_countries)
    bmi_data %>%
      filter(Location %in% input$select_countries) %>% 
      mutate(
        # Apply square root scaling for balanced bubble sizes
        Overweight_transformed = sqrt(OverweightRate),
        # Normalize to 0-1 range
        Overweight_normalized = (Overweight_transformed - min(Overweight_transformed, na.rm = TRUE)) /
          (max(Overweight_transformed, na.rm = TRUE) - min(Overweight_transformed, na.rm = TRUE)),
        # Scale to visual range with 1.2 multiplier
        Overweight_scaled = 15 + (Overweight_normalized * 50 * 1.2)
      )
  })
  
  # Bubble Plot
  output$bubble_plot <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~Alcohol_Consumption,
      y = ~GDP_per_capita,
      size = ~Overweight_scaled,
      color = ~Location,
      colors = country_color_map,
      frame = ~Year,
      text = ~paste0(
        "<b>", Location, "</b><br>",
        "Year: ", Year, "<br>",
        "Overweight Rate: ", round(OverweightRate, 1), "%<br>",
        "Alcohol Consumption: ", round(Alcohol_Consumption, 1), "%<br>",
        "GDP per Capita: $", format(round(GDP_per_capita, 0), big.mark = ",")
      ),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      marker = list(
        opacity = 0.75,
        sizemode = 'diameter'
      )
    ) %>%
      layout(
        margin = list(t = 50),
        title = "<b>Alcohol vs GDP (Size: Overweight Rate)</b>",
        xaxis = list(title = "<b>Alcohol Consumption (%)</b>", titlefont = 50),
        yaxis = list(title = "<b>GDP per Capita (USD)</b>"),
        legend = list(title = list(text = "<b>Country</b>"))
      ) %>%
      animation_opts(
        frame = 500,
        transition = 500,
        redraw = FALSE
      )
  })
  
  # Bubble Plot Time Slider
  observeEvent(input$select_year, {
    proxy <- plotlyProxy("bubble_plot", session)
    plotlyProxyInvoke(proxy, "animate", 
                      list(as.character(input$select_year)), 
                      list(
                        mode = "immediate", 
                        frame = list(duration = 0, redraw = FALSE), 
                        transition = list(duration = 0)
                      )
    )
  })

  
  # Timeseries Plot
  output$timeseries_plot <- renderPlotly({
    
    req(input$select_variable, input$select_countries) # Filters
    
    # Data
    col_map <- switch(input$select_variable,
                      "OverweightRate" = list(
                        var = "OverweightRate", 
                        low = "OverweightRate_Lower", 
                        high = "OverweightRate_Upper", 
                        label = "Overweight Rate (%)"
                      ),
                      "Alcohol_Consumption" = list(
                        var = "Alcohol_Consumption", 
                        low = "Alcohol_Lower_CI", 
                        high = "FactValueNumericHigh_y", 
                        label = "Alcohol Consumption (%)"
                      ),
                      "GDP_per_capita" = list(
                        var = "GDP_per_capita",
                        low = NA,
                        high = NA,
                        label = "GDP per Capita (USD)"
                      )
    )
    # Data
    timeseries_data <- bmi_data %>% 
      filter(Location %in% input$select_countries)
    
    p <- plot_ly()
    
    for (country in input$select_countries) {
      country_df <- timeseries_data %>% filter(Location == country)
      
      color <- country_color_map[country]
      
      # --- Confidence Interval Ribbon ---
      if (!is.na(col_map$low) && !is.na(col_map$high)) {
        p <- p %>%
          add_trace(
            data = country_df,
            x = ~Year,
            y = ~get(col_map$high),
            type = "scatter",
            mode = "lines",
            line = list(width = 0),
            showlegend = FALSE,
            hoverinfo = "skip",
            legendgroup = country
          ) %>%
          add_trace(
            data = country_df,
            x = ~Year,
            y = ~get(col_map$low),
            type = "scatter",
            mode = "lines",
            fill = "tonexty",
            fillcolor = scales::alpha(color, 0.20),
            line = list(width = 0),
            showlegend = FALSE,
            hoverinfo = "skip",
            legendgroup = country
          )
      }
      p <- p %>%
        add_trace(
          data = country_df,
          x = ~Year,
          y = ~get(col_map$var),
          type = "scatter",
          mode = "lines+markers",
          name = country,
          line = list(color = color, width = 3),
          marker = list(color = color, size = 8),
          legendgroup = country,
          hovertemplate = paste0(
            "<b>", country, "</b><br>",
            "Year: %{x}<br>",
            col_map$label, ": %{y:.2f}<br>",
            "<extra></extra>"
          )
        )
    }
    p %>%
      layout(
        title = paste0("<b>", col_map$label, " Over Time</b>"),
        xaxis = list(title = "<b>Year</b>"),
        yaxis = list(title = paste0("<b>", col_map$label, "</b>")),
        hovermode = "x unified",
        plot_bgcolor = "#F9F9F9",
        margin = list(t = 50),
        legend = list(title = list(text = "<b>Country</b>"))
      )
  })
  
} # End of Server


shinyApp(ui = ui, server = server)