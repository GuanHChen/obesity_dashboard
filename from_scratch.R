# --- Libraries ---
library(shiny)
library(tidyverse)
library(plotly)
library(scales)

# --- Load data ---
bmi_data <- read_csv("merged_data_with_gdp.csv")[, -c(1,2,4,15)]
unique_years <- sort(unique(bmi_data$Year), decreasing=TRUE)
all_countries <- unique(bmi_data$Location)
country_color_map <- c(
  "United States of America" = "#E63946",
  "Germany" = "#457B9D",
  "Japan" = "#2A9D8F"
)
plot_height = "400px"

# --- UI ---
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
    "))
  ),
  
  div(
    style = "
    position: sticky;
    top: 0;
    z-index: 999;
    background-color: #F2F2F2;
  ",
    
    div(style = "padding: 20px; padding-bottom: 0;",
    tags$h1(
      HTML("<b>Global Overweight, GDP, and Alcohol Consumption Visualization</b>"),
      style = "font-size: 40px;"
    ),
    
    # Filters
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
    )
    ),
    # Divider Bar
    div(style = "height: 6px; background-color: #CCCCCC; width: 100%;"),
  ), 
  
  # Spacer
  div(style = "margin-top: 30px;"),
  # First row of plots
  fluidRow(
    column(6, plotlyOutput("bubble_plot", height = plot_height)),
    column(6, plotlyOutput("timeseries_plot", height = plot_height))
  ),
  # Spacer
  div(style = "margin-top: 60px;"),
  # Second row of plots
  fluidRow(
    column(6, plotlyOutput("heatmap_plot", height = plot_height)),
    column(6, plotlyOutput("bar_plot", height = plot_height))
  ),
  # Bottom Margin
  div(style = "margin-top: 60px;")
  
)

# --- Server Logic Definition ---
server <- function(input, output, session) {
  
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
        legend = list(title = list(text = "<b>Country</b>")),
        plot_bgcolor = "#FAFAFA"
      ) %>%
      animation_opts(
        frame = 500,
        transition = 500,
        redraw = FALSE
      ) %>% 
      animation_slider(
        currentvalue = list(
          prefix = "Year: ",
          font = list(color = "#545454") # Use a dark gray/black color
        )
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
    
    # Manually set legend order for consistency
    legend_order <- c("Germany", "Japan", "United States of America")
    display <- legend_order[legend_order %in% input$select_countries]
    
    for (country in display) {
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
        plot_bgcolor = "#FAFAFA",
        margin = list(t = 50),
        legend = list(title = list(text = "<b>Country</b>"))
      )
  })
  
  # Heatmap Plot
  output$heatmap_plot <- renderPlotly({
    corr_data <- lapply(input$select_countries, function(cty) {
      
      df <- bmi_data %>% filter(Location == cty)
      
      tibble(
        Country = cty,
        Pair = c("Alcohol vs GDP",
                 "Overweight vs Alcohol",
                 "Overweight vs GDP"),
        Correlation = c(
          cor(df$Alcohol_Consumption, df$GDP_per_capita, use = "complete.obs"),
          cor(df$OverweightRate, df$Alcohol_Consumption, use = "complete.obs"),
          cor(df$OverweightRate, df$GDP_per_capita, use = "complete.obs")
        ),
        Color = country_color_map[cty]
      )
    }) %>% bind_rows()
    
    corr_data %>%
      plot_ly(
        x = ~Pair,
        y = ~Correlation,
        color = ~Country,
        colors = country_color_map,
        type = "bar",
        hovertemplate = paste0(
          "<b>%{fullData.name}</b><br>",
          "%{x}<br>",
          "Correlation: %{y:.3f}<extra></extra>"
        )
      ) %>%
      layout(
        barmode = "group",
        title = "<b>Correlation Comparison Across Countries</b>",
        xaxis = list(title = "<b>Variable Pairs</b>"),
        yaxis = list(title = "<b>Correlation</b>", range = c(-1,1)),
        plot_bgcolor = "#FAFAFA",
        margin = list(t = 60),
        legend = list(title = list(text = "<b>Country</b>"))
      )
  })
  
  # Barplot
  output$bar_plot <- renderPlotly({

    # Filter by selected countries
    df <- bmi_data %>% filter(Location %in% input$select_countries)
    
    # Filter by year if selected
    if (!is.null(input$select_year) && input$select_year != "") {
      df <- df %>% filter(Year == input$select_year)
    }
    
    # Use col_map logic to pick the correct column
    col_map <- switch(input$select_variable,
                      "OverweightRate" = list(var = "OverweightRate", label = "Overweight Rate (%)"),
                      "Alcohol_Consumption" = list(var = "Alcohol_Consumption", label = "Alcohol Consumption (%)"),
                      "GDP_per_capita" = list(var = "GDP_per_capita", label = "GDP per Capita (USD)")
    )
    
    plot_ly(
      df,
      x = ~Location,
      y = df[[col_map$var]],
      type = "bar",
      marker = list(color = country_color_map[df$Location]),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        col_map$label, ": %{y:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        title = paste0("<b>", col_map$label, " by Country</b>"),
        xaxis = list(title = "<b>Country</b>"),
        yaxis = list(title = paste0("<b>", col_map$label, "</b>")),
        plot_bgcolor = "#FAFAFA",
        margin = list(t = 60)
      )
  })
  
} # End of Server


shinyApp(ui = ui, server = server)