# --- Libraries ---
library(shiny)
library(tidyverse)
library(plotly)
library(scales)

# --- Load data ---
bmi_data <- read_csv("cleaned_merged_data.csv")
gdp <- read_csv("merged_data_with_gdp.csv")

# --- UI ---
ui <- fluidPage(
  titlePanel("Global Overweight, GDP, and Alcohol Consumption Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Visualization:"),
      radioButtons("vizType", 
                   label = NULL,
                   choices = c("Animated Bubble Chart" = "bubble",
                               "Time Series Plot" = "timeseries",
                               "Correlation Heatmap" = "heatmap",
                               "Country Comparison" = "comparison"),
                   selected = "bubble"),
      hr(),
      conditionalPanel(
        condition = "input.vizType == 'bubble'",
        helpText("Animated bubble chart showing overweight rate, alcohol consumption, 
                 and GDP per capita across countries over time.")
      ),
      conditionalPanel(
        condition = "input.vizType == 'timeseries'",
        checkboxGroupInput("tsCountries",
                           "Select Countries:",
                           choices = NULL,
                           selected = NULL),
        selectInput("tsVariable",
                    "Select Variable:",
                    choices = c("Overweight Rate" = "OverweightRate",
                                "Alcohol Consumption" = "Alcohol_Consumption",
                                "GDP per Capita" = "GDP_per_capita")),
        conditionalPanel(
          condition = "input.tsVariable != 'GDP_per_capita'",
          checkboxInput("showCI", "Show Confidence Intervals", value = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.vizType == 'comparison'",
        selectInput("compYear",
                    "Select Year:",
                    choices = NULL,
                    selected = NULL),
        selectInput("compVariable",
                    "Compare by:",
                    choices = c("Overweight Rate" = "OverweightRate",
                                "Alcohol Consumption" = "Alcohol_Consumption",
                                "GDP per Capita" = "GDP_per_capita"))
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.vizType == 'bubble'",
        plotlyOutput("bubblePlot", height = "700px")
      ),
      conditionalPanel(
        condition = "input.vizType == 'timeseries'",
        plotlyOutput("timeseriesPlot", height = "700px")
      ),
      conditionalPanel(
        condition = "input.vizType == 'heatmap'",
        plotlyOutput("heatmapPlot", height = "700px")
      ),
      conditionalPanel(
        condition = "input.vizType == 'comparison'",
        plotlyOutput("comparisonPlot", height = "700px")
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Populate dynamic UI elements with data
  observe({
    countries <- unique(gdp$Location)
    years <- sort(unique(gdp$Year))
    
    updateCheckboxGroupInput(session, "tsCountries",
                             choices = countries,
                             selected = head(countries, 3))
    
    updateSelectInput(session, "compYear",
                      choices = years,
                      selected = max(years, na.rm = TRUE))
  })
  
  output$bubblePlot <- renderPlotly({
    req(input$vizType == "bubble")
    
    # --- Prepare data inside renderPlotly ---
    data_all <- gdp %>%
      drop_na(GDP_per_capita, Alcohol_Consumption, OverweightRate, Year, Location) %>%
      mutate(
        # Apply square root scaling for balanced bubble sizes
        Overweight_transformed = sqrt(OverweightRate),
        # Normalize to 0-1 range
        Overweight_normalized = (Overweight_transformed - min(Overweight_transformed, na.rm = TRUE)) /
          (max(Overweight_transformed, na.rm = TRUE) - min(Overweight_transformed, na.rm = TRUE)),
        # Scale to visual range with 1.2 multiplier
        Overweight_scaled = 15 + (Overweight_normalized * 50 * 1.2)
      )
    
    # --- Create interactive animated bubble chart ---
    plot_ly(
      data = data_all,
      x = ~Alcohol_Consumption,
      y = ~GDP_per_capita,
      size = ~Overweight_scaled,
      color = ~Location,
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
        line = list(width = 2, color = '#FFFFFF'),
        sizemode = 'diameter'
      ),
      sizes = c(15, 65)
    ) %>%
      layout(
        title = list(
          text = "<b>Alcohol Consumption vs GDP per Capita</b><br><sup>Bubble Size = Overweight Rate (%)</sup>",
          x = 0.5,
          font = list(size = 18)
        ),
        xaxis = list(
          title = "<b>Alcohol Consumption (%)</b>",
          titlefont = list(size = 14),
          gridcolor = '#E8E8E8'
        ),
        yaxis = list(
          title = "<b>GDP per Capita (USD)</b>",
          titlefont = list(size = 14),
          gridcolor = '#E8E8E8'
        ),
        legend = list(
          title = list(text = "<b>Country</b>"), 
          orientation = "v", 
          x = 1.02, 
          xanchor = "left",
          y = 1,
          yanchor = "top"
        ),
        plot_bgcolor = '#F9F9F9',
        paper_bgcolor = '#FFFFFF'
      ) %>%
      animation_opts(
        frame = 1200,
        transition = 300,
        redraw = FALSE
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Year: ",
          font = list(size = 16, color = "#333333")
        )
      )
    
  })
  
  output$timeseriesPlot <- renderPlotly({
    req(input$vizType == "timeseries", input$tsCountries, input$tsVariable)
    
    data_filtered <- gdp %>%
      filter(Location %in% input$tsCountries) %>%
      drop_na(Year, !!sym(input$tsVariable))
    
    var_label <- case_when(
      input$tsVariable == "OverweightRate" ~ "Overweight Rate (%)",
      input$tsVariable == "Alcohol_Consumption" ~ "Alcohol Consumption (%)",
      input$tsVariable == "GDP_per_capita" ~ "GDP per Capita (USD)"
    )
    
    # Determine CI columns based on variable
    ci_lower <- case_when(
      input$tsVariable == "OverweightRate" ~ "OverweightRate_Lower",
      input$tsVariable == "Alcohol_Consumption" ~ "Alcohol_Lower_CI",
      input$tsVariable == "GDP_per_capita" ~ NA_character_
    )
    
    ci_upper <- case_when(
      input$tsVariable == "OverweightRate" ~ "OverweightRate_Upper",
      input$tsVariable == "Alcohol_Consumption" ~ "FactValueNumericHigh_y",
      input$tsVariable == "GDP_per_capita" ~ NA_character_
    )
    
    # Define custom color palette
    custom_colors <- c("#E63946", "#457B9D", "#2A9D8F", "#F4A261", "#E76F51", 
                       "#8338EC", "#3A86FF", "#FB5607", "#06FFA5", "#FFBE0B")
    
    # Create base plot
    p <- plot_ly()
    
    # Add traces for each country
    for (i in seq_along(input$tsCountries)) {
      country <- input$tsCountries[i]
      country_data <- data_filtered %>% filter(Location == country)
      country_color <- custom_colors[((i - 1) %% length(custom_colors)) + 1]
      
      # Add confidence interval if available and checkbox is selected
      show_ci <- if(input$tsVariable == "GDP_per_capita") FALSE else isTRUE(input$showCI)
      
      if (show_ci && !is.na(ci_lower) && !is.na(ci_upper)) {
        # Add CI ribbon
        p <- p %>%
          add_trace(
            data = country_data,
            x = ~Year,
            y = ~get(ci_upper),
            type = 'scatter',
            mode = 'lines',
            line = list(width = 0),
            showlegend = FALSE,
            name = paste(country, "Upper"),
            hoverinfo = 'skip',
            legendgroup = country
          ) %>%
          add_trace(
            data = country_data,
            x = ~Year,
            y = ~get(ci_lower),
            type = 'scatter',
            mode = 'lines',
            fill = 'tonexty',
            fillcolor = scales::alpha(country_color, 0.2),
            line = list(width = 0),
            showlegend = FALSE,
            name = paste(country, "Lower"),
            hoverinfo = 'skip',
            legendgroup = country
          )
      }
      
      # Add main line with markers
      p <- p %>%
        add_trace(
          data = country_data,
          x = ~Year,
          y = ~get(input$tsVariable),
          type = 'scatter',
          mode = 'lines+markers',
          name = country,
          line = list(width = 3, color = country_color),
          marker = list(size = 8, color = country_color),
          legendgroup = country,
          hovertemplate = paste0(
            "<b>", country, "</b><br>",
            "Year: %{x}<br>",
            var_label, ": %{y:.2f}<br>",
            "<extra></extra>"
          )
        )
    }
    
    p %>%
      layout(
        title = paste("<b>", var_label, "Over Time</b>"),
        xaxis = list(title = "<b>Year</b>"),
        yaxis = list(title = paste0("<b>", var_label, "</b>")),
        hovermode = "x unified",
        plot_bgcolor = '#F9F9F9'
      )
  })
  
  output$heatmapPlot <- renderPlotly({
    req(input$vizType == "heatmap")
    
    # Calculate correlations by country and year
    corr_by_country <- gdp %>%
      group_by(Location) %>%
      summarise(
        `Overweight vs Alcohol` = cor(OverweightRate, Alcohol_Consumption, use = "complete.obs"),
        `Overweight vs GDP` = cor(OverweightRate, GDP_per_capita, use = "complete.obs"),
        `Alcohol vs GDP` = cor(Alcohol_Consumption, GDP_per_capita, use = "complete.obs"),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = -Location, names_to = "Relationship", values_to = "Correlation")
    
    # Create grouped bar chart
    plot_ly(corr_by_country, 
            x = ~Relationship, 
            y = ~Correlation, 
            color = ~Location,
            type = 'bar',
            text = ~round(Correlation, 2),
            textposition = 'outside',
            hovertemplate = paste0(
              "<b>%{fullData.name}</b><br>",
              "%{x}<br>",
              "Correlation: %{y:.3f}<extra></extra>"
            )) %>%
      layout(
        title = "<b>Variable Correlations by Country</b>",
        xaxis = list(title = "<b>Relationship</b>"),
        yaxis = list(title = "<b>Correlation Coefficient</b>", range = c(-1, 1)),
        barmode = 'group',
        plot_bgcolor = '#F9F9F9',
        shapes = list(
          # Add reference line at y=0
          list(type = "line", x0 = -0.5, x1 = 2.5, y0 = 0, y1 = 0,
               line = list(color = "black", width = 1, dash = "dash"))
        ),
        annotations = list(
          list(x = 0.5, y = -0.25, xref = "paper", yref = "paper",
               text = "<i>Positive correlation (>0) means variables increase together<br>Negative correlation (<0) means one increases as other decreases</i>",
               showarrow = FALSE, xanchor = "center", font = list(size = 11, color = "#666"))
        )
      )
  })
  
  output$comparisonPlot <- renderPlotly({
    req(input$vizType == "comparison", input$compYear, input$compVariable)
    
    data_filtered <- gdp %>%
      filter(Year == input$compYear) %>%
      drop_na(Location, !!sym(input$compVariable)) %>%
      arrange(desc(!!sym(input$compVariable)))
    
    var_label <- case_when(
      input$compVariable == "OverweightRate" ~ "Overweight Rate (%)",
      input$compVariable == "Alcohol_Consumption" ~ "Alcohol Consumption (%)",
      input$compVariable == "GDP_per_capita" ~ "GDP per Capita (USD)"
    )
    
    plot_ly(data_filtered, x = ~get(input$compVariable), y = ~reorder(Location, get(input$compVariable)),
            type = 'bar', orientation = 'h',
            marker = list(color = '#1f77b4'),
            text = ~round(get(input$compVariable), 1),
            textposition = 'outside') %>%
      layout(
        title = paste0("<b>", var_label, " by Country (", input$compYear, ")</b>"),
        xaxis = list(title = paste0("<b>", var_label, "</b>")),
        yaxis = list(title = ""),
        plot_bgcolor = '#F9F9F9'
      )
  })
  
}

# --- Run the app ---
shinyApp(ui = ui, server = server)