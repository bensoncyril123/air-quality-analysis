# Load necessary libraries
library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(dplyr)
library(lubridate)

# Load the dataset
data <- read_csv("air_quality_index.csv")

# Convert the DATE column to Date type and rename columns for convenience
data <- data %>%
  rename(date = DATE, country = COUNTRY, city = CITY, PM2.5 = VALUE) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Filter the data for the USA and India
data_us <- data %>% filter(country == "US")
data_india <- data %>% filter(country == "IN")

# Define UI for the application
ui <- fluidPage(
  titlePanel("PM2.5 Levels Over Time in the USA and India"),
  p("PM2.5 refers to particulate matter with a diameter of less than 2.5 micrometers, which can penetrate deep into the lungs and even enter the bloodstream, causing various health problems. It is caused by sources such as vehicle emissions, industrial processes, power plants, residential heating, agricultural burning, wildfires, construction activities, and natural events like dust storms. . This interactive visualization allows users to explore PM2.5 levels in major cities across the USA and India. By selecting specific countries, cities, and date ranges, users can analyze how air quality has changed over time. The application offers four plot types—line plots to observe trends, bar plots for monthly averages, histograms to understand the distribution of PM2.5 levels, and comparison plots to compare cities in both countries. Annotations for significant events, like the COVID-19 lockdown, provide context to the data, enhancing the analysis of air quality patterns and their correlation with major events. Users can zoom in on specific date ranges, navigate through the data using interactive sliders, and switch between plot types to gain different perspectives. This tool aids in assessing environmental impacts and informing public health policies."),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_country", "Select Country:", 
                  choices = c("US", "IN"), 
                  selected = "US"),
      uiOutput("city_selector"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(data_us$date),
                     end = max(data_us$date),
                     min = min(data_us$date),
                     max = max(data_us$date)),
      checkboxInput("show_trend", "Show Trend Line", value = FALSE),
      radioButtons("plot_type", "Select Plot Type:",
                   choices = list("Line Plot" = "line", 
                                  "Bar Plot (Monthly Average)" = "bar",
                                  "Histogram" = "histogram",
                                  "Comparison Plot" = "comparison")),
      tags$p("Data source: Air Quality Index Network")
    ),
    mainPanel(
      plotlyOutput("mainPlot"),
      plotlyOutput("summaryPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data_selected_country <- reactive({
    if (input$plot_type == "comparison") {
      data
    } else {
      if (input$selected_country == "US") {
        data_us
      } else {
        data_india
      }
    }
  })
  
  output$city_selector <- renderUI({
    selectInput("selected_cities", "Select Cities:", 
                choices = unique(data_selected_country()$city), 
                selected = unique(data_selected_country()$city)[1:5], 
                multiple = TRUE)
  })
  
  filtered_data <- reactive({
    data_selected_country() %>% 
      filter(city %in% input$selected_cities & 
               date >= input$date_range[1] & 
               date <= input$date_range[2])
  })
  
  output$mainPlot <- renderPlotly({
    data_to_plot <- filtered_data()
    
    if (input$plot_type == "line") {
      p <- plot_ly(data_to_plot, x = ~date, y = ~`PM2.5`, color = ~city, type = 'scatter', mode = 'lines+markers',
                   hoverinfo = 'text', 
                   text = ~paste("City: ", city, "<br>Date: ", date, "<br>PM2.5: ", `PM2.5`,
                                 "<br>Country: ", country)) %>%
        layout(title = list(text = "PM2.5 Levels Over Time",
                            font = list(size = 20)),
               xaxis = list(title = "Date", 
                            titlefont = list(size = 18),
                            tickfont = list(size = 14),
                            rangeslider = list(visible = TRUE)),
               yaxis = list(title = "PM2.5 Level (µg/m³)",
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               legend = list(title = list(text = '<b>City</b>'),
                             font = list(size = 14)),
               margin = list(l = 80, r = 80, b = 80, t = 80)) %>%
        add_annotations(
          x = as.Date("2020-03-01"), y = 60, 
          text = "COVID-19 Lockdown Begins", 
          showarrow = TRUE, arrowhead = 2,
          ax = -50, ay = -50
        ) %>%
        add_annotations(
          x = as.Date("2021-01-20"), y = 50, 
          text = "New Administration", 
          showarrow = TRUE, arrowhead = 2,
          ax = -50, ay = -50
        )
      
      if (input$show_trend) {
        p <- p %>% add_trace(x = ~date, y = ~fitted(lm(`PM2.5` ~ date, data = data_to_plot)), type = 'scatter', mode = 'lines', line = list(dash = 'dash'), name = 'Trend Line')
      }
      
    } else if (input$plot_type == "bar") {
      data_to_plot <- data_to_plot %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(city, month, country) %>%
        summarize(mean_PM2.5 = mean(`PM2.5`, na.rm = TRUE))
      
      p <- plot_ly(data_to_plot, x = ~month, y = ~mean_PM2.5, color = ~city, type = 'bar',
                   hoverinfo = 'text', 
                   text = ~paste("City: ", city, "<br>Month: ", month, "<br>Average PM2.5: ", mean_PM2.5,
                                 "<br>Country: ", country)) %>%
        layout(title = list(text = "Average Monthly PM2.5 Levels",
                            font = list(size = 20)),
               xaxis = list(title = "Month", 
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               yaxis = list(title = "Average PM2.5 Level (µg/m³)",
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               legend = list(title = list(text = '<b>City</b>'),
                             font = list(size = 14)),
               margin = list(l = 80, r = 80, b = 80, t = 80))
      
    } else if (input$plot_type == "histogram") {
      p <- plot_ly(data_to_plot, x = ~`PM2.5`, color = ~city, type = 'histogram',
                   hoverinfo = 'text', 
                   text = ~paste("City: ", city, "<br>PM2.5: ", `PM2.5`,
                                 "<br>Country: ", country)) %>%
        layout(title = list(text = "Distribution of PM2.5 Levels",
                            font = list(size = 20)),
               xaxis = list(title = "PM2.5 Level (µg/m³)", 
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               yaxis = list(title = "Count",
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               legend = list(title = list(text = '<b>City</b>'),
                             font = list(size = 14)),
               margin = list(l = 80, r = 80, b = 80, t = 80))
      
    } else if (input$plot_type == "comparison") {
      data_to_plot <- filtered_data()
      data_to_plot <- data_to_plot %>%
        group_by(country, city) %>%
        summarize(mean_PM2.5 = mean(`PM2.5`, na.rm = TRUE))
      
      p <- plot_ly(data_to_plot, x = ~city, y = ~mean_PM2.5, color = ~country, type = 'bar',
                   hoverinfo = 'text', 
                   text = ~paste("City: ", city, "<br>Country: ", country, "<br>Average PM2.5: ", mean_PM2.5)) %>%
        layout(title = list(text = "Comparison of PM2.5 Levels Between Cities in USA and India",
                            font = list(size = 20)),
               xaxis = list(title = "City", 
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               yaxis = list(title = "Average PM2.5 Level (µg/m³)",
                            titlefont = list(size = 18),
                            tickfont = list(size = 14)),
               legend = list(title = list(text = '<b>Country</b>'),
                             font = list(size = 14)),
               margin = list(l = 80, r = 80, b = 80, t = 80))
    }
    
    p
  })
  
  output$summaryPlot <- renderPlotly({
    summary_data <- filtered_data() %>%
      group_by(city) %>%
      summarize(mean_PM2.5 = mean(`PM2.5`, na.rm = TRUE), 
                max_PM2.5 = max(`PM2.5`, na.rm = TRUE), 
                min_PM2.5 = min(`PM2.5`, na.rm = TRUE)) %>%
      gather(key = "stat", value = "value", mean_PM2.5, max_PM2.5, min_PM2.5)
    
    plot_ly(summary_data, x = ~city, y = ~value, type = 'bar', color = ~stat,
            hoverinfo = 'text', text = ~paste("City: ", city, "<br>", stat, ": ", value)) %>%
      layout(title = list(text = "Summary of PM2.5 Levels by City",
                          font = list(size = 20)),
             xaxis = list(title = "City", 
                          titlefont = list(size = 18),
                          tickfont = list(size = 14)),
             yaxis = list(title = "PM2.5 Level (µg/m³)",
                          titlefont = list(size = 18),
                          tickfont = list(size = 14)),
             barmode = 'group',
             legend = list(title = list(text = '<b>Statistic</b>'),
                           font = list(size = 14)),
             margin = list(l = 80, r = 80, b = 80, t = 80))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
