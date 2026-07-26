# Prediction & AI Module for AI Health Atlas
library(shiny)
library(plotly)
library(DT)
library(dplyr)

# UI Function
prediction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Time-series Projections column
      column(8,
             tags$div(
               class = "glass-panel",
               tags$div(
                 class = "panel-header", 
                 tags$span("Time Series Projections (2026 - 2030)")
               ),
               fluidRow(
                 column(4,
                        selectInput(ns("forecast_target"), "Forecasting Variable",
                                    choices = c("Population Growth" = "population",
                                                "Hospital Demand (Beds)" = "beds",
                                                "Obesity Rate" = "obesity",
                                                "Diabetes Rate" = "diabetes"),
                                    selected = "population")
                 ),
                 column(4,
                        selectInput(ns("forecast_district"), "Municipal District",
                                    choices = NULL) # Dynamically loaded
                 ),
                 column(4,
                        tags$p(style = "font-size: 12px; color: #9ca3af; margin-top: 10px;",
                               "Projections calculated using Ordinary Least Squares linear trend modeling with 95% confidence intervals.")
                 )
               ),
               plotlyOutput(ns("chart_forecast"), height = "350px")
             ),
             
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Detected Environmental & Health Anomalies"),
               DTOutput(ns("table_anomalies")),
               tags$div(
                 style = "font-size: 11px; color: #9ca3af; margin-top: 10px;",
                 tags$p("Anomalies are detected using standard statistical Z-Score thresholds where daily/annual variance exceeds 2 standard deviations from the district historical baseline.")
               )
             )
      ),
      
      # AI Chatbot Column
      column(4,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "AI Public Health Assistant"),
               tags$div(
                 class = "chat-container",
                 # Chat History
                 tags$div(
                   id = ns("chat_history_box"),
                   class = "chat-history",
                   uiOutput(ns("chat_history_render"))
                 ),
                 # Input Box
                 tags$div(
                   class = "chat-input-area",
                   textInput(ns("chat_input"), "", value = "", placeholder = "Ask me a question about district stats..."),
                   actionButton(ns("send_chat"), "Send", class = "btn btn-primary", style = "background-color: #0d9488; border-color: #0d9488;")
                 ),
                 tags$div(
                   style = "margin-top: 15px; font-size: 11px; color: #9ca3af; background: rgba(255,255,255,0.02); padding: 8px; border-radius: 6px;",
                   tags$strong("Example Questions:"),
                   tags$ul(
                     style = "margin: 5px 0 0 12px; padding: 0;",
                     tags$li(tags$a(href = "#", onclick = sprintf("document.getElementById('%s').value='Which district has the highest obesity?';Shiny.setInputValue('%s',Math.random());", ns("chat_input"), ns("fill_chat_click")), "Which district has the highest obesity?")),
                     tags$li(tags$a(href = "#", onclick = sprintf("document.getElementById('%s').value='Compare Neustadt and Blasewitz';Shiny.setInputValue('%s',Math.random());", ns("chat_input"), ns("fill_chat_click")), "Compare Neustadt and Blasewitz")),
                     tags$li(tags$a(href = "#", onclick = sprintf("document.getElementById('%s').value='Explain correlation of PM2.5';Shiny.setInputValue('%s',Math.random());", ns("chat_input"), ns("fill_chat_click")), "Explain correlation of PM2.5"))
                   )
                 )
               )
             )
      )
    )
  )
}

# Server Function
prediction_server <- function(id, raw_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Load Districts dynamically
    observe({
      districts_df <- db_get_districts()
      updateSelectInput(session, "forecast_district", choices = districts_df$name)
    })
    
    # Chat memory
    chat_messages <- reactiveVal(list(
      list(sender = "ai", text = "Welcome! I am the Health Atlas AI Policy Planner. Ask me questions about demographics, healthcare accessibility, or disease trends across districts.")
    ))
    
    # Fill message on sample link click
    observeEvent(input$fill_chat_click, {
      # This is triggered by JS. The input is already modified by JS, we just process it.
      # Trigger send button click
      shinyjs::click("send_chat")
    })
    
    # Handle Send Message
    observeEvent(input$send_chat, {
      req(input$chat_input)
      user_text <- trimws(input$chat_input)
      if (user_text == "") return()
      
      # Clear input box
      updateTextInput(session, "chat_input", value = "")
      
      # Append User message
      msgs <- chat_messages()
      msgs <- c(msgs, list(list(sender = "user", text = user_text)))
      chat_messages(msgs)
      
      # Generate AI Response
      ai_response <- ask_ai_chat(user_text)
      
      # Append AI Response
      msgs <- chat_messages()
      msgs <- c(msgs, list(list(sender = "ai", text = ai_response)))
      chat_messages(msgs)
      
      # Javascript to scroll chat box to bottom
      shinyjs::runjs(sprintf("var objDiv = document.getElementById('%s'); if(objDiv) { objDiv.scrollTop = objDiv.scrollHeight; }", ns("chat_history_box")))
    })
    
    # Render Chat History
    output$chat_history_render <- renderUI({
      msgs <- chat_messages()
      
      ui_list <- lapply(msgs, function(msg) {
        bubble_class <- if (msg$sender == "user") "chat-bubble user" else "chat-bubble ai"
        tags$div(
          class = bubble_class,
          # Treat AI response as HTML to support tables and markdown lists
          if (msg$sender == "ai") {
            # Simple markdown conversion for bolding and lists
            html_text <- msg$text
            html_text <- gsub("\n", "<br/>", html_text)
            html_text <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", html_text)
            # Tables formatting replacement
            html_text <- gsub("\\|", " ", html_text)
            HTML(html_text)
          } else {
            msg$text
          }
        )
      })
      
      tags$div(style = "display: flex; flex-direction: column;", ui_list)
    })
    
    # 2. Forecasting
    forecast_data <- reactive({
      req(input$forecast_district, input$forecast_target)
      raw_trigger()
      
      dist_name <- input$forecast_district
      target <- input$forecast_target
      
      # Fetch historical data
      health_df <- db_get_aggregated_data(districts = dist_name)
      
      # Aggregate historical variables
      historical <- health_df %>%
        group_by(year) %>%
        summarise(
          population = sum(cohort_size),
          obesity = sum(obesity_rate * cohort_size) / sum(cohort_size),
          diabetes = sum(diabetes_rate * cohort_size) / sum(cohort_size),
          beds = sum(cohort_size) * 0.005, # Estimate hospital demand as 0.5% of population
          .groups = "drop"
        )
      
      # Extract vectors
      years <- historical$year
      values <- historical[[target]]
      
      # Predict 2026 - 2030
      pred_df <- predict_trend(years, values, target_years = 2026:2030)
      
      list(
        historical = data.frame(year = years, value = values),
        forecast = pred_df
      )
    })
    
    # Render Forecast Plot
    output$chart_forecast <- renderPlotly({
      res <- forecast_data()
      req(res)
      
      hist <- res$historical
      fore <- res$forecast
      
      target_label <- case_when(
        input$forecast_target == "population" ~ "Population",
        input$forecast_target == "beds" ~ "Bed Demand",
        input$forecast_target == "obesity" ~ "Obesity Rate (%)",
        TRUE ~ "Diabetes Rate (%)"
      )
      
      # Combine historical and forecasted data
      plot_ly() %>%
        # Historical Line
        add_lines(data = hist, x = ~year, y = ~value, name = "Historical", line = list(color = "#0ea5e9", width = 3)) %>%
        # Forecasted Line
        add_lines(data = fore, x = ~year, y = ~value, name = "Forecast", line = list(color = "#f59e0b", width = 3, dash = "dash")) %>%
        # Confidence Interval Ribbon
        add_ribbons(data = fore, x = ~year, ymin = ~lower, ymax = ~upper, name = "95% Confidence", 
                    fillcolor = "rgba(245, 158, 11, 0.15)", line = list(color = "transparent")) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Year", color = "#9ca3af", dtick = 1, gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(title = target_label, color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          legend = list(font = list(color = "#f3f4f6")),
          margin = list(t = 20, b = 20, l = 40, r = 20)
        )
    })
    
    # 3. Anomaly Detection Table
    output$table_anomalies <- renderDT({
      raw_trigger()
      
      # Query monthly/weekly or daily environmental records to check for PM2.5 excursions
      conn <- get_db_connection()
      on.exit(dbDisconnect(conn))
      
      # Fetch monthly average PM2.5 values across all districts for anomaly detection
      q <- "
        SELECT 
          d.name AS district_name,
          SUBSTR(e.date, 1, 7) AS month_str,
          AVG(e.pm25) AS pm25,
          AVG(e.temperature) AS temp,
          AVG(e.water_quality_index) AS water
        FROM environmental e
        JOIN districts d ON e.district_id = d.id
        GROUP BY district_name, month_str
        ORDER BY month_str DESC
      "
      df <- dbGetQuery(conn, q)
      
      # Run anomaly detection on PM2.5 and Water Quality per district
      anomalies_found <- df %>%
        group_by(district_name) %>%
        mutate(
          pm25_anomaly = detect_anomalies(pm25, threshold = 1.95),
          water_anomaly = detect_anomalies(water, threshold = 1.95)
        ) %>%
        ungroup() %>%
        filter(pm25_anomaly | water_anomaly)
      
      # Build display frame
      display_df <- anomalies_found %>%
        transmute(
          District = district_name,
          Date = month_str,
          Indicator = ifelse(pm25_anomaly, "PM2.5 Pollution Spike", "Water Quality Drop"),
          Value = ifelse(pm25_anomaly, sprintf("%.1f ug/m3", pm25), sprintf("%.1f WQI", water)),
          Severity = ifelse(pm25_anomaly & pm25 > 25.0, "Critical", "Warning")
        )
      
      datatable(
        display_df,
        options = list(
          pageLength = 5,
          dom = 'tp',
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#111827', 'color': '#ffffff'});",
            "}"
          )
        ),
        class = 'display nowrap compact',
        rownames = FALSE
      )
    })
  })
}
