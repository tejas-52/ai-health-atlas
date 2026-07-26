# Dashboard Module for AI Health Atlas
library(shiny)
library(plotly)
library(DT)
library(dplyr)

# UI Function
dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Filters Row
    tags$div(
      class = "filter-panel",
      tags$div(
        class = "filter-title",
        icon("sliders-h"),
        "Global Filters"
      ),
      fluidRow(
        column(3, 
               selectizeInput(ns("filter_districts"), "Select Districts", 
                              choices = NULL, multiple = TRUE, 
                              options = list(placeholder = "All Districts"))
        ),
        column(3, 
               selectizeInput(ns("filter_years"), "Select Years", 
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All Years"))
        ),
        column(3, 
               checkboxGroupInput(ns("filter_gender"), "Gender", 
                                  choices = c("Male", "Female"), 
                                  selected = c("Male", "Female"), 
                                  inline = TRUE)
        ),
        column(3, 
               checkboxGroupInput(ns("filter_age_groups"), "Age Groups", 
                                  choices = c("0-17", "18-34", "35-49", "50-64", "65+"), 
                                  selected = c("0-17", "18-34", "35-49", "50-64", "65+"), 
                                  inline = TRUE)
        )
      )
    ),
    
    # KPIs Row (using our beautiful custom CSS)
    fluidRow(
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Total Analyzed Population"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_pop"))),
               tags$div(class = "kpi-icon", icon("users"))
             )
      ),
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Life Expectancy (Avg)"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_life_exp"))),
               tags$div(class = "kpi-icon", icon("heartbeat"))
             )
      ),
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Average Household Income"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_income"))),
               tags$div(class = "kpi-icon", icon("euro-sign"))
             )
      ),
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Average Mental Health"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_mental_health"))),
               tags$div(class = "kpi-icon", icon("brain"))
             )
      )
    ),
    br(),
    fluidRow(
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Obesity Rate"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_obesity"))),
               tags$div(class = "kpi-icon", icon("weight"))
             )
      ),
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Diabetes Rate"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_diabetes"))),
               tags$div(class = "kpi-icon", icon("syringe"))
             )
      ),
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Smoking Rate"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_smoking"))),
               tags$div(class = "kpi-icon", icon("smoking"))
             )
      ),
      column(3, 
             tags$div(
               class = "kpi-card",
               tags$div(class = "kpi-title", "Vaccination Rate"),
               tags$div(class = "kpi-value", textOutput(ns("kpi_vaccination"))),
               tags$div(class = "kpi-icon", icon("shield-virus"))
             )
      )
    ),
    
    br(),
    
    # Charts Grid
    fluidRow(
      column(6, 
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Disease Trend Over Time"),
               plotlyOutput(ns("chart_disease_trends"), height = "350px")
             )
      ),
      column(6, 
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Life Expectancy vs. Average Income by District"),
               plotlyOutput(ns("chart_income_vs_life"), height = "350px")
             )
      )
    ),
    
    fluidRow(
      column(6, 
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Health Indicator Comparisons across Districts"),
               plotlyOutput(ns("chart_district_bars"), height = "350px")
             )
      ),
      column(6, 
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Mental Health Score Distribution by Age Group"),
               plotlyOutput(ns("chart_mental_box"), height = "350px")
             )
      )
    ),
    
    # Detailed Data Preview
    fluidRow(
      column(12,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Micro-Level Health Records (Sample Preview)"),
               DTOutput(ns("table_raw_preview"))
             )
      )
    )
  )
}

# Server Function
dashboard_server <- function(id, raw_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Dynamic choice updates from database
    observe({
      districts_df <- db_get_districts()
      updateSelectizeInput(session, "filter_districts", choices = districts_df$name, server = TRUE)
      
      # Hardcoded or fetched years
      updateSelectizeInput(session, "filter_years", choices = c(2020, 2021, 2022, 2023, 2024, 2025), selected = 2025, server = TRUE)
    })
    
    # Reactive aggregated cohort data
    dashboard_data <- reactive({
      # Require a year to prevent rendering huge dataset
      req(input$filter_gender, input$filter_age_groups)
      
      # Trigger when data is refreshed in admin
      raw_trigger()
      
      db_get_aggregated_data(
        districts = input$filter_districts,
        years = input$filter_years,
        genders = input$filter_gender,
        age_groups = input$filter_age_groups
      )
    })
    
    # 1. Render KPIs
    output$kpi_pop <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("0")
      format(sum(df$cohort_size), big.mark = ",")
    })
    
    output$kpi_life_exp <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      # Weighted average life expectancy based on cohort sizes
      val <- sum(df$avg_life_expectancy * df$cohort_size) / sum(df$cohort_size)
      sprintf("%.1f yrs", val)
    })
    
    output$kpi_income <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      val <- sum(df$avg_income * df$cohort_size) / sum(df$cohort_size)
      sprintf("€%,.0f", val)
    })
    
    output$kpi_mental_health <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      val <- sum(df$avg_mental_health_index * df$cohort_size) / sum(df$cohort_size)
      sprintf("%.1f/100", val)
    })
    
    output$kpi_obesity <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      val <- sum(df$obesity_rate * df$cohort_size) / sum(df$cohort_size)
      sprintf("%.1f%%", val)
    })
    
    output$kpi_diabetes <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      val <- sum(df$diabetes_rate * df$cohort_size) / sum(df$cohort_size)
      sprintf("%.1f%%", val)
    })
    
    output$kpi_smoking <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      val <- sum(df$smoking_rate * df$cohort_size) / sum(df$cohort_size)
      sprintf("%.1f%%", val)
    })
    
    output$kpi_vaccination <- renderText({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      val <- sum(df$vaccination_rate * df$cohort_size) / sum(df$cohort_size)
      sprintf("%.1f%%", val)
    })
    
    # 2. Charts
    
    # Line chart: Disease trends over time
    output$chart_disease_trends <- renderPlotly({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return(plotly_empty())
      
      # Group by year
      yearly <- df %>%
        group_by(year) %>%
        summarise(
          Obesity = sum(obesity_rate * cohort_size) / sum(cohort_size),
          Diabetes = sum(diabetes_rate * cohort_size) / sum(cohort_size),
          Heart_Disease = sum(heart_disease_rate * cohort_size) / sum(cohort_size),
          .groups = "drop"
        )
      
      plot_ly(yearly, x = ~year) %>%
        add_lines(y = ~Obesity, name = "Obesity", line = list(color = "#f43f5e", width = 3)) %>%
        add_lines(y = ~Diabetes, name = "Diabetes", line = list(color = "#0ea5e9", width = 3)) %>%
        add_lines(y = ~Heart_Disease, name = "Heart Disease", line = list(color = "#10b981", width = 3)) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Year", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(title = "Rate (%)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          legend = list(font = list(color = "#f3f4f6")),
          margin = list(t = 20, b = 20, l = 40, r = 20)
        )
    })
    
    # Scatter: Income vs. Life expectancy
    output$chart_income_vs_life <- renderPlotly({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return(plotly_empty())
      
      # Group by district
      district_summary <- df %>%
        group_by(district_name) %>%
        summarise(
          income = sum(avg_income * cohort_size) / sum(cohort_size),
          life_exp = sum(avg_life_expectancy * cohort_size) / sum(cohort_size),
          cohort_size = sum(cohort_size),
          .groups = "drop"
        )
      
      plot_ly(
        district_summary, 
        x = ~income, 
        y = ~life_exp, 
        type = 'scatter', 
        mode = 'markers+text',
        text = ~district_name,
        textposition = 'top center',
        size = ~cohort_size,
        marker = list(
          color = '#0d9488', 
          opacity = 0.8,
          line = list(color = 'rgba(255,255,255,0.2)', width = 1)
        ),
        hoverinfo = "text",
        hovertext = ~sprintf("<b>%s</b><br>Avg Income: €%,.0f<br>Life Exp: %.1f yrs<br>Sample Size: %s", 
                            district_name, income, life_exp, format(cohort_size, big.mark=","))
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Average Household Income (€)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(title = "Life Expectancy (Years)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          margin = list(t = 20, b = 20, l = 40, r = 20)
        )
    })
    
    # Bar Chart: Comparing health indicators
    output$chart_district_bars <- renderPlotly({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return(plotly_empty())
      
      district_summary <- df %>%
        group_by(district_name) %>%
        summarise(
          Obesity = sum(obesity_rate * cohort_size) / sum(cohort_size),
          Smoking = sum(smoking_rate * cohort_size) / sum(cohort_size),
          .groups = "drop"
        ) %>%
        arrange(desc(Obesity))
      
      plot_ly(district_summary, x = ~district_name) %>%
        add_bars(y = ~Obesity, name = "Obesity Rate", marker = list(color = "#0ea5e9")) %>%
        add_bars(y = ~Smoking, name = "Smoking Rate", marker = list(color = "#f43f5e")) %>%
        layout(
          barmode = 'group',
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "District", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(title = "Rate (%)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          legend = list(font = list(color = "#f3f4f6")),
          margin = list(t = 20, b = 20, l = 40, r = 20)
        )
    })
    
    # Box plot: Mental health score by age group
    output$chart_mental_box <- renderPlotly({
      df <- dashboard_data()
      if (is.null(df) || nrow(df) == 0) return(plotly_empty())
      
      # Note: Since the DB returned cohort aggregated data, we can display the average mental health index by age group and district
      # To look like a beautiful boxplot, we'll plot the distribution of average mental health indexes across districts/cohorts per age group
      plot_ly(
        df, 
        y = ~avg_mental_health_index, 
        color = ~age_group, 
        type = "box",
        colors = c("#10b981", "#0ea5e9", "#f59e0b", "#ec4899", "#8b5cf6")
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Age Group", color = "#9ca3af"),
          yaxis = list(title = "Mental Health Index (Avg)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          legend = list(font = list(color = "#f3f4f6")),
          margin = list(t = 20, b = 20, l = 40, r = 20)
        )
    })
    
    # DT table preview
    output$table_raw_preview <- renderDT({
      raw_trigger()
      df <- db_get_raw_dashboard_data(limit = 1500)
      
      # Select display columns and translate 0/1 to Yes/No
      df_display <- df %>%
        transmute(
          ID = id,
          District = district_name,
          Year = year,
          Age = age,
          Gender = gender,
          Income = sprintf("€%,.0f", income),
          Employed = ifelse(employed == 1, "Yes", "No"),
          `Life Exp` = life_expectancy,
          Diabetes = ifelse(diabetes == 1, "Yes", "No"),
          `Heart Disease` = ifelse(heart_disease == 1, "Yes", "No"),
          Obese = ifelse(obesity == 1, "Yes", "No"),
          Smoker = ifelse(smoking == 1, "Yes", "No"),
          Vaccinated = ifelse(vaccinated == 1, "Yes", "No"),
          `Mental Health` = mental_health_index
        )
      
      datatable(
        df_display,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50),
          scrollX = TRUE,
          dom = 'lftipr',
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
