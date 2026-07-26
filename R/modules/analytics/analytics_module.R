# Analytics Module for AI Health Atlas
library(shiny)
library(plotly)
library(DT)
library(dplyr)

# UI Function
analytics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Rankings column
      column(4,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "District Health Leaderboard"),
               tags$div(
                 style = "margin-bottom: 20px;",
                 tags$strong("Top Performing Districts (Health Score)"),
                 tableOutput(ns("table_best_districts"))
               ),
               tags$div(
                 tags$strong("Highest Risk Districts (Risk Score)"),
                 tableOutput(ns("table_worst_districts"))
               ),
               tags$div(
                 style = "font-size: 11px; color: #9ca3af; margin-top: 15px; border-top: 1px solid rgba(255,255,255,0.05); padding-top: 10px;",
                 tags$p("Health Score (0-100) is calculated as a composite index of life expectancy, mental health index, and low prevalence of chronic diseases. Risk Score (0-100) aggregates air pollution, smoking rates, and noise levels.")
               )
             )
      ),
      
      # Correlation and clustering column
      column(8,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Environmental vs Clinical Correlation Matrix"),
               plotlyOutput(ns("heatmap_correlation"), height = "350px")
             )
      )
    ),
    
    fluidRow(
      column(12,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "District Cluster Analysis (K-Means Clustering)"),
               fluidRow(
                 column(3,
                        tags$p("Statistical grouping of districts using K-Means (k=3) based on Socio-Economic, Environmental, and Chronic Disease baselines."),
                        sliderInput(ns("cluster_k"), "Number of Clusters (k)", min = 2, max = 5, value = 3, step = 1),
                        uiOutput(ns("cluster_insights"))
                 ),
                 column(9,
                        plotlyOutput(ns("chart_clusters"), height = "400px")
                 )
               )
             )
      )
    )
  )
}

# Server Function
analytics_server <- function(id, raw_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Base Aggregated Dataset for Analytics (constant year 2025)
    analytics_data <- reactive({
      raw_trigger()
      
      health <- db_get_aggregated_data(years = 2025)
      env <- db_get_environmental_stats(years = 2025)
      
      # Group health data by district
      health_sum <- health %>%
        group_by(district_name) %>%
        summarise(
          pop = sum(cohort_size),
          income = sum(avg_income * cohort_size) / sum(cohort_size),
          life_exp = sum(avg_life_expectancy * cohort_size) / sum(cohort_size),
          obesity = sum(obesity_rate * cohort_size) / sum(cohort_size),
          diabetes = sum(diabetes_rate * cohort_size) / sum(cohort_size),
          heart_disease = sum(heart_disease_rate * cohort_size) / sum(cohort_size),
          smoking = sum(smoking_rate * cohort_size) / sum(cohort_size),
          vaccinated = sum(vaccination_rate * cohort_size) / sum(cohort_size),
          mental_health = sum(avg_mental_health_index * cohort_size) / sum(cohort_size),
          .groups = "drop"
        )
      
      env_sum <- env %>%
        group_by(district_name) %>%
        summarise(
          pm25 = mean(avg_pm25),
          noise = mean(avg_noise_level_db),
          green = mean(avg_green_space_pct),
          water = mean(avg_water_quality_index),
          .groups = "drop"
        )
      
      inner_join(health_sum, env_sum, by = "district_name")
    })
    
    # Calculate Composite Scores
    scored_data <- reactive({
      df <- analytics_data()
      req(df)
      
      # Health Score calculation
      # Min-max scale components
      scale_val <- function(x) {
        if(max(x) == min(x)) return(rep(50, length(x)))
        (x - min(x)) / (max(x) - min(x)) * 100
      }
      
      df <- df %>%
        mutate(
          s_life = scale_val(life_exp),
          s_mental = scale_val(mental_health),
          s_obesity = 100 - scale_val(obesity), # inverted: lower is better
          s_diabetes = 100 - scale_val(diabetes),
          s_heart = 100 - scale_val(heart_disease),
          
          # Weighted sum
          health_score = round((s_life * 0.3) + (s_mental * 0.25) + (s_obesity * 0.15) + (s_diabetes * 0.15) + (s_heart * 0.15), 1),
          
          # Risk Score (High is risky)
          s_pm25 = scale_val(pm25),
          s_noise = scale_val(noise),
          s_smoke = scale_val(smoking),
          risk_score = round((s_pm25 * 0.4) + (s_noise * 0.3) + (s_smoke * 0.3), 1)
        )
      
      df
    })
    
    # 2. Leaderboards
    output$table_best_districts <- renderTable({
      df <- scored_data()
      req(df)
      
      df %>%
        select(district_name, health_score, life_exp) %>%
        arrange(desc(health_score)) %>%
        head(3) %>%
        transmute(
          District = district_name,
          `Health Score` = health_score,
          `Life Expectancy` = sprintf("%.1f yrs", life_exp)
        )
    }, width = "100%", spacing = "s")
    
    output$table_worst_districts <- renderTable({
      df <- scored_data()
      req(df)
      
      df %>%
        select(district_name, risk_score, pm25) %>%
        arrange(desc(risk_score)) %>%
        head(3) %>%
        transmute(
          District = district_name,
          `Risk Score` = risk_score,
          `Avg PM2.5` = sprintf("%.1f ug/m3", pm25)
        )
    }, width = "100%", spacing = "s")
    
    # 3. Heatmap Correlation Matrix
    output$heatmap_correlation <- renderPlotly({
      df <- analytics_data()
      req(df)
      
      # Select columns for correlation
      cor_cols <- df %>%
        select(pm25, noise, green, water, obesity, diabetes, heart_disease, smoking, mental_health)
      
      colnames(cor_cols) <- c("PM2.5", "Noise", "Green Space", "Water Quality", "Obesity", "Diabetes", "Heart Disease", "Smoking", "Mental Health")
      
      cor_mat <- round(cor(cor_cols, use = "pairwise.complete.obs"), 2)
      
      plot_ly(
        x = colnames(cor_cols),
        y = colnames(cor_cols),
        z = cor_mat,
        type = "heatmap",
        colorscale = "RdBu",
        zmin = -1, zmax = 1
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(tickangle = -45, color = "#f3f4f6"),
          yaxis = list(color = "#f3f4f6"),
          margin = list(t = 20, b = 80, l = 80, r = 20)
        )
    })
    
    # 4. K-Means Cluster Plot
    output$chart_clusters <- renderPlotly({
      df <- scored_data()
      k <- input$cluster_k
      req(df, k)
      
      # Standardize data for clustering
      cluster_features <- df %>%
        select(income, life_exp, obesity, smoking, pm25)
      
      scaled_features <- scale(cluster_features)
      
      # Perform K-Means
      set.seed(42) # For stability
      km_res <- kmeans(scaled_features, centers = k, nstart = 25)
      
      df$cluster <- as.factor(km_res$cluster)
      
      # Plot: Income vs. Obesity colored by Cluster
      plot_ly(
        df, 
        x = ~income, 
        y = ~obesity, 
        color = ~cluster,
        colors = c("#10b981", "#0ea5e9", "#f59e0b", "#ec4899", "#8b5cf6")[1:k],
        type = "scatter",
        mode = "markers+text",
        text = ~district_name,
        textposition = "top center",
        marker = list(size = 14, line = list(color = "#ffffff", width = 1.5)),
        hoverinfo = "text",
        hovertext = ~sprintf("<b>%s</b><br/>Cluster: %s<br/>Health Score: %.1f<br/>Income: €%,.0f<br/>Obesity: %.1f%%", 
                            district_name, cluster, health_score, income, obesity)
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Average Household Income (€)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(title = "Obesity Rate (%)", color = "#9ca3af", gridcolor = "rgba(255,255,255,0.05)"),
          legend = list(title = list(text = "Cluster", font = list(color = "#ffffff")), font = list(color = "#f3f4f6")),
          margin = list(t = 20, b = 20, l = 40, r = 20)
        )
    })
    
    # 5. Dynamic Cluster Insights text
    output$cluster_insights <- renderUI({
      df <- scored_data()
      k <- input$cluster_k
      req(df, k)
      
      # Get some descriptive text
      tags$div(
        style = "margin-top: 15px; font-size: 11px; color: #9ca3af; background: rgba(255,255,255,0.02); padding: 8px; border-radius: 6px;",
        tags$strong("Statistical Summary:"),
        tags$p(sprintf("K-Means successfully partitioned the 10 municipal districts into %d distinct health profiles. Select different sizes (k) to refine partitioning thresholds.", k))
      )
    })
  })
}
