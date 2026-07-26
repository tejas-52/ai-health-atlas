# app.R - Main application entry point for AI Health Atlas
source("global.R")

# Main Application UI
ui <- fluidPage(
  theme = app_theme,
  shinyjs::useShinyjs(),
  
  # Custom Stylesheet Integration
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$title("AI Health Atlas")
  ),
  
  # Main Layout Conditional Router
  uiOutput("app_layout")
)

# Main Application Server
server <- function(input, output, session) {
  
  # 1. Initialize Authentication Server
  user_session <- auth_server("auth")
  
  # 2. Global Data Update Trigger (Shared state for reactive updates on CSV uploads)
  raw_data_trigger <- reactiveVal(0)
  
  # 3. Dynamic Router Layout
  output$app_layout <- renderUI({
    session_data <- user_session()
    
    if (!session_data$logged_in) {
      # Render Login Page
      auth_ui("auth")
    } else {
      # Render Authenticated Dashboard Layout
      role <- session_data$role
      
      # Build Tab Panel items based on Role permissions
      tab_list <- list(
        id = "sidebar_tabs",
        type = "pills",
        
        # Public Dashboard & Map (Viewer, Researcher, Admin)
        tabPanel("Dashboard", 
                 value = "tab_dashboard",
                 icon = icon("chart-pie"),
                 dashboard_ui("dashboard")),
                 
        tabPanel("Spatial Map", 
                 value = "tab_map",
                 icon = icon("map-marked-alt"),
                 map_ui("map"))
      )
      
      # Analytics & Leaderboard (Researcher, Admin)
      if (role %in% c("Researcher", "Admin")) {
        tab_list <- c(tab_list, list(
          tabPanel("Analytics", 
                   value = "tab_analytics",
                   icon = icon("brain"),
                   analytics_ui("analytics"))
        ))
      }
      
      # AI Projections & Chat (Researcher, Admin)
      if (role %in% c("Researcher", "Admin")) {
        tab_list <- c(tab_list, list(
          tabPanel("AI & Forecasting", 
                   value = "tab_prediction",
                   icon = icon("magic"),
                   prediction_ui("prediction"))
        ))
      }
      
      # Admin Console (Admin Only)
      if (role == "Admin") {
        tab_list <- c(tab_list, list(
          tabPanel("Admin Console", 
                   value = "tab_admin",
                   icon = icon("cog"),
                   admin_ui("admin"))
        ))
      }
      
      # Construct Panel Layout
      navbar_layout <- do.call(tabsetPanel, tab_list)
      
      tagList(
        # App Header Branding Bar
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 12px 24px; background-color: #111827; border-bottom: 1px solid rgba(255,255,255,0.08); margin-bottom: 24px;",
          tags$div(
            class = "header-brand",
            tags$h1("AI Health Atlas"),
            tags$span("Dresden Portal")
          ),
          
          tags$div(
            style = "display: flex; align-items: center; gap: 15px;",
            tags$div(
              style = "text-align: right;",
              tags$div(style = "font-weight: 600; font-size: 14px; color: #ffffff;", session_data$full_name),
              tags$div(style = "font-size: 11px; color: #9ca3af;", sprintf("Role: %s", session_data$role))
            ),
            actionButton("logout_btn", "Logout", class = "btn btn-outline-danger btn-sm", 
                         style = "padding: 4px 10px; font-size: 12px; border-radius: 6px;")
          )
        ),
        
        # Tabs and Main Content Area
        tags$div(
          class = "main-content",
          navbar_layout
        )
      )
    }
  })
  
  # 4. Activate Server Modules when user is logged in
  observe({
    session_data <- user_session()
    req(session_data$logged_in)
    
    # Standard dashboard elements
    dashboard_server("dashboard", raw_data_trigger)
    map_server("map", raw_data_trigger)
    
    # Analyst dashboard elements
    if (session_data$role %in% c("Researcher", "Admin")) {
      analytics_server("analytics", raw_data_trigger)
      prediction_server("prediction", raw_data_trigger)
    }
    
    # System administrator elements
    if (session_data$role == "Admin") {
      admin_server("admin", user_session, raw_data_trigger)
    }
  })
  
  # 5. Handle Logout
  observeEvent(input$logout_btn, {
    # Reset auth credentials
    session$reload()
  })
}

# Run Shiny Application
shinyApp(ui = ui, server = server)
