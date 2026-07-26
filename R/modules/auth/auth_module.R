# Authentication Module for AI Health Atlas

# UI Function
auth_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "login-container",
    tags$div(
      class = "login-box",
      tags$div(
        class = "header-brand",
        style = "justify-content: center; margin-bottom: 20px;",
        tags$h1("AI Health Atlas"),
        tags$span("v1.0")
      ),
      tags$p("Evidence-based municipal health planning portal"),
      
      textInput(ns("username"), "Username", value = "", placeholder = "e.g., admin"),
      passwordInput(ns("password"), "Password", value = "", placeholder = "••••••••"),
      
      tags$div(
        style = "margin-top: 15px; margin-bottom: 15px;",
        uiOutput(ns("error_msg"))
      ),
      
      actionButton(ns("login_btn"), "Log In", class = "login-btn"),
      
      tags$div(
        style = "margin-top: 25px; font-size: 11px; color: #9ca3af; text-align: left; background: rgba(255,255,255,0.03); padding: 10px; border-radius: 6px; border: 1px solid rgba(255,255,255,0.05);",
        tags$strong("Demo Credentials:"),
        tags$ul(
          style = "margin: 5px 0 0 15px; padding: 0;",
          tags$li("Admin: admin / admin123"),
          tags$li("Researcher: researcher / research123"),
          tags$li("Viewer: viewer / view123")
        )
      )
    )
  )
}

# Server Function
auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store user state
    user_state <- reactiveVal(list(
      logged_in = FALSE,
      username = NULL,
      role = NULL,
      full_name = NULL
    ))
    
    error_text <- reactiveVal("")
    
    output$error_msg <- renderUI({
      if (error_text() != "") {
        tags$div(
          class = "alert alert-danger",
          style = "padding: 8px 12px; font-size: 13px; border-radius: 6px; text-align: left;",
          error_text()
        )
      } else {
        NULL
      }
    })
    
    observeEvent(input$login_btn, {
      req(input$username, input$password)
      
      # Clean input
      username_val <- trimws(input$username)
      password_val <- input$password
      
      # Call DB verification
      user_info <- verify_user_credentials(username_val, password_val)
      
      if (!is.null(user_info)) {
        # Success!
        user_state(list(
          logged_in = TRUE,
          username = user_info$username,
          role = user_info$role,
          full_name = user_info$full_name
        ))
        error_text("")
      } else {
        # Failure
        error_text("Invalid username or password. Please try again.")
      }
    })
    
    # Return user state so the main app can react to it
    return(user_state)
  })
}
