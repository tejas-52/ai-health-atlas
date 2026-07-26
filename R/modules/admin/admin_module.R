# Admin Panel Module for AI Health Atlas
library(shiny)
library(DT)
library(dplyr)

# UI Function
admin_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # User Management Column
      column(6,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "User Access Control"),
               
               # User creation form
               fluidRow(
                 column(6, textInput(ns("new_username"), "Username", placeholder = "e.g., analyst2")),
                 column(6, passwordInput(ns("new_password"), "Password", placeholder = "Min 6 chars"))
               ),
               fluidRow(
                 column(6, textInput(ns("new_fullname"), "Full Name", placeholder = "e.g., Jane Doe")),
                 column(6, selectInput(ns("new_role"), "System Role", choices = c("Viewer", "Researcher", "Admin")))
               ),
               actionButton(ns("create_user_btn"), "Create User Account", class = "btn btn-success", style = "margin-bottom: 20px; width: 100%;"),
               
               tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.05);"),
               
               # Existing Users List
               tags$strong("Existing System Accounts"),
               DTOutput(ns("table_users")),
               br(),
               fluidRow(
                 column(8, numericInput(ns("delete_user_id"), "Enter User ID to Delete", value = NA, min = 1)),
                 column(4, actionButton(ns("delete_user_btn"), "Delete Account", class = "btn btn-danger", style = "margin-top: 25px; width: 100%;"))
               )
             )
      ),
      
      # Data Operations Column
      column(6,
             tags$div(
               class = "glass-panel",
               tags$div(class = "panel-header", "Data Pipeline Pipeline Management"),
               
               tags$strong("Refresh Database from CSV Uploads"),
               tags$p(style = "font-size: 12px; color: #9ca3af; margin-top: 5px;",
                      "Upload raw files corresponding to Demographics, Epidemiology, and Environmental metrics to overwrite active SQLite tables. Column headers must align with standard formats."),
               
               fileInput(ns("upload_demographics"), "Upload Demographics CSV", accept = ".csv"),
               fileInput(ns("upload_epidemiology"), "Upload Epidemiology CSV", accept = ".csv"),
               fileInput(ns("upload_environmental"), "Upload Environmental CSV", accept = ".csv"),
               
               actionButton(ns("refresh_db_btn"), "Validate and Ingest CSV Data", class = "btn btn-warning", style = "width: 100%; font-weight: 600;"),
               
               tags$div(
                 style = "margin-top: 15px; min-height: 40px;",
                 uiOutput(ns("pipeline_status"))
               ),
               
               tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.05);"),
               
               # Exports
               tags$strong("Export Aggregated Reports"),
               tags$p(style = "font-size: 12px; color: #9ca3af;",
                      "Export high-level municipal health statistics for reporting packages."),
               fluidRow(
                 column(6, downloadButton(ns("export_csv"), "Download CSV", class = "btn btn-outline-info", style = "width: 100%; color: #0ea5e9; border-color: #0ea5e9;")),
                 column(6, downloadButton(ns("export_excel"), "Download Excel", class = "btn btn-outline-info", style = "width: 100%; color: #0ea5e9; border-color: #0ea5e9;"))
               )
             )
      )
    )
  )
}

# Server Function
admin_server <- function(id, user_session, raw_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Internal trigger to refresh users table
    users_trigger <- reactiveVal(0)
    
    # Validate active permissions
    is_admin <- reactive({
      req(user_session())
      user_session()$role == "Admin"
    })
    
    # Render Users Table
    output$table_users <- renderDT({
      users_trigger()
      df <- db_get_all_users()
      datatable(
        df %>% select(id, username, role, full_name),
        options = list(pageLength = 5, dom = 'tp', scrollX = TRUE),
        class = 'display compact',
        rownames = FALSE
      )
    })
    
    # Create User Account
    observeEvent(input$create_user_btn, {
      req(is_admin())
      req(input$new_username, input$new_password, input$new_fullname)
      
      username <- trimws(input$new_username)
      password <- input$new_password
      fullname <- trimws(input$new_fullname)
      role <- input$new_role
      
      if (nchar(username) < 3 || nchar(password) < 6) {
        showNotification("Username must be >= 3 chars. Password must be >= 6 chars.", type = "error")
        return()
      }
      
      tryCatch({
        db_add_user(username, password, role, fullname)
        showNotification("Account created successfully!", type = "message")
        users_trigger(users_trigger() + 1)
        
        # Clear inputs
        updateTextInput(session, "new_username", value = "")
        updateTextInput(session, "new_password", value = "")
        updateTextInput(session, "new_fullname", value = "")
      }, error = function(e) {
        showNotification(paste("Account creation failed:", e$message), type = "error")
      })
    })
    
    # Delete User Account
    observeEvent(input$delete_user_btn, {
      req(is_admin())
      req(input$delete_user_id)
      
      user_id <- input$delete_user_id
      
      # Check if self-deleting
      current_user <- user_session()$username
      all_users <- db_get_all_users()
      user_to_delete <- all_users %>% filter(id == user_id)
      
      if (nrow(user_to_delete) == 0) {
        showNotification("User ID not found.", type = "error")
        return()
      }
      
      if (user_to_delete$username[1] == current_user) {
        showNotification("Cannot delete your own active account.", type = "error")
        return()
      }
      
      tryCatch({
        db_delete_user(user_id)
        showNotification("Account deleted.", type = "message")
        users_trigger(users_trigger() + 1)
        updateNumericInput(session, "delete_user_id", value = NA)
      }, error = function(e) {
        showNotification(paste("Deletion failed:", e$message), type = "error")
      })
    })
    
    # Ingest CSV Data
    pipeline_message <- reactiveVal("")
    
    output$pipeline_status <- renderUI({
      msg <- pipeline_message()
      if (msg == "") return(NULL)
      
      if (grepl("Success", msg)) {
        tags$div(class = "alert alert-success", style = "padding: 8px 12px; font-size: 13px;", msg)
      } else {
        tags$div(class = "alert alert-danger", style = "padding: 8px 12px; font-size: 13px;", msg)
      }
    })
    
    observeEvent(input$refresh_db_btn, {
      req(is_admin())
      
      # Ensure all three files are selected
      if (is.null(input$upload_demographics) || is.null(input$upload_epidemiology) || is.null(input$upload_environmental)) {
        pipeline_message("Failure: Please upload all three files (Demographics, Epidemiology, and Environmental) to run validation.")
        return()
      }
      
      pipeline_message("Running validation checks...")
      
      dem_file <- input$upload_demographics$datapath
      epi_file <- input$upload_epidemiology$datapath
      env_file <- input$upload_environmental$datapath
      
      # Perform Schema Validation
      dem_headers <- colnames(read.csv(dem_file, nrows = 1))
      epi_headers <- colnames(read.csv(epi_file, nrows = 1))
      env_headers <- colnames(read.csv(env_file, nrows = 1))
      
      expected_dem <- c("id", "district_name", "year", "age", "gender", "income", "education", "employed", "life_expectancy")
      expected_epi <- c("demographic_id", "diabetes", "heart_disease", "obesity", "smoking", "vaccinated", "mental_health_index")
      expected_env <- c("district_name", "date", "pm25", "temperature", "green_space_pct", "noise_level_db", "water_quality_index")
      
      if (!all(expected_dem %in% dem_headers)) {
        pipeline_message("Validation Failure: Demographics CSV is missing required columns.")
        return()
      }
      if (!all(expected_epi %in% epi_headers)) {
        pipeline_message("Validation Failure: Epidemiology CSV is missing required columns.")
        return()
      }
      if (!all(expected_env %in% env_headers)) {
        pipeline_message("Validation Failure: Environmental CSV is missing required columns.")
        return()
      }
      
      # Execute refresh
      tryCatch({
        db_refresh_data(dem_file, epi_file, env_file)
        pipeline_message("Success: All files passed validations and the SQLite database was refreshed! App metrics reloaded.")
        # Trigger global data update
        raw_trigger(raw_trigger() + 1)
      }, error = function(e) {
        pipeline_message(paste("Database Ingestion Error:", e$message))
      })
    })
    
    # Exports
    output$export_csv <- downloadHandler(
      filename = function() {
        paste("AI-Health-Atlas-Export-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # Export aggregated view
        data <- db_get_aggregated_data()
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    output$export_excel <- downloadHandler(
      filename = function() {
        paste("AI-Health-Atlas-Export-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        data <- db_get_aggregated_data()
        # Fallback to writexl if available, otherwise write CSV disguised or notify
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(data, file)
        } else {
          # Write as CSV if writexl is missing
          write.csv(data, file, row.names = FALSE)
        }
      }
    )
  })
}
