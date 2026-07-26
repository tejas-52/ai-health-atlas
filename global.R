# global.R - Environment setup for AI Health Atlas
library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(sf)
library(shinyjs)

# Print startup information
message("Initializing AI Health Atlas Application...")

# Source all utils and modules
source("R/utils/db_helpers.R")
source("R/utils/ai_helpers.R")
source("R/modules/auth/auth_module.R")
source("R/modules/dashboard/dashboard_module.R")
source("R/modules/map/map_module.R")
source("R/modules/analytics/analytics_module.R")
source("R/modules/prediction/prediction_module.R")
source("R/modules/admin/admin_module.R")

# Configure premium Dark Mode bslib theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#0d9488",      # Custom Teal Accent
  bg = "#0b0f19",           # Match CSS bg-primary
  fg = "#f3f4f6",           # Match CSS text-primary
  success = "#10b981",      # Emerald
  warning = "#f59e0b",      # Amber
  danger = "#ef4444",       # Rose
  base_font = font_google("Inter")
)

# Test database connection on startup
tryCatch({
  conn <- get_db_connection()
  dist_count <- dbGetQuery(conn, "SELECT COUNT(*) AS count FROM districts")$count[1]
  dbDisconnect(conn)
  message(sprintf("Database connected successfully. Found %d districts.", dist_count))
}, error = function(e) {
  warning("Database connection failed on startup: ", e$message)
})
