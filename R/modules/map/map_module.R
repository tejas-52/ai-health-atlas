# Map Module for AI Health Atlas
library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Helper function to generate synthetic district boundaries (sf object)
get_synthetic_district_shapes <- function() {
  # Centers for Dresden districts
  centers <- list(
    "Altstadt"  = c(51.050, 13.737),
    "Neustadt"  = c(51.068, 13.748),
    "Pieschen"  = c(51.082, 13.712),
    "Klotzsche" = c(51.118, 13.774),
    "Loschwitz" = c(51.065, 13.825),
    "Blasewitz" = c(51.045, 13.805),
    "Plauen"    = c(51.022, 13.712),
    "Cotta"     = c(51.052, 13.678),
    "Prohlis"   = c(50.995, 13.785),
    "Leuben"    = c(51.012, 13.828)
  )
  
  polys <- list()
  for (name in names(centers)) {
    lat <- centers[[name]][1]
    lon <- centers[[name]][2]
    
    # Create a small hexagonal or rectangular block around each center
    # Adjust offsets slightly to make them look like styled administrative zones
    offset_lat <- 0.009
    offset_lon <- 0.013
    
    p <- matrix(c(
      lon - offset_lon, lat - offset_lat,
      lon + offset_lon, lat - offset_lat,
      lon + offset_lon * 1.5, lat,
      lon + offset_lon, lat + offset_lat,
      lon - offset_lon, lat + offset_lat,
      lon - offset_lon * 1.5, lat,
      lon - offset_lon, lat - offset_lat # Close polygon
    ), ncol = 2, byrow = TRUE)
    
    polys[[name]] <- st_polygon(list(p))
  }
  
  sfc <- st_sfc(polys, crs = 4326)
  sf_df <- st_sf(district_name = names(centers), geometry = sfc)
  return(sf_df)
}

# UI Function
map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             tags$div(
               class = "filter-panel",
               tags$div(class = "filter-title", icon("map-marked-alt"), "Map Layers"),
               
               selectInput(ns("map_metric"), "Overlay Metric",
                           choices = c(
                             "Population Density" = "population",
                             "Life Expectancy" = "life_exp",
                             "Air Pollution (PM2.5)" = "pm25",
                             "Obesity Rate" = "obesity",
                             "Diabetes Rate" = "diabetes",
                             "Average Income" = "income"
                           ),
                           selected = "population"),
               
               checkboxGroupInput(ns("map_facilities"), "Healthcare Facilities",
                                  choices = c("Hospital", "Clinic", "Specialist"),
                                  selected = c("Hospital", "Clinic", "Specialist")),
               
               tags$div(
                 style = "margin-top: 15px; font-size: 12px; color: #9ca3af;",
                 tags$strong("Interactive Map:"),
                 tags$p("Click on district boundaries to view aggregate parameters, or click hospital markers for capacity details.")
               )
             )
      ),
      column(9,
             tags$div(
               class = "glass-panel",
               style = "padding: 8px;",
               leafletOutput(ns("leaflet_map"), height = "650px")
             )
      )
    )
  )
}

# Server Function
map_server <- function(id, raw_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Fetch data for mapping
    mapping_data <- reactive({
      raw_trigger()
      
      # We want the latest year (2025) stats for map rendering
      agg_health <- db_get_aggregated_data(years = 2025)
      agg_env <- db_get_environmental_stats(years = 2025)
      
      # Merge
      merged <- agg_health %>%
        group_by(district_name) %>%
        summarise(
          population = sum(cohort_size),
          life_exp = sum(avg_life_expectancy * cohort_size) / sum(cohort_size),
          income = sum(avg_income * cohort_size) / sum(cohort_size),
          obesity = sum(obesity_rate * cohort_size) / sum(cohort_size),
          diabetes = sum(diabetes_rate * cohort_size) / sum(cohort_size),
          .groups = "drop"
        )
      
      env_sum <- agg_env %>%
        group_by(district_name) %>%
        summarise(pm25 = mean(avg_pm25), .groups = "drop")
      
      final_df <- left_join(merged, env_sum, by = "district_name")
      return(final_df)
    })
    
    # 2. Fetch hospital locations
    hospital_data <- reactive({
      raw_trigger()
      db_get_healthcare_resources(facility_types = input$map_facilities)
    })
    
    # 3. Base Leaflet Map
    output$leaflet_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter, 
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lng = 13.75, lat = 51.055, zoom = 12)
    })
    
    # 4. Reactive updates for overlays
    observe({
      df <- mapping_data()
      hospitals <- hospital_data()
      metric <- input$map_metric
      
      req(df, metric)
      
      # Create sf object merged with data
      shapes <- get_synthetic_district_shapes()
      map_sf <- left_join(shapes, df, by = "district_name")
      
      # Prepare palette
      val_vector <- map_sf[[metric]]
      
      # Determine palette colors based on metric
      if (metric == "pm25" || metric == "obesity" || metric == "diabetes") {
        # High is bad: Orange/Red scale
        palette_colors <- "OrRd"
      } else {
        # High is good: Viridis/Teal/Green scale
        palette_colors <- "YlGnBu"
      }
      
      pal <- colorNumeric(palette = palette_colors, domain = val_vector, na.color = "#333333")
      
      # Setup Popups
      popups <- sprintf(
        "<strong>District: %s</strong><br/>
         Population: %s<br/>
         Avg Income: €%,.0f<br/>
         Life Expectancy: %.1f yrs<br/>
         PM2.5: %.1f ug/m3<br/>
         Obesity Rate: %.1f%%<br/>
         Diabetes Rate: %.1f%%",
        map_sf$district_name,
        format(map_sf$population, big.mark = ","),
        map_sf$income,
        map_sf$life_exp,
        map_sf$pm25,
        map_sf$obesity,
        map_sf$diabetes
      )
      
      # Update Leaflet Map
      leafletProxy("leaflet_map", data = map_sf) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor = ~pal(val_vector),
          weight = 1.5,
          opacity = 0.8,
          color = "#ffffff",
          dashArray = "3",
          fillOpacity = 0.6,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#0d9488",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE
          ),
          popup = popups
        ) %>%
        addLegend(
          pal = pal,
          values = val_vector,
          opacity = 0.7,
          title = paste("Overlay:", tools::toTitleCase(metric)),
          position = "bottomright"
        )
      
      # Update Markers for Hospitals/Facilities
      leafletProxy("leaflet_map") %>% clearMarkers()
      
      if (nrow(hospitals) > 0) {
        # Determine marker color based on type
        hospitals <- hospitals %>%
          mutate(
            marker_color = case_when(
              type == "Hospital" ~ "red",
              type == "Clinic" ~ "blue",
              TRUE ~ "green"
            )
          )
        
        # We can use simple circle markers for speed and compatibility
        leafletProxy("leaflet_map", data = hospitals) %>%
          addCircleMarkers(
            lng = ~longitude,
            lat = ~latitude,
            radius = 8,
            color = ~case_when(
              type == "Hospital" ~ "#ef4444",
              type == "Clinic" ~ "#3b82f6",
              TRUE ~ "#10b981"
            ),
            fillColor = ~case_when(
              type == "Hospital" ~ "#f87171",
              type == "Clinic" ~ "#60a5fa",
              TRUE ~ "#34d399"
            ),
            fillOpacity = 0.8,
            weight = 1.5,
            popup = ~sprintf(
              "<strong>%s</strong><br/>
               Type: %s<br/>
               Beds: %d<br/>
               Doctors: %d<br/>
               Emergency Response: %.1f mins<br/>
               District: %s",
              name, type, beds, doctors, emergency_response_time_min, district_name
            )
          )
      }
    })
  })
}
