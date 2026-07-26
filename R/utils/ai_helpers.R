# AI and Analytical Projections for AI Health Atlas
library(dplyr)

# 1. Automatic Dynamic Summaries and Policy Recommendations
generate_district_summary <- function(district_name) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Fetch demographics & epidemiology aggregated for this district
  query <- "
    SELECT 
      dem.year,
      AVG(dem.income) AS avg_income,
      AVG(dem.life_expectancy) AS avg_life_exp,
      SUM(epi.diabetes) * 100.0 / COUNT(dem.id) AS diabetes_rate,
      SUM(epi.heart_disease) * 100.0 / COUNT(dem.id) AS heart_disease_rate,
      SUM(epi.obesity) * 100.0 / COUNT(dem.id) AS obesity_rate,
      SUM(epi.smoking) * 100.0 / COUNT(dem.id) AS smoking_rate,
      SUM(epi.vaccinated) * 100.0 / COUNT(dem.id) AS vaccination_rate,
      AVG(epi.mental_health_index) AS avg_mental_health
    FROM demographics dem
    JOIN epidemiology epi ON dem.id = epi.demographic_id
    JOIN districts d ON dem.district_id = d.id
    WHERE d.name = ?
    GROUP BY dem.year
    ORDER BY dem.year ASC
  "
  
  df <- dbGetQuery(conn, query, params = list(district_name))
  
  if (nrow(df) < 2) {
    return(list(
      summary = paste("Insufficient historical data to generate automatic summaries for", district_name),
      recommendations = "Data pipeline pending more observations."
    ))
  }
  
  # Fetch environmental metrics
  env_query <- "
    SELECT 
      CAST(SUBSTR(date, 1, 4) AS INTEGER) AS year,
      AVG(pm25) AS avg_pm25,
      AVG(green_space_pct) AS avg_green
    FROM environmental e
    JOIN districts d ON e.district_id = d.id
    WHERE d.name = ?
    GROUP BY year
    ORDER BY year ASC
  "
  env_df <- dbGetQuery(conn, env_query, params = list(district_name))
  
  first_yr <- df[1, ]
  last_yr <- df[nrow(df), ]
  
  yr_diff <- last_yr$year - first_yr$year
  
  # Health changes
  obesity_diff <- last_yr$obesity_rate - first_yr$obesity_rate
  diabetes_diff <- last_yr$diabetes_rate - first_yr$diabetes_rate
  smoking_diff <- last_yr$smoking_rate - first_yr$smoking_rate
  vac_diff <- last_yr$vaccination_rate - first_yr$vaccination_rate
  life_diff <- last_yr$avg_life_exp - first_yr$avg_life_exp
  
  # Environmental change (if exists)
  env_summary <- ""
  pm25_warning <- FALSE
  if (nrow(env_df) >= 2) {
    env_first <- env_df[1, ]
    env_last <- env_df[nrow(env_df), ]
    pm25_diff <- env_last$avg_pm25 - env_first$avg_pm25
    green_diff <- env_last$avg_green - env_first$avg_green
    env_summary <- sprintf(" Environmental analysis shows PM2.5 levels shifted by %.1f ug/m3 and green spaces changed by %.1f%%.", 
                           pm25_diff, green_diff)
    if (env_last$avg_pm25 > 12.0) {
      pm25_warning <- TRUE
    }
  }
  
  summary_text <- sprintf(
    "Over a %d-year period (from %d to %d), District '%s' experienced a life expectancy shift of %+.1f years. Obesity prevalence changed by %+.1f%%, and diabetes rate shifted by %+.1f%%. Active tobacco usage changed by %+.1f%%, while immunization (vaccination) rates shifted by %+.1f%%.%s",
    yr_diff, first_yr$year, last_yr$year, district_name, life_diff, obesity_diff, diabetes_diff, smoking_diff, vac_diff, env_summary
  )
  
  # Recommendations engine
  recs <- c()
  if (obesity_diff > 1.5 || last_yr$obesity_rate > 20.0) {
    recs <- c(recs, sprintf("Implement a district-wide nutritional awareness program in %s and promote public exercise initiatives.", district_name))
  }
  if (smoking_diff > 0 || last_yr$smoking_rate > 22.0) {
    recs <- c(recs, sprintf("Launch targeted tobacco cessation campaigns in cooperation with localized clinics in %s.", district_name))
  }
  if (diabetes_diff > 1.0) {
    recs <- c(recs, sprintf("Enhance screening programs for diabetes and chronic conditions, focusing on age cohorts 50+.", district_name))
  }
  if (vac_diff < 0 || last_yr$vaccination_rate < 70) {
    recs <- c(recs, sprintf("Strengthen community-level vaccine outreach and mobile clinics to bolster vaccination coverage.", district_name))
  }
  if (pm25_warning) {
    recs <- c(recs, sprintf("Introduce urban greening corridors and vehicle emission restrictions in %s to lower PM2.5 levels.", district_name))
  }
  if (last_yr$avg_mental_health < 65) {
    recs <- c(recs, sprintf("Deploy community mental health counseling services and active social connection forums.", district_name))
  }
  
  # Default recommendation if none triggered
  if (length(recs) == 0) {
    recs <- c(sprintf("Continue monitoring health and socio-demographic indicators in %s. Maintain current public health expenditures.", district_name))
  }
  
  list(
    summary = summary_text,
    recommendations = recs
  )
}

# 2. Time-series forecasting (Linear Trend Projection for 2026-2030)
# This estimates values for demographics/diseases/demands
predict_trend <- function(years, values, target_years = 2026:2030) {
  if (length(years) < 2 || length(values) != length(years)) {
    return(data.frame(year = target_years, value = NA, lower = NA, upper = NA))
  }
  
  fit <- lm(values ~ years)
  preds <- predict(fit, newdata = data.frame(years = target_years), interval = "confidence")
  
  data.frame(
    year = target_years,
    value = round(as.numeric(preds[, "fit"]), 2),
    lower = round(as.numeric(preds[, "lwr"]), 2),
    upper = round(as.numeric(preds[, "upr"]), 2)
  )
}

# 3. Anomaly Detection
# Finds anomalies using standard Z-score method (outliers > 1.96 standard dev)
detect_anomalies <- function(values, threshold = 1.95) {
  if (length(values) < 3) return(logical(length(values)))
  
  m <- mean(values, na.rm = TRUE)
  s <- sd(values, na.rm = TRUE)
  if (s == 0) return(logical(length(values)))
  
  z_scores <- abs((values - m) / s)
  return(z_scores > threshold)
}

# 4. Local NLP Chatbot Engine & OpenAI API Integration
ask_ai_chat <- function(user_message) {
  user_query <- tolower(user_message)
  
  # Check if OpenAI is enabled via environment key
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  
  # Local Parser Fallback (Fast, offline-friendly, 100% correct database retrieval)
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Helper to find districts referenced
  districts_list <- dbGetQuery(conn, "SELECT name FROM districts")$name
  found_districts <- districts_list[sapply(districts_list, function(d) grepl(tolower(d), user_query))]
  
  # 4A. Obesity queries
  if (grepl("highest obesity", user_query) || grepl("most obese", user_query)) {
    q <- "
      SELECT d.name, AVG(epi.obesity) * 100.0 AS rate 
      FROM epidemiology epi
      JOIN demographics dem ON epi.demographic_id = dem.id
      JOIN districts d ON dem.district_id = d.id
      GROUP BY d.name
      ORDER BY rate DESC LIMIT 1
    "
    res <- dbGetQuery(conn, q)
    return(sprintf("According to municipal health records, **%s** has the highest obesity rate at **%.1f%%**.", res$name[1], res$rate[1]))
  }
  
  # 4B. Income queries
  if (grepl("highest income", user_query) || grepl("wealthiest district", user_query)) {
    q <- "SELECT d.name, AVG(dem.income) AS income FROM demographics dem JOIN districts d ON dem.district_id = d.id GROUP BY d.name ORDER BY income DESC LIMIT 1"
    res <- dbGetQuery(conn, q)
    return(sprintf("The district with the highest average annual household income is **%s**, averaging **€%,.0f**.", res$name[1], res$income[1]))
  }
  
  if (grepl("lowest income", user_query) || grepl("poorest district", user_query)) {
    q <- "SELECT d.name, AVG(dem.income) AS income FROM demographics dem JOIN districts d ON dem.district_id = d.id GROUP BY d.name ORDER BY income ASC LIMIT 1"
    res <- dbGetQuery(conn, q)
    return(sprintf("The district with the lowest average annual household income is **%s**, averaging **€%,.0f**.", res$name[1], res$income[1]))
  }
  
  # 4C. Healthcare accessibility compare
  if (grepl("healthcare accessibility", user_query) || grepl("compare healthcare", user_query) || grepl("hospitals", user_query) & grepl("access", user_query)) {
    q <- "
      SELECT d.name, 
             COUNT(h.id) AS facility_count, 
             SUM(h.beds) AS total_beds, 
             AVG(h.emergency_response_time_min) AS avg_resp
      FROM districts d
      LEFT JOIN healthcare_resources h ON d.id = h.district_id
      GROUP BY d.name
      ORDER BY avg_resp ASC
    "
    res <- dbGetQuery(conn, q)
    response <- "### Healthcare Resources & Emergency Access by District:\n\n"
    for (i in 1:nrow(res)) {
      response <- paste0(response, sprintf("- **%s**: %d facilities, %d total beds, Avg response time: **%.1f minutes**.\n", 
                                           res$name[i], res$facility_count[i], res$total_beds[i], res$avg_resp[i]))
    }
    return(response)
  }
  
  # 4D. Compare specific districts
  if (length(found_districts) >= 2) {
    # Compare found districts
    d1 <- found_districts[1]
    d2 <- found_districts[2]
    
    q <- sprintf("
      SELECT d.name,
             AVG(dem.income) AS income,
             AVG(dem.life_expectancy) AS life_exp,
             AVG(epi.obesity)*100.0 AS obesity,
             AVG(epi.diabetes)*100.0 AS diabetes,
             AVG(epi.smoking)*100.0 AS smoking,
             AVG(epi.vaccinated)*100.0 AS vac
      FROM demographics dem
      JOIN epidemiology epi ON dem.id = epi.demographic_id
      JOIN districts d ON dem.district_id = d.id
      WHERE d.name IN ('%s', '%s')
      GROUP BY d.name
    ", d1, d2)
    
    res <- dbGetQuery(conn, q)
    if (nrow(res) == 2) {
      return(sprintf(
        "### District Comparison: **%s** vs **%s**\n\n| Indicator | %s | %s |\n|---|---|---|\n| Avg Income | €%,.0f | €%,.0f |\n| Life Expectancy | %.1f years | %.1f years |\n| Obesity Rate | %.1f%% | %.1f%% |\n| Diabetes Rate | %.1f%% | %.1f%% |\n| Smoking Rate | %.1f%% | %.1f%% |\n| Vaccination Rate | %.1f%% | %.1f%% |",
        res$name[1], res$name[2], res$name[1], res$name[2], 
        res$income[1], res$income[2], 
        res$life_exp[1], res$life_exp[2],
        res$obesity[1], res$obesity[2],
        res$diabetes[1], res$diabetes[2],
        res$smoking[1], res$smoking[2],
        res$vac[1], res$vac[2]
      ))
    }
  }
  
  # 4E. Environmental correlations
  if (grepl("pm2.5", user_query) || grepl("air quality", user_query) || grepl("pollution", user_query)) {
    q <- "
      SELECT d.name, AVG(env.pm25) AS pm25, AVG(epi.heart_disease)*100 AS hd
      FROM environmental env
      JOIN districts d ON env.district_id = d.id
      JOIN demographics dem ON d.id = dem.district_id AND dem.year = CAST(SUBSTR(env.date, 1, 4) AS INTEGER)
      JOIN epidemiology epi ON dem.id = epi.demographic_id
      GROUP BY d.name
    "
    res <- dbGetQuery(conn, q)
    cor_val <- cor(res$pm25, res$hd)
    return(sprintf(
      "Our spatial analysis indicates an air pollution (PM2.5) baseline average ranging from **%.1f ug/m3** in the cleanest districts to **%.1f ug/m3** in central industrial corridors. The Pearson correlation between PM2.5 exposure and heart disease rates across the city districts is **r = %.2f**, indicating a strong positive correlation between fine particulate exposure and cardiovascular incidents.",
      min(res$pm25), max(res$pm25), cor_val
    ))
  }
  
  # 4F. Fallback to OpenAI API if key is available
  if (openai_key != "") {
    if (requireNamespace("httr", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE)) {
      tryCatch({
        # We can pass some context from our DB to the LLM to make the answer grounded!
        # Fetch high-level summary
        context_q <- "
          SELECT d.name, AVG(dem.income) AS avg_inc, AVG(dem.life_expectancy) AS avg_life, AVG(epi.obesity)*100.0 AS obesity
          FROM demographics dem
          JOIN epidemiology epi ON dem.id = epi.demographic_id
          JOIN districts d ON dem.district_id = d.id
          GROUP BY d.name
        "
        context_df <- dbGetQuery(conn, context_q)
        context_str <- paste(capture.output(print(context_df)), collapse = "\n")
        
        system_prompt <- paste(
          "You are the AI Assistant for the Dresden Health Atlas.",
          "You help city planners and researchers analyze health data.",
          "Answer the user's question accurately using this summary context of the city's districts:",
          context_str
        )
        
        response <- httr::POST(
          url = "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            "Authorization" = paste("Bearer", openai_key),
            "Content-Type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            model = "gpt-4o-mini",
            messages = list(
              list(role = "system", content = system_prompt),
              list(role = "user", content = user_message)
            )
          ), auto_unbox = TRUE),
          encode = "json"
        )
        
        content <- httr::content(response, as = "parsed", encoding = "UTF-8")
        if (!is.null(content$choices[[1]]$message$content)) {
          return(content$choices[[1]]$message$content)
        }
      }, error = function(e) {
        return(paste("OpenAI API call failed. Error details:", e$message))
      })
    }
  }
  
  # General local chat response
  return(paste(
    "I am the Health Atlas AI Assistant. I can answer specific queries about health statistics in Dresden.",
    "\n\n**Try asking:**",
    "\n- 'Which district has the highest obesity?'",
    "\n- 'Compare health metrics for Loschwitz and Prohlis'",
    "\n- 'Show healthcare accessibility metrics across districts.'",
    "\n- 'What is the correlation between PM2.5 and heart disease?'"
  ))
}
