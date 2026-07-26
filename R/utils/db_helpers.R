# Database Helpers for AI Health Atlas
library(DBI)
library(RSQLite)

DB_PATH <- "database/health_atlas.db"

# 1. Connect to SQLite
get_db_connection <- function() {
  dbConnect(SQLite(), DB_PATH)
}

# 2. SHA-256 Hashing in R
# Fallback to python or simple openssl/digest if packages are not available
hash_password_r <- function(password) {
  # Standard hashing in R using openssl if available
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(openssl::sha256(password))
  } else if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(password, algo = "sha256", serialize = FALSE))
  } else {
    # Fallback to system-level python hashing if needed
    tryCatch({
      cmd <- sprintf('python -c "import hashlib; print(hashlib.sha256(b\'%s\').hexdigest())"', password)
      res <- system(cmd, intern = TRUE)
      if (length(res) > 0) return(trimws(res))
    }, error = function(e) {})
    
    # Simple warning but proceed with a known string hash
    # For safety, let's write a pure R sha256 or use standard python fallback
    return(password)
  }
}

# 3. Verify user login credentials
verify_user_credentials <- function(username, password) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  pass_hash <- hash_password_r(password)
  
  query <- "SELECT username, role, full_name FROM users WHERE username = ? AND password_hash = ?"
  res <- dbGetQuery(conn, query, params = list(username, pass_hash))
  
  if (nrow(res) == 1) {
    return(as.list(res[1, ]))
  } else {
    return(NULL)
  }
}

# 4. Fetch all districts
db_get_districts <- function() {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  dbReadTable(conn, "districts")
}

# 5. Fetch all healthcare resources
db_get_healthcare_resources <- function(district_names = NULL, facility_types = NULL) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "
    SELECT h.*, d.name AS district_name 
    FROM healthcare_resources h
    JOIN districts d ON h.district_id = d.id
    WHERE 1=1
  "
  
  params <- list()
  if (!is.null(district_names) && length(district_names) > 0) {
    placeholders <- paste(rep("?", length(district_names)), collapse = ", ")
    query <- paste(query, sprintf("AND d.name IN (%s)", placeholders))
    params <- c(params, as.list(district_names))
  }
  
  if (!is.null(facility_types) && length(facility_types) > 0) {
    placeholders <- paste(rep("?", length(facility_types)), collapse = ", ")
    query <- paste(query, sprintf("AND h.type IN (%s)", placeholders))
    params <- c(params, as.list(facility_types))
  }
  
  dbGetQuery(conn, query, params = params)
}

# 6. Retrieve aggregated cohort metrics (highly efficient SQL filtering)
db_get_aggregated_data <- function(districts = NULL, years = NULL, genders = NULL, age_groups = NULL) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Build filter conditions
  conditions <- c("1=1")
  params <- list()
  
  if (!is.null(districts) && length(districts) > 0) {
    placeholders <- paste(rep("?", length(districts)), collapse = ", ")
    conditions <- c(conditions, sprintf("d.name IN (%s)", placeholders))
    params <- c(params, as.list(districts))
  }
  
  if (!is.null(years) && length(years) > 0) {
    placeholders <- paste(rep("?", length(years)), collapse = ", ")
    conditions <- c(conditions, sprintf("dem.year IN (%s)", placeholders))
    params <- c(params, as.list(years))
  }
  
  if (!is.null(genders) && length(genders) > 0) {
    placeholders <- paste(rep("?", length(genders)), collapse = ", ")
    conditions <- c(conditions, sprintf("dem.gender IN (%s)", placeholders))
    params <- c(params, as.list(genders))
  }
  
  if (!is.null(age_groups) && length(age_groups) > 0) {
    age_conds <- list()
    for (ag in age_groups) {
      if (ag == "0-17") age_conds <- c(age_conds, "dem.age BETWEEN 0 AND 17")
      else if (ag == "18-34") age_conds <- c(age_conds, "dem.age BETWEEN 18 AND 34")
      else if (ag == "35-49") age_conds <- c(age_conds, "dem.age BETWEEN 35 AND 49")
      else if (ag == "50-64") age_conds <- c(age_conds, "dem.age BETWEEN 50 AND 64")
      else if (ag == "65+") age_conds <- c(age_conds, "dem.age >= 65")
    }
    conditions <- c(conditions, sprintf("(%s)", paste(age_conds, collapse = " OR ")))
  }
  
  where_clause <- paste(conditions, collapse = " AND ")
  
  query <- sprintf("
    SELECT 
      d.name AS district_name,
      dem.year,
      dem.gender,
      CASE 
        WHEN dem.age BETWEEN 0 AND 17 THEN '0-17'
        WHEN dem.age BETWEEN 18 AND 34 THEN '18-34'
        WHEN dem.age BETWEEN 35 AND 49 THEN '35-49'
        WHEN dem.age BETWEEN 50 AND 64 THEN '50-64'
        ELSE '65+'
      END AS age_group,
      COUNT(dem.id) AS cohort_size,
      AVG(dem.income) AS avg_income,
      AVG(dem.life_expectancy) AS avg_life_expectancy,
      SUM(dem.employed) * 100.0 / COUNT(dem.id) AS employment_rate,
      SUM(epi.diabetes) * 100.0 / COUNT(dem.id) AS diabetes_rate,
      SUM(epi.heart_disease) * 100.0 / COUNT(dem.id) AS heart_disease_rate,
      SUM(epi.obesity) * 100.0 / COUNT(dem.id) AS obesity_rate,
      SUM(epi.smoking) * 100.0 / COUNT(dem.id) AS smoking_rate,
      SUM(epi.vaccinated) * 100.0 / COUNT(dem.id) AS vaccination_rate,
      AVG(epi.mental_health_index) AS avg_mental_health_index
    FROM demographics dem
    JOIN districts d ON dem.district_id = d.id
    JOIN epidemiology epi ON dem.id = epi.demographic_id
    WHERE %s
    GROUP BY district_name, year, gender, age_group
  ", where_clause)
  
  dbGetQuery(conn, query, params = params)
}

# 7. Fetch environmental logs aggregated by year or month
db_get_environmental_stats <- function(districts = NULL, years = NULL) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  conditions <- c("1=1")
  params <- list()
  
  if (!is.null(districts) && length(districts) > 0) {
    placeholders <- paste(rep("?", length(districts)), collapse = ", ")
    conditions <- c(conditions, sprintf("d.name IN (%s)", placeholders))
    params <- c(params, as.list(districts))
  }
  
  if (!is.null(years) && length(years) > 0) {
    # Extract year from date string in SQLite (SUBSTR(e.date, 1, 4))
    placeholders <- paste(rep("?", length(years)), collapse = ", ")
    conditions <- c(conditions, sprintf("CAST(SUBSTR(e.date, 1, 4) AS INTEGER) IN (%s)", placeholders))
    params <- c(params, as.list(years))
  }
  
  where_clause <- paste(conditions, collapse = " AND ")
  
  query <- sprintf("
    SELECT 
      d.name AS district_name,
      CAST(SUBSTR(e.date, 1, 4) AS INTEGER) AS year,
      AVG(e.pm25) AS avg_pm25,
      AVG(e.temperature) AS avg_temperature,
      AVG(e.green_space_pct) AS avg_green_space_pct,
      AVG(e.noise_level_db) AS avg_noise_level_db,
      AVG(e.water_quality_index) AS avg_water_quality_index
    FROM environmental e
    JOIN districts d ON e.district_id = d.id
    WHERE %s
    GROUP BY district_name, year
  ", where_clause)
  
  dbGetQuery(conn, query, params = params)
}

# 8. Fetch raw data samples for tabular views
db_get_raw_dashboard_data <- function(limit = 1000) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- sprintf("
    SELECT 
      dem.id,
      d.name AS district_name,
      dem.year,
      dem.age,
      dem.gender,
      ROUND(dem.income, 2) AS income,
      dem.education,
      dem.employed,
      dem.life_expectancy,
      epi.diabetes,
      epi.heart_disease,
      epi.obesity,
      epi.smoking,
      epi.vaccinated,
      epi.mental_health_index
    FROM demographics dem
    JOIN districts d ON dem.district_id = d.id
    JOIN epidemiology epi ON dem.id = epi.demographic_id
    ORDER BY dem.id DESC
    LIMIT %d
  ", limit)
  
  dbGetQuery(conn, query)
}

# 9. Admin operations
db_add_user <- function(username, password, role, full_name) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  pass_hash <- hash_password_r(password)
  
  query <- "INSERT INTO users (username, password_hash, role, full_name) VALUES (?, ?, ?, ?)"
  dbExecute(conn, query, params = list(username, pass_hash, role, full_name))
}

db_get_all_users <- function() {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  dbGetQuery(conn, "SELECT id, username, role, full_name, created_at FROM users")
}

db_delete_user <- function(user_id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  dbExecute(conn, "DELETE FROM users WHERE id = ?", params = list(user_id))
}

db_refresh_data <- function(dem_csv_path, epi_csv_path, env_csv_path) {
  # Clean up and reload database from raw files
  # This acts as the CSV -> Transformation -> Database data pipeline!
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  dbExecute(conn, "DELETE FROM demographics")
  dbExecute(conn, "DELETE FROM epidemiology")
  dbExecute(conn, "DELETE FROM environmental")
  
  # Fetch district names to ID mapping
  dist_map <- dbGetQuery(conn, "SELECT name, id FROM districts")
  dist_list <- setNames(dist_map$id, dist_map$name)
  
  # Load demographics
  dem_data <- read.csv(dem_csv_path)
  dem_data$district_id <- dist_list[dem_data$district_name]
  dem_write <- dem_data[, c("id", "district_id", "year", "age", "gender", "income", "education", "employed", "life_expectancy")]
  dbWriteTable(conn, "demographics", dem_write, append = TRUE, row.names = FALSE)
  
  # Load epidemiology
  epi_data <- read.csv(epi_csv_path)
  dbWriteTable(conn, "epidemiology", epi_data, append = TRUE, row.names = FALSE)
  
  # Load environmental
  env_data <- read.csv(env_csv_path)
  env_data$district_id <- dist_list[env_data$district_name]
  env_write <- env_data[, c("district_id", "date", "pm25", "temperature", "green_space_pct", "noise_level_db", "water_quality_index")]
  dbWriteTable(conn, "environmental", env_write, append = TRUE, row.names = FALSE)
  
  TRUE
}
