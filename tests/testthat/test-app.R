# Unit Tests for AI Health Atlas Backend
library(testthat)

# Source files needed for testing
source("../../R/utils/db_helpers.R")
source("../../R/utils/ai_helpers.R")

test_that("Database connectivity and schemas", {
  # 1. DB path exists
  expect_true(file.exists("../../database/health_atlas.db"))
  
  # 2. Test fetching districts
  districts <- db_get_districts()
  expect_s3_class(districts, "data.frame")
  expect_equal(nrow(districts), 10)
  expect_true("Altstadt" %in% districts$name)
  
  # 3. Test user credential checking
  # Invalid user
  expect_null(verify_user_credentials("nonexistent", "wrongpass"))
  
  # Valid user
  admin_user <- verify_user_credentials("admin", "admin123")
  expect_type(admin_user, "list")
  expect_equal(admin_user$role, "Admin")
})

test_that("Aggregations and data loading", {
  # 1. Aggregated cohort metrics
  agg_df <- db_get_aggregated_data(districts = "Altstadt", years = 2025)
  expect_s3_class(agg_df, "data.frame")
  if (nrow(agg_df) > 0) {
    expect_true("district_name" %in% colnames(agg_df))
    expect_equal(unique(agg_df$district_name), "Altstadt")
  }
  
  # 2. Environmental logs aggregation
  env_df <- db_get_environmental_stats(districts = "Altstadt", years = 2025)
  expect_s3_class(env_df, "data.frame")
  if (nrow(env_df) > 0) {
    expect_true("avg_pm25" %in% colnames(env_df))
  }
})

test_that("AI summary and policy recommendations", {
  sum_res <- generate_district_summary("Altstadt")
  expect_type(sum_res, "list")
  expect_true("summary" %in% names(sum_res))
  expect_true("recommendations" %in% names(sum_res))
  expect_gt(nchar(sum_res$summary), 20)
})

test_that("Statistical forecasting & anomaly models", {
  # 1. Forecast
  years <- 2020:2025
  vals <- c(10, 11, 12, 13, 14, 15)
  pred <- predict_trend(years, vals, target_years = 2026:2028)
  
  expect_equal(pred$year, 2026:2028)
  expect_equal(pred$value[1], 16)
  
  # 2. Anomalies
  vals_outlier <- c(10, 10, 11, 10, 10, 50, 10, 10, 11, 10) # 50 is outlier
  anom <- detect_anomalies(vals_outlier, threshold = 1.95)
  expect_true(anom[6])
  expect_false(anom[1])
})

test_that("NLP Chatbot engine responses", {
  # 1. Obesity query
  obs_res <- ask_ai_chat("Which district has the highest obesity?")
  expect_true(grepl("obesity rate", obs_res, ignore.case = TRUE))
  
  # 2. Income query
  inc_res <- ask_ai_chat("wealthiest district")
  expect_true(grepl("highest average", inc_res, ignore.case = TRUE))
  
  # 3. Compare query
  comp_res <- ask_ai_chat("Compare Altstadt and Neustadt")
  expect_true(grepl("District Comparison", comp_res))
})
