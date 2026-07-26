# Test Runner for AI Health Atlas
library(testthat)

message("Running test suite for AI Health Atlas...")
test_dir("tests/testthat", reporter = "summary")
