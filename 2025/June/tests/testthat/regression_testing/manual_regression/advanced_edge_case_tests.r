# ================================================================================
# Atlas Labs HR Analytics - Advanced Edge Case & Boundary Testing Suite
# Author: akhapwoyaco
# Focus: Edge cases, boundary conditions, and corner cases not covered in manual testing
# ================================================================================

library(testthat)
library(shiny)
library(lubridate)
library(stringr)

# ================================================================================
# 11. BOUNDARY VALUE TESTING
# ================================================================================

test_that("Boundary Value Analysis - Edge Cases", {
  
  # Test 11.1: Date boundary conditions
  test_that("handles date edge cases correctly", {
    edge_dates <- list(
      leap_year = as.Date("2024-02-29"),
      year_2000 = as.Date("2000-01-01"),
      far_future = as.Date("2099-12-31"),
      epoch_start = as.Date("1970-01-01"),
      february_28 = as.Date("2023-02-28"),
      december_31 = as.Date("2023-12-31")
    )
    
    for(date_type in names(edge_dates)) {
      test_data <- create_test_dataset(100)
      test_data$HireDate <- edge_dates[[date_type]]
      
      expect_silent({
        tenure_calc <- calculate_tenure(test_data)
        date_filters <- apply_date_filters(test_data, edge_dates[[date_type]], edge_dates[[date_type]])
      })
      
      expect_true(