# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Developer: akhapwoyaco
# Coverage: All application areas except infrastructure security
# Framework: testthat with extensive edge case testing
# =============================================================================

# Test Setup and Configuration
library(testthat)
library(shiny)
library(shinytestlib)
library(mockery)
library(withr)
library(DT)
library(plotly)
library(tidyverse)
library(R6)

# Source application files for testing
source("global.R")
source("utils.R")
source("custom_theme.R")
source("modules/logger_module.R")
source("modules/data_loader_module.R")

# =============================================================================
# 1. DATA VALIDATION & INTEGRITY TESTS
# =============================================================================

test_that("Data Loader Module - File Validation", {
  
  # Test valid CSV files
  test_employee_data <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("John", "Jane", "Bob", "Alice", "Charlie"),
    LastName = c("Doe", "Smith", "Johnson", "Williams", "Brown"),
    Gender = c("Male", "Female", "Male", "Female", "Male"),
    Age = c(30, 25, 35, 28, 32),
    Department = c("IT", "HR", "Finance", "IT", "Marketing"),
    Salary = c(75000, 65000, 80000, 70000, 68000),
    Attrition = c("No", "Yes", "No", "No", "Yes")
  )
  
  # Create temporary test files
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_employee_data, temp_file, row.names = FALSE)
  
  # Test successful file loading
  expect_silent(result <- load_employee_data(temp_file))
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 8)
  
  # Test missing file
  expect_error(load_employee_data("nonexistent.csv"), "File does not exist")
  
  # Test empty file
  empty_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(), empty_file, row.names = FALSE)
  expect_error(load_employee_data(empty_file), "File is empty")
  
  # Test corrupted file
  corrupted_file <- tempfile(fileext = ".csv")
  writeLines("invalid,csv,data\n1,2", corrupted_file)
  expect_error(load_employee_data(corrupted_file), "Invalid CSV format")
  
  # Cleanup
  unlink(c(temp_file, empty_file, corrupted_file))
})

test_that("Data Validation - Required Columns", {
  
  # Test missing required columns
  incomplete_data <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob")
    # Missing required columns
  )
  
  expect_error(validate_data_integrity(incomplete_data), "Missing required columns")
  
  # Test duplicate employee IDs
  duplicate_data <- data.frame(
    EmployeeID = c(1, 1, 2),
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Gender = c("Male", "Female", "Male"),
    Age = c(30, 25, 35),
    Department = c("IT", "HR", "Finance"),
    Salary = c(75000, 65000, 80000),
    Attrition = c("No", "Yes", "No")
  )
  
  expect_error(validate_data_integrity(duplicate_data), "Duplicate Employee IDs found")
})

test_that("Data Type Conversion - Edge Cases", {
  
  # Test age validation
  invalid_age_data <- data.frame(
    EmployeeID = 1:3,
    Age = c(-5, 150, "invalid")
  )
  
  expect_error(convert_data_types(invalid_age_data), "Invalid age values")
  
  # Test salary validation
  invalid_salary_data <- data.frame(
    EmployeeID = 1:3,
    Salary = c(-1000, 0, "not_numeric")
  )
  
  expect_error(convert_data_types(invalid_salary_data), "Invalid salary values")
  
  # Test date validation
  invalid_date_data <- data.frame(
    EmployeeID = 1:3,
    HireDate = c("2023-01-01", "invalid_date", "2025-12-31")
  )
  
  expect_error(convert_data_types(invalid_date_data), "Invalid date format")
})

# =============================================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# =============================================================================

test_that("AtlasLogger R6 Class - Initialization", {
  
  # Test logger initialization
  logger <- AtlasLogger$new()
  
  expect_is(logger, "AtlasLogger")
  expect_is(logger, "R6")
  expect_true(exists("log_info", envir = logger))
  expect_true(exists("log_warning", envir = logger))
  expect_true(exists("log_error", envir = logger))
  
  # Test initial state
  expect_equal(length(logger$get_logs()), 0)
  expect_equal(logger$get_error_count(), 0)
  expect_equal(logger$get_warning_count(), 0)
})

test_that("AtlasLogger - Logging Functions", {
  
  logger <- AtlasLogger$new()
  
  # Test info logging
  logger$log_info("Test info message", "test_module")
  logs <- logger$get_logs()
  expect_equal(length(logs), 1)
  expect_equal(logs[[1]]$level, "INFO")
  expect_equal(logs[[1]]$message, "Test info message")
  expect_equal(logs[[1]]$module, "test_module")
  
  # Test warning logging
  logger$log_warning("Test warning message", "test_module")
  expect_equal(logger$get_warning_count(), 1)
  
  # Test error logging
  logger$log_error("Test error message", "test_module")
  expect_equal(logger$get_error_count(), 1)
  
  # Test log filtering
  info_logs <- logger$get_logs(level = "INFO")
  expect_equal(length(info_logs), 1)
  
  warning_logs <- logger$get_logs(level = "WARNING")
  expect_equal(length(warning_logs), 1)
  
  error_logs <- logger$get_logs(level = "ERROR")
  expect_equal(length(error_logs), 1)
})

test_that("AtlasLogger - Performance Tracking", {
  
  logger <- AtlasLogger$new()
  
  # Test memory usage tracking
  initial_memory <- logger$get_memory_usage()
  expect_is(initial_memory, "numeric")
  expect_gt(initial_memory, 0)
  
  # Test execution time tracking
  start_time <- Sys.time()
  logger$start_timer("test_operation")
  Sys.sleep(0.1) # Small delay for testing
  execution_time <- logger$stop_timer("test_operation")
  
  expect_is(execution_time, "numeric")
  expect_gt(execution_time, 0.05) # Should be at least 50ms
  
  # Test performance summary
  summary <- logger$get_performance_summary()
  expect_is(summary, "list")
  expect_true("memory_usage" %in% names(summary))
  expect_true("execution_times" %in% names(summary))
})

test_that("AtlasLogger - Edge Cases", {
  
  logger <- AtlasLogger$new()
  
  # Test null/empty message handling
  expect_error(logger$log_info(NULL, "test_module"), "Message cannot be null")
  expect_error(logger$log_info("", "test_module"), "Message cannot be empty")
  
  # Test invalid module name
  expect_error(logger$log_info("test", NULL), "Module name cannot be null")
  expect_error(logger$log_info("test", ""), "Module name cannot be empty")
  
  # Test very long messages
  long_message <- paste(rep("A", 10000), collapse = "")
  logger$log_info(long_message, "test_module")
  logs <- logger$get_logs()
  expect_equal(nchar(logs[[1]]$message), 10000)
  
  # Test special characters in messages
  special_message <- "Test with 特殊字符 and émojis 🚀"
  logger$log_info(special_message, "test_module")
  logs <- logger$get_logs()
  expect_equal(logs[[length(logs)]]$message, special_message)
})

# =============================================================================
# 3. UTILITY FUNCTIONS TESTS
# =============================================================================

test_that("Utility Functions - Data Processing", {
  
  # Test calculate_attrition_rate
  test_data <- data.frame(
    EmployeeID = 1:10,
    Attrition = c(rep("Yes", 3), rep("No", 7))
  )
  
  rate <- calculate_attrition_rate(test_data)
  expect_equal(rate, 0.3)
  
  # Test empty data
  empty_data <- data.frame(EmployeeID = integer(0), Attrition = character(0))
  expect_equal(calculate_attrition_rate(empty_data), 0)
  
  # Test all attrition
  all_attrition <- data.frame(
    EmployeeID = 1:5,
    Attrition = rep("Yes", 5)
  )
  expect_equal(calculate_attrition_rate(all_attrition), 1.0)
  
  # Test no attrition
  no_attrition <- data.frame(
    EmployeeID = 1:5,
    Attrition = rep("No", 5)
  )
  expect_equal(calculate_attrition_rate(no_attrition), 0.0)
})

test_that("Utility Functions - Statistical Calculations", {
  
  # Test safe_mean function
  expect_equal(safe_mean(c(1, 2, 3, 4, 5)), 3)
  expect_equal(safe_mean(c(1, 2, NA, 4, 5)), 3)
  expect_equal(safe_mean(c(NA, NA, NA)), 0)
  expect_equal(safe_mean(numeric(0)), 0)
  
  # Test safe_median function
  expect_equal(safe_median(c(1, 2, 3, 4, 5)), 3)
  expect_equal(safe_median(c(1, 2, NA, 4, 5)), 3)
  expect_equal(safe_median(c(NA, NA, NA)), 0)
  
  # Test safe_sd function
  expect_equal(round(safe_sd(c(1, 2, 3, 4, 5)), 2), 1.58)
  expect_equal(safe_sd(c(1, 1, 1, 1, 1)), 0)
  expect_equal(safe_sd(c(NA, NA, NA)), 0)
})

test_that("Utility Functions - Format Helpers", {
  
  # Test format_currency
  expect_equal(format_currency(50000), "$50,000")
  expect_equal(format_currency(1234567.89), "$1,234,568")
  expect_equal(format_currency(0), "$0")
  expect_equal(format_currency(-1000), "-$1,000")
  
  # Test format_percentage
  expect_equal(format_percentage(0.1234), "12.3%")
  expect_equal(format_percentage(0.0), "0.0%")
  expect_equal(format_percentage(1.0), "100.0%")
  expect_equal(format_percentage(1.5), "150.0%")
  
  # Test format_number
  expect_equal(format_number(1234567), "1,234,567")
  expect_equal(format_number(0), "0")
  expect_equal(format_number(-1234), "-1,234")
})

# =============================================================================
# 4. SHINY MODULE TESTS
# =============================================================================

test_that("Data Loader Module - UI Generation", {
  
  # Test UI function
  ui_output <- dataLoaderUI("test_id")
  
  expect_is(ui_output, "shiny.tag")
  expect_true(any(grepl("test_id", as.character(ui_output))))
  
  # Test with different namespaces
  ui_output2 <- dataLoaderUI("different_namespace")
  expect_true(any(grepl("different_namespace", as.character(ui_output2))))
})

test_that("Overview Module - KPI Calculations", {
  
  # Mock data for testing
  mock_data <- data.frame(
    EmployeeID = 1:100,
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 100, replace = TRUE),
    Salary = runif(100, 40000, 120000),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.15, 0.85)),
    Age = sample(18:65, 100, replace = TRUE),
    Gender = sample(c("Male", "Female"), 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE)
  )
  
  # Test KPI calculations
  kpis <- calculate_kpis(mock_data)
  
  expect_is(kpis, "list")
  expect_true("total_employees" %in% names(kpis))
  expect_true("attrition_rate" %in% names(kpis))
  expect_true("avg_salary" %in% names(kpis))
  expect_true("avg_age" %in% names(kpis))
  
  expect_equal(kpis$total_employees, 100)
  expect_gte(kpis$attrition_rate, 0)
  expect_lte(kpis$attrition_rate, 1)
  expect_gt(kpis$avg_salary, 0)
  expect_gte(kpis$avg_age, 18)
  expect_lte(kpis$avg_age, 65)
})

test_that("Attrition Module - Analysis Functions", {
  
  # Test attrition analysis by department
  test_data <- data.frame(
    EmployeeID = 1:50,
    Department = rep(c("IT", "HR", "Finance", "Marketing", "Sales"), each = 10),
    Attrition = c(
      rep(c("Yes", "No"), c(2, 8)), # IT: 20% attrition
      rep(c("Yes", "No"), c(3, 7)), # HR: 30% attrition
      rep(c("Yes", "No"), c(1, 9)), # Finance: 10% attrition
      rep(c("Yes", "No"), c(4, 6)), # Marketing: 40% attrition
      rep(c("Yes", "No"), c(1, 9))  # Sales: 10% attrition
    )
  )
  
  analysis <- analyze_attrition_by_department(test_data)
  
  expect_is(analysis, "data.frame")
  expect_equal(nrow(analysis), 5)
  expect_true("Department" %in% names(analysis))
  expect_true("AttritionRate" %in% names(analysis))
  expect_true("EmployeeCount" %in% names(analysis))
  
  # Check specific department rates
  marketing_rate <- analysis[analysis$Department == "Marketing", "AttritionRate"]
  expect_equal(marketing_rate, 0.4)
  
  finance_rate <- analysis[analysis$Department == "Finance", "AttritionRate"]
  expect_equal(finance_rate, 0.1)
})

test_that("Performance Module - Rating Analysis", {
  
  # Test performance rating calculations
  performance_data <- data.frame(
    EmployeeID = 1:30,
    JobSatisfaction = sample(1:5, 30, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 30, replace = TRUE),
    WorkLifeBalance = sample(1:5, 30, replace = TRUE),
    SelfRating = sample(1:5, 30, replace = TRUE),
    ManagerRating = sample(1:5, 30, replace = TRUE)
  )
  
  # Test average ratings calculation
  avg_ratings <- calculate_average_ratings(performance_data)
  
  expect_is(avg_ratings, "list")
  expect_true("job_satisfaction" %in% names(avg_ratings))
  expect_true("environment_satisfaction" %in% names(avg_ratings))
  expect_true("work_life_balance" %in% names(avg_ratings))
  expect_true("self_rating" %in% names(avg_ratings))
  expect_true("manager_rating" %in% names(avg_ratings))
  
  # Check rating ranges
  expect_gte(avg_ratings$job_satisfaction, 1)
  expect_lte(avg_ratings$job_satisfaction, 5)
  expect_gte(avg_ratings$manager_rating, 1)
  expect_lte(avg_ratings$manager_rating, 5)
})

# =============================================================================
# 5. CUSTOM THEME TESTS
# =============================================================================

test_that("Custom Theme - ggplot2 Theme", {
  
  # Test atlas_theme function
  theme_obj <- atlas_theme()
  
  expect_is(theme_obj, "theme")
  expect_is(theme_obj, "gg")
  
  # Test theme with different parameters
  theme_dark <- atlas_theme(base_size = 14, dark_mode = TRUE)
  expect_is(theme_dark, "theme")
  
  # Test color palette
  colors <- atlas_color_palette()
  expect_is(colors, "character")
  expect_gt(length(colors), 0)
  
  # Test color validation
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("Custom Theme - Color Functions", {
  
  # Test get_atlas_color
  primary_color <- get_atlas_color("primary")
  expect_is(primary_color, "character")
  expect_equal(length(primary_color), 1)
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", primary_color))
  
  # Test invalid color name
  expect_error(get_atlas_color("invalid_color"), "Color not found")
  
  # Test generate_color_gradient
  gradient <- generate_color_gradient(5)
  expect_is(gradient, "character")
  expect_equal(length(gradient), 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", gradient)))
})

# =============================================================================
# 6. VISUALIZATION TESTS
# =============================================================================

test_that("Visualization Functions - Chart Generation", {
  
  # Test data for visualizations
  viz_data <- data.frame(
    Department = c("IT", "HR", "Finance", "Marketing", "Sales"),
    EmployeeCount = c(25, 15, 20, 18, 22),
    AttritionRate = c(0.12, 0.20, 0.08, 0.15, 0.18),
    AvgSalary = c(75000, 55000, 70000, 60000, 65000)
  )
  
  # Test bar chart creation
  bar_chart <- create_department_bar_chart(viz_data)
  expect_is(bar_chart, "ggplot")
  
  # Test scatter plot creation
  scatter_plot <- create_salary_attrition_scatter(viz_data)
  expect_is(scatter_plot, "ggplot")
  
  # Test plotly conversion
  plotly_chart <- create_interactive_chart(bar_chart)
  expect_is(plotly_chart, "plotly")
})

test_that("Visualization Functions - Edge Cases", {
  
  # Test empty data
  empty_data <- data.frame(
    Department = character(0),
    EmployeeCount = numeric(0),
    AttritionRate = numeric(0)
  )
  
  expect_error(create_department_bar_chart(empty_data), "No data to plot")
  
  # Test single row data
  single_row <- data.frame(
    Department = "IT",
    EmployeeCount = 10,
    AttritionRate = 0.1
  )
  
  chart <- create_department_bar_chart(single_row)
  expect_is(chart, "ggplot")
  
  # Test data with NA values
  na_data <- data.frame(
    Department = c("IT", "HR", "Finance"),
    EmployeeCount = c(10, NA, 15),
    AttritionRate = c(0.1, 0.2, NA)
  )
  
  chart_na <- create_department_bar_chart(na_data)
  expect_is(chart_na, "ggplot")
})

# =============================================================================
# 7. REACTIVE TESTING
# =============================================================================

test_that("Reactive Functions - Data Filtering", {
  
  # Test data filtering function
  test_data <- data.frame(
    EmployeeID = 1:20,
    Department = rep(c("IT", "HR", "Finance", "Marketing"), each = 5),
    Gender = rep(c("Male", "Female"), 10),
    Age = seq(25, 44, 1),
    Salary = seq(50000, 69000, 1000)
  )
  
  # Test department filtering
  filtered_data <- apply_filters(test_data, list(Department = "IT"))
  expect_equal(nrow(filtered_data), 5)
  expect_true(all(filtered_data$Department == "IT"))
  
  # Test multiple filters
  multi_filtered <- apply_filters(test_data, list(
    Department = c("IT", "HR"),
    Gender = "Male"
  ))
  
  expect_true(all(multi_filtered$Department %in% c("IT", "HR")))
  expect_true(all(multi_filtered$Gender == "Male"))
  
  # Test age range filtering
  age_filtered <- apply_filters(test_data, list(
    Age = c(30, 35)
  ))
  
  expect_true(all(age_filtered$Age >= 30 & age_filtered$Age <= 35))
})

test_that("Reactive Functions - Cross-Module Communication", {
  
  # Test shared reactive values
  shared_values <- reactiveValues(
    employee_data = NULL,
    filtered_data = NULL,
    selected_filters = list()
  )
  
  # Test data update
  test_data <- data.frame(EmployeeID = 1:10)
  shared_values$employee_data <- test_data
  
  expect_equal(nrow(shared_values$employee_data), 10)
  
  # Test filter update
  shared_values$selected_filters <- list(Department = "IT")
  expect_equal(length(shared_values$selected_filters), 1)
  expect_equal(shared_values$selected_filters$Department, "IT")
})

# =============================================================================
# 8. ERROR HANDLING TESTS
# =============================================================================

test_that("Error Handling - Graceful Degradation", {
  
  # Test handling of missing data columns
  incomplete_data <- data.frame(EmployeeID = 1:5)
  
  expect_error(
    safely_calculate_kpis(incomplete_data),
    "Required columns missing"
  )
  
  # Test handling of invalid data types
  invalid_data <- data.frame(
    EmployeeID = c("A", "B", "C"),
    Salary = c("high", "medium", "low")
  )
  
  expect_error(
    safely_calculate_kpis(invalid_data),
    "Invalid data types"
  )
  
  # Test network/file access errors
  expect_error(
    safely_load_data("http://nonexistent.com/data.csv"),
    "Network error"
  )
})

test_that("Error Handling - Input Validation", {
  
  # Test negative values
  negative_data <- data.frame(
    EmployeeID = 1:3,
    Age = c(-5, 25, 30),
    Salary = c(50000, -1000, 75000)
  )
  
  validation_result <- validate_input_data(negative_data)
  expect_false(validation_result$valid)
  expect_true(any(grepl("negative", validation_result$errors)))
  
  # Test out-of-range values
  range_data <- data.frame(
    EmployeeID = 1:3,
    Age = c(150, 25, 30),
    JobSatisfaction = c(1, 6, 3)
  )
  
  validation_result2 <- validate_input_data(range_data)
  expect_false(validation_result2$valid)
  expect_true(any(grepl("range", validation_result2$errors)))
})

# =============================================================================
# 9. PERFORMANCE TESTS
# =============================================================================

test_that("Performance Tests - Large Dataset Handling", {
  
  # Create large test dataset
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Department = sample(c("IT", "HR", "Finance", "Marketing", "Sales"), 10000, replace = TRUE),
    Salary = runif(10000, 40000, 120000),
    Attrition = sample(c("Yes", "No"), 10000, replace = TRUE),
    Age = sample(18:65, 10000, replace = TRUE)
  )
  
  # Test performance of KPI calculations
  start_time <- Sys.time()
  kpis <- calculate_kpis(large_data)
  end_time <- Sys.time()
  
  execution_time <- as.numeric(end_time - start_time)
  
  # Should complete within reasonable time (< 5 seconds)
  expect_lt(execution_time, 5)
  expect_is(kpis, "list")
  expect_equal(kpis$total_employees, 10000)
})

test_that("Performance Tests - Memory Usage", {
  
  # Test memory usage with different dataset sizes
  small_data <- data.frame(EmployeeID = 1:100)
  medium_data <- data.frame(EmployeeID = 1:1000)
  large_data <- data.frame(EmployeeID = 1:10000)
  
  # Monitor memory usage
  mem_before <- pryr::object_size(ls())
  
  # Process datasets
  process_dataset(small_data)
  process_dataset(medium_data)
  process_dataset(large_data)
  
  mem_after <- pryr::object_size(ls())
  
  # Memory usage should not grow excessively
  memory_growth <- as.numeric(mem_after - mem_before)
  expect_lt(memory_growth, 100000000) # Less than 100MB growth
})

# =============================================================================
# 10. INTEGRATION TESTS
# =============================================================================

test_that("Integration Tests - Module Interaction", {
  
  # Test data flow between modules
  test_data <- data.frame(
    EmployeeID = 1:50,
    Department = sample(c("IT", "HR", "Finance"), 50, replace = TRUE),
    Salary = runif(50, 40000, 100000),
    Attrition = sample(c("Yes", "No"), 50, replace = TRUE)
  )
  
  # Test data loader -> overview module
  loaded_data <- load_and_validate_data(test_data)
  kpis <- calculate_kpis(loaded_data)
  
  expect_is(loaded_data, "data.frame")
  expect_is(kpis, "list")
  expect_equal(nrow(loaded_data), 50)
  expect_true("total_employees" %in% names(kpis))
  
  # Test overview -> attrition module
  attrition_analysis <- analyze_attrition_by_department(loaded_data)
  
  expect_is(attrition_analysis, "data.frame")
  expect_true("Department" %in% names(attrition_analysis))
  expect_true("AttritionRate" %in% names(attrition_analysis))
})

test_that("Integration Tests - End-to-End Workflow", {
  
  # Simulate complete workflow
  logger <- AtlasLogger$new()
  
  # Step 1: Load data
  logger$log_info("Starting data load", "integration_test")
  test_data <- create_mock_employee_data(100)
  logger$log_info("Data loaded successfully", "integration_test")
  
  # Step 2: Validate data
  validation_result <- validate_data_integrity(test_data)
  expect_true(validation_result$valid)
  logger$log_info("Data validation passed", "integration_test")
  
  # Step 3: Calculate KPIs
  kpis <- calculate_kpis(test_data)
  expect_is(kpis, "list")
  logger$log_info("KPIs calculated", "integration_test")
  
  # Step 4: Generate visualizations
  viz <- create_department_bar_chart(test_data)
  expect_is(viz, "ggplot")
  logger$log_info("Visualizations generated", "integration_test")
  
  # Step 5: Check logs
  logs <- logger$get_logs()
  expect_gte(length(logs), 4)
  expect_equal(logger$get_error_count(), 0)
})

# =============================================================================
# 11. UI/UX TESTS
# =============================================================================

test_that("UI Tests - Component Rendering", {
  
  # Test value box creation
  value_box <- create_value_box("Total Employees", 150, "users", "primary")
  expect_is(value_box, "shiny.tag")
  
  # Test card creation
  card <- create_info_card("Test Title", "Test content")
  expect_is(card, "shiny.tag")
  
  # Test alert creation
  alert <- create_alert("Test message", "info")
  expect_is(alert, "shiny.tag")
})

test_that("UI Tests - Responsive Design", {
  
  # Test responsive grid creation
  grid <- create_responsive_grid(
    list(
      create_value_box("KPI 1", 100, "chart", "primary"),
      create_value_box("KPI 2", 200, "users", "info")
    )
  )
  
  expect_is(grid, "shiny.tag")
  expect_