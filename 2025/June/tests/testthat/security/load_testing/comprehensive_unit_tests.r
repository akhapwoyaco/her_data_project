# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Developer: akhapwoyaco
# Purpose: Comprehensive unit testing covering all functional areas
# Excludes: Load testing, concurrency, database pooling, network bandwidth
# =============================================================================

# Test Dependencies
library(testthat)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(R6)
library(lubridate)
library(mockery)
library(withr)

# Source application files for testing
source("global.R")
source("utils.R")
source("custom_theme.R")
source("modules/logger_module.R")
source("modules/data_loader_module.R")
source("modules/overview_module.R")
source("modules/attrition_module.R")
source("modules/demographics_module.R")
source("modules/performance_module.R")
source("modules/compensation_module.R")
source("modules/satisfaction_module.R")
source("modules/report_module.R")
source("modules/footer_module.R")
source("modules/sidebar_module.R")

# =============================================================================
# 1. DATA VALIDATION AND INTEGRITY TESTS
# =============================================================================

test_that("Data Loader Module - File Validation", {
  # Test valid CSV file loading
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    EmployeeID = 1:100,
    FirstName = paste0("Employee", 1:100),
    Age = sample(22:65, 100, replace = TRUE),
    Department = sample(c("HR", "Engineering", "Sales"), 100, replace = TRUE),
    Salary = sample(30000:150000, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE)
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  
  # Test successful file loading
  expect_silent(loaded_data <- load_employee_data(temp_csv))
  expect_equal(nrow(loaded_data), 100)
  expect_true(all(c("EmployeeID", "FirstName", "Age") %in% names(loaded_data)))
  
  # Test file not found error
  expect_error(load_employee_data("nonexistent_file.csv"), "File not found")
  
  # Test invalid file format
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("invalid content", temp_txt)
  expect_error(load_employee_data(temp_txt), "Invalid file format")
  
  # Cleanup
  unlink(c(temp_csv, temp_txt))
})

test_that("Data Loader Module - Data Type Validation", {
  # Test numeric columns
  invalid_data <- data.frame(
    EmployeeID = c("A", "B", "C"),  # Should be numeric
    Age = c("twenty", "thirty", "forty"),  # Should be numeric
    Salary = c("high", "medium", "low")  # Should be numeric
  )
  
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(invalid_data, temp_csv, row.names = FALSE)
  
  expect_error(validate_data_types(invalid_data), "Invalid data types detected")
  
  # Test valid data types
  valid_data <- data.frame(
    EmployeeID = 1:3,
    Age = c(25, 35, 45),
    Salary = c(50000, 75000, 100000)
  )
  
  expect_silent(validate_data_types(valid_data))
  unlink(temp_csv)
})

test_that("Data Loader Module - Missing Value Handling", {
  # Test data with missing values
  data_with_na <- data.frame(
    EmployeeID = c(1, 2, NA, 4),
    FirstName = c("John", NA, "Jane", "Bob"),
    Age = c(25, 35, 45, NA),
    Salary = c(50000, NA, 75000, 100000)
  )
  
  # Test missing value detection
  missing_summary <- detect_missing_values(data_with_na)
  expect_true(missing_summary$EmployeeID > 0)
  expect_true(missing_summary$FirstName > 0)
  expect_true(missing_summary$Age > 0)
  expect_true(missing_summary$Salary > 0)
  
  # Test missing value imputation
  imputed_data <- handle_missing_values(data_with_na)
  expect_true(sum(is.na(imputed_data$Age)) < sum(is.na(data_with_na$Age)))
})

test_that("Data Loader Module - Data Integrity Checks", {
  # Test duplicate employee IDs
  duplicate_data <- data.frame(
    EmployeeID = c(1, 2, 2, 3),  # Duplicate ID
    FirstName = c("John", "Jane", "Bob", "Alice")
  )
  
  expect_error(validate_data_integrity(duplicate_data), "Duplicate employee IDs found")
  
  # Test invalid date formats
  invalid_date_data <- data.frame(
    EmployeeID = 1:3,
    HireDate = c("2023-01-01", "invalid-date", "2023-03-01")
  )
  
  expect_error(validate_date_columns(invalid_date_data), "Invalid date format")
  
  # Test salary range validation
  invalid_salary_data <- data.frame(
    EmployeeID = 1:3,
    Salary = c(50000, -1000, 10000000)  # Negative and unrealistic values
  )
  
  expect_warning(validate_salary_ranges(invalid_salary_data), "Suspicious salary values")
})

# =============================================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# =============================================================================

test_that("AtlasLogger R6 Class - Initialization", {
  # Test logger initialization
  logger <- AtlasLogger$new()
  expect_true(R6::is.R6(logger))
  expect_true(inherits(logger, "AtlasLogger"))
  
  # Test logger with custom settings
  logger_custom <- AtlasLogger$new(
    log_level = "DEBUG",
    console_output = TRUE,
    file_output = FALSE
  )
  expect_equal(logger_custom$log_level, "DEBUG")
  expect_true(logger_custom$console_output)
  expect_false(logger_custom$file_output)
})

test_that("AtlasLogger R6 Class - Logging Functions", {
  logger <- AtlasLogger$new()
  
  # Test info logging
  expect_silent(logger$log_info("Test info message", "test_module"))
  expect_true(length(logger$get_logs()) > 0)
  
  # Test warning logging
  expect_silent(logger$log_warning("Test warning message", "test_module"))
  
  # Test error logging
  expect_silent(logger$log_error("Test error message", "test_module"))
  
  # Test log level filtering
  logger$set_log_level("ERROR")
  initial_count <- length(logger$get_logs())
  logger$log_info("This should be filtered out", "test_module")
  expect_equal(length(logger$get_logs()), initial_count)
  
  logger$log_error("This should be logged", "test_module")
  expect_true(length(logger$get_logs()) > initial_count)
})

test_that("AtlasLogger R6 Class - Performance Tracking", {
  logger <- AtlasLogger$new()
  
  # Test execution time tracking
  start_time <- logger$start_timer("test_operation")
  Sys.sleep(0.1)  # Simulate operation
  end_time <- logger$end_timer("test_operation")
  
  expect_true(end_time > start_time)
  expect_true(logger$get_execution_time("test_operation") >= 0.1)
  
  # Test memory usage tracking
  initial_memory <- logger$track_memory_usage("test_module")
  expect_true(is.numeric(initial_memory))
  expect_true(initial_memory > 0)
  
  # Test performance summary
  perf_summary <- logger$get_performance_summary()
  expect_true(is.list(perf_summary))
  expect_true("execution_times" %in% names(perf_summary))
  expect_true("memory_usage" %in% names(perf_summary))
})

test_that("AtlasLogger R6 Class - Edge Cases", {
  logger <- AtlasLogger$new()
  
  # Test logging with NULL message
  expect_error(logger$log_info(NULL, "test_module"), "Message cannot be NULL")
  
  # Test logging with empty message
  expect_error(logger$log_info("", "test_module"), "Message cannot be empty")
  
  # Test logging without module specification
  expect_error(logger$log_info("Test message", NULL), "Module name is required")
  
  # Test timer operations with invalid names
  expect_error(logger$start_timer(""), "Timer name cannot be empty")
  expect_error(logger$end_timer("nonexistent_timer"), "Timer not found")
  
  # Test memory tracking with invalid module
  expect_error(logger$track_memory_usage(""), "Module name cannot be empty")
})

# =============================================================================
# 3. UTILITY FUNCTIONS TESTS
# =============================================================================

test_that("Utility Functions - Data Processing", {
  # Test calculate_attrition_rate
  test_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = c(rep("Yes", 20), rep("No", 80))
  )
  
  attrition_rate <- calculate_attrition_rate(test_data)
  expect_equal(attrition_rate, 0.2)
  
  # Test with edge cases
  all_attrition <- data.frame(Attrition = rep("Yes", 10))
  expect_equal(calculate_attrition_rate(all_attrition), 1.0)
  
  no_attrition <- data.frame(Attrition = rep("No", 10))
  expect_equal(calculate_attrition_rate(no_attrition), 0.0)
  
  # Test with empty data
  empty_data <- data.frame(Attrition = character(0))
  expect_error(calculate_attrition_rate(empty_data), "No data provided")
})

test_that("Utility Functions - Statistical Calculations", {
  # Test calculate_satisfaction_metrics
  satisfaction_data <- data.frame(
    JobSatisfaction = c(4, 5, 3, 4, 5, 2, 4, 3, 5, 4),
    EnvironmentSatisfaction = c(3, 4, 5, 3, 4, 2, 3, 4, 5, 3),
    WorkLifeBalance = c(5, 4, 3, 4, 5, 3, 4, 3, 4, 5)
  )
  
  metrics <- calculate_satisfaction_metrics(satisfaction_data)
  expect_true(all(c("job_satisfaction", "environment_satisfaction", "work_life_balance") %in% names(metrics)))
  expect_true(all(sapply(metrics, is.numeric)))
  expect_true(all(sapply(metrics, function(x) x >= 1 && x <= 5)))
  
  # Test with missing values
  satisfaction_na <- satisfaction_data
  satisfaction_na$JobSatisfaction[1:3] <- NA
  
  metrics_na <- calculate_satisfaction_metrics(satisfaction_na)
  expect_true(is.numeric(metrics_na$job_satisfaction))
  expect_false(is.na(metrics_na$job_satisfaction))
})

test_that("Utility Functions - Date Operations", {
  # Test format_date_for_display
  test_dates <- c("2023-01-15", "2023-12-31", "2024-06-15")
  formatted <- format_date_for_display(test_dates)
  
  expect_equal(length(formatted), 3)
  expect_true(all(nchar(formatted) > 0))
  
  # Test with invalid dates
  invalid_dates <- c("2023-13-45", "not-a-date", "2023/01/15")
  expect_error(format_date_for_display(invalid_dates), "Invalid date format")
  
  # Test calculate_tenure
  hire_dates <- as.Date(c("2020-01-01", "2021-06-15", "2022-12-31"))
  tenure <- calculate_tenure(hire_dates)
  
  expect_true(all(tenure >= 0))
  expect_true(is.numeric(tenure))
})

test_that("Utility Functions - Data Transformation", {
  # Test create_age_groups
  ages <- c(22, 28, 35, 42, 48, 55, 62)
  age_groups <- create_age_groups(ages)
  
  expect_true(all(age_groups %in% c("22-30", "31-40", "41-50", "51-60", "61+")))
  expect_equal(length(age_groups), length(ages))
  
  # Test with edge cases
  edge_ages <- c(22, 30, 31, 40, 41, 50, 51, 60, 61, 70)
  edge_groups <- create_age_groups(edge_ages)
  expect_equal(length(unique(edge_groups)), 5)
  
  # Test normalize_salary_data
  salary_data <- data.frame(
    EmployeeID = 1:5,
    Salary = c(50000, 75000, 100000, 125000, 150000)
  )
  
  normalized <- normalize_salary_data(salary_data)
  expect_true(all(normalized$Salary >= 0 & normalized$Salary <= 1))
})

# =============================================================================
# 4. CUSTOM THEME TESTS
# =============================================================================

test_that("Custom Theme - ggplot2 Themes", {
  # Test theme_atlas function
  theme_obj <- theme_atlas()
  expect_true(inherits(theme_obj, "theme"))
  expect_true(inherits(theme_obj, "gg"))
  
  # Test theme with different variants
  theme_minimal <- theme_atlas(variant = "minimal")
  theme_dark <- theme_atlas(variant = "dark")
  
  expect_true(inherits(theme_minimal, "theme"))
  expect_true(inherits(theme_dark, "theme"))
})

test_that("Custom Theme - Color Palettes", {
  # Test get_atlas_colors function
  colors <- get_atlas_colors()
  expect_true(length(colors) > 0)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  
  # Test specific color requests
  primary_colors <- get_atlas_colors(n = 3, type = "primary")
  expect_equal(length(primary_colors), 3)
  
  # Test color validation
  expect_error(get_atlas_colors(n = 0), "Number of colors must be positive")
  expect_error(get_atlas_colors(n = 100), "Too many colors requested")
})

test_that("Custom Theme - Theme Consistency", {
  # Test that theme elements are properly set
  theme_obj <- theme_atlas()
  
  # Extract theme elements
  theme_elements <- names(theme_obj)
  
  # Check for essential theme elements
  essential_elements <- c("text", "plot.title", "axis.title", "legend.title")
  expect_true(any(grepl("text", theme_elements)))
  expect_true(any(grepl("title", theme_elements)))
})

# =============================================================================
# 5. MODULE UI TESTS
# =============================================================================

test_that("Module UI Functions - Structure Validation", {
  # Test overview module UI
  overview_ui <- overviewUI("test_overview")
  expect_true(inherits(overview_ui, "shiny.tag"))
  
  # Test attrition module UI
  attrition_ui <- attritionUI("test_attrition")
  expect_true(inherits(attrition_ui, "shiny.tag"))
  
  # Test demographics module UI
  demographics_ui <- demographicsUI("test_demographics")
  expect_true(inherits(demographics_ui, "shiny.tag"))
  
  # Test performance module UI
  performance_ui <- performanceUI("test_performance")
  expect_true(inherits(performance_ui, "shiny.tag"))
  
  # Test compensation module UI
  compensation_ui <- compensationUI("test_compensation")
  expect_true(inherits(compensation_ui, "shiny.tag"))
  
  # Test satisfaction module UI
  satisfaction_ui <- satisfactionUI("test_satisfaction")
  expect_true(inherits(satisfaction_ui, "shiny.tag"))
})

test_that("Module UI Functions - ID Validation", {
  # Test that modules properly namespace their IDs
  test_id <- "test_module_123"
  
  overview_ui <- overviewUI(test_id)
  ui_html <- as.character(overview_ui)
  
  # Check that the ID is properly used in the UI
  expect_true(grepl(test_id, ui_html))
  
  # Test with invalid IDs
  expect_error(overviewUI(""), "Module ID cannot be empty")
  expect_error(overviewUI(NULL), "Module ID cannot be NULL")
})

# =============================================================================
# 6. DATA ANALYSIS FUNCTIONS TESTS
# =============================================================================

test_that("Attrition Analysis - Core Functions", {
  # Create test data
  test_data <- data.frame(
    EmployeeID = 1:100,
    Department = sample(c("HR", "Engineering", "Sales", "Marketing"), 100, replace = TRUE),
    Age = sample(25:65, 100, replace = TRUE),
    Salary = sample(40000:150000, 100, replace = TRUE),
    YearsAtCompany = sample(0:20, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8))
  )
  
  # Test analyze_attrition_by_department
  dept_analysis <- analyze_attrition_by_department(test_data)
  expect_true(is.data.frame(dept_analysis))
  expect_true(all(c("Department", "AttritionRate", "EmployeeCount") %in% names(dept_analysis)))
  expect_true(all(dept_analysis$AttritionRate >= 0 & dept_analysis$AttritionRate <= 1))
  
  # Test analyze_attrition_by_age_group
  age_analysis <- analyze_attrition_by_age_group(test_data)
  expect_true(is.data.frame(age_analysis))
  expect_true(all(c("AgeGroup", "AttritionRate") %in% names(age_analysis)))
  
  # Test identify_attrition_risk_factors
  risk_factors <- identify_attrition_risk_factors(test_data)
  expect_true(is.list(risk_factors))
  expect_true(length(risk_factors) > 0)
})

test_that("Attrition Analysis - Edge Cases", {
  # Test with no attrition data
  no_attrition_data <- data.frame(
    EmployeeID = 1:10,
    Department = rep("Engineering", 10),
    Attrition = rep("No", 10)
  )
  
  result <- analyze_attrition_by_department(no_attrition_data)
  expect_true(all(result$AttritionRate == 0))
  
  # Test with all attrition data
  all_attrition_data <- data.frame(
    EmployeeID = 1:10,
    Department = rep("Engineering", 10),
    Attrition = rep("Yes", 10)
  )
  
  result <- analyze_attrition_by_department(all_attrition_data)
  expect_true(all(result$AttritionRate == 1))
  
  # Test with single employee
  single_employee <- data.frame(
    EmployeeID = 1,
    Department = "HR",
    Attrition = "Yes"
  )
  
  result <- analyze_attrition_by_department(single_employee)
  expect_equal(nrow(result), 1)
  expect_equal(result$AttritionRate, 1)
})

test_that("Performance Analysis - Core Functions", {
  # Create test performance data
  performance_data <- data.frame(
    EmployeeID = 1:50,
    JobSatisfaction = sample(1:5, 50, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 50, replace = TRUE),
    WorkLifeBalance = sample(1:5, 50, replace = TRUE),
    SelfRating = sample(1:5, 50, replace = TRUE),
    ManagerRating = sample(1:5, 50, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 50, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 50, replace = TRUE)
  )
  
  # Test calculate_performance_metrics
  perf_metrics <- calculate_performance_metrics(performance_data)
  expect_true(is.list(perf_metrics))
  expect_true(all(c("avg_self_rating", "avg_manager_rating", "rating_correlation") %in% names(perf_metrics)))
  
  # Test analyze_training_effectiveness
  training_analysis <- analyze_training_effectiveness(performance_data)
  expect_true(is.data.frame(training_analysis))
  expect_true(all(c("TrainingTaken", "AvgPerformance") %in% names(training_analysis)))
  
  # Test identify_high_performers
  high_performers <- identify_high_performers(performance_data)
  expect_true(is.vector(high_performers))
  expect_true(all(high_performers %in% performance_data$EmployeeID))
})

test_that("Compensation Analysis - Core Functions", {
  # Create test compensation data
  comp_data <- data.frame(
    EmployeeID = 1:100,
    Gender = sample(c("Male", "Female"), 100, replace = TRUE),
    Department = sample(c("HR", "Engineering", "Sales"), 100, replace = TRUE),
    JobRole = sample(c("Manager", "Senior", "Junior"), 100, replace = TRUE),
    Salary = sample(40000:150000, 100, replace = TRUE),
    StockOptionLevel = sample(0:3, 100, replace = TRUE),
    YearsAtCompany = sample(0:20, 100, replace = TRUE)
  )
  
  # Test analyze_pay_equity
  pay_equity <- analyze_pay_equity(comp_data)
  expect_true(is.list(pay_equity))
  expect_true("gender_pay_gap" %in% names(pay_equity))
  
  # Test create_salary_bands
  salary_bands <- create_salary_bands(comp_data)
  expect_true(is.data.frame(salary_bands))
  expect_true(all(c("SalaryBand", "Count", "AvgSalary") %in% names(salary_bands)))
  
  # Test analyze_compensation_trends
  comp_trends <- analyze_compensation_trends(comp_data)
  expect_true(is.data.frame(comp_trends))
})

# =============================================================================
# 7. VISUALIZATION TESTS
# =============================================================================

test_that("Visualization Functions - Chart Creation", {
  # Create test data for visualizations
  viz_data <- data.frame(
    Department = sample(c("HR", "Engineering", "Sales"), 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    Salary = sample(40000:150000, 100, replace = TRUE),
    Age = sample(25:65, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE)
  )
  
  # Test create_attrition_chart
  attrition_chart <- create_attrition_chart(viz_data)
  expect_true(inherits(attrition_chart, "ggplot"))
  
  # Test create_salary_distribution_chart
  salary_chart <- create_salary_distribution_chart(viz_data)
  expect_true(inherits(salary_chart, "ggplot"))
  
  # Test create_satisfaction_radar_chart
  satisfaction_chart <- create_satisfaction_radar_chart(viz_data)
  expect_true(inherits(satisfaction_chart, "ggplot"))
})

test_that("Visualization Functions - Plotly Integration", {
  # Test plotly conversion
  viz_data <- data.frame(
    x = 1:10,
    y = sample(1:100, 10),
    category = sample(c("A", "B", "C"), 10, replace = TRUE)
  )
  
  gg_plot <- ggplot(viz_data, aes(x = x, y = y, color = category)) +
    geom_point() +
    theme_atlas()
  
  plotly_chart <- convert_to_plotly(gg_plot)
  expect_true(inherits(plotly_chart, "plotly"))
})

test_that("Visualization Functions - Edge Cases", {
  # Test with empty data
  empty_data <- data.frame()
  expect_error(create_attrition_chart(empty_data), "No data provided")
  
  # Test with single row
  single_row <- data.frame(
    Department = "HR",
    Attrition = "Yes",
    Salary = 50000
  )
  
  chart <- create_attrition_chart(single_row)
  expect_true(inherits(chart, "ggplot"))
  
  # Test with missing columns
  incomplete_data <- data.frame(x = 1:5)
  expect_error(create_attrition_chart(incomplete_data), "Required columns missing")
})

# =============================================================================
# 8. REACTIVE BEHAVIOR TESTS
# =============================================================================

test_that("Reactive Behavior - Mock Shiny Session", {
  # Create mock reactive values
  mock_session <- MockShinySession$new()
  
  # Test reactive value updates
  test_reactive <- reactive({
    input$test_input * 2
  })
  
  # Simulate input change
  mock_session$setInputs(test_input = 5)
  
  # Test reactive invalidation
  expect_equal(isolate(test_reactive()), 10)
  
  # Test reactive dependency
  mock_session$setInputs(test_input = 10)
  expect_equal(isolate(test_reactive()), 20)
})

test_that("Reactive Behavior - Module Communication", {
  # Test bidirectional communication between modules
  shared_values <- reactiveValues(
    selected_filters = list(),
    current_data = NULL,
    analysis_results = NULL
  )
  
  # Simulate filter update
  shared_values$selected_filters <- list(
    department = c("HR", "Engineering"),
    age_range = c(25, 45)
  )
  
  # Test that values are properly updated
  expect_equal(length(shared_values$selected_filters), 2)
  expect_true("department" %in% names(shared_values$selected_filters))
  expect_true("age_range" %in% names(shared_values$selected_filters))
})

# =============================================================================
# 9. INPUT VALIDATION TESTS
# =============================================================================

test_that("Input Validation - User Input Sanitization", {
  # Test numeric input validation
  expect_true(validate_numeric_input("123"))
  expect_true(validate_numeric_input("123.45"))
  expect_false(validate_numeric_input("abc"))
  expect_false(validate_numeric_input("123abc"))
  
  # Test date input validation
  expect_true(validate_date_input("2023-01-01"))
  expect_true(validate_date_input("2023-12-31"))
  expect_false(validate_date_input("2023-13-01"))
  expect_false(validate_date_input("not-a-date"))
  
  # Test text input sanitization
  sanitized <- sanitize_text_input("<script>alert('xss')</script>")
  expect_false(grepl("<script>", sanitized))
  expect_false(grepl("alert", sanitized))
})

test_that("Input Validation - Filter Validation", {
  # Test department filter validation
  valid_departments <- c("HR", "Engineering", "Sales", "Marketing")
  
  expect_true(validate_department_filter(c("HR", "Engineering"), valid_departments))
  expect_false(validate_department_filter(c("InvalidDept"), valid_departments))
  expect_false(validate_department_filter(c(), valid_departments))
  
  # Test date range validation
  expect_true(validate_date_range("2023-01-01", "2023-12-31"))
  expect_false(validate_date_range("2023-12-31", "2023-01-01"))  # End before start
  expect_false(validate_date_range("invalid", "2023-12-31"))
  
  # Test salary range validation
  expect_true(validate_salary_range(30000, 200000))
  expect_false(validate_salary_range(200000, 30000))  # Max less than min
  expect_false(validate_salary_range(-1000, 100000))  # Negative salary
})

# =============================================================================
# 10. ERROR HANDLING TESTS
# =============================================================================

test_that("Error Handling - Graceful Degradation", {
  # Test handling of missing data files
  expect_error(
    load_data_with_fallback("nonexistent_file.csv"),
    "Data file not found and no fallback available"
  )
  
  # Test handling of corrupted data
  corrupted_data <- data.frame(
    EmployeeID = c(1, 2, NA, 4),
    Salary = c(50000, NA, -1000, 1000000)
  )
  
  cleaned_data <- handle_data_corruption(corrupted_data)
  expect_true(nrow(cleaned_data) > 0)
  expect_true(all(cleaned_data$Salary > 0, na.rm = TRUE))
})

test_that("Error Handling - User-Friendly Messages", {
  # Test error message formatting
  technical_error <- "Object 'undefined_variable' not found"
  user_message <- format_user_error_message(technical_error)
  
  expect_false(grepl("Object", user_message))
  expect_true(grepl("please", user_message, ignore.case = TRUE))
  
  # Test validation error messages
  validation_errors <- list(
    "Invalid date format in HireDate column",
    "Negative salary values detected",
    "Duplicate employee IDs found"
  )
  
  formatted_errors <- format_validation_errors(validation_errors)
  expect_true(length(formatted_errors) == 3