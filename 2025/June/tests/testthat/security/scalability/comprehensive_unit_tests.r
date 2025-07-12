# ============================================================================
# Atlas Labs HR Analytics Dashboard - Comprehensive Unit Tests
# ============================================================================
# Coverage: Data validation, Module functionality, UI/UX, Performance, Security
# Excluded: Scalability testing (horizontal/vertical scaling, auto-scaling, etc.)
# ============================================================================

# Required libraries for testing
library(testthat)
library(shiny)
library(shinydashboard)
library(mockery)
library(withr)
library(DT)
library(plotly)
library(tidyverse)
library(R6)
library(profvis)
library(bench)
library(fs)
library(yaml)

# Source application files
source("global.R")
source("utils.R")
source("custom_theme.R")
source("modules/logger_module.R")
source("modules/data_loader_module.R")

# ============================================================================
# 1. DATA VALIDATION & INTEGRITY TESTS
# ============================================================================

test_that("Data Validation - Employee Data Structure", {
  # Test valid employee data structure
  valid_employee_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Gender = c("Male", "Female", "Male"),
    Age = c(30, 25, 40),
    BusinessTravel = c("Travel_Rarely", "Travel_Frequently", "Non-Travel"),
    Department = c("Sales", "HR", "Technology"),
    DistanceFromHome = c(10, 15, 5),
    State = c("NY", "CA", "TX"),
    Ethnicity = c("White", "Hispanic", "Black"),
    Education = c(3, 4, 2),
    EducationField = c("Life Sciences", "Marketing", "Technical Degree"),
    JobRole = c("Sales Executive", "HR", "Software Engineer"),
    MaritalStatus = c("Married", "Single", "Divorced"),
    Salary = c(65000, 55000, 85000),
    StockOptionLevel = c(1, 0, 2),
    OverTime = c("Yes", "No", "Yes"),
    HireDate = as.Date(c("2020-01-15", "2021-03-20", "2019-06-10")),
    Attrition = c("No", "Yes", "No"),
    YearsAtCompany = c(4, 3, 5),
    YearsInMostRecentRole = c(2, 1, 3),
    YearsSinceLastPromotion = c(1, 2, 0),
    YearsWithCurrManager = c(2, 1, 2),
    stringsAsFactors = FALSE
  )
  
  # Test data structure validation
  expect_true(validate_employee_data_structure(valid_employee_data))
  expect_equal(nrow(valid_employee_data), 3)
  expect_equal(ncol(valid_employee_data), 23)
  
  # Test required columns
  required_cols <- c("EmployeeID", "FirstName", "LastName", "Age", "Department", 
                    "Salary", "Attrition", "HireDate")
  expect_true(all(required_cols %in% names(valid_employee_data)))
})

test_that("Data Validation - Performance Rating Data", {
  valid_performance_data <- data.frame(
    PerformanceID = c("PERF001", "PERF002", "PERF003"),
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    ReviewDate = as.Date(c("2023-12-01", "2023-11-15", "2023-10-30")),
    EnvironmentSatisfaction = c(4, 3, 5),
    JobSatisfaction = c(4, 2, 5),
    RelationshipSatisfaction = c(3, 4, 5),
    WorkLifeBalance = c(3, 2, 4),
    SelfRating = c(4, 3, 5),
    ManagerRating = c(4, 3, 4),
    TrainingOpportunitiesWithinYear = c(3, 2, 4),
    TrainingOpportunitiesTaken = c(2, 1, 3),
    stringsAsFactors = FALSE
  )
  
  expect_true(validate_performance_data_structure(valid_performance_data))
  expect_true(all(valid_performance_data$EnvironmentSatisfaction %in% 1:5))
  expect_true(all(valid_performance_data$JobSatisfaction %in% 1:5))
  expect_true(all(valid_performance_data$WorkLifeBalance %in% 1:5))
})

test_that("Data Validation - Edge Cases & Error Handling", {
  # Empty data frame
  empty_data <- data.frame()
  expect_error(validate_employee_data_structure(empty_data))
  
  # Missing required columns
  incomplete_data <- data.frame(
    EmployeeID = c("EMP001"),
    FirstName = c("John")
  )
  expect_error(validate_employee_data_structure(incomplete_data))
  
  # Invalid data types
  invalid_data <- data.frame(
    EmployeeID = c("EMP001"),
    Age = c("thirty"), # Should be numeric
    Salary = c("high") # Should be numeric
  )
  expect_error(validate_employee_data_structure(invalid_data))
  
  # Negative values where not allowed
  negative_data <- data.frame(
    EmployeeID = c("EMP001"),
    Age = c(-25),
    Salary = c(-50000)
  )
  expect_error(validate_employee_data_structure(negative_data))
})

test_that("Data Validation - Data Integrity Checks", {
  # Duplicate employee IDs
  duplicate_data <- data.frame(
    EmployeeID = c("EMP001", "EMP001", "EMP002"),
    FirstName = c("John", "Jane", "Bob"),
    Age = c(30, 25, 40)
  )
  expect_error(validate_data_integrity(duplicate_data))
  
  # Future hire dates
  future_date_data <- data.frame(
    EmployeeID = c("EMP001"),
    HireDate = as.Date("2030-01-01")
  )
  expect_warning(validate_data_integrity(future_date_data))
  
  # Inconsistent tenure calculations
  inconsistent_tenure <- data.frame(
    EmployeeID = c("EMP001"),
    HireDate = as.Date("2020-01-01"),
    YearsAtCompany = c(10) # Should be ~4 years
  )
  expect_warning(validate_data_integrity(inconsistent_tenure))
})

# ============================================================================
# 2. MODULE FUNCTIONALITY TESTS
# ============================================================================

test_that("Logger Module - R6 Class Functionality", {
  # Create logger instance
  logger <- AtlasLogger$new(app_name = "test_app")
  
  # Test basic logging
  expect_silent(logger$log_info("Test info message", "test_module"))
  expect_silent(logger$log_warning("Test warning", "test_module"))
  expect_silent(logger$log_error("Test error", "test_module"))
  
  # Test log retrieval
  logs <- logger$get_logs()
  expect_true(is.data.frame(logs))
  expect_true(nrow(logs) >= 3)
  expect_true(all(c("timestamp", "level", "message", "module") %in% names(logs)))
  
  # Test performance tracking
  logger$start_performance_tracking("test_operation")
  Sys.sleep(0.1)
  perf_data <- logger$stop_performance_tracking("test_operation")
  
  expect_true(is.numeric(perf_data$duration))
  expect_true(perf_data$duration > 0)
})

test_that("Logger Module - Memory Usage Tracking", {
  logger <- AtlasLogger$new()
  
  # Test memory usage tracking
  initial_memory <- logger$get_memory_usage()
  expect_true(is.numeric(initial_memory))
  expect_true(initial_memory > 0)
  
  # Create large object and track memory change
  large_object <- matrix(runif(10000), nrow = 100)
  new_memory <- logger$get_memory_usage()
  
  expect_true(new_memory >= initial_memory)
  
  # Clean up
  rm(large_object)
  gc()
})

test_that("Logger Module - Error Handling & Edge Cases", {
  logger <- AtlasLogger$new()
  
  # Test with NULL inputs
  expect_error(logger$log_info(NULL, "test"))
  expect_error(logger$log_info("message", NULL))
  
  # Test with empty strings
  expect_silent(logger$log_info("", "test"))
  expect_silent(logger$log_info("message", ""))
  
  # Test with special characters
  expect_silent(logger$log_info("Test with special chars: !@#$%^&*()", "test"))
  
  # Test performance tracking edge cases
  expect_error(logger$stop_performance_tracking("non_existent_operation"))
})

test_that("Data Loader Module - File Loading", {
  # Create temporary test files
  temp_dir <- tempdir()
  
  # Create test employee data
  test_employee <- data.frame(
    EmployeeID = c("EMP001", "EMP002"),
    FirstName = c("John", "Jane"),
    LastName = c("Doe", "Smith"),
    Age = c(30, 25),
    Department = c("Sales", "HR"),
    Salary = c(65000, 55000),
    Attrition = c("No", "Yes"),
    HireDate = as.Date(c("2020-01-15", "2021-03-20"))
  )
  
  employee_file <- file.path(temp_dir, "employee.csv")
  write.csv(test_employee, employee_file, row.names = FALSE)
  
  # Test file loading
  loaded_data <- load_employee_data(employee_file)
  expect_equal(nrow(loaded_data), 2)
  expect_equal(loaded_data$EmployeeID, c("EMP001", "EMP002"))
  
  # Test non-existent file
  expect_error(load_employee_data("non_existent_file.csv"))
  
  # Test corrupted file
  corrupted_file <- file.path(temp_dir, "corrupted.csv")
  writeLines("invalid,csv,content", corrupted_file)
  expect_error(load_employee_data(corrupted_file))
  
  # Clean up
  unlink(c(employee_file, corrupted_file))
})

test_that("Data Loader Module - Data Merging", {
  # Create test datasets
  employee_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    FirstName = c("John", "Jane", "Bob"),
    Department = c("Sales", "HR", "Tech")
  )
  
  performance_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002"),
    JobSatisfaction = c(4, 3),
    WorkLifeBalance = c(3, 4)
  )
  
  education_data <- data.frame(
    Education = c(1, 2, 3),
    EducationLevel = c("High School", "Bachelor", "Master")
  )
  
  # Test successful merge
  merged_data <- merge_datasets(employee_data, performance_data, education_data)
  expect_true(is.data.frame(merged_data))
  expect_true("JobSatisfaction" %in% names(merged_data))
  
  # Test merge with missing employees
  expect_equal(sum(is.na(merged_data$JobSatisfaction)), 1) # EMP003 missing
  
  # Test merge with empty performance data
  empty_performance <- data.frame(
    EmployeeID = character(0),
    JobSatisfaction = numeric(0)
  )
  merged_empty <- merge_datasets(employee_data, empty_performance, education_data)
  expect_equal(nrow(merged_empty), 3)
  expect_true(all(is.na(merged_empty$JobSatisfaction)))
})

# ============================================================================
# 3. UI/UX FUNCTIONALITY TESTS
# ============================================================================

test_that("UI Components - Module UI Generation", {
  # Test overview module UI
  overview_ui <- overviewUI("test_overview")
  expect_s3_class(overview_ui, "shiny.tag")
  
  # Test attrition module UI
  attrition_ui <- attritionUI("test_attrition")
  expect_s3_class(attrition_ui, "shiny.tag")
  
  # Test demographics module UI
  demographics_ui <- demographicsUI("test_demographics")
  expect_s3_class(demographics_ui, "shiny.tag")
  
  # Test that UIs contain expected elements
  expect_true(grepl("test_overview", as.character(overview_ui)))
  expect_true(grepl("test_attrition", as.character(attrition_ui)))
})

test_that("UI Components - Input Validation", {
  # Test filter input validation
  valid_filters <- list(
    department = c("Sales", "HR", "Technology"),
    age_range = c(25, 65),
    salary_range = c(40000, 120000)
  )
  
  expect_true(validate_filter_inputs(valid_filters))
  
  # Test invalid filters
  invalid_filters <- list(
    department = c("InvalidDept"),
    age_range = c(-5, 200),
    salary_range = c("low", "high")
  )
  
  expect_false(validate_filter_inputs(invalid_filters))
})

test_that("UI Components - Responsive Design", {
  # Test responsive breakpoints
  expect_true(is_mobile_device("Mozilla/5.0 (iPhone; CPU iPhone OS 14_0)"))
  expect_false(is_mobile_device("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
  
  # Test CSS class generation for responsive design
  mobile_classes <- get_responsive_classes(TRUE)
  desktop_classes <- get_responsive_classes(FALSE)
  
  expect_true(grepl("mobile", mobile_classes))
  expect_true(grepl("desktop", desktop_classes))
})

test_that("UI Components - Accessibility", {
  # Test ARIA labels
  aria_element <- create_accessible_element("Test Button", "button")
  expect_true(grepl("aria-label", as.character(aria_element)))
  expect_true(grepl("role", as.character(aria_element)))
  
  # Test color contrast validation
  expect_true(validate_color_contrast("#000000", "#FFFFFF")) # High contrast
  expect_false(validate_color_contrast("#CCCCCC", "#FFFFFF")) # Low contrast
  
  # Test keyboard navigation
  nav_element <- create_keyboard_navigable_element("nav_item")
  expect_true(grepl("tabindex", as.character(nav_element)))
})

# ============================================================================
# 4. VISUALIZATION & CHART TESTS
# ============================================================================

test_that("Visualization - Chart Generation", {
  # Create test data
  test_data <- data.frame(
    Department = c("Sales", "HR", "Technology", "Marketing"),
    AttritionRate = c(0.15, 0.08, 0.12, 0.10),
    EmployeeCount = c(50, 20, 80, 30)
  )
  
  # Test attrition chart creation
  attrition_chart <- create_attrition_chart(test_data)
  expect_s3_class(attrition_chart, "plotly")
  
  # Test demographic chart creation
  demo_chart <- create_demographic_chart(test_data)
  expect_s3_class(demo_chart, "plotly")
  
  # Test performance chart creation
  performance_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    SelfRating = c(4, 3, 5),
    ManagerRating = c(4, 3, 4)
  )
  
  performance_chart <- create_performance_chart(performance_data)
  expect_s3_class(performance_chart, "plotly")
})

test_that("Visualization - Chart Customization", {
  test_data <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(10, 20, 15, 25)
  )
  
  # Test custom theme application
  themed_chart <- apply_atlas_theme(test_data)
  expect_s3_class(themed_chart, "ggplot")
  
  # Test color palette application
  colored_chart <- apply_atlas_colors(themed_chart)
  expect_s3_class(colored_chart, "ggplot")
  
  # Test interactive features
  interactive_chart <- make_chart_interactive(colored_chart)
  expect_s3_class(interactive_chart, "plotly")
})

test_that("Visualization - Error Handling", {
  # Test with empty data
  empty_data <- data.frame()
  expect_error(create_attrition_chart(empty_data))
  
  # Test with missing required columns
  incomplete_data <- data.frame(x = c(1, 2, 3))
  expect_error(create_attrition_chart(incomplete_data))
  
  # Test with invalid data types
  invalid_data <- data.frame(
    Department = c("Sales", "HR"),
    AttritionRate = c("high", "low") # Should be numeric
  )
  expect_error(create_attrition_chart(invalid_data))
})

# ============================================================================
# 5. BUSINESS LOGIC TESTS
# ============================================================================

test_that("Business Logic - Attrition Calculations", {
  # Test attrition rate calculation
  employee_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003", "EMP004"),
    Department = c("Sales", "Sales", "HR", "HR"),
    Attrition = c("Yes", "No", "Yes", "No")
  )
  
  attrition_rates <- calculate_attrition_rates(employee_data)
  
  expect_equal(attrition_rates$Sales, 0.5) # 1 out of 2
  expect_equal(attrition_rates$HR, 0.5) # 1 out of 2
  
  # Test overall attrition rate
  overall_rate <- calculate_overall_attrition_rate(employee_data)
  expect_equal(overall_rate, 0.5) # 2 out of 4
})

test_that("Business Logic - Salary Analysis", {
  salary_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003", "EMP004"),
    Gender = c("Male", "Female", "Male", "Female"),
    Department = c("Sales", "Sales", "HR", "HR"),
    Salary = c(65000, 60000, 55000, 58000)
  )
  
  # Test pay gap analysis
  pay_gap <- calculate_pay_gap(salary_data)
  expect_true(is.numeric(pay_gap$gender_gap))
  expect_true(is.numeric(pay_gap$department_gap))
  
  # Test salary percentiles
  percentiles <- calculate_salary_percentiles(salary_data)
  expect_true(all(c("p25", "p50", "p75", "p90") %in% names(percentiles)))
})

test_that("Business Logic - Performance Metrics", {
  performance_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    SelfRating = c(4, 3, 5),
    ManagerRating = c(4, 3, 4),
    JobSatisfaction = c(4, 2, 5)
  )
  
  # Test performance correlation
  correlations <- calculate_performance_correlations(performance_data)
  expect_true(is.numeric(correlations$self_manager))
  expect_true(is.numeric(correlations$performance_satisfaction))
  
  # Test performance categories
  categories <- categorize_performance(performance_data)
  expect_true(all(categories %in% c("Low", "Medium", "High")))
})

test_that("Business Logic - Edge Cases", {
  # Test with single employee
  single_employee <- data.frame(
    EmployeeID = "EMP001",
    Department = "Sales",
    Attrition = "No",
    Salary = 65000
  )
  
  single_attrition <- calculate_attrition_rates(single_employee)
  expect_equal(single_attrition$Sales, 0)
  
  # Test with all employees having same values
  uniform_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    Salary = c(65000, 65000, 65000)
  )
  
  uniform_percentiles <- calculate_salary_percentiles(uniform_data)
  expect_true(all(uniform_percentiles == 65000))
})

# ============================================================================
# 6. PERFORMANCE TESTS
# ============================================================================

test_that("Performance - Data Processing Speed", {
  # Create large dataset for performance testing
  large_data <- data.frame(
    EmployeeID = paste0("EMP", 1:10000),
    Department = sample(c("Sales", "HR", "Technology", "Marketing"), 10000, replace = TRUE),
    Salary = runif(10000, 40000, 120000),
    Attrition = sample(c("Yes", "No"), 10000, replace = TRUE, prob = c(0.15, 0.85))
  )
  
  # Test processing time
  processing_time <- system.time({
    attrition_rates <- calculate_attrition_rates(large_data)
  })
  
  # Should process 10k records in reasonable time (< 1 second)
  expect_true(processing_time["elapsed"] < 1.0)
  
  # Test memory efficiency
  memory_before <- pryr::mem_used()
  result <- calculate_salary_percentiles(large_data)
  memory_after <- pryr::mem_used()
  
  # Memory increase should be reasonable
  memory_increase <- as.numeric(memory_after - memory_before)
  expect_true(memory_increase < 50000000) # Less than 50MB
})

test_that("Performance - Chart Rendering Speed", {
  # Test chart rendering performance
  chart_data <- data.frame(
    x = 1:1000,
    y = runif(1000),
    category = sample(letters[1:5], 1000, replace = TRUE)
  )
  
  rendering_time <- system.time({
    chart <- create_performance_chart(chart_data)
  })
  
  # Chart rendering should be fast
  expect_true(rendering_time["elapsed"] < 2.0)
})

test_that("Performance - Memory Leaks", {
  # Test for memory leaks in repeated operations
  initial_memory <- pryr::mem_used()
  
  # Perform repeated operations
  for (i in 1:100) {
    test_data <- data.frame(
      x = runif(100),
      y = runif(100)
    )
    
    # Simulate chart creation and disposal
    chart <- create_test_chart(test_data)
    rm(chart)
  }
  
  # Force garbage collection
  gc()
  
  final_memory <- pryr::mem_used()
  memory_increase <- as.numeric(final_memory - initial_memory)
  
  # Memory increase should be minimal (< 10MB)
  expect_true(memory_increase < 10000000)
})

# ============================================================================
# 7. SECURITY TESTS
# ============================================================================

test_that("Security - Input Sanitization", {
  # Test SQL injection prevention
  malicious_input <- "'; DROP TABLE employees; --"
  sanitized <- sanitize_input(malicious_input)
  expect_false(grepl("DROP", sanitized))
  expect_false(grepl("--", sanitized))
  
  # Test XSS prevention
  xss_input <- "<script>alert('XSS')</script>"
  sanitized_xss <- sanitize_input(xss_input)
  expect_false(grepl("<script>", sanitized_xss))
  
  # Test path traversal prevention
  path_input <- "../../etc/passwd"
  sanitized_path <- sanitize_file_path(path_input)
  expect_false(grepl("\\.\\.", sanitized_path))
})

test_that("Security - Data Access Control", {
  # Test role-based access
  user_roles <- c("admin", "hr_manager", "employee")
  
  # Admin should have full access
  admin_permissions <- get_user_permissions("admin")
  expect_true(admin_permissions$can_view_salary)
  expect_true(admin_permissions$can_export_data)
  
  # Employee should have limited access
  employee_permissions <- get_user_permissions("employee")
  expect_false(employee_permissions$can_view_salary)
  expect_false(employee_permissions$can_export_data)
  
  # Test data filtering by role
  sensitive_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002"),
    Salary = c(65000, 70000),
    Performance = c("High", "Medium")
  )
  
  filtered_data <- filter_data_by_role(sensitive_data, "employee")
  expect_false("Salary" %in% names(filtered_data))
})

test_that("Security - Session Management", {
  # Test session timeout
  session_data <- list(
    user_id = "user123",
    login_time = Sys.time() - 3600, # 1 hour ago
    last_activity = Sys.time() - 1800 # 30 minutes ago
  )
  
  expect_true(is_session_expired(session_data, timeout_minutes = 30))
  expect_false(is_session_expired(session_data, timeout_minutes = 60))
  
  # Test session token validation
  valid_token <- generate_session_token("user123")
  expect_true(validate_session_token(valid_token, "user123"))
  
  invalid_token <- "invalid_token_123"
  expect_false(validate_session_token(invalid_token, "user123"))
})

# ============================================================================
# 8. INTEGRATION TESTS
# ============================================================================

test_that("Integration - Module Communication", {
  # Test data flow between modules
  test_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002"),
    Department = c("Sales", "HR"),
    Attrition = c("Yes", "No")
  )
  
  # Test overview module processing
  overview_result <- process_overview_data(test_data)
  expect_true(is.list(overview_result))
  expect_true("total_employees" %in% names(overview_result))
  
  # Test attrition module processing same data
  attrition_result <- process_attrition_data(test_data)
  expect_true(is.list(attrition_result))
  expect_true("attrition_rate" %in% names(attrition_result))
  
  # Test consistency between modules
  expect_equal(overview_result$total_employees, 2)
  expect_equal(attrition_result$attrition_rate, 0.5)
})

test_that("Integration - Report Generation", {
  # Test end-to-end report generation
  mock_data <- list(
    employee_data = data.frame(
      EmployeeID = c("EMP001", "EMP002"),
      Department = c("Sales", "HR"),
      Salary = c(65000, 55000)
    ),
    performance_data = data.frame(
      EmployeeID = c("EMP001", "EMP002"),
      JobSatisfaction = c(4, 3)
    )
  )
  
  # Test report parameter generation
  report_params <- generate_report_params(mock_data)
  expect_true(is.list(report_params))
  expect_true("kpi_metrics" %in% names(report_params))
  
  # Test report rendering (mock)
  temp_report <- tempfile(fileext = ".html")
  expect_silent(render_report(report_params, temp_report))
  expect_true(file.exists(temp_report))
  
  # Clean up
  unlink(temp_report)
})

# ============================================================================
# 9. ERROR HANDLING & EDGE CASES
# ============================================================================

test_that("Error Handling - Graceful Degradation", {
  # Test handling of missing data files
  expect_error(load_employee_data("nonexistent.csv"))
  
  # Test handling of corrupted data
  corrupted_data <- data.frame(
    EmployeeID = c(NA, "EMP002"),
    Department = c("Sales", NA),
    Salary = c(-1000, "invalid")
  )
  
  cleaned_data <- handle_corrupted_data(corrupted_data)
  expect_true(nrow(cleaned_data) <= nrow(corrupted_data))
  expect_true(all(!is.na(cleaned_data$EmployeeID)))
  
  # Test handling of network failures (mock)
  mock_network_failure <- function() stop("Network error")
  
  result <- safely_execute(mock_network_failure)
  expect_true(is.null(result$result))
  expect_true(!is.null(result$error))
})

test_that("Error Handling - User Input Validation", {
  # Test invalid date ranges
  invalid_dates <- list(
    start_date = "2024-01-01",
    end_date = "2023-01-01" # End before start
  )
  
  expect_error(validate_date_range(invalid_dates))
  
  # Test invalid numeric ranges
  invalid_range <- list(
    min_value = 100,
    max_value = 50 # Max less than min
  )
  
  expect_error(validate_numeric_range(invalid_range))
  
  # Test invalid filter combinations
  invalid_filters <- list(
    department = "NonexistentDept",
    salary_range = c(-1000, 1000000)
  )
  
  expect_error(validate_filter_combination(invalid_filters))
})

# ============================================================================
# 10. REGRESSION TESTS
# ============================================================================

test_that("Regression - Previous Bug Fixes", {
  # Test fix for division by zero in attrition calculation
  zero_employees <- data.frame(
    Department = character(0),
    Attrition = character(0)
  )
  
  expect_silent(calculate_attrition_rates(zero_employees))
  
  # Test fix for memory leak in chart generation
  for (i in 1:10) {
    test_chart <- create_test_