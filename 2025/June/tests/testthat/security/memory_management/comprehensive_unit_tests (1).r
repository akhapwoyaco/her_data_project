# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Developer: akhapwoyaco
# Comprehensive testing suite covering all aspects except memory management
# =============================================================================

# Load required libraries for testing
library(testthat)
library(shiny)
library(shintest2)
library(mockery)
library(withr)
library(R6)
library(tidyverse)
library(plotly)
library(DT)
library(here)

# Source application files
source("global.R")
source("utils.R")
source("custom_theme.R")
purrr::walk(list.files("modules", full.names = TRUE), source)

# =============================================================================
# 1. DATA LOADER MODULE TESTS
# =============================================================================

test_that("Data Loader - File Loading and Validation", {
  
  # Test 1.1: Valid CSV file loading
  test_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(EmployeeID = 1:3, Name = c("John", "Jane", "Bob")), 
            test_csv, row.names = FALSE)
  
  expect_no_error(load_employee_data(test_csv))
  result <- load_employee_data(test_csv)
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  
  # Test 1.2: Non-existent file handling
  expect_error(load_employee_data("nonexistent.csv"))
  
  # Test 1.3: Empty file handling
  empty_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(), empty_csv, row.names = FALSE)
  expect_error(load_employee_data(empty_csv))
  
  # Test 1.4: Corrupted file handling
  corrupted_csv <- tempfile(fileext = ".csv")
  writeLines("invalid,csv,content\n1,2", corrupted_csv)
  expect_error(load_employee_data(corrupted_csv))
  
  # Test 1.5: Large file handling simulation
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Name = paste0("Employee", 1:10000),
    Salary = runif(10000, 30000, 100000)
  )
  large_csv <- tempfile(fileext = ".csv")
  write.csv(large_data, large_csv, row.names = FALSE)
  
  expect_no_error(load_employee_data(large_csv))
  large_result <- load_employee_data(large_csv)
  expect_equal(nrow(large_result), 10000)
  
  # Test 1.6: Special characters in data
  special_data <- data.frame(
    Name = c("José", "François", "北京", "مرحبا"),
    Department = c("IT", "HR", "Finance", "Marketing")
  )
  special_csv <- tempfile(fileext = ".csv")
  write.csv(special_data, special_csv, row.names = FALSE)
  
  expect_no_error(load_employee_data(special_csv))
  special_result <- load_employee_data(special_csv)
  expect_equal(nrow(special_result), 4)
  
  # Test 1.7: Missing columns handling
  incomplete_data <- data.frame(EmployeeID = 1:3)
  incomplete_csv <- tempfile(fileext = ".csv")
  write.csv(incomplete_data, incomplete_csv, row.names = FALSE)
  
  expect_error(validate_data_integrity(read.csv(incomplete_csv)))
  
  # Test 1.8: Duplicate employee IDs
  duplicate_data <- data.frame(
    EmployeeID = c(1, 2, 2, 3),
    Name = c("John", "Jane", "Jane2", "Bob")
  )
  duplicate_csv <- tempfile(fileext = ".csv")
  write.csv(duplicate_data, duplicate_csv, row.names = FALSE)
  
  expect_error(validate_data_integrity(read.csv(duplicate_csv)))
  
  # Test 1.9: Data type validation
  invalid_types <- data.frame(
    EmployeeID = c("A", "B", "C"),
    Age = c("twenty", "thirty", "forty")
  )
  invalid_csv <- tempfile(fileext = ".csv")
  write.csv(invalid_types, invalid_csv, row.names = FALSE)
  
  expect_error(validate_data_integrity(read.csv(invalid_csv)))
  
  # Test 1.10: Date format validation
  date_data <- data.frame(
    EmployeeID = 1:3,
    HireDate = c("2020-01-01", "invalid-date", "2021-12-31")
  )
  date_csv <- tempfile(fileext = ".csv")
  write.csv(date_data, date_csv, row.names = FALSE)
  
  expect_error(validate_data_integrity(read.csv(date_csv)))
  
  # Cleanup
  unlink(c(test_csv, empty_csv, corrupted_csv, large_csv, special_csv, 
           incomplete_csv, duplicate_csv, invalid_csv, date_csv))
})

test_that("Data Loader - Data Merging and Relationships", {
  
  # Test 2.1: Successful data merging
  employee_data <- data.frame(
    EmployeeID = 1:3,
    Name = c("John", "Jane", "Bob"),
    Department = c("IT", "HR", "Finance")
  )
  
  performance_data <- data.frame(
    EmployeeID = 1:3,
    JobSatisfaction = c(4, 5, 3),
    PerformanceRating = c(3, 4, 5)
  )
  
  merged_data <- merge_datasets(employee_data, performance_data)
  expect_equal(nrow(merged_data), 3)
  expect_true(all(c("Name", "JobSatisfaction") %in% names(merged_data)))
  
  # Test 2.2: Partial data merging (left join)
  partial_performance <- data.frame(
    EmployeeID = 1:2,
    JobSatisfaction = c(4, 5)
  )
  
  partial_merged <- merge_datasets(employee_data, partial_performance)
  expect_equal(nrow(partial_merged), 3)
  expect_true(is.na(partial_merged$JobSatisfaction[3]))
  
  # Test 2.3: No matching records
  no_match_performance <- data.frame(
    EmployeeID = 4:6,
    JobSatisfaction = c(4, 5, 3)
  )
  
  no_match_merged <- merge_datasets(employee_data, no_match_performance)
  expect_equal(nrow(no_match_merged), 3)
  expect_true(all(is.na(no_match_merged$JobSatisfaction)))
  
  # Test 2.4: Education level lookup
  education_lookup <- data.frame(
    Education = 1:4,
    EducationLevel = c("High School", "Bachelor", "Master", "PhD")
  )
  
  employee_with_education <- data.frame(
    EmployeeID = 1:3,
    Name = c("John", "Jane", "Bob"),
    Education = c(2, 3, 4)
  )
  
  education_merged <- merge_datasets(employee_with_education, education_lookup, 
                                   by.x = "Education", by.y = "Education")
  expect_equal(nrow(education_merged), 3)
  expect_true("EducationLevel" %in% names(education_merged))
})

# =============================================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# =============================================================================

test_that("Logger Module - R6 Class Functionality", {
  
  # Test 3.1: Logger initialization
  logger <- AtlasLogger$new()
  expect_r6(logger, "AtlasLogger")
  expect_true(logger$is_initialized)
  
  # Test 3.2: Basic logging functionality
  expect_no_error(logger$log_info("Test message", "test_module"))
  expect_no_error(logger$log_warning("Warning message", "test_module"))
  expect_no_error(logger$log_error("Error message", "test_module"))
  
  # Test 3.3: Log level filtering
  logger$set_log_level("ERROR")
  logger$log_info("This should be filtered", "test_module")
  logger$log_error("This should appear", "test_module")
  
  logs <- logger$get_logs()
  expect_true(any(grepl("This should appear", logs)))
  expect_false(any(grepl("This should be filtered", logs)))
  
  # Test 3.4: Performance tracking
  start_time <- Sys.time()
  Sys.sleep(0.1)
  perf_data <- list(
    execution_time = as.numeric(Sys.time() - start_time),
    cpu_usage = 45.2,
    memory_used = 1024000
  )
  
  expect_no_error(logger$log_performance("test_function", perf_data))
  
  # Test 3.5: Module-specific logging
  logger$log_info("Module A message", "module_a")
  logger$log_info("Module B message", "module_b")
  
  module_a_logs <- logger$get_module_logs("module_a")
  expect_true(any(grepl("Module A message", module_a_logs)))
  expect_false(any(grepl("Module B message", module_a_logs)))
  
  # Test 3.6: Log rotation
  for (i in 1:1000) {
    logger$log_info(paste("Log message", i), "test_module")
  }
  
  all_logs <- logger$get_logs()
  expect_lte(length(all_logs), 500)  # Assuming max 500 logs
  
  # Test 3.7: Color coding functionality
  colored_output <- logger$format_log_message("INFO", "Test message", "module")
  expect_true(nchar(colored_output) > nchar("Test message"))
  
  # Test 3.8: Log export functionality
  temp_file <- tempfile(fileext = ".log")
  expect_no_error(logger$export_logs(temp_file))
  expect_true(file.exists(temp_file))
  
  log_content <- readLines(temp_file)
  expect_gt(length(log_content), 0)
  
  unlink(temp_file)
  
  # Test 3.9: Thread safety simulation
  expect_no_error({
    for (i in 1:10) {
      logger$log_info(paste("Concurrent message", i), "concurrent_test")
    }
  })
  
  # Test 3.10: Invalid input handling
  expect_error(logger$log_info(NULL, "test_module"))
  expect_error(logger$log_info("Message", NULL))
  expect_error(logger$set_log_level("INVALID_LEVEL"))
})

# =============================================================================
# 3. UTILITY FUNCTIONS TESTS
# =============================================================================

test_that("Utility Functions - Data Processing", {
  
  # Test 4.1: Data cleaning functions
  dirty_data <- data.frame(
    Name = c("John", "  Jane  ", "Bob", ""),
    Age = c(25, NA, 30, 35),
    Salary = c(50000, 60000, NA, 70000)
  )
  
  cleaned_data <- clean_data(dirty_data)
  expect_false(any(grepl("^\\s|\\s$", cleaned_data$Name)))
  expect_false(any(cleaned_data$Name == ""))
  
  # Test 4.2: Data type conversion
  string_numbers <- data.frame(
    ID = c("1", "2", "3"),
    Salary = c("50000", "60000", "70000")
  )
  
  converted_data <- convert_data_types(string_numbers)
  expect_true(is.numeric(converted_data$ID))
  expect_true(is.numeric(converted_data$Salary))
  
  # Test 4.3: Date parsing
  date_strings <- c("2020-01-01", "01/02/2021", "2022-12-31")
  parsed_dates <- parse_dates(date_strings)
  expect_true(all(inherits(parsed_dates, "Date")))
  
  # Test 4.4: Outlier detection
  test_values <- c(1, 2, 3, 4, 5, 100, 2, 3, 4, 5)
  outliers <- detect_outliers(test_values)
  expect_true(100 %in% outliers)
  expect_false(3 %in% outliers)
  
  # Test 4.5: Statistical summary generation
  numeric_data <- data.frame(
    Age = c(25, 30, 35, 40, 45),
    Salary = c(50000, 60000, 70000, 80000, 90000)
  )
  
  summary_stats <- generate_summary_stats(numeric_data)
  expect_true(all(c("mean", "median", "sd") %in% names(summary_stats$Age)))
  
  # Test 4.6: Data validation
  valid_data <- data.frame(
    EmployeeID = 1:3,
    Age = c(25, 30, 35),
    Salary = c(50000, 60000, 70000)
  )
  
  expect_true(validate_data_structure(valid_data))
  
  invalid_data <- data.frame(
    EmployeeID = c(1, 1, 3),  # Duplicate ID
    Age = c(25, 30, 35)
  )
  
  expect_false(validate_data_structure(invalid_data))
  
  # Test 4.7: Safe division function
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(10, 0), Inf)
  expect_true(is.na(safe_divide(NA, 2)))
  
  # Test 4.8: Percentage calculation
  expect_equal(calculate_percentage(50, 100), 50)
  expect_equal(calculate_percentage(0, 100), 0)
  expect_true(is.na(calculate_percentage(50, 0)))
  
  # Test 4.9: Text processing
  text_data <- c("John Doe", "jane smith", "BOB JOHNSON")
  expect_equal(standardize_text(text_data), c("John Doe", "Jane Smith", "Bob Johnson"))
  
  # Test 4.10: Range validation
  expect_true(is_in_range(5, 1, 10))
  expect_false(is_in_range(15, 1, 10))
  expect_false(is_in_range(NA, 1, 10))
})

# =============================================================================
# 4. VISUALIZATION TESTS
# =============================================================================

test_that("Visualization Functions - Chart Generation", {
  
  # Test 5.1: Basic ggplot creation
  test_data <- data.frame(
    Department = c("IT", "HR", "Finance"),
    Count = c(10, 15, 8)
  )
  
  plot <- create_bar_chart(test_data, "Department", "Count")
  expect_s3_class(plot, "ggplot")
  
  # Test 5.2: Plotly conversion
  plotly_chart <- create_interactive_chart(plot)
  expect_s3_class(plotly_chart, "plotly")
  
  # Test 5.3: Empty data handling
  empty_data <- data.frame(Department = character(0), Count = numeric(0))
  expect_error(create_bar_chart(empty_data, "Department", "Count"))
  
  # Test 5.4: Missing values in visualization
  missing_data <- data.frame(
    Department = c("IT", "HR", "Finance", "Marketing"),
    Count = c(10, NA, 8, 12)
  )
  
  plot_with_missing <- create_bar_chart(missing_data, "Department", "Count")
  expect_s3_class(plot_with_missing, "ggplot")
  
  # Test 5.5: Custom theme application
  themed_plot <- apply_atlas_theme(plot)
  expect_s3_class(themed_plot, "ggplot")
  
  # Test 5.6: Color palette validation
  colors <- get_atlas_colors(3)
  expect_length(colors, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  
  # Test 5.7: Histogram generation
  numeric_data <- data.frame(Salary = rnorm(100, 50000, 10000))
  hist_plot <- create_histogram(numeric_data, "Salary")
  expect_s3_class(hist_plot, "ggplot")
  
  # Test 5.8: Scatter plot with correlation
  scatter_data <- data.frame(
    Age = runif(50, 20, 60),
    Salary = runif(50, 30000, 100000)
  )
  
  scatter_plot <- create_scatter_plot(scatter_data, "Age", "Salary")
  expect_s3_class(scatter_plot, "ggplot")
  
  # Test 5.9: Heatmap generation
  heatmap_data <- matrix(runif(25), nrow = 5, ncol = 5)
  heatmap_plot <- create_heatmap(heatmap_data)
  expect_s3_class(heatmap_plot, "ggplot")
  
  # Test 5.10: Chart export functionality
  temp_file <- tempfile(fileext = ".png")
  expect_no_error(export_chart(plot, temp_file))
  expect_true(file.exists(temp_file))
  
  unlink(temp_file)
})

# =============================================================================
# 5. ANALYSIS MODULE TESTS
# =============================================================================

test_that("Analysis Functions - Statistical Calculations", {
  
  # Test 6.1: Attrition rate calculation
  employee_data <- data.frame(
    EmployeeID = 1:10,
    Attrition = c(rep("Yes", 3), rep("No", 7)),
    Department = rep(c("IT", "HR"), 5)
  )
  
  attrition_rate <- calculate_attrition_rate(employee_data)
  expect_equal(attrition_rate, 0.3)
  
  # Test 6.2: Department-wise attrition
  dept_attrition <- calculate_department_attrition(employee_data)
  expect_s3_class(dept_attrition, "data.frame")
  expect_true(all(c("Department", "AttritionRate") %in% names(dept_attrition)))
  
  # Test 6.3: Satisfaction score calculation
  satisfaction_data <- data.frame(
    EmployeeID = 1:5,
    JobSatisfaction = c(4, 5, 3, 4, 5),
    WorkLifeBalance = c(3, 4, 5, 4, 3)
  )
  
  avg_satisfaction <- calculate_average_satisfaction(satisfaction_data)
  expect_true(is.numeric(avg_satisfaction))
  expect_true(avg_satisfaction >= 1 && avg_satisfaction <= 5)
  
  # Test 6.4: Performance correlation
  performance_data <- data.frame(
    EmployeeID = 1:10,
    PerformanceRating = runif(10, 1, 5),
    Salary = runif(10, 30000, 100000)
  )
  
  correlation <- calculate_performance_correlation(performance_data)
  expect_true(is.numeric(correlation))
  expect_true(correlation >= -1 && correlation <= 1)
  
  # Test 6.5: Tenure analysis
  tenure_data <- data.frame(
    EmployeeID = 1:10,
    YearsAtCompany = runif(10, 0, 20),
    Attrition = sample(c("Yes", "No"), 10, replace = TRUE)
  )
  
  tenure_analysis <- analyze_tenure_impact(tenure_data)
  expect_s3_class(tenure_analysis, "data.frame")
  
  # Test 6.6: Salary distribution analysis
  salary_data <- data.frame(
    EmployeeID = 1:100,
    Salary = rnorm(100, 50000, 15000),
    Department = sample(c("IT", "HR", "Finance"), 100, replace = TRUE)
  )
  
  salary_stats <- analyze_salary_distribution(salary_data)
  expect_true(all(c("mean", "median", "q25", "q75") %in% names(salary_stats)))
  
  # Test 6.7: Demographic analysis
  demographic_data <- data.frame(
    EmployeeID = 1:50,
    Gender = sample(c("Male", "Female", "Other"), 50, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian"), 50, replace = TRUE),
    Age = runif(50, 22, 65)
  )
  
  demo_summary <- analyze_demographics(demographic_data)
  expect_s3_class(demo_summary, "list")
  
  # Test 6.8: Training effectiveness
  training_data <- data.frame(
    EmployeeID = 1:30,
    TrainingHours = runif(30, 0, 100),
    PerformanceImprovement = runif(30, -1, 3)
  )
  
  training_effect <- analyze_training_effectiveness(training_data)
  expect_true(is.numeric(training_effect))
  
  # Test 6.9: Risk factor identification
  risk_data <- data.frame(
    EmployeeID = 1:20,
    Age = runif(20, 20, 60),
    YearsAtCompany = runif(20, 0, 15),
    JobSatisfaction = runif(20, 1, 5),
    Attrition = sample(c("Yes", "No"), 20, replace = TRUE)
  )
  
  risk_factors <- identify_risk_factors(risk_data)
  expect_s3_class(risk_factors, "data.frame")
  
  # Test 6.10: Predictive modeling
  prediction_data <- data.frame(
    Age = runif(100, 20, 60),
    Salary = runif(100, 30000, 100000),
    YearsAtCompany = runif(100, 0, 20),
    JobSatisfaction = runif(100, 1, 5),
    Attrition = sample(c(0, 1), 100, replace = TRUE)
  )
  
  model <- build_attrition_model(prediction_data)
  expect_s3_class(model, "glm")
  
  predictions <- predict_attrition_probability(model, prediction_data[1:10, ])
  expect_true(all(predictions >= 0 & predictions <= 1))
})

# =============================================================================
# 6. USER INTERFACE TESTS
# =============================================================================

test_that("User Interface - Input Validation", {
  
  # Test 7.1: Date range validation
  expect_true(validate_date_range("2020-01-01", "2020-12-31"))
  expect_false(validate_date_range("2020-12-31", "2020-01-01"))
  expect_false(validate_date_range("invalid", "2020-12-31"))
  
  # Test 7.2: Numeric input validation
  expect_true(validate_numeric_input("123.45"))
  expect_false(validate_numeric_input("abc"))
  expect_false(validate_numeric_input(""))
  
  # Test 7.3: Select input validation
  valid_options <- c("IT", "HR", "Finance")
  expect_true(validate_select_input("IT", valid_options))
  expect_false(validate_select_input("Marketing", valid_options))
  
  # Test 7.4: File upload validation
  test_file <- tempfile(fileext = ".csv")
  writeLines("col1,col2\n1,2", test_file)
  
  expect_true(validate_file_upload(test_file, "csv"))
  expect_false(validate_file_upload(test_file, "xlsx"))
  
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("test", temp_txt)
  expect_false(validate_file_upload(temp_txt, "csv"))
  
  unlink(c(test_file, temp_txt))
  
  # Test 7.5: Form validation
  form_data <- list(
    name = "John Doe",
    email = "john@example.com",
    age = "30"
  )
  
  expect_true(validate_form_data(form_data))
  
  invalid_form <- list(
    name = "",
    email = "invalid-email",
    age = "not-a-number"
  )
  
  expect_false(validate_form_data(invalid_form))
  
  # Test 7.6: Filter validation
  filter_values <- list(
    department = c("IT", "HR"),
    salary_range = c(30000, 80000),
    age_range = c(25, 55)
  )
  
  expect_true(validate_filter_inputs(filter_values))
  
  # Test 7.7: Search input sanitization
  search_term <- "<script>alert('xss')</script>"
  sanitized <- sanitize_search_input(search_term)
  expect_false(grepl("<script>", sanitized))
  
  # Test 7.8: URL parameter validation
  valid_params <- list(tab = "overview", filter = "department")
  expect_true(validate_url_params(valid_params))
  
  invalid_params <- list(tab = "invalid_tab", filter = "")
  expect_false(validate_url_params(invalid_params))
  
  # Test 7.9: Session state validation
  session_state <- list(
    user_id = "user123",
    permissions = c("read", "write"),
    last_activity = Sys.time()
  )
  
  expect_true(validate_session_state(session_state))
  
  # Test 7.10: Export format validation
  expect_true(validate_export_format("pdf"))
  expect_true(validate_export_format("csv"))
  expect_false(validate_export_format("invalid"))
})

# =============================================================================
# 7. SECURITY TESTS
# =============================================================================

test_that("Security - Input Sanitization and Validation", {
  
  # Test 8.1: SQL injection prevention
  malicious_input <- "'; DROP TABLE users; --"
  sanitized_sql <- sanitize_sql_input(malicious_input)
  expect_false(grepl("DROP TABLE", sanitized_sql))
  
  # Test 8.2: XSS prevention
  xss_input <- "<script>alert('xss')</script>"
  sanitized_xss <- sanitize_html_input(xss_input)
  expect_false(grepl("<script>", sanitized_xss))
  
  # Test 8.3: Path traversal prevention
  path_input <- "../../etc/passwd"
  sanitized_path <- sanitize_file_path(path_input)
  expect_false(grepl("\\.\\.", sanitized_path))
  
  # Test 8.4: Email validation
  expect_true(validate_email("user@example.com"))
  expect_false(validate_email("invalid-email"))
  expect_false(validate_email("user@"))
  
  # Test 8.5: Password strength validation
  expect_true(validate_password_strength("StrongPass123!"))
  expect_false(validate_password_strength("weak"))
  expect_false(validate_password_strength(""))
  
  # Test 8.6: Rate limiting simulation
  rate_limiter <- create_rate_limiter(max_requests = 5, window_seconds = 60)
  
  # Should allow first 5 requests
  for (i in 1:5) {
    expect_true(rate_limiter$check_request("user123"))
  }
  
  # Should block 6th request
  expect_false(rate_limiter$check_request("user123"))
  
  # Test 8.7: Session token validation
  valid_token <- generate_session_token()
  expect_true(validate_session_token(valid_token))
  expect_false(validate_session_token("invalid-token"))
  
  # Test 8.8: File type validation
  expect_true(validate_file_type("document.pdf", c("pdf", "doc", "docx")))
  expect_false(validate_file_type("script.exe", c("pdf", "doc", "docx")))
  
  # Test 8.9: Input length validation
  expect_true(validate_input_length("Valid input", max_length = 50))
  expect_false(validate_input_length(paste(rep("x", 1000), collapse = ""), max_length = 50))
  
  # Test 8.10: CSRF token validation
  csrf_token <- generate_csrf_token()
  expect_true(validate_csrf_token(csrf_token))
  expect_false(validate_csrf_token("invalid-csrf"))
})

# =============================================================================
# 8. PERFORMANCE TESTS
# =============================================================================

test_that("Performance - Function Execution Speed", {
  
  # Test 9.1: Data loading performance
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Name = paste0("Employee", 1:10000),
    Department = sample(c("IT", "HR", "Finance"), 10000, replace = TRUE),
    Salary = runif(10000, 30000, 100000)
  )
  
  start_time <- Sys.time()
  processed_data <- process_large_dataset(large_data)
  execution_time <- as.numeric(Sys.time() - start_time)
  
  expect_lt(execution_time, 5)  # Should complete in under 5 seconds
  expect_equal(nrow(processed_data), 10000)
  
  # Test 9.2: Visualization rendering performance
  start_time <- Sys.time()
  plot <- create_complex_visualization(large_data)
  render_time <- as.numeric(Sys.time() - start_time)
  
  expect_lt(render_time, 10)  # Should render in under 10 seconds
  expect_s3_class(plot, "ggplot")
  
  # Test 9.3: Database query performance
  mock_query <- function(n) {
    Sys.sleep(0.01)  # Simulate query time
    return(data.frame(id = 1:n, value = runif(n)))
  }
  
  start_time <-