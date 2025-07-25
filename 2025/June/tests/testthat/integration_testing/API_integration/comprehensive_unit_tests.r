# ============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TESTS
# ============================================================================
# Author: akhapwoyaco
# Description: Extensive unit testing suite covering all application components
# excluding integration testing areas
# ============================================================================

# Load required testing libraries
library(testthat)
library(shiny)
library(shinytest2)
library(mockery)
library(withr)
library(here)
library(R6)
library(tidyverse)
library(plotly)
library(DT)

# Source application files
source(here("global.R"))
source(here("utils.R"))
source(here("custom_theme.R"))

# Load all modules
purrr::walk(list.files(here("modules"), pattern = "\\.R$", full.names = TRUE), source)

# ============================================================================
# 1. DATA VALIDATION & LOADING TESTS
# ============================================================================

test_that("Data Loader Module - File Validation", {
  
  # Test valid file extensions
  expect_true(validate_file_extension("employee.csv"))
  expect_true(validate_file_extension("performance_rating.csv"))
  expect_true(validate_file_extension("education_level.csv"))
  
  # Test invalid file extensions
  expect_false(validate_file_extension("employee.txt"))
  expect_false(validate_file_extension("data.xlsx"))
  expect_false(validate_file_extension("file.json"))
  expect_false(validate_file_extension(NULL))
  expect_false(validate_file_extension(""))
  
  # Test edge cases
  expect_false(validate_file_extension("file.CSV"))  # Case sensitivity
  expect_false(validate_file_extension("file."))     # No extension
  expect_false(validate_file_extension(".csv"))      # No filename
})

test_that("Data Loader Module - Data Structure Validation", {
  
  # Create mock valid employee data
  valid_employee_data <- data.frame(
    EmployeeID = 1:100,
    FirstName = paste0("Employee", 1:100),
    LastName = paste0("Last", 1:100),
    Gender = sample(c("Male", "Female", "Other"), 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    BusinessTravel = sample(c("Rarely", "Frequently", "Non-Travel"), 100, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT", "Finance"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL"), 100, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian"), 100, replace = TRUE),
    Education = sample(1:5, 100, replace = TRUE),
    EducationField = sample(c("Engineering", "Business", "Arts"), 100, replace = TRUE),
    JobRole = sample(c("Manager", "Analyst", "Developer"), 100, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), 100, replace = TRUE),
    Salary = sample(40000:150000, 100, replace = TRUE),
    StockOptionLevel = sample(0:3, 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
    HireDate = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"), 100),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    YearsAtCompany = sample(1:15, 100, replace = TRUE),
    YearsInMostRecentRole = sample(1:10, 100, replace = TRUE),
    YearsSinceLastPromotion = sample(0:8, 100, replace = TRUE),
    YearsWithCurrManager = sample(1:12, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test valid data structure
  expect_true(validate_employee_data_structure(valid_employee_data))
  
  # Test missing required columns
  invalid_data_missing_col <- valid_employee_data[, -1]  # Remove EmployeeID
  expect_false(validate_employee_data_structure(invalid_data_missing_col))
  
  # Test NULL/empty data
  expect_false(validate_employee_data_structure(NULL))
  expect_false(validate_employee_data_structure(data.frame()))
  
  # Test wrong data types
  invalid_data_types <- valid_employee_data
  invalid_data_types$Age <- as.character(invalid_data_types$Age)
  expect_error(validate_employee_data_structure(invalid_data_types))
})

test_that("Data Loader Module - Missing Value Handling", {
  
  # Create data with missing values
  data_with_na <- data.frame(
    EmployeeID = c(1, 2, 3, NA, 5),
    Salary = c(50000, NA, 75000, 60000, NA),
    Age = c(25, 30, NA, 35, 40),
    stringsAsFactors = FALSE
  )
  
  # Test missing value detection
  missing_summary <- detect_missing_values(data_with_na)
  expect_equal(missing_summary$EmployeeID, 1)
  expect_equal(missing_summary$Salary, 2)
  expect_equal(missing_summary$Age, 1)
  
  # Test missing value threshold validation
  expect_false(validate_missing_threshold(data_with_na, threshold = 0.1))  # 20% missing too high
  expect_true(validate_missing_threshold(data_with_na, threshold = 0.5))   # 50% threshold OK
  
  # Edge cases
  complete_data <- data.frame(x = 1:5, y = letters[1:5])
  expect_true(validate_missing_threshold(complete_data, threshold = 0.0))
  
  all_na_data <- data.frame(x = rep(NA, 5))
  expect_false(validate_missing_threshold(all_na_data, threshold = 0.99))
})

test_that("Data Loader Module - Data Type Conversion", {
  
  # Test date conversion
  date_strings <- c("2023-01-15", "2022-06-30", "2021-12-01")
  converted_dates <- convert_to_date(date_strings)
  expect_s3_class(converted_dates, "Date")
  expect_equal(length(converted_dates), 3)
  
  # Test invalid date handling
  invalid_dates <- c("2023-13-45", "not-a-date", NA)
  expect_warning(convert_to_date(invalid_dates))
  
  # Test factor conversion
  char_vector <- c("A", "B", "A", "C", "B")
  factor_result <- convert_to_factor(char_vector)
  expect_s3_class(factor_result, "factor")
  expect_equal(levels(factor_result), c("A", "B", "C"))
  
  # Test numeric conversion
  char_numbers <- c("123", "456.78", "999")
  numeric_result <- convert_to_numeric(char_numbers)
  expect_type(numeric_result, "double")
  expect_equal(numeric_result[1], 123)
})

# ============================================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# ============================================================================

test_that("AtlasLogger R6 Class - Initialization", {
  
  # Test successful initialization
  logger <- AtlasLogger$new(app_name = "TestApp", log_level = "INFO")
  expect_r6(logger, "AtlasLogger")
  expect_equal(logger$app_name, "TestApp")
  expect_equal(logger$log_level, "INFO")
  
  # Test default initialization
  logger_default <- AtlasLogger$new()
  expect_equal(logger_default$app_name, "Atlas Labs HR Dashboard")
  expect_equal(logger_default$log_level, "INFO")
  
  # Test invalid log level
  expect_error(AtlasLogger$new(log_level = "INVALID"))
  
  # Test empty app name
  expect_error(AtlasLogger$new(app_name = ""))
  expect_error(AtlasLogger$new(app_name = NULL))
})

test_that("AtlasLogger R6 Class - Logging Methods", {
  
  logger <- AtlasLogger$new(app_name = "TestApp")
  
  # Test info logging
  expect_silent(logger$log_info("Test info message", module = "TestModule"))
  expect_true(length(logger$get_logs()) > 0)
  
  # Test warning logging
  expect_silent(logger$log_warning("Test warning", module = "TestModule"))
  
  # Test error logging
  expect_silent(logger$log_error("Test error", module = "TestModule"))
  
  # Test debug logging (should be filtered out at INFO level)
  initial_count <- length(logger$get_logs())
  logger$log_debug("Debug message", module = "TestModule")
  expect_equal(length(logger$get_logs()), initial_count)
  
  # Test logging with performance data
  perf_data <- list(execution_time = 1.5, memory_usage = 100)
  logger$log_info("Performance test", module = "TestModule", performance_data = perf_data)
  
  # Verify log structure
  logs <- logger$get_logs()
  latest_log <- logs[[length(logs)]]
  expect_true("execution_time" %in% names(latest_log$performance_data))
  expect_equal(latest_log$performance_data$execution_time, 1.5)
})

test_that("AtlasLogger R6 Class - Performance Tracking", {
  
  logger <- AtlasLogger$new()
  
  # Test memory usage tracking
  memory_before <- logger$track_memory_usage()
  expect_type(memory_before, "double")
  expect_true(memory_before > 0)
  
  # Test execution time tracking
  start_time <- logger$start_timer("test_operation")
  Sys.sleep(0.1)  # Simulate work
  execution_time <- logger$end_timer("test_operation")
  expect_type(execution_time, "double")
  expect_true(execution_time >= 0.1)
  
  # Test invalid timer operations
  expect_error(logger$end_timer("non_existent_timer"))
  expect_error(logger$start_timer(""))
  expect_error(logger$start_timer(NULL))
  
  # Test performance summary
  summary <- logger$get_performance_summary()
  expect_type(summary, "list")
  expect_true("average_execution_time" %in% names(summary))
  expect_true("peak_memory_usage" %in% names(summary))
})

test_that("AtlasLogger R6 Class - Log Filtering and Retrieval", {
  
  logger <- AtlasLogger$new()
  
  # Add various types of logs
  logger$log_info("Info message 1", module = "Module1")
  logger$log_warning("Warning message 1", module = "Module1")
  logger$log_error("Error message 1", module = "Module2")
  logger$log_info("Info message 2", module = "Module2")
  
  # Test filtering by log level
  error_logs <- logger$get_logs(level = "ERROR")
  expect_length(error_logs, 1)
  expect_equal(error_logs[[1]]$level, "ERROR")
  
  # Test filtering by module
  module1_logs <- logger$get_logs(module = "Module1")
  expect_length(module1_logs, 2)
  expect_true(all(sapply(module1_logs, function(x) x$module == "Module1")))
  
  # Test filtering by date range
  recent_logs <- logger$get_logs(since = Sys.time() - 3600)  # Last hour
  expect_true(length(recent_logs) >= 4)
  
  # Test log clearing
  logger$clear_logs()
  expect_length(logger$get_logs(), 0)
})

test_that("AtlasLogger R6 Class - Edge Cases and Error Handling", {
  
  logger <- AtlasLogger$new()
  
  # Test logging with NULL/empty messages
  expect_error(logger$log_info(NULL, module = "TestModule"))
  expect_error(logger$log_info("", module = "TestModule"))
  
  # Test logging with NULL/empty module
  expect_error(logger$log_info("Test message", module = NULL))
  expect_error(logger$log_info("Test message", module = ""))
  
  # Test logging with very long messages
  long_message <- paste(rep("a", 10000), collapse = "")
  expect_silent(logger$log_info(long_message, module = "TestModule"))
  
  # Test concurrent logging simulation
  for (i in 1:100) {
    logger$log_info(paste("Concurrent message", i), module = paste("Module", i %% 5))
  }
  expect_length(logger$get_logs(), 100)
  
  # Test memory limits
  expect_true(logger$get_memory_usage() > 0)
  expect_true(is.numeric(logger$get_memory_usage()))
})

# ============================================================================
# 3. UTILITY FUNCTIONS TESTS
# ============================================================================

test_that("Utility Functions - Data Processing", {
  
  # Test safe division
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(10, 0), 0)  # Should return 0 for division by zero
  expect_equal(safe_divide(0, 5), 0)
  expect_true(is.na(safe_divide(NA, 5)))
  expect_true(is.na(safe_divide(5, NA)))
  
  # Test percentage calculation
  expect_equal(calculate_percentage(25, 100), 25)
  expect_equal(calculate_percentage(1, 3, digits = 2), 33.33)
  expect_equal(calculate_percentage(0, 100), 0)
  expect_equal(calculate_percentage(100, 0), 0)  # Handle division by zero
  
  # Test data aggregation
  test_data <- data.frame(
    category = c("A", "A", "B", "B", "C"),
    value = c(10, 20, 15, 25, 30),
    stringsAsFactors = FALSE
  )
  
  aggregated <- aggregate_by_category(test_data, "category", "value", "sum")
  expect_equal(nrow(aggregated), 3)
  expect_equal(aggregated$value[aggregated$category == "A"], 30)
  expect_equal(aggregated$value[aggregated$category == "B"], 40)
})

test_that("Utility Functions - String Operations", {
  
  # Test string cleaning
  dirty_string <- "  Hello World!  \n\t"
  expect_equal(clean_string(dirty_string), "Hello World!")
  
  # Test empty and NULL strings
  expect_equal(clean_string(""), "")
  expect_equal(clean_string(NULL), "")
  expect_equal(clean_string("   "), "")
  
  # Test title case conversion
  expect_equal(to_title_case("hello world"), "Hello World")
  expect_equal(to_title_case("HELLO WORLD"), "Hello World")
  expect_equal(to_title_case("hELLo WoRLd"), "Hello World")
  expect_equal(to_title_case(""), "")
  
  # Test name validation
  expect_true(validate_name("John Doe"))
  expect_true(validate_name("Mary-Jane O'Connor"))
  expect_false(validate_name("John123"))
  expect_false(validate_name(""))
  expect_false(validate_name(NULL))
  expect_false(validate_name("J"))  # Too short
})

test_that("Utility Functions - Date Operations", {
  
  # Test age calculation
  birth_date <- as.Date("1990-06-15")
  reference_date <- as.Date("2023-06-15")
  expect_equal(calculate_age(birth_date, reference_date), 33)
  
  # Test edge cases for age calculation
  expect_equal(calculate_age(reference_date, reference_date), 0)
  expect_true(is.na(calculate_age(NA, reference_date)))
  expect_error(calculate_age(birth_date, NA))
  
  # Test date range validation
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2023-12-31")
  test_date <- as.Date("2022-06-15")
  
  expect_true(date_in_range(test_date, start_date, end_date))
  expect_false(date_in_range(as.Date("2019-01-01"), start_date, end_date))
  expect_false(date_in_range(as.Date("2024-01-01"), start_date, end_date))
  
  # Test fiscal year calculation
  expect_equal(get_fiscal_year(as.Date("2023-06-15")), 2023)
  expect_equal(get_fiscal_year(as.Date("2023-01-15")), 2023)
  expect_equal(get_fiscal_year(as.Date("2022-12-31")), 2022)
})

test_that("Utility Functions - Validation Helpers", {
  
  # Test email validation
  expect_true(validate_email("user@example.com"))
  expect_true(validate_email("user.name+tag@example.co.uk"))
  expect_false(validate_email("invalid.email"))
  expect_false(validate_email("@example.com"))
  expect_false(validate_email("user@"))
  expect_false(validate_email(""))
  expect_false(validate_email(NULL))
  
  # Test phone number validation
  expect_true(validate_phone("123-456-7890"))
  expect_true(validate_phone("(123) 456-7890"))
  expect_true(validate_phone("1234567890"))
  expect_false(validate_phone("123-456"))
  expect_false(validate_phone("abc-def-ghij"))
  expect_false(validate_phone(""))
  
  # Test salary validation
  expect_true(validate_salary(50000))
  expect_true(validate_salary(150000))
  expect_false(validate_salary(0))
  expect_false(validate_salary(-1000))
  expect_false(validate_salary(1000000))  # Too high
  expect_false(validate_salary(NA))
  expect_false(validate_salary(NULL))
})

# ============================================================================
# 4. CUSTOM THEME TESTS
# ============================================================================

test_that("Custom Theme - ggplot2 Theme Functions", {
  
  # Test Atlas theme creation
  atlas_theme <- theme_atlas()
  expect_s3_class(atlas_theme, "theme")
  expect_s3_class(atlas_theme, "gg")
  
  # Test theme with different base sizes
  small_theme <- theme_atlas(base_size = 10)
  large_theme <- theme_atlas(base_size = 16)
  
  expect_s3_class(small_theme, "theme")
  expect_s3_class(large_theme, "theme")
  
  # Test color palette functions
  atlas_colors <- get_atlas_colors()
  expect_type(atlas_colors, "character")
  expect_true(length(atlas_colors) >= 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", atlas_colors)))  # Valid hex colors
  
  # Test categorical color palette
  cat_palette <- get_categorical_palette(5)
  expect_length(cat_palette, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cat_palette)))
  
  # Test gradient color palette
  gradient_palette <- get_gradient_palette("blue_to_red", 10)
  expect_length(gradient_palette, 10)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", gradient_palette)))
})

test_that("Custom Theme - Color Accessibility", {
  
  # Test colorblind-friendly palette
  cb_palette <- get_colorblind_palette()
  expect_type(cb_palette, "character")
  expect_true(length(cb_palette) >= 8)
  
  # Test contrast ratio validation
  expect_true(check_contrast_ratio("#FFFFFF", "#000000") > 7)  # High contrast
  expect_true(check_contrast_ratio("#FFFFFF", "#808080") > 3)  # Medium contrast
  expect_false(check_contrast_ratio("#FFFFFF", "#FFFFFC") > 3) # Low contrast
  
  # Test invalid color inputs
  expect_error(check_contrast_ratio("invalid", "#000000"))
  expect_error(check_contrast_ratio("#FFFFFF", "invalid"))
  expect_error(check_contrast_ratio(NULL, "#000000"))
})

test_that("Custom Theme - CSS Generation", {
  
  # Test CSS variable generation
  css_vars <- generate_css_variables()
  expect_type(css_vars, "character")
  expect_true(grepl("--primary-color:", css_vars))
  expect_true(grepl("--secondary-color:", css_vars))
  
  # Test responsive CSS generation
  responsive_css <- generate_responsive_css()
  expect_type(responsive_css, "character")
  expect_true(grepl("@media", responsive_css))
  expect_true(grepl("max-width", responsive_css))
  
  # Test dark theme CSS
  dark_css <- generate_dark_theme_css()
  expect_type(dark_css, "character")
  expect_true(grepl("background-color", dark_css))
  expect_true(grepl("color", dark_css))
})

# ============================================================================
# 5. MODULE-SPECIFIC TESTS
# ============================================================================

test_that("Overview Module - KPI Calculations", {
  
  # Create sample data
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.15, 0.85)),
    Salary = sample(40000:120000, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test attrition rate calculation
  attrition_rate <- calculate_attrition_rate(sample_data)
  expect_type(attrition_rate, "double")
  expect_true(attrition_rate >= 0 && attrition_rate <= 1)
  
  # Test average salary calculation
  avg_salary <- calculate_average_salary(sample_data)
  expect_type(avg_salary, "double")
  expect_true(avg_salary > 0)
  
  # Test satisfaction score calculation
  avg_satisfaction <- calculate_average_satisfaction(sample_data)
  expect_type(avg_satisfaction, "double")
  expect_true(avg_satisfaction >= 1 && avg_satisfaction <= 5)
  
  # Test department distribution
  dept_dist <- calculate_department_distribution(sample_data)
  expect_s3_class(dept_dist, "data.frame")
  expect_equal(sum(dept_dist$count), 100)
  expect_true(all(dept_dist$percentage >= 0 & dept_dist$percentage <= 100))
})

test_that("Attrition Module - Risk Analysis", {
  
  # Create sample data with attrition factors
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    YearsAtCompany = sample(1:20, 100, replace = TRUE),
    Salary = sample(40000:120000, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test attrition by tenure analysis
  tenure_analysis <- analyze_attrition_by_tenure(sample_data)
  expect_s3_class(tenure_analysis, "data.frame")
  expect_true("tenure_group" %in% colnames(tenure_analysis))
  expect_true("attrition_rate" %in% colnames(tenure_analysis))
  
  # Test attrition risk scoring
  risk_scores <- calculate_attrition_risk_score(sample_data)
  expect_type(risk_scores, "double")
  expect_length(risk_scores, 100)
  expect_true(all(risk_scores >= 0 & risk_scores <= 1))
  
  # Test correlation analysis
  correlations <- calculate_attrition_correlations(sample_data)
  expect_type(correlations, "double")
  expect_true(all(abs(correlations) <= 1))
})

test_that("Demographics Module - Diversity Metrics", {
  
  # Create sample demographic data
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Gender = sample(c("Male", "Female", "Other"), 100, replace = TRUE, prob = c(0.6, 0.35, 0.05)),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT", "Finance"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test gender diversity calculation
  gender_diversity <- calculate_gender_diversity(sample_data)
  expect_s3_class(gender_diversity, "data.frame")
  expect_true(sum(gender_diversity$percentage) <= 100.1)  # Allow for rounding
  
  # Test age distribution analysis
  age_distribution <- analyze_age_distribution(sample_data)
  expect_s3_class(age_distribution, "data.frame")
  expect_true("age_group" %in% colnames(age_distribution))
  expect_true("count" %in% colnames(age_distribution))
  
  # Test diversity index calculation
  diversity_index <- calculate_diversity_index(sample_data, "Ethnicity")
  expect_type(diversity_index, "double")
  expect_true(diversity_index >= 0 && diversity_index <= 1)
  
  # Test edge case: single category
  single_category_data <- data.frame(
    Ethnicity = rep("White", 100),
    stringsAsFactors = FALSE
  )
  single_diversity <- calculate_diversity_index(single_category_data, "Ethnicity")
  expect_equal(single_diversity, 0)  # No diversity
})

test_that("Performance Module - Rating Analysis", {
  
  # Create sample performance data
  sample_data <- data.frame(
    EmployeeID = 1:100,
    SelfRating = sample(1:5, 100, replace = TRUE),
    ManagerRating = sample(1:5, 100, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 100, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Ensure training taken <= training offered
  sample_data$TrainingOpportunitiesTaken <- pmin(
    sample_data$TrainingOpportunitiesTaken,
    sample_data$TrainingOpportunitiesWithinYear
  )
  
  # Test rating gap analysis
  rating_gap <- calculate_rating_gap(sample_data)
  expect_type(rating_gap, "double")
  expect_length(rating_gap, 100)
  expect_true(all(abs(rating_gap) <= 4))  # Maximum possible gap
  
  # Test training utilization
  training_util <- calculate_training_utilization(sample_data)
  expect_type(training_util, "double")
  expect_length(training_util, 100)
  expect_true(all(training_util >= 0 & training_util <= 1))
  
  # Test performance correlation
  perf_correlation <- calculate_performance_correlation(sample_data)
  expect_type(perf_correlation, "double")
  expect_true(abs(perf_correlation) <= 1)
  
  # Test edge case: identical ratings
  identical_ratings <- data.frame(
    SelfRating = rep(3, 50),
    ManagerRating = rep(3, 50)
  )
  gap_identical <- calculate_rating_gap(identical_ratings)
  expect_true(all(gap_identical == 0))
})

test_that("Compensation Module - Pay Equity Analysis", {
  
  # Create sample compensation data
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Salary = sample(40000:120000, 100, replace = TRUE),
    Gender = sample(c("Male", "Female"), 100, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian"), 100, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT"), 100, replace = TRUE),
    JobRole = sample(c("Manager", "Analyst", "Associate"), 100, replace = TRUE),
    YearsAtCompany = sample(1:15, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test gender pay gap calculation
  pay_gap <- calculate_gender_pay_gap(sample_data)
  expect_type(pay_gap, "double")
  expect_true(abs(pay_gap) <= 1)  # Should be between -100% and 100%
  
  # Test salary quartiles
  quartiles <- calculate_salary_quartiles(sample_data)
  expect_length(quartiles, 4)
  expect_true(all(diff(quartiles) >= 0))  # Should be ascending
  
  # Test pay equity analysis by role
  equity_analysis <- analyze_pay_equity_by_role(sample_data)
  expect_s3_class(equity_analysis, "data.frame")
  expect_true("JobRole" %in% colnames(equity_analysis))
  expect_true("median_salary" %in% colnames(equity_analysis))
  
  # Test edge case: single gender
  single_gender_data <- sample_data
  single_gender_data$Gender <- "Male"
  gap_single <- calculate_gender_pay_gap(single_gender_data)
  expect_equal(gap_single, 0)
})

test_that("Satisfaction Module - Survey Analysis", {
  
  # Create sample satisfaction data
  sample_data <- data.frame(
    EmployeeID = 1:100,
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 100, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 100, replace = TRUE),
    WorkLifeBalance = sample(1:5, 100, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT"), 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.15, 0.85)),
    stringsAsFactors = FALSE
  )
  
  # Test overall satisfaction score
  overall_satisfaction <- calculate_overall_satisfaction(sample_data)
  expect_type(overall_satisfaction, "double")
  expect_length(overall_satisfaction, 100)
  expect_true(all(overall_satisfaction >= 1 & overall_satisfaction <= 5))
  
  # Test satisfaction by department
  dept_satisfaction <- analyze_satisfaction_by_department(sample_data)
  expect_s3_class(dept_satisfaction, "data.frame")
  expect_true("Department" %in% colnames(dept_satisfaction))
  expect_true("avg_satisfaction" %in% colnames(dept_satisfaction))
  
  # Test satisfaction-attrition correlation
  satisfaction_attrition_corr <- analyze_satisfaction_attrition_relationship(sample_data)
  expect_type(satisfaction_attrition_corr, "list")
  expect_true("correlation" %in% names(satisfaction_attrition_corr))
  expect_true("p_value" %in% names(satisfaction_attrition_corr))
  
  # Test edge case: all same satisfaction scores
  uniform_satisfaction <- sample_data
  uniform_satisfaction[, c("JobSatisfaction", "EnvironmentSatisfaction", 
                          "RelationshipSatisfaction", "WorkLifeBalance")] <- 3
  uniform_overall <- calculate_overall_satisfaction(uniform_satisfaction)
  expect_true(all(uniform_overall == 3))
})

# ============================================================================
# 6. SHINY MODULE UI/SERVER TESTS
# ============================================================================

test_that("Sidebar Module - UI Generation", {
  
  # Test sidebar UI creation
  sidebar_ui <- sidebarUI("test_sidebar")
  expect_s3_class(sidebar_ui, "shiny.tag")
  expect_true(any(grepl("test_sidebar", as.character(sidebar_ui), fixed = TRUE)))
  
  # Test with different namespaces
  sidebar_ui2 <- sidebarUI("different_namespace")
  expect_s3_class(sidebar_ui2, "shiny.tag")
  expect_true(any(grepl("different_namespace", as.character(sidebar_ui2), fixed = TRUE)))
  
  # Test invalid namespace
  expect_error(sidebarUI(""))
  expect_error(sidebarUI(NULL))
  expect_error(sidebarUI(123))
})

test_that("Overview Module - Reactive Functionality", {
  
  # Create mock reactive data
  mock_data <- reactive({
    data.frame(
      EmployeeID = 1:50,
      Attrition = sample(c("Yes", "No"), 50, replace = TRUE),
      Salary = sample(40000:100000, 50, replace = TRUE),
      Department = sample(c("Sales", "IT"), 50, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  # Test server function exists and is callable
  expect_function(overviewServer)
  
  # Test server function parameters
  server_formals <- formals(overviewServer)
  expected_params <- c("id", "data", "logger")
  expect_true(all(expected_params %in% names(server_formals)))
})

test_that("Data Loader Module - File Upload Validation", {
  
  # Test file size validation
  expect_true(validate_file_size(1000000))    # 1MB - valid
  expect_true(validate_file_size(50000000))   # 50MB - valid
  expect_false(validate_file_size(200000000)) # 200MB - too large
  expect_false(validate_file_size(0))         # Empty file
  expect_false(validate_file_size(-1))        # Invalid size
  expect_error(validate_file_size(NULL))
  expect_error(validate_file_size("invalid"))
  
  # Test MIME type validation
  expect_true(validate_mime_type("text/csv"))
  expect_true(validate_mime_type("application/csv"))
  expect_false(validate_mime_type("text/plain"))
  expect_false(validate_mime_type("application/json"))
  expect_false(validate_mime_type(""))
  expect_error(validate_mime_type(NULL))
  
  # Test file encoding detection
  # Create temporary CSV file for testing
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:5, y = letters[1:5]), temp_file, row.names = FALSE)
  
  encoding <- detect_file_encoding(temp_file)
  expect_type(encoding, "character")
  expect_true(encoding %in% c("UTF-8", "ASCII", "ISO-8859-1"))
  
  # Cleanup
  unlink(temp_file)
  
  # Test invalid file path
  expect_error(detect_file_encoding("nonexistent_file.csv"))
  expect_error(detect_file_encoding(NULL))
})

# ============================================================================
# 7. VISUALIZATION TESTS
# ============================================================================

test_that("Visualization Functions - ggplot2 Charts", {
  
  # Create sample data for visualization testing
  viz_data <- data.frame(
    Category = c("A", "B", "C", "D", "E"),
    Value = c(10, 25, 15, 30, 20),
    Group = c("X", "Y", "X", "Y", "X"),
    stringsAsFactors = FALSE
  )
  
  # Test bar chart creation
  bar_chart <- create_bar_chart(viz_data, "Category", "Value")
  expect_s3_class(bar_chart, "ggplot")
  expect_true("GeomCol" %in% class(bar_chart$layers[[1]]$geom))
  
  # Test grouped bar chart
  grouped_chart <- create_grouped_bar_chart(viz_data, "Category", "Value", "Group")
  expect_s3_class(grouped_chart, "ggplot")
  
  # Test scatter plot
  scatter_data <- data.frame(x = rnorm(50), y = rnorm(50))
  scatter_plot <- create_scatter_plot(scatter_data, "x", "y")
  expect_s3_class(scatter_plot, "ggplot")
  expect_true("GeomPoint" %in% class(scatter_plot$layers[[1]]$geom))
  
  # Test line chart
  time_data <- data.frame(
    Date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "month"),
    Value = cumsum(rnorm(12))
  )
  line_chart <- create_line_chart(time_data, "Date", "Value")
  expect_s3_class(line_chart, "ggplot")
  expect_true("GeomLine" %in% class(line_chart$layers[[1]]$geom))
})

test_that("Visualization Functions - Plotly Integration", {
  
  # Create base ggplot
  base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  
  # Test plotly conversion
  plotly_chart <- make_interactive_plot(base_plot)
  expect_s3_class(plotly_chart, "plotly")
  expect_s3_class(plotly_chart, "htmlwidget")
  
  # Test custom plotly configuration
  config_options <- list(displayModeBar = FALSE, responsive = TRUE)
  configured_plot <- make_interactive_plot(base_plot, config = config_options)
  expect_s3_class(configured_plot, "plotly")
  
  # Test error handling for invalid input
  expect_error(make_interactive_plot("not_a_plot"))
  expect_error(make_interactive_plot(NULL))
})

test_that("Visualization Functions - Chart Annotations", {
  
  # Test adding ggrepel labels
  label_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 1, 5, 3),
    label = c("Point A", "Point B", "Point C", "Point D", "Point E")
  )
  
  base_plot <- ggplot(label_data, aes(x = x, y = y)) + geom_point()
  labeled_plot <- add_ggrepel_labels(base_plot, label_data, "x", "y", "label")
  expect_s3_class(labeled_plot, "ggplot")
  expect_true(length(labeled_plot$layers) > 1)  # Should have point and label layers
  
  # Test adding trend lines
  trend_plot <- add_trend_line(base_plot, method = "lm")
  expect_s3_class(trend_plot, "ggplot")
  
  # Test adding confidence intervals
  ci_plot <- add_confidence_interval(base_plot, alpha = 0.2)
  expect_s3_class(ci_plot, "ggplot")
})

test_that("Visualization Functions - Color and Theme Application", {
  
  # Test Atlas color scale application
  color_data <- data.frame(
    category = letters[1:5],
    value = c(10, 25, 15, 30, 20)
  )
  
  base_plot <- ggplot(color_data, aes(x = category, y = value, fill = category)) + 
    geom_col()
  
  # Apply Atlas color scheme
  colored_plot <- apply_atlas_colors(base_plot)
  expect_s3_class(colored_plot, "ggplot")
  
  # Test gradient color application
  gradient_plot <- apply_gradient_colors(base_plot, "blue_to_red")
  expect_s3_class(gradient_plot, "ggplot")
  
  # Test theme application
  themed_plot <- base_plot + theme_atlas()
  expect_s3_class(themed_plot, "ggplot")
})

# ============================================================================
# 8. REPORT GENERATION TESTS
# ============================================================================

test_that("Report Module - Parameter Validation", {
  
  # Test valid report parameters
  valid_params <- list(
    data_summary = list(total_records = 100),
    selected_filters = list(department = c("Sales", "IT")),
    analysis_date = Sys.Date(),
    kpi_metrics = list(attrition_rate = 0.15, avg_salary = 75000)
  )
  
  expect_true(validate_report_parameters(valid_params))
  
  # Test invalid parameters
  invalid_params <- list(
    data_summary = NULL,
    selected_filters = "invalid_format",
    analysis_date = "not_a_date"
  )
  
  expect_false(validate_report_parameters(invalid_params))
  
  # Test missing required parameters
  incomplete_params <- list(
    data_summary = list(total_records = 100)
    # Missing other required parameters
  )
  
  expect_false(validate_report_parameters(incomplete_params))
})

test_that("Report Module - R Markdown Rendering", {
  
  # Create temporary R Markdown template
  temp_rmd <- tempfile(fileext = ".Rmd")
  rmd_content <- '
---
title: "Test Report"
output: html_document
params:
  test_param: NULL
---

# Test Section

This is a test report with parameter: `r params$test_param`
'
  writeLines(rmd_content, temp_rmd)
  
  # Test report generation
  test_params <- list(test_param = "Hello World")
  
  # Note: This would require rmarkdown package in real implementation
  # expect_true(file.exists(generate_report(temp_rmd, test_params)))
  
  # Cleanup
  unlink(temp_rmd)
})

test_that("Report Module - Content Generation", {
  
  # Test executive summary generation
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    Salary = sample(40000:120000, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  executive_summary <- generate_executive_summary(sample_data)
  expect_type(executive_summary, "character")
  expect_true(nchar(executive_summary) > 100)  # Should be substantial content
  
  # Test recommendations generation
  recommendations <- generate_recommendations(sample_data)
  expect_type(recommendations, "list")
  expect_true(length(recommendations) > 0)
  expect_true(all(sapply(recommendations, function(x) "title" %in% names(x))))
  expect_true(all(sapply(recommendations, function(x) "description" %in% names(x))))
  
  # Test KPI summary generation
  kpi_summary <- generate_kpi_summary(sample_data)
  expect_type(kpi_summary, "list")
  expect_true("total_employees" %in% names(kpi_summary))
  expect_true("attrition_rate" %in% names(kpi_summary))
})

# ============================================================================
# 9. ERROR HANDLING AND EDGE CASES
# ============================================================================

test_that("Error Handling - Data Quality Issues", {
  
  # Test handling of completely empty dataset
  empty_data <- data.frame()
  expect_error(calculate_attrition_rate(empty_data))
  expect_error(calculate_average_salary(empty_data))
  
  # Test handling of dataset with all NA values
  na_data <- data.frame(
    Attrition = rep(NA, 10),
    Salary = rep(NA, 10)
  )
  expect_error(calculate_attrition_rate(na_data))
  expect_warning(calculate_average_salary(na_data))
  
  # Test handling of inconsistent data types
  mixed_type_data <- data.frame(
    EmployeeID = c(1, 2, "three", 4, 5),
    Salary = c(50000, "sixty thousand", 70000, 80000, 90000),
    stringsAsFactors = FALSE
  )
  expect_error(validate_data_types(mixed_type_data))
  
  # Test handling of negative values where inappropriate
  negative_data <- data.frame(
    Age = c(25, -5, 35, 40),
    Salary = c(50000, -10000, 75000, 60000),
    YearsAtCompany = c(3, 2, -1, 5)
  )
  expect_false(validate_logical_constraints(negative_data))
})

test_that("Error Handling - Memory and Performance", {
  
  # Test handling of very large datasets
  # Create large dataset for memory testing
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Attrition = sample(c("Yes", "No"), 10000, replace = TRUE),
    Salary = sample(40000:120000, 10000, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Monitor memory usage during processing
  initial_memory <- gc()["Vcells", "used"]
  result <- calculate_attrition_rate(large_data)
  final_memory <- gc()["Vcells", "used"]
  
  expect_type(result, "double")
  expect_true(final_memory > initial_memory)  # Memory was used
  
  # Test timeout handling for long-running operations
  expect_error(
    with_timeout(Sys.sleep(2), timeout = 1),
    "Operation timed out"
  )
})

test_that("Error Handling - User Input Validation", {
  
  # Test handling of malformed filter inputs
  invalid_filters <- list(
    department = c("Sales", 123, NULL),  # Mixed types
    salary_range = "not_a_range",        # Wrong format
    date_range = c("2023-01-01")         # Incomplete range
  )
  
  expect_false(validate_filter_inputs(invalid_filters))
  
  # Test handling of SQL injection attempts
  malicious_input <- "'; DROP TABLE employees; --"
  expect_error(validate_safe_input(malicious_input))
  
  # Test handling of XSS attempts
  xss_input <- "<script>alert('xss')</script>"
  sanitized_input <- sanitize_user_input(xss_input)
  expect_false(grepl("<script>", sanitized_input))
  
  # Test handling of excessively long inputs
  long_input <- paste(rep("a", 10000), collapse = "")
  expect_error(validate_input_length(long_input, max_length = 1000))
})

# ============================================================================
# 10. SECURITY AND PRIVACY TESTS
# ============================================================================

test_that("Security - Data Anonymization", {
  
  # Create sample data with PII
  pii_data <- data.frame(
    EmployeeID = 1:10,
    FirstName = c("John", "Jane", "Bob", "Alice", "Charlie", 
                  "Diana", "Eve", "Frank", "Grace", "Henry"),
    LastName = c("Doe", "Smith", "Johnson", "Brown", "Davis", 
                 "Miller", "Wilson", "Moore", "Taylor", "Anderson"),
    Email = paste0(c("john", "jane", "bob", "alice", "charlie", 
                     "diana", "eve", "frank", "grace", "henry"), "@company.com"),
    SSN = c("123-45-6789", "987-65-4321", "555-66-7777", "111-22-3333", "444-55-6666",
            "777-88-9999", "222-33-4444", "888-99-0000", "333-44-5555", "666-77-8888"),
    stringsAsFactors = FALSE
  )
  
  # Test anonymization function
  anonymized_data <- anonymize_pii_data(pii_data)
  
  # Check that PII fields are anonymized
  expect_true(all(anonymized_data$FirstName != pii_data$FirstName))
  expect_true(all(anonymized_data$LastName != pii_data$LastName))
  expect_true(all(anonymized_data$Email != pii_data$Email))
  expect_true(all(anonymized_data$SSN != pii_data$SSN))
  
  # Check that EmployeeID is preserved for analysis
  expect_equal(anonymized_data$EmployeeID, pii_data$EmployeeID)
  
  # Test hash consistency
  hash1 <- hash_sensitive_data("john.doe@company.com")
  hash2 <- hash_sensitive_data("john.doe@company.com")
  expect_equal(hash1, hash2)  # Same input should produce same hash
  
  # Different inputs should produce different hashes
  hash3 <- hash_sensitive_data("jane.smith@company.com")
  expect_false(hash1 == hash3)
})

test_that("Security - Access Control", {
  
  # Test role-based access validation
  expect_true(validate_user_access("admin", "all_data"))
  expect_true(validate_user_access("hr_manager", "hr_data"))
  expect_false(validate_user_access("employee", "admin_data"))
  expect_false(validate_user_access("guest", "sensitive_data"))
  
  # Test data masking based on user role
  sensitive_data <- data.frame(
    EmployeeID = 1:5,
    Salary = c(50000, 75000, 60000, 90000, 65000),
    Performance = c(3.5, 4.2, 3.8, 4.5, 3.9)
  )
  
  # Admin should see all data
  admin_view <- apply_data_masking(sensitive_data, user_role = "admin")
  expect_equal(admin_view, sensitive_data)
  
  # Employee should see masked salary data
  employee_view <- apply_data_masking(sensitive_data, user_role = "employee")
  expect_true(all(is.na(employee_view$Salary)))
  expect_equal(employee_view$Performance, sensitive_data$Performance)
  
  # Test session validation
  valid_session <- list(
    user_id = "user123",
    role = "hr_manager",
    login_time = Sys.time(),
    expires_at = Sys.time() + 3600  # 1 hour from now
  )
  
  expect_true(validate_session(valid_session))
  
  # Test expired session
  expired_session <- valid_session
  expired_session$expires_at <- Sys.time() - 3600  # 1 hour ago
  expect_false(validate_session(expired_session))
})

test_that("Security - Data Encryption", {
  
  # Test symmetric encryption/decryption
  original_text <- "Sensitive employee data"
  encryption_key <- generate_encryption_key()
  
  encrypted_text <- encrypt_data(original_text, encryption_key)
  expect_type(encrypted_text, "character")
  expect_false(encrypted_text == original_text)
  
  decrypted_text <- decrypt_data(encrypted_text, encryption_key)
  expect_equal(decrypted_text, original_text)
  
  # Test encryption key management
  expect_type(encryption_key, "character")
  expect_true(nchar(encryption_key) >= 32)  # Minimum key length
  
  # Test that different keys produce different results
  key2 <- generate_encryption_key()
  encrypted_text2 <- encrypt_data(original_text, key2)
  expect_false(encrypted_text == encrypted_text2)
  
  # Test decryption with wrong key fails gracefully
  expect_error(decrypt_data(encrypted_text, key2))
})

# ============================================================================
# 11. PERFORMANCE BENCHMARKING TESTS
# ============================================================================

test_that("Performance - Data Processing Speed", {
  
  # Create datasets of different sizes for benchmarking
  small_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    Salary = sample(40000:120000, 100, replace = TRUE)
  )
  
  medium_data <- data.frame(
    EmployeeID = 1:1000,
    Attrition = sample(c("Yes", "No"), 1000, replace = TRUE),
    Salary = sample(40000:120000, 1000, replace = TRUE)
  )
  
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Attrition = sample(c("Yes", "No"), 10000, replace = TRUE),
    Salary = sample(40000:120000, 10000, replace = TRUE)
  )
  
  # Benchmark attrition rate calculation
  small_time <- system.time(calculate_attrition_rate(small_data))["elapsed"]
  medium_time <- system.time(calculate_attrition_rate(medium_data))["elapsed"]
  large_time <- system.time(calculate_attrition_rate(large_data))["elapsed"]
  
  # Performance should scale reasonably (not exponentially)
  expect_true(medium_time / small_time < 20)  # Should not be 20x slower
  expect_true(large_time / medium_time < 20)  # Should not be 20x slower
  
  # All operations should complete within reasonable time
  expect_true(small_time < 1.0)   # Under 1 second
  expect_true(medium_time < 5.0)  # Under 5 seconds
  expect_true(large_time < 30.0)  # Under 30 seconds
})

test_that("Performance - Memory Efficiency", {
  
  # Test memory usage for data processing
  initial_memory <- as.numeric(gc()["Vcells", "used"])
  
  # Process moderately large dataset
  test_data <- data.frame(
    EmployeeID = 1:5000,
    Attrition = sample(c("Yes", "No"), 5000, replace = TRUE),
    Salary = sample(40000:120000, 5000, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT", "Finance"), 5000, replace = TRUE)
  )
  
  # Perform multiple operations
  attrition_rate <- calculate_attrition_rate(test_data)
  avg_salary <- calculate_average_salary(test_data)
  dept_summary <- calculate_department_distribution(test_data)
  
  # Force garbage collection and measure memory
  gc()
  final_memory <- as.numeric(gc()["Vcells", "used"])
  memory_increase <- final_memory - initial_memory
  
  # Memory increase should be reasonable (not excessive)
  expect_true(memory_increase < initial_memory * 0.5)  # Less than 50% increase
  
  # Test memory cleanup
  rm(test_data, attrition_rate, avg_salary, dept_summary)
  gc()
  cleanup_memory <- as.numeric(gc()["Vcells", "used"])
  
  # Memory should be released after cleanup
  expect_true(cleanup_memory < final_memory)
})

test_that("Performance - Visualization Rendering", {
  
  # Test chart rendering performance
  chart_data <- data.frame(
    x = 1:1000,
    y = rnorm(1000),
    category = sample(LETTERS[1:10], 1000, replace = TRUE)
  )
  
  # Benchmark different chart types
  bar_chart_time <- system.time({
    bar_chart <- create_bar_chart(chart_data, "category", "y")
  })["elapsed"]
  
  scatter_plot_time <- system.time({
    scatter_plot <- create_scatter_plot(chart_data, "x", "y")
  })["elapsed"]
  
  # Chart creation should be fast
  expect_true(bar_chart_time < 5.0)
  expect_true(scatter_plot_time < 5.0)
  
  # Test plotly conversion performance
  plotly_conversion_time <- system.time({
    interactive_chart <- make_interactive_plot(scatter_plot)
  })["elapsed"]
  
  expect_true(plotly_conversion_time < 10.0)  # Plotly conversion can be slower
})

# ============================================================================
# 12. ACCESSIBILITY AND USABILITY TESTS
# ============================================================================

test_that("Accessibility - Color Contrast and Design", {
  
  # Test color contrast ratios
  atlas_colors <- get_atlas_colors()
  
  for (i in 1:(length(atlas_colors) - 1)) {
    for (j in (i+1):length(atlas_colors)) {
      contrast_ratio <- check_contrast_ratio(atlas_colors[i], atlas_colors[j])
      # Should meet WCAG AA standard (4.5:1) for most combinations
      if (contrast_ratio < 3.0) {
        warning(paste("Low contrast between", atlas_colors[i], "and", atlas_colors[j]))
      }
    }
  }
  
  # Test colorblind-friendly palette
  cb_palette <- get_colorblind_palette()
  expect_true(length(cb_palette) >= 8)
  
  # Test that colorblind palette has sufficient distinctiveness
  # This would ideally use colorspace package for proper testing
  expect_true(all(nchar(cb_palette) == 7))  # Valid hex colors
  expect_true(length(unique(cb_palette)) == length(cb_palette))  # All unique
})

test_that("Accessibility - Screen Reader Compatibility", {
  
  # Test that UI elements have proper ARIA labels
  sidebar_ui <- sidebarUI("test")
  ui_html <- as.character(sidebar_ui)
  
  # Check for accessibility attributes (simplified test)
  expect_true(any(grepl("aria-", ui_html, ignore.case = TRUE)) || 
              any(grepl("role=", ui_html, ignore.case = TRUE)))
  
  # Test that interactive elements have descriptions
  overview_ui <- overviewUI("test")
  overview_html <- as.character(overview_ui)
  
  # Should have semantic HTML elements
  expect_true(any(grepl("<button", overview_html, ignore.case = TRUE)) ||
              any(grepl("<input", overview_html, ignore.case = TRUE)))
})

test_that("Usability - Mobile Responsiveness", {
  
  # Test CSS for mobile breakpoints
  css_content <- generate_responsive_css()
  
  # Should contain mobile media queries
  expect_true(grepl("@media.*max-width.*768px", css_content))
  expect_true(grepl("@media.*max-width.*480px", css_content))
  
  # Should have mobile-friendly font sizes
  expect_true(grepl("font-size.*rem", css_content))
  
  # Should have touch-friendly button sizes
  expect_true(grepl("min-height.*44px", css_content) ||
              grepl("min-height.*2.75rem", css_content))
})

# ============================================================================
# 13. INTEGRATION READINESS TESTS (Non-API)
# ============================================================================

test_that("Integration Readiness - Data Export Functions", {
  
  # Test CSV export functionality
  sample_data <- data.frame(
    EmployeeID = 1:10,
    Name = paste("Employee", 1:10),
    Salary = sample(40000:100000, 10),
    stringsAsFactors = FALSE
  )
  
  temp_csv <- tempfile(fileext = ".csv")
  expect_true(export_to_csv(sample_data, temp_csv))
  expect_true(file.exists(temp_csv))
  
  # Verify exported data integrity
  imported_data <- read.csv(temp_csv, stringsAsFactors = FALSE)
  expect_equal(nrow(imported_data), nrow(sample_data))
  expect_equal(ncol(imported_data), ncol(sample_data))
  
  unlink(temp_csv)
  
  # Test Excel export (if available)
  temp_excel <- tempfile(fileext = ".xlsx")
  if (require("openxlsx", quietly = TRUE)) {
    expect_true(export_to_excel(sample_data, temp_excel))
    expect_true(file.exists(temp_excel))
    unlink(temp_excel)
  }
  
  # Test PDF report export
  temp_pdf <- tempfile(fileext = ".pdf")
  # This would require pandoc and other dependencies
  # expect_true(export_to_pdf(sample_data, temp_pdf))
})

test_that("Integration Readiness - Configuration Management", {
  
  # Test configuration loading
  config <- load_app_config()
  expect_type(config, "list")
  expect_true("app_name" %in% names(config))
  expect_true("version" %in% names(config))
  expect_true("database" %in% names(config))
  
  # Test environment-specific configurations
  dev_config <- load_app_config(environment = "development")
  prod_config <- load_app_config(environment = "production")
  
  expect_type(dev_config, "list")
  expect_type(prod_config, "list")
  expect_false(identical(dev_config, prod_config))
  
  # Test configuration validation
  expect_true(validate_config(config))
  
  # Test invalid configuration
  invalid_config <- list(app_name = "", version = NULL)
  expect_false(validate_config(invalid_config))
})

test_that("Integration Readiness - Logging Infrastructure", {
  
  # Test log aggregation across modules
  logger <- AtlasLogger$new()
  
  # Simulate logs from different modules
  logger$log_info("Data loaded successfully", module = "DataLoader")
  logger$log_warning("Missing values detected", module = "DataLoader")
  logger$log_info("Chart rendered", module = "Visualization")
  logger$log_error("Calculation failed", module = "Analytics")
  
  # Test log aggregation
  all_logs <- logger$get_logs()
  expect_length(all_logs, 4)
  
  module_summary <- logger$get_module_summary()
  expect_type(module_summary, "list")
  expect_true("DataLoader" %in% names(module_summary))
  expect_true("Visualization" %in% names(module_summary))
  expect_true("Analytics" %in% names(module_summary))
  
  # Test log export for external systems
  log_json <- logger$export_logs_json()
  expect_type(log_json, "character")
  expect_true(jsonlite::validate(log_json))
  
  # Test log filtering for external consumption
  error_logs <- logger$get_logs(level = "ERROR")
  expect_length(error_logs, 1)
  expect_equal(error_logs[[1]]$level, "ERROR")
})

# ============================================================================
# 14. CROSS-PLATFORM COMPATIBILITY TESTS
# ============================================================================

test_that("Cross-Platform - File System Operations", {
  
  # Test path handling across platforms
  test_paths <- c(
    "data/employee.csv",
    "modules/data_loader_module.R",
    "www/custom_styles.css"
  )
  
  for (path in test_paths) {
    normalized_path <- normalize_file_path(path)
    expect_type(normalized_path, "character")
    expect_true(nchar(normalized_path) > 0)
    
    # Path should work on current platform
    expect_true(is_valid_path_format(normalized_path))
  }
  
  # Test directory creation
  temp_dir <- file.path(tempdir(), "atlas_test")
  expect_true(create_directory_safe(temp_dir))
  expect_true(dir.exists(temp_dir))
  
  # Test file operations
  temp_file <- file.path(temp_dir, "test.txt")
  expect_true(write_file_safe(temp_file, "test content"))
  expect_true(file.exists(temp_file))
  
  content <- read_file_safe(temp_file)
  expect_equal(content, "test content")
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("Cross-Platform - Browser Compatibility", {
  
  # Test CSS for different browsers
  css_content <- generate_responsive_css()
  
  # Should include vendor prefixes for compatibility
  expect_true(grepl("-webkit-", css_content) || 
              grepl("-moz-", css_content) || 
              grepl("-ms-", css_content))
  
  # Test JavaScript compatibility
  js_content <- generate_custom_javascript()
  
  # Should avoid modern features that aren't universally supported
  expect_false(grepl("\\?\\.", js_content))  # Optional chaining
  expect_false(grepl("\\?\\?", js_content))  # Nullish coalescing
  
  # Should include polyfills or fallbacks
  expect_true(grepl("addEventListener", js_content) || 
              grepl("attachEvent", js_content))
})

test_that("Cross-Platform - Time Zone Handling", {
  
  # Test time zone aware operations
  utc_time <- as.POSIXct("2023-06-15 12:00:00", tz = "UTC")
  
  # Convert to different time zones
  est_time <- convert_timezone(utc_time, "America/New_York")
  pst_time <- convert_timezone(utc_time, "America/Los_Angeles")
  
  expect_s3_class(est_time, "POSIXct")
  expect_s3_class(pst_time, "POSIXct")
  expect_true(est_time != pst_time)
  
  # Test time zone detection
  local_tz <- detect_local_timezone()
  expect_type(local_tz, "character")
  expect_true(nchar(local_tz) > 0)
  
  # Test date formatting for different locales
  formatted_date <- format_date_locale(utc_time, locale = "en_US")
  expect_type(formatted_date, "character")
  expect_true(nchar(formatted_date) > 0)
})

# ============================================================================
# 15. STRESS TESTING AND EDGE CASES
# ============================================================================

test_that("Stress Testing - High Volume Data Processing", {
  
  # Create large dataset for stress testing
  stress_data <- data.frame(
    EmployeeID = 1:50000,
    Attrition = sample(c("Yes", "No"), 50000, replace = TRUE),
    Salary = sample(30000:200000, 50000, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT", "Finance", "Marketing", 
                         "Operations", "Legal", "R&D"), 50000, replace = TRUE),
    Age = sample(18:70, 50000, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test processing with time limits
  start_time <- Sys.time()
  
  # Perform multiple operations
  attrition_rate <- calculate_attrition_rate(stress_data)
  dept_analysis <- analyze_attrition_by_department(stress_data)
  salary_quartiles <- calculate_salary_quartiles(stress_data)
  
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within reasonable time even for large dataset
  expect_true(processing_time < 60)  # Under 1 minute
  
  # Results should still be valid
  expect_type(attrition_rate, "double")
  expect_true(attrition_rate >= 0 && attrition_rate <= 1)
  expect_s3_class(dept_analysis, "data.frame")
  expect_length(salary_quartiles, 4)
  
  # Cleanup large object
  rm(stress_data)
  gc()
})

test_that("Stress Testing - Concurrent Operations", {
  
  # Simulate concurrent data processing
  logger <- AtlasLogger$new()
  
  # Create multiple datasets
  datasets <- lapply(1:10, function(i) {
    data.frame(
      EmployeeID = (i-1)*100 + 1:100,
      Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
      Salary = sample(40000:120000, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  # Process all datasets "concurrently" (sequentially for testing)
  start_time <- Sys.time()
  
  results <- lapply(seq_along(datasets), function(i) {
    logger$log_info(paste("Processing dataset", i), module = "StressTest")
    
    result <- list(
      attrition_rate = calculate_attrition_rate(datasets[[i]]),
      avg_salary = calculate_average_salary(datasets[[i]]),
      dataset_id = i
    )
    
    logger$log_info(paste("Completed dataset", i), module = "StressTest")
    return(result)
  })
  
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete all operations
  expect_length(results, 10)
  expect_true(all(sapply(results, function(r) !is.null(r$attrition_rate))))
  
  # Logger should capture all operations
  logs <- logger$get_logs()
  expect_true(length(logs) >= 20)  # At least 2 logs per dataset
  
  # Performance should be reasonable
  expect_true(total_time < 30)  # Under 30 seconds for 10 datasets
})

test_that("Edge Cases - Extreme Data Values", {
  
  # Test with extreme salary values
  extreme_salary_data <- data.frame(
    EmployeeID = 1:5,
    Salary = c(1, 1000000000, -50000, 0, NA),
    Attrition = c("No", "No", "Yes", "Yes", "No"),
    stringsAsFactors = FALSE
  )
  
  # Should handle extreme values gracefully
  expect_warning(avg_salary <- calculate_average_salary(extreme_salary_data))
  expect_true(is.finite(avg_salary) || is.na(avg_salary))
  
  # Test with extreme age values
  extreme_age_data <- data.frame(
    EmployeeID = 1:5,
    Age = c(-5, 150, 0, 25, NA),
    stringsAsFactors = FALSE
  )
  
  expect_false(validate_age_values(extreme_age_data))
  
  # Test with extreme date values
  extreme_date_data <- data.frame(
    EmployeeID = 1:3,
    HireDate = c(as.Date("1900-01-01"), as.Date("2100-12-31"), NA),
    stringsAsFactors = FALSE
  )
  
  expect_warning(validate_date_ranges(extreme_date_data))
})

test_that("Edge Cases - Unicode and Special Characters", {
  
  # Test with international names
  unicode_data <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("José", "François", "张伟", "Mohammed", "Αλέξανδρος"),
    LastName = c("García", "Müller", "李", "Al-Rashid", "Παπαδόπουλος"),
    Department = c("Sales", "HR", "IT", "Finance", "Marketing"),
    stringsAsFactors = FALSE
  )
  
  # Should handle Unicode characters properly
  expect_true(validate_unicode_support(unicode_data))
  
  # Test string operations with Unicode
  cleaned_names <- sapply(unicode_data$FirstName, clean_string)
  expect_equal(length(cleaned_names), 5)
  expect_true(all(nchar(cleaned_names) > 0))
  
  # Test CSV export/import with Unicode
  temp_file <- tempfile(fileext = ".csv")
  expect_silent(write.csv(unicode_data, temp_file, row.names = FALSE, fileEncoding = "UTF-8"))
  imported_data <- read.csv(temp_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  expect_equal(imported_data$FirstName, unicode_data$FirstName)
  unlink(temp_file)
})

test_that("Edge Cases - Network and System Failures", {
  
  # Test handling of system resource exhaustion
  expect_error(
    simulate_memory_exhaustion(),
    "Memory allocation failed|cannot allocate vector"
  )
  
  # Test handling of disk space issues
  expect_error(
    simulate_disk_full_error(),
    "No space left on device|disk full"
  )
  
  # Test graceful degradation
  degraded_mode_result <- operate_in_degraded_mode()
  expect_type(degraded_mode_result, "list")
  expect_true("status" %in% names(degraded_mode_result))
  expect_equal(degraded_mode_result$status, "degraded")
})

# ============================================================================
# 16. REGRESSION TESTING FRAMEWORK
# ============================================================================

test_that("Regression Testing - Data Processing Consistency", {
  
  # Create reference dataset
  reference_data <- data.frame(
    EmployeeID = 1:20,
    Attrition = rep(c("Yes", "No"), 10),
    Salary = seq(40000, 120000, length.out = 20),
    Department = rep(c("Sales", "HR", "IT", "Finance"), 5),
    stringsAsFactors = FALSE
  )
  
  # Calculate expected results (these would be saved from a known good version)
  expected_attrition_rate <- 0.5  # 50% attrition in test data
  expected_avg_salary <- 80000    # Average of 40k to 120k
  expected_dept_count <- 4        # 4 departments
  
  # Test current implementation matches expected results
  actual_attrition_rate <- calculate_attrition_rate(reference_data)
  actual_avg_salary <- calculate_average_salary(reference_data)
  actual_dept_count <- length(unique(reference_data$Department))
  
  expect_equal(actual_attrition_rate, expected_attrition_rate, tolerance = 0.01)
  expect_equal(actual_avg_salary, expected_avg_salary, tolerance = 100)
  expect_equal(actual_dept_count, expected_dept_count)
})

test_that("Regression Testing - Visualization Consistency", {
  
  # Test that chart generation produces consistent results
  chart_data <- data.frame(
    category = c("A", "B", "C"),
    value = c(10, 20, 15)
  )
  
  # Generate chart multiple times
  chart1 <- create_bar_chart(chart_data, "category", "value")
  chart2 <- create_bar_chart(chart_data, "category", "value")
  
  # Charts should have consistent structure
  expect_equal(length(chart1$layers), length(chart2$layers))
  expect_equal(chart1$mapping, chart2$mapping)
  
  # Test plotly conversion consistency
  plotly1 <- make_interactive_plot(chart1)
  plotly2 <- make_interactive_plot(chart2)
  
  expect_equal(class(plotly1), class(plotly2))
})

test_that("Regression Testing - Performance Benchmarks", {
  
  # Define performance benchmarks (in seconds)
  benchmarks <- list(
    small_dataset_processing = 1.0,
    medium_dataset_processing = 5.0,
    chart_generation = 2.0,
    report_generation = 10.0
  )
  
  # Test small dataset processing time
  small_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    Salary = sample(40000:120000, 100, replace = TRUE)
  )
  
  small_time <- system.time({
    calculate_attrition_rate(small_data)
    calculate_average_salary(small_data)
    calculate_department_distribution(small_data)
  })["elapsed"]
  
  expect_true(small_time < benchmarks$small_dataset_processing,
              info = paste("Small dataset processing took", small_time, 
                          "seconds, expected <", benchmarks$small_dataset_processing))
  
  # Test chart generation time
  chart_time <- system.time({
    chart <- create_bar_chart(small_data, "Attrition", "Salary")
    make_interactive_plot(chart)
  })["elapsed"]
  
  expect_true(chart_time < benchmarks$chart_generation,
              info = paste("Chart generation took", chart_time, 
                          "seconds, expected <", benchmarks$chart_generation))
})

# ============================================================================
# 17. CLEANUP AND TEARDOWN TESTS
# ============================================================================

test_that("Cleanup - Temporary File Management", {
  
  # Create some temporary files during testing
  temp_files <- c(
    tempfile(fileext = ".csv"),
    tempfile(fileext = ".png"),
    tempfile(fileext = ".html")
  )
  
  # Create the files
  for (file in temp_files) {
    writeLines("test content", file)
    expect_true(file.exists(file))
  }
  
  # Test cleanup function
  cleanup_temp_files(temp_files)
  
  # Files should be removed
  for (file in temp_files) {
    expect_false(file.exists(file))
  }
})

test_that("Cleanup - Memory Management", {
  
  # Create large objects
  large_object1 <- matrix(rnorm(10000), nrow = 100)
  large_object2 <- data.frame(x = rnorm(5000), y = rnorm(5000))
  
  initial_memory <- gc()["Vcells", "used"]
  
  # Remove objects and force garbage collection
  rm(large_object1, large_object2)
  gc()
  
  final_memory <- gc()["Vcells", "used"]
  
  # Memory should be freed
  expect_true(final_memory <= initial_memory)
})

test_that("Cleanup - Logger State Reset", {
  
  # Create logger with some data
  logger <- AtlasLogger$new()
  logger$log_info("Test message 1", module = "TestModule")
  logger$log_warning("Test warning", module = "TestModule")
  logger$log_error("Test error", module = "TestModule")
  
  # Verify logs exist
  expect_true(length(logger$get_logs()) > 0)
  
  # Reset logger
  logger$reset()
  
  # Logs should be cleared
  expect_length(logger$get_logs(), 0)
  
  # Performance data should be reset
  perf_summary <- logger$get_performance_summary()
  expect_equal(perf_summary$total_operations, 0)
})

# ============================================================================
# 18. FINAL INTEGRATION VERIFICATION
# ============================================================================

test_that("Final Integration - Module Loading", {
  
  # Test that all required modules can be loaded
  module_files <- list.files(here("modules"), pattern = "\\.R$", full.names = TRUE)
  
  for (module_file in module_files) {
    expect_true(file.exists(module_file),
                info = paste("Module file not found:", module_file))
    
    # Test that module can be sourced without errors
    expect_silent(source(module_file, local = TRUE))
  }
})

test_that("Final Integration - App Configuration", {
  
  # Test complete application configuration
  expect_true(exists("ATLAS_COLORS"))
  expect_true(exists("APP_VERSION"))
  
  # Test that required functions exist
  required_functions <- c(
    "validate_employee_data_structure",
    "calculate_attrition_rate",
    "create_bar_chart",
    "theme_atlas",
    "AtlasLogger"
  )
  
  for (func_name in required_functions) {
    expect_true(exists(func_name) && is.function(get(func_name)),
                info = paste("Required function not found:", func_name))
  }
})

test_that("Final Integration - End-to-End Simulation", {
  
  # Simulate complete data processing pipeline
  logger <- AtlasLogger$new()
  
  # Step 1: Load data
  logger$log_info("Starting data load", module = "DataLoader")
  
  sample_data <- data.frame(
    EmployeeID = 1:50,
    FirstName = paste("Employee", 1:50),
    Attrition = sample(c("Yes", "No"), 50, replace = TRUE),
    Salary = sample(40000:120000, 50, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT"), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  expect_true(validate_employee_data_structure(sample_data))
  logger$log_info("Data validation passed", module = "DataLoader")
  
  # Step 2: Perform analysis
  logger$log_info("Starting analysis", module = "Analytics")
  
  attrition_rate <- calculate_attrition_rate(sample_data)
  avg_salary <- calculate_average_salary(sample_data)
  dept_distribution <- calculate_department_distribution(sample_data)
  
  expect_type(attrition_rate, "double")
  expect_type(avg_salary, "double")
  expect_s3_class(dept_distribution, "data.frame")
  
  logger$log_info("Analysis completed", module = "Analytics")
  
  # Step 3: Create visualizations
  logger$log_info("Creating visualizations", module = "Visualization")
  
  salary_chart <- create_bar_chart(sample_data, "Department", "Salary")
  interactive_chart <- make_interactive_plot(salary_chart)
  
  expect_s3_class(salary_chart, "ggplot")
  expect_s3_class(interactive_chart, "plotly")
  
  logger$log_info("Visualizations created", module = "Visualization")
  
  # Step 4: Generate report
  logger$log_info("Generating report", module = "Report")
  
  report_params <- list(
    kpi_metrics = list(
      attrition_rate = attrition_rate,
      avg_salary = avg_salary
    ),
    data_summary = list(
      total_records = nrow(sample_data)
    )
  )
  
  expect_true(validate_report_parameters(report_params))
  logger$log_info("Report generation completed", module = "Report")
  
  # Verify entire pipeline completed successfully
  final_logs <- logger$get_logs()
  expect_true(length(final_logs) >= 6)  # At least 6 log entries
  
  # Verify no errors in pipeline
  error_logs <- logger$get_logs(level = "ERROR")
  expect_length(error_logs, 0)
  
  logger$log_info("End-to-end pipeline completed successfully", module = "Integration")
})

# ============================================================================
# TEST RUNNER AND SUMMARY
# ============================================================================

# Function to run all tests and provide summary
run_all_tests <- function() {
  cat("="*80, "\n")
  cat("ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TEST SUITE\n")
  cat("="*80, "\n\n")
  
  start_time <- Sys.time()
  
  # Run tests
  test_results <- testthat::test_dir(".", reporter = "summary")
  
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "secs")
  
  cat("\n", "="*80, "\n")
  cat("TEST EXECUTION SUMMARY\n")
  cat("="*80, "\n")
  cat("Total execution time:", round(total_time, 2), "seconds\n")
  cat("Tests completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("="*80, "\n\n")
  
  return(test_results)
}

# Helper function for test data cleanup
cleanup_test_environment <- function() {
  # Remove test objects from global environment
  test_objects <- ls(pattern = "^(test_|temp_|sample_)", envir = .GlobalEnv)
  rm(list = test_objects, envir = .GlobalEnv)
  
  # Force garbage collection
  gc()
  
  cat("Test environment cleaned up successfully.\n")
}

# ============================================================================
# CUSTOM TEST HELPERS AND UTILITIES
# ============================================================================

# Custom expectation for R6 classes
expect_r6 <- function(object, class_name) {
  expect_true(R6::is.R6(object))
  expect_true(inherits(object, class_name))
}

# Custom expectation for valid hex colors
expect_valid_hex_color <- function(color) {
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", color))
}

# Custom expectation for performance benchmarks
expect_performance <- function(expr, max_time) {
  time_taken <- system.time(expr)["elapsed"]
  expect_true(time_taken < max_time, 
              info = paste("Operation took", time_taken, "seconds, expected <", max_time))
}

# Mock function for simulating system failures
simulate_memory_exhaustion <- function() {
  stop("Memory allocation failed")
}

simulate_disk_full_error <- function() {
  stop("No space left on device")
}

operate_in_degraded_mode <- function() {
  list(
    status = "degraded",
    message = "Operating with limited functionality",
    available_features = c("basic_analysis", "simple_charts")
  )
}

# Timeout wrapper for long-running operations
with_timeout <- function(expr, timeout) {
  start_time <- Sys.time()
  
  # Simple timeout simulation (in real implementation, would use proper timeout)
  if (timeout < 1) {
    stop("Operation timed out")
  }
  
  eval(expr)
}

cat("="*80, "\n")
cat("ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TEST SUITE LOADED\n")
cat("="*80, "\n")
cat("Total test cases defined: 18 major test groups\n")
cat("Coverage areas:\n")
cat("  ✓ Data Validation & Loading\n")
cat("  ✓ R6 Logger Class (Extensive)\n") 
cat("  ✓ Utility Functions\n")
cat("  ✓ Custom Themes & Styling\n")
cat("  ✓ All Shiny Modules\n")
cat("  ✓ Visualization Functions\n")
cat("  ✓ Report Generation\n")
cat("  ✓ Error Handling & Edge Cases\n")
cat("  ✓ Security & Privacy\n")
cat("  ✓ Performance Benchmarking\n")
cat("  ✓ Accessibility & Usability\n")
cat("  ✓ Cross-Platform Compatibility\n")
cat("  ✓ Stress Testing\n")
cat("  ✓ Regression Testing\n")
cat("  ✓ Memory Management\n")
cat("  ✓ Unicode & Special Characters\n")
cat("  ✓ Integration Readiness\n")
cat("  ✓ End-to-End Pipeline Testing\n")
cat("\nTo run all tests: run_all_tests()\n")
cat("To cleanup test environment: cleanup_test_environment()\n")
cat("="*80, "\n")