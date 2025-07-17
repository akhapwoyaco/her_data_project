# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Author: akhapwoyaco
# Purpose: Extensive unit testing covering all components except database performance
# Libraries: testthat, shinytest2, mockery, withr, fs
# =============================================================================

# Required Libraries
library(testthat)
library(shinytest2)
library(mockery)
library(withr)
library(fs)
library(shiny)
library(tidyverse)
library(R6)

# Source application files (assuming they exist)
source("global.R")
source("utils.R")
source("custom_theme.R")

# Load all modules
purrr::walk(list.files("modules", full.names = TRUE), source)

# =============================================================================
# 1. R6 LOGGER SYSTEM TESTS
# =============================================================================

test_that("AtlasLogger R6 Class - Basic Functionality", {
  # Test logger initialization
  logger <- AtlasLogger$new()
  expect_s3_class(logger, "AtlasLogger")
  expect_s3_class(logger, "R6")
  
  # Test basic logging methods exist
  expect_true(exists("log_info", where = logger))
  expect_true(exists("log_warning", where = logger))
  expect_true(exists("log_error", where = logger))
  expect_true(exists("track_memory_usage", where = logger))
  expect_true(exists("track_execution_time", where = logger))
})

test_that("AtlasLogger - Log Level Validation", {
  logger <- AtlasLogger$new()
  
  # Test valid log levels
  expect_no_error(logger$log_info("Test info message", "test_module"))
  expect_no_error(logger$log_warning("Test warning message", "test_module"))
  expect_no_error(logger$log_error("Test error message", "test_module"))
  
  # Test invalid inputs
  expect_error(logger$log_info(NULL, "test_module"))
  expect_error(logger$log_info("Test", NULL))
  expect_error(logger$log_info(123, "test_module"))
})

test_that("AtlasLogger - Memory Tracking Edge Cases", {
  logger <- AtlasLogger$new()
  
  # Test memory tracking with different scenarios
  initial_memory <- logger$track_memory_usage()
  expect_type(initial_memory, "double")
  expect_gte(initial_memory, 0)
  
  # Create large object and track memory
  large_object <- rep(1:1000000, 10)
  post_allocation_memory <- logger$track_memory_usage()
  expect_gt(post_allocation_memory, initial_memory)
  
  # Clean up and test memory release
  rm(large_object)
  gc()
  post_cleanup_memory <- logger$track_memory_usage()
  expect_lte(post_cleanup_memory, post_allocation_memory)
})

test_that("AtlasLogger - Performance Timing Edge Cases", {
  logger <- AtlasLogger$new()
  
  # Test execution time tracking
  start_time <- Sys.time()
  Sys.sleep(0.1)  # 100ms sleep
  execution_time <- logger$track_execution_time(start_time)
  
  expect_type(execution_time, "double")
  expect_gte(execution_time, 0.1)
  expect_lte(execution_time, 0.2)  # Should be close to 0.1 seconds
  
  # Test with negative time (edge case)
  future_time <- Sys.time() + 10
  expect_error(logger$track_execution_time(future_time))
})

test_that("AtlasLogger - Concurrent Logging Stress Test", {
  logger <- AtlasLogger$new()
  
  # Simulate concurrent logging from multiple modules
  modules <- paste0("module_", 1:50)
  messages <- paste0("Test message ", 1:50)
  
  # Test rapid logging without errors
  expect_no_error({
    for (i in 1:50) {
      logger$log_info(messages[i], modules[i])
      logger$log_warning(paste("Warning", messages[i]), modules[i])
    }
  })
  
  # Verify log count
  log_summary <- logger$get_performance_summary()
  expect_gte(log_summary$total_logs, 100)
})

# =============================================================================
# 2. DATA LOADER MODULE TESTS
# =============================================================================

test_that("Data Loader - File Validation", {
  # Create temporary test files
  temp_dir <- tempdir()
  
  # Valid CSV file
  valid_csv <- file.path(temp_dir, "valid_test.csv")
  write.csv(data.frame(ID = 1:5, Name = letters[1:5]), valid_csv, row.names = FALSE)
  
  # Invalid CSV file (corrupted)
  invalid_csv <- file.path(temp_dir, "invalid_test.csv")
  writeLines("Invalid,CSV,Content\n1,2", invalid_csv)
  
  # Empty file
  empty_csv <- file.path(temp_dir, "empty_test.csv")
  file.create(empty_csv)
  
  # Test file validation
  expect_true(validate_data_integrity(valid_csv))
  expect_false(validate_data_integrity(invalid_csv))
  expect_false(validate_data_integrity(empty_csv))
  expect_false(validate_data_integrity("nonexistent_file.csv"))
  
  # Cleanup
  unlink(c(valid_csv, invalid_csv, empty_csv))
})

test_that("Data Loader - CSV Parsing Edge Cases", {
  temp_dir <- tempdir()
  
  # CSV with special characters
  special_csv <- file.path(temp_dir, "special_chars.csv")
  special_data <- data.frame(
    Name = c("John O'Connor", "María José", "王小明"),
    Description = c("Line 1\nLine 2", "Tab\tSeparated", "Comma, Separated"),
    stringsAsFactors = FALSE
  )
  write.csv(special_data, special_csv, row.names = FALSE)
  
  # Test parsing with special characters
  parsed_data <- load_employee_data(special_csv)
  expect_equal(nrow(parsed_data), 3)
  expect_equal(ncol(parsed_data), 2)
  expect_true(grepl("'", parsed_data$Name[1]))
  
  # CSV with missing values
  missing_csv <- file.path(temp_dir, "missing_values.csv")
  writeLines("ID,Name,Age\n1,John,25\n2,,30\n3,Jane,", missing_csv)
  
  parsed_missing <- load_employee_data(missing_csv)
  expect_equal(nrow(parsed_missing), 3)
  expect_true(is.na(parsed_missing$Name[2]))
  expect_true(is.na(parsed_missing$Age[3]))
  
  # Cleanup
  unlink(c(special_csv, missing_csv))
})

test_that("Data Loader - Large File Handling", {
  temp_dir <- tempdir()
  large_csv <- file.path(temp_dir, "large_file.csv")
  
  # Create large dataset (10,000 rows)
  large_data <- data.frame(
    ID = 1:10000,
    Name = paste0("Employee_", 1:10000),
    Department = sample(c("HR", "IT", "Finance", "Marketing"), 10000, replace = TRUE),
    Salary = round(runif(10000, 30000, 120000), 2),
    stringsAsFactors = FALSE
  )
  
  write.csv(large_data, large_csv, row.names = FALSE)
  
  # Test loading large file
  start_time <- Sys.time()
  loaded_data <- load_employee_data(large_csv)
  load_time <- as.numeric(Sys.time() - start_time)
  
  expect_equal(nrow(loaded_data), 10000)
  expect_equal(ncol(loaded_data), 4)
  expect_lt(load_time, 5)  # Should load within 5 seconds
  
  # Cleanup
  unlink(large_csv)
})

# =============================================================================
# 3. UTILITY FUNCTIONS TESTS
# =============================================================================

test_that("Utility Functions - Data Type Conversion", {
  # Test numeric conversion
  expect_equal(safe_numeric("123"), 123)
  expect_equal(safe_numeric("123.45"), 123.45)
  expect_is(safe_numeric("invalid"), "numeric")
  expect_true(is.na(safe_numeric("invalid")))
  
  # Test date conversion
  expect_equal(safe_date("2023-01-01"), as.Date("2023-01-01"))
  expect_equal(safe_date("01/01/2023", format = "%m/%d/%Y"), as.Date("2023-01-01"))
  expect_true(is.na(safe_date("invalid_date")))
  
  # Test percentage conversion
  expect_equal(safe_percentage("50%"), 0.5)
  expect_equal(safe_percentage("100%"), 1.0)
  expect_true(is.na(safe_percentage("invalid%")))
})

test_that("Utility Functions - Data Validation", {
  # Test email validation
  expect_true(validate_email("test@example.com"))
  expect_true(validate_email("user.name+tag@domain.co.uk"))
  expect_false(validate_email("invalid.email"))
  expect_false(validate_email("@domain.com"))
  expect_false(validate_email("user@"))
  
  # Test phone number validation
  expect_true(validate_phone("123-456-7890"))
  expect_true(validate_phone("(123) 456-7890"))
  expect_true(validate_phone("123.456.7890"))
  expect_false(validate_phone("123-456-789"))  # Too short
  expect_false(validate_phone("abc-def-ghij"))  # Non-numeric
  
  # Test ID validation
  expect_true(validate_employee_id("EMP001"))
  expect_true(validate_employee_id("EMP12345"))
  expect_false(validate_employee_id("123"))  # No prefix
  expect_false(validate_employee_id("EMP"))  # No number
})

test_that("Utility Functions - Statistical Calculations", {
  # Test confidence interval calculation
  sample_data <- rnorm(100, mean = 50, sd = 10)
  ci <- calculate_confidence_interval(sample_data)
  
  expect_length(ci, 2)
  expect_named(ci, c("lower", "upper"))
  expect_lt(ci$lower, mean(sample_data))
  expect_gt(ci$upper, mean(sample_data))
  
  # Test with edge cases
  expect_error(calculate_confidence_interval(c()))  # Empty vector
  expect_error(calculate_confidence_interval(c(1)))  # Single value
  expect_error(calculate_confidence_interval(c(NA, NA, NA)))  # All NA
  
  # Test outlier detection
  outlier_data <- c(1, 2, 3, 4, 5, 100)  # 100 is outlier
  outliers <- detect_outliers(outlier_data)
  expect_true(100 %in% outliers)
  expect_false(3 %in% outliers)
})

# =============================================================================
# 4. ATTRITION MODULE TESTS
# =============================================================================

test_that("Attrition Module - Analysis Functions", {
  # Create sample attrition data
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Department = sample(c("HR", "IT", "Finance"), 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7)),
    YearsAtCompany = sample(1:20, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    Salary = round(runif(100, 30000, 120000), 2),
    stringsAsFactors = FALSE
  )
  
  # Test attrition rate calculation
  attrition_rates <- calculate_attrition_rates(sample_data)
  expect_type(attrition_rates, "list")
  expect_true("overall" %in% names(attrition_rates))
  expect_true("by_department" %in% names(attrition_rates))
  
  # Test attrition rate bounds
  expect_gte(attrition_rates$overall, 0)
  expect_lte(attrition_rates$overall, 1)
  
  # Test department-specific rates
  dept_rates <- attrition_rates$by_department
  expect_true(all(dept_rates$attrition_rate >= 0))
  expect_true(all(dept_rates$attrition_rate <= 1))
})

test_that("Attrition Module - Predictive Modeling", {
  # Create training data
  training_data <- data.frame(
    YearsAtCompany = sample(1:20, 500, replace = TRUE),
    JobSatisfaction = sample(1:5, 500, replace = TRUE),
    Salary = round(runif(500, 30000, 120000), 2),
    OverTime = sample(c("Yes", "No"), 500, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 500, replace = TRUE, prob = c(0.25, 0.75)),
    stringsAsFactors = FALSE
  )
  
  # Test model training
  model <- train_attrition_model(training_data)
  expect_type(model, "list")
  expect_true("model" %in% names(model))
  expect_true("accuracy" %in% names(model))
  
  # Test predictions
  test_data <- training_data[1:10, ]
  predictions <- predict_attrition_risk(model, test_data)
  expect_length(predictions, 10)
  expect_true(all(predictions >= 0 & predictions <= 1))
})

# =============================================================================
# 5. DEMOGRAPHICS MODULE TESTS
# =============================================================================

test_that("Demographics Module - Distribution Analysis", {
  # Create sample demographic data
  demo_data <- data.frame(
    EmployeeID = 1:200,
    Gender = sample(c("Male", "Female", "Other"), 200, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
    Age = sample(22:65, 200, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 200, replace = TRUE),
    Department = sample(c("HR", "IT", "Finance", "Marketing", "Sales"), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test demographic distribution
  distribution <- analyze_demographic_distribution(demo_data)
  expect_type(distribution, "list")
  expect_true("gender" %in% names(distribution))
  expect_true("age_groups" %in% names(distribution))
  expect_true("ethnicity" %in% names(distribution))
  
  # Test diversity metrics
  diversity_metrics <- calculate_diversity_metrics(demo_data)
  expect_type(diversity_metrics, "list")
  expect_true("diversity_index" %in% names(diversity_metrics))
  expect_gte(diversity_metrics$diversity_index, 0)
  expect_lte(diversity_metrics$diversity_index, 1)
})

test_that("Demographics Module - Age Group Analysis", {
  # Test age group categorization
  ages <- c(22, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  age_groups <- categorize_age_groups(ages)
  
  expect_length(age_groups, 10)
  expect_true(all(age_groups %in% c("22-30", "31-40", "41-50", "51-60", "60+")))
  
  # Test edge cases
  expect_equal(categorize_age_groups(c(22)), "22-30")
  expect_equal(categorize_age_groups(c(30)), "22-30")
  expect_equal(categorize_age_groups(c(31)), "31-40")
  expect_equal(categorize_age_groups(c(65)), "60+")
  expect_equal(categorize_age_groups(c(100)), "60+")
})

# =============================================================================
# 6. PERFORMANCE MODULE TESTS
# =============================================================================

test_that("Performance Module - Rating Analysis", {
  # Create sample performance data
  perf_data <- data.frame(
    EmployeeID = 1:150,
    ManagerRating = sample(1:5, 150, replace = TRUE),
    SelfRating = sample(1:5, 150, replace = TRUE),
    JobSatisfaction = sample(1:5, 150, replace = TRUE),
    TrainingOpportunitiesOffered = sample(0:10, 150, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Ensure training taken <= training offered
  perf_data$TrainingOpportunitiesTaken <- pmin(perf_data$TrainingOpportunitiesTaken, 
                                               perf_data$TrainingOpportunitiesOffered)
  
  # Test performance metrics calculation
  metrics <- calculate_performance_metrics(perf_data)
  expect_type(metrics, "list")
  expect_true("avg_manager_rating" %in% names(metrics))
  expect_true("avg_self_rating" %in% names(metrics))
  expect_true("rating_correlation" %in% names(metrics))
  
  # Test training utilization
  training_util <- calculate_training_utilization(perf_data)
  expect_gte(training_util, 0)
  expect_lte(training_util, 1)
})

test_that("Performance Module - Rating Bias Detection", {
  # Create biased rating data
  biased_data <- data.frame(
    EmployeeID = 1:100,
    ManagerRating = c(rep(5, 50), rep(1, 50)),  # Extreme ratings
    SelfRating = sample(3:4, 100, replace = TRUE),  # Moderate self-ratings
    Department = c(rep("Favorites", 50), rep("Others", 50)),
    stringsAsFactors = FALSE
  )
  
  # Test bias detection
  bias_analysis <- detect_rating_bias(biased_data)
  expect_type(bias_analysis, "list")
  expect_true("department_bias" %in% names(bias_analysis))
  expect_true("rating_variance" %in% names(bias_analysis))
  
  # Should detect significant bias
  expect_gt(bias_analysis$rating_variance, 1)
})

# =============================================================================
# 7. COMPENSATION MODULE TESTS
# =============================================================================

test_that("Compensation Module - Salary Analysis", {
  # Create sample compensation data
  comp_data <- data.frame(
    EmployeeID = 1:300,
    Salary = round(runif(300, 35000, 150000), 2),
    Gender = sample(c("Male", "Female"), 300, replace = TRUE),
    Department = sample(c("HR", "IT", "Finance", "Marketing"), 300, replace = TRUE),
    JobRole = sample(c("Analyst", "Manager", "Director", "VP"), 300, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05)),
    YearsAtCompany = sample(1:25, 300, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test pay equity analysis
  pay_equity <- analyze_pay_equity(comp_data)
  expect_type(pay_equity, "list")
  expect_true("gender_pay_gap" %in% names(pay_equity))
  expect_true("department_analysis" %in% names(pay_equity))
  
  # Test salary percentiles
  percentiles <- calculate_salary_percentiles(comp_data)
  expect_length(percentiles, 9)  # 10th, 25th, 50th, 75th, 90th percentiles
  expect_true(all(diff(percentiles) >= 0))  # Should be increasing
})

test_that("Compensation Module - Pay Gap Analysis", {
  # Create data with known pay gap
  gap_data <- data.frame(
    EmployeeID = 1:100,
    Salary = c(rep(80000, 50), rep(70000, 50)),  # $10k gap
    Gender = c(rep("Male", 50), rep("Female", 50)),
    JobRole = rep("Analyst", 100),
    stringsAsFactors = FALSE
  )
  
  # Test gap calculation
  gap_analysis <- calculate_pay_gap(gap_data, "Gender")
  expect_type(gap_analysis, "list")
  expect_true("absolute_gap" %in% names(gap_analysis))
  expect_true("percentage_gap" %in% names(gap_analysis))
  
  # Should detect the $10k gap
  expect_near(gap_analysis$absolute_gap, 10000, tolerance = 100)
  expect_near(gap_analysis$percentage_gap, 0.14, tolerance = 0.01)  # ~14% gap
})

# =============================================================================
# 8. SATISFACTION MODULE TESTS
# =============================================================================

test_that("Satisfaction Module - Score Calculation", {
  # Create sample satisfaction data
  sat_data <- data.frame(
    EmployeeID = 1:200,
    JobSatisfaction = sample(1:5, 200, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 200, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 200, replace = TRUE),
    WorkLifeBalance = sample(1:5, 200, replace = TRUE),
    Department = sample(c("HR", "IT", "Finance"), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test satisfaction score calculation
  scores <- calculate_satisfaction_scores(sat_data)
  expect_type(scores, "list")
  expect_true("overall_score" %in% names(scores))
  expect_true("by_dimension" %in% names(scores))
  
  # Test score bounds
  expect_gte(scores$overall_score, 1)
  expect_lte(scores$overall_score, 5)
})

test_that("Satisfaction Module - Correlation Analysis", {
  # Create correlated satisfaction data
  n <- 100
  base_satisfaction <- rnorm(n, mean = 3, sd = 1)
  
  corr_data <- data.frame(
    EmployeeID = 1:n,
    JobSatisfaction = pmax(1, pmin(5, round(base_satisfaction + rnorm(n, 0, 0.5)))),
    EnvironmentSatisfaction = pmax(1, pmin(5, round(base_satisfaction + rnorm(n, 0, 0.5)))),
    WorkLifeBalance = pmax(1, pmin(5, round(base_satisfaction + rnorm(n, 0, 0.5)))),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test correlation analysis
  correlations <- analyze_satisfaction_correlations(corr_data)
  expect_type(correlations, "list")
  expect_true("correlation_matrix" %in% names(correlations))
  expect_true("attrition_correlation" %in% names(correlations))
})

# =============================================================================
# 9. REPORT MODULE TESTS
# =============================================================================

test_that("Report Module - Data Preparation", {
  # Create sample data for report
  report_data <- list(
    employees = data.frame(
      EmployeeID = 1:50,
      Department = sample(c("HR", "IT", "Finance"), 50, replace = TRUE),
      Attrition = sample(c("Yes", "No"), 50, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    performance = data.frame(
      EmployeeID = 1:50,
      ManagerRating = sample(1:5, 50, replace = TRUE),
      stringsAsFactors = FALSE
    )
  )
  
  # Test report data preparation
  prepared_data <- prepare_report_data(report_data)
  expect_type(prepared_data, "list")
  expect_true("summary_stats" %in% names(prepared_data))
  expect_true("visualizations" %in% names(prepared_data))
})

test_that("Report Module - Parameter Validation", {
  # Test parameter validation for report generation
  valid_params <- list(
    report_type = "executive",
    date_range = c("2023-01-01", "2023-12-31"),
    departments = c("HR", "IT"),
    include_charts = TRUE
  )
  
  invalid_params <- list(
    report_type = "invalid_type",
    date_range = c("invalid_date"),
    departments = c(),
    include_charts = "not_boolean"
  )
  
  expect_true(validate_report_parameters(valid_params))
  expect_false(validate_report_parameters(invalid_params))
})

# =============================================================================
# 10. SHINY MODULE INTEGRATION TESTS
# =============================================================================

test_that("Shiny Modules - Server Function Returns", {
  # Test that all module server functions return proper reactive objects
  testServer(overviewServer, {
    # Mock data
    session$setInputs(date_range = c("2023-01-01", "2023-12-31"))
    
    # Test outputs exist
    expect_true(exists("kpi_output"))
    expect_true(exists("chart_output"))
  })
  
  testServer(attritionServer, {
    # Mock data
    session$setInputs(department_filter = "All")
    
    # Test outputs exist
    expect_true(exists("attrition_chart"))
    expect_true(exists("attrition_table"))
  })
})

test_that("Shiny Modules - Input Validation", {
  # Test input validation in modules
  testServer(demographicsServer, {
    # Test invalid date range
    session$setInputs(date_range = c("2023-12-31", "2023-01-01"))  # End before start
    
    # Should handle gracefully
    expect_no_error(output$demographics_chart)
  })
  
  testServer(performanceServer, {
    # Test empty filter selection
    session$setInputs(department_filter = character(0))
    
    # Should handle gracefully
    expect_no_error(output$performance_chart)
  })
})

# =============================================================================
# 11. CUSTOM THEME TESTS
# =============================================================================

test_that("Custom Theme - ggplot2 Theme", {
  # Test custom theme function
  theme_atlas <- create_atlas_theme()
  
  expect_s3_class(theme_atlas, "theme")
  expect_s3_class(theme_atlas, "gg")
  
  # Test theme application
  p <- ggplot(mtcars, aes(x = mpg, y = hp)) + 
    geom_point() + 
    theme_atlas
  
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("Custom Theme - Color Palette", {
  # Test color palette functions
  atlas_colors <- get_atlas_colors()
  
  expect_type(atlas_colors, "character")
  expect_length(atlas_colors, 10)  # Should have 10 colors
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", atlas_colors)))  # Valid hex colors
  
  # Test color scaling
  scale_fill_atlas <- scale_fill_atlas()
  expect_s3_class(scale_fill_atlas, "ggproto")
})

# =============================================================================
# 12. ERROR HANDLING TESTS
# =============================================================================

test_that("Error Handling - Graceful Degradation", {
  # Test app behavior with missing data
  expect_no_error(handle_missing_data(data.frame()))
  expect_no_error(handle_missing_data(NULL))
  
  # Test with corrupted data
  corrupted_data <- data.frame(
    EmployeeID = c(1, 2, NA, 4),
    Name = c("John", "", "Jane", "Bob"),
    Salary = c(50000, -1000, 75000, NA)  # Negative salary
  )
  
  cleaned_data <- clean_data(corrupted_data)
  expect_false(any(is.na(cleaned_data$EmployeeID)))
  expect_false(any(cleaned_data$Salary < 0, na.rm = TRUE))
})

test_that("Error Handling - Network Failures", {
  # Test handling of network-related errors
  mock_network_error <- function() {
    stop("Network timeout")
  }
  
  # Should handle network errors gracefully
  expect_error(mock_network_error(), "Network timeout")
  
  # Test retry mechanism
  result <- with_retry(mock_network_error, max_attempts = 3)
  expect_null(result)  # Should return NULL after failed attempts
})

# =============================================================================
# 13. PERFORMANCE TESTS
# =============================================================================

test_that("Performance - Large Dataset Handling", {
  # Create large dataset
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Department = sample(c("HR", "IT", "Finance"), 10000, replace = TRUE),
    Salary = round(runif(10000, 30000, 150000), 2),
    stringsAsFactors = FALSE
  )
  
  # Test processing time
  start_time <- Sys.time()
  result <- process_large_dataset(large_data)
  processing_time <- as.numeric(Sys.time() - start_time)
  
  expect_lt(processing_time, 10)  # Should process within 10 seconds
  expect_equal(nrow(result), 10000)
})

test_that("Performance - Memory Usage", {
  # Test memory usage doesn't exceed limits
  initial_memory <- pryr::mem_used()
  
  # Create and process data
  test_data <- replicate(100, rnorm(1000), simplify = FALSE)
  processed_data <- lapply(test_data, function(x) summary(x))
  
  final_memory <- pryr::mem_used()
  memory_increase <- final_memory - initial_memory
  
  # Should not use more than 100MB
  expect_lt(as.numeric(memory_increase), 100 * 1024^2)
  
  # Cleanup
  rm(test_data, processed_data)
  gc()
})

# =============