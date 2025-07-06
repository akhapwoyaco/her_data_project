# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Author: akhapwoyaco
# Coverage: All modules except data protection (PII, encryption, GDPR, etc.)
# Testing Framework: testthat with extensive edge case coverage
# =============================================================================

library(testthat)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(R6)
library(mockery)
library(shinytestjs)

# Test Configuration
test_config <- list(
  test_data_dir = "test_data",
  mock_data_size = 1000,
  performance_threshold_ms = 5000,
  memory_threshold_mb = 100
)

# =============================================================================
# 1. LOGGER MODULE TESTS
# =============================================================================

test_that("AtlasLogger R6 Class - Core Functionality", {
  # Test logger initialization
  logger <- AtlasLogger$new(app_name = "test_app")
  expect_s3_class(logger, "AtlasLogger")
  expect_equal(logger$app_name, "test_app")
  expect_true(is.list(logger$logs))
  expect_equal(length(logger$logs), 0)
})

test_that("AtlasLogger - Log Level Functionality", {
  logger <- AtlasLogger$new()
  
  # Test all log levels
  logger$log_info("Test info message", "test_module")
  logger$log_warning("Test warning message", "test_module")
  logger$log_error("Test error message", "test_module")
  logger$log_debug("Test debug message", "test_module")
  
  expect_equal(length(logger$logs), 4)
  expect_equal(logger$logs[[1]]$level, "INFO")
  expect_equal(logger$logs[[2]]$level, "WARNING")
  expect_equal(logger$logs[[3]]$level, "ERROR")
  expect_equal(logger$logs[[4]]$level, "DEBUG")
})

test_that("AtlasLogger - Performance Tracking", {
  logger <- AtlasLogger$new()
  
  # Test performance tracking
  perf_data <- list(
    execution_time = 1234.5,
    memory_usage = 67.8,
    cpu_usage = 45.2
  )
  
  logger$log_performance("test_function", "test_module", perf_data)
  
  expect_true(length(logger$performance_logs) > 0)
  expect_equal(logger$performance_logs[[1]]$function_name, "test_function")
  expect_equal(logger$performance_logs[[1]]$execution_time, 1234.5)
})

test_that("AtlasLogger - Memory Usage Tracking", {
  logger <- AtlasLogger$new()
  
  # Mock memory usage
  mock_memory <- function() list(used = 1024 * 1024 * 50) # 50MB
  with_mock(
    "pryr::mem_used" = mock_memory,
    {
      logger$track_memory_usage("test_module")
      expect_true(length(logger$memory_logs) > 0)
      expect_equal(logger$memory_logs[[1]]$module, "test_module")
    }
  )
})

test_that("AtlasLogger - Edge Cases", {
  logger <- AtlasLogger$new()
  
  # Test with NULL values
  expect_warning(logger$log_info(NULL, "test_module"))
  expect_warning(logger$log_info("test", NULL))
  
  # Test with empty strings
  logger$log_info("", "")
  expect_equal(logger$logs[[1]]$message, "")
  
  # Test with very long messages
  long_message <- paste(rep("a", 10000), collapse = "")
  logger$log_info(long_message, "test_module")
  expect_equal(nchar(logger$logs[[2]]$message), 10000)
  
  # Test with special characters
  special_msg <- "Test with émojis 🚀 and spëcial chars: <>\"&'"
  logger$log_info(special_msg, "test_module")
  expect_equal(logger$logs[[3]]$message, special_msg)
})

# =============================================================================
# 2. DATA LOADER MODULE TESTS
# =============================================================================

test_that("Data Loader - File Validation", {
  # Create mock data files
  mock_employee_data <- data.frame(
    EmployeeID = 1:100,
    FirstName = paste("Employee", 1:100),
    LastName = paste("Lastname", 1:100),
    Gender = sample(c("Male", "Female", "Other"), 100, replace = TRUE),
    Age = sample(18:65, 100, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 100, replace = TRUE),
    Salary = sample(30000:150000, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE)
  )
  
  # Test valid data structure
  expect_true(validate_employee_data(mock_employee_data))
  
  # Test missing required columns
  incomplete_data <- mock_employee_data[, -1] # Remove EmployeeID
  expect_error(validate_employee_data(incomplete_data))
  
  # Test invalid data types
  invalid_data <- mock_employee_data
  invalid_data$Age <- as.character(invalid_data$Age)
  expect_warning(validate_employee_data(invalid_data))
})

test_that("Data Loader - File Reading Edge Cases", {
  # Test non-existent file
  expect_error(load_csv_file("non_existent_file.csv"))
  
  # Test empty file
  write.csv(data.frame(), "empty_file.csv", row.names = FALSE)
  expect_error(load_csv_file("empty_file.csv"))
  unlink("empty_file.csv")
  
  # Test file with only headers
  write.csv(data.frame(EmployeeID = numeric(0)), "headers_only.csv", row.names = FALSE)
  expect_error(load_csv_file("headers_only.csv"))
  unlink("headers_only.csv")
  
  # Test file with special characters in data
  special_data <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("José", "François", "München"),
    LastName = c("García", "Müller", "王"),
    stringsAsFactors = FALSE
  )
  write.csv(special_data, "special_chars.csv", row.names = FALSE)
  result <- load_csv_file("special_chars.csv")
  expect_equal(nrow(result), 3)
  unlink("special_chars.csv")
})

test_that("Data Loader - Data Integrity Checks", {
  # Test duplicate employee IDs
  duplicate_data <- data.frame(
    EmployeeID = c(1, 1, 2),
    FirstName = c("John", "Jane", "Bob"),
    Age = c(25, 30, 35)
  )
  expect_error(check_data_integrity(duplicate_data))
  
  # Test negative ages
  negative_age_data <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob"),
    Age = c(25, -5, 35)
  )
  expect_warning(check_data_integrity(negative_age_data))
  
  # Test unrealistic salary values
  unrealistic_salary <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob"),
    Salary = c(50000, 999999999, 30000)
  )
  expect_warning(check_data_integrity(unrealistic_salary))
})

# =============================================================================
# 3. OVERVIEW MODULE TESTS
# =============================================================================

test_that("Overview Module - KPI Calculations", {
  mock_data <- data.frame(
    EmployeeID = 1:1000,
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 1000, replace = TRUE),
    Salary = sample(30000:150000, 1000, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 1000, replace = TRUE, prob = c(0.15, 0.85)),
    Age = sample(18:65, 1000, replace = TRUE),
    Gender = sample(c("Male", "Female", "Other"), 1000, replace = TRUE)
  )
  
  # Test KPI calculations
  kpis <- calculate_kpis(mock_data)
  
  expect_true(is.list(kpis))
  expect_true("total_employees" %in% names(kpis))
  expect_true("attrition_rate" %in% names(kpis))
  expect_true("avg_salary" %in% names(kpis))
  expect_true("avg_age" %in% names(kpis))
  
  expect_equal(kpis$total_employees, 1000)
  expect_true(kpis$attrition_rate >= 0 && kpis$attrition_rate <= 1)
  expect_true(kpis$avg_salary >= 30000 && kpis$avg_salary <= 150000)
})

test_that("Overview Module - Edge Cases", {
  # Test with empty data
  empty_data <- data.frame()
  expect_error(calculate_kpis(empty_data))
  
  # Test with single row
  single_row <- data.frame(
    EmployeeID = 1,
    Department = "IT",
    Salary = 50000,
    Attrition = "No",
    Age = 30,
    Gender = "Male"
  )
  kpis <- calculate_kpis(single_row)
  expect_equal(kpis$total_employees, 1)
  expect_equal(kpis$attrition_rate, 0)
  
  # Test with all attrition
  all_attrition <- data.frame(
    EmployeeID = 1:10,
    Department = rep("IT", 10),
    Salary = rep(50000, 10),
    Attrition = rep("Yes", 10),
    Age = rep(30, 10),
    Gender = rep("Male", 10)
  )
  kpis <- calculate_kpis(all_attrition)
  expect_equal(kpis$attrition_rate, 1)
})

# =============================================================================
# 4. ATTRITION MODULE TESTS
# =============================================================================

test_that("Attrition Module - Analysis Functions", {
  mock_data <- data.frame(
    EmployeeID = 1:500,
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 500, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 500, replace = TRUE, prob = c(0.2, 0.8)),
    YearsAtCompany = sample(0:20, 500, replace = TRUE),
    YearsInMostRecentRole = sample(0:15, 500, replace = TRUE),
    YearsSinceLastPromotion = sample(0:10, 500, replace = TRUE),
    Salary = sample(30000:150000, 500, replace = TRUE),
    Age = sample(22:65, 500, replace = TRUE)
  )
  
  # Test attrition by department
  dept_attrition <- analyze_attrition_by_department(mock_data)
  expect_true(is.data.frame(dept_attrition))
  expect_true("Department" %in% names(dept_attrition))
  expect_true("AttritionRate" %in% names(dept_attrition))
  expect_true(all(dept_attrition$AttritionRate >= 0 & dept_attrition$AttritionRate <= 1))
  
  # Test attrition risk factors
  risk_factors <- identify_attrition_risk_factors(mock_data)
  expect_true(is.list(risk_factors))
  expect_true(length(risk_factors) > 0)
})

test_that("Attrition Module - Predictive Modeling", {
  # Create more realistic mock data for modeling
  set.seed(123)
  mock_data <- data.frame(
    EmployeeID = 1:1000,
    Age = sample(22:65, 1000, replace = TRUE),
    YearsAtCompany = sample(0:20, 1000, replace = TRUE),
    Salary = sample(30000:150000, 1000, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 1000, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 1000, replace = TRUE)
  )
  
  # Create attrition based on some logic
  mock_data$Attrition <- ifelse(
    mock_data$YearsAtCompany < 2 | 
    mock_data$OverTime == "Yes" | 
    mock_data$Salary < 40000,
    sample(c("Yes", "No"), 1000, replace = TRUE, prob = c(0.4, 0.6)),
    sample(c("Yes", "No"), 1000, replace = TRUE, prob = c(0.1, 0.9))
  )
  
  # Test predictive model
  model_results <- build_attrition_model(mock_data)
  expect_true(is.list(model_results))
  expect_true("model" %in% names(model_results))
  expect_true("accuracy" %in% names(model_results))
  expect_true("predictions" %in% names(model_results))
})

test_that("Attrition Module - Edge Cases", {
  # Test with no attrition
  no_attrition <- data.frame(
    EmployeeID = 1:100,
    Department = sample(c("IT", "HR"), 100, replace = TRUE),
    Attrition = rep("No", 100)
  )
  result <- analyze_attrition_by_department(no_attrition)
  expect_true(all(result$AttritionRate == 0))
  
  # Test with all attrition
  all_attrition <- data.frame(
    EmployeeID = 1:100,
    Department = sample(c("IT", "HR"), 100, replace = TRUE),
    Attrition = rep("Yes", 100)
  )
  result <- analyze_attrition_by_department(all_attrition)
  expect_true(all(result$AttritionRate == 1))
  
  # Test with missing department data
  missing_dept <- data.frame(
    EmployeeID = 1:100,
    Department = c(rep("IT", 50), rep(NA, 50)),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE)
  )
  expect_warning(analyze_attrition_by_department(missing_dept))
})

# =============================================================================
# 5. DEMOGRAPHICS MODULE TESTS
# =============================================================================

test_that("Demographics Module - Age Distribution Analysis", {
  mock_data <- data.frame(
    EmployeeID = 1:500,
    Age = sample(18:65, 500, replace = TRUE),
    Gender = sample(c("Male", "Female", "Other"), 500, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 500, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 500, replace = TRUE)
  )
  
  # Test age distribution calculation
  age_dist <- calculate_age_distribution(mock_data)
  expect_true(is.data.frame(age_dist))
  expect_true("AgeGroup" %in% names(age_dist))
  expect_true("Count" %in% names(age_dist))
  expect_equal(sum(age_dist$Count), 500)
})

test_that("Demographics Module - Diversity Metrics", {
  mock_data <- data.frame(
    EmployeeID = 1:1000,
    Gender = sample(c("Male", "Female", "Other"), 1000, replace = TRUE, prob = c(0.5, 0.45, 0.05)),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 1000, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 1000, replace = TRUE)
  )
  
  # Test diversity calculations
  diversity_metrics <- calculate_diversity_metrics(mock_data)
  expect_true(is.list(diversity_metrics))
  expect_true("gender_diversity" %in% names(diversity_metrics))
  expect_true("ethnic_diversity" %in% names(diversity_metrics))
  expect_true("department_diversity" %in% names(diversity_metrics))
  
  # Test Simpson's diversity index calculations
  expect_true(diversity_metrics$gender_diversity >= 0 && diversity_metrics$gender_diversity <= 1)
  expect_true(diversity_metrics$ethnic_diversity >= 0 && diversity_metrics$ethnic_diversity <= 1)
})

test_that("Demographics Module - Geographic Analysis", {
  mock_data <- data.frame(
    EmployeeID = 1:500,
    State = sample(c("CA", "NY", "TX", "FL", "IL"), 500, replace = TRUE),
    DistanceFromHome = sample(1:100, 500, replace = TRUE)
  )
  
  # Test geographic distribution
  geo_analysis <- analyze_geographic_distribution(mock_data)
  expect_true(is.data.frame(geo_analysis))
  expect_true("State" %in% names(geo_analysis))
  expect_true("EmployeeCount" %in% names(geo_analysis))
  expect_true("AvgDistance" %in% names(geo_analysis))
})

# =============================================================================
# 6. PERFORMANCE MODULE TESTS
# =============================================================================

test_that("Performance Module - Rating Analysis", {
  mock_performance <- data.frame(
    EmployeeID = 1:500,
    ManagerRating = sample(1:5, 500, replace = TRUE),
    SelfRating = sample(1:5, 500, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 500, replace = TRUE),
    JobSatisfaction = sample(1:5, 500, replace = TRUE),
    WorkLifeBalance = sample(1:5, 500, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 500, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 500, replace = TRUE)
  )
  
  # Test performance metrics calculation
  perf_metrics <- calculate_performance_metrics(mock_performance)
  expect_true(is.list(perf_metrics))
  expect_true("avg_manager_rating" %in% names(perf_metrics))
  expect_true("avg_self_rating" %in% names(perf_metrics))
  expect_true("rating_alignment" %in% names(perf_metrics))
  
  # Test rating distribution
  rating_dist <- analyze_rating_distribution(mock_performance)
  expect_true(is.data.frame(rating_dist))
  expect_true(nrow(rating_dist) == 5) # 5 rating levels
})

test_that("Performance Module - Training Analysis", {
  mock_data <- data.frame(
    EmployeeID = 1:200,
    TrainingOpportunitiesWithinYear = sample(0:10, 200, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 200, replace = TRUE),
    ManagerRating = sample(1:5, 200, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance"), 200, replace = TRUE)
  )
  
  # Ensure training taken doesn't exceed opportunities
  mock_data$TrainingOpportunitiesTaken <- pmin(
    mock_data$TrainingOpportunitiesTaken,
    mock_data$TrainingOpportunitiesWithinYear
  )
  
  # Test training analysis
  training_analysis <- analyze_training_effectiveness(mock_data)
  expect_true(is.list(training_analysis))
  expect_true("training_completion_rate" %in% names(training_analysis))
  expect_true("training_performance_correlation" %in% names(training_analysis))
})

# =============================================================================
# 7. COMPENSATION MODULE TESTS
# =============================================================================

test_that("Compensation Module - Salary Analysis", {
  mock_data <- data.frame(
    EmployeeID = 1:1000,
    Salary = sample(30000:200000, 1000, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 1000, replace = TRUE),
    JobRole = sample(c("Analyst", "Manager", "Director", "VP"), 1000, replace = TRUE),
    Gender = sample(c("Male", "Female"), 1000, replace = TRUE),
    YearsAtCompany = sample(0:20, 1000, replace = TRUE),
    StockOptionLevel = sample(0:4, 1000, replace = TRUE)
  )
  
  # Test salary distribution analysis
  salary_dist <- analyze_salary_distribution(mock_data)
  expect_true(is.list(salary_dist))
  expect_true("by_department" %in% names(salary_dist))
  expect_true("by_role" %in% names(salary_dist))
  expect_true("by_gender" %in% names(salary_dist))
  
  # Test salary bands
  salary_bands <- create_salary_bands(mock_data)
  expect_true(is.data.frame(salary_bands))
  expect_true("SalaryBand" %in% names(salary_bands))
  expect_true("Count" %in% names(salary_bands))
})

test_that("Compensation Module - Pay Equity Analysis", {
  # Create data with potential pay gaps
  mock_data <- data.frame(
    EmployeeID = 1:500,
    Salary = c(
      sample(45000:65000, 200, replace = TRUE), # Group 1
      sample(50000:70000, 200, replace = TRUE), # Group 2
      sample(40000:60000, 100, replace = TRUE)  # Group 3
    ),
    Gender = c(
      rep("Female", 200),
      rep("Male", 200),
      rep("Other", 100)
    ),
    Department = sample(c("IT", "HR", "Finance"), 500, replace = TRUE),
    JobRole = sample(c("Analyst", "Manager"), 500, replace = TRUE),
    YearsAtCompany = sample(1:15, 500, replace = TRUE)
  )
  
  # Test pay equity analysis
  pay_equity <- analyze_pay_equity(mock_data)
  expect_true(is.list(pay_equity))
  expect_true("gender_pay_gap" %in% names(pay_equity))
  expect_true("department_equity" %in% names(pay_equity))
  
  # Test statistical significance
  expect_true(is.numeric(pay_equity$gender_pay_gap$gap_percentage))
  expect_true(is.logical(pay_equity$gender_pay_gap$statistically_significant))
})

# =============================================================================
# 8. SATISFACTION MODULE TESTS
# =============================================================================

test_that("Satisfaction Module - Satisfaction Metrics", {
  mock_data <- data.frame(
    EmployeeID = 1:500,
    JobSatisfaction = sample(1:5, 500, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 500, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 500, replace = TRUE),
    WorkLifeBalance = sample(1:5, 500, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 500, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 500, replace = TRUE)
  )
  
  # Test satisfaction calculations
  satisfaction_metrics <- calculate_satisfaction_metrics(mock_data)
  expect_true(is.list(satisfaction_metrics))
  expect_true("overall_satisfaction" %in% names(satisfaction_metrics))
  expect_true("satisfaction_by_department" %in% names(satisfaction_metrics))
  expect_true("satisfaction_drivers" %in% names(satisfaction_metrics))
  
  # Test satisfaction-attrition correlation
  satisfaction_impact <- analyze_satisfaction_impact(mock_data)
  expect_true(is.list(satisfaction_impact))
  expect_true("correlations" %in% names(satisfaction_impact))
})

test_that("Satisfaction Module - Correlation Analysis", {
  # Create correlated data
  set.seed(123)
  n <- 300
  job_sat <- sample(1:5, n, replace = TRUE)
  
  mock_data <- data.frame(
    EmployeeID = 1:n,
    JobSatisfaction = job_sat,
    EnvironmentSatisfaction = pmax(1, pmin(5, job_sat + sample(-1:1, n, replace = TRUE))),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    Attrition = ifelse(job_sat <= 2, 
                      sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.7, 0.3)),
                      sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.1, 0.9)))
  )
  
  # Test correlation analysis
  correlations <- calculate_satisfaction_correlations(mock_data)
  expect_true(is.matrix(correlations) || is.data.frame(correlations))
  expect_true(all(abs(correlations) <= 1, na.rm = TRUE))
})

# =============================================================================
# 9. REPORT MODULE TESTS
# =============================================================================

test_that("Report Module - Report Generation", {
  # Mock data for report
  mock_data <- list(
    kpi_metrics = list(
      total_employees = 1000,
      attrition_rate = 0.15,
      avg_satisfaction = 3.7,
      avg_salary = 75000
    ),
    selected_filters = list(
      department = c("IT", "HR"),
      year = 2023
    ),
    recommendations = list(
      list(
        title = "Improve Work-Life Balance",
        description = "Implement flexible working arrangements",
        priority = "High"
      )
    )
  )
  
  # Test report parameter generation
  report_params <- generate_report_parameters(mock_data)
  expect_true(is.list(report_params))
  expect_true("kpi_metrics" %in% names(report_params))
  expect_true("selected_filters" %in% names(report_params))
  
  # Test report validation
  validation_result <- validate_report_data(mock_data)
  expect_true(is.logical(validation_result))
  expect_true(validation_result)
})

test_that("Report Module - Edge Cases", {
  # Test with empty data
  empty_data <- list()
  expect_error(generate_report_parameters(empty_data))
  
  # Test with partial data
  partial_data <- list(kpi_metrics = list(total_employees = 100))
  params <- generate_report_parameters(partial_data)
  expect_true(is.list(params))
  expect_true("kpi_metrics" %in% names(params))
})

# =============================================================================
# 10. UTILS AND HELPER FUNCTION TESTS
# =============================================================================

test_that("Utility Functions - Data Validation", {
  # Test is_valid_employee_id
  expect_true(is_valid_employee_id(12345))
  expect_true(is_valid_employee_id("EMP001"))
  expect_false(is_valid_employee_id(""))
  expect_false(is_valid_employee_id(NULL))
  expect_false(is_valid_employee_id(-1))
  
  # Test is_valid_salary
  expect_true(is_valid_salary(50000))
  expect_true(is_valid_salary(25000))
  expect_false(is_valid_salary(-1000))
  expect_false(is_valid_salary(0))
  expect_false(is_valid_salary(1000000)) # Unrealistic high salary
  
  # Test is_valid_rating
  expect_true(is_valid_rating(3))
  expect_true(is_valid_rating(1))
  expect_true(is_valid_rating(5))
  expect_false(is_valid_rating(0))
  expect_false(is_valid_rating(6))
  expect_false(is_valid_rating(-1))
})

test_that("Utility Functions - Data Transformation", {
  # Test age_to_generation
  expect_equal(age_to_generation(25), "Millennial")
  expect_equal(age_to_generation(45), "Gen X")
  expect_equal(age_to_generation(65), "Baby Boomer")
  expect_equal(age_to_generation(22), "Gen Z")
  
  # Test salary_to_band
  expect_equal(salary_to_band(35000), "Entry Level")
  expect_equal(salary_to_band(75000), "Mid Level")
  expect_equal(salary_to_band(150000), "Senior Level")
  expect_equal(salary_to_band(250000), "Executive Level")
  
  # Test calculate_tenure_category
  expect_equal(calculate_tenure_category(0.5), "New Hire")
  expect_equal(calculate_tenure_category(3), "Experienced")
  expect_equal(calculate_tenure_category(8), "Veteran")
  expect_equal(calculate_tenure_category(20), "Long-term")
})

test_that("Utility Functions - Statistical Calculations", {
  # Test calculate_confidence_interval
  test_data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  ci <- calculate_confidence_interval(test_data, confidence_level = 0.95)
  expect_true(is.list(ci))
  expect_true("lower" %in% names(ci))
  expect_true("upper" %in% names(ci))
  expect_true("mean" %in% names(ci))
  expect_true(ci$lower < ci$upper)
  
  # Test calculate_effect_size
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(6, 7, 8, 9, 10)
  effect_size <- calculate_effect_size(group1, group2)
  expect_true(is.numeric(effect_size))
  expect_true(abs(effect_size) > 0)
})

# =============================================================================
# 11. INTEGRATION TESTS
# =============================================================================

test_that("Integration - Data Flow Between Modules", {
  # Create comprehensive mock data
  employee_data <- data.frame(
    EmployeeID = 1:100,
    FirstName = paste("Employee", 1: