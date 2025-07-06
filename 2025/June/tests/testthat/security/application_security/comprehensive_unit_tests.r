# ============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - COMPREHENSIVE UNIT TESTS
# ============================================================================
# Author: akhapwoyaco
# Purpose: Extensive unit testing covering all non-security aspects
# Coverage: Data validation, modules, performance, UI/UX, edge cases
# ============================================================================

library(testthat)
library(shiny)
library(shinytest2)
library(mockery)
library(tibble)
library(dplyr)
library(lubridate)
library(R6)
library(plotly)
library(DT)

# Source application files (assuming they exist)
source("global.R", local = TRUE)
source("utils.R", local = TRUE)
source("custom_theme.R", local = TRUE)

# Load all modules
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
purrr::walk(module_files, source, local = TRUE)

# ============================================================================
# 1. DATA VALIDATION AND INTEGRITY TESTS
# ============================================================================

test_that("Data Loading Module - File Validation", {
  
  # Test 1.1: Valid CSV file loading
  expect_no_error({
    mock_employee_data <- tibble(
      EmployeeID = 1:100,
      FirstName = paste("Employee", 1:100),
      LastName = paste("Lastname", 1:100),
      Gender = sample(c("Male", "Female", "Other"), 100, replace = TRUE),
      Age = sample(22:65, 100, replace = TRUE),
      Department = sample(c("Sales", "HR", "IT", "Marketing"), 100, replace = TRUE),
      Salary = sample(30000:150000, 100, replace = TRUE),
      Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8))
    )
    validate_employee_data(mock_employee_data)
  })
  
  # Test 1.2: Missing required columns
  expect_error({
    invalid_data <- tibble(
      EmployeeID = 1:10,
      FirstName = paste("Employee", 1:10)
      # Missing required columns
    )
    validate_employee_data(invalid_data)
  }, "Missing required columns")
  
  # Test 1.3: Invalid data types
  expect_error({
    invalid_data <- tibble(
      EmployeeID = paste("EMP", 1:10),  # Should be numeric
      FirstName = 1:10,  # Should be character
      Age = paste("Age", 1:10),  # Should be numeric
      Salary = paste("$", 1:10)  # Should be numeric
    )
    validate_employee_data(invalid_data)
  }, "Invalid data types")
  
  # Test 1.4: Empty dataset
  expect_error({
    empty_data <- tibble()
    validate_employee_data(empty_data)
  }, "Dataset is empty")
  
  # Test 1.5: Duplicate EmployeeIDs
  expect_error({
    duplicate_data <- tibble(
      EmployeeID = c(1, 1, 2, 2),
      FirstName = c("John", "Jane", "Bob", "Alice"),
      Age = c(25, 30, 35, 28),
      Salary = c(50000, 60000, 70000, 55000)
    )
    validate_employee_data(duplicate_data)
  }, "Duplicate EmployeeIDs found")
  
  # Test 1.6: Out of range values
  expect_error({
    invalid_range_data <- tibble(
      EmployeeID = 1:5,
      Age = c(15, 25, 30, 150, 40),  # Age 15 and 150 are invalid
      Salary = c(-5000, 50000, 60000, 70000, 1000000000)  # Negative salary
    )
    validate_employee_data(invalid_range_data)
  }, "Values out of valid range")
  
  # Test 1.7: Special characters in names
  expect_no_error({
    special_char_data <- tibble(
      EmployeeID = 1:3,
      FirstName = c("José", "François", "Müller"),
      LastName = c("García", "Dupont", "Schmidt"),
      Age = c(25, 30, 35),
      Salary = c(50000, 60000, 70000)
    )
    validate_employee_data(special_char_data)
  })
  
  # Test 1.8: Date validation
  expect_no_error({
    date_data <- tibble(
      EmployeeID = 1:3,
      HireDate = as.Date(c("2020-01-01", "2021-06-15", "2022-12-31")),
      ReviewDate = as.Date(c("2021-01-01", "2022-06-15", "2023-12-31"))
    )
    validate_date_columns(date_data)
  })
  
  # Test 1.9: Future hire dates (should fail)
  expect_error({
    future_date_data <- tibble(
      EmployeeID = 1:2,
      HireDate = as.Date(c("2020-01-01", "2030-01-01"))  # Future date
    )
    validate_date_columns(future_date_data)
  }, "Future hire dates not allowed")
  
  # Test 1.10: Logical data consistency
  expect_error({
    inconsistent_data <- tibble(
      EmployeeID = 1:3,
      HireDate = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
      YearsAtCompany = c(5, 2, 1),  # Inconsistent with hire date
      YearsInMostRecentRole = c(10, 3, 2)  # Cannot be more than years at company
    )
    validate_logical_consistency(inconsistent_data)
  }, "Logical inconsistencies found")
})

test_that("Data Merging and Relationships", {
  
  # Test 2.1: Successful data merging
  employee_data <- tibble(
    EmployeeID = 1:5,
    FirstName = c("John", "Jane", "Bob", "Alice", "Charlie"),
    Department = c("Sales", "HR", "IT", "Marketing", "Sales")
  )
  
  performance_data <- tibble(
    EmployeeID = 1:5,
    JobSatisfaction = c(4, 5, 3, 4, 2),
    PerformanceRating = c(3, 4, 5, 4, 3)
  )
  
  merged_data <- merge_employee_performance(employee_data, performance_data)
  expect_equal(nrow(merged_data), 5)
  expect_true(all(c("FirstName", "JobSatisfaction") %in% names(merged_data)))
  
  # Test 2.2: Orphan records in performance data
  performance_orphan <- tibble(
    EmployeeID = c(1, 2, 6, 7),  # 6 and 7 don't exist in employee data
    JobSatisfaction = c(4, 5, 3, 4)
  )
  
  expect_warning({
    merged_orphan <- merge_employee_performance(employee_data, performance_orphan)
  }, "Orphan performance records found")
  
  # Test 2.3: Missing performance data for employees
  performance_missing <- tibble(
    EmployeeID = c(1, 2),  # Missing data for employees 3, 4, 5
    JobSatisfaction = c(4, 5)
  )
  
  expect_warning({
    merged_missing <- merge_employee_performance(employee_data, performance_missing)
  }, "Employees missing performance data")
  
  # Test 2.4: Education level lookup
  education_levels <- tibble(
    EducationID = 1:5,
    EducationLevel = c("High School", "Bachelor", "Master", "PhD", "Associate")
  )
  
  employee_with_edu <- tibble(
    EmployeeID = 1:3,
    Education = c(2, 3, 4)  # References to education levels
  )
  
  result <- add_education_levels(employee_with_edu, education_levels)
  expect_true("EducationLevel" %in% names(result))
  expect_equal(result$EducationLevel, c("Bachelor", "Master", "PhD"))
  
  # Test 2.5: Invalid education references
  expect_error({
    invalid_edu <- tibble(
      EmployeeID = 1:2,
      Education = c(2, 99)  # 99 doesn't exist in education levels
    )
    add_education_levels(invalid_edu, education_levels)
  }, "Invalid education level references")
})

# ============================================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# ============================================================================

test_that("AtlasLogger R6 Class Functionality", {
  
  # Test 3.1: Logger initialization
  logger <- AtlasLogger$new()
  expect_true(is.R6(logger))
  expect_true(inherits(logger, "AtlasLogger"))
  
  # Test 3.2: Basic logging functions
  expect_no_error({
    logger$log_info("Test info message", "test_module")
    logger$log_warning("Test warning", "test_module")
    logger$log_error("Test error", "test_module")
  })
  
  # Test 3.3: Performance tracking
  expect_no_error({
    logger$start_timer("test_operation")
    Sys.sleep(0.1)  # Simulate work
    logger$stop_timer("test_operation")
  })
  
  performance_data <- logger$get_performance_data()
  expect_true("test_operation" %in% names(performance_data))
  expect_true(performance_data$test_operation > 0.05)  # At least 50ms
  
  # Test 3.4: Memory tracking
  initial_memory <- logger$get_memory_usage()
  
  # Create large object to increase memory
  large_object <- rep(1:1000, 1000)
  
  current_memory <- logger$get_memory_usage()
  expect_true(current_memory > initial_memory)
  
  # Test 3.5: Log level filtering
  logger$set_log_level("ERROR")
  
  # Only errors should be logged now
  logger$log_info("This should not appear", "test_module")
  logger$log_error("This should appear", "test_module")
  
  logs <- logger$get_logs()
  recent_logs <- tail(logs, 2)
  expect_true(any(grepl("This should appear", recent_logs)))
  expect_false(any(grepl("This should not appear", recent_logs)))
  
  # Test 3.6: Module-specific logging
  logger$log_info("Module A message", "module_a")
  logger$log_info("Module B message", "module_b")
  
  module_a_logs <- logger$get_module_logs("module_a")
  expect_true(any(grepl("Module A message", module_a_logs)))
  expect_false(any(grepl("Module B message", module_a_logs)))
  
  # Test 3.7: Log rotation
  logger$set_max_log_entries(5)
  
  # Add more than 5 entries
  for (i in 1:10) {
    logger$log_info(paste("Entry", i), "test_module")
  }
  
  all_logs <- logger$get_logs()
  expect_true(length(all_logs) <= 5)
  
  # Test 3.8: Performance alerts
  logger$set_performance_threshold(0.5)  # 500ms threshold
  
  logger$start_timer("slow_operation")
  Sys.sleep(0.6)  # Simulate slow operation
  logger$stop_timer("slow_operation")
  
  alerts <- logger$get_performance_alerts()
  expect_true(any(grepl("slow_operation", alerts)))
  
  # Test 3.9: Memory leak detection
  logger$start_memory_monitoring()
  
  # Simulate memory leak
  memory_hog <- list()
  for (i in 1:100) {
    memory_hog[[i]] <- rep(1:1000, 100)
  }
  
  logger$check_memory_leaks()
  memory_alerts <- logger$get_memory_alerts()
  expect_true(length(memory_alerts) > 0)
  
  # Test 3.10: Logger serialization
  logger_state <- logger$serialize()
  expect_true(is.list(logger_state))
  expect_true("logs" %in% names(logger_state))
  expect_true("performance_data" %in% names(logger_state))
  
  # Test logger restoration
  new_logger <- AtlasLogger$new()
  new_logger$deserialize(logger_state)
  
  restored_logs <- new_logger$get_logs()
  expect_true(length(restored_logs) > 0)
})

# ============================================================================
# 3. MODULE FUNCTIONALITY TESTS
# ============================================================================

test_that("Overview Module Functionality", {
  
  # Test 4.1: KPI calculation
  sample_data <- tibble(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.15, 0.85)),
    Salary = sample(40000:120000, 100, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT", "Marketing"), 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE)
  )
  
  kpis <- calculate_kpis(sample_data)
  
  expect_true(is.list(kpis))
  expect_true("total_employees" %in% names(kpis))
  expect_true("attrition_rate" %in% names(kpis))
  expect_true("avg_salary" %in% names(kpis))
  expect_true("avg_age" %in% names(kpis))
  
  expect_equal(kpis$total_employees, 100)
  expect_true(kpis$attrition_rate >= 0 && kpis$attrition_rate <= 1)
  expect_true(kpis$avg_salary > 0)
  
  # Test 4.2: Department distribution
  dept_dist <- calculate_department_distribution(sample_data)
  expect_true(is.data.frame(dept_dist))
  expect_true(all(c("Department", "Count", "Percentage") %in% names(dept_dist)))
  expect_equal(sum(dept_dist$Count), 100)
  expect_equal(sum(dept_dist$Percentage), 100)
  
  # Test 4.3: Empty data handling
  empty_data <- tibble()
  expect_error({
    calculate_kpis(empty_data)
  }, "Cannot calculate KPIs for empty dataset")
  
  # Test 4.4: Single employee edge case
  single_employee <- tibble(
    EmployeeID = 1,
    Attrition = "No",
    Salary = 50000,
    Department = "Sales",
    JobSatisfaction = 4,
    Age = 30
  )
  
  single_kpis <- calculate_kpis(single_employee)
  expect_equal(single_kpis$total_employees, 1)
  expect_equal(single_kpis$attrition_rate, 0)
  expect_equal(single_kpis$avg_salary, 50000)
  
  # Test 4.5: All employees left (100% attrition)
  all_left <- tibble(
    EmployeeID = 1:5,
    Attrition = rep("Yes", 5),
    Salary = rep(50000, 5),
    Department = rep("Sales", 5),
    JobSatisfaction = rep(3, 5),
    Age = rep(30, 5)
  )
  
  all_left_kpis <- calculate_kpis(all_left)
  expect_equal(all_left_kpis$attrition_rate, 1.0)
})

test_that("Attrition Analysis Module", {
  
  # Test 5.1: Attrition by department
  sample_data <- tibble(
    EmployeeID = 1:200,
    Department = sample(c("Sales", "HR", "IT", "Marketing"), 200, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 200, replace = TRUE, prob = c(0.2, 0.8)),
    YearsAtCompany = sample(1:20, 200, replace = TRUE),
    Salary = sample(40000:120000, 200, replace = TRUE),
    JobSatisfaction = sample(1:5, 200, replace = TRUE)
  )
  
  attrition_analysis <- analyze_attrition_by_department(sample_data)
  
  expect_true(is.data.frame(attrition_analysis))
  expect_true(all(c("Department", "TotalEmployees", "Attrition", "AttritionRate") %in% names(attrition_analysis)))
  expect_true(all(attrition_analysis$AttritionRate >= 0 & attrition_analysis$AttritionRate <= 1))
  
  # Test 5.2: Attrition by tenure
  tenure_analysis <- analyze_attrition_by_tenure(sample_data)
  expect_true(is.data.frame(tenure_analysis))
  expect_true("TenureGroup" %in% names(tenure_analysis))
  
  # Test 5.3: Attrition risk scoring
  risk_scores <- calculate_attrition_risk_scores(sample_data)
  expect_true(is.data.frame(risk_scores))
  expect_true("RiskScore" %in% names(risk_scores))
  expect_true(all(risk_scores$RiskScore >= 0 & risk_scores$RiskScore <= 1))
  
  # Test 5.4: Survival analysis
  survival_data <- perform_survival_analysis(sample_data)
  expect_true(is.list(survival_data))
  expect_true("survival_curves" %in% names(survival_data))
  
  # Test 5.5: Attrition prediction model
  prediction_model <- build_attrition_prediction_model(sample_data)
  expect_true(inherits(prediction_model, "glm"))
  
  # Test predictions
  predictions <- predict_attrition_probability(prediction_model, sample_data[1:10, ])
  expect_true(is.numeric(predictions))
  expect_true(all(predictions >= 0 & predictions <= 1))
  
  # Test 5.6: Department with no attrition
  no_attrition_data <- tibble(
    EmployeeID = 1:50,
    Department = rep("StableTeam", 50),
    Attrition = rep("No", 50),
    YearsAtCompany = sample(1:20, 50, replace = TRUE)
  )
  
  stable_analysis <- analyze_attrition_by_department(no_attrition_data)
  expect_equal(stable_analysis$AttritionRate, 0)
  
  # Test 5.7: Department with 100% attrition
  full_attrition_data <- tibble(
    EmployeeID = 1:10,
    Department = rep("FailedTeam", 10),
    Attrition = rep("Yes", 10),
    YearsAtCompany = sample(1:5, 10, replace = TRUE)
  )
  
  failed_analysis <- analyze_attrition_by_department(full_attrition_data)
  expect_equal(failed_analysis$AttritionRate, 1.0)
})

test_that("Performance Analysis Module", {
  
  # Test 6.1: Performance metrics calculation
  performance_data <- tibble(
    EmployeeID = 1:100,
    SelfRating = sample(1:5, 100, replace = TRUE),
    ManagerRating = sample(1:5, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    TrainingOpportunitiesOffered = sample(0:10, 100, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 100, replace = TRUE)
  )
  
  # Ensure training taken <= training offered
  performance_data$TrainingOpportunitiesTaken <- pmin(
    performance_data$TrainingOpportunitiesTaken,
    performance_data$TrainingOpportunitiesOffered
  )
  
  perf_metrics <- calculate_performance_metrics(performance_data)
  
  expect_true(is.list(perf_metrics))
  expect_true("avg_self_rating" %in% names(perf_metrics))
  expect_true("avg_manager_rating" %in% names(perf_metrics))
  expect_true("training_utilization_rate" %in% names(perf_metrics))
  
  # Test 6.2: Performance vs satisfaction correlation
  correlation_analysis <- analyze_performance_satisfaction_correlation(performance_data)
  expect_true(is.numeric(correlation_analysis))
  expect_true(correlation_analysis >= -1 && correlation_analysis <= 1)
  
  # Test 6.3: Training effectiveness analysis
  training_effectiveness <- analyze_training_effectiveness(performance_data)
  expect_true(is.data.frame(training_effectiveness))
  
  # Test 6.4: Performance distribution analysis
  perf_distribution <- analyze_performance_distribution(performance_data)
  expect_true(is.data.frame(perf_distribution))
  expect_true(all(c("Rating", "SelfRating_Count", "ManagerRating_Count") %in% names(perf_distribution)))
  
  # Test 6.5: Rating discrepancy analysis
  rating_discrepancy <- analyze_rating_discrepancy(performance_data)
  expect_true(is.data.frame(rating_discrepancy))
  expect_true("RatingDifference" %in% names(rating_discrepancy))
  
  # Test 6.6: Edge case - No training offered
  no_training_data <- tibble(
    EmployeeID = 1:20,
    TrainingOpportunitiesOffered = rep(0, 20),
    TrainingOpportunitiesTaken = rep(0, 20)
  )
  
  no_training_metrics <- calculate_performance_metrics(no_training_data)
  expect_equal(no_training_metrics$training_utilization_rate, 0)
  
  # Test 6.7: Perfect training utilization
  perfect_training_data <- tibble(
    EmployeeID = 1:20,
    TrainingOpportunitiesOffered = rep(5, 20),
    TrainingOpportunitiesTaken = rep(5, 20)
  )
  
  perfect_training_metrics <- calculate_performance_metrics(perfect_training_data)
  expect_equal(perfect_training_metrics$training_utilization_rate, 1.0)
})

# ============================================================================
# 4. USER INTERFACE TESTS
# ============================================================================

test_that("UI Component Generation", {
  
  # Test 7.1: Sidebar module UI
  sidebar_ui <- sidebarUI("test_sidebar")
  expect_true(inherits(sidebar_ui, "shiny.tag"))
  expect_true(any(grepl("sidebar", as.character(sidebar_ui))))
  
  # Test 7.2: Overview module UI
  overview_ui <- overviewUI("test_overview")
  expect_true(inherits(overview_ui, "shiny.tag"))
  
  # Test 7.3: Data loader UI
  data_loader_ui <- dataLoaderUI("test_loader")
  expect_true(inherits(data_loader_ui, "shiny.tag"))
  
  # Test 7.4: Report module UI
  report_ui <- reportUI("test_report")
  expect_true(inherits(report_ui, "shiny.tag"))
  
  # Test 7.5: Footer module UI
  footer_ui <- footerUI("test_footer")
  expect_true(inherits(footer_ui, "shiny.tag"))
  
  # Test 7.6: Module ID consistency
  expect_true(grepl("test_sidebar", as.character(sidebar_ui)))
  expect_true(grepl("test_overview", as.character(overview_ui)))
  expect_true(grepl("test_loader", as.character(data_loader_ui)))
})

test_that("Reactive Value Management", {
  
  # Test 8.1: Shared reactive values initialization
  shared_values <- reactiveValues(
    employee_data = NULL,
    performance_data = NULL,
    filtered_data = NULL,
    selected_filters = list(),
    current_analysis = NULL
  )
  
  expect_true(is.reactivevalues(shared_values))
  expect_null(shared_values$employee_data)
  expect_true(is.list(shared_values$selected_filters))
  
  # Test 8.2: Data assignment
  test_data <- tibble(
    EmployeeID = 1:10,
    Name = paste("Employee", 1:10)
  )
  
  shared_values$employee_data <- test_data
  expect_equal(nrow(shared_values$employee_data), 10)
  
  # Test 8.3: Filter management
  shared_values$selected_filters <- list(
    Department = c("Sales", "HR"),
    Age = c(25, 45)
  )
  
  expect_equal(length(shared_values$selected_filters), 2)
  expect_true("Department" %in% names(shared_values$selected_filters))
})

# ============================================================================
# 5. VISUALIZATION TESTS
# ============================================================================

test_that("ggplot2 Visualization Generation", {
  
  # Test 9.1: Basic plot creation
  sample_data <- tibble(
    Department = sample(c("Sales", "HR", "IT", "Marketing"), 100, replace = TRUE),
    Salary = sample(40000:120000, 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE)
  )
  
  # Test salary distribution plot
  salary_plot <- create_salary_distribution_plot(sample_data)
  expect_true(inherits(salary_plot, "ggplot"))
  
  # Test attrition by department plot
  attrition_plot <- create_attrition_by_department_plot(sample_data)
  expect_true(inherits(attrition_plot, "ggplot"))
  
  # Test age distribution plot
  age_plot <- create_age_distribution_plot(sample_data)
  expect_true(inherits(age_plot, "ggplot"))
  
  # Test 9.2: Plotly conversion
  interactive_salary_plot <- plotly::ggplotly(salary_plot)
  expect_true(inherits(interactive_salary_plot, "plotly"))
  
  # Test 9.3: Custom theme application
  themed_plot <- salary_plot + atlas_theme()
  expect_true(inherits(themed_plot, "ggplot"))
  
  # Test 9.4: Empty data handling
  empty_data <- tibble()
  expect_error({
    create_salary_distribution_plot(empty_data)
  }, "Cannot create plot with empty data")
  
  # Test 9.5: Single data point
  single_point_data <- tibble(
    Department = "Sales",
    Salary = 50000,
    Age = 30,
    Attrition = "No"
  )
  
  single_point_plot <- create_salary_distribution_plot(single_point_data)
  expect_true(inherits(single_point_plot, "ggplot"))
  
  # Test 9.6: Missing values handling
  missing_data <- tibble(
    Department = c("Sales", "HR", NA, "IT"),
    Salary = c(50000, 60000, 70000, NA),
    Age = c(25, 30, 35, 40),
    Attrition = c("Yes", "No", "Yes", "No")
  )
  
  expect_warning({
    missing_plot <- create_salary_distribution_plot(missing_data)
  }, "Missing values detected")
  
  # Test 9.7: Extreme values
  extreme_data <- tibble(
    Department = c("Sales", "HR", "IT"),
    Salary = c(1000000, 20000, 50000),  # One extreme high value
    Age = c(25, 30, 35),
    Attrition = c("Yes", "No", "Yes")
  )
  
  extreme_plot <- create_salary_distribution_plot(extreme_data)
  expect_true(inherits(extreme_plot, "ggplot"))
})

test_that("Data Table Generation", {
  
  # Test 10.1: Basic data table
  sample_data <- tibble(
    EmployeeID = 1:50,
    Name = paste("Employee", 1:50),
    Department = sample(c("Sales", "HR", "IT"), 50, replace = TRUE),
    Salary = sample(40000:120000, 50, replace = TRUE)
  )
  
  dt_table <- create_employee_data_table(sample_data)
  expect_true(inherits(dt_table, "datatables"))
  
  # Test 10.2: Filtered data table
  filtered_data <- sample_data %>% filter(Department == "Sales")
  filtered_dt <- create_employee_data_table(filtered_data)
  expect_true(inherits(filtered_dt, "datatables"))
  
  # Test 10.3: Empty data table
  empty_data <- tibble()
  empty_dt <- create_employee_data_table(empty_data)
  expect_true(inherits(empty_dt, "datatables"))
  
  # Test 10.4: Large data table (performance)
  large_data <- tibble(
    EmployeeID = 1:10000,
    Name = paste("Employee", 1:10000),
    Department = sample(c("Sales", "HR", "IT", "Marketing"), 10000, replace = TRUE),
    Salary = sample(40000:120000, 10000, replace = TRUE)
  )
  
  expect_no_error({
    large_dt <- create_employee_data_table(large_data)
  })
  
  # Test 10.5: Data table with special characters
  special_data <- tibble(
    EmployeeID = 1:3,
    Name = c("José García", "François Müller", "李伟"),
    Department = c("Sales & Marketing", "R&D", "HR"),
    Salary = c(50000, 60000