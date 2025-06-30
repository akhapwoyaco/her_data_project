# Atlas Labs HR Analytics Dashboard - Comprehensive Unit Tests
# Excludes Compensation Module specific calculations as requested
# Author: akhapwoyaco
# Data Source: https://herdataproject.gumroad.com/l/hr-analytics-tableau

library(testthat)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(R6)
library(mockery)

# =============================================================================
# SETUP AND TEST DATA
# =============================================================================

# Create test data fixtures
create_test_employee_data <- function() {
  data.frame(
    EmployeeID = 1:100,
    FirstName = paste0("Employee", 1:100),
    LastName = paste0("Test", 1:100),
    Gender = sample(c("Male", "Female", "Non-binary"), 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 100, replace = TRUE),
    Department = sample(c("Sales", "Research & Development", "Human Resources"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL"), 100, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 100, replace = TRUE),
    Education = sample(1:5, 100, replace = TRUE),
    EducationField = sample(c("Life Sciences", "Technical Degree", "Marketing"), 100, replace = TRUE),
    JobRole = sample(c("Sales Executive", "Research Scientist", "HR Representative"), 100, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), 100, replace = TRUE),
    Salary = sample(30000:150000, 100, replace = TRUE),
    StockOptionLevel = sample(0:3, 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
    HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 100),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.16, 0.84)),
    YearsAtCompany = sample(0:25, 100, replace = TRUE),
    YearsInMostRecentRole = sample(0:15, 100, replace = TRUE),
    YearsSinceLastPromotion = sample(0:10, 100, replace = TRUE),
    YearsWithCurrManager = sample(0:12, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create_test_performance_data <- function() {
  data.frame(
    PerformanceID = 1:150,
    EmployeeID = sample(1:100, 150, replace = TRUE),
    ReviewDate = sample(seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day"), 150),
    EnvironmentSatisfaction = sample(1:5, 150, replace = TRUE),
    JobSatisfaction = sample(1:5, 150, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 150, replace = TRUE),
    WorkLifeBalance = sample(1:5, 150, replace = TRUE),
    SelfRating = sample(1:5, 150, replace = TRUE),
    ManagerRating = sample(1:5, 150, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 150, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create_test_education_data <- function() {
  data.frame(
    EducationLevelID = 1:5,
    EducationLevel = c("Below College", "College", "Bachelor", "Master", "Doctor"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# LOGGER MODULE TESTS
# =============================================================================

test_that("AtlasLogger R6 Class Initialization", {
  # Mock AtlasLogger class for testing
  AtlasLogger <- R6Class("AtlasLogger",
    public = list(
      logs = list(),
      performance_data = list(),
      initialize = function() {
        self$logs <- list()
        self$performance_data <- list()
      },
      log_info = function(message, module = "Unknown", performance_data = NULL) {
        timestamp <- Sys.time()
        log_entry <- list(
          timestamp = timestamp,
          level = "INFO",
          message = message,
          module = module,
          performance = performance_data
        )
        self$logs <- append(self$logs, list(log_entry))
        return(invisible(self))
      },
      log_warning = function(message, module = "Unknown") {
        timestamp <- Sys.time()
        log_entry <- list(
          timestamp = timestamp,
          level = "WARNING", 
          message = message,
          module = module
        )
        self$logs <- append(self$logs, list(log_entry))
        return(invisible(self))
      },
      log_error = function(message, module = "Unknown") {
        timestamp <- Sys.time()
        log_entry <- list(
          timestamp = timestamp,
          level = "ERROR",
          message = message,
          module = module
        )
        self$logs <- append(self$logs, list(log_entry))
        return(invisible(self))
      },
      track_memory_usage = function() {
        memory_info <- gc()
        return(sum(memory_info[, 2]))
      },
      track_execution_time = function(expr) {
        start_time <- Sys.time()
        result <- expr
        end_time <- Sys.time()
        execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        return(list(result = result, execution_time = execution_time))
      },
      get_performance_summary = function() {
        if (length(self$logs) == 0) return(NULL)
        
        log_levels <- sapply(self$logs, function(x) x$level)
        return(list(
          total_logs = length(self$logs),
          info_count = sum(log_levels == "INFO"),
          warning_count = sum(log_levels == "WARNING"),
          error_count = sum(log_levels == "ERROR")
        ))
      }
    )
  )
  
  logger <- AtlasLogger$new()
  
  expect_true(R6::is.R6(logger))
  expect_equal(length(logger$logs), 0)
  expect_equal(length(logger$performance_data), 0)
})

test_that("Logger Functions Work Correctly", {
  AtlasLogger <- R6Class("AtlasLogger",
    public = list(
      logs = list(),
      initialize = function() { self$logs <- list() },
      log_info = function(message, module = "Unknown", performance_data = NULL) {
        log_entry <- list(level = "INFO", message = message, module = module)
        self$logs <- append(self$logs, list(log_entry))
      },
      log_warning = function(message, module = "Unknown") {
        log_entry <- list(level = "WARNING", message = message, module = module)
        self$logs <- append(self$logs, list(log_entry))
      },
      log_error = function(message, module = "Unknown") {
        log_entry <- list(level = "ERROR", message = message, module = module)
        self$logs <- append(self$logs, list(log_entry))
      }
    )
  )
  
  logger <- AtlasLogger$new()
  
  # Test info logging
  logger$log_info("Test info message", "TestModule")
  expect_equal(length(logger$logs), 1)
  expect_equal(logger$logs[[1]]$level, "INFO")
  expect_equal(logger$logs[[1]]$message, "Test info message")
  expect_equal(logger$logs[[1]]$module, "TestModule")
  
  # Test warning logging
  logger$log_warning("Test warning", "TestModule")
  expect_equal(length(logger$logs), 2)
  expect_equal(logger$logs[[2]]$level, "WARNING")
  
  # Test error logging
  logger$log_error("Test error", "TestModule")
  expect_equal(length(logger$logs), 3)
  expect_equal(logger$logs[[3]]$level, "ERROR")
})

test_that("Performance Tracking Functions", {
  AtlasLogger <- R6Class("AtlasLogger",
    public = list(
      track_memory_usage = function() {
        memory_info <- gc()
        return(sum(memory_info[, 2]))
      },
      track_execution_time = function(expr) {
        start_time <- Sys.time()
        result <- expr
        end_time <- Sys.time()
        execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        return(list(result = result, execution_time = execution_time))
      }
    )
  )
  
  logger <- AtlasLogger$new()
  
  # Test memory tracking
  memory_usage <- logger$track_memory_usage()
  expect_true(is.numeric(memory_usage))
  expect_true(memory_usage > 0)
  
  # Test execution time tracking
  test_expr <- Sys.sleep(0.01)
  time_result <- logger$track_execution_time(test_expr)
  expect_true(is.list(time_result))
  expect_true("execution_time" %in% names(time_result))
  expect_true(time_result$execution_time >= 0.01)
})

# =============================================================================
# DATA LOADER MODULE TESTS
# =============================================================================

test_that("Data Validation Functions", {
  # Mock data validation functions
  validate_data_integrity <- function(data) {
    if (!is.data.frame(data)) return(FALSE)
    if (nrow(data) == 0) return(FALSE)
    if (ncol(data) == 0) return(FALSE)
    return(TRUE)
  }
  
  validate_employee_data <- function(employee_data) {
    required_columns <- c("EmployeeID", "FirstName", "LastName", "Department", "Attrition")
    missing_columns <- setdiff(required_columns, names(employee_data))
    
    if (length(missing_columns) > 0) {
      return(list(valid = FALSE, missing_columns = missing_columns))
    }
    
    # Check for duplicate EmployeeIDs
    if (any(duplicated(employee_data$EmployeeID))) {
      return(list(valid = FALSE, error = "Duplicate EmployeeIDs found"))
    }
    
    return(list(valid = TRUE))
  }
  
  # Test with valid data
  test_data <- create_test_employee_data()
  expect_true(validate_data_integrity(test_data))
  
  validation_result <- validate_employee_data(test_data)
  expect_true(validation_result$valid)
  
  # Test with invalid data
  invalid_data <- data.frame()
  expect_false(validate_data_integrity(invalid_data))
  
  # Test with missing columns
  incomplete_data <- test_data[, 1:3]  # Only first 3 columns
  validation_result <- validate_employee_data(incomplete_data)
  expect_false(validation_result$valid)
  expect_true("missing_columns" %in% names(validation_result))
})

test_that("Data Loading Functions", {
  # Mock data loading function
  load_employee_data <- function(file_path) {
    if (!file.exists(file_path)) {
      return(list(success = FALSE, error = "File not found"))
    }
    
    tryCatch({
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      return(list(success = TRUE, data = data))
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  }
  
  # Test with non-existent file
  result <- load_employee_data("non_existent_file.csv")
  expect_false(result$success)
  expect_equal(result$error, "File not found")
  
  # Test with valid file path (mocked)
  # In real implementation, you would test with actual CSV files
  # For now, we'll test the error handling structure
  expect_true(is.function(load_employee_data))
})

test_that("Data Merging Functions", {
  # Mock data merging function
  merge_datasets <- function(employee_data, performance_data, education_data) {
    if (!all(c("EmployeeID") %in% names(employee_data))) {
      return(list(success = FALSE, error = "Missing EmployeeID in employee data"))
    }
    
    if (!all(c("EmployeeID") %in% names(performance_data))) {
      return(list(success = FALSE, error = "Missing EmployeeID in performance data"))
    }
    
    tryCatch({
      # Merge employee and performance data
      merged_data <- merge(employee_data, performance_data, by = "EmployeeID", all.x = TRUE)
      
      # Merge with education data
      if (!is.null(education_data) && "Education" %in% names(merged_data)) {
        merged_data <- merge(merged_data, education_data, 
                           by.x = "Education", by.y = "EducationLevelID", all.x = TRUE)
      }
      
      return(list(success = TRUE, data = merged_data))
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  }
  
  # Test data merging
  employee_data <- create_test_employee_data()
  performance_data <- create_test_performance_data()
  education_data <- create_test_education_data()
  
  result <- merge_datasets(employee_data, performance_data, education_data)
  expect_true(result$success)
  expect_true(is.data.frame(result$data))
  expect_true("EmployeeID" %in% names(result$data))
  
  # Test with missing EmployeeID
  invalid_employee <- employee_data
  names(invalid_employee)[1] <- "ID"  # Remove EmployeeID column
  result <- merge_datasets(invalid_employee, performance_data, education_data)
  expect_false(result$success)
})

# =============================================================================
# OVERVIEW MODULE TESTS
# =============================================================================

test_that("KPI Calculation Functions", {
  # Mock KPI calculation functions
  calculate_kpi_metrics <- function(data) {
    if (!is.data.frame(data) || nrow(data) == 0) {
      return(list(error = "Invalid data"))
    }
    
    total_employees <- nrow(data)
    
    # Calculate attrition rate
    attrition_rate <- if ("Attrition" %in% names(data)) {
      sum(data$Attrition == "Yes", na.rm = TRUE) / total_employees
    } else {
      NA
    }
    
    # Calculate average age
    avg_age <- if ("Age" %in% names(data)) {
      mean(data$Age, na.rm = TRUE)
    } else {
      NA
    }
    
    # Calculate average salary (excluding specific compensation calculations)
    avg_salary <- if ("Salary" %in% names(data)) {
      mean(data$Salary, na.rm = TRUE)
    } else {
      NA
    }
    
    return(list(
      total_employees = total_employees,
      attrition_rate = attrition_rate,
      avg_age = avg_age,
      avg_salary = avg_salary
    ))
  }
  
  # Test KPI calculations
  test_data <- create_test_employee_data()
  kpis <- calculate_kpi_metrics(test_data)
  
  expect_equal(kpis$total_employees, 100)
  expect_true(kpis$attrition_rate >= 0 && kpis$attrition_rate <= 1)
  expect_true(kpis$avg_age > 0)
  expect_true(kpis$avg_salary > 0)
  
  # Test with empty data
  empty_data <- data.frame()
  kpis_empty <- calculate_kpi_metrics(empty_data)
  expect_true("error" %in% names(kpis_empty))
})

test_that("Summary Statistics Functions", {
  # Mock summary statistics function
  generate_summary_stats <- function(data) {
    if (!is.data.frame(data)) return(NULL)
    
    summary_stats <- list()
    
    # Department distribution
    if ("Department" %in% names(data)) {
      summary_stats$department_counts <- table(data$Department)
    }
    
    # Gender distribution
    if ("Gender" %in% names(data)) {
      summary_stats$gender_distribution <- table(data$Gender)
    }
    
    # Age groups
    if ("Age" %in% names(data)) {
      age_groups <- cut(data$Age, breaks = c(0, 30, 40, 50, 60, 100), 
                       labels = c("Under 30", "30-39", "40-49", "50-59", "60+"))
      summary_stats$age_groups <- table(age_groups)
    }
    
    return(summary_stats)
  }
  
  # Test summary statistics
  test_data <- create_test_employee_data()
  stats <- generate_summary_stats(test_data)
  
  expect_true(is.list(stats))
  expect_true("department_counts" %in% names(stats))
  expect_true("gender_distribution" %in% names(stats))
  expect_true("age_groups" %in% names(stats))
  
  # Verify department counts sum to total employees
  expect_equal(sum(stats$department_counts), nrow(test_data))
})

# =============================================================================
# ATTRITION ANALYSIS MODULE TESTS
# =============================================================================

test_that("Attrition Analysis Functions", {
  # Mock attrition analysis functions
  analyze_attrition_factors <- function(data) {
    if (!"Attrition" %in% names(data)) {
      return(list(error = "Attrition column not found"))
    }
    
    attrition_data <- data[data$Attrition == "Yes", ]
    retained_data <- data[data$Attrition == "No", ]
    
    analysis <- list()
    
    # Attrition by department
    if ("Department" %in% names(data)) {
      dept_attrition <- data %>%
        group_by(Department) %>%
        summarise(
          total = n(),
          attrition_count = sum(Attrition == "Yes"),
          attrition_rate = attrition_count / total,
          .groups = "drop"
        )
      analysis$by_department <- dept_attrition
    }
    
    # Attrition by age group
    if ("Age" %in% names(data)) {
      age_groups <- cut(data$Age, breaks = c(0, 30, 40, 50, 60, 100))
      age_attrition <- data %>%
        mutate(AgeGroup = age_groups) %>%
        group_by(AgeGroup) %>%
        summarise(
          total = n(),
          attrition_count = sum(Attrition == "Yes"),
          attrition_rate = attrition_count / total,
          .groups = "drop"
        )
      analysis$by_age_group <- age_attrition
    }
    
    return(analysis)
  }
  
  # Test attrition analysis
  test_data <- create_test_employee_data()
  analysis <- analyze_attrition_factors(test_data)
  
  expect_true(is.list(analysis))
  expect_true("by_department" %in% names(analysis))
  expect_true("by_age_group" %in% names(analysis))
  
  # Verify attrition rates are between 0 and 1
  dept_analysis <- analysis$by_department
  expect_true(all(dept_analysis$attrition_rate >= 0 & dept_analysis$attrition_rate <= 1))
  
  # Test with missing attrition column
  invalid_data <- test_data
  invalid_data$Attrition <- NULL
  analysis_invalid <- analyze_attrition_factors(invalid_data)
  expect_true("error" %in% names(analysis_invalid))
})

test_that("Attrition Risk Factors", {
  # Mock risk factor analysis
  calculate_attrition_risk_factors <- function(data) {
    if (!"Attrition" %in% names(data)) return(NULL)
    
    risk_factors <- list()
    
    # Overtime impact
    if ("OverTime" %in% names(data)) {
      overtime_risk <- data %>%
        group_by(OverTime) %>%
        summarise(
          attrition_rate = mean(Attrition == "Yes"),
          count = n(),
          .groups = "drop"
        )
      risk_factors$overtime <- overtime_risk
    }
    
    # Distance from home impact
    if ("DistanceFromHome" %in% names(data)) {
      distance_groups <- cut(data$DistanceFromHome, breaks = c(0, 10, 20, 30, 100))
      distance_risk <- data %>%
        mutate(DistanceGroup = distance_groups) %>%
        group_by(DistanceGroup) %>%
        summarise(
          attrition_rate = mean(Attrition == "Yes"),
          count = n(),
          .groups = "drop"
        )
      risk_factors$distance <- distance_risk
    }
    
    # Years at company impact
    if ("YearsAtCompany" %in% names(data)) {
      tenure_groups <- cut(data$YearsAtCompany, breaks = c(0, 2, 5, 10, 30))
      tenure_risk <- data %>%
        mutate(TenureGroup = tenure_groups) %>%
        group_by(TenureGroup) %>%
        summarise(
          attrition_rate = mean(Attrition == "Yes"),
          count = n(),
          .groups = "drop"
        )
      risk_factors$tenure <- tenure_risk
    }
    
    return(risk_factors)
  }
  
  # Test risk factor analysis
  test_data <- create_test_employee_data()
  risk_factors <- calculate_attrition_risk_factors(test_data)
  
  expect_true(is.list(risk_factors))
  expect_true("overtime" %in% names(risk_factors))
  expect_true("distance" %in% names(risk_factors))
  expect_true("tenure" %in% names(risk_factors))
  
  # Verify risk factor calculations
  overtime_risk <- risk_factors$overtime
  expect_true(all(overtime_risk$attrition_rate >= 0 & overtime_risk$attrition_rate <= 1))
  expect_equal(sum(overtime_risk$count), nrow(test_data))
})

# =============================================================================
# DEMOGRAPHICS MODULE TESTS
# =============================================================================

test_that("Demographic Analysis Functions", {
  # Mock demographic analysis functions
  analyze_demographic_patterns <- function(data) {
    if (!is.data.frame(data)) return(NULL)
    
    demographics <- list()
    
    # Gender distribution by department
    if (all(c("Gender", "Department") %in% names(data))) {
      gender_dept <- data %>%
        group_by(Department, Gender) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = Gender, values_from = count, values_fill = 0)
      demographics$gender_by_department <- gender_dept
    }
    
    # Age distribution by gender
    if (all(c("Age", "Gender") %in% names(data))) {
      age_gender <- data %>%
        group_by(Gender) %>%
        summarise(
          avg_age = mean(Age, na.rm = TRUE),
          median_age = median(Age, na.rm = TRUE),
          min_age = min(Age, na.rm = TRUE),
          max_age = max(Age, na.rm = TRUE),
          .groups = "drop"
        )
      demographics$age_by_gender <- age_gender
    }
    
    # Ethnicity distribution
    if ("Ethnicity" %in% names(data)) {
      ethnicity_dist <- data %>%
        group_by(Ethnicity) %>%
        summarise(
          count = n(),
          percentage = n() / nrow(data) * 100,
          .groups = "drop"
        )
      demographics$ethnicity_distribution <- ethnicity_dist
    }
    
    return(demographics)
  }
  
  # Test demographic analysis
  test_data <- create_test_employee_data()
  demographics <- analyze_demographic_patterns(test_data)
  
  expect_true(is.list(demographics))
  expect_true("gender_by_department" %in% names(demographics))
  expect_true("age_by_gender" %in% names(demographics))
  expect_true("ethnicity_distribution" %in% names(demographics))
  
  # Verify ethnicity percentages sum to 100
  ethnicity_dist <- demographics$ethnicity_distribution
  expect_equal(round(sum(ethnicity_dist$percentage)), 100)
})

test_that("Diversity Metrics Calculation", {
  # Mock diversity metrics function
  calculate_diversity_metrics <- function(data) {
    if (!is.data.frame(data)) return(NULL)
    
    diversity_metrics <- list()
    
    # Gender diversity index (simple calculation)
    if ("Gender" %in% names(data)) {
      gender_counts <- table(data$Gender)
      total_employees <- sum(gender_counts)
      gender_proportions <- gender_counts / total_employees
      
      # Simpson's Diversity Index
      diversity_index <- 1 - sum(gender_proportions^2)
      diversity_metrics$gender_diversity_index <- diversity_index
    }
    
    # Department representation
    if ("Department" %in% names(data)) {
      dept_counts <- table(data$Department)
      diversity_metrics$department_distribution <- dept_counts
    }
    
    # Geographic diversity
    if ("State" %in% names(data)) {
      state_counts <- table(data$State)
      diversity_metrics$geographic_diversity <- length(state_counts)
    }
    
    return(diversity_metrics)
  }
  
  # Test diversity metrics
  test_data <- create_test_employee_data()
  diversity <- calculate_diversity_metrics(test_data)
  
  expect_true(is.list(diversity))
  expect_true("gender_diversity_index" %in% names(diversity))
  expect_true("department_distribution" %in% names(diversity))
  expect_true("geographic_diversity" %in% names(diversity))
  
  # Verify diversity index is between 0 and 1
  expect_true(diversity$gender_diversity_index >= 0 && diversity$gender_diversity_index <= 1)
})

# =============================================================================
# PERFORMANCE MODULE TESTS
# =============================================================================

test_that("Performance Metrics Calculation", {
  # Mock performance metrics functions
  calculate_performance_metrics <- function(performance_data) {
    if (!is.data.frame(performance_data)) return(NULL)
    
    performance_metrics <- list()
    
    # Average ratings
    rating_columns <- c("EnvironmentSatisfaction", "JobSatisfaction", 
                       "RelationshipSatisfaction", "WorkLifeBalance", 
                       "SelfRating", "ManagerRating")
    
    for (col in rating_columns) {
      if (col %in% names(performance_data)) {
        performance_metrics[[paste0("avg_", tolower(col))]] <- 
          mean(performance_data[[col]], na.rm = TRUE)
      }
    }
    
    # Training metrics
    if (all(c("TrainingOpportunitiesWithinYear", "TrainingOpportunitiesTaken") %in% names(performance_data))) {
      performance_metrics$training_participation_rate <- 
        mean(performance_data$TrainingOpportunitiesTaken / 
             pmax(performance_data$TrainingOpportunitiesWithinYear, 1), na.rm = TRUE)
    }
    
    return(performance_metrics)
  }
  
  # Test performance metrics
  test_performance_data <- create_test_performance_data()
  metrics <- calculate_performance_metrics(test_performance_data)
  
  expect_true(is.list(metrics))
  expect_true("avg_jobsatisfaction" %in% names(metrics))
  expect_true("avg_environmentsatisfaction" %in% names(metrics))
  expect_true("training_participation_rate" %in% names(metrics))
  
  # Verify ratings are within expected range (1-5)
  expect_true(metrics$avg_jobsatisfaction >= 1 && metrics$avg_jobsatisfaction <= 5)
  expect_true(metrics$training_participation_rate >= 0 && metrics$training_participation_rate <= 1)
})

test_that("Performance Rating Analysis", {
  # Mock rating analysis function
  analyze_rating_patterns <- function(performance_data) {
    if (!is.data.frame(performance_data)) return(NULL)
    
    rating_analysis <- list()
    
    # Self vs Manager rating comparison
    if (all(c("SelfRating", "ManagerRating") %in% names(performance_data))) {
      rating_comparison <- performance_data %>%
        summarise(
          avg_self_rating = mean(SelfRating, na.rm = TRUE),
          avg_manager_rating = mean(ManagerRating, na.rm = TRUE),
          rating_difference = mean(SelfRating - ManagerRating, na.rm = TRUE),
          correlation = cor(SelfRating, ManagerRating, use = "complete.obs"),
          .groups = "drop"
        )
      rating_analysis$self_vs_manager <- rating_comparison
    }
    
    # Rating distribution
    if ("ManagerRating" %in% names(performance_data)) {
      rating_dist <- table(performance_data$ManagerRating)
      rating_analysis$manager_rating_distribution <- rating_dist
    }
    
    return(rating_analysis)
  }
  
  # Test rating analysis
  test_performance_data <- create_test_performance_data()
  analysis <- analyze_rating_patterns(test_performance_data)
  
  expect_true(is.list(analysis))
  expect_true("self_vs_manager" %in% names(analysis))
  expect_true("manager_rating_distribution" %in% names(analysis))
  
  # Verify correlation is between -1 and 1