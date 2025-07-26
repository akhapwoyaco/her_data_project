# =============================================================================
# ATLAS LABS HR ANALYTICS - DATA INTEGRITY UNIT TESTS
# =============================================================================
# Comprehensive unit tests for data validation, outlier identification, 
# and data quality scoring with extensive edge cases
# 
# Test Framework: testthat
# Focus Areas: Data Validation, Outlier Detection, Data Quality Scoring
# =============================================================================

library(testthat)
library(tidyverse)
library(lubridate)

# =============================================================================
# HELPER FUNCTIONS FOR DATA INTEGRITY TESTING
# =============================================================================

#' Generate synthetic test data with controlled anomalies
#' @param n_rows Number of rows to generate
#' @param anomaly_rate Proportion of anomalous records (0-1)
#' @param missing_rate Proportion of missing values (0-1)
generate_test_employee_data <- function(n_rows = 100, anomaly_rate = 0.1, missing_rate = 0.05) {
  set.seed(123)
  
  # Base realistic data
  data <- tibble(
    EmployeeID = 1:n_rows,
    FirstName = sample(c("John", "Jane", "Michael", "Sarah", "David", "Lisa"), n_rows, replace = TRUE),
    LastName = sample(c("Smith", "Johnson", "Williams", "Brown", "Davis", "Miller"), n_rows, replace = TRUE),
    Gender = sample(c("Male", "Female", "Non-binary"), n_rows, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    Age = pmax(18, pmin(75, rnorm(n_rows, 40, 12))),
    BusinessTravel = sample(c("Rarely", "Frequently", "Non-Travel"), n_rows, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    Department = sample(c("Sales", "R&D", "HR", "Finance", "Marketing", "IT"), n_rows, replace = TRUE),
    DistanceFromHome = pmax(0, rnorm(n_rows, 15, 10)),
    State = sample(state.abb[1:10], n_rows, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n_rows, replace = TRUE),
    Education = sample(1:5, n_rows, replace = TRUE),
    EducationField = sample(c("Engineering", "Business", "Liberal Arts", "Science", "Medical", "Other"), n_rows, replace = TRUE),
    JobRole = sample(c("Manager", "Developer", "Analyst", "Specialist", "Director", "VP"), n_rows, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n_rows, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
    Salary = pmax(30000, rnorm(n_rows, 75000, 25000)),
    StockOptionLevel = sample(0:3, n_rows, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    OverTime = sample(c("Yes", "No"), n_rows, replace = TRUE, prob = c(0.3, 0.7)),
    HireDate = as.Date("2020-01-01") + sample(0:1460, n_rows, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n_rows, replace = TRUE, prob = c(0.15, 0.85)),
    YearsAtCompany = pmax(0, rnorm(n_rows, 5, 3)),
    YearsInMostRecentRole = pmax(0, rnorm(n_rows, 3, 2)),
    YearsSinceLastPromotion = pmax(0, rnorm(n_rows, 2, 1.5)),
    YearsWithCurrManager = pmax(0, rnorm(n_rows, 2.5, 1.8))
  )
  
  # Inject anomalies
  anomaly_indices <- sample(1:n_rows, floor(n_rows * anomaly_rate))
  
  # Age anomalies
  data$Age[anomaly_indices[1:max(1, length(anomaly_indices) %/% 4)]] <- 
    sample(c(-5, 0, 150, 200), max(1, length(anomaly_indices) %/% 4), replace = TRUE)
  
  # Salary anomalies
  data$Salary[anomaly_indices[max(1, length(anomaly_indices) %/% 4 + 1):max(2, length(anomaly_indices) %/% 2)]] <- 
    sample(c(-1000, 0, 5000000, 10000000), max(1, length(anomaly_indices) %/% 4), replace = TRUE)
  
  # Date anomalies
  data$HireDate[anomaly_indices[max(2, length(anomaly_indices) %/% 2 + 1):max(3, 3 * length(anomaly_indices) %/% 4)]] <- 
    as.Date(c("1800-01-01", "2050-12-31", "3000-01-01"))
  
  # Inject missing values
  missing_indices <- sample(1:n_rows, floor(n_rows * missing_rate))
  cols_to_miss <- sample(names(data)[-1], min(5, ncol(data)-1))
  for(col in cols_to_miss) {
    missing_col_indices <- sample(missing_indices, max(1, length(missing_indices) %/% length(cols_to_miss)))
    data[[col]][missing_col_indices] <- NA
  }
  
  return(data)
}

#' Generate synthetic performance rating test data
generate_test_performance_data <- function(n_rows = 100, anomaly_rate = 0.1) {
  set.seed(456)
  
  data <- tibble(
    PerformanceID = 1:n_rows,
    EmployeeID = sample(1:80, n_rows, replace = TRUE),
    ReviewDate = as.Date("2023-01-01") + sample(0:365, n_rows, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n_rows, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.35, 0.15)),
    JobSatisfaction = sample(1:5, n_rows, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.35, 0.15)),
    RelationshipSatisfaction = sample(1:5, n_rows, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.35, 0.15)),
    WorkLifeBalance = sample(1:5, n_rows, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.35, 0.15)),
    SelfRating = sample(1:5, n_rows, replace = TRUE, prob = c(0.05, 0.1, 0.25, 0.4, 0.2)),
    ManagerRating = sample(1:5, n_rows, replace = TRUE, prob = c(0.05, 0.1, 0.25, 0.4, 0.2)),
    TrainingOpportunitiesWithinYear = sample(0:10, n_rows, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, n_rows, replace = TRUE)
  )
  
  # Inject anomalies
  anomaly_indices <- sample(1:n_rows, floor(n_rows * anomaly_rate))
  data$EnvironmentSatisfaction[anomaly_indices[1:max(1, length(anomaly_indices) %/% 3)]] <- 
    sample(c(-1, 0, 6, 10, 100), max(1, length(anomaly_indices) %/% 3), replace = TRUE)
  
  data$TrainingOpportunitiesTaken[anomaly_indices] <- 
    pmax(data$TrainingOpportunitiesWithinYear[anomaly_indices] + sample(1:5, length(anomaly_indices), replace = TRUE), 
         sample(c(-5, 50, 100), length(anomaly_indices), replace = TRUE))
  
  return(data)
}

# =============================================================================
# DATA VALIDATION FUNCTIONS TO TEST
# =============================================================================

#' Validate employee data structure and content
validate_employee_data <- function(data) {
  validation_results <- list(
    is_valid = TRUE,
    errors = character(0),
    warnings = character(0),
    data_quality_score = 100
  )
  
  # Required columns check
  required_cols <- c("EmployeeID", "Age", "Department", "Salary", "HireDate", "Attrition")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    validation_results$is_valid <- FALSE
    validation_results$errors <- c(validation_results$errors, 
                                   paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Data type validation
  if ("Age" %in% names(data) && !is.numeric(data$Age)) {
    validation_results$errors <- c(validation_results$errors, "Age must be numeric")
    validation_results$is_valid <- FALSE
  }
  
  if ("Salary" %in% names(data) && !is.numeric(data$Salary)) {
    validation_results$errors <- c(validation_results$errors, "Salary must be numeric")
    validation_results$is_valid <- FALSE
  }
  
  if ("HireDate" %in% names(data) && !inherits(data$HireDate, "Date")) {
    validation_results$errors <- c(validation_results$errors, "HireDate must be Date type")
    validation_results$is_valid <- FALSE
  }
  
  # Business logic validation
  if ("Age" %in% names(data)) {
    invalid_ages <- which(data$Age < 16 | data$Age > 80 | is.na(data$Age))
    if (length(invalid_ages) > 0) {
      validation_results$warnings <- c(validation_results$warnings, 
                                       paste("Invalid ages found in rows:", paste(head(invalid_ages, 10), collapse = ", ")))
      validation_results$data_quality_score <- validation_results$data_quality_score - (length(invalid_ages) / nrow(data)) * 20
    }
  }
  
  if ("Salary" %in% names(data)) {
    invalid_salaries <- which(data$Salary < 0 | data$Salary > 1000000 | is.na(data$Salary))
    if (length(invalid_salaries) > 0) {
      validation_results$warnings <- c(validation_results$warnings, 
                                       paste("Invalid salaries found in rows:", paste(head(invalid_salaries, 10), collapse = ", ")))
      validation_results$data_quality_score <- validation_results$data_quality_score - (length(invalid_salaries) / nrow(data)) * 15
    }
  }
  
  if ("HireDate" %in% names(data)) {
    current_date <- Sys.Date()
    future_dates <- which(data$HireDate > current_date)
    ancient_dates <- which(data$HireDate < as.Date("1950-01-01"))
    invalid_dates <- c(future_dates, ancient_dates)
    if (length(invalid_dates) > 0) {
      validation_results$warnings <- c(validation_results$warnings, 
                                       paste("Invalid hire dates found in rows:", paste(head(invalid_dates, 10), collapse = ", ")))
      validation_results$data_quality_score <- validation_results$data_quality_score - (length(invalid_dates) / nrow(data)) * 10
    }
  }
  
  # Missing data penalty
  missing_data_penalty <- sum(is.na(data)) / (nrow(data) * ncol(data)) * 25
  validation_results$data_quality_score <- max(0, validation_results$data_quality_score - missing_data_penalty)
  
  return(validation_results)
}

#' Identify outliers in numerical columns using multiple methods
identify_outliers <- function(data, method = "iqr", threshold = 1.5) {
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  outliers <- list()
  
  for (col in numeric_cols) {
    col_data <- data[[col]][!is.na(data[[col]])]
    if (length(col_data) == 0) next
    
    outlier_indices <- switch(method,
      "iqr" = {
        Q1 <- quantile(col_data, 0.25)
        Q3 <- quantile(col_data, 0.75)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - threshold * IQR
        upper_bound <- Q3 + threshold * IQR
        which(data[[col]] < lower_bound | data[[col]] > upper_bound)
      },
      "zscore" = {
        z_scores <- abs((data[[col]] - mean(col_data, na.rm = TRUE)) / sd(col_data, na.rm = TRUE))
        which(z_scores > threshold)
      },
      "modified_zscore" = {
        median_val <- median(col_data, na.rm = TRUE)
        mad_val <- mad(col_data, na.rm = TRUE)
        modified_z_scores <- 0.6745 * (data[[col]] - median_val) / mad_val
        which(abs(modified_z_scores) > threshold)
      }
    )
    
    if (length(outlier_indices) > 0) {
      outliers[[col]] <- list(
        indices = outlier_indices,
        values = data[[col]][outlier_indices],
        method = method,
        threshold = threshold
      )
    }
  }
  
  return(outliers)
}

#' Calculate comprehensive data quality score
calculate_data_quality_score <- function(data, weights = NULL) {
  if (is.null(weights)) {
    weights <- list(
      completeness = 0.25,
      validity = 0.25,
      consistency = 0.20,
      uniqueness = 0.15,
      accuracy = 0.15
    )
  }
  
  scores <- list()
  
  # Completeness Score (percentage of non-missing values)
  scores$completeness <- (sum(!is.na(data)) / (nrow(data) * ncol(data))) * 100
  
  # Validity Score (percentage of values within expected ranges)
  validity_checks <- 0
  total_checks <- 0
  
  if ("Age" %in% names(data)) {
    validity_checks <- validity_checks + sum(data$Age >= 16 & data$Age <= 80, na.rm = TRUE)
    total_checks <- total_checks + sum(!is.na(data$Age))
  }
  
  if ("Salary" %in% names(data)) {
    validity_checks <- validity_checks + sum(data$Salary > 0 & data$Salary <= 1000000, na.rm = TRUE)
    total_checks <- total_checks + sum(!is.na(data$Salary))
  }
  
  scores$validity <- ifelse(total_checks > 0, (validity_checks / total_checks) * 100, 100)
  
  # Consistency Score (relationships between fields make sense)
  consistency_score <- 100
  
  if (all(c("YearsAtCompany", "HireDate") %in% names(data))) {
    expected_years <- as.numeric(Sys.Date() - data$HireDate) / 365.25
    inconsistent <- abs(data$YearsAtCompany - expected_years) > 1
    consistency_score <- consistency_score - (sum(inconsistent, na.rm = TRUE) / nrow(data)) * 30
  }
  
  scores$consistency <- max(0, consistency_score)
  
  # Uniqueness Score (for ID fields)
  uniqueness_score <- 100
  if ("EmployeeID" %in% names(data)) {
    duplicate_ids <- sum(duplicated(data$EmployeeID))
    uniqueness_score <- uniqueness_score - (duplicate_ids / nrow(data)) * 100
  }
  scores$uniqueness <- max(0, uniqueness_score)
  
  # Accuracy Score (based on outlier detection)
  outliers <- identify_outliers(data, method = "iqr")
  total_outliers <- sum(sapply(outliers, function(x) length(x$indices)))
  accuracy_penalty <- (total_outliers / nrow(data)) * 20
  scores$accuracy <- max(0, 100 - accuracy_penalty)
  
  # Calculate weighted average
  overall_score <- sum(mapply(function(score, weight) score * weight, scores, weights))
  
  return(list(
    overall_score = round(overall_score, 2),
    component_scores = scores,
    weights = weights
  ))
}

# =============================================================================
# UNIT TESTS - DATA VALIDATION
# =============================================================================

test_that("Data Validation - Basic Structure Validation", {
  # Test with valid data
  valid_data <- generate_test_employee_data(50, 0, 0)
  result <- validate_employee_data(valid_data)
  
  expect_true(result$is_valid)
  expect_length(result$errors, 0)
  expect_equal(result$data_quality_score, 100)
})

test_that("Data Validation - Missing Required Columns", {
  # Test with missing required columns
  incomplete_data <- tibble(
    EmployeeID = 1:10,
    FirstName = rep("John", 10)
    # Missing Age, Department, Salary, HireDate, Attrition
  )
  
  result <- validate_employee_data(incomplete_data)
  
  expect_false(result$is_valid)
  expect_length(result$errors, 1)
  expect_true(str_detect(result$errors[1], "Missing required columns"))
})

test_that("Data Validation - Wrong Data Types", {
  # Test with wrong data types
  wrong_type_data <- tibble(
    EmployeeID = 1:10,
    Age = rep("thirty", 10), # Should be numeric
    Department = rep("Sales", 10),
    Salary = rep("50000", 10), # Should be numeric
    HireDate = rep("2020-01-01", 10), # Should be Date
    Attrition = rep("No", 10)
  )
  
  result <- validate_employee_data(wrong_type_data)
  
  expect_false(result$is_valid)
  expect_true(any(str_detect(result$errors, "Age must be numeric")))
  expect_true(any(str_detect(result$errors, "Salary must be numeric")))
  expect_true(any(str_detect(result$errors, "HireDate must be Date")))
})

test_that("Data Validation - Business Logic Violations", {
  # Test with business logic violations
  invalid_data <- tibble(
    EmployeeID = 1:10,
    Age = c(25, -5, 30, 150, 35, 0, 40, 200, 45, 12), # Invalid ages
    Department = rep("Sales", 10),
    Salary = c(50000, -1000, 60000, 5000000, 70000, 0, 80000, 10000000, 90000, 100000), # Invalid salaries
    HireDate = as.Date(c("2020-01-01", "1800-01-01", "2021-01-01", "2050-12-31", "2022-01-01", 
                        "1900-01-01", "2023-01-01", "3000-01-01", "2024-01-01", "1850-01-01")),
    Attrition = rep("No", 10)
  )
  
  result <- validate_employee_data(invalid_data)
  
  expect_true(result$is_valid) # Structure is valid, but has warnings
  expect_true(length(result$warnings) > 0)
  expect_true(any(str_detect(result$warnings, "Invalid ages")))
  expect_true(any(str_detect(result$warnings, "Invalid salaries")))
  expect_true(any(str_detect(result$warnings, "Invalid hire dates")))
  expect_true(result$data_quality_score < 100)
})

test_that("Data Validation - Empty Dataset", {
  # Test with empty dataset
  empty_data <- tibble()
  
  result <- validate_employee_data(empty_data)
  
  expect_false(result$is_valid)
  expect_true(any(str_detect(result$errors, "Missing required columns")))
})

test_that("Data Validation - Single Row Dataset", {
  # Test with single row
  single_row_data <- tibble(
    EmployeeID = 1,
    Age = 30,
    Department = "Sales",
    Salary = 50000,
    HireDate = as.Date("2020-01-01"),
    Attrition = "No"
  )
  
  result <- validate_employee_data(single_row_data)
  
  expect_true(result$is_valid)
  expect_length(result$errors, 0)
  expect_equal(result$data_quality_score, 100)
})

test_that("Data Validation - All Missing Values", {
  # Test with all missing values
  missing_data <- tibble(
    EmployeeID = c(1, 2, 3),
    Age = c(NA, NA, NA),
    Department = c(NA, NA, NA),
    Salary = c(NA, NA, NA),
    HireDate = as.Date(c(NA, NA, NA)),
    Attrition = c(NA, NA, NA)
  )
  
  result <- validate_employee_data(missing_data)
  
  expect_true(result$is_valid) # Structure is valid
  expect_true(length(result$warnings) > 0)
  expect_true(result$data_quality_score < 50) # Heavy penalty for missing data
})

# =============================================================================
# UNIT TESTS - OUTLIER IDENTIFICATION
# =============================================================================

test_that("Outlier Detection - IQR Method", {
  # Test data with known outliers
  test_data <- tibble(
    EmployeeID = 1:20,
    Age = c(rep(30, 15), 10, 80, 90, 5, 100), # Last 5 are outliers
    Salary = c(rep(50000, 15), 10000, 200000, 300000, 5000, 500000) # Last 5 are outliers
  )
  
  outliers <- identify_outliers(test_data, method = "iqr", threshold = 1.5)
  
  expect_true("Age" %in% names(outliers))
  expect_true("Salary" %in% names(outliers))
  expect_true(length(outliers$Age$indices) > 0)
  expect_true(length(outliers$Salary$indices) > 0)
  expect_equal(outliers$Age$method, "iqr")
  expect_equal(outliers$Age$threshold, 1.5)
})

test_that("Outlier Detection - Z-Score Method", {
  set.seed(123)
  # Generate normal data with outliers
  normal_data <- rnorm(100, 50, 10)
  outlier_data <- c(normal_data, 150, -50, 200) # Add extreme outliers
  
  test_data <- tibble(
    EmployeeID = 1:103,
    TestValue = outlier_data
  )
  
  outliers <- identify_outliers(test_data, method = "zscore", threshold = 3)
  
  expect_true("TestValue" %in% names(outliers))
  expect_true(length(outliers$TestValue$indices) >= 2) # At least the extreme outliers
  expect_equal(outliers$TestValue$method, "zscore")
})

test_that("Outlier Detection - Modified Z-Score Method", {
  # Test with data that has extreme outliers (where modified z-score is more robust)
  test_data <- tibble(
    EmployeeID = 1:21,
    Value = c(rep(100, 19), 1000, 10000) # Two extreme outliers
  )
  
  outliers <- identify_outliers(test_data, method = "modified_zscore", threshold = 3.5)
  
  expect_true("Value" %in% names(outliers))
  expect_equal(outliers$Value$method, "modified_zscore")
})

test_that("Outlier Detection - No Outliers", {
  # Test with perfectly normal data
  set.seed(456)
  test_data <- tibble(
    EmployeeID = 1:50,
    Age = rnorm(50, 40, 5), # Tightly distributed
    Salary = rnorm(50, 75000, 5000) # Tightly distributed
  )
  
  outliers <- identify_outliers(test_data, method = "iqr", threshold = 3) # High threshold
  
  expect_true(length(outliers) == 0 || all(sapply(outliers, function(x) length(x$indices)) == 0))
})

test_that("Outlier Detection - Single Column with Missing Values", {
  # Test with missing values
  test_data <- tibble(
    EmployeeID = 1:10,
    Age = c(25, 30, NA, 35, 40, NA, 45, 200, 50, NA), # One outlier, several NAs
    Salary = c(rep(50000, 7), 500000, rep(55000, 2)) # One outlier
  )
  
  outliers <- identify_outliers(test_data, method = "iqr")
  
  expect_true("Age" %in% names(outliers) || "Salary" %in% names(outliers))
  # Should handle NAs gracefully without errors
})

test_that("Outlier Detection - All Missing Values", {
  # Test with column of all missing values
  test_data <- tibble(
    EmployeeID = 1:5,
    Age = c(NA, NA, NA, NA, NA),
    Salary = c(50000, 60000, 70000, 80000, 90000)
  )
  
  outliers <- identify_outliers(test_data, method = "iqr")
  
  expect_false("Age" %in% names(outliers)) # Should skip all-NA columns
  expect_false("Salary" %in% names(outliers)) # No outliers in salary
})

test_that("Outlier Detection - Edge Case: Single Value", {
  # Test with single non-missing value
  test_data <- tibble(
    EmployeeID = 1:5,
    Age = c(30, NA, NA, NA, NA),
    Salary = c(50000, 60000, 70000, 80000, 90000)
  )
  
  outliers <- identify_outliers(test_data, method = "iqr")
  
  # Should handle gracefully - single value can't be an outlier
  expect_true(length(outliers) == 0 || all(sapply(outliers, function(x) length(x$indices)) == 0))
})

# =============================================================================
# UNIT TESTS - DATA QUALITY SCORING
# =============================================================================

test_that("Data Quality Scoring - Perfect Data", {
  # Test with perfect data
  perfect_data <- tibble(
    EmployeeID = 1:50,
    Age = sample(25:60, 50, replace = TRUE),
    Department = sample(c("Sales", "IT", "HR"), 50, replace = TRUE),
    Salary = sample(40000:120000, 50, replace = TRUE),
    HireDate = as.Date("2020-01-01") + sample(0:1000, 50),
    YearsAtCompany = sample(1:10, 50, replace = TRUE)
  )
  # Make YearsAtCompany consistent with HireDate
  perfect_data$YearsAtCompany <- pmax(0, as.numeric(Sys.Date() - perfect_data$HireDate) / 365.25)
  
  quality_score <- calculate_data_quality_score(perfect_data)
  
  expect_gte(quality_score$overall_score, 85) # Should be high quality
  expect_equal(quality_score$component_scores$completeness, 100)
  expect_gte(quality_score$component_scores$validity, 90)
  expect_gte(quality_score$component_scores$uniqueness, 100)
})

test_that("Data Quality Scoring - Poor Completeness", {
  # Test with lots of missing data
  incomplete_data <- tibble(
    EmployeeID = 1:20,
    Age = c(rep(30, 10), rep(NA, 10)),
    Department = c(rep("Sales", 5), rep(NA, 15)),
    Salary = c(rep(50000, 3), rep(NA, 17)),
    HireDate = c(rep(as.Date("2020-01-01"), 2), rep(as.Date(NA), 18))
  )
  
  quality_score <- calculate_data_quality_score(incomplete_data)
  
  expect_lt(quality_score$overall_score, 50) # Should be low due to missing data
  expect_lt(quality_score$component_scores$completeness, 50)
})

test_that("Data Quality Scoring - Poor Validity", {
  # Test with invalid values
  invalid_data <- tibble(
    EmployeeID = 1:20,
    Age = c(rep(30, 10), rep(-10, 5), rep(150, 5)), # Invalid ages
    Department = rep("Sales", 20),
    Salary = c(rep(50000, 10), rep(-5000, 5), rep(2000000, 5)), # Invalid salaries
    HireDate = rep(as.Date("2020-01-01"), 20)
  )
  
  quality_score <- calculate_data_quality_score(invalid_data)
  
  expect_lt(quality_score$overall_score, 80) # Should be penalized for invalid values
  expect_lt(quality_score$component_scores$validity, 60)
})

test_that("Data Quality Scoring - Poor Consistency", {
  # Test with inconsistent relationships
  inconsistent_data <- tibble(
    EmployeeID = 1:10,
    Age = rep(30, 10),
    Department = rep("Sales", 10),
    Salary = rep(50000, 10),
    HireDate = rep(as.Date("2020-01-01"), 10),
    YearsAtCompany = c(rep(1, 5), rep(10