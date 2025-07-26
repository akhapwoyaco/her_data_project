# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE DATA INTEGRITY UNIT TESTS
# =============================================================================
# Testing Framework: testthat
# Coverage: Data Validation, Schema Compliance, Integrity Checking
# Author: Atlas Labs Development Team
# =============================================================================

library(testthat)
library(dplyr)
library(lubridate)
library(stringr)

# =============================================================================
# TEST DATA SETUP AND FIXTURES
# =============================================================================

# Mock data generators for testing edge cases
create_mock_employee_data <- function(n = 100, introduce_issues = FALSE) {
  set.seed(123)
  
  base_data <- data.frame(
    EmployeeID = 1:n,
    FirstName = sample(c("John", "Jane", "Mike", "Sarah", "David"), n, replace = TRUE),
    LastName = sample(c("Smith", "Johnson", "Williams", "Brown", "Davis"), n, replace = TRUE),
    Gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, prob = c(0.5, 0.45, 0.05)),
    Age = sample(22:65, n, replace = TRUE),
    BusinessTravel = sample(c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), n, replace = TRUE),
    Department = sample(c("Sales", "Research & Development", "Human Resources", "Technology"), n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL", "IL"), n, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n, replace = TRUE),
    Education = sample(1:5, n, replace = TRUE),
    EducationField = sample(c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Other"), n, replace = TRUE),
    JobRole = sample(c("Sales Executive", "Research Scientist", "Manager", "Sales Representative"), n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    Salary = sample(30000:150000, n, replace = TRUE),
    StockOptionLevel = sample(0:3, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    YearsAtCompany = sample(0:40, n, replace = TRUE),
    YearsInMostRecentRole = sample(0:18, n, replace = TRUE),
    YearsSinceLastPromotion = sample(0:15, n, replace = TRUE),
    YearsWithCurrManager = sample(0:17, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  if (introduce_issues) {
    # Introduce various data quality issues for testing
    base_data[sample(nrow(base_data), 5), "FirstName"] <- NA
    base_data[sample(nrow(base_data), 3), "Age"] <- c(-5, 150, 999)
    base_data[sample(nrow(base_data), 2), "Salary"] <- c(-50000, 5000000)
    base_data[sample(nrow(base_data), 4), "Gender"] <- c("Unknown", "", "Male ", " Female")
    base_data[sample(nrow(base_data), 2), "DistanceFromHome"] <- c(-10, 1000)
    base_data[c(10, 20), ] <- base_data[c(10, 20), ] # Create duplicates
    base_data[sample(nrow(base_data), 3), "HireDate"] <- as.Date("2030-01-01") # Future dates
  }
  
  return(base_data)
}

create_mock_performance_data <- function(n = 150, introduce_issues = FALSE) {
  set.seed(456)
  
  base_data <- data.frame(
    PerformanceID = 1:n,
    EmployeeID = sample(1:100, n, replace = TRUE),
    ReviewDate = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month"), n, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, n, replace = TRUE),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    SelfRating = sample(1:5, n, replace = TRUE),
    ManagerRating = sample(1:5, n, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, n, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:10, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  if (introduce_issues) {
    # Introduce performance-specific issues
    base_data[sample(nrow(base_data), 3), "EnvironmentSatisfaction"] <- c(0, 6, 10)
    base_data[sample(nrow(base_data), 2), "EmployeeID"] <- c(999, 1001) # Invalid foreign keys
    base_data[sample(nrow(base_data), 4), "TrainingOpportunitiesTaken"] <- 
      base_data[sample(nrow(base_data), 4), "TrainingOpportunitiesWithinYear"] + 5 # More taken than offered
    base_data[sample(nrow(base_data), 2), "ReviewDate"] <- as.Date("1990-01-01") # Too old
  }
  
  return(base_data)
}

create_mock_education_data <- function() {
  data.frame(
    EducationLevelID = 1:5,
    EducationLevel = c("Below College", "College", "Bachelor", "Master", "Doctor"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 5.1.1 SCHEMA COMPLIANCE VERIFICATION TESTS
# =============================================================================

test_that("Schema Compliance - Employee Data Structure", {
  # Test correct schema
  employee_data <- create_mock_employee_data(50)
  expected_columns <- c("EmployeeID", "FirstName", "LastName", "Gender", "Age", 
                       "BusinessTravel", "Department", "DistanceFromHome", "State", 
                       "Ethnicity", "Education", "EducationField", "JobRole", 
                       "MaritalStatus", "Salary", "StockOptionLevel", "OverTime", 
                       "HireDate", "Attrition", "YearsAtCompany", 
                       "YearsInMostRecentRole", "YearsSinceLastPromotion", 
                       "YearsWithCurrManager")
  
  expect_equal(sort(names(employee_data)), sort(expected_columns))
  expect_equal(nrow(employee_data), 50)
  expect_true(all(sapply(employee_data, function(x) !is.null(x))))
})

test_that("Schema Compliance - Missing Columns Detection", {
  employee_data <- create_mock_employee_data(10)
  
  # Test missing critical columns
  incomplete_data <- employee_data[, !names(employee_data) %in% c("EmployeeID", "Salary")]
  expect_false("EmployeeID" %in% names(incomplete_data))
  expect_false("Salary" %in% names(incomplete_data))
  
  # Test extra columns
  extra_data <- cbind(employee_data, ExtraColumn = 1:nrow(employee_data))
  expect_true("ExtraColumn" %in% names(extra_data))
  expect_gt(ncol(extra_data), ncol(employee_data))
})

test_that("Schema Compliance - Performance Data Structure", {
  performance_data <- create_mock_performance_data(30)
  expected_columns <- c("PerformanceID", "EmployeeID", "ReviewDate", 
                       "EnvironmentSatisfaction", "JobSatisfaction", 
                       "RelationshipSatisfaction", "WorkLifeBalance", 
                       "SelfRating", "ManagerRating", 
                       "TrainingOpportunitiesWithinYear", "TrainingOpportunitiesTaken")
  
  expect_equal(sort(names(performance_data)), sort(expected_columns))
  expect_equal(nrow(performance_data), 30)
})

test_that("Schema Compliance - Column Name Validation", {
  employee_data <- create_mock_employee_data(10)
  
  # Test column name patterns
  expect_true(all(grepl("^[A-Za-z][A-Za-z0-9]*$", names(employee_data))))
  expect_false(any(grepl("^[0-9]", names(employee_data)))) # No columns starting with numbers
  expect_false(any(grepl("\\s", names(employee_data))))    # No spaces in column names
})

# =============================================================================
# 5.1.2 DATA TYPE CONSISTENCY TESTS
# =============================================================================

test_that("Data Type Consistency - Employee Data Types", {
  employee_data <- create_mock_employee_data(100)
  
  # Numeric fields
  expect_true(is.numeric(employee_data$EmployeeID))
  expect_true(is.numeric(employee_data$Age))
  expect_true(is.numeric(employee_data$Salary))
  expect_true(is.numeric(employee_data$DistanceFromHome))
  expect_true(is.numeric(employee_data$Education))
  expect_true(is.numeric(employee_data$StockOptionLevel))
  expect_true(is.numeric(employee_data$YearsAtCompany))
  
  # Character fields
  expect_true(is.character(employee_data$FirstName))
  expect_true(is.character(employee_data$LastName))
  expect_true(is.character(employee_data$Gender))
  expect_true(is.character(employee_data$Department))
  expect_true(is.character(employee_data$State))
  expect_true(is.character(employee_data$JobRole))
  
  # Date fields
  expect_true(inherits(employee_data$HireDate, "Date"))
})

test_that("Data Type Consistency - Type Conversion Edge Cases", {
  # Test data with mixed types
  mixed_data <- data.frame(
    EmployeeID = c("1", "2", "3", "4.0", "5.5"),  # String numbers
    Age = c("25", "30", "35", "40", "45"),         # String ages
    Salary = c("50000", "60000.50", "70000", "invalid", "80000"),
    stringsAsFactors = FALSE
  )
  
  # Test conversion validation
  expect_error(as.numeric(mixed_data$Salary[4])) # "invalid" should fail
  expect_true(is.na(as.numeric(mixed_data$Salary[4])))
  
  # Test successful conversions
  expect_equal(as.numeric(mixed_data$EmployeeID[1]), 1)
  expect_equal(as.numeric(mixed_data$Age[1]), 25)
  expect_equal(as.numeric(mixed_data$Salary[1]), 50000)
})

test_that("Data Type Consistency - Performance Rating Types", {
  performance_data <- create_mock_performance_data(50)
  
  # All satisfaction ratings should be numeric
  satisfaction_cols <- c("EnvironmentSatisfaction", "JobSatisfaction", 
                        "RelationshipSatisfaction", "WorkLifeBalance")
  
  for (col in satisfaction_cols) {
    expect_true(is.numeric(performance_data[[col]]), 
                info = paste("Column", col, "should be numeric"))
  }
  
  # Rating columns
  rating_cols <- c("SelfRating", "ManagerRating")
  for (col in rating_cols) {
    expect_true(is.numeric(performance_data[[col]]),
                info = paste("Column", col, "should be numeric"))
  }
})

test_that("Data Type Consistency - Boolean Field Handling", {
  # Test various boolean representations
  boolean_variations <- data.frame(
    Overtime1 = c("Yes", "No", "YES", "NO", "yes", "no"),
    Overtime2 = c("True", "False", "TRUE", "FALSE", "true", "false"),
    Overtime3 = c("1", "0", "1", "0", "1", "0"),
    Attrition1 = c("Y", "N", "y", "n", "Yes", "No"),
    stringsAsFactors = FALSE
  )
  
  # Test standardization function behavior
  standardize_boolean <- function(x) {
    toupper(trimws(x)) %in% c("YES", "Y", "TRUE", "1")
  }
  
  expect_equal(standardize_boolean(boolean_variations$Overtime1), 
               c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(standardize_boolean(boolean_variations$Overtime2), 
               c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(standardize_boolean(boolean_variations$Overtime3), 
               c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
})

# =============================================================================
# 5.1.3 RANGE AND CONSTRAINT VALIDATION TESTS
# =============================================================================

test_that("Range Validation - Age Constraints", {
  employee_data <- create_mock_employee_data(100, introduce_issues = TRUE)
  
  # Valid age range (18-65)
  valid_ages <- employee_data$Age[employee_data$Age >= 18 & employee_data$Age <= 65 & !is.na(employee_data$Age)]
  invalid_ages <- employee_data$Age[employee_data$Age < 18 | employee_data$Age > 65 | is.na(employee_data$Age)]
  
  expect_true(length(valid_ages) > 0)
  expect_true(all(valid_ages >= 18 & valid_ages <= 65))
  
  # Test specific invalid cases
  test_ages <- c(-5, 0, 17, 150, 999, NA)
  age_validation <- function(age) {
    if (is.na(age)) return("missing")
    if (age < 18) return("too_young")
    if (age > 65) return("too_old")
    return("valid")
  }
  
  expect_equal(age_validation(-5), "too_young")
  expect_equal(age_validation(150), "too_old")
  expect_equal(age_validation(NA), "missing")
  expect_equal(age_validation(30), "valid")
})

test_that("Range Validation - Salary Constraints", {
  # Test salary ranges
  test_salaries <- c(-50000, 0, 15000, 30000, 150000, 500000, 5000000)
  
  salary_validation <- function(salary) {
    if (is.na(salary)) return("missing")
    if (salary < 0) return("negative")
    if (salary < 20000) return("too_low")
    if (salary > 300000) return("too_high")
    return("valid")
  }
  
  expect_equal(salary_validation(-50000), "negative")
  expect_equal(salary_validation(15000), "too_low")
  expect_equal(salary_validation(5000000), "too_high")
  expect_equal(salary_validation(75000), "valid")
})

test_that("Range Validation - Satisfaction Ratings (1-5 Scale)", {
  performance_data <- create_mock_performance_data(100, introduce_issues = TRUE)
  
  satisfaction_cols <- c("EnvironmentSatisfaction", "JobSatisfaction", 
                        "RelationshipSatisfaction", "WorkLifeBalance")
  
  for (col in satisfaction_cols) {
    valid_ratings <- performance_data[[col]][performance_data[[col]] >= 1 & 
                                            performance_data[[col]] <= 5 & 
                                            !is.na(performance_data[[col]])]
    
    expect_true(all(valid_ratings %in% 1:5),
                info = paste("All valid ratings in", col, "should be 1-5"))
  }
  
  # Test specific invalid ratings
  rating_validation <- function(rating) {
    if (is.na(rating)) return("missing")
    if (rating < 1) return("below_scale")
    if (rating > 5) return("above_scale")
    if (rating != floor(rating)) return("not_integer")
    return("valid")
  }
  
  expect_equal(rating_validation(0), "below_scale")
  expect_equal(rating_validation(6), "above_scale")
  expect_equal(rating_validation(3.5), "not_integer")
  expect_equal(rating_validation(3), "valid")
})

test_that("Range Validation - Years-Based Constraints", {
  employee_data <- create_mock_employee_data(100)
  
  # Years at company should not exceed age - 18
  invalid_years <- which(employee_data$YearsAtCompany > (employee_data$Age - 18))
  
  if (length(invalid_years) > 0) {
    expect_true(length(invalid_years) < nrow(employee_data) * 0.1) # Less than 10% invalid
  }
  
  # Years in role should not exceed years at company
  role_exceeds_company <- which(employee_data$YearsInMostRecentRole > employee_data$YearsAtCompany)
  expect_equal(length(role_exceeds_company), 0,
               info = "Years in role should not exceed years at company")
  
  # Years with manager should not exceed years in role
  manager_exceeds_role <- which(employee_data$YearsWithCurrManager > employee_data$YearsInMostRecentRole)
  expect_equal(length(manager_exceeds_role), 0,
               info = "Years with manager should not exceed years in role")
})

test_that("Range Validation - Distance From Home", {
  # Test realistic distance constraints
  test_distances <- c(-10, 0, 1, 25, 50, 100, 1000, 5000)
  
  distance_validation <- function(distance) {
    if (is.na(distance)) return("missing")
    if (distance < 0) return("negative")
    if (distance > 200) return("unrealistic")
    return("valid")
  }
  
  expect_equal(distance_validation(-10), "negative")
  expect_equal(distance_validation(1000), "unrealistic")
  expect_equal(distance_validation(25), "valid")
})

# =============================================================================
# 5.1.4 REFERENTIAL INTEGRITY CHECKING TESTS
# =============================================================================

test_that("Referential Integrity - Employee-Performance Relationship", {
  employee_data <- create_mock_employee_data(100)
  performance_data <- create_mock_performance_data(150)
  
  # All EmployeeIDs in performance should exist in employee data
  employee_ids <- employee_data$EmployeeID
  performance_employee_ids <- unique(performance_data$EmployeeID)
  
  missing_employees <- setdiff(performance_employee_ids, employee_ids)
  orphaned_performance <- length(missing_employees)
  
  expect_equal(orphaned_performance, 0,
               info = "All performance records should have corresponding employee records")
  
  # Test with intentionally broken referential integrity
  broken_performance <- performance_data
  broken_performance$EmployeeID[1:5] <- c(999, 1001, 1002, 1003, 1004) # Non-existent IDs
  
  missing_in_broken <- setdiff(unique(broken_performance$EmployeeID), employee_ids)
  expect_gt(length(missing_in_broken), 0,
            info = "Should detect orphaned performance records")
})

test_that("Referential Integrity - Education Level Relationships", {
  employee_data <- create_mock_employee_data(100)
  education_data <- create_mock_education_data()
  
  # All Education IDs in employee data should exist in education_data
  employee_education_ids <- unique(employee_data$Education)
  valid_education_ids <- education_data$EducationLevelID
  
  invalid_education_refs <- setdiff(employee_education_ids, valid_education_ids)
  expect_equal(length(invalid_education_refs), 0,
               info = "All education references should be valid")
  
  # Test with invalid education references
  broken_employee <- employee_data
  broken_employee$Education[1:3] <- c(99, 100, 101) # Non-existent education levels
  
  invalid_refs <- setdiff(unique(broken_employee$Education), valid_education_ids)
  expect_gt(length(invalid_refs), 0,
            info = "Should detect invalid education level references")
})

test_that("Referential Integrity - Foreign Key Constraint Validation", {
  # Test comprehensive foreign key validation function
  validate_foreign_keys <- function(child_data, child_key, parent_data, parent_key) {
    child_keys <- unique(child_data[[child_key]][!is.na(child_data[[child_key]])])
    parent_keys <- parent_data[[parent_key]]
    
    missing_keys <- setdiff(child_keys, parent_keys)
    
    list(
      valid = length(missing_keys) == 0,
      missing_keys = missing_keys,
      missing_count = length(missing_keys),
      total_child_keys = length(child_keys)
    )
  }
  
  employee_data <- create_mock_employee_data(50)
  performance_data <- create_mock_performance_data(75)
  education_data <- create_mock_education_data()
  
  # Test employee-performance relationship
  emp_perf_validation <- validate_foreign_keys(
    performance_data, "EmployeeID", employee_data, "EmployeeID"
  )
  
  expect_true(emp_perf_validation$valid)
  expect_equal(emp_perf_validation$missing_count, 0)
  
  # Test employee-education relationship
  emp_edu_validation <- validate_foreign_keys(
    employee_data, "Education", education_data, "EducationLevelID"
  )
  
  expect_true(emp_edu_validation$valid)
  expect_equal(emp_edu_validation$missing_count, 0)
})

# =============================================================================
# 5.1.5 DUPLICATE DETECTION ACCURACY TESTS
# =============================================================================

test_that("Duplicate Detection - Exact Duplicates", {
  employee_data <- create_mock_employee_data(100)
  
  # Create exact duplicates
  duplicate_rows <- employee_data[c(1, 5, 10), ]
  data_with_duplicates <- rbind(employee_data, duplicate_rows)
  
  # Test duplicate detection
  duplicate_indices <- which(duplicated(data_with_duplicates))
  expect_equal(length(duplicate_indices), 3)
  
  # Test by EmployeeID (should be unique)
  id_duplicates <- which(duplicated(data_with_duplicates$EmployeeID))
  expect_equal(length(id_duplicates), 3)
})

test_that("Duplicate Detection - Partial Duplicates (Same Person, Different Records)", {
  # Test duplicates based on name and demographic info
  employee_data <- create_mock_employee_data(50)
  
  # Create partial duplicates (same person, different employee ID)
  partial_duplicate <- employee_data[1, ]
  partial_duplicate$EmployeeID <- 999
  partial_duplicate$Salary <- 75000 # Different salary
  
  combined_data <- rbind(employee_data, partial_duplicate)
  
  # Test duplicate detection by name and demographics
  potential_duplicates <- combined_data %>%
    group_by(FirstName, LastName, Age, Gender) %>%
    filter(n() > 1) %>%
    ungroup()
  
  expect_gt(nrow(potential_duplicates), 0,
            info = "Should detect potential duplicate persons")
})

test_that("Duplicate Detection - EmployeeID Uniqueness", {
  employee_data <- create_mock_employee_data(100)
  
  # Test unique EmployeeID constraint
  expect_equal(length(employee_data$EmployeeID), length(unique(employee_data$EmployeeID)),
               info = "EmployeeID should be unique")
  
  # Test with duplicate IDs
  duplicate_id_data <- employee_data
  duplicate_id_data$EmployeeID[50] <- duplicate_id_data$EmployeeID[1]
  
  duplicate_ids <- duplicate_id_data$EmployeeID[duplicated(duplicate_id_data$EmployeeID)]
  expect_gt(length(duplicate_ids), 0,
            info = "Should detect duplicate EmployeeIDs")
})

test_that("Duplicate Detection - Performance Record Duplicates", {
  performance_data <- create_mock_performance_data(100)
  
  # Test for duplicate performance reviews (same employee, same date)
  duplicate_reviews <- performance_data %>%
    group_by(EmployeeID, ReviewDate) %>%
    filter(n() > 1) %>%
    ungroup()
  
  expect_equal(nrow(duplicate_reviews), 0,
               info = "Should not have duplicate reviews for same employee on same date")
  
  # Create intentional duplicates
  duplicate_review <- performance_data[1, ]
  duplicate_review$PerformanceID <- max(performance_data$PerformanceID) + 1
  
  data_with_dup_review <- rbind(performance_data, duplicate_review)
  
  duplicate_reviews_created <- data_with_dup_review %>%
    group_by(EmployeeID, ReviewDate) %>%
    filter(n() > 1) %>%
    ungroup()
  
  expect_gt(nrow(duplicate_reviews_created), 0,
            info = "Should detect duplicate review records")
})

# =============================================================================
# 5.1.6 MISSING VALUE HANDLING TESTS
# =============================================================================

test_that("Missing Value Detection - Critical Fields", {
  employee_data <- create_mock_employee_data(100, introduce_issues = TRUE)
  
  critical_fields <- c("EmployeeID", "FirstName", "LastName", "Department", "JobRole")
  
  for (field in critical_fields) {
    missing_count <- sum(is.na(employee_data[[field]]))
    missing_percentage <- missing_count / nrow(employee_data)
    
    if (field == "EmployeeID") {
      expect_equal(missing_count, 0, 
                   info = paste("EmployeeID should never be missing"))
    } else {
      expect_lt(missing_percentage, 0.05,
                info = paste("Missing percentage for", field, "should be less than 5%"))
    }
  }
})

test_that("Missing Value Detection - Optional vs Required Fields", {
  employee_data <- create_mock_employee_data(100)
  
  # Define field requirements
  required_fields <- c("EmployeeID", "FirstName", "LastName", "Age", "Department", 
                      "JobRole", "Salary", "HireDate", "Attrition")
  optional_fields <- c("DistanceFromHome", "YearsSinceLastPromotion")
  
  # Test required fields
  for (field in required_fields) {
    if (field %in% names(employee_data)) {
      missing_count <- sum(is.na(employee_data[[field]]))
      expect_equal(missing_count, 0,
                   info = paste("Required field", field, "should not have missing values"))
    }
  }
  
  # Optional fields can have missing values but should be flagged
  for (field in optional_fields) {
    if (field %in% names(employee_data)) {
      missing_count <- sum(is.na(employee_data[[field]]))
      missing_percentage <- missing_count / nrow(employee_data)
      
      # Should not exceed 20% missing for optional fields
      expect_lt(missing_percentage, 0.20,
                info = paste("Optional field", field, "missing percentage should be reasonable"))
    }
  }
})

test_that("Missing Value Patterns - Systematic Missing Data", {
  # Create data with systematic missing patterns
  employee_data <- create_mock_employee_data(100)
  
  # Simulate systematic missing data (e.g., all part-time employees missing overtime data)
  systematic_missing <- employee_data
  systematic_missing$OverTime[systematic_missing$Department == "Human Resources"] <- NA
  
  # Test for systematic missing patterns
  missing_by_dept <- systematic_missing %>%
    group_by(Department) %>%
    summarise(
      missing_overtime = sum(is.na(OverTime)),
      total = n(),
      missing_rate = missing_overtime / total
    )
  
  hr_missing_rate <- missing_by_dept$missing_rate[missing_by_dept$Department == "Human Resources"]
  expect_equal(hr_missing_rate, 1.0,
               info = "Should detect systematic missing data patterns")
})

test_that("Missing Value Handling - Empty String vs NA", {
  # Test different representations of missing data
  mixed_missing <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("John", "", NA, "   ", "Jane"),
    LastName = c("Smith", "Johnson", NA, "", "Doe"),
    Department = c("Sales", "N/A", "Unknown", "", "IT"),
    stringsAsFactors = FALSE
  )
  
  # Function to standardize missing values
  standardize_missing <- function(x) {
    if (is.character(x)) {
      x[trimws(x) == "" | toupper(trimws(x)) %in% c("N/A", "UNKNOWN", "NULL")] <- NA
    }
    return(x)
  }
  
  standardized_names <- standardize_missing(mixed_missing$FirstName)
  standardized_dept <- standardize_missing(mixed_missing$Department)
  
  expect_equal(sum(is.na(standardized_names)), 3) # "", NA, "   " should be NA
  expect_equal(sum(is.na(standardized_dept)), 3)  # "N/A", "Unknown", "" should be NA
})

# =============================================================================
# 5.1.7 OUTLIER IDENTIFICATION TESTS
# =============================================================================

test_that("Outlier Detection - Statistical Outliers (IQR Method)", {
  employee_data <- create_mock_employee_data(1000)
  
  # Test salary outliers using IQR method
  detect_outliers_iqr <- function(x) {
    Q1 <- quantile(x,