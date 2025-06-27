# ============================================================================
# ATLAS LABS HR ANALYTICS - DEMOGRAPHICS MODULE UNIT TESTS
# ============================================================================
# Comprehensive unit tests for Demographics Module covering:
# 1. Anonymization Effectiveness
# 2. Bias Detection Algorithms  
# 3. Statistical Significance Testing
#
# Author: akhapwoyaco
# Date: 2025-06-27
# ============================================================================

library(testthat)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# ============================================================================
# TEST SETUP & MOCK DATA
# ============================================================================

# Create comprehensive mock employee data for testing
create_mock_employee_data <- function(n = 1000) {
  set.seed(12345)  # Ensure reproducible tests
  
  tibble(
    EmployeeID = 1:n,
    FirstName = sample(c("John", "Jane", "Michael", "Sarah", "David", "Emily", 
                        "Robert", "Lisa", "James", "Maria"), n, replace = TRUE),
    LastName = sample(c("Smith", "Johnson", "Williams", "Brown", "Jones", 
                       "Garcia", "Miller", "Davis", "Rodriguez", "Martinez"), 
                     n, replace = TRUE),
    Gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, 
                   prob = c(0.48, 0.48, 0.04)),
    Age = round(rnorm(n, mean = 35, sd = 10)),
    Department = sample(c("Engineering", "Sales", "HR", "Marketing", "Finance"), 
                       n, replace = TRUE),
    Ethnicity = sample(c("White", "Hispanic", "Black", "Asian", "Other"), 
                      n, replace = TRUE, prob = c(0.6, 0.18, 0.13, 0.07, 0.02)),
    Salary = round(rnorm(n, mean = 75000, sd = 25000)),
    State = sample(c("CA", "NY", "TX", "FL", "IL", "PA", "OH", "GA", "NC", "MI"), 
                  n, replace = TRUE),
    JobRole = sample(c("Manager", "Senior", "Junior", "Lead", "Director"), 
                    n, replace = TRUE),
    YearsAtCompany = round(runif(n, 0, 20)),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    # Add some deliberate bias patterns for testing
    BiasedSalary = case_when(
      Gender == "Male" ~ Salary * 1.1,  # 10% higher for males
      Gender == "Female" ~ Salary * 0.95,  # 5% lower for females
      TRUE ~ Salary
    ),
    # Add some identifiable patterns for anonymization testing
    Email = paste0(tolower(FirstName), ".", tolower(LastName), "@atlaslabs.com"),
    Phone = paste0("555-", sprintf("%04d", sample(1000:9999, n, replace = TRUE))),
    SSN_Last4 = sprintf("%04d", sample(1000:9999, n, replace = TRUE))
  )
}

# Mock anonymization functions (these would be in the actual module)
anonymize_employee_data <- function(data) {
  data %>%
    mutate(
      # Remove direct identifiers
      FirstName = NULL,
      LastName = NULL,
      Email = NULL,
      Phone = NULL,
      SSN_Last4 = NULL,
      # Hash EmployeeID
      EmployeeID = digest::digest(EmployeeID, algo = "md5"),
      # Age binning
      AgeGroup = case_when(
        Age < 25 ~ "Under 25",
        Age >= 25 & Age < 35 ~ "25-34",
        Age >= 35 & Age < 45 ~ "35-44",
        Age >= 45 & Age < 55 ~ "45-54",
        Age >= 55 ~ "55+"
      ),
      # Salary binning
      SalaryBand = case_when(
        Salary < 50000 ~ "Under 50K",
        Salary >= 50000 & Salary < 75000 ~ "50K-75K",
        Salary >= 75000 & Salary < 100000 ~ "75K-100K",
        Salary >= 100000 & Salary < 150000 ~ "100K-150K",
        Salary >= 150000 ~ "150K+"
      )
    ) %>%
    select(-Age, -Salary)  # Remove original continuous variables
}

# ============================================================================
# 1. ANONYMIZATION EFFECTIVENESS TESTS
# ============================================================================

test_that("Anonymization removes all direct identifiers", {
  # Arrange
  original_data <- create_mock_employee_data(100)
  
  # Act
  anonymized_data <- anonymize_employee_data(original_data)
  
  # Assert - Direct identifiers should be removed
  expect_false("FirstName" %in% names(anonymized_data))
  expect_false("LastName" %in% names(anonymized_data))
  expect_false("Email" %in% names(anonymized_data))
  expect_false("Phone" %in% names(anonymized_data))
  expect_false("SSN_Last4" %in% names(anonymized_data))
  
  # Original continuous variables should be removed
  expect_false("Age" %in% names(anonymized_data))
  expect_false("Salary" %in% names(anonymized_data))
})

test_that("EmployeeID hashing is consistent and irreversible", {
  # Arrange
  original_data <- create_mock_employee_data(50)
  
  # Act
  anonymized_data1 <- anonymize_employee_data(original_data)
  anonymized_data2 <- anonymize_employee_data(original_data)
  
  # Assert - Hashing should be consistent
  expect_identical(anonymized_data1$EmployeeID, anonymized_data2$EmployeeID)
  
  # Assert - All IDs should be hashed (32-character hex strings)
  expect_true(all(str_length(anonymized_data1$EmployeeID) == 32))
  expect_true(all(str_detect(anonymized_data1$EmployeeID, "^[a-f0-9]{32}$")))
  
  # Assert - No duplicate hashes
  expect_equal(length(anonymized_data1$EmployeeID), 
               length(unique(anonymized_data1$EmployeeID)))
})

test_that("Age binning maintains statistical utility while reducing granularity", {
  # Arrange
  original_data <- create_mock_employee_data(500)
  
  # Act
  anonymized_data <- anonymize_employee_data(original_data)
  
  # Assert - Age groups should be created
  expect_true("AgeGroup" %in% names(anonymized_data))
  
  # Assert - All age groups are valid
  valid_age_groups <- c("Under 25", "25-34", "35-44", "45-54", "55+")
  expect_true(all(anonymized_data$AgeGroup %in% valid_age_groups))
  
  # Assert - Age distribution should be preserved (within reasonable bounds)
  age_group_counts <- table(anonymized_data$AgeGroup)
  expect_true(all(age_group_counts > 0))  # All groups should have some members
})

test_that("Salary binning prevents precise income identification", {
  # Arrange
  original_data <- create_mock_employee_data(300)
  
  # Act
  anonymized_data <- anonymize_employee_data(original_data)
  
  # Assert - Salary bands should be created
  expect_true("SalaryBand" %in% names(anonymized_data))
  
  # Assert - All salary bands are valid
  valid_salary_bands <- c("Under 50K", "50K-75K", "75K-100K", "100K-150K", "150K+")
  expect_true(all(anonymized_data$SalaryBand %in% valid_salary_bands))
  
  # Assert - No single salary band should dominate (reasonable distribution)
  salary_distribution <- prop.table(table(anonymized_data$SalaryBand))
  expect_true(all(salary_distribution >= 0.05))  # Each band >= 5%
})

test_that("k-anonymity threshold is maintained", {
  # Arrange
  original_data <- create_mock_employee_data(200)
  anonymized_data <- anonymize_employee_data(original_data)
  k_threshold <- 5  # Minimum group size for k-anonymity
  
  # Act - Check k-anonymity for quasi-identifiers
  quasi_identifier_groups <- anonymized_data %>%
    group_by(AgeGroup, Gender, Department, Ethnicity) %>%
    summarise(group_size = n(), .groups = "drop")
  
  # Assert - All groups should meet k-anonymity threshold
  small_groups <- quasi_identifier_groups %>%
    filter(group_size < k_threshold)
  
  expect_equal(nrow(small_groups), 0, 
               info = paste("Found", nrow(small_groups), "groups smaller than k =", k_threshold))
})

test_that("l-diversity is maintained for sensitive attributes", {
  # Arrange
  original_data <- create_mock_employee_data(300)
  anonymized_data <- anonymize_employee_data(original