# =============================================================================
# Atlas Labs HR Analytics Dashboard - Comprehensive Utility Functions Tests
# =============================================================================
# Author: akhapwoyaco
# Purpose: Unit tests for all utility functions used in the HR Analytics app
# Coverage: Data validation, helpers, themes, calculations, and statistical functions
# =============================================================================

library(testthat)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Load utility functions (assuming they're in utils.R)
source("utils.R")
source("custom_theme.R")

# =============================================================================
# 1. DATA VALIDATION FUNCTIONS TESTING
# =============================================================================

test_that("validate_employee_data works correctly", {
  # Valid employee data
  valid_data <- data.frame(
    EmployeeID = c(1, 2, 3),
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Age = c(25, 30, 35),
    Salary = c(50000, 60000, 70000),
    HireDate = as.Date(c("2020-01-01", "2019-06-15", "2021-03-10")),
    stringsAsFactors = FALSE
  )
  
  expect_true(validate_employee_data(valid_data))
  
  # Invalid data - missing required columns
  invalid_data_1 <- data.frame(
    EmployeeID = c(1, 2, 3),
    FirstName = c("John", "Jane", "Bob")
  )
  
  expect_false(validate_employee_data(invalid_data_1))
  
  # Invalid data - wrong data types
  invalid_data_2 <- data.frame(
    EmployeeID = c("A", "B", "C"),  # Should be numeric
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Age = c("25", "30", "35"),  # Should be numeric
    Salary = c(50000, 60000, 70000),
    HireDate = c("2020-01-01", "2019-06-15", "2021-03-10")  # Should be Date
  )
  
  expect_false(validate_employee_data(invalid_data_2))
  
  # Invalid data - negative values where not allowed
  invalid_data_3 <- valid_data
  invalid_data_3$Age[1] <- -5
  invalid_data_3$Salary[2] <- -1000
  
  expect_false(validate_employee_data(invalid_data_3))
})

test_that("validate_performance_data works correctly", {
  # Valid performance data
  valid_perf_data <- data.frame(
    PerformanceID = c(1, 2, 3),
    EmployeeID = c(1, 2, 3),
    ReviewDate = as.Date(c("2023-01-15", "2023-02-15", "2023-03-15")),
    EnvironmentSatisfaction = c(4, 3, 5),
    JobSatisfaction = c(4, 4, 3),
    WorkLifeBalance = c(3, 4, 4),
    SelfRating = c(4, 3, 4),
    ManagerRating = c(4, 4, 3),
    stringsAsFactors = FALSE
  )
  
  expect_true(validate_performance_data(valid_perf_data))
  
  # Invalid - ratings out of range (1-5)
  invalid_perf_data <- valid_perf_data
  invalid_perf_data$JobSatisfaction[1] <- 6
  invalid_perf_data$SelfRating[2] <- 0
  
  expect_false(validate_performance_data(invalid_perf_data))
})

test_that("validate_file_format works correctly", {
  # Mock file validation
  expect_true(validate_file_format("test.csv", "csv"))
  expect_true(validate_file_format("data.xlsx", "xlsx"))
  expect_false(validate_file_format("document.txt", "csv"))
  expect_false(validate_file_format("", "csv"))
  expect_false(validate_file_format(NULL, "csv"))
})

test_that("check_data_completeness works correctly", {
  complete_data <- data.frame(
    A = c(1, 2, 3),
    B = c("a", "b", "c"),
    C = c(TRUE, FALSE, TRUE)
  )
  
  incomplete_data <- data.frame(
    A = c(1, NA, 3),
    B = c("a", "b", NA),
    C = c(TRUE, FALSE, TRUE)
  )
  
  expect_equal(check_data_completeness(complete_data), 100)
  expect_lt(check_data_completeness(incomplete_data), 100)
  expect_gte(check_data_completeness(incomplete_data), 0)
})

# =============================================================================
# 2. HELPER FUNCTION ACCURACY TESTING
# =============================================================================

test_that("calculate_attrition_rate works correctly", {
  # Test data
  employee_data <- data.frame(
    EmployeeID = 1:10,
    Attrition = c(rep("Yes", 3), rep("No", 7))
  )
  
  expected_rate <- 0.3  # 3 out of 10
  actual_rate <- calculate_attrition_rate(employee_data)
  
  expect_equal(actual_rate, expected_rate)
  
  # Edge case - no attrition
  no_attrition_data <- data.frame(
    EmployeeID = 1:5,
    Attrition = rep("No", 5)
  )
  
  expect_equal(calculate_attrition_rate(no_attrition_data), 0)
  
  # Edge case - all attrition
  all_attrition_data <- data.frame(
    EmployeeID = 1:5,
    Attrition = rep("Yes", 5)
  )
  
  expect_equal(calculate_attrition_rate(all_attrition_data), 1)
})

test_that("generate_age_groups works correctly", {
  ages <- c(22, 28, 35, 42, 55, 63)
  expected_groups <- c("20-29", "20-29", "30-39", "40-49", "50-59", "60+")
  
  actual_groups <- generate_age_groups(ages)
  expect_equal(actual_groups, expected_groups)
  
  # Edge cases
  expect_equal(generate_age_groups(c(20, 29, 30)), c("20-29", "20-29", "30-39"))
  expect_equal(generate_age_groups(c(65, 70)), c("60+", "60+"))
})

test_that("calculate_tenure_years works correctly", {
  hire_dates <- as.Date(c("2020-01-01", "2018-06-15", "2021-12-01"))
  reference_date <- as.Date("2023-01-01")
  
  expected_tenure <- c(3, 4.5, 1.08)  # Approximate values
  actual_tenure <- calculate_tenure_years(hire_dates, reference_date)
  
  expect_equal(length(actual_tenure), 3)
  expect_true(all(actual_tenure >= 0))
  expect_equal(round(actual_tenure[1]), 3)
  expect_equal(round(actual_tenure[2]), 5)
})

test_that("format_currency works correctly", {
  values <- c(50000, 75000.5, 100000)
  
  formatted <- format_currency(values)
  expect_equal(formatted, c("$50,000", "$75,001", "$100,000"))
  
  # With decimals
  formatted_decimal <- format_currency(values, decimals = TRUE)
  expect_equal(formatted_decimal, c("$50,000.00", "$75,000.50", "$100,000.00"))
  
  # Edge cases
  expect_equal(format_currency(0), "$0")
  expect_equal(format_currency(1000000), "$1,000,000")
})

test_that("safe_division works correctly", {
  expect_equal(safe_division(10, 2), 5)
  expect_equal(safe_division(10, 0), 0)  # Default for division by zero
  expect_equal(safe_division(10, 0, default = NA), NA)
  expect_equal(safe_division(0, 5), 0)
})

# =============================================================================
# 3. CUSTOM THEME APPLICATIONS TESTING
# =============================================================================

test_that("atlas_theme creates valid ggplot2 theme", {
  theme_obj <- atlas_theme()
  
  expect_s3_class(theme_obj, "theme")
  expect_s3_class(theme_obj, "gg")
  
  # Test that theme can be applied to a plot
  p <- ggplot(mtcars, aes(mpg, hp)) + 
    geom_point() + 
    atlas_theme()
  
  expect_s3_class(p, "ggplot")
})

test_that("atlas_theme_dark creates valid dark theme", {
  dark_theme <- atlas_theme_dark()
  
  expect_s3_class(dark_theme, "theme")
  expect_s3_class(dark_theme, "gg")
  
  # Test dark theme application
  p_dark <- ggplot(mtcars, aes(mpg, hp)) + 
    geom_point() + 
    atlas_theme_dark()
  
  expect_s3_class(p_dark, "ggplot")
})

test_that("atlas_theme_minimal creates valid minimal theme", {
  minimal_theme <- atlas_theme_minimal()
  
  expect_s3_class(minimal_theme, "theme")
  
  # Test that it produces different output than base theme
  base_theme <- atlas_theme()
  expect_false(identical(minimal_theme, base_theme))
})

# =============================================================================
# 4. COLOR PALETTE CONSISTENCY TESTING
# =============================================================================

test_that("atlas_colors returns consistent color palette", {
  colors <- atlas_colors()
  
  expect_type(colors, "character")
  expect_true(length(colors) >= 5)  # Should have at least 5 colors
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))  # Valid hex colors
})

test_that("atlas_colors_categorical works correctly", {
  cat_colors <- atlas_colors_categorical(3)
  
  expect_length(cat_colors, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cat_colors)))
  
  # Test edge cases
  expect_length(atlas_colors_categorical(1), 1)
  expect_error(atlas_colors_categorical(0))
  expect_error(atlas_colors_categorical(-1))
})

test_that("atlas_colors_sequential works correctly", {
  seq_colors <- atlas_colors_sequential(5)
  
  expect_length(seq_colors, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", seq_colors)))
  
  # Colors should be ordered (gradient-like)
  # This is a simplified test - in practice you'd check color intensity
  expect_true(length(unique(seq_colors)) == length(seq_colors))
})

test_that("validate_color_contrast works correctly", {
  # High contrast (should pass)
  expect_true(validate_color_contrast("#FFFFFF", "#000000"))
  expect_true(validate_color_contrast("#000000", "#FFFFFF"))
  
  # Low contrast (should fail)
  expect_false(validate_color_contrast("#FFFFFF", "#FFFFEE"))
  expect_false(validate_color_contrast("#000000", "#111111"))
})

test_that("get_color_by_value works correctly", {
  values <- c(1, 5, 10)
  colors <- get_color_by_value(values, c("#FF0000", "#00FF00", "#0000FF"))
  
  expect_length(colors, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  
  # Test single value
  single_color <- get_color_by_value(5, c("#FF0000", "#0000FF"))
  expect_length(single_color, 1)
})

# =============================================================================
# 5. DATE/TIME PROCESSING TESTING
# =============================================================================

test_that("standardize_date_format works correctly", {
  # Various date formats
  dates_mixed <- c("2023-01-15", "01/15/2023", "15-Jan-2023", "2023/01/15")
  standardized <- standardize_date_format(dates_mixed)
  
  expect_true(all(class(standardized) == "Date"))
  expect_true(all(!is.na(standardized)))
  
  # Invalid dates should return NA
  invalid_dates <- c("invalid-date", "2023-13-01", "")
  standardized_invalid <- standardize_date_format(invalid_dates)
  expect_true(all(is.na(standardized_invalid)))
})

test_that("calculate_age_from_birthdate works correctly", {
  birthdates <- as.Date(c("1990-01-01", "1985-06-15", "2000-12-31"))
  reference_date <- as.Date("2023-01-01")
  
  ages <- calculate_age_from_birthdate(birthdates, reference_date)
  
  expect_equal(ages, c(33, 37, 22))
  expect_true(all(ages >= 0))
  expect_type(ages, "double")
})

test_that("get_fiscal_year works correctly", {
  # Assuming fiscal year starts April 1st
  dates <- as.Date(c("2023-03-31", "2023-04-01", "2023-12-31", "2024-03-31"))
  fiscal_years <- get_fiscal_year(dates, start_month = 4)
  
  expected <- c(2022, 2023, 2023, 2023)  # Fiscal years
  expect_equal(fiscal_years, expected)
})

test_that("calculate_business_days works correctly", {
  start_date <- as.Date("2023-01-01")  # Sunday
  end_date <- as.Date("2023-01-07")    # Saturday
  
  # Should be 5 business days (Mon-Fri)
  business_days <- calculate_business_days(start_date, end_date)
  expect_equal(business_days, 5)
  
  # Same day
  expect_equal(calculate_business_days(start_date, start_date), 0)
})

test_that("format_duration works correctly", {
  durations <- c(1, 30, 365, 1000)
  
  formatted <- format_duration(durations, "days")
  expect_equal(formatted, c("1 day", "30 days", "1 year", "2.7 years"))
  
  # Test different units
  formatted_years <- format_duration(c(2.5, 5.0), "years")
  expect_equal(formatted_years, c("2.5 years", "5 years"))
})

# =============================================================================
# 6. STRING MANIPULATION UTILITIES TESTING
# =============================================================================

test_that("clean_text works correctly", {
  dirty_text <- c("  John Doe  ", "JANE SMITH", "bob_johnson", "")
  cleaned <- clean_text(dirty_text)
  
  expect_equal(cleaned, c("John Doe", "Jane Smith", "Bob Johnson", ""))
  
  # Test special characters
  special_text <- c("John O'Connor", "Mary-Jane", "José García")
  cleaned_special <- clean_text(special_text)
  expect_equal(cleaned_special, c("John O'Connor", "Mary-Jane", "José García"))
})

test_that("extract_initials works correctly", {
  names <- c("John Doe", "Mary Jane Smith", "Bob", "")
  initials <- extract_initials(names)
  
  expect_equal(initials, c("JD", "MJS", "B", ""))
  
  # Test with middle names and special characters
  complex_names <- c("John F. Kennedy", "Mary-Jane O'Connor")
  complex_initials <- extract_initials(complex_names)
  expect_equal(complex_initials, c("JFK", "MJO"))
})

test_that("normalize_department_names works correctly", {
  dept_names <- c("human resources", "INFORMATION TECHNOLOGY", "Sales & Marketing")
  normalized <- normalize_department_names(dept_names)
  
  expect_equal(normalized, c("Human Resources", "Information Technology", "Sales & Marketing"))
})

test_that("validate_email_format works correctly", {
  valid_emails <- c("john.doe@company.com", "user123@domain.org")
  invalid_emails <- c("invalid-email", "@domain.com", "user@", "")
  
  expect_true(all(validate_email_format(valid_emails)))
  expect_false(any(validate_email_format(invalid_emails)))
})

test_that("generate_employee_code works correctly", {
  first_names <- c("John", "Mary")
  last_names <- c("Doe", "Smith")
  
  codes <- generate_employee_code(first_names, last_names)
  expect_equal(codes, c("JODO", "MASM"))
  
  # Test with numbers for uniqueness
  codes_with_nums <- generate_employee_code(first_names, last_names, use_numbers = TRUE)
  expect_true(all(grepl("^[A-Z]{4}[0-9]+$", codes_with_nums)))
})

# =============================================================================
# 7. MATHEMATICAL CALCULATIONS TESTING
# =============================================================================

test_that("calculate_percentile works correctly", {
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  expect_equal(calculate_percentile(data, 50), 5.5)  # Median
  expect_equal(calculate_percentile(data, 25), 3.25)  # Q1
  expect_equal(calculate_percentile(data, 75), 7.75)  # Q3
  
  # Edge cases
  expect_equal(calculate_percentile(data, 0), 1)
  expect_equal(calculate_percentile(data, 100), 10)
})

test_that("calculate_coefficient_of_variation works correctly", {
  data1 <- c(10, 10, 10, 10)  # No variation
  data2 <- c(1, 2, 3, 4, 5)   # Some variation
  
  expect_equal(calculate_coefficient_of_variation(data1), 0)
  expect_gt(calculate_coefficient_of_variation(data2), 0)
  expect_lt(calculate_coefficient_of_variation(data2), 1)  # Should be reasonable
})

test_that("calculate_growth_rate works correctly", {
  old_value <- 100
  new_value <- 150
  
  growth_rate <- calculate_growth_rate(old_value, new_value)
  expect_equal(growth_rate, 0.5)  # 50% growth
  
  # Negative growth
  decline_rate <- calculate_growth_rate(150, 100)
  expect_equal(round(decline_rate, 3), -0.333)
  
  # Zero division handling
  expect_equal(calculate_growth_rate(0, 100), Inf)
})

test_that("normalize_values works correctly", {
  values <- c(10, 20, 30, 40, 50)
  normalized <- normalize_values(values)
  
  expect_equal(min(normalized), 0)
  expect_equal(max(normalized), 1)
  expect_equal(length(normalized), 5)
  
  # Test with identical values
  identical_values <- c(5, 5, 5, 5)
  normalized_identical <- normalize_values(identical_values)
  expect_true(all(normalized_identical == 0))  # All should be 0 when no variation
})

test_that("calculate_weighted_average works correctly", {
  values <- c(10, 20, 30)
  weights <- c(1, 2, 3)
  
  weighted_avg <- calculate_weighted_average(values, weights)
  expected <- (10*1 + 20*2 + 30*3) / (1+2+3)  # 23.33...
  
  expect_equal(round(weighted_avg, 2), round(expected, 2))
  
  # Equal weights should equal regular average
  equal_weights <- c(1, 1, 1)
  equal_weighted <- calculate_weighted_average(values, equal_weights)
  regular_avg <- mean(values)
  
  expect_equal(equal_weighted, regular_avg)
})

# =============================================================================
# 8. STATISTICAL FUNCTIONS TESTING
# =============================================================================

test_that("calculate_correlation_matrix works correctly", {
  data <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(2, 4, 6, 8, 10),
    C = c(5, 4, 3, 2, 1)
  )
  
  corr_matrix <- calculate_correlation_matrix(data)
  
  expect_equal(dim(corr_matrix), c(3, 3))
  expect_equal(diag(corr_matrix), c(1, 1, 1))  # Diagonal should be 1
  expect_equal(corr_matrix[1, 2], 1)  # Perfect positive correlation
  expect_equal(corr_matrix[1, 3], -1)  # Perfect negative correlation
})

test_that("perform_t_test works correctly", {
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(6, 7, 8, 9, 10)
  
  t_test_result <- perform_t_test(group1, group2)
  
  expect_true("p_value" %in% names(t_test_result))
  expect_true("t_statistic" %in% names(t_test_result))
  expect_true("significant" %in% names(t_test_result))
  
  expect_true(t_test_result$p_value < 0.05)  # Should be significant
  expect_true(t_test_result$significant)
})

test_that("calculate_confidence_interval works correctly", {
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  ci_95 <- calculate_confidence_interval(data, confidence = 0.95)
  ci_99 <- calculate_confidence_interval(data, confidence = 0.99)
  
  expect_true("lower" %in% names(ci_95))
  expect_true("upper" %in% names(ci_95))
  expect_true("mean" %in% names(ci_95))
  
  # 99% CI should be wider than 95% CI
  expect_gt(ci_99$upper - ci_99$lower, ci_95$upper - ci_95$lower)
})

test_that("perform_chi_square_test works correctly", {
  # Create contingency table data
  observed <- matrix(c(10, 15, 20, 25), nrow = 2)
  
  chi_result <- perform_chi_square_test(observed)
  
  expect_true("p_value" %in% names(chi_result))
  expect_true("chi_square" %in% names(chi_result))
  expect_true("significant" %in% names(chi_result))
  expect_true("degrees_freedom" %in% names(chi_result))
})

test_that("calculate_effect_size works correctly", {
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(3, 4, 5, 6, 7)
  
  effect_size <- calculate_effect_size(group1, group2)
  
  expect_type(effect_size, "double")
  expect_true(is.finite(effect_size))
  expect_gt(abs(effect_size), 0)  # Should have some effect
})

test_that("detect_outliers works correctly", {
  # Data with obvious outliers
  data_with_outliers <- c(1, 2, 3, 4, 5, 100)  # 100 is an outlier
  
  outliers <- detect_outliers(data_with_outliers, method = "iqr")
  expect_true(100 %in% outliers)
  
  # Data without outliers
  normal_data <- c(1, 2, 3, 4, 5)
  no_outliers <- detect_outliers(normal_data, method = "iqr")
  expect_equal(length(no_outliers), 0)
})

test_that("calculate_skewness works correctly", {
  # Normal distribution (should be close to 0)
  normal_data <- rnorm(1000, mean = 0, sd = 1)
  skewness_normal <- calculate_skewness(normal_data)
  expect_lt(abs(skewness_normal), 0.2)  # Should be close to 0
  
  # Right-skewed data
  right_skewed <- c(rep(1, 10), rep(2, 5), rep(10, 1))
  skewness_right <- calculate_skewness(right_skewed)
  expect_gt(skewness_right, 0)
  
  # Left-skewed data  
  left_skewed <- c(rep(10, 1), rep(8, 5), rep(7, 10))
  skewness_left <- calculate_skewness(left_skewed)
  expect_lt(skewness_left, 0)
})

# =============================================================================
# PERFORMANCE AND EDGE CASE TESTING
# =============================================================================

test_that("utility functions handle edge cases", {
  # Empty data
  expect_error(calculate_attrition_rate(data.frame()))
  expect_equal(clean_text(character(0)), character(0))
  expect_equal(format_currency(numeric(0)), character(0))
  
  # NA values
  expect_true(is.na(safe_division(NA, 5)))
  expect_true(is.na(calculate_age_from_birthdate(as.Date(NA), Sys.Date())))
  
  # Large datasets performance (basic check)
  large_data <- data.frame(
    values = rnorm(10000),
    categories = sample(letters[1:5], 10000, replace = TRUE)
  )
  
  # These should complete without error
  expect_no_error(normalize_values(large_data$values))
  expect_no_error(calculate_percentile(large_data$values, 50))
})

test_that("functions maintain data integrity", {
  # Test that functions don't modify input data unexpectedly
  original_data <- c(1, 2, 3, 4, 5)
  modified_data <- original_data
  
  normalize_values(modified_data)
  expect_equal(original_data, modified_data)  # Original should be unchanged
  
  # Test that string functions preserve character encoding
  unicode_text <- c("José", "François", "北京")
  cleaned_unicode <- clean_text(unicode_text)
  expect_equal(length(cleaned_unicode), length(unicode_text))
})

# =============================================================================
# INTEGRATION TESTING
# =============================================================================

test_that("functions work together correctly", {
  # Create sample employee data
  sample_employees <- data.frame(
    EmployeeID = 1:100,
    FirstName = paste("Employee", 1:100),
    LastName = paste("Surname", 1:100),
    Age = sample(22:65, 100, replace = TRUE),
    Salary = sample(40000:120000, 100, replace = TRUE),
    HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-01-01"), by = "day"), 100),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE
  )
  
  # Test data validation pipeline
  expect_true(validate_employee_data(sample_employees))
  
  # Test analysis pipeline
  attrition_rate <- calculate_attrition_rate(sample_employees)
  expect_gte(attrition_rate, 0)
  expect_lte(attrition_rate, 1)
  
  age_groups <- generate_age_groups(sample_employees$Age)
  expect_equal(length(age_groups), nrow(sample_employees))
  
  formatted_salaries <- format_currency(sample_employees$Salary)
  expect_equal(length(formatted_salaries), nrow(sample_employees))
  expect_true(all(grepl("^\\$", formatted_salaries)))
})

# =============================================================================
# SUMMARY REPORTING
# =============================================================================

# Run all tests and generate summary
test_results_summary <- function() {
  cat("=================================================================\n")
  cat("ATLAS LABS HR ANALYTICS - UTILITY FUNCTIONS TEST SUMMARY\n")
  cat("=================================================================\n")
  cat("Test Coverage Areas:\n")
  cat("✓ Data Validation Functions\n")
  cat("✓ Helper Function Accuracy\n")
  cat("✓ Custom Theme Applications\n")
  cat("✓ Color Palette Consistency\n")
  cat("✓ Date/Time Processing\n")
  cat("✓ String Manipulation Utilities\n")
  cat("✓ Mathematical Calculations\n")
  cat("✓ Statistical Functions\n")
  cat("✓ Performance & Edge Cases\n")
  cat("✓ Integration Testing\n")
  cat("=================================================================\n")
  cat("Total Test Functions: 25+\n")
  cat("Total Test Cases: 100+\n")
  cat("Coverage: Comprehensive utility function validation\n")
  cat("=================================================================\n")
}

# Print summary when tests are run
test_results_summary()