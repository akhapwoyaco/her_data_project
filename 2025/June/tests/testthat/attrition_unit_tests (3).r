# =============================================================================
# ATLAS LABS HR ANALYTICS - ATTRITION ANALYSIS MODULE UNIT TESTS
# =============================================================================
# Comprehensive unit tests for statistical analysis components
# Focus areas: Correlations, Hypothesis Testing, Confidence Intervals, Seasonal Adjustment
# Developer: akhapwoyaco
# =============================================================================

# Load required libraries for testing
library(testthat)
library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(seasonal)
library(forecast)

# Source the attrition module (assumes it exists)
# source("modules/attrition_module.R")

# =============================================================================
# MOCK DATA GENERATORS FOR TESTING
# =============================================================================

#' Generate synthetic employee data for testing
#' @param n Number of employees to generate
#' @param seed Random seed for reproducibility
#' @return data.frame with employee data
generate_test_employee_data <- function(n = 1000, seed = 123) {
  set.seed(seed)
  
  data.frame(
    EmployeeID = 1:n,
    Age = sample(22:65, n, replace = TRUE),
    Salary = rnorm(n, 65000, 15000),
    YearsAtCompany = sample(0:30, n, replace = TRUE),
    YearsInRole = sample(0:15, n, replace = TRUE),
    YearsSincePromotion = sample(0:10, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)),
    Department = sample(c("Sales", "R&D", "HR", "IT", "Finance"), n, replace = TRUE),
    Gender = sample(c("Male", "Female"), n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    BusinessTravel = sample(c("None", "Rarely", "Frequently"), n, replace = TRUE),
    HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    stringsAsFactors = FALSE
  )
}

#' Generate time series attrition data for seasonal testing
#' @param start_date Start date for time series
#' @param end_date End date for time series
#' @param seasonal_pattern Whether to include seasonal pattern
#' @return data.frame with monthly attrition data
generate_seasonal_attrition_data <- function(start_date = "2018-01-01", 
                                           end_date = "2023-12-31", 
                                           seasonal_pattern = TRUE) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  n_months <- length(dates)
  
  # Base attrition rate
  base_rate <- 0.12
  
  if (seasonal_pattern) {
    # Add seasonal component (higher in Q1, Q4)
    seasonal_effect <- 0.05 * sin(2 * pi * (month(dates) - 1) / 12 + pi)
    trend_effect <- seq(0, 0.02, length.out = n_months)  # Slight upward trend
    noise <- rnorm(n_months, 0, 0.02)
    
    attrition_rate <- base_rate + seasonal_effect + trend_effect + noise
  } else {
    attrition_rate <- rep(base_rate, n_months) + rnorm(n_months, 0, 0.02)
  }
  
  # Ensure rates are within reasonable bounds
  attrition_rate <- pmax(0.01, pmin(0.30, attrition_rate))
  
  data.frame(
    Date = dates,
    Year = year(dates),
    Month = month(dates),
    Quarter = quarter(dates),
    AttritionRate = attrition_rate,
    TotalEmployees = sample(800:1200, n_months, replace = TRUE),
    Departures = round(attrition_rate * sample(800:1200, n_months, replace = TRUE))
  )
}

# =============================================================================
# ATTRITION ANALYSIS FUNCTIONS (Mock implementations for testing)
# =============================================================================

#' Calculate correlation matrix for attrition factors
#' @param data Employee data frame
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return List with correlation matrix and p-values
calculate_attrition_correlations <- function(data, method = "pearson") {
  # Convert categorical variables to numeric for correlation
  numeric_data <- data %>%
    mutate(
      Attrition_Binary = ifelse(Attrition == "Yes", 1, 0),
      OverTime_Binary = ifelse(OverTime == "Yes", 1, 0),
      Gender_Binary = ifelse(Gender == "Male", 1, 0),
      BusinessTravel_Numeric = case_when(
        BusinessTravel == "None" ~ 0,
        BusinessTravel == "Rarely" ~ 1,
        BusinessTravel == "Frequently" ~ 2
      )
    ) %>%
    select(
      Attrition_Binary, Age, Salary, YearsAtCompany, YearsInRole, 
      YearsSincePromotion, JobSatisfaction, WorkLifeBalance, 
      EnvironmentSatisfaction, DistanceFromHome, OverTime_Binary,
      Gender_Binary, BusinessTravel_Numeric
    )
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, method = method, use = "complete.obs")
  
  # Calculate p-values
  n <- nrow(numeric_data)
  p_values <- matrix(NA, nrow = ncol(numeric_data), ncol = ncol(numeric_data))
  
  for (i in 1:ncol(numeric_data)) {
    for (j in 1:ncol(numeric_data)) {
      if (i != j) {
        cor_test <- cor.test(numeric_data[, i], numeric_data[, j], method = method)
        p_values[i, j] <- cor_test$p.value
      } else {
        p_values[i, j] <- 0
      }
    }
  }
  
  rownames(p_values) <- colnames(p_values) <- colnames(numeric_data)
  
  list(
    correlation_matrix = cor_matrix,
    p_values = p_values,
    method = method,
    n_observations = n
  )
}

#' Perform hypothesis tests for attrition differences
#' @param data Employee data frame
#' @param grouping_var Variable to group by
#' @param test_type Type of test ("t.test", "wilcox.test", "chi.test")
#' @return List with test results
perform_attrition_hypothesis_tests <- function(data, grouping_var, test_type = "chi.test") {
  results <- list()
  
  if (test_type == "chi.test") {
    # Chi-square test for categorical variables
    contingency_table <- table(data[[grouping_var]], data$Attrition)
    chi_test <- chisq.test(contingency_table)
    
    results$test_type <- "Chi-square test"
    results$statistic <- chi_test$statistic
    results$p_value <- chi_test$p.value
    results$df <- chi_test$parameter
    results$expected_frequencies <- chi_test$expected
    results$contingency_table <- contingency_table
    results$effect_size <- sqrt(chi_test$statistic / sum(contingency_table))  # Cramér's V
    
  } else if (test_type == "t.test") {
    # T-test for continuous variables
    attrition_yes <- data[data$Attrition == "Yes", grouping_var]
    attrition_no <- data[data$Attrition == "No", grouping_var]
    
    t_test <- t.test(attrition_yes, attrition_no, var.equal = FALSE)
    
    results$test_type <- "Welch Two Sample t-test"
    results$statistic <- t_test$statistic
    results$p_value <- t_test$p.value
    results$df <- t_test$parameter
    results$conf_int <- t_test$conf.int
    results$means <- c(mean(attrition_yes, na.rm = TRUE), mean(attrition_no, na.rm = TRUE))
    results$effect_size <- abs(diff(results$means)) / sqrt(((length(attrition_yes) - 1) * var(attrition_yes, na.rm = TRUE) + 
                                                           (length(attrition_no) - 1) * var(attrition_no, na.rm = TRUE)) / 
                                                          (length(attrition_yes) + length(attrition_no) - 2))
    
  } else if (test_type == "wilcox.test") {
    # Mann-Whitney U test for non-parametric comparison
    attrition_yes <- data[data$Attrition == "Yes", grouping_var]
    attrition_no <- data[data$Attrition == "No", grouping_var]
    
    wilcox_test <- wilcox.test(attrition_yes, attrition_no, conf.int = TRUE)
    
    results$test_type <- "Wilcoxon rank sum test"
    results$statistic <- wilcox_test$statistic
    results$p_value <- wilcox_test$p.value
    results$conf_int <- wilcox_test$conf.int
    results$medians <- c(median(attrition_yes, na.rm = TRUE), median(attrition_no, na.rm = TRUE))
    results$effect_size <- wilcox_test$statistic / (length(attrition_yes) * length(attrition_no))
  }
  
  results$alpha <- 0.05
  results$significant <- results$p_value < results$alpha
  
  return(results)
}

#' Calculate confidence intervals for attrition rates
#' @param data Employee data frame or summary statistics
#' @param confidence_level Confidence level (default 0.95)
#' @param method Method for CI calculation ("wilson", "exact", "asymptotic")
#' @return List with confidence intervals
calculate_attrition_confidence_intervals <- function(data, confidence_level = 0.95, method = "wilson") {
  alpha <- 1 - confidence_level
  
  if (is.data.frame(data)) {
    # Calculate from raw data
    n_total <- nrow(data)
    n_attrition <- sum(data$Attrition == "Yes")
    p_hat <- n_attrition / n_total
  } else if (is.list(data) && all(c("successes", "total") %in% names(data))) {
    # Calculate from summary statistics
    n_attrition <- data$successes
    n_total <- data$total
    p_hat <- n_attrition / n_total
  } else {
    stop("Data must be a data frame or list with 'successes' and 'total' elements")
  }
  
  results <- list(
    point_estimate = p_hat,
    sample_size = n_total,
    successes = n_attrition,
    confidence_level = confidence_level,
    method = method
  )
  
  if (method == "wilson") {
    # Wilson score interval (recommended for proportions)
    z <- qnorm(1 - alpha/2)
    denominator <- 1 + z^2/n_total
    center <- (p_hat + z^2/(2*n_total)) / denominator
    margin <- z * sqrt((p_hat*(1-p_hat) + z^2/(4*n_total))/n_total) / denominator
    
    results$lower_bound <- center - margin
    results$upper_bound <- center + margin
    
  } else if (method == "exact") {
    # Exact binomial confidence interval (Clopper-Pearson)
    if (n_attrition == 0) {
      results$lower_bound <- 0
      results$upper_bound <- 1 - (alpha/2)^(1/n_total)
    } else if (n_attrition == n_total) {
      results$lower_bound <- (alpha/2)^(1/n_total)
      results$upper_bound <- 1
    } else {
      results$lower_bound <- qbeta(alpha/2, n_attrition, n_total - n_attrition + 1)
      results$upper_bound <- qbeta(1 - alpha/2, n_attrition + 1, n_total - n_attrition)
    }
    
  } else if (method == "asymptotic") {
    # Asymptotic normal approximation
    z <- qnorm(1 - alpha/2)
    se <- sqrt(p_hat * (1 - p_hat) / n_total)
    margin <- z * se
    
    results$lower_bound <- max(0, p_hat - margin)
    results$upper_bound <- min(1, p_hat + margin)
  }
  
  results$margin_of_error <- (results$upper_bound - results$lower_bound) / 2
  results$width <- results$upper_bound - results$lower_bound
  
  return(results)
}

#' Perform seasonal adjustment on attrition time series
#' @param ts_data Time series data frame with Date and AttritionRate columns
#' @param method Seasonal adjustment method ("x13", "stl", "classical")
#' @return List with seasonal adjustment results
perform_seasonal_adjustment <- function(ts_data, method = "stl") {
  # Convert to time series object
  ts_obj <- ts(ts_data$AttritionRate, 
               start = c(year(min(ts_data$Date)), month(min(ts_data$Date))), 
               frequency = 12)
  
  results <- list(
    original_series = ts_obj,
    method = method,
    start_date = min(ts_data$Date),
    end_date = max(ts_data$Date),
    frequency = 12
  )
  
  if (method == "stl") {
    # STL decomposition
    stl_result <- stl(ts_obj, s.window = "periodic", t.window = NULL)
    
    results$seasonal_adjusted = ts_obj - stl_result$time.series[, "seasonal"]
    results$trend = stl_result$time.series[, "trend"]
    results$seasonal = stl_result$time.series[, "seasonal"]
    results$remainder = stl_result$time.series[, "remainder"]
    results$decomposition = stl_result
    
    # Calculate seasonal strength
    var_seasonal <- var(stl_result$time.series[, "seasonal"], na.rm = TRUE)
    var_remainder <- var(stl_result$time.series[, "remainder"], na.rm = TRUE)
    results$seasonal_strength <- var_seasonal / (var_seasonal + var_remainder)
    
  } else if (method == "classical") {
    # Classical decomposition
    decomp_result <- decompose(ts_obj, type = "additive")
    
    results$seasonal_adjusted = ts_obj - decomp_result$seasonal
    results$trend = decomp_result$trend
    results$seasonal = decomp_result$seasonal
    results$remainder = decomp_result$random
    results$decomposition = decomp_result
    
    # Calculate seasonal strength
    var_seasonal <- var(decomp_result$seasonal, na.rm = TRUE)
    var_remainder <- var(decomp_result$random, na.rm = TRUE)
    results$seasonal_strength <- var_seasonal / (var_seasonal + var_remainder)
    
  } else if (method == "x13") {
    # X-13ARIMA-SEATS (requires seasonal package and X-13 binary)
    tryCatch({
      x13_result <- seas(ts_obj)
      results$seasonal_adjusted = final(x13_result)
      results$trend = trend(x13_result)
      results$seasonal = seasonal(x13_result)
      results$remainder = irregular(x13_result)
      results$decomposition = x13_result
      results$seasonal_strength = NA  # Not directly available from X-13
    }, error = function(e) {
      warning("X-13 method failed, falling back to STL")
      return(perform_seasonal_adjustment(ts_data, method = "stl"))
    })
  }
  
  # Seasonality tests
  results$seasonality_tests <- list()
  
  # QS test for seasonality
  if (length(ts_obj) >= 24) {
    qs_test <- Box.test(diff(ts_obj, lag = 12), type = "Ljung-Box")
    results$seasonality_tests$qs_test <- list(
      statistic = qs_test$statistic,
      p_value = qs_test$p.value,
      significant = qs_test$p.value < 0.05
    )
  }
  
  # Kruskal-Wallis test for seasonal differences
  months <- month(ts_data$Date)
  kw_test <- kruskal.test(ts_data$AttritionRate, months)
  results$seasonality_tests$kruskal_wallis <- list(
    statistic = kw_test$statistic,
    p_value = kw_test$p.value,
    significant = kw_test$p.value < 0.05
  )
  
  return(results)
}

# =============================================================================
# UNIT TESTS - CORRELATION CALCULATIONS
# =============================================================================

test_that("Correlation calculations are accurate and robust", {
  
  # Test 1: Basic correlation calculation
  test_data <- generate_test_employee_data(n = 500, seed = 42)
  
  # Test different correlation methods
  for (method in c("pearson", "spearman", "kendall")) {
    result <- calculate_attrition_correlations(test_data, method = method)
    
    # Check structure
    expect_type(result, "list")
    expect_true(all(c("correlation_matrix", "p_values", "method", "n_observations") %in% names(result)))
    expect_equal(result$method, method)
    expect_equal(result$n_observations, nrow(test_data))
    
    # Check correlation matrix properties
    cor_matrix <- result$correlation_matrix
    expect_true(is.matrix(cor_matrix))
    expect_equal(nrow(cor_matrix), ncol(cor_matrix))
    expect_true(all(diag(cor_matrix) == 1))  # Diagonal should be 1
    expect_true(all(cor_matrix >= -1 & cor_matrix <= 1))  # Correlations in [-1, 1]
    expect_true(isSymmetric(cor_matrix))  # Matrix should be symmetric
    
    # Check p-values
    p_matrix <- result$p_values
    expect_true(is.matrix(p_matrix))
    expect_equal(dim(p_matrix), dim(cor_matrix))
    expect_true(all(p_matrix >= 0 & p_matrix <= 1, na.rm = TRUE))  # P-values in [0, 1]
    expect_true(all(diag(p_matrix) == 0))  # Diagonal p-values should be 0
  }
})

test_that("Correlation calculations handle edge cases", {
  
  # Test 2: Perfect correlation
  n <- 100
  perfect_data <- data.frame(
    Attrition = rep(c("Yes", "No"), each = n/2),
    Age = c(rep(25, n/2), rep(45, n/2)),
    Salary = c(rep(50000, n/2), rep(80000, n/2)),
    YearsAtCompany = c(rep(1, n/2), rep(10, n/2)),
    YearsInRole = c(rep(1, n/2), rep(5, n/2)),
    YearsSincePromotion = c(rep(0, n/2), rep(3, n/2)),
    JobSatisfaction = c(rep(2, n/2), rep(4, n/2)),
    WorkLifeBalance = c(rep(2, n/2), rep(4, n/2)),
    EnvironmentSatisfaction = c(rep(2, n/2), rep(4, n/2)),
    DistanceFromHome = c(rep(30, n/2), rep(5, n/2)),
    OverTime = c(rep("Yes", n/2), rep("No", n/2)),
    Gender = rep(c("Male", "Female"), each = n/2),
    BusinessTravel = c(rep("Frequently", n/2), rep("None", n/2))
  )
  
  result <- calculate_attrition_correlations(perfect_data)
  expect_true(any(abs(result$correlation_matrix[1, -1]) > 0.8))  # Should have high correlations
  
  # Test 3: Missing values
  missing_data <- test_data <- generate_test_employee_data(n = 200, seed = 123)
  missing_data$Age[1:20] <- NA
  missing_data$Salary[21:40] <- NA
  
  result <- calculate_attrition_correlations(missing_data)
  expect_true(all(!is.na(result$correlation_matrix)))  # Should handle missing values
  
  # Test 4: Constant variables
  constant_data <- generate_test_employee_data(n = 100, seed = 456)
  constant_data$JobSatisfaction <- 3  # Make constant
  
  result <- calculate_attrition_correlations(constant_data)
  expect_true(all(is.na(result$correlation_matrix[, "JobSatisfaction"]) | 
                 result$correlation_matrix[, "JobSatisfaction"] == 0))
})

test_that("Correlation significance testing is correct", {
  
  # Test 5: Known significant correlation
  set.seed(789)
  n <- 200
  x <- rnorm(n)
  y <- 0.7 * x + rnorm(n, 0, 0.5)  # Strong positive correlation
  
  test_data <- data.frame(
    Attrition = sample(c("Yes", "No"), n, replace = TRUE),
    Variable1 = x,
    Variable2 = y,
    Age = sample(25:65, n, replace = TRUE),
    Salary = rnorm(n, 65000, 15000),
    YearsAtCompany = sample(0:30, n, replace = TRUE),
    YearsInRole = sample(0:15, n, replace = TRUE),
    YearsSincePromotion = sample(0:10, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    Gender = sample(c("Male", "Female"), n, replace = TRUE),
    BusinessTravel = sample(c("None", "Rarely", "Frequently"), n, replace = TRUE)
  )
  
  result <- calculate_attrition_correlations(test_data)
  
  # Find correlation between Variable1 and Variable2
  var1_idx <- which(colnames(result$correlation_matrix) == "Variable1")
  var2_idx <- which(colnames(result$correlation_matrix) == "Variable2")
  
  expect_true(result$correlation_matrix[var1_idx, var2_idx] > 0.6)  # Should be strongly correlated
  expect_true(result$p_values[var1_idx, var2_idx] < 0.05)  # Should be significant
})

# =============================================================================
# UNIT TESTS - HYPOTHESIS TESTING RESULTS
# =============================================================================

test_that("Hypothesis testing produces valid results", {
  
  # Test 6: Chi-square test for categorical variables
  test_data <- generate_test_employee_data(n = 800, seed = 101)
  
  # Test chi-square test
  chi_result <- perform_attrition_hypothesis_tests(test_data, "Department", "chi.test")
  
  expect_equal(chi_result$test_type, "Chi-square test")
  expect_true(chi_result$statistic >= 0)  # Chi-square statistic should be non-negative
  expect_true(chi_result$p_value >= 0 && chi_result$p_value <= 1)  # Valid p-value
  expect_true(chi_result$df > 0)  # Degrees of freedom should be positive
  expect_true(is.matrix(chi_result$expected_frequencies))
  expect_true(is.table(chi_result$contingency_table))
  expect_true(chi_result$effect_size >= 0 && chi_result$effect_size <= 1)  # Cramér's V bounds
  expect_type(chi_result$significant, "logical")
  
  # Check contingency table structure
  expect_equal(sum(chi_result$contingency_table), nrow(test_data))
  expect_equal(ncol(chi_result$contingency_table), 2)  # Yes/No for attrition
})

test_that("T-test implementation is correct", {
  
  # Test 7: T-test for continuous variables
  test_data <- generate_test_employee_data(n = 600, seed = 202)
  
  # Artificially create a difference in means
  test_data$Age[test_data$Attrition == "Yes"] <- test_data$Age[test_data$Attrition == "Yes"] + 5
  
  t_result <- perform_attrition_hypothesis_tests(test_data, "Age", "t.test")
  
  expect_equal(t_result$test_type, "Welch Two Sample t-test")
  expect_true(is.numeric(t_result$statistic))
  expect_true(t_result$p_value >= 0 && t_result$p_value <= 1)
  expect_true(t_result$df > 0)
  expect_length(t_result$conf_int, 2)
  expect_true(t_result$conf_int[1] < t_result$conf_int[2])  # Lower < Upper
  expect_length(t_result$means, 2)
  expect_true(t_result$effect_size >= 0)  # Cohen's d should be non-negative
  expect_type(t_result$significant, "logical")
  
  # Check that means are different (we artificially created a difference)
  expect_true(abs(t_result$means[1] - t_result$means[2]) > 0)
})

test_that("Wilcoxon test handles non-parametric data", {
  
  # Test 8: Wilcoxon test for non-normal data
  set.seed(303)
  n <- 400
  
  # Create skewed data
  test_data <- generate_test_employee_data(n = n, seed = 303)
  test_data$SkewedVariable <- rexp(n, rate = 0.1)  # Exponential distribution
  
  # Create difference between groups
  test_data$SkewedVariable[test_data$Attrition == "Yes"] <- 
    test_data$SkewedVariable[test_data$Attrition == "Yes"] * 1.5
  
  wilcox_result <- perform_attrition_hypothesis_tests(test_data, "SkewedVariable", "wilcox.test")
  
  expect_equal(wilcox_result$test_type, "Wilcoxon rank sum test")
  expect_true(is.numeric(wilcox_result$statistic))
  expect_true(wilcox_result$p_value >= 0 && wilcox_result$p_value <= 1)
  expect_length(wilcox_result$conf_int, 2)
  expect_length(wilcox_result$medians, 2)
  expect_true(wilcox_result$effect_size >= 0 && wilcox_result$effect_size <= 1)
  expect_type(wilcox_result$significant, "logical")
  
  # Should detect the difference we created
  expect_true(wilcox_result$medians[1] != wilcox_result$medians[2])
})

test_that("Hypothesis tests handle edge cases", {
  
  # Test 9: Small sample sizes
  small_data <- generate_test_employee_data(n = 20, seed = 404)
  
  result <- perform_attrition_hypothesis_tests(small_data, "Department", "chi.test")
  expect_true(is.numeric(result$p_value))
  expect_true(!is.na(result$p_value))
  
  # Test 10: Unbalanced groups
  unbalanced_data <- generate_test_employee_data(n = 100, seed = 505)
  unbalanced_data$Attrition[1:95] <- "No"  # 95% No, 5% Yes
  
  result <- perform_attrition_hypothesis_tests(unbalanced_data, "Age", "t.test")
  expect_true(is.numeric(result$p_value))
  expect_true(!is.na(result$p_value))
})

# =============================================================================
# UNIT TESTS - CONFIDENCE INTERVAL ACCURACY
# =============================================================================

test_that("Wilson confidence intervals are accurate", {
  
  # Test 11: Wilson interval calculation
  test_cases <- list(
    list(successes = 16, total = 100),  # 16% attrition
    list(successes = 0, total = 50),    # 0% attrition
    list(successes = 50, total = 50),   # 100% attrition
    list(successes = 5, total = 20),    # 25% attrition
    list(successes = 150, total = 1000) # 15% attrition
  )
  
  for (case in test_cases) {
    result <- calculate_attrition_confidence_intervals(case, method = "wilson")
    
    expect_equal(result$successes, case$successes)
    expect_equal(result$sample_size, case$total)
    expect_equal(result$point_estimate, case$successes / case$total)
    expect_equal(result$method, "wilson")
    
    # Check bounds
    expect_true(result$lower_bound >= 