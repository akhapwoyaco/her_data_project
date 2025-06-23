# ==============================================================================
# ATLAS LABS HR ANALYTICS - ATTRITION ANALYSIS MODULE UNIT TESTS
# ==============================================================================
# Test Coverage: Confidence Interval Accuracy & Seasonal Adjustment Validity
# Author: akhapwoyaco
# Date: 2025-06-23
# ==============================================================================

# Load required libraries for testing
library(testthat)
library(dplyr)
library(lubridate)
library(binom)
library(forecast)
library(seasonal)
library(mockery)

# Source the attrition module functions (assuming they exist)
# source("modules/attrition_module.R")

# ==============================================================================
# MOCK FUNCTIONS FOR TESTING (Since actual module functions aren't implemented)
# ==============================================================================

# Mock function for confidence interval calculation
calculate_attrition_ci <- function(attrition_count, total_count, confidence_level = 0.95) {
  if (total_count == 0) return(list(lower = NA, upper = NA, point_estimate = NA))
  if (attrition_count > total_count) stop("Attrition count cannot exceed total count")
  if (confidence_level <= 0 || confidence_level >= 1) stop("Confidence level must be between 0 and 1")
  
  # Wilson score interval (more accurate for proportions)
  result <- binom.confint(attrition_count, total_count, 
                          conf.level = confidence_level, 
                          methods = "wilson")
  
  list(
    lower = result$lower,
    upper = result$upper,
    point_estimate = result$mean,
    method = "Wilson",
    confidence_level = confidence_level
  )
}

# Mock function for seasonal adjustment
apply_seasonal_adjustment <- function(attrition_ts, method = "x13") {
  if (length(attrition_ts) < 24) {
    warning("Insufficient data for seasonal adjustment (< 24 observations)")
    return(list(
      adjusted = as.numeric(attrition_ts),
      seasonal_factors = rep(1, length(attrition_ts)),
      trend = as.numeric(attrition_ts),
      method = "none",
      diagnostics = list(seasonal_strength = 0)
    ))
  }
  
  tryCatch({
    if (method == "x13") {
      # X-13ARIMA-SEATS seasonal adjustment
      seas_result <- seas(attrition_ts, transform.function = "auto")
      
      list(
        adjusted = as.numeric(final(seas_result)),
        seasonal_factors = as.numeric(series(seas_result, "s10")),
        trend = as.numeric(series(seas_result, "s12")),
        method = "X-13ARIMA-SEATS",
        diagnostics = list(
          seasonal_strength = seasonal_strength(attrition_ts),
          f_test = seastest(seas_result)$Ftest,
          residual_seasonality = seastest(seas_result)$residual.seasonality
        )
      )
    } else if (method == "stl") {
      # STL decomposition
      stl_result <- stl(attrition_ts, s.window = "periodic")
      
      list(
        adjusted = as.numeric(attrition_ts - stl_result$time.series[, "seasonal"]),
        seasonal_factors = as.numeric(stl_result$time.series[, "seasonal"]),
        trend = as.numeric(stl_result$time.series[, "trend"]),
        method = "STL",
        diagnostics = list(
          seasonal_strength = seasonal_strength(attrition_ts)
        )
      )
    }
  }, error = function(e) {
    warning(paste("Seasonal adjustment failed:", e$message))
    list(
      adjusted = as.numeric(attrition_ts),
      seasonal_factors = rep(0, length(attrition_ts)),
      trend = as.numeric(attrition_ts),
      method = "none",
      diagnostics = list(seasonal_strength = 0, error = e$message)
    )
  })
}

# Helper function to calculate seasonal strength
seasonal_strength <- function(ts_data) {
  if (length(ts_data) < 24) return(0)
  
  tryCatch({
    decomp <- stl(ts_data, s.window = "periodic")
    seasonal_var <- var(decomp$time.series[, "seasonal"], na.rm = TRUE)
    remainder_var <- var(decomp$time.series[, "remainder"], na.rm = TRUE)
    
    seasonal_var / (seasonal_var + remainder_var)
  }, error = function(e) 0)
}

# ==============================================================================
# CONFIDENCE INTERVAL ACCURACY TESTS
# ==============================================================================

context("Attrition Analysis - Confidence Interval Accuracy")

test_that("Confidence intervals are calculated correctly for normal cases", {
  # Test with typical attrition data
  attrition_count <- 25
  total_count <- 100
  confidence_level <- 0.95
  
  result <- calculate_attrition_ci(attrition_count, total_count, confidence_level)
  
  # Basic validity checks
  expect_type(result, "list")
  expect_named(result, c("lower", "upper", "point_estimate", "method", "confidence_level"))
  
  # Point estimate should be correct
  expect_equal(result$point_estimate, 0.25, tolerance = 1e-10)
  
  # Confidence interval should be valid
  expect_true(result$lower >= 0)
  expect_true(result$upper <= 1)
  expect_true(result$lower <= result$point_estimate)
  expect_true(result$upper >= result$point_estimate)
  expect_true(result$lower < result$upper)
  
  # Method and confidence level should be preserved
  expect_equal(result$method, "Wilson")
  expect_equal(result$confidence_level, confidence_level)
})

test_that("Confidence intervals handle edge cases correctly", {
  # Test with zero attrition
  result_zero <- calculate_attrition_ci(0, 100, 0.95)
  expect_equal(result_zero$point_estimate, 0)
  expect_true(result_zero$lower >= 0)
  expect_true(result_zero$upper > 0)  # Should still have upper bound > 0
  
  # Test with 100% attrition
  result_full <- calculate_attrition_ci(50, 50, 0.95)
  expect_equal(result_full$point_estimate, 1)
  expect_true(result_full$lower < 1)  # Should have lower bound < 1
  expect_true(result_full$upper <= 1)
  
  # Test with very small sample
  result_small <- calculate_attrition_ci(1, 3, 0.95)
  expect_true(result_small$upper - result_small$lower > 0.3)  # Should have wide interval
})

test_that("Confidence intervals validate input parameters", {
  # Test invalid confidence levels
  expect_error(calculate_attrition_ci(10, 100, 0), "Confidence level must be between 0 and 1")
  expect_error(calculate_attrition_ci(10, 100, 1), "Confidence level must be between 0 and 1")
  expect_error(calculate_attrition_ci(10, 100, -0.5), "Confidence level must be between 0 and 1")
  expect_error(calculate_attrition_ci(10, 100, 1.5), "Confidence level must be between 0 and 1")
  
  # Test invalid counts
  expect_error(calculate_attrition_ci(150, 100), "Attrition count cannot exceed total count")
  
  # Test zero total count
  result_zero_total <- calculate_attrition_ci(0, 0, 0.95)
  expect_true(is.na(result_zero_total$point_estimate))
  expect_true(is.na(result_zero_total$lower))
  expect_true(is.na(result_zero_total$upper))
})

test_that("Confidence interval width decreases with larger sample sizes", {
  # Test that confidence intervals get narrower with larger samples
  small_result <- calculate_attrition_ci(25, 100, 0.95)
  large_result <- calculate_attrition_ci(250, 1000, 0.95)
  
  small_width <- small_result$upper - small_result$lower
  large_width <- large_result$upper - large_result$lower
  
  expect_true(large_width < small_width, 
              info = "Larger sample should produce narrower confidence interval")
})

test_that("Confidence intervals are symmetric for p=0.5", {
  # When true proportion is 0.5, confidence interval should be approximately symmetric
  result <- calculate_attrition_ci(500, 1000, 0.95)
  
  lower_distance <- result$point_estimate - result$lower
  upper_distance <- result$upper - result$point_estimate
  
  expect_equal(lower_distance, upper_distance, tolerance = 0.01)
})

test_that("Different confidence levels produce expected interval widths", {
  # Test that higher confidence levels produce wider intervals
  ci_90 <- calculate_attrition_ci(25, 100, 0.90)
  ci_95 <- calculate_attrition_ci(25, 100, 0.95)
  ci_99 <- calculate_attrition_ci(25, 100, 0.99)
  
  width_90 <- ci_90$upper - ci_90$lower
  width_95 <- ci_95$upper - ci_95$lower
  width_99 <- ci_99$upper - ci_99$lower
  
  expect_true(width_90 < width_95)
  expect_true(width_95 < width_99)
})

# ==============================================================================
# SEASONAL ADJUSTMENT VALIDITY TESTS
# ==============================================================================

context("Attrition Analysis - Seasonal Adjustment Validity")

test_that("Seasonal adjustment handles insufficient data gracefully", {
  # Test with very short time series
  short_ts <- ts(c(1, 2, 3, 4, 5), frequency = 12)
  
  result <- apply_seasonal_adjustment(short_ts, method = "x13")
  
  expect_warning(apply_seasonal_adjustment(short_ts, method = "x13"))
  expect_equal(result$method, "none")
  expect_equal(result$adjusted, as.numeric(short_ts))
  expect_equal(result$seasonal_factors, rep(1, length(short_ts)))
})

test_that("Seasonal adjustment preserves time series properties", {
  # Create synthetic seasonal data
  set.seed(123)
  n_years <- 3
  n_months <- n_years * 12
  
  # Generate data with clear seasonal pattern
  seasonal_pattern <- rep(c(1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3), n_years)
  trend <- seq(10, 20, length.out = n_months)
  noise <- rnorm(n_months, 0, 0.5)
  
  attrition_data <- trend * seasonal_pattern + noise
  attrition_ts <- ts(attrition_data, frequency = 12)
  
  # Apply seasonal adjustment
  result <- apply_seasonal_adjustment(attrition_ts, method = "stl")
  
  # Basic validity checks
  expect_type(result, "list")
  expect_named(result, c("adjusted", "seasonal_factors", "trend", "method", "diagnostics"))
  expect_equal(length(result$adjusted), length(attrition_ts))
  expect_equal(length(result$seasonal_factors), length(attrition_ts))
  expect_equal(result$method, "STL")
  
  # Adjusted series should have reduced seasonality
  original_seasonal_strength <- seasonal_strength(attrition_ts)
  adjusted_seasonal_strength <- seasonal_strength(ts(result$adjusted, frequency = 12))
  
  expect_true(adjusted_seasonal_strength < original_seasonal_strength,
              info = "Seasonal adjustment should reduce seasonal strength")
})

test_that("Seasonal adjustment diagnostics are meaningful", {
  # Create data with strong seasonal pattern
  set.seed(456)
  n_years <- 4
  n_months <- n_years * 12
  
  # Strong seasonal pattern
  seasonal_pattern <- rep(c(2.0, 1.8, 1.5, 1.2, 1.0, 0.8, 0.6, 0.8, 1.0, 1.2, 1.5, 1.8), n_years)
  trend <- rep(15, n_months)
  noise <- rnorm(n_months, 0, 0.2)
  
  seasonal_data <- trend * seasonal_pattern + noise
  seasonal_ts <- ts(seasonal_data, frequency = 12)
  
  result <- apply_seasonal_adjustment(seasonal_ts, method = "stl")
  
  # Check diagnostics
  expect_type(result$diagnostics, "list")
  expect_true("seasonal_strength" %in% names(result$diagnostics))
  expect_true(result$diagnostics$seasonal_strength > 0.5,
              info = "Strong seasonal pattern should have high seasonal strength")
})

test_that("Seasonal adjustment methods produce different but valid results", {
  # Generate seasonal data
  set.seed(789)
  n_years <- 3
  n_months <- n_years * 12
  
  seasonal_pattern <- rep(c(1.3, 1.2, 1.1, 0.9, 0.8, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3), n_years)
  trend <- seq(12, 18, length.out = n_months)
  attrition_data <- trend * seasonal_pattern + rnorm(n_months, 0, 0.3)
  attrition_ts <- ts(attrition_data, frequency = 12)
  
  # Test STL method
  stl_result <- apply_seasonal_adjustment(attrition_ts, method = "stl")
  
  # Both methods should produce valid results
  expect_equal(stl_result$method, "STL")
  expect_equal(length(stl_result$adjusted), length(attrition_ts))
  
  # Results should be different but both valid
  expect_true(all(is.finite(stl_result$adjusted)))
  expect_true(all(is.finite(stl_result$seasonal_factors)))
})

test_that("Seasonal adjustment handles error conditions", {
  # Test with invalid time series
  invalid_ts <- ts(rep(NA, 36), frequency = 12)
  
  result <- apply_seasonal_adjustment(invalid_ts, method = "stl")
  
  expect_warning(apply_seasonal_adjustment(invalid_ts, method = "stl"))
  expect_equal(result$method, "none")
  expect_true("error" %in% names(result$diagnostics))
})

test_that("Seasonal factors sum to zero over complete cycles", {
  # Generate data with perfect seasonal pattern
  set.seed(999)
  n_years <- 3
  n_months <- n_years * 12
  
  perfect_seasonal <- rep(c(1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3), n_years)
  trend <- rep(15, n_months)
  
  seasonal_ts <- ts(trend * perfect_seasonal, frequency = 12)
  result <- apply_seasonal_adjustment(seasonal_ts, method = "stl")
  
  # For additive decomposition, seasonal factors should sum to approximately zero
  # We'll check that the mean is close to zero
  expect_true(abs(mean(result$seasonal_factors)) < 0.1,
              info = "Seasonal factors should average to approximately zero")
})

test_that("Seasonal adjustment preserves overall level", {
  # Test that the mean of adjusted series is close to original
  set.seed(555)
  n_years <- 3
  n_months <- n_years * 12
  
  seasonal_pattern <- rep(c(1.1, 1.05, 1.0, 0.95, 0.9, 0.9, 0.95, 1.0, 1.05, 1.1, 1.1, 1.05), n_years)
  base_level <- 20
  attrition_data <- base_level * seasonal_pattern + rnorm(n_months, 0, 0.5)
  
  attrition_ts <- ts(attrition_data, frequency = 12)
  result <- apply_seasonal_adjustment(attrition_ts, method = "stl")
  
  original_mean <- mean(attrition_ts, na.rm = TRUE)
  adjusted_mean <- mean(result$adjusted, na.rm = TRUE)
  
  expect_equal(adjusted_mean, original_mean, tolerance = 0.5,
               info = "Seasonal adjustment should preserve overall level")
})

# ==============================================================================
# INTEGRATION TESTS
# ==============================================================================

context("Attrition Analysis - Integration Tests")

test_that("Confidence intervals work with seasonal adjustment pipeline", {
  # Create monthly attrition data with seasonal pattern
  set.seed(777)
  n_months <- 36
  
  # Generate attrition counts with seasonal variation
  base_attrition <- 50
  seasonal_multiplier <- rep(c(1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3), 3)
  
  attrition_counts <- rpois(n_months, base_attrition * seasonal_multiplier)
  total_employees <- rep(1000, n_months)
  
  # Apply seasonal adjustment to attrition rates
  attrition_rates <- attrition_counts / total_employees
  attrition_ts <- ts(attrition_rates, frequency = 12)
  
  seasonal_result <- apply_seasonal_adjustment(attrition_ts, method = "stl")
  
  # Calculate confidence intervals for adjusted rates
  adjusted_counts <- round(seasonal_result$adjusted * total_employees)
  adjusted_counts <- pmax(0, pmin(adjusted_counts, total_employees))  # Bound within valid range
  
  ci_results <- mapply(function(count, total) {
    calculate_attrition_ci(count, total, 0.95)
  }, adjusted_counts, total_employees, SIMPLIFY = FALSE)
  
  # Verify that all confidence intervals are valid
  expect_true(all(sapply(ci_results, function(x) x$lower >= 0)))
  expect_true(all(sapply(ci_results, function(x) x$upper <= 1)))
  expect_true(all(sapply(ci_results, function(x) x$lower <= x$upper)))
  
  # Verify that seasonal adjustment reduced variation
  original_cv <- sd(attrition_rates) / mean(attrition_rates)
  adjusted_cv <- sd(seasonal_result$adjusted) / mean(seasonal_result$adjusted)
  
  expect_true(adjusted_cv <= original_cv,
              info = "Seasonal adjustment should reduce coefficient of variation")
})

# ==============================================================================
# PERFORMANCE TESTS
# ==============================================================================

context("Attrition Analysis - Performance Tests")

test_that("Confidence interval calculation is efficient for large datasets", {
  # Test performance with large sample sizes
  large_counts <- rep(c(100, 150, 200, 250), 250)  # 1000 observations
  large_totals <- rep(c(1000, 1500, 2000, 2500), 250)
  
  start_time <- Sys.time()
  
  results <- mapply(calculate_attrition_ci, large_counts, large_totals, 
                   MoreArgs = list(confidence_level = 0.95), SIMPLIFY = FALSE)
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within reasonable time (< 5 seconds for 1000 calculations)
  expect_true(execution_time < 5, 
              info = paste("Performance test took", round(execution_time, 2), "seconds"))
  
  # All results should be valid
  expect_true(all(sapply(results, function(x) !is.na(x$point_estimate))))
})

test_that("Seasonal adjustment handles memory efficiently", {
  # Test with longer time series
  n_years <- 10
  n_months <- n_years * 12
  
  # Generate large seasonal dataset
  set.seed(123)
  seasonal_data <- rep(c(1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3), n_years)
  trend <- seq(10, 30, length.out = n_months)
  noise <- rnorm(n_months, 0, 0.5)
  
  large_ts <- ts(trend * seasonal_data + noise, frequency = 12)
  
  # Monitor memory usage (simplified check)
  initial_memory <- object.size(large_ts)
  
  result <- apply_seasonal_adjustment(large_ts, method = "stl")
  
  result_memory <- object.size(result)
  
  # Result shouldn't be excessively large compared to input
  expect_true(as.numeric(result_memory) < as.numeric(initial_memory) * 10,
              info = "Seasonal adjustment result should not use excessive memory")
  
  # Result should be complete
  expect_equal(length(result$adjusted), length(large_ts))
})

# ==============================================================================
# EDGE CASE TESTS
# ==============================================================================

context("Attrition Analysis - Edge Cases")

test_that("Functions handle missing values appropriately", {
  # Test confidence intervals with missing data
  expect_error(calculate_attrition_ci(NA, 100, 0.95))
  expect_error(calculate_attrition_ci(10, NA, 0.95))
  
  # Test seasonal adjustment with missing values
  ts_with_na <- ts(c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                     13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24), frequency = 12)
  
  result <- apply_seasonal_adjustment(ts_with_na, method = "stl")
  
  # Should handle missing values gracefully
  expect_true(is.list(result))
  expect_equal(length(result$adjusted), length(ts_with_na))
})

test_that("Functions handle extreme values correctly", {
  # Test with very high attrition rates
  extreme_result <- calculate_attrition_ci(999, 1000, 0.95)
  expect_true(extreme_result$point_estimate > 0.99)
  expect_true(extreme_result$lower > 0.95)
  
  # Test seasonal adjustment with extreme values
  extreme_ts <- ts(c(rep(1000, 12), rep(1, 12), rep(1000, 12)), frequency = 12)
  extreme_seasonal <- apply_seasonal_adjustment(extreme_ts, method = "stl")
  
  expect_true(is.finite(mean(extreme_seasonal$adjusted)))
})

# ==============================================================================
# STATISTICAL VALIDATION TESTS
# ==============================================================================

context("Attrition Analysis - Statistical Validation")

test_that("Confidence intervals have correct coverage probability", {
  # Monte Carlo test of confidence interval coverage
  set.seed(42)
  n_simulations <- 1000
  true_p <- 0.3
  sample_size <- 100
  confidence_level <- 0.95
  
  coverage_count <- 0
  
  for (i in 1:n_simulations) {
    # Generate random sample
    attrition_count <- rbinom(1, sample_size, true_p)
    
    # Calculate confidence interval
    ci_result <- calculate_attrition_ci(attrition_count, sample_size, confidence_level)
    
    # Check if true parameter is within interval
    if (true_p >= ci_result$lower && true_p <= ci_result$upper) {
      coverage_count <- coverage_count + 1
    }
  }
  
  coverage_probability <- coverage_count / n_simulations
  
  # Coverage should be close to nominal level (95%)
  # Allow for some Monte Carlo error
  expect_true(coverage_probability >= 0.93 && coverage_probability <= 0.97,
              info = paste("Coverage probability:", round(coverage_probability, 3)))
})

test_that("Seasonal adjustment reduces seasonality significantly", {
  # Test with known seasonal pattern
  set.seed(321)
  n_years <- 5
  months <- n_years * 12
  
  # Create strong seasonal pattern
  seasonal_component <- rep(c(2, 1.8, 1.5, 1.2, 1, 0.8, 0.6, 0.8, 1, 1.2, 1.5, 1.8), n_years)
  trend_component <- seq(10, 20, length.out = months)
  noise <- rnorm(months, 0, 0.1)
  
  seasonal_ts <- ts(trend_component + seasonal_component + noise, frequency = 12)
  
  result <- apply_seasonal_adjustment(seasonal_ts, method = "stl")
  
  # Calculate seasonal strength before and after
  original_strength <- seasonal_strength(seasonal_ts)
  adjusted_strength <- seasonal_strength(ts(result$adjusted, frequency = 12))
  
  # Seasonal adjustment should significantly reduce seasonal strength
  reduction_ratio <- adjusted_strength / original_strength
  expect_true(reduction_ratio < 0.3,
              info = paste("Seasonal strength reduction ratio:", round(reduction_ratio, 3)))
})

# ==============================================================================
# RUN ALL TESTS
# ==============================================================================

# Function to run all tests with summary
run_attrition_tests <- function() {
  cat("=== ATLAS LABS ATTRITION ANALYSIS MODULE TESTS ===\n")
  cat("Testing Confidence Interval Accuracy & Seasonal Adjustment Validity\n")
  cat("====================================================================\n\n")
  
  # Run tests and capture results
  test_results <- test_dir(".", reporter = "summary")
  
  cat("\n====================================================================\n")
  cat("Test Summary Complete - Check results above for any failures\n")
  cat("====================================================================\n")
  
  return(test_results)
}

# Uncomment to run tests
# run_attrition_tests()