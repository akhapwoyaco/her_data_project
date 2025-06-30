# =============================================================================
# Atlas Labs HR Analytics Dashboard
# Satisfaction Module Unit Tests - Bias Correction & Longitudinal Analysis
# =============================================================================
# Developer: akhapwoyaco
# Focus: Comprehensive testing of bias correction algorithms and longitudinal trend analysis
# Test Framework: testthat with custom Atlas Labs testing extensions

library(testthat)
library(tidyverse)
library(lubridate)
library(mockery)

# Load the satisfaction module functions (assuming they exist)
# source("modules/satisfaction_module.R")

# =============================================================================
# TEST HELPER FUNCTIONS & MOCK DATA GENERATORS
# =============================================================================

#' Generate mock satisfaction data with controlled bias patterns
#' @param n_employees Number of employees
#' @param bias_type Type of bias to introduce ("response", "survivorship", "temporal", "demographic")
#' @param bias_strength Strength of bias (0-1)
#' @return Tibble with mock satisfaction data
generate_mock_satisfaction_data <- function(n_employees = 1000, 
                                          bias_type = "none", 
                                          bias_strength = 0.3) {
  set.seed(42) # Reproducible tests
  
  base_data <- tibble(
    employee_id = 1:n_employees,
    gender = sample(c("Male", "Female", "Non-binary"), n_employees, 
                   replace = TRUE, prob = c(0.45, 0.45, 0.1)),
    age_group = sample(c("20-30", "31-40", "41-50", "51-60"), n_employees,
                      replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
    department = sample(c("IT", "HR", "Sales", "Finance", "Operations"), 
                       n_employees, replace = TRUE),
    tenure_years = sample(1:20, n_employees, replace = TRUE),
    review_date = sample(seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "month"),
                        n_employees, replace = TRUE),
    job_satisfaction = rnorm(n_employees, mean = 3.5, sd = 0.8),
    environment_satisfaction = rnorm(n_employees, mean = 3.2, sd = 0.9),
    work_life_balance = rnorm(n_employees, mean = 3.0, sd = 1.0),
    relationship_satisfaction = rnorm(n_employees, mean = 3.4, sd = 0.7)
  )
  
  # Apply bias based on type
  if (bias_type == "response") {
    # Response bias: dissatisfied employees less likely to respond
    response_prob <- pmax(0.1, base_data$job_satisfaction / 5)
    base_data <- base_data[rbinom(n_employees, 1, response_prob) == 1, ]
  } else if (bias_type == "survivorship") {
    # Survivorship bias: employees with low satisfaction more likely to leave
    survival_prob <- pmax(0.2, (base_data$job_satisfaction - 1) / 4)
    base_data <- base_data[rbinom(nrow(base_data), 1, survival_prob) == 1, ]
  } else if (bias_type == "temporal") {
    # Temporal bias: satisfaction artificially inflated in recent periods
    recent_boost <- ifelse(base_data$review_date > as.Date("2023-01-01"), 
                          bias_strength, 0)
    base_data$job_satisfaction <- base_data$job_satisfaction + recent_boost
  } else if (bias_type == "demographic") {
    # Demographic bias: certain groups systematically rate differently
    gender_adjustment <- case_when(
      base_data$gender == "Male" ~ bias_strength,
      base_data$gender == "Female" ~ -bias_strength,
      TRUE ~ 0
    )
    base_data$job_satisfaction <- base_data$job_satisfaction + gender_adjustment
  }
  
  # Ensure ratings are within valid range (1-5)
  satisfaction_cols <- c("job_satisfaction", "environment_satisfaction", 
                        "work_life_balance", "relationship_satisfaction")
  base_data[satisfaction_cols] <- map_dfc(base_data[satisfaction_cols], 
                                         ~pmax(1, pmin(5, .x)))
  
  return(base_data)
}

#' Generate longitudinal satisfaction data with known trends
#' @param n_employees Number of employees
#' @param time_periods Number of time periods
#' @param trend_type Type of trend ("linear", "seasonal", "cyclical", "volatile")
#' @return Tibble with longitudinal satisfaction data
generate_longitudinal_data <- function(n_employees = 200, 
                                     time_periods = 48, 
                                     trend_type = "linear") {
  set.seed(123)
  
  # Base employee characteristics
  employees <- tibble(
    employee_id = 1:n_employees,
    department = sample(c("IT", "HR", "Sales", "Finance"), n_employees, replace = TRUE),
    baseline_satisfaction = rnorm(n_employees, 3.5, 0.5)
  )
  
  # Generate time series
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = time_periods)
  
  # Create all combinations
  longitudinal_data <- expand_grid(employees, review_date = dates) %>%
    arrange(employee_id, review_date) %>%
    mutate(time_index = as.numeric(review_date - min(review_date)) / 30.44) # months
  
  # Apply trend pattern
  if (trend_type == "linear") {
    longitudinal_data$trend_component <- 0.02 * longitudinal_data$time_index
  } else if (trend_type == "seasonal") {
    longitudinal_data$trend_component <- 0.3 * sin(2 * pi * longitudinal_data$time_index / 12)
  } else if (trend_type == "cyclical") {
    longitudinal_data$trend_component <- 0.2 * cos(2 * pi * longitudinal_data$time_index / 24) +
                                       0.1 * sin(2 * pi * longitudinal_data$time_index / 6)
  } else if (trend_type == "volatile") {
    set.seed(456)
    longitudinal_data$trend_component <- cumsum(rnorm(nrow(longitudinal_data), 0, 0.05))
  } else {
    longitudinal_data$trend_component <- 0
  }
  
  # Calculate final satisfaction with noise
  longitudinal_data <- longitudinal_data %>%
    mutate(
      job_satisfaction = pmax(1, pmin(5, baseline_satisfaction + trend_component + 
                                     rnorm(n(), 0, 0.2))),
      environment_satisfaction = pmax(1, pmin(5, baseline_satisfaction + 
                                             0.8 * trend_component + rnorm(n(), 0, 0.25))),
      work_life_balance = pmax(1, pmin(5, baseline_satisfaction + 
                                      0.6 * trend_component + rnorm(n(), 0, 0.3)))
    )
  
  return(longitudinal_data)
}

# =============================================================================
# BIAS CORRECTION ALGORITHM TESTS
# =============================================================================

test_that("Response Bias Correction - Inverse Probability Weighting", {
  
  # Test 1: Basic IPW correction functionality
  test_data <- generate_mock_satisfaction_data(1000, "response", 0.4)
  
  # Mock function for response probability estimation
  estimate_response_probability <- function(data) {
    # Simple logistic model based on department and tenure
    model_data <- data %>%
      mutate(
        response = 1, # All observed data has response = 1
        dept_it = as.numeric(department == "IT"),
        high_tenure = as.numeric(tenure_years > 10)
      )
    
    # Simulate response probabilities (in real implementation, this would be estimated)
    model_data$response_prob <- pmax(0.1, pmin(0.9, 
      0.3 + 0.2 * model_data$dept_it + 0.1 * model_data$high_tenure + 
      0.05 * model_data$job_satisfaction))
    
    return(model_data$response_prob)
  }
  
  correct_response_bias <- function(data, satisfaction_var) {
    response_probs <- estimate_response_probability(data)
    weights <- 1 / response_probs
    
    # Weighted mean
    weighted_mean <- sum(data[[satisfaction_var]] * weights) / sum(weights)
    
    # Standard error calculation
    n <- nrow(data)
    weighted_var <- sum(weights * (data[[satisfaction_var]] - weighted_mean)^2) / 
                   (sum(weights) - sum(weights^2) / sum(weights))
    se <- sqrt(weighted_var / n)
    
    return(list(
      corrected_mean = weighted_mean,
      uncorrected_mean = mean(data[[satisfaction_var]]),
      standard_error = se,
      bias_magnitude = weighted_mean - mean(data[[satisfaction_var]])
    ))
  }
  
  # Apply correction
  result <- correct_response_bias(test_data, "job_satisfaction")
  
  # Tests
  expect_type(result, "list")
  expect_named(result, c("corrected_mean", "uncorrected_mean", "standard_error", "bias_magnitude"))
  expect_true(is.numeric(result$corrected_mean))
  expect_true(result$corrected_mean >= 1 && result$corrected_mean <= 5)
  expect_true(result$standard_error > 0)
  expect_true(abs(result$bias_magnitude) < 2) # Reasonable bias magnitude
})

test_that("Survivorship Bias Correction - Heckman Selection Model", {
  
  # Test 2: Heckman-style correction for survivorship bias
  test_data <- generate_mock_satisfaction_data(800, "survivorship", 0.5)
  
  correct_survivorship_bias <- function(data) {
    # Stage 1: Selection model (who stays vs. leaves)
    # In practice, this would use historical data including departed employees
    
    # Stage 2: Outcome model with correction
    # Simplified correction using propensity score matching approach
    
    # Calculate propensity scores for "survival" (staying in company)
    survival_model_data <- data %>%
      mutate(
        high_tenure = as.numeric(tenure_years > 5),
        it_dept = as.numeric(department == "IT"),
        older_employee = as.numeric(str_detect(age_group, "^(41|51)"))
      )
    
    # Simulate propensity scores (in real implementation, estimated from full data)
    survival_model_data$propensity_score <- pmax(0.1, pmin(0.9,
      0.4 + 0.2 * survival_model_data$high_tenure + 
      0.1 * survival_model_data$it_dept - 
      0.15 * (5 - survival_model_data$job_satisfaction)))
    
    # Apply correction weights
    weights <- 1 / survival_model_data$propensity_score
    
    # Calculate corrected statistics
    satisfaction_vars <- c("job_satisfaction", "environment_satisfaction", 
                          "work_life_balance", "relationship_satisfaction")
    
    results <- map(satisfaction_vars, function(var) {
      weighted_mean <- sum(survival_model_data[[var]] * weights) / sum(weights)
      uncorrected_mean <- mean(survival_model_data[[var]])
      
      list(
        variable = var,
        corrected_mean = weighted_mean,
        uncorrected_mean = uncorrected_mean,
        correction_magnitude = weighted_mean - uncorrected_mean
      )
    })
    
    return(results)
  }
  
  results <- correct_survivorship_bias(test_data)
  
  # Tests
  expect_length(results, 4)
  expect_true(all(map_lgl(results, ~is.list(.x))))
  expect_true(all(map_lgl(results, ~"corrected_mean" %in% names(.x))))
  expect_true(all(map_dbl(results, ~.x$corrected_mean) >= 1))
  expect_true(all(map_dbl(results, ~.x$corrected_mean) <= 5))
  
  # Survivorship bias should typically lead to upward correction
  correction_magnitudes <- map_dbl(results, ~.x$correction_magnitude)
  expect_true(mean(correction_magnitudes) < 0) # Corrected means should be lower
})

test_that("Demographic Bias Correction - Stratification and Standardization", {
  
  # Test 3: Demographic bias correction using direct standardization
  test_data <- generate_mock_satisfaction_data(1200, "demographic", 0.4)
  
  correct_demographic_bias <- function(data, target_population = NULL) {
    # Define target population distribution (e.g., US workforce demographics)
    if (is.null(target_population)) {
      target_population <- tibble(
        gender = c("Male", "Female", "Non-binary"),
        target_proportion = c(0.48, 0.47, 0.05)
      )
    }
    
    # Calculate stratum-specific means
    stratum_stats <- data %>%
      group_by(gender) %>%
      summarise(
        n = n(),
        job_satisfaction_mean = mean(job_satisfaction),
        environment_satisfaction_mean = mean(environment_satisfaction),
        work_life_balance_mean = mean(work_life_balance),
        .groups = "drop"
      ) %>%
      left_join(target_population, by = "gender") %>%
      mutate(
        observed_proportion = n / sum(n),
        weight = target_proportion / observed_proportion
      )
    
    # Calculate standardized means
    satisfaction_vars <- c("job_satisfaction", "environment_satisfaction", 
                          "work_life_balance")
    
    standardized_results <- map(satisfaction_vars, function(var) {
      var_col <- paste0(var, "_mean")
      
      standardized_mean <- sum(stratum_stats[[var_col]] * stratum_stats$target_proportion)
      observed_mean <- sum(stratum_stats[[var_col]] * stratum_stats$observed_proportion)
      
      list(
        variable = var,
        standardized_mean = standardized_mean,
        observed_mean = observed_mean,
        bias_correction = standardized_mean - observed_mean
      )
    })
    
    return(list(
      stratum_details = stratum_stats,
      standardized_results = standardized_results
    ))
  }
  
  result <- correct_demographic_bias(test_data)
  
  # Tests
  expect_type(result, "list")
  expect_named(result, c("stratum_details", "standardized_results"))
  expect_s3_class(result$stratum_details, "data.frame")
  expect_length(result$standardized_results, 3)
  
  # Check stratum details
  expect_true(all(c("gender", "weight", "target_proportion") %in% 
                 names(result$stratum_details)))
  expect_true(all(result$stratum_details$weight > 0))
  
  # Check standardized results
  standardized_means <- map_dbl(result$standardized_results, ~.x$standardized_mean)
  expect_true(all(standardized_means >= 1 & standardized_means <= 5))
})

test_that("Temporal Bias Correction - Detrending and Normalization", {
  
  # Test 4: Temporal bias correction for time-based measurement artifacts
  test_data <- generate_mock_satisfaction_data(600, "temporal", 0.6)
  
  correct_temporal_bias <- function(data) {
    # Add time-based features
    data_with_time <- data %>%
      arrange(review_date) %>%
      mutate(
        year = year(review_date),
        month = month(review_date),
        quarter = quarter(review_date),
        time_index = as.numeric(review_date - min(review_date)) / 365.25,
        year_month = floor_date(review_date, "month")
      )
    
    # Method 1: Linear detrending
    detrend_satisfaction <- function(satisfaction_scores, time_indices) {
      trend_model <- lm(satisfaction_scores ~ time_indices)
      detrended <- satisfaction_scores - predict(trend_model) + mean(satisfaction_scores)
      
      list(
        detrended_scores = detrended,
        trend_coefficient = coef(trend_model)[2],
        r_squared = summary(trend_model)$r.squared
      )
    }
    
    # Method 2: Moving average normalization
    normalize_by_period <- function(data, satisfaction_var) {
      data %>%
        group_by(year_month) %>%
        mutate(
          period_mean = mean(.data[[satisfaction_var]]),
          period_sd = sd(.data[[satisfaction_var]]),
          normalized_score = (.data[[satisfaction_var]] - period_mean) / 
                           ifelse(period_sd == 0, 1, period_sd)
        ) %>%
        ungroup() %>%
        mutate(
          # Re-scale to original range
          final_normalized = pmax(1, pmin(5, 3.5 + normalized_score))
        )
    }
    
    # Apply corrections
    satisfaction_vars <- c("job_satisfaction", "environment_satisfaction", 
                          "work_life_balance")
    
    correction_results <- map(satisfaction_vars, function(var) {
      # Linear detrending
      detrend_result <- detrend_satisfaction(data_with_time[[var]], 
                                           data_with_time$time_index)
      
      # Period normalization
      normalized_data <- normalize_by_period(data_with_time, var)
      
      list(
        variable = var,
        original_mean = mean(data_with_time[[var]]),
        detrended_mean = mean(detrend_result$detrended_scores),
        normalized_mean = mean(normalized_data$final_normalized),
        trend_strength = abs(detrend_result$trend_coefficient),
        trend_r_squared = detrend_result$r_squared
      )
    })
    
    return(correction_results)
  }
  
  results <- correct_temporal_bias(test_data)
  
  # Tests
  expect_length(results, 3)
  expect_true(all(map_lgl(results, ~is.list(.x))))
  
  # Check each correction result
  for (result in results) {
    expect_named(result, c("variable", "original_mean", "detrended_mean", 
                          "normalized_mean", "trend_strength", "trend_r_squared"))
    expect_true(result$original_mean >= 1 && result$original_mean <= 5)
    expect_true(result$detrended_mean >= 1 && result$detrended_mean <= 5)
    expect_true(result$normalized_mean >= 1 && result$normalized_mean <= 5)
    expect_true(result$trend_r_squared >= 0 && result$trend_r_squared <= 1)
  }
})

# =============================================================================
# LONGITUDINAL TREND ANALYSIS TESTS
# =============================================================================

test_that("Linear Trend Detection and Significance Testing", {
  
  # Test 5: Linear trend analysis with statistical significance
  linear_data <- generate_longitudinal_data(100, 36, "linear")
  
  analyze_linear_trends <- function(data) {
    # Analyze trends for each employee
    employee_trends <- data %>%
      group_by(employee_id) %>%
      do({
        if (nrow(.) < 3) return(tibble()) # Need minimum observations
        
        # Fit linear model
        time_numeric <- as.numeric(.data$review_date - min(.data$review_date)) / 365.25
        
        job_model <- lm(job_satisfaction ~ time_numeric, data = .)
        env_model <- lm(environment_satisfaction ~ time_numeric, data = .)
        wlb_model <- lm(work_life_balance ~ time_numeric, data = .)
        
        tibble(
          job_sat_slope = coef(job_model)[2],
          job_sat_pvalue = summary(job_model)$coefficients[2, 4],
          job_sat_r2 = summary(job_model)$r.squared,
          env_sat_slope = coef(env_model)[2],
          env_sat_pvalue = summary(env_model)$coefficients[2, 4],
          env_sat_r2 = summary(env_model)$r.squared,
          wlb_slope = coef(wlb_model)[2],
          wlb_pvalue = summary(wlb_model)$coefficients[2, 4],
          wlb_r2 = summary(wlb_model)$r.squared,
          n_observations = nrow(.)
        )
      }) %>%
      ungroup()
    
    # Aggregate statistics
    trend_summary <- employee_trends %>%
      summarise(
        across(ends_with("_slope"), list(mean = mean, median = median, sd = sd), 
               na.rm = TRUE, .names = "{.col}_{.fn}"),
        across(ends_with("_pvalue"), ~mean(.x < 0.05, na.rm = TRUE), 
               .names = "{.col}_prop_significant"),
        across(ends_with("_r2"), list(mean = mean, median = median), 
               na.rm = TRUE, .names = "{.col}_{.fn}")
      )
    
    return(list(
      individual_trends = employee_trends,
      summary_statistics = trend_summary
    ))
  }
  
  result <- analyze_linear_trends(linear_data)
  
  # Tests
  expect_type(result, "list")
  expect_named(result, c("individual_trends", "summary_statistics"))
  expect_s3_class(result$individual_trends, "data.frame")
  expect_s3_class(result$summary_statistics, "data.frame")
  
  # Check individual trends structure
  individual_trends <- result$individual_trends
  expected_cols <- c("employee_id", "job_sat_slope", "job_sat_pvalue", "job_sat_r2",
                    "env_sat_slope", "env_sat_pvalue", "env_sat_r2",
                    "wlb_slope", "wlb_pvalue", "wlb_r2", "n_observations")
  expect_true(all(expected_cols %in% names(individual_trends)))
  
  # Check that slopes are reasonable
  expect_true(all(abs(individual_trends$job_sat_slope) < 2, na.rm = TRUE))
  expect_true(all(individual_trends$job_sat_pvalue >= 0 & 
                 individual_trends$job_sat_pvalue <= 1, na.rm = TRUE))
  
  # For linear data, should detect positive trends
  expect_true(result$summary_statistics$job_sat_slope_mean > 0)
})

test_that("Seasonal Pattern Detection - Fourier Analysis", {
  
  # Test 6: Seasonal pattern detection using Fourier analysis
  seasonal_data <- generate_longitudinal_data(150, 48, "seasonal")
  
  detect_seasonal_patterns <- function(data, satisfaction_var = "job_satisfaction") {
    # Aggregate to monthly averages
    monthly_data <- data %>%
      mutate(year_month = floor_date(review_date, "month")) %>%
      group_by(year_month) %>%
      summarise(
        avg_satisfaction = mean(.data[[satisfaction_var]]),
        n_responses = n(),
        .groups = "drop"
      ) %>%
      arrange(year_month) %>%
      filter(n_responses >= 5) # Minimum sample size per month
    
    if (nrow(monthly_data) < 12) {
      return(list(error = "Insufficient data for seasonal analysis"))
    }
    
    # Prepare time series
    ts_data <- ts(monthly_data$avg_satisfaction, 
                  start = c(year(min(monthly_data$year_month)), 
                           month(min(monthly_data$year_month))), 
                  frequency = 12)
    
    # Fourier analysis for seasonality
    n <- length(ts_data)
    fft_result <- fft(ts_data)
    frequencies <- (0:(n-1)) / n
    power_spectrum <- Mod(fft_result)^2
    
    # Identify dominant frequencies (excluding DC component)
    seasonal_frequencies <- frequencies[2:(n/2)]
    seasonal_power <- power_spectrum[2:(n/2)]
    
    # Find peaks corresponding to annual cycles
    annual_freq_idx <- which.min(abs(seasonal_frequencies - 1/12))
    semi_annual_freq_idx <- which.min(abs(seasonal_frequencies - 2/12))
    
    # Statistical tests for seasonality
    # Kruskal-Wallis test by month
    monthly_data$month <- month(monthly_data$year_month)
    kw_test <- kruskal.test(avg_satisfaction ~ month, data = monthly_data)
    
    # Seasonal decomposition
    if (n >= 24) { # Need at least 2 years for decomposition
      decomp <- decompose(ts_data)
      seasonal_strength <- var(decomp$seasonal, na.rm = TRUE) / 
                          var(ts_data, na.rm = TRUE)
    } else {
      seasonal_strength <- NA
    }
    
    return(list(
      monthly_averages = monthly_data,
      annual_cycle_power = seasonal_power[annual_freq_idx],
      semi_annual_cycle_power = seasonal_power[semi_annual_freq_idx],
      seasonality_pvalue = kw_test$p.value,
      seasonal_strength = seasonal_strength,
      dominant_frequency = seasonal_frequencies[which.max(seasonal_power)],
      has_strong_seasonality = !is.na(seasonal_strength) && seasonal_strength > 0.1
    ))
  }
  
  result <- detect_seasonal_patterns(seasonal_data, "job_satisfaction")
  
  # Tests
  expect_type(result, "list")
  expect_false("error" %in% names(result))
  
  # Check required components
  expected_components <- c("monthly_averages", "annual_cycle_power", 
                          "seasonality_pvalue", "seasonal_strength",
                          "has_strong_seasonality")
  expect_true(all(expected_components %in% names(result)))
  
  # Check data types and ranges
  expect_s3_class(result$monthly_averages, "data.frame")
  expect_true(is.numeric(result$annual_cycle_power))
  expect_true(result$seasonality_pvalue >= 0 && result$seasonality_pvalue <= 1)
  expect_true(is.logical(result$has_strong_seasonality))
  
  # For seasonal data, should detect seasonality
  expect_true(result$seasonality_pvalue < 0.1) # Should be significant
})

test_that("Change Point Detection - CUSUM and Structural Breaks", {
  
  # Test 7: Change point detection for satisfaction trends
  # Create data with known change points
  change_point_data <- bind_rows(
    generate_longitudinal_data(50, 20, "linear") %>% 
      mutate(job_satisfaction = job_satisfaction + 0.5), # Higher baseline
    generate_longitudinal_data(50, 20, "linear") %>%
      mutate(review_date = review_date + months(20),
             job_satisfaction = job_satisfaction - 0.3) # Lower baseline after change
  ) %>%
    arrange(employee_id, review_date)
  
  detect_change_points <- function(data, satisfaction_var = "job_satisfaction") {
    # Aggregate to time series
    ts_data <- data %>%
      mutate(year_month = floor_date(review_date, "month")) %>%
      group_by(year_month) %>%
      summarise(avg_satisfaction = mean(.data[[satisfaction_var]]), .groups = "drop") %>%
      arrange(year_month)
    
    if (nrow(ts_data) < 10) {
      return(list(error = "Insufficient data for change point detection"))
    }
    
    values <- ts_data$avg_satisfaction
    n <- length(values)
    
    # CUSUM approach
    overall_mean <- mean(values)
    cusum_pos <- cumsum(pmax(0, values - overall_mean))
    cusum_neg <- cumsum(pmax(0, overall_mean - values))
    
    # Detect change points using CUSUM
    threshold <- 2 * sd(values) # Configurable threshold
    cusum_change_points <- which(cusum_pos > threshold | cusum_neg > threshold)
    
    # Simple structural break test (Chow test approximation)
    # Test each potential change point
    chow_test_results <- map_dfr(seq(3, n-3), function(cp) {
      # Fit models before and after change point
      before_data <- tibble(y = values[1:cp], x = 1:cp)
      after_data <- tibble(y = values[(cp+1):n], x = (cp+1):n)
      
      # Calculate residual sum of squares
      before_model <- lm(y ~ x, data = before_data)
      after_model <- lm(y ~ x, data = after_data)
      full_model <- lm(values ~ seq_along(values))
      
      rss_separate <- sum(residuals(before_model)^2) + sum(residuals(after_model)^2)
      rss_full <- sum(residuals(full_model)^2)
      
      # F-statistic for structural break
      f_stat <- ((rss_full - rss_separate) / 2) / (rss_separate / (n - 4))
      p_value <- 1 - pf(f_stat, 2, n - 4)
      
      tibble(
        change_point = cp,
        time_period = ts_data$year_month[cp],
        f_statistic = f_stat,
        p_value = p_value
      )
    })
    
    # Identify significant change points
    significant_changes <- chow_test_results %>%
      filter(p_value < 0.05) %>%
      arrange(p_value)
    
    return(list(
      time_series_data = ts_data,
      cusum_positive = cusum_pos,
      cusum_negative = cusum_neg,
      cusum_change_points = cusum_change_points,
      structural_break_tests = chow_test_results,
      significant_change_points = significant_changes,
      has