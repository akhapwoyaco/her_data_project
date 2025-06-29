# =============================================================================
# ATLAS LABS HR ANALYTICS - PERFORMANCE MODULE UNIT TESTS
# =============================================================================
# Comprehensive unit tests for Performance Module analytics functions
# Testing: Rating distributions, comparisons, training impact, trends, 
#          outliers, normalization, scoring, and prediction models
# =============================================================================

library(testthat)
library(tidyverse)
library(lubridate)
library(broom)
library(caret)
library(randomForest)

# Mock data generation for consistent testing
generate_mock_performance_data <- function(n = 1000, seed = 123) {
  set.seed(seed)
  
  # Employee data
  employees <- tibble(
    EmployeeID = 1:n,
    Department = sample(c("IT", "HR", "Finance", "Sales", "Marketing"), n, replace = TRUE),
    JobRole = sample(c("Analyst", "Manager", "Director", "Associate", "Senior"), n, replace = TRUE),
    YearsAtCompany = pmax(1, rnorm(n, 5, 3)),
    Age = pmax(22, rnorm(n, 35, 8)),
    Salary = pmax(30000, rnorm(n, 65000, 20000))
  )
  
  # Performance ratings data
  performance <- tibble(
    PerformanceID = 1:(n*2),
    EmployeeID = rep(1:n, 2),
    ReviewDate = rep(c(today() - months(12), today() - months(6)), each = n),
    EnvironmentSatisfaction = sample(1:5, n*2, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.35, 0.1)),
    JobSatisfaction = sample(1:5, n*2, replace = TRUE, prob = c(0.05, 0.1, 0.25, 0.4, 0.2)),
    RelationshipSatisfaction = sample(1:5, n*2, replace = TRUE, prob = c(0.08, 0.12, 0.3, 0.35, 0.15)),
    WorkLifeBalance = sample(1:5, n*2, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.35, 0.15)),
    SelfRating = sample(1:5, n*2, replace = TRUE, prob = c(0.02, 0.08, 0.25, 0.45, 0.2)),
    ManagerRating = pmax(1, pmin(5, round(rep(employees$YearsAtCompany, 2) * 0.3 + 
                                          rnorm(n*2, 3.2, 0.8)))),
    TrainingOpportunitiesWithinYear = sample(0:12, n*2, replace = TRUE, prob = c(0.1, rep(0.075, 12))),
    TrainingOpportunitiesTaken = pmax(0, sample(0:8, n*2, replace = TRUE))
  ) %>%
    mutate(
      TrainingOpportunitiesTaken = pmin(TrainingOpportunitiesTaken, TrainingOpportunitiesWithinYear),
      # Add some realistic bias to manager ratings
      ManagerRating = case_when(
        SelfRating >= 4 & ManagerRating < SelfRating ~ SelfRating - sample(0:1, n(), replace = TRUE),
        SelfRating <= 2 & ManagerRating > SelfRating ~ SelfRating + sample(0:1, n(), replace = TRUE),
        TRUE ~ ManagerRating
      )
    )
  
  list(employees = employees, performance = performance)
}

# =============================================================================
# RATING DISTRIBUTION ANALYSIS TESTS
# =============================================================================

test_that("Rating distribution analysis functions correctly", {
  
  # Setup
  mock_data <- generate_mock_performance_data(500)
  performance_data <- mock_data$performance
  
  # Function to test
  analyze_rating_distribution <- function(data, rating_col) {
    data %>%
      group_by(!!sym(rating_col)) %>%
      summarise(
        count = n(),
        percentage = round(n() / nrow(data) * 100, 2),
        .groups = "drop"
      ) %>%
      arrange(!!sym(rating_col)) %>%
      mutate(
        cumulative_pct = cumsum(percentage),
        is_normal_dist = between(percentage, 5, 40) # Reasonable distribution check
      )
  }
  
  # Test self-rating distribution
  self_dist <- analyze_rating_distribution(performance_data, "SelfRating")
  
  # Tests for self-rating distribution
  expect_true(nrow(self_dist) == 5, "Should have 5 rating levels")
  expect_true(all(self_dist$SelfRating %in% 1:5), "Ratings should be 1-5")
  expect_equal(sum(self_dist$count), nrow(performance_data), "Counts should sum to total records")
  expect_equal(round(sum(self_dist$percentage), 1), 100.0, "Percentages should sum to 100")
  expect_true(max(self_dist$cumulative_pct) == 100, "Cumulative should reach 100%")
  
  # Test manager rating distribution
  mgr_dist <- analyze_rating_distribution(performance_data, "ManagerRating")
  
  expect_true(nrow(mgr_dist) == 5, "Manager ratings should have 5 levels")
  expect_equal(sum(mgr_dist$count), nrow(performance_data), "Manager rating counts should match total")
  
  # Test distribution properties
  expect_true(all(self_dist$percentage >= 0), "All percentages should be non-negative")
  expect_true(all(mgr_dist$percentage >= 0), "All manager percentages should be non-negative")
})

test_that("Rating distribution handles edge cases", {
  
  # Edge case: Single rating value
  single_rating <- tibble(SelfRating = rep(3, 100))
  dist_single <- single_rating %>%
    group_by(SelfRating) %>%
    summarise(count = n(), percentage = n()/nrow(single_rating)*100, .groups = "drop")
  
  expect_equal(nrow(dist_single), 1, "Single rating should result in one row")
  expect_equal(dist_single$percentage, 100, "Single rating should be 100%")
  
  # Edge case: Empty data
  empty_data <- tibble(SelfRating = numeric(0))
  expect_error(
    empty_data %>% group_by(SelfRating) %>% summarise(count = n(), .groups = "drop"),
    NA # Should not error on empty grouping
  )
})

# =============================================================================
# MANAGER VS SELF-RATING COMPARISON TESTS
# =============================================================================

test_that("Manager vs self-rating comparison analysis works correctly", {
  
  # Setup
  mock_data <- generate_mock_performance_data(300)
  performance_data <- mock_data$performance
  
  # Function to test
  compare_manager_self_ratings <- function(data) {
    comparison <- data %>%
      select(EmployeeID, ReviewDate, SelfRating, ManagerRating) %>%
      mutate(
        rating_difference = ManagerRating - SelfRating,
        agreement_level = case_when(
          rating_difference == 0 ~ "Perfect Agreement",
          abs(rating_difference) == 1 ~ "Close Agreement",
          abs(rating_difference) >= 2 ~ "Significant Disagreement"
        ),
        bias_direction = case_when(
          rating_difference > 0 ~ "Manager Higher",
          rating_difference < 0 ~ "Self Higher",
          TRUE ~ "Equal"
        )
      )
    
    # Summary statistics
    summary_stats <- list(
      mean_difference = mean(comparison$rating_difference, na.rm = TRUE),
      median_difference = median(comparison$rating_difference, na.rm = TRUE),
      sd_difference = sd(comparison$rating_difference, na.rm = TRUE),
      correlation = cor(comparison$SelfRating, comparison$ManagerRating, use = "complete.obs"),
      agreement_distribution = table(comparison$agreement_level),
      bias_distribution = table(comparison$bias_direction)
    )
    
    list(comparison = comparison, summary = summary_stats)
  }
  
  # Run comparison analysis
  rating_comparison <- compare_manager_self_ratings(performance_data)
  
  # Test comparison data structure
  expect_true("rating_difference" %in% names(rating_comparison$comparison), 
              "Should calculate rating difference")
  expect_true("agreement_level" %in% names(rating_comparison$comparison), 
              "Should categorize agreement levels")
  expect_true("bias_direction" %in% names(rating_comparison$comparison), 
              "Should identify bias direction")
  
  # Test summary statistics
  expect_true(is.numeric(rating_comparison$summary$mean_difference), 
              "Mean difference should be numeric")
  expect_true(is.numeric(rating_comparison$summary$correlation), 
              "Correlation should be numeric")
  expect_true(rating_comparison$summary$correlation >= -1 && rating_comparison$summary$correlation <= 1, 
              "Correlation should be between -1 and 1")
  
  # Test agreement level categorization
  agreement_levels <- names(rating_comparison$summary$agreement_distribution)
  expected_levels <- c("Perfect Agreement", "Close Agreement", "Significant Disagreement")
  expect_true(all(agreement_levels %in% expected_levels), 
              "Agreement levels should match expected categories")
  
  # Test bias direction categorization
  bias_directions <- names(rating_comparison$summary$bias_distribution)
  expected_bias <- c("Manager Higher", "Self Higher", "Equal")
  expect_true(all(bias_directions %in% expected_bias), 
              "Bias directions should match expected categories")
})

test_that("Rating comparison handles statistical edge cases", {
  
  # Perfect correlation case
  perfect_corr_data <- tibble(
    SelfRating = 1:5,
    ManagerRating = 1:5
  )
  
  correlation <- cor(perfect_corr_data$SelfRating, perfect_corr_data$ManagerRating)
  expect_equal(correlation, 1, "Perfect positive correlation should be 1")
  
  # Perfect negative correlation case
  negative_corr_data <- tibble(
    SelfRating = 1:5,
    ManagerRating = 5:1
  )
  
  neg_correlation <- cor(negative_corr_data$SelfRating, negative_corr_data$ManagerRating)
  expect_equal(neg_correlation, -1, "Perfect negative correlation should be -1")
})

# =============================================================================
# TRAINING IMPACT ANALYSIS TESTS
# =============================================================================

test_that("Training impact analysis calculates correctly", {
  
  # Setup
  mock_data <- generate_mock_performance_data(400)
  performance_data <- mock_data$performance %>%
    # Add realistic training impact
    mutate(
      performance_score = (SelfRating + ManagerRating) / 2,
      training_utilization = TrainingOpportunitiesTaken / pmax(1, TrainingOpportunitiesWithinYear),
      # Simulate positive training impact
      adjusted_performance = performance_score + (training_utilization * 0.3) + rnorm(n(), 0, 0.2)
    )
  
  # Function to test
  analyze_training_impact <- function(data) {
    
    # Calculate training metrics
    training_analysis <- data %>%
      mutate(
        training_category = case_when(
          TrainingOpportunitiesTaken == 0 ~ "No Training",
          training_utilization < 0.5 ~ "Low Training",
          training_utilization < 0.8 ~ "Moderate Training",
          TRUE ~ "High Training"
        )
      ) %>%
      group_by(training_category) %>%
      summarise(
        avg_performance = mean(performance_score, na.rm = TRUE),
        avg_manager_rating = mean(ManagerRating, na.rm = TRUE),
        avg_self_rating = mean(SelfRating, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      )
    
    # Statistical significance test
    high_training <- data %>% filter(training_utilization >= 0.8)
    no_training <- data %>% filter(TrainingOpportunitiesTaken == 0)
    
    if(nrow(high_training) > 10 && nrow(no_training) > 10) {
      t_test_result <- t.test(high_training$performance_score, no_training$performance_score)
      effect_size <- (mean(high_training$performance_score, na.rm = TRUE) - 
                     mean(no_training$performance_score, na.rm = TRUE)) / 
                     sd(data$performance_score, na.rm = TRUE)
    } else {
      t_test_result <- NULL
      effect_size <- NA
    }
    
    # Correlation analysis
    correlation_training_performance <- cor(data$TrainingOpportunitiesTaken, 
                                          data$performance_score, 
                                          use = "complete.obs")
    
    list(
      training_analysis = training_analysis,
      t_test = t_test_result,
      effect_size = effect_size,
      correlation = correlation_training_performance
    )
  }
  
  # Run analysis
  training_impact <- analyze_training_impact(performance_data)
  
  # Test training analysis structure
  expect_true(is.data.frame(training_impact$training_analysis), 
              "Training analysis should return a dataframe")
  expect_true("training_category" %in% names(training_impact$training_analysis), 
              "Should categorize training levels")
  expect_true("avg_performance" %in% names(training_impact$training_analysis), 
              "Should calculate average performance by training category")
  
  # Test statistical measures
  expect_true(is.numeric(training_impact$correlation), 
              "Correlation should be numeric")
  expect_true(training_impact$correlation >= -1 && training_impact$correlation <= 1, 
              "Correlation should be valid")
  
  # Test training categories
  expected_categories <- c("No Training", "Low Training", "Moderate Training", "High Training")
  actual_categories <- training_impact$training_analysis$training_category
  expect_true(all(actual_categories %in% expected_categories), 
              "Training categories should match expected values")
  
  # Test performance calculations
  expect_true(all(training_impact$training_analysis$avg_performance >= 1 & 
                 training_impact$training_analysis$avg_performance <= 5), 
              "Average performance should be within valid range")
})

test_that("Training impact analysis handles missing data", {
  
  # Data with missing training information
  incomplete_data <- tibble(
    TrainingOpportunitiesTaken = c(1, 2, NA, 4, 5),
    TrainingOpportunitiesWithinYear = c(10, 8, 6, NA, 12),
    SelfRating = c(3, 4, 3, 5, 4),
    ManagerRating = c(3, 4, 4, 4, 5)
  ) %>%
    mutate(performance_score = (SelfRating + ManagerRating) / 2)
  
  # Should handle missing data gracefully
  expect_no_error({
    result <- incomplete_data %>%
      filter(!is.na(TrainingOpportunitiesTaken), !is.na(TrainingOpportunitiesWithinYear)) %>%
      summarise(correlation = cor(TrainingOpportunitiesTaken, performance_score, use = "complete.obs"))
  })
})

# =============================================================================
# PERFORMANCE TREND CALCULATIONS TESTS
# =============================================================================

test_that("Performance trend calculations work correctly", {
  
  # Setup time series data
  mock_data <- generate_mock_performance_data(200)
  trend_data <- mock_data$performance %>%
    arrange(EmployeeID, ReviewDate) %>%
    group_by(EmployeeID) %>%
    mutate(
      performance_score = (SelfRating + ManagerRating) / 2,
      review_period = row_number()
    ) %>%
    ungroup()
  
  # Function to test
  calculate_performance_trends <- function(data) {
    
    # Individual employee trends
    individual_trends <- data %>%
      group_by(EmployeeID) %>%
      arrange(ReviewDate) %>%
      summarise(
        trend_slope = if(n() >= 2) {
          lm(performance_score ~ review_period)$coefficients[2]
        } else {
          NA_real_
        },
        performance_change = if(n() >= 2) {
          last(performance_score) - first(performance_score)
        } else {
          NA_real_
        },
        trend_direction = case_when(
          is.na(trend_slope) ~ "Insufficient Data",
          trend_slope > 0.1 ~ "Improving",
          trend_slope < -0.1 ~ "Declining",
          TRUE ~ "Stable"
        ),
        reviews_count = n(),
        .groups = "drop"
      )
    
    # Overall organizational trends
    org_trend <- data %>%
      group_by(ReviewDate) %>%
      summarise(
        avg_performance = mean(performance_score, na.rm = TRUE),
        median_performance = median(performance_score, na.rm = TRUE),
        performance_variance = var(performance_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(ReviewDate) %>%
      mutate(
        period = row_number(),
        org_trend_slope = if(n() >= 2) {
          lm(avg_performance ~ period)$coefficients[2]
        } else {
          NA_real_
        }
      )
    
    # Trend summary statistics
    trend_summary <- list(
      improving_employees = sum(individual_trends$trend_direction == "Improving", na.rm = TRUE),
      declining_employees = sum(individual_trends$trend_direction == "Declining", na.rm = TRUE),
      stable_employees = sum(individual_trends$trend_direction == "Stable", na.rm = TRUE),
      avg_slope = mean(individual_trends$trend_slope, na.rm = TRUE),
      org_trend_slope = unique(org_trend$org_trend_slope)[1]
    )
    
    list(
      individual_trends = individual_trends,
      org_trend = org_trend,
      summary = trend_summary
    )
  }
  
  # Run trend analysis
  trends <- calculate_performance_trends(trend_data)
  
  # Test individual trends structure
  expect_true(is.data.frame(trends$individual_trends), 
              "Individual trends should be a dataframe")
  expect_true("trend_slope" %in% names(trends$individual_trends), 
              "Should calculate trend slope")
  expect_true("trend_direction" %in% names(trends$individual_trends), 
              "Should categorize trend direction")
  expect_true("performance_change" %in% names(trends$individual_trends), 
              "Should calculate performance change")
  
  # Test organizational trends
  expect_true(is.data.frame(trends$org_trend), 
              "Organizational trend should be a dataframe")
  expect_true("avg_performance" %in% names(trends$org_trend), 
              "Should calculate average performance by period")
  
  # Test trend direction categorization
  trend_directions <- unique(trends$individual_trends$trend_direction)
  expected_directions <- c("Improving", "Declining", "Stable", "Insufficient Data")
  expect_true(all(trend_directions %in% expected_directions), 
              "Trend directions should match expected categories")
  
  # Test summary statistics
  expect_true(is.numeric(trends$summary$improving_employees), 
              "Count of improving employees should be numeric")
  expect_true(trends$summary$improving_employees >= 0, 
              "Count should be non-negative")
  
  # Test slope calculations are reasonable
  valid_slopes <- trends$individual_trends$trend_slope[!is.na(trends$individual_trends$trend_slope)]
  expect_true(all(abs(valid_slopes) <= 5), 
              "Trend slopes should be within reasonable range")
})

test_that("Performance trends handle single data points", {
  
  # Single review per employee
  single_review_data <- tibble(
    EmployeeID = 1:10,
    ReviewDate = rep(today(), 10),
    SelfRating = sample(1:5, 10, replace = TRUE),
    ManagerRating = sample(1:5, 10, replace = TRUE)
  ) %>%
    mutate(performance_score = (SelfRating + ManagerRating) / 2)
  
  # Should handle single points gracefully
  expect_no_error({
    single_trends <- single_review_data %>%
      group_by(EmployeeID) %>%
      summarise(
        trend_slope = if(n() >= 2) lm(performance_score ~ row_number())$coefficients[2] else NA_real_,
        .groups = "drop"
      )
  })
  
  expect_true(all(is.na(single_trends$trend_slope)), 
              "Single data points should result in NA slopes")
})

# =============================================================================
# OUTLIER DETECTION ACCURACY TESTS
# =============================================================================

test_that("Outlier detection identifies anomalies correctly", {
  
  # Setup data with known outliers
  set.seed(456)
  normal_performance <- rnorm(950, mean = 3.5, sd = 0.8)
  outlier_performance <- c(rep(1, 25), rep(5, 25))  # Known outliers
  all_performance <- c(normal_performance, outlier_performance)
  
  outlier_data <- tibble(
    EmployeeID = 1:1000,
    performance_score = all_performance,
    is_known_outlier = c(rep(FALSE, 950), rep(TRUE, 50))
  )
  
  # Function to test - Multiple outlier detection methods
  detect_performance_outliers <- function(data, method = "iqr") {
    
    if(method == "iqr") {
      # IQR method
      Q1 <- quantile(data$performance_score, 0.25, na.rm = TRUE)
      Q3 <- quantile(data$performance_score, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      data <- data %>%
        mutate(
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR,
          is_outlier_iqr = performance_score < lower_bound | performance_score > upper_bound
        )
      
    } else if(method == "zscore") {
      # Z-score method
      data <- data %>%
        mutate(
          z_score = abs((performance_score - mean(performance_score, na.rm = TRUE)) / 
                       sd(performance_score, na.rm = TRUE)),
          is_outlier_zscore = z_score > 2.5
        )
      
    } else if(method == "modified_zscore") {
      # Modified Z-score using median
      median_score <- median(data$performance_score, na.rm = TRUE)
      mad_score <- mad(data$performance_score, na.rm = TRUE)
      
      data <- data %>%
        mutate(
          modified_z_score = abs(0.6745 * (performance_score - median_score) / mad_score),
          is_outlier_modified = modified_z_score > 3.5
        )
    }
    
    return(data)
  }
  
  # Test IQR method
  iqr_results <- detect_performance_outliers(outlier_data, "iqr")
  
  expect_true("is_outlier_iqr" %in% names(iqr_results), 
              "Should create outlier flag column")
  expect_true(is.logical(iqr_results$is_outlier_iqr), 
              "Outlier flag should be logical")
  
  # Test Z-score method
  zscore_results <- detect_performance_outliers(outlier_data, "zscore")
  
  expect_true("z_score" %in% names(zscore_results), 
              "Should calculate Z-scores")
  expect_true("is_outlier_zscore" %in% names(zscore_results), 
              "Should create Z-score outlier flag")
  expect_true(all(zscore_results$z_score >= 0), 
              "Z-scores should be non-negative (absolute values)")
  
  # Test Modified Z-score method
  modified_results <- detect_performance_outliers(outlier_data, "modified_zscore")
  
  expect_true("modified_z_score" %in% names(modified_results), 
              "Should calculate modified Z-scores")
  expect_true("is_outlier_modified" %in% names(modified_results), 
              "Should create modified Z-score outlier flag")
  
  # Test detection accuracy (should catch some of the known outliers)
  iqr_accuracy <- sum(iqr_results$is_outlier_iqr & iqr_results$is_known_outlier) / 
                  sum(iqr_results$is_known_outlier)
  
  expect_true(iqr_accuracy > 0.3, 
              "IQR method should detect at least 30% of known outliers")
  
  # Test false positive rate
  iqr_false_positive <- sum(iqr_results$is_outlier_iqr & !iqr_results$is_known_outlier) / 
                       sum(!iqr_results$is_known_outlier)
  
  expect_true(iqr_false_positive < 0.1, 
              "IQR method should have less than 10% false positive rate")
})

test_that("Outlier detection handles edge cases", {
  
  # All identical values
  identical_data <- tibble(performance_score = rep(3, 100))
  
  expect_no_error({
    identical_outliers <- identical_data %>%
      mutate(
        z_score = abs((performance_score - mean(performance_score)) / sd(performance_score)),
        is_outlier = !is.na(z_score) & z_score > 2
      )
  })
  
  # Single value
  single_value <- tibble(performance_score = 3.5)
  
  expect_no_error({
    single_outlier <- single_value %>%
      mutate(is_outlier = FALSE)  # Single value cannot be outlier
  })
})

# =============================================================================
# NORMALIZATION PROCEDURES TESTS
# =============================================================================

test_that("Normalization procedures work correctly", {
  
  # Setup data with different scales
  mixed_scale_data <- tibble(
    EmployeeID = 1:100,
    performance_score = runif(100, 1, 5),
    salary = runif(100, 30000, 120000),
    years_experience = runif(100, 0, 25),
    training_hours = runif(100, 0, 80)
  )
  
  # Function to test - Multiple normalization methods
  normalize_performance_data <- function(data, method = "min_max") {
    
    numeric_cols <- c("performance_score", "salary", "years_experience", "training_hours")
    
    if(method == "min_max") {
      # Min-Max normalization (0-1 scale)
      normalized_data <- data %>%
        mutate(across(all_of(numeric_cols), 
                     ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
                     .names = "{.col}_normalized"))
      
    } else if(method == "z_score") {
      # Z-score standardization
      normalized_data <- data %>%
        mutate(across(all_of(numeric_cols),
                     ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
                     .names = "{.col}_standardized"))
      
    } else if(method == "robust") {
      # Robust scaling using median and MAD
      normalized_data <- data %>%
        mutate(across(all_of(numeric_cols),
                     ~ (.x - median(.x, na.rm = TRUE)) / mad(.x, na.rm = TRUE),
                     .names = "{.col}_robust"))
      
    } else if(method == "decimal_scaling") {
      # Decimal scaling
      normalized_data <- data %>%
        mutate(across(all_of(numeric_cols),
                     ~ .x / (10^ceiling(log10(max(abs(.x), na.rm = TRUE)))),
                     .names = "{.col}_decimal"))
    }
    
    return(normalized_data)
  }
  
  # Test Min-Max normalization
  minmax_normalized <- normalize_performance_data(mixed_scale_data, "min_max")
  
  # Check if normalized columns exist
  expect_true("performance_score_normalized" %in% names(minmax_normalized), 
              "Should create normalized performance score column")
  expect_true("salary_normalized" %in% names(minmax_normalized), 
              "Should create normalized salary column")
  
  # Check Min-Max range (0-1)
  expect_true(all(minmax_normalized$performance_score_normalized >= 0 & 
                 minmax_normalized$performance_score_normalized <= 1, na.rm = TRUE), 
              "Min-Max normalized values should be between 0 and 1")
  expect_true(all(minmax_normalized$salary_normalized >= 0 & 
                 minmax_normalized$salary_normalized <= 1, na.rm = TRUE), 
              "Min-Max normalized salary should be between 0 and 1")
  
  # Test Z-score standardization
  zscore_normalized <- normalize_performance_data(mixed_scale_data, "z_score")
  
  expect_true("performance_score_standardized" %in% names(zscore_normalized), 
              "Should create standardized performance score column")
  
  # Check Z-score properties (mean ≈ 0, sd ≈ 1)
  expect_true(abs(mean(zscore_normalized$performance_score_standardized, na.rm = TRUE)) < 0.01, 
              "Standardized mean should be approximately 0")
  expect_true(abs(sd(zscore_normalized$performance_score_standardized, na.rm = TRUE) - 1) < 0.01, 
              "Standardized standard deviation should be approximately 1")
  
  # Test Robust scaling
  robust_normalized <- normalize_performance_data(mixed_scale_data, "robust")
  
  expect_true("performance_score_robust" %in% names(robust_normalized), 
              "Should create robust scaled performance score column")
  
  #