# ==============================================================================
# ATLAS LABS HR ANALYTICS - SATISFACTION MODULE UNIT TESTS
# ==============================================================================
# Comprehensive unit tests for satisfaction module core functionalities
# Developer: akhapwoyaco (GitHub)
# ==============================================================================

# Load required libraries for testing
library(testthat)
library(tidyverse)
library(lubridate)
library(mockery)

# Source the satisfaction module (assuming it exists)
# source("modules/satisfaction_module.R")

# ==============================================================================
# TEST FIXTURES AND MOCK DATA
# ==============================================================================

# Create mock satisfaction data for testing
create_mock_satisfaction_data <- function(n = 100) {
  set.seed(123)
  
  tibble(
    employee_id = 1:n,
    response_date = seq(
      from = as.Date("2023-01-01"),
      to = as.Date("2024-12-31"),
      length.out = n
    ),
    job_satisfaction = sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1)),
    environment_satisfaction = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.15, 0.25, 0.35, 0.2)),
    relationship_satisfaction = sample(1:5, n, replace = TRUE, prob = c(0.08, 0.17, 0.3, 0.3, 0.15)),
    work_life_balance = sample(1:5, n, replace = TRUE, prob = c(0.12, 0.23, 0.3, 0.25, 0.1)),
    manager_rating = sample(1:5, n, replace = TRUE),
    self_rating = sample(1:5, n, replace = TRUE),
    department = sample(c("IT", "HR", "Sales", "Marketing", "Finance"), n, replace = TRUE),
    tenure_years = runif(n, 0.5, 15),
    salary = runif(n, 40000, 120000),
    attrition = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),
    survey_completion_status = sample(c("Complete", "Partial", "Not_Started"), n, 
                                    replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    response_time_minutes = rnorm(n, 15, 5),
    text_feedback = sample(c("Great workplace", "Could be better", "Love the team", 
                           "Need more flexibility", NA), n, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
  )
}

# Create longitudinal mock data
create_longitudinal_data <- function(employees = 50, time_points = 6) {
  expand_grid(
    employee_id = 1:employees,
    survey_period = 1:time_points
  ) %>%
    mutate(
      response_date = as.Date("2023-01-01") + months(survey_period * 2),
      job_satisfaction = pmax(1, pmin(5, 3 + rnorm(n(), 0, 1) + 
                                      ifelse(survey_period > 3, -0.2, 0))),
      environment_satisfaction = pmax(1, pmin(5, 3.2 + rnorm(n(), 0, 0.8))),
      relationship_satisfaction = pmax(1, pmin(5, 3.5 + rnorm(n(), 0, 0.7))),
      work_life_balance = pmax(1, pmin(5, 2.8 + rnorm(n(), 0, 1.1)))
    ) %>%
    round(1)
}

# ==============================================================================
# SURVEY RESPONSE PROCESSING TESTS
# ==============================================================================

test_that("Survey Response Processing - Data Validation", {
  mock_data <- create_mock_satisfaction_data(50)
  
  # Test data structure validation
  expect_true(all(c("employee_id", "job_satisfaction", "environment_satisfaction", 
                   "relationship_satisfaction", "work_life_balance") %in% names(mock_data)))
  
  # Test satisfaction score ranges
  expect_true(all(mock_data$job_satisfaction >= 1 & mock_data$job_satisfaction <= 5))
  expect_true(all(mock_data$environment_satisfaction >= 1 & mock_data$environment_satisfaction <= 5))
  expect_true(all(mock_data$relationship_satisfaction >= 1 & mock_data$relationship_satisfaction <= 5))
  expect_true(all(mock_data$work_life_balance >= 1 & mock_data$work_life_balance <= 5))
})

test_that("Survey Response Processing - Missing Data Handling", {
  # Create data with missing values
  mock_data <- create_mock_satisfaction_data(30)
  mock_data$job_satisfaction[c(1, 5, 10)] <- NA
  mock_data$environment_satisfaction[c(2, 7)] <- NA
  
  # Function to handle missing data (mock implementation)
  handle_missing_responses <- function(data) {
    data %>%
      mutate(
        job_satisfaction_imputed = ifelse(is.na(job_satisfaction), 
                                        round(mean(job_satisfaction, na.rm = TRUE)), 
                                        job_satisfaction),
        missing_responses = rowSums(is.na(select(., ends_with("satisfaction"), work_life_balance)))
      )
  }
  
  processed_data <- handle_missing_responses(mock_data)
  
  # Test missing data identification
  expect_true("missing_responses" %in% names(processed_data))
  expect_true(all(processed_data$missing_responses >= 0))
  
  # Test imputation logic
  expect_true(all(!is.na(processed_data$job_satisfaction_imputed)))
})

test_that("Survey Response Processing - Response Quality Assessment", {
  mock_data <- create_mock_satisfaction_data(40)
  
  # Function to assess response quality
  assess_response_quality <- function(data) {
    data %>%
      mutate(
        # Straight-lining detection (all same scores)
        straight_line = (job_satisfaction == environment_satisfaction & 
                        environment_satisfaction == relationship_satisfaction & 
                        relationship_satisfaction == work_life_balance),
        
        # Response time quality
        response_quality = case_when(
          response_time_minutes < 2 ~ "Too_Fast",
          response_time_minutes > 45 ~ "Too_Slow", 
          TRUE ~ "Normal"
        ),
        
        # Completion status quality
        quality_flag = case_when(
          straight_line ~ "Straight_Line",
          survey_completion_status != "Complete" ~ "Incomplete",
          response_quality != "Normal" ~ response_quality,
          TRUE ~ "Valid"
        )
      )
  }
  
  quality_data <- assess_response_quality(mock_data)
  
  expect_true("quality_flag" %in% names(quality_data))
  expect_true("straight_line" %in% names(quality_data))
  expect_true(all(quality_data$quality_flag %in% c("Valid", "Straight_Line", "Incomplete", "Too_Fast", "Too_Slow")))
})

# ==============================================================================
# SATISFACTION SCORE CALCULATIONS TESTS
# ==============================================================================

test_that("Satisfaction Score Calculations - Composite Scores", {
  mock_data <- create_mock_satisfaction_data(60)
  
  # Function to calculate composite satisfaction scores
  calculate_composite_scores <- function(data) {
    data %>%
      mutate(
        # Simple average composite
        composite_satisfaction_avg = (job_satisfaction + environment_satisfaction + 
                                    relationship_satisfaction + work_life_balance) / 4,
        
        # Weighted composite (job satisfaction weighted higher)
        composite_satisfaction_weighted = (job_satisfaction * 0.4 + 
                                         environment_satisfaction * 0.25 +
                                         relationship_satisfaction * 0.20 +
                                         work_life_balance * 0.15),
        
        # Normalized scores (0-100 scale)
        composite_satisfaction_normalized = ((composite_satisfaction_avg - 1) / 4) * 100
      ) %>%
      round(2)
  }
  
  scored_data <- calculate_composite_scores(mock_data)
  
  # Test composite score ranges
  expect_true(all(scored_data$composite_satisfaction_avg >= 1 & 
                 scored_data$composite_satisfaction_avg <= 5))
  expect_true(all(scored_data$composite_satisfaction_weighted >= 1 & 
                 scored_data$composite_satisfaction_weighted <= 5))
  expect_true(all(scored_data$composite_satisfaction_normalized >= 0 & 
                 scored_data$composite_satisfaction_normalized <= 100))
  
  # Test mathematical accuracy
  test_row <- scored_data[1, ]
  expected_avg <- (test_row$job_satisfaction + test_row$environment_satisfaction + 
                  test_row$relationship_satisfaction + test_row$work_life_balance) / 4
  expect_equal(test_row$composite_satisfaction_avg, expected_avg, tolerance = 0.01)
})

test_that("Satisfaction Score Calculations - Percentile Rankings", {
  mock_data <- create_mock_satisfaction_data(100)
  
  # Function to calculate percentile rankings
  calculate_percentile_rankings <- function(data) {
    data %>%
      mutate(
        job_satisfaction_percentile = round(percent_rank(job_satisfaction) * 100, 1),
        environment_satisfaction_percentile = round(percent_rank(environment_satisfaction) * 100, 1),
        composite_score = (job_satisfaction + environment_satisfaction + 
                          relationship_satisfaction + work_life_balance) / 4
      ) %>%
      mutate(
        composite_percentile = round(percent_rank(composite_score) * 100, 1)
      )
  }
  
  ranked_data <- calculate_percentile_rankings(mock_data)
  
  # Test percentile ranges
  expect_true(all(ranked_data$job_satisfaction_percentile >= 0 & 
                 ranked_data$job_satisfaction_percentile <= 100))
  expect_true(all(ranked_data$composite_percentile >= 0 & 
                 ranked_data$composite_percentile <= 100))
  
  # Test percentile accuracy
  expect_equal(max(ranked_data$job_satisfaction_percentile), 100)
  expect_equal(min(ranked_data$job_satisfaction_percentile), 0)
})

test_that("Satisfaction Score Calculations - Departmental Benchmarks", {
  mock_data <- create_mock_satisfaction_data(80)
  
  # Function to calculate departmental benchmarks
  calculate_department_benchmarks <- function(data) {
    dept_benchmarks <- data %>%
      group_by(department) %>%
      summarise(
        dept_avg_job_satisfaction = round(mean(job_satisfaction, na.rm = TRUE), 2),
        dept_median_job_satisfaction = round(median(job_satisfaction, na.rm = TRUE), 2),
        dept_satisfaction_sd = round(sd(job_satisfaction, na.rm = TRUE), 2),
        dept_n = n(),
        .groups = "drop"
      )
    
    # Join back to main data
    data %>%
      left_join(dept_benchmarks, by = "department") %>%
      mutate(
        satisfaction_vs_dept = round(job_satisfaction - dept_avg_job_satisfaction, 2),
        satisfaction_z_score = round((job_satisfaction - dept_avg_job_satisfaction) / dept_satisfaction_sd, 2)
      )
  }
  
  benchmark_data <- calculate_department_benchmarks(mock_data)
  
  # Test benchmark calculations
  expect_true("dept_avg_job_satisfaction" %in% names(benchmark_data))
  expect_true("satisfaction_vs_dept" %in% names(benchmark_data))
  expect_true("satisfaction_z_score" %in% names(benchmark_data))
  
  # Test mathematical accuracy for one department
  it_data <- filter(benchmark_data, department == "IT")
  if(nrow(it_data) > 0) {
    expected_avg <- mean(it_data$job_satisfaction, na.rm = TRUE)
    expect_equal(unique(it_data$dept_avg_job_satisfaction), expected_avg, tolerance = 0.01)
  }
})

# ==============================================================================
# CORRELATION ANALYSIS ACCURACY TESTS
# ==============================================================================

test_that("Correlation Analysis - Satisfaction Intercorrelations", {
  mock_data <- create_mock_satisfaction_data(150)
  
  # Function to calculate satisfaction correlations
  calculate_satisfaction_correlations <- function(data) {
    satisfaction_vars <- c("job_satisfaction", "environment_satisfaction", 
                          "relationship_satisfaction", "work_life_balance")
    
    cor_matrix <- data %>%
      select(all_of(satisfaction_vars)) %>%
      cor(use = "complete.obs", method = "pearson")
    
    # Convert to long format for analysis
    cor_data <- cor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
      filter(var1 != var2) %>%
      mutate(
        correlation = round(correlation, 3),
        correlation_strength = case_when(
          abs(correlation) >= 0.7 ~ "Strong",
          abs(correlation) >= 0.5 ~ "Moderate",
          abs(correlation) >= 0.3 ~ "Weak",
          TRUE ~ "Very Weak"
        )
      )
    
    list(matrix = cor_matrix, long_format = cor_data)
  }
  
  cor_results <- calculate_satisfaction_correlations(mock_data)
  
  # Test correlation matrix properties
  expect_equal(dim(cor_results$matrix), c(4, 4))
  expect_true(all(diag(cor_results$matrix) == 1))
  expect_true(all(cor_results$matrix >= -1 & cor_results$matrix <= 1))
  
  # Test correlation data format
  expect_true(all(c("var1", "var2", "correlation", "correlation_strength") %in% 
                 names(cor_results$long_format)))
  expect_true(all(abs(cor_results$long_format$correlation) <= 1))
})

test_that("Correlation Analysis - Satisfaction vs Performance", {
  mock_data <- create_mock_satisfaction_data(120)
  
  # Function to analyze satisfaction-performance correlations
  analyze_satisfaction_performance <- function(data) {
    # Calculate correlations with performance metrics
    perf_correlations <- data %>%
      select(job_satisfaction, environment_satisfaction, relationship_satisfaction, 
             work_life_balance, manager_rating, self_rating) %>%
      cor(use = "complete.obs") %>%
      as.data.frame() %>%
      select(manager_rating, self_rating) %>%
      slice(1:4) %>%
      rownames_to_column("satisfaction_metric") %>%
      pivot_longer(-satisfaction_metric, names_to = "performance_metric", values_to = "correlation") %>%
      mutate(
        correlation = round(correlation, 3),
        significance = abs(correlation) > 0.2,  # Simplified significance test
        direction = ifelse(correlation > 0, "Positive", "Negative")
      )
    
    return(perf_correlations)
  }
  
  perf_cor <- analyze_satisfaction_performance(mock_data)
  
  # Test correlation analysis structure
  expect_true(all(c("satisfaction_metric", "performance_metric", "correlation") %in% names(perf_cor)))
  expect_equal(nrow(perf_cor), 8)  # 4 satisfaction metrics × 2 performance metrics
  expect_true(all(abs(perf_cor$correlation) <= 1))
})

test_that("Correlation Analysis - Satisfaction vs Attrition", {
  mock_data <- create_mock_satisfaction_data(100)
  
  # Function to analyze satisfaction-attrition relationships
  analyze_satisfaction_attrition <- function(data) {
    # Point-biserial correlations (satisfaction vs binary attrition)
    attrition_cors <- data %>%
      summarise(
        job_sat_attrition_cor = cor(job_satisfaction, attrition, use = "complete.obs"),
        env_sat_attrition_cor = cor(environment_satisfaction, attrition, use = "complete.obs"),
        rel_sat_attrition_cor = cor(relationship_satisfaction, attrition, use = "complete.obs"),
        wlb_attrition_cor = cor(work_life_balance, attrition, use = "complete.obs")
      ) %>%
      pivot_longer(everything(), names_to = "correlation_type", values_to = "correlation") %>%
      mutate(
        correlation = round(correlation, 3),
        interpretation = case_when(
          correlation < -0.3 ~ "Strong Negative (Good)",
          correlation < -0.1 ~ "Weak Negative (Good)",
          correlation < 0.1 ~ "No Relationship",
          correlation < 0.3 ~ "Weak Positive (Concerning)",
          TRUE ~ "Strong Positive (Very Concerning)"
        )
      )
    
    return(attrition_cors)
  }
  
  attrition_analysis <- analyze_satisfaction_attrition(mock_data)
  
  # Test attrition correlation analysis
  expect_equal(nrow(attrition_analysis), 4)
  expect_true(all(abs(attrition_analysis$correlation) <= 1))
  expect_true("interpretation" %in% names(attrition_analysis))
})

# ==============================================================================
# SENTIMENT ANALYSIS VALIDATION TESTS
# ==============================================================================

test_that("Sentiment Analysis - Text Processing", {
  # Mock sentiment analysis function (simplified)
  analyze_sentiment <- function(text_data) {
    # Simple keyword-based sentiment analysis
    positive_words <- c("great", "excellent", "love", "amazing", "fantastic", "good")
    negative_words <- c("terrible", "hate", "awful", "bad", "horrible", "poor")
    
    text_data %>%
      mutate(
        text_lower = tolower(text_feedback),
        positive_count = str_count(text_lower, paste(positive_words, collapse = "|")),
        negative_count = str_count(text_lower, paste(negative_words, collapse = "|")),
        sentiment_score = positive_count - negative_count,
        sentiment_category = case_when(
          is.na(text_feedback) ~ "No_Feedback",
          sentiment_score > 0 ~ "Positive",
          sentiment_score < 0 ~ "Negative",
          TRUE ~ "Neutral"
        )
      )
  }
  
  mock_data <- create_mock_satisfaction_data(50)
  sentiment_results <- analyze_sentiment(mock_data)
  
  # Test sentiment analysis outputs
  expect_true("sentiment_score" %in% names(sentiment_results))
  expect_true("sentiment_category" %in% names(sentiment_results))
  expect_true(all(sentiment_results$sentiment_category %in% 
                 c("Positive", "Negative", "Neutral", "No_Feedback")))
})

test_that("Sentiment Analysis - Validation Against Numeric Scores", {
  # Create data with known sentiment-score relationships
  validation_data <- tibble(
    employee_id = 1:20,
    job_satisfaction = c(rep(5, 5), rep(4, 5), rep(2, 5), rep(1, 5)),
    text_feedback = c(
      rep("Great workplace, love the team", 5),
      rep("Good environment, could be better", 5),
      rep("Not satisfied, needs improvement", 5),
      rep("Terrible experience, hate it here", 5)
    )
  )
  
  # Simple sentiment validation function
  validate_sentiment_accuracy <- function(data) {
    # Expected: high satisfaction should correlate with positive sentiment
    positive_keywords <- c("great", "love", "good", "excellent")
    negative_keywords <- c("terrible", "hate", "not satisfied")
    
    data %>%
      mutate(
        has_positive = str_detect(tolower(text_feedback), paste(positive_keywords, collapse = "|")),
        has_negative = str_detect(tolower(text_feedback), paste(negative_keywords, collapse = "|")),
        sentiment_numeric_match = case_when(
          job_satisfaction >= 4 & has_positive ~ "Match_Positive",
          job_satisfaction <= 2 & has_negative ~ "Match_Negative",
          job_satisfaction == 3 ~ "Match_Neutral",
          TRUE ~ "Mismatch"
        )
      )
  }
  
  validation_results <- validate_sentiment_accuracy(validation_data)
  match_rate <- mean(validation_results$sentiment_numeric_match != "Mismatch")
  
  # Test sentiment-satisfaction alignment
  expect_true(match_rate > 0.7)  # At least 70% should match
  expect_true("sentiment_numeric_match" %in% names(validation_results))
})

# ==============================================================================
# RESPONSE RATE CALCULATIONS TESTS
# ==============================================================================

test_that("Response Rate Calculations - Overall Response Rates", {
  # Mock employee population and survey responses
  total_employees <- 500
  mock_responses <- create_mock_satisfaction_data(350)  # 350 responses out of 500
  
  # Function to calculate response rates
  calculate_response_rates <- function(responses, total_population) {
    response_summary <- responses %>%
      summarise(
        total_invitations = total_population,
        total_responses = n(),
        complete_responses = sum(survey_completion_status == "Complete"),
        partial_responses = sum(survey_completion_status == "Partial"),
        no_response = total_population - n(),
        
        overall_response_rate = round((total_responses / total_population) * 100, 2),
        completion_rate = round((complete_responses / total_responses) * 100, 2),
        quality_response_rate = round((complete_responses / total_population) * 100, 2)
      )
    
    return(response_summary)
  }
  
  response_rates <- calculate_response_rates(mock_responses, total_employees)
  
  # Test response rate calculations
  expect_equal(response_rates$total_invitations, 500)
  expect_equal(response_rates$total_responses, 350)
  expect_true(response_rates$overall_response_rate >= 0 & response_rates$overall_response_rate <= 100)
  expect_true(response_rates$completion_rate >= 0 & response_rates$completion_rate <= 100)
  
  # Test mathematical accuracy
  expected_rate <- (350 / 500) * 100
  expect_equal(response_rates$overall_response_rate, expected_rate)
})

test_that("Response Rate Calculations - Departmental Response Rates", {
  mock_data <- create_mock_satisfaction_data(200)
  
  # Function to calculate departmental response rates
  calculate_dept_response_rates <- function(data) {
    # Mock total department sizes
    dept_sizes <- tibble(
      department = c("IT", "HR", "Sales", "Marketing", "Finance"),
      total_employees = c(80, 30, 60, 40, 50)
    )
    
    dept_responses <- data %>%
      count(department, survey_completion_status) %>%
      pivot_wider(names_from = survey_completion_status, values_from = n, values_fill = 0) %>%
      left_join(dept_sizes, by = "department") %>%
      mutate(
        total_responses = Complete + Partial + Not_Started,
        response_rate = round((total_responses / total_employees) * 100, 2),
        completion_rate = round((Complete / total_responses) * 100, 2)
      )
    
    return(dept_responses)
  }
  
  dept_rates <- calculate_dept_response_rates(mock_data)
  
  # Test departmental response rate structure
  expect_true("response_rate" %in% names(dept_rates))
  expect_true("completion_rate" %in% names(dept_rates))
  expect_true(all(dept_rates$response_rate >= 0 & dept_rates$response_rate <= 100))
  expect_true(all(dept_rates$completion_rate >= 0 & dept_rates$completion_rate <= 100))
})

test_that("Response Rate Calculations - Time-based Response Tracking", {
  # Create time-based response data
  time_responses <- tibble(
    survey_date = seq(as.Date("2024-01-01"), as.Date("2024-01-30"), by = "day"),
    daily_responses = c(rep(0, 5), sample(5:25, 20, replace = TRUE), rep(0, 5)),
    cumulative_responses = cumsum(daily_responses)
  )
  
  # Function to calculate response velocity
  calculate_response_velocity <- function(data) {
    data %>%
      mutate(
        days_since_launch = as.numeric(survey_date - min(survey_date)),
        response_velocity = daily_responses / lag(daily_responses, default = 1),
        response_momentum = case_when(
          response_velocity > 1.2 ~ "Accelerating",
          response_velocity > 0.8 ~ "Steady",
          TRUE ~ "Declining"
        )
      )
  }
  
  velocity_data <- calculate_response_velocity(time_responses)
  
  # Test response velocity calculations
  expect_true("response_velocity" %in% names(velocity_data))
  expect_true("response_momentum" %in% names(velocity_data))
  expect_true(all(velocity_data$response_momentum %in% c("Accelerating", "Steady", "Declining")))
})

# ==============================================================================
# BIAS CORRECTION ALGORITHMS TESTS
# ==============================================================================

test_that("Bias Correction - Non-Response Bias Adjustment", {
  # Create biased sample (higher satisfaction in responses)
  all_employees <- tibble(
    employee_id = 1:1000,
    true_satisfaction = rnorm(1000, 3, 1),
    department = sample(c("IT", "HR", "Sales", "Marketing", "Finance"), 1000, replace = TRUE),
    tenure = runif(1000, 0.5, 15)
  ) %>%
    mutate(
      true_satisfaction = pmax(1, pmin(5, true_satisfaction)),
      # Higher satisfaction employees more likely to respond
      response_probability = plogis((true_satisfaction - 3) * 0.5 + rnorm(1000, 0, 0.3)),
      responded = rbinom(1000, 1, response_probability)
    )
  
  # Function to apply non-response bias correction
  correct_nonresponse_bias <- function(population_data) {
    respondents <- filter(population_data, responded == 1)
    
    # Calculate response propensities by subgroup
    response_model <- glm(responded ~ department + tenure, 
                         data = population_data, family = binomial)
    
    population_data$response_propensity <- predict(response_model, type = "response")
    respondents$response_propensity <- predict(response_model, 
                                             newdata = respondents, type = "response")
    
    # Apply inverse propensity weighting
    respondents <- respondents %>%
      mutate(
        weight = 1 / response_propensity,
        weighted_satisfaction = true_satisfaction * weight
      )
    
    # Calculate weighted vs unweighted means
    bias_correction <- tibble(
      unweighted_mean = mean(respondents$true_satisfaction),
      weighted_mean = sum(respondents$weighted_satisfaction) / sum(respondents$weight),
      true_population_mean = mean(population_data$true_satisfaction),
      bias_reduction = abs(weighted_mean - true_population_mean) < 
                      abs(unweighted_mean - true_population_mean)
    )
    
    return(bias_correction)
  }
  
  bias_results <- correct_nonresponse_bias(all_employees)
  
  # Test bias correction effectiveness
  expect_true("bias_reduction" %in% names(bias_results))
  expect_true(is.logical(bias_results$bias_reduction))
  expect_true(abs(bias_results$weighted_mean - bias_results$true_population_mean) < 
             abs(bias_results$unweighted_mean - bias_results$true_population_mean))
})

test_that("Bias Correction - Social Desirability Bias", {
  mock_data <- create_mock_satisfaction_data(100)
  
  # Function to detect and correct social desirability bias
  correct_social_desirability <- function(data) {
    # Detect potential social desirability patterns
    data %>%
      mutate(
        # High scores across all dimensions (ceiling effect)
        all_high_scores = (job_satisfaction >= 4 & environment_satisfaction >= 4 & 
                          relationship_satisfaction >= 4 & work_life_balance >= 4),
        
        # Identical scores across dimensions
        identical_scores = (job_satisfaction == environment_satisfaction & 
                           environment_satisfaction == relationship_satisfaction),
        
        # Fast response time with high scores
        rushed_positive = (response_time_minutes < 5 & job_satisfaction >= 4),
        
        # Social desirability index
        social_desirability_score = as.numeric(all_high_scores) + 
                                   as.numeric(identical_scores) + 
                                   as.numeric(rushed_positive),
        
        # Apply correction factor
        correction_factor = case_when(
          social_desirability_score >= 2 ~ 0.8,
          social_desirability_score == 1 ~ 0.9,
          TRUE ~ 1.0
        ),
        
        # Corrected satisfaction scores
        job_satisfaction_corrected = pmax(1, job_satisfaction * correction_factor),
        environment_satisfaction_corrected = pmax(1, environment_satisfaction * correction_factor)
      )
  }
  
  corrected_data <- correct_social_desirability(mock_data)
  
  # Test social desirability correction
  expect_true("social_desirability_score" %in% names(corrected_data))
  expect_true("correction_factor" %in% names(corrected_data))
  expect_true(all(corrected_data$correction_factor <= 1))
  expect_true(all(corrected_data$job_satisfaction_corrected >= 1))
})

# ==============================================================================
# LONGITUDINAL TREND ANALYSIS TESTS
# ==============================================================================

test_that("Longitudinal Trend Analysis - Individual Employee Trends", {
  longitudinal_data <- create_longitudinal_data(30, 6)
  
  # Function to analyze individual trends
  analyze_individual_trends <- function(data) {
    data %>%
      group_by(employee_id) %>%
      summarise(
        trend_job_satisfaction =