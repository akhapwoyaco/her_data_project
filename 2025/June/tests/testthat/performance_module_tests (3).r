# =============================================================================
# ATLAS LABS HR ANALYTICS - PERFORMANCE MODULE UNIT TESTS
# =============================================================================
# 
# Comprehensive unit tests for Performance Module focusing on:
# 1. Normalization procedures
# 2. Weighted scoring algorithms  
# 3. Performance prediction models
#
# Author: akhapwoyaco
# Framework: testthat with Shiny module testing
# =============================================================================

library(testthat)
library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(caret)
library(randomForest)
library(glmnet)

# Source the performance module functions (adjust path as needed)
# source("modules/performance_module.R")

# =============================================================================
# TEST DATA SETUP
# =============================================================================

setup_test_data <- function() {
  # Create realistic performance test data
  set.seed(12345)
  n <- 1000
  
  performance_data <- tibble(
    EmployeeID = paste0("EMP", sprintf("%04d", 1:n)),
    EnvironmentSatisfaction = sample(1:5, n, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15)),
    JobSatisfaction = sample(1:5, n, replace = TRUE, prob = c(0.08, 0.12, 0.25, 0.35, 0.2)),
    RelationshipSatisfaction = sample(1:5, n, replace = TRUE, prob = c(0.12, 0.18, 0.28, 0.28, 0.14)),
    WorkLifeBalance = sample(1:5, n, replace = TRUE, prob = c(0.15, 0.2, 0.25, 0.25, 0.15)),
    SelfRating = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.4, 0.25)),
    ManagerRating = sample(1:5, n, replace = TRUE, prob = c(0.08, 0.12, 0.25, 0.35, 0.2)),
    TrainingOpportunitiesWithinYear = sample(0:10, n, replace = TRUE),
    TrainingOpportunitiesTaken = NA,
    YearsAtCompany = sample(0:40, n, replace = TRUE),
    YearsInMostRecentRole = sample(0:20, n, replace = TRUE),
    Age = sample(20:65, n, replace = TRUE),
    Salary = sample(30000:150000, n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.15, 0.85))
  )
  
  # Make TrainingOpportunitiesTaken realistic (can't exceed offered)
  performance_data$TrainingOpportunitiesTaken <- purrr::map2_dbl(
    performance_data$TrainingOpportunitiesWithinYear,
    runif(n),
    ~ min(.x, rbinom(1, .x, 0.7))
  )
  
  # Add some missing values for realistic testing
  missing_indices <- sample(1:n, size = n * 0.02)  # 2% missing
  performance_data$EnvironmentSatisfaction[missing_indices[1:5]] <- NA
  performance_data$WorkLifeBalance[missing_indices[6:10]] <- NA
  
  return(performance_data)
}

# =============================================================================
# 1. NORMALIZATION PROCEDURES TESTS
# =============================================================================

test_that("Normalization Procedures - Core Functions", {
  
  # Test data setup
  test_data <- setup_test_data()
  
  # Test z-score normalization
  normalize_zscore <- function(x) {
    if (all(is.na(x))) return(x)
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
  test_that("Z-score normalization works correctly", {
    scores <- c(1, 2, 3, 4, 5)
    normalized <- normalize_zscore(scores)
    
    expect_equal(mean(normalized, na.rm = TRUE), 0, tolerance = 1e-10)
    expect_equal(sd(normalized, na.rm = TRUE), 1, tolerance = 1e-10)
    expect_length(normalized, length(scores))
  })
  
  test_that("Z-score handles missing values correctly", {
    scores_with_na <- c(1, 2, NA, 4, 5)
    normalized <- normalize_zscore(scores_with_na)
    
    expect_true(is.na(normalized[3]))
    expect_false(any(is.na(normalized[-3])))
    expect_equal(length(normalized), length(scores_with_na))
  })
  
  test_that("Z-score handles constant values", {
    constant_scores <- rep(3, 5)
    normalized <- normalize_zscore(constant_scores)
    
    expect_true(all(is.nan(normalized) | normalized == 0))
  })
  
  # Test min-max normalization
  normalize_minmax <- function(x, min_val = 0, max_val = 1) {
    if (all(is.na(x))) return(x)
    min_x <- min(x, na.rm = TRUE)
    max_x <- max(x, na.rm = TRUE)
    if (min_x == max_x) return(rep(min_val, length(x)))
    min_val + (x - min_x) * (max_val - min_val) / (max_x - min_x)
  }
  
  test_that("Min-max normalization works correctly", {
    scores <- c(10, 20, 30, 40, 50)
    normalized <- normalize_minmax(scores)
    
    expect_equal(min(normalized, na.rm = TRUE), 0)
    expect_equal(max(normalized, na.rm = TRUE), 1)
    expect_true(all(normalized >= 0 & normalized <= 1, na.rm = TRUE))
  })
  
  test_that("Min-max normalization with custom range", {
    scores <- c(1, 2, 3, 4, 5)
    normalized <- normalize_minmax(scores, min_val = -1, max_val = 1)
    
    expect_equal(min(normalized, na.rm = TRUE), -1)
    expect_equal(max(normalized, na.rm = TRUE), 1)
  })
  
  # Test robust normalization (using median and MAD)
  normalize_robust <- function(x) {
    if (all(is.na(x))) return(x)
    median_x <- median(x, na.rm = TRUE)
    mad_x <- mad(x, na.rm = TRUE)
    if (mad_x == 0) return(rep(0, length(x)))
    (x - median_x) / mad_x
  }
  
  test_that("Robust normalization handles outliers", {
    scores_with_outliers <- c(1, 2, 3, 4, 100)  # 100 is outlier
    normalized_robust <- normalize_robust(scores_with_outliers)
    normalized_zscore <- normalize_zscore(scores_with_outliers)
    
    # Robust should be less affected by outliers
    expect_lt(max(abs(normalized_robust[-5])), max(abs(normalized_zscore[-5])))
  })
  
  # Test normalization with grouped data
  normalize_by_group <- function(data, value_col, group_col, method = "zscore") {
    data %>%
      group_by(!!sym(group_col)) %>%
      mutate(
        normalized = case_when(
          method == "zscore" ~ normalize_zscore(!!sym(value_col)),
          method == "minmax" ~ normalize_minmax(!!sym(value_col)),
          method == "robust" ~ normalize_robust(!!sym(value_col)),
          TRUE ~ !!sym(value_col)
        )
      ) %>%
      ungroup()
  }
  
  test_that("Group-wise normalization works correctly", {
    group_data <- tibble(
      group = rep(c("A", "B"), each = 5),
      value = c(1:5, 11:15)
    )
    
    normalized_data <- normalize_by_group(group_data, "value", "group", "zscore")
    
    # Each group should have mean ~0 and sd ~1
    group_stats <- normalized_data %>%
      group_by(group) %>%
      summarise(
        mean_norm = mean(normalized, na.rm = TRUE),
        sd_norm = sd(normalized, na.rm = TRUE),
        .groups = "drop"
      )
    
    expect_true(all(abs(group_stats$mean_norm) < 1e-10))
    expect_true(all(abs(group_stats$sd_norm - 1) < 1e-10))
  })
})

# =============================================================================
# 2. WEIGHTED SCORING ALGORITHMS TESTS  
# =============================================================================

test_that("Weighted Scoring Algorithms - Core Functions", {
  
  test_data <- setup_test_data()
  
  # Weighted average function
  calculate_weighted_score <- function(scores, weights) {
    if (length(scores) != length(weights)) {
      stop("Scores and weights must have the same length")
    }
    if (any(weights < 0)) {
      stop("Weights must be non-negative")
    }
    if (sum(weights) == 0) {
      stop("Sum of weights cannot be zero")
    }
    
    # Handle missing values
    valid_idx <- !is.na(scores)
    if (!any(valid_idx)) return(NA_real_)
    
    sum(scores[valid_idx] * weights[valid_idx]) / sum(weights[valid_idx])
  }
  
  test_that("Basic weighted score calculation", {
    scores <- c(4, 3, 5, 2)
    weights <- c(0.3, 0.2, 0.4, 0.1)
    
    expected <- sum(scores * weights) / sum(weights)
    result <- calculate_weighted_score(scores, weights)
    
    expect_equal(result, expected)
    expect_true(result >= min(scores) && result <= max(scores))
  })
  
  test_that("Weighted score handles missing values", {
    scores <- c(4, NA, 5, 2)
    weights <- c(0.3, 0.2, 0.4, 0.1)
    
    # Should ignore NA and adjust weights accordingly
    valid_scores <- c(4, 5, 2)
    valid_weights <- c(0.3, 0.4, 0.1)
    expected <- sum(valid_scores * valid_weights) / sum(valid_weights)
    
    result <- calculate_weighted_score(scores, weights)
    expect_equal(result, expected)
  })
  
  test_that("Weighted score validation checks", {
    scores <- c(1, 2, 3)
    
    # Mismatched lengths
    expect_error(calculate_weighted_score(scores, c(0.5, 0.5)))
    
    # Negative weights
    expect_error(calculate_weighted_score(scores, c(-0.1, 0.6, 0.5)))
    
    # Zero sum weights
    expect_error(calculate_weighted_score(scores, c(0, 0, 0)))
    
    # All NA scores
    expect_true(is.na(calculate_weighted_score(c(NA, NA, NA), c(1, 1, 1))))
  })
  
  # Multi-dimensional weighted scoring
  calculate_composite_performance_score <- function(data, weight_config) {
    # Default weights if not provided
    default_weights <- list(
      satisfaction_weight = 0.25,
      performance_weight = 0.40,
      development_weight = 0.20,
      tenure_weight = 0.15
    )
    
    weights <- modifyList(default_weights, weight_config)
    
    data %>%
      mutate(
        # Satisfaction composite (0-5 scale)
        satisfaction_score = calculate_weighted_score(
          c(EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance),
          c(0.2, 0.3, 0.2, 0.3)
        ),
        
        # Performance composite (0-5 scale)
        performance_score = calculate_weighted_score(
          c(SelfRating, ManagerRating),
          c(0.3, 0.7)  # Manager rating weighted higher
        ),
        
        # Development score (normalized training utilization)
        development_score = ifelse(
          TrainingOpportunitiesWithinYear > 0,
          5 * (TrainingOpportunitiesTaken / TrainingOpportunitiesWithinYear),
          2.5  # Neutral score if no opportunities offered
        ),
        
        # Tenure score (experience factor, capped at 5)
        tenure_score = pmin(5, 1 + (YearsAtCompany / 10) * 4),
        
        # Final composite score (0-5 scale)
        composite_score = calculate_weighted_score(
          c(satisfaction_score, performance_score, development_score, tenure_score),
          c(weights$satisfaction_weight, weights$performance_weight, 
            weights$development_weight, weights$tenure_weight)
        )
      )
  }
  
  test_that("Composite performance scoring works correctly", {
    sample_data <- test_data[1:10, ]
    
    scored_data <- calculate_composite_performance_score(sample_data, list())
    
    # Check that composite scores are within expected range
    expect_true(all(scored_data$composite_score >= 0 & scored_data$composite_score <= 5, na.rm = TRUE))
    
    # Check that all component scores are calculated
    expect_true(all(!is.na(scored_data$satisfaction_score) | 
                   rowSums(is.na(sample_data[, c("EnvironmentSatisfaction", "JobSatisfaction", 
                                                 "RelationshipSatisfaction", "WorkLifeBalance")])) == 4))
    
    # Check score ranges for components
    expect_true(all(scored_data$development_score >= 0 & scored_data$development_score <= 5, na.rm = TRUE))
    expect_true(all(scored_data$tenure_score >= 1 & scored_data$tenure_score <= 5, na.rm = TRUE))
  })
  
  test_that("Custom weight configurations work", {
    sample_data <- test_data[1:5, ]
    
    # Performance-heavy weighting
    perf_heavy <- list(performance_weight = 0.6, satisfaction_weight = 0.2, 
                      development_weight = 0.1, tenure_weight = 0.1)
    
    # Satisfaction-heavy weighting  
    sat_heavy <- list(satisfaction_weight = 0.6, performance_weight = 0.2,
                     development_weight = 0.1, tenure_weight = 0.1)
    
    scored_perf <- calculate_composite_performance_score(sample_data, perf_heavy)
    scored_sat <- calculate_composite_performance_score(sample_data, sat_heavy)
    
    # Scores should be different with different weightings
    expect_false(identical(scored_perf$composite_score, scored_sat$composite_score))
  })
  
  # Percentile-based weighted scoring
  calculate_percentile_weighted_score <- function(data, score_col, weight_col, percentile_groups = 4) {
    data %>%
      mutate(
        percentile_group = ntile(!!sym(score_col), percentile_groups),
        weight_factor = case_when(
          percentile_group == 1 ~ 0.5,  # Bottom quartile gets lower weight
          percentile_group == 2 ~ 0.75,
          percentile_group == 3 ~ 1.0,
          percentile_group == 4 ~ 1.25,  # Top quartile gets higher weight
          TRUE ~ 1.0
        ),
        weighted_score = !!sym(score_col) * weight_factor
      )
  }
  
  test_that("Percentile-based weighting works correctly", {
    sample_data <- tibble(
      id = 1:100,
      base_score = runif(100, 1, 5),
      weight = runif(100, 0.5, 1.5)
    )
    
    weighted_data <- calculate_percentile_weighted_score(sample_data, "base_score", "weight")
    
    # Check percentile groups are created correctly
    expect_equal(length(unique(weighted_data$percentile_group)), 4)
    expect_true(all(weighted_data$percentile_group %in% 1:4))
    
    # Check that top performers get higher weighted scores
    top_performers <- weighted_data %>% filter(percentile_group == 4)
    bottom_performers <- weighted_data %>% filter(percentile_group == 1)
    
    expect_gt(mean(top_performers$weighted_score), mean(top_performers$base_score))
    expect_lt(mean(bottom_performers$weighted_score), mean(bottom_performers$base_score))
  })
})

# =============================================================================
# 3. PERFORMANCE PREDICTION MODELS TESTS
# =============================================================================

test_that("Performance Prediction Models - Core Functions", {
  
  test_data <- setup_test_data()
  
  # Prepare features for modeling
  prepare_model_features <- function(data) {
    data %>%
      select(
        EmployeeID,
        EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance,
        SelfRating, ManagerRating, TrainingOpportunitiesWithinYear, TrainingOpportunitiesTaken,
        YearsAtCompany, YearsInMostRecentRole, Age, Salary, Attrition
      ) %>%
      mutate(
        # Create target variable - high performer (top 30% of manager ratings)
        high_performer = as.factor(ifelse(ManagerRating >= quantile(ManagerRating, 0.7, na.rm = TRUE), "Yes", "No")),
        
        # Feature engineering
        training_utilization = ifelse(TrainingOpportunitiesWithinYear > 0, 
                                    TrainingOpportunitiesTaken / TrainingOpportunitiesWithinYear, 0),
        experience_ratio = YearsInMostRecentRole / pmax(YearsAtCompany, 1),
        salary_per_year = Salary / pmax(YearsAtCompany, 1),
        
        # Convert categorical variables
        attrition_flag = as.numeric(Attrition == "Yes")
      ) %>%
      filter(complete.cases(.)) # Remove rows with missing values for modeling
  }
  
  test_that("Feature preparation works correctly", {
    model_data <- prepare_model_features(test_data)
    
    # Check target variable creation
    expect_true("high_performer" %in% colnames(model_data))
    expect_true(is.factor(model_data$high_performer))
    expect_true(all(levels(model_data$high_performer) %in% c("Yes", "No")))
    
    # Check feature engineering
    expect_true(all(model_data$training_utilization >= 0 & model_data$training_utilization <= 1))
    expect_true(all(model_data$experience_ratio >= 0 & model_data$experience_ratio <= 1))
    expect_true(all(model_data$salary_per_year > 0))
    
    # Check no missing values
    expect_equal(sum(is.na(model_data)), 0)
  })
  
  # Logistic regression model
  build_logistic_model <- function(train_data) {
    model <- glm(
      high_performer ~ EnvironmentSatisfaction + JobSatisfaction + RelationshipSatisfaction + 
                      WorkLifeBalance + SelfRating + training_utilization + 
                      experience_ratio + Age + salary_per_year,
      data = train_data,
      family = binomial()
    )
    return(model)
  }
  
  test_that("Logistic regression model builds correctly", {
    model_data <- prepare_model_features(test_data)
    
    # Skip if insufficient data
    skip_if(nrow(model_data) < 100, "Insufficient data for modeling")
    
    # Split data
    set.seed(123)
    train_idx <- sample(1:nrow(model_data), size = floor(0.7 * nrow(model_data)))
    train_data <- model_data[train_idx, ]
    test_data <- model_data[-train_idx, ]
    
    model <- build_logistic_model(train_data)
    
    # Check model object
    expect_s3_class(model, "glm")
    expect_equal(model$family$family, "binomial")
    
    # Check predictions
    predictions <- predict(model, test_data, type = "response")
    expect_true(all(predictions >= 0 & predictions <= 1))
    expect_equal(length(predictions), nrow(test_data))
  })
  
  # Random forest model
  build_random_forest_model <- function(train_data) {
    model <- randomForest(
      high_performer ~ EnvironmentSatisfaction + JobSatisfaction + RelationshipSatisfaction + 
                      WorkLifeBalance + SelfRating + training_utilization + 
                      experience_ratio + Age + salary_per_year,
      data = train_data,
      ntree = 100,
      importance = TRUE
    )
    return(model)
  }
  
  test_that("Random forest model builds correctly", {
    model_data <- prepare_model_features(test_data)
    
    # Skip if insufficient data
    skip_if(nrow(model_data) < 100, "Insufficient data for modeling")
    skip_if(!requireNamespace("randomForest", quietly = TRUE), "randomForest package not available")
    
    # Split data
    set.seed(123)
    train_idx <- sample(1:nrow(model_data), size = floor(0.7 * nrow(model_data)))
    train_data <- model_data[train_idx, ]
    test_data <- model_data[-train_idx, ]
    
    model <- build_random_forest_model(train_data)
    
    # Check model object
    expect_s3_class(model, "randomForest")
    
    # Check predictions
    predictions <- predict(model, test_data, type = "prob")
    expect_true(all(predictions[, "Yes"] >= 0 & predictions[, "Yes"] <= 1))
    expect_equal(nrow(predictions), nrow(test_data))
    
    # Check variable importance
    importance_scores <- importance(model)
    expect_true(nrow(importance_scores) > 0)
  })
  
  # Model evaluation functions
  evaluate_model_performance <- function(predictions, actual) {
    # Convert predictions to binary if they're probabilities
    if (all(predictions >= 0 & predictions <= 1)) {
      pred_binary <- as.factor(ifelse(predictions > 0.5, "Yes", "No"))
    } else {
      pred_binary <- predictions
    }
    
    # Confusion matrix
    cm <- table(Predicted = pred_binary, Actual = actual)
    
    # Calculate metrics
    accuracy <- sum(diag(cm)) / sum(cm)
    
    if ("Yes" %in% rownames(cm) && "Yes" %in% colnames(cm)) {
      tp <- cm["Yes", "Yes"]
      fp <- cm["Yes", "No"]  
      fn <- cm["No", "Yes"]
      tn <- cm["No", "No"]
      
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      f1_score <- 2 * precision * recall / (precision + recall)
    } else {
      precision <- recall <- f1_score <- NA
    }
    
    return(list(
      accuracy = accuracy,
      precision = precision,
      recall = recall,
      f1_score = f1_score,
      confusion_matrix = cm
    ))
  }
  
  test_that("Model evaluation works correctly", {
    # Test with perfect predictions
    perfect_pred <- factor(c("Yes", "No", "Yes", "No"), levels = c("No", "Yes"))
    perfect_actual <- factor(c("Yes", "No", "Yes", "No"), levels = c("No", "Yes"))
    
    eval_perfect <- evaluate_model_performance(perfect_pred, perfect_actual)
    expect_equal(eval_perfect$accuracy, 1.0)
    expect_equal(eval_perfect$precision, 1.0)
    expect_equal(eval_perfect$recall, 1.0)
    expect_equal(eval_perfect$f1_score, 1.0)
    
    # Test with probability predictions
    prob_pred <- c(0.8, 0.2, 0.9, 0.1)
    prob_actual <- factor(c("Yes", "No", "Yes", "No"), levels = c("No", "Yes"))
    
    eval_prob <- evaluate_model_performance(prob_pred, prob_actual)
    expect_equal(eval_prob$accuracy, 1.0)
    
    # Test with poor predictions
    poor_pred <- factor(c("No", "Yes", "No", "Yes"), levels = c("No", "Yes"))
    poor_actual <- factor(c("Yes", "No", "Yes", "No"), levels = c("No", "Yes"))
    
    eval_poor <- evaluate_model_performance(poor_pred, poor_actual)
    expect_equal(eval_poor$accuracy, 0.0)
  })
  
  # Cross-validation function
  perform_cross_validation <- function(data, model_type = "logistic", k_folds = 5) {
    set.seed(123)
    fold_ids <- sample(rep(1:k_folds, length.out = nrow(data)))
    
    cv_results <- map_dfr(1:k_folds, function(fold) {
      train_data <- data[fold_ids != fold, ]
      test_data <- data[fold_ids == fold, ]
      
      # Build model
      if (model_type == "logistic") {
        model <- build_logistic_model(train_data)
        predictions <- predict(model, test_data, type = "response")
      } else if (model_type == "rf") {
        model <- build_random_forest_model(train_data)
        predictions <- predict(model, test_data, type = "prob")[, "Yes"]
      }
      
      # Evaluate
      eval_results <- evaluate_model_performance(predictions, test_data$high_performer)
      
      tibble(
        fold = fold,
        accuracy = eval_results$accuracy,
        precision = eval_results$precision,
        recall = eval_results$recall,
        f1_score = eval_results$f1_score
      )
    })
    
    return(cv_results)
  }
  
  test_that("Cross-validation works correctly", {
    model_data <- prepare_model_features(test_data)
    
    # Skip if insufficient data
    skip_if(nrow(model_data) < 50, "Insufficient data for cross-validation")
    
    cv_results <- perform_cross_validation(model_data, "logistic", k_folds = 3)
    
    # Check results structure
    expect_equal(nrow(cv_results), 3)
    expect_true(all(c("fold", "accuracy", "precision", "recall", "f1_score") %in% colnames(cv_results)))
    
    # Check that all folds completed
    expect_equal(sort(cv_results$fold), 1:3)
    
    # Check metrics are in valid ranges
    expect_true(all(cv_results$accuracy >= 0 & cv_results$accuracy <= 1, na.rm = TRUE))
    expect_true(all(cv_results$precision >= 0 & cv_results$precision <= 1, na.rm = TRUE))
    expect_true(all(cv_results$recall >= 0 & cv_results$recall <= 1, na.rm = TRUE))
  })
  
  # Feature importance analysis
  analyze_feature_importance <- function(model, model_type = "logistic") {
    if (model_type == "logistic") {
      # Extract coefficients
      coefs <- summary(model)$coefficients
      importance_df <- tibble(
        feature = rownames(coefs)[-1],  # Exclude intercept
        importance = abs(coefs[-1, "Estimate"]),
        p_value = coefs[-1, "Pr(>|z|)"]
      ) %>%
        arrange(desc(importance))
        
    } else if (model_type == "rf") {
      # Extract variable importance
      importance_scores <- importance(model)
      importance_df <- tibble(
        feature = rownames(importance_scores),
        importance = importance_scores[, "MeanDecreaseGini"]
      ) %>%
        arrange(desc(importance))
    }
    
    return(importance_df)
  }
  
  test_that("Feature importance analysis works correctly", {
    model_data <- prepare_model_features(test_data)
    
    # Skip if insufficient data
    skip_if(nrow(model_data) < 100, "Insufficient data for modeling")
    
    # Test logistic regression feature importance
    model <- build_logistic_model(model_data)
    importance_log <- analyze_feature_importance(model, "logistic")
    
    expect_true(is.data.frame(importance_log))
    expect_true(all(c("feature", "importance", "p_value") %in% colnames(importance_log)))
    expect_true(nrow(importance_log) > 0)
    expect_true(all(importance_log$importance >= 0))
    
    # Check that results are sorted by importance
    expect_true(all(diff(importance_log$importance) <= 0))
  })
})

# =============================================================================
# INTEGRATION TESTS - MODULE TESTING WITH SHINY
# =============================================================================

test_that("Shiny Module Integration Tests", {
  
  # Mock performance module server function