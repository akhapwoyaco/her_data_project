# =============================================================================
# ATLAS LABS HR ANALYTICS - ATTRITION ANALYSIS MODULE UNIT TESTS
# =============================================================================
# Comprehensive unit tests for statistical calculations and analytical functions
# Developer: akhapwoyaco
# Focus: Statistical accuracy, model validation, and analytical correctness

# Load required libraries for testing
library(testthat)
library(dplyr)
library(survival)
library(caret)
library(broom)
library(lubridate)
library(corrplot)

# =============================================================================
# TEST DATA SETUP AND FIXTURES
# =============================================================================

# Create comprehensive test dataset
create_test_data <- function(n = 1000) {
  set.seed(42)  # Reproducible results
  
  data.frame(
    EmployeeID = 1:n,
    Age = sample(22:65, n, replace = TRUE),
    Gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)),
    Department = sample(c("Sales", "IT", "HR", "Finance", "Marketing"), n, replace = TRUE),
    Salary = rnorm(n, 75000, 15000),
    YearsAtCompany = sample(0:25, n, replace = TRUE),
    YearsInRole = sample(0:15, n, replace = TRUE),
    YearsSincePromotion = sample(0:10, n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 
                           n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    HireDate = seq(as.Date("2015-01-01"), as.Date("2023-12-31"), length.out = n),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    TerminationDate = ifelse(runif(n) < 0.16, 
                            sample(seq(as.Date("2020-01-01"), Sys.Date(), by = "day"), n, replace = TRUE),
                            NA),
    stringsAsFactors = FALSE
  )
}

# Create seasonal test data
create_seasonal_data <- function() {
  dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
  data.frame(
    Month = dates,
    AttritionCount = c(15, 12, 18, 22, 25, 20, 16, 14, 19, 23, 21, 28,  # 2020
                      17, 14, 20, 24, 27, 22, 18, 16, 21, 25, 23, 30,  # 2021
                      19, 16, 22, 26, 29, 24, 20, 18, 23, 27, 25, 32,  # 2022
                      21, 18, 24, 28, 31, 26, 22, 20, 25, 29, 27, 34), # 2023
    TotalEmployees = rep(1000, length(dates))
  )
}

# =============================================================================
# 1. STATISTICAL CALCULATION ACCURACY TESTS
# =============================================================================

test_that("Attrition rate calculations are statistically accurate", {
  test_data <- create_test_data(1000)
  
  # Test basic attrition rate
  calculate_attrition_rate <- function(data) {
    attrition_count <- sum(data$Attrition == "Yes", na.rm = TRUE)
    total_employees <- nrow(data)
    return(attrition_count / total_employees)
  }
  
  rate <- calculate_attrition_rate(test_data)
  expected_rate <- sum(test_data$Attrition == "Yes") / nrow(test_data)
  
  expect_equal(rate, expected_rate, tolerance = 1e-10)
  expect_gte(rate, 0)
  expect_lte(rate, 1)
  
  # Test attrition rate by department
  dept_rates <- test_data %>%
    group_by(Department) %>%
    summarise(
      total = n(),
      attrition = sum(Attrition == "Yes"),
      rate = attrition / total,
      .groups = "drop"
    )
  
  expect_equal(sum(dept_rates$total), nrow(test_data))
  expect_true(all(dept_rates$rate >= 0 & dept_rates$rate <= 1))
  expect_equal(sum(dept_rates$attrition), sum(test_data$Attrition == "Yes"))
})

test_that("Statistical measures are correctly calculated", {
  test_data <- create_test_data(500)
  
  # Test mean, median, standard deviation
  calculate_summary_stats <- function(data, column) {
    list(
      mean = mean(data[[column]], na.rm = TRUE),
      median = median(data[[column]], na.rm = TRUE),
      sd = sd(data[[column]], na.rm = TRUE),
      var = var(data[[column]], na.rm = TRUE),
      min = min(data[[column]], na.rm = TRUE),
      max = max(data[[column]], na.rm = TRUE)
    )
  }
  
  salary_stats <- calculate_summary_stats(test_data, "Salary")
  
  # Verify calculations
  expect_equal(salary_stats$mean, mean(test_data$Salary, na.rm = TRUE))
  expect_equal(salary_stats$median, median(test_data$Salary, na.rm = TRUE))
  expect_equal(salary_stats$sd, sd(test_data$Salary, na.rm = TRUE))
  expect_equal(salary_stats$var, var(test_data$Salary, na.rm = TRUE))
  expect_true(salary_stats$sd^2 - salary_stats$var < 1e-10)  # Variance = SD²
})

# =============================================================================
# 2. PREDICTIVE MODEL VALIDATION TESTS
# =============================================================================

test_that("Logistic regression model for attrition prediction is accurate", {
  test_data <- create_test_data(800)
  
  # Create binary attrition variable
  test_data$AttritionBinary <- ifelse(test_data$Attrition == "Yes", 1, 0)
  
  # Fit logistic regression model
  build_attrition_model <- function(data) {
    model <- glm(AttritionBinary ~ Age + Salary + YearsAtCompany + 
                 JobSatisfaction + WorkLifeBalance + DistanceFromHome,
                 data = data, family = binomial)
    return(model)
  }
  
  model <- build_attrition_model(test_data)
  
  # Test model validity
  expect_s3_class(model, "glm")
  expect_equal(model$family$family, "binomial")
  expect_equal(model$family$link, "logit")
  
  # Test predictions are between 0 and 1
  predictions <- predict(model, type = "response")
  expect_true(all(predictions >= 0 & predictions <= 1))
  
  # Test model coefficients exist
  expect_true(length(coef(model)) > 1)
  expect_false(any(is.na(coef(model))))
  
  # Test AUC calculation
  if (requireNamespace("pROC", quietly = TRUE)) {
    library(pROC)
    roc_obj <- roc(test_data$AttritionBinary, predictions)
    auc_value <- auc(roc_obj)
    expect_gte(as.numeric(auc_value), 0.5)  # AUC should be > 0.5 for meaningful model
    expect_lte(as.numeric(auc_value), 1.0)
  }
})

test_that("Model cross-validation produces consistent results", {
  test_data <- create_test_data(600)
  test_data$AttritionBinary <- ifelse(test_data$Attrition == "Yes", 1, 0)
  
  # K-fold cross-validation function
  perform_cv <- function(data, k = 5) {
    set.seed(123)
    folds <- createFolds(data$AttritionBinary, k = k, list = TRUE)
    accuracies <- numeric(k)
    
    for (i in 1:k) {
      train_data <- data[-folds[[i]], ]
      test_data <- data[folds[[i]], ]
      
      model <- glm(AttritionBinary ~ Age + Salary + YearsAtCompany + JobSatisfaction,
                   data = train_data, family = binomial)
      
      predictions <- predict(model, test_data, type = "response")
      predicted_class <- ifelse(predictions > 0.5, 1, 0)
      
      accuracies[i] <- mean(predicted_class == test_data$AttritionBinary)
    }
    
    return(accuracies)
  }
  
  cv_results <- perform_cv(test_data)
  
  expect_length(cv_results, 5)
  expect_true(all(cv_results >= 0 & cv_results <= 1))
  expect_true(sd(cv_results) < 0.2)  # Consistency check
})

# =============================================================================
# 3. RISK FACTOR IDENTIFICATION TESTS
# =============================================================================

test_that("Risk factor analysis identifies significant predictors", {
  test_data <- create_test_data(1000)
  test_data$AttritionBinary <- ifelse(test_data$Attrition == "Yes", 1, 0)
  
  # Function to identify risk factors
  identify_risk_factors <- function(data) {
    model <- glm(AttritionBinary ~ Age + Salary + YearsAtCompany + 
                 JobSatisfaction + WorkLifeBalance + DistanceFromHome,
                 data = data, family = binomial)
    
    # Get coefficients and p-values
    summary_model <- summary(model)
    coefficients <- summary_model$coefficients
    
    # Extract significant factors (p < 0.05)
    significant_factors <- coefficients[coefficients[, "Pr(>|z|)"] < 0.05, ]
    
    return(list(
      model = model,
      significant_factors = significant_factors,
      all_coefficients = coefficients
    ))
  }
  
  risk_analysis <- identify_risk_factors(test_data)
  
  # Test that analysis produces results
  expect_s3_class(risk_analysis$model, "glm")
  expect_true(is.matrix(risk_analysis$all_coefficients))
  expect_true(ncol(risk_analysis$all_coefficients) == 4)  # Estimate, Std. Error, z value, Pr(>|z|)
  
  # Test odds ratios calculation
  odds_ratios <- exp(coef(risk_analysis$model))
  expect_true(all(odds_ratios > 0))
  expect_equal(length(odds_ratios), length(coef(risk_analysis$model)))
})

test_that("Risk scoring function works correctly", {
  test_data <- create_test_data(200)
  
  # Risk scoring function
  calculate_risk_score <- function(data) {
    # Normalize factors to 0-1 scale
    risk_scores <- data %>%
      mutate(
        age_risk = ifelse(Age < 30 | Age > 55, 0.3, 0.1),
        tenure_risk = ifelse(YearsAtCompany < 2, 0.4, ifelse(YearsAtCompany > 15, 0.2, 0.1)),
        satisfaction_risk = (6 - JobSatisfaction) / 5 * 0.3,
        distance_risk = pmin(DistanceFromHome / 50, 1) * 0.2,
        total_risk = age_risk + tenure_risk + satisfaction_risk + distance_risk
      ) %>%
      select(EmployeeID, total_risk)
    
    return(risk_scores)
  }
  
  risk_scores <- calculate_risk_score(test_data)
  
  expect_equal(nrow(risk_scores), nrow(test_data))
  expect_true(all(risk_scores$total_risk >= 0))
  expect_true(all(risk_scores$total_risk <= 1.2))  # Maximum possible score
  expect_false(any(is.na(risk_scores$total_risk)))
})

# =============================================================================
# 4. SURVIVAL ANALYSIS CORRECTNESS TESTS
# =============================================================================

test_that("Kaplan-Meier survival analysis is correctly implemented", {
  test_data <- create_test_data(500)
  
  # Prepare survival data
  prepare_survival_data <- function(data) {
    data %>%
      mutate(
        time_to_event = YearsAtCompany,
        event = ifelse(Attrition == "Yes", 1, 0)
      ) %>%
      filter(time_to_event > 0)  # Remove zero tenure
  }
  
  survival_data <- prepare_survival_data(test_data)
  
  # Fit Kaplan-Meier model
  km_fit <- survfit(Surv(time_to_event, event) ~ 1, data = survival_data)
  
  # Test survival object
  expect_s3_class(km_fit, "survfit")
  expect_true(length(km_fit$time) > 0)
  expect_true(all(km_fit$surv >= 0 & km_fit$surv <= 1))
  expect_true(all(diff(km_fit$surv) <= 0))  # Survival probability should be non-increasing
  
  # Test survival probabilities at specific time points
  survival_at_1_year <- summary(km_fit, times = 1)$surv
  survival_at_5_years <- summary(km_fit, times = 5)$surv
  
  if (!is.null(survival_at_1_year) && !is.null(survival_at_5_years)) {
    expect_gte(survival_at_1_year, survival_at_5_years)  # 1-year survival >= 5-year survival
  }
})

test_that("Cox proportional hazards model is properly fitted", {
  test_data <- create_test_data(400)
  
  # Prepare data for Cox model
  cox_data <- test_data %>%
    mutate(
      time_to_event = YearsAtCompany,
      event = ifelse(Attrition == "Yes", 1, 0)
    ) %>%
    filter(time_to_event > 0)
  
  # Fit Cox model
  cox_model <- coxph(Surv(time_to_event, event) ~ Age + Salary + JobSatisfaction + 
                     WorkLifeBalance, data = cox_data)
  
  # Test model validity
  expect_s3_class(cox_model, "coxph")
  expect_true(length(coef(cox_model)) > 0)
  expect_false(any(is.na(coef(cox_model))))
  
  # Test hazard ratios
  hazard_ratios <- exp(coef(cox_model))
  expect_true(all(hazard_ratios > 0))
  
  # Test model summary
  model_summary <- summary(cox_model)
  expect_true("concordance" %in% names(model_summary))
  expect_gte(model_summary$concordance["C"], 0.5)
  expect_lte(model_summary$concordance["C"], 1.0)
})

# =============================================================================
# 5. CROSS-TABULATION ACCURACY TESTS
# =============================================================================

test_that("Cross-tabulation calculations are accurate", {
  test_data <- create_test_data(1000)
  
  # Create cross-tabulation function
  create_crosstab <- function(data, var1, var2) {
    # Manual calculation
    manual_tab <- table(data[[var1]], data[[var2]])
    
    # Using dplyr
    dplyr_tab <- data %>%
      count(!!sym(var1), !!sym(var2)) %>%
      pivot_wider(names_from = !!sym(var2), values_from = n, values_fill = 0)
    
    return(list(manual = manual_tab, dplyr = dplyr_tab))
  }
  
  # Test Department vs Attrition cross-tab
  crosstab_result <- create_crosstab(test_data, "Department", "Attrition")
  
  # Test dimensions
  expect_true(is.table(crosstab_result$manual))
  expect_equal(sum(crosstab_result$manual), nrow(test_data))
  
  # Test chi-square test
  chi_test <- chisq.test(crosstab_result$manual)
  expect_s3_class(chi_test, "htest")
  expect_true(chi_test$statistic >= 0)
  expect_true(chi_test$p.value >= 0 & chi_test$p.value <= 1)
  
  # Test expected frequencies
  expect_equal(dim(chi_test$expected), dim(crosstab_result$manual))
  expect_true(abs(sum(chi_test$expected) - sum(crosstab_result$manual)) < 1e-10)
})

test_that("Contingency table percentages are correct", {
  test_data <- create_test_data(500)
  
  # Calculate percentages
  calculate_percentages <- function(data, row_var, col_var) {
    tab <- table(data[[row_var]], data[[col_var]])
    
    # Row percentages
    row_pct <- prop.table(tab, margin = 1) * 100
    
    # Column percentages  
    col_pct <- prop.table(tab, margin = 2) * 100
    
    # Total percentages
    total_pct <- prop.table(tab) * 100
    
    return(list(
      counts = tab,
      row_pct = row_pct,
      col_pct = col_pct,
      total_pct = total_pct
    ))
  }
  
  pct_result <- calculate_percentages(test_data, "Gender", "Attrition")
  
  # Test row percentages sum to 100
  expect_true(all(abs(rowSums(pct_result$row_pct) - 100) < 1e-10))
  
  # Test column percentages sum to 100
  expect_true(all(abs(colSums(pct_result$col_pct) - 100) < 1e-10))
  
  # Test total percentages sum to 100
  expect_true(abs(sum(pct_result$total_pct) - 100) < 1e-10)
})

# =============================================================================
# 6. TREND ANALYSIS VALIDITY TESTS
# =============================================================================

test_that("Time series trend analysis is statistically valid", {
  trend_data <- create_seasonal_data()
  
  # Calculate attrition rate trend
  trend_data <- trend_data %>%
    mutate(
      AttritionRate = AttritionCount / TotalEmployees,
      Year = year(Month),
      MonthNum = month(Month)
    )
  
  # Linear trend analysis
  trend_model <- lm(AttritionRate ~ as.numeric(Month), data = trend_data)
  
  # Test model validity
  expect_s3_class(trend_model, "lm")
  expect_equal(length(coef(trend_model)), 2)  # Intercept + slope
  
  # Test R-squared
  r_squared <- summary(trend_model)$r.squared
  expect_gte(r_squared, 0)
  expect_lte(r_squared, 1)
  
  # Test residuals
  residuals <- residuals(trend_model)
  expect_equal(length(residuals), nrow(trend_data))
  expect_true(abs(mean(residuals)) < 1e-10)  # Mean of residuals should be ~0
  
  # Moving average calculation
  calculate_moving_average <- function(x, n = 3) {
    stats::filter(x, rep(1/n, n), sides = 2)
  }
  
  ma_values <- calculate_moving_average(trend_data$AttritionRate, 3)
  expect_equal(length(ma_values), nrow(trend_data))
})

test_that("Seasonal decomposition works correctly", {
  trend_data <- create_seasonal_data()
  
  # Create time series
  ts_data <- ts(trend_data$AttritionCount, frequency = 12, start = c(2020, 1))
  
  # Seasonal decomposition
  decomp <- decompose(ts_data)
  
  # Test decomposition components
  expect_s3_class(decomp, "decomposed.ts")
  expect_equal(length(decomp$seasonal), length(ts_data))
  expect_equal(length(decomp$trend), length(ts_data))
  expect_equal(length(decomp$random), length(ts_data))
  
  # Test that components sum to original (excluding NAs)
  reconstructed <- decomp$seasonal + decomp$trend + decomp$random
  valid_indices <- !is.na(reconstructed)
  expect_true(all(abs(ts_data[valid_indices] - reconstructed[valid_indices]) < 1e-10))
})

# =============================================================================
# 7. CORRELATION CALCULATIONS TESTS
# =============================================================================

test_that("Correlation matrices are mathematically correct", {
  test_data <- create_test_data(300)
  
  # Select numeric variables
  numeric_vars <- test_data %>%
    select(Age, Salary, YearsAtCompany, YearsInRole, JobSatisfaction, 
           WorkLifeBalance, DistanceFromHome) %>%
    select_if(is.numeric)
  
  # Calculate correlation matrix
  corr_matrix <- cor(numeric_vars, use = "complete.obs")
  
  # Test correlation matrix properties
  expect_true(is.matrix(corr_matrix))
  expect_equal(nrow(corr_matrix), ncol(corr_matrix))
  expect_true(all(diag(corr_matrix) == 1))  # Diagonal should be 1
  expect_true(all(corr_matrix >= -1 & corr_matrix <= 1))  # Values between -1 and 1
  expect_true(all(corr_matrix == t(corr_matrix)))  # Matrix should be symmetric
  
  # Test specific correlation calculations
  manual_corr <- cor(numeric_vars$Age, numeric_vars$Salary, use = "complete.obs")
  expect_equal(corr_matrix["Age", "Salary"], manual_corr, tolerance = 1e-10)
  
  # Test different correlation methods
  pearson_corr <- cor(numeric_vars, method = "pearson")
  spearman_corr <- cor(numeric_vars, method = "spearman")
  kendall_corr <- cor(numeric_vars, method = "kendall")
  
  expect_true(all(abs(pearson_corr) <= 1))
  expect_true(all(abs(spearman_corr) <= 1))
  expect_true(all(abs(kendall_corr) <= 1))
})

test_that("Correlation significance tests are accurate", {
  test_data <- create_test_data(200)
  
  # Function to test correlation significance
  test_correlation_significance <- function(x, y, alpha = 0.05) {
    corr_test <- cor.test(x, y)
    
    list(
      correlation = corr_test$estimate,
      p_value = corr_test$p.value,
      confidence_interval = corr_test$conf.int,
      significant = corr_test$p.value < alpha
    )
  }
  
  # Test correlation between salary and job satisfaction
  corr_result <- test_correlation_significance(test_data$Salary, test_data$JobSatisfaction)
  
  expect_gte(corr_result$correlation, -1)
  expect_lte(corr_result$correlation, 1)
  expect_gte(corr_result$p_value, 0)
  expect_lte(corr_result$p_value, 1)
  expect_length(corr_result$confidence_interval, 2)
  expect_lte(corr_result$confidence_interval[1], corr_result$correlation)
  expect_gte(corr_result$confidence_interval[2], corr_result$correlation)
})

# =============================================================================
# 8. HYPOTHESIS TESTING RESULTS TESTS
# =============================================================================

test_that("T-tests for group comparisons are statistically valid", {
  test_data <- create_test_data(400)
  
  # Test salary differences between attrition groups
  perform_ttest <- function(data, group_var, numeric_var) {
    group1 <- data[data[[group_var]] == "Yes", numeric_var]
    group2 <- data[data[[group_var]] == "No", numeric_var]
    
    # Remove NAs
    group1 <- group1[!is.na(group1)]
    group2 <- group2[!is.na(group2)]
    
    # Perform t-test
    t_result <- t.test(group1, group2)
    
    return(list(
      statistic = t_result$statistic,
      p_value = t_result$p.value,
      confidence_interval = t_result$conf.int,
      mean_group1 = mean(group1),
      mean_group2 = mean(group2),
      effect_size = (mean(group1) - mean(group2)) / sqrt(((length(group1)-1)*var(group1) + (length(group2)-1)*var(group2)) / (length(group1) + length(group2) - 2))
    ))
  }
  
  ttest_result <- perform_ttest(test_data, "Attrition", "Salary")
  
  # Test t-test results
  expect_true(is.numeric(ttest_result$statistic))
  expect_gte(ttest_result$p_value, 0)
  expect_lte(ttest_result$p_value, 1)
  expect_length(ttest_result$confidence_interval, 2)
  expect_true(is.numeric(ttest_result$effect_size))
})

test_that("ANOVA tests for multiple group comparisons work correctly", {
  test_data <- create_test_data(500)
  
  # Test salary differences across departments
  perform_anova <- function(data, group_var, numeric_var) {
    formula_str <- paste(numeric_var, "~", group_var)
    anova_result <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_result)
    
    return(list(
      model = anova_result,
      f_statistic = anova_summary[[1]][["F value"]][1],
      p_value = anova_summary[[1]][["Pr(>F)"]][1],
      degrees_freedom = anova_summary[[1]][["Df"]],
      sum_squares = anova_summary[[1]][["Sum Sq"]]
    ))
  }
  
  anova_result <- perform_anova(test_data, "Department", "Salary")
  
  # Test ANOVA results
  expect_s3_class(anova_result$model, "aov")
  expect_gte(anova_result$f_statistic, 0)
  expect_gte(anova_result$p_value, 0)
  expect_lte(anova_result$p_value, 1)
  expect_true(length(anova_result$degrees_freedom) >= 2)
  expect_true(all(anova_result$sum_squares >= 0))
})

# =============================================================================
# 9. CONFIDENCE INTERVAL ACCURACY TESTS
# =============================================================================

test_that("Confidence intervals are mathematically correct", {
  test_data <- create_test_data(300)
  
  # Function to calculate confidence intervals for means
  calculate_mean_ci <- function(x, confidence_level = 0.95) {
    x <- x[!is.na(x)]
    n <- length(x)
    mean_x <- mean(x)
    se <- sd(x) / sqrt(n)
    
    alpha <- 1 - confidence_level
    t_critical <- qt(1 - alpha/2, df = n - 1)
    
    margin_error <- t_critical * se
    
    return(list(
      mean = mean_x,
      lower = mean_x - margin_error,
      upper = mean_x + margin_error,
      margin_error = margin_error,
      confidence_level = confidence_level
    ))
  }
  
  # Test CI for salary
  salary_ci <- calculate_mean_ci(test_data$Salary, 0.95)
  
  expect_lte(salary_ci$lower, salary_ci$mean)
  expect_gte(salary_ci$upper, salary_ci$mean)
  expect_equal(salary_ci$upper - salary_ci$lower, 2 * salary_ci$margin_error, tolerance = 1e-10)
  expect_equal(salary_ci$confidence_level, 0.95)
  
  # Test different confidence levels
  ci_90 <- calculate_mean_ci(test_data$Salary, 0.90)
  ci_99 <- calculate_mean_ci(test_data$Salary, 0.99)
  
  # 99% CI should be wider than 90% CI
  expect_gt(ci_99$upper - ci_99$lower, ci_90$upper - ci_90$lower)
})

test_that("Proportion confidence intervals are accurate", {
  test_data <- create_test_data(400)
  
  # Function to calculate proportion CI
  calculate_proportion_ci <- function(successes, n, confidence_level = 0.95) {
    p_hat <- successes / n
    alpha <- 1 - confidence_level
    z_critical <- qnorm(1 - alpha/2)
    
    # Wilson score interval (more accurate than normal approximation)
    wilson_center <- (p_hat + z_critical^2/(2*n)) / (1 + z_critical^2/n)
    wilson_width <- z_critical * sqrt(p_hat*(1-p_hat)/n + z_critical^2/(4*n^2)) / (1 + z_critical^2/n)
    
    return