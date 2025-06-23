# =============================================================================
# ATLAS LABS HR ANALYTICS - ATTRITION ANALYSIS MODULE UNIT TESTS
# =============================================================================
# Comprehensive statistical validation tests for attrition analysis functions
# Author: akhapwoyaco
# Focus Areas: Statistical accuracy, predictive models, survival analysis
# =============================================================================

# Load required libraries for testing
library(testthat)
library(tidyverse)
library(survival)
library(randomForest)
library(corrplot)
library(forecast)
library(boot)
library(car)

# Source the attrition module (assuming it exists)
# source("modules/attrition_module.R")

# =============================================================================
# TEST SETUP & MOCK DATA GENERATION
# =============================================================================

#' Generate comprehensive mock HR data for testing
#' @param n_employees Number of employees to generate
#' @param seed Random seed for reproducibility
#' @return Tibble with realistic HR data structure
generate_test_data <- function(n_employees = 1000, seed = 42) {
  set.seed(seed)
  
  tibble(
    EmployeeID = 1:n_employees,
    Age = sample(22:65, n_employees, replace = TRUE),
    Gender = sample(c("Male", "Female", "Other"), n_employees, 
                   replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    Department = sample(c("IT", "HR", "Finance", "Marketing", "Operations"), 
                       n_employees, replace = TRUE, prob = c(0.3, 0.15, 0.2, 0.15, 0.2)),
    JobRole = sample(c("Analyst", "Manager", "Director", "Specialist", "Coordinator"),
                    n_employees, replace = TRUE),
    Salary = rnorm(n_employees, 75000, 25000),
    YearsAtCompany = sample(0:20, n_employees, replace = TRUE),
    YearsInRole = pmin(sample(0:15, n_employees, replace = TRUE), YearsAtCompany),
    YearsSincePromotion = sample(0:8, n_employees, replace = TRUE),
    DistanceFromHome = sample(1:50, n_employees, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n_employees, replace = TRUE, prob = c(0.3, 0.7)),
    BusinessTravel = sample(c("None", "Rarely", "Frequently"), n_employees,
                           replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    JobSatisfaction = sample(1:5, n_employees, replace = TRUE),
    WorkLifeBalance = sample(1:5, n_employees, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n_employees, replace = TRUE),
    # Create realistic attrition based on factors
    Attrition = case_when(
      JobSatisfaction <= 2 & YearsSincePromotion > 5 ~ sample(c("Yes", "No"), n_employees, 
                                                             replace = TRUE, prob = c(0.7, 0.3)),
      OverTime == "Yes" & WorkLifeBalance <= 2 ~ sample(c("Yes", "No"), n_employees,
                                                       replace = TRUE, prob = c(0.6, 0.4)),
      Salary < 50000 & YearsAtCompany > 5 ~ sample(c("Yes", "No"), n_employees,
                                                   replace = TRUE, prob = c(0.5, 0.5)),
      TRUE ~ sample(c("Yes", "No"), n_employees, replace = TRUE, prob = c(0.15, 0.85))
    ),
    # Add time-based data for survival analysis
    HireDate = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), 
                   length.out = n_employees) + sample(-365:365, n_employees, replace = TRUE),
    ExitDate = if_else(Attrition == "Yes", 
                      HireDate + days(sample(30:1460, n_employees, replace = TRUE)),
                      as.Date(NA))
  ) %>%
  # Add seasonal hiring patterns
  mutate(
    HireMonth = month(HireDate),
    HireQuarter = quarter(HireDate),
    # Calculate tenure in days for survival analysis
    TenureDays = if_else(is.na(ExitDate), 
                        as.numeric(Sys.Date() - HireDate),
                        as.numeric(ExitDate - HireDate)),
    # Event indicator for survival analysis (1 = left, 0 = censored)
    Event = if_else(Attrition == "Yes", 1, 0)
  )
}

# Generate test datasets
test_data_small <- generate_test_data(100, 123)
test_data_large <- generate_test_data(1000, 456)
test_data_edge <- generate_test_data(50, 789)

# =============================================================================
# 1. STATISTICAL CALCULATION ACCURACY TESTS
# =============================================================================

test_that("Basic attrition rate calculations are accurate", {
  
  # Test basic attrition rate calculation
  calc_attrition_rate <- function(data) {
    data %>%
      summarise(
        total_employees = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / total_employees,
        .groups = "drop"
      )
    }
  
  result <- calc_attrition_rate(test_data_small)
  
  # Verify calculations
  expect_equal(result$total_employees, nrow(test_data_small))
  expect_equal(result$attrition_count, sum(test_data_small$Attrition == "Yes"))
  expect_equal(result$attrition_rate, result$attrition_count / result$total_employees)
  expect_true(result$attrition_rate >= 0 && result$attrition_rate <= 1)
})

test_that("Departmental attrition calculations are precise", {
  
  calc_dept_attrition <- function(data) {
    data %>%
      group_by(Department) %>%
      summarise(
        employees = n(),
        attritions = sum(Attrition == "Yes"),
        rate = attritions / employees,
        .groups = "drop"
      )
  }
  
  result <- calc_dept_attrition(test_data_large)
  
  # Verify group calculations
  total_employees <- sum(result$employees)
  total_attritions <- sum(result$attritions)
  
  expect_equal(total_employees, nrow(test_data_large))
  expect_equal(total_attritions, sum(test_data_large$Attrition == "Yes"))
  expect_true(all(result$rate >= 0 & result$rate <= 1))
  expect_true(all(result$attritions <= result$employees))
})

test_that("Salary band attrition analysis accuracy", {
  
  calc_salary_band_attrition <- function(data, bands = 5) {
    data %>%
      mutate(
        SalaryBand = cut(Salary, breaks = bands, labels = paste0("Band_", 1:bands))
      ) %>%
      group_by(SalaryBand) %>%
      summarise(
        count = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / count,
        avg_salary = mean(Salary, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  result <- calc_salary_band_attrition(test_data_large)
  
  # Verify band calculations
  expect_equal(sum(result$count), nrow(test_data_large))
  expect_equal(sum(result$attrition_count), sum(test_data_large$Attrition == "Yes"))
  expect_true(all(!is.na(result$attrition_rate)))
  expect_true(all(result$attrition_rate >= 0 & result$attrition_rate <= 1))
})

# =============================================================================
# 2. PREDICTIVE MODEL VALIDATION TESTS
# =============================================================================

test_that("Logistic regression model accuracy and validation", {
  
  build_attrition_model <- function(data) {
    # Prepare data
    model_data <- data %>%
      mutate(
        Attrition_Binary = if_else(Attrition == "Yes", 1, 0),
        OverTime_Binary = if_else(OverTime == "Yes", 1, 0)
      ) %>%
      select(Attrition_Binary, Age, Salary, YearsAtCompany, JobSatisfaction, 
             WorkLifeBalance, DistanceFromHome, OverTime_Binary) %>%
      na.omit()
    
    # Build logistic regression model
    model <- glm(Attrition_Binary ~ ., data = model_data, family = binomial)
    
    # Calculate model metrics
    predictions <- predict(model, type = "response")
    predicted_class <- if_else(predictions > 0.5, 1, 0)
    
    list(
      model = model,
      predictions = predictions,
      accuracy = mean(predicted_class == model_data$Attrition_Binary),
      auc = pROC::auc(model_data$Attrition_Binary, predictions)
    )
  }
  
  skip_if_not_installed("pROC")
  
  model_result <- build_attrition_model(test_data_large)
  
  # Validate model structure
  expect_s3_class(model_result$model, "glm")
  expect_true(model_result$accuracy >= 0 && model_result$accuracy <= 1)
  expect_true(model_result$auc >= 0.5 && model_result$auc <= 1)
  expect_equal(length(model_result$predictions), 
               nrow(na.omit(test_data_large %>% 
                           select(Age, Salary, YearsAtCompany, JobSatisfaction, 
                                 WorkLifeBalance, DistanceFromHome, OverTime))))
})

test_that("Random Forest model validation and feature importance", {
  
  build_rf_model <- function(data, ntree = 100) {
    # Prepare data
    model_data <- data %>%
      mutate(
        Attrition_Factor = factor(Attrition),
        OverTime_Factor = factor(OverTime),
        Department_Factor = factor(Department)
      ) %>%
      select(Attrition_Factor, Age, Salary, YearsAtCompany, JobSatisfaction,
             WorkLifeBalance, DistanceFromHome, OverTime_Factor, Department_Factor) %>%
      na.omit()
    
    # Build Random Forest model
    rf_model <- randomForest(Attrition_Factor ~ ., data = model_data, 
                           ntree = ntree, importance = TRUE)
    
    # Calculate out-of-bag error
    oob_error <- rf_model$err.rate[ntree, "OOB"]
    
    list(
      model = rf_model,
      oob_error = oob_error,
      importance = importance(rf_model),
      accuracy = 1 - oob_error
    )
  }
  
  skip_if_not_installed("randomForest")
  
  rf_result <- build_rf_model(test_data_large, ntree = 50)
  
  # Validate Random Forest model
  expect_s3_class(rf_result$model, "randomForest")
  expect_true(rf_result$accuracy >= 0 && rf_result$accuracy <= 1)
  expect_true(rf_result$oob_error >= 0 && rf_result$oob_error <= 1)
  expect_true(nrow(rf_result$importance) > 0)
  expect_true(ncol(rf_result$importance) >= 2) # MeanDecreaseAccuracy and MeanDecreaseGini
})

# =============================================================================
# 3. RISK FACTOR IDENTIFICATION TESTS
# =============================================================================

test_that("Risk factor identification accuracy", {
  
  identify_risk_factors <- function(data) {
    # Calculate risk scores for different factors
    risk_analysis <- data %>%
      group_by(Department) %>%
      summarise(
        dept_attrition_rate = mean(Attrition == "Yes"),
        .groups = "drop"
      ) %>%
      mutate(dept_risk_score = scale(dept_attrition_rate)[,1])
    
    # Tenure risk analysis
    tenure_risk <- data %>%
      mutate(
        tenure_bucket = cut(YearsAtCompany, breaks = c(0, 2, 5, 10, Inf),
                          labels = c("0-2", "2-5", "5-10", "10+"))
      ) %>%
      group_by(tenure_bucket) %>%
      summarise(
        tenure_attrition_rate = mean(Attrition == "Yes"),
        .groups = "drop"
      )
    
    # Satisfaction risk analysis
    satisfaction_risk <- data %>%
      group_by(JobSatisfaction) %>%
      summarise(
        satisfaction_attrition_rate = mean(Attrition == "Yes"),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count >= 10) # Only consider groups with sufficient data
    
    list(
      department_risk = risk_analysis,
      tenure_risk = tenure_risk,
      satisfaction_risk = satisfaction_risk
    )
  }
  
  risk_result <- identify_risk_factors(test_data_large)
  
  # Validate risk factor calculations
  expect_true(all(risk_result$department_risk$dept_attrition_rate >= 0 & 
                 risk_result$department_risk$dept_attrition_rate <= 1))
  expect_true(all(risk_result$tenure_risk$tenure_attrition_rate >= 0 & 
                 risk_result$tenure_risk$tenure_attrition_rate <= 1))
  expect_true(all(risk_result$satisfaction_risk$satisfaction_attrition_rate >= 0 & 
                 risk_result$satisfaction_risk$satisfaction_attrition_rate <= 1))
  
  # Validate risk score standardization
  expect_true(abs(mean(risk_result$department_risk$dept_risk_score)) < 1e-10)
  expect_true(abs(sd(risk_result$department_risk$dept_risk_score) - 1) < 1e-10)
})

test_that("High-risk employee identification", {
  
  identify_high_risk_employees <- function(data, threshold = 0.7) {
    # Create composite risk score
    high_risk <- data %>%
      mutate(
        risk_score = (
          (6 - JobSatisfaction) * 0.25 +  # Higher weight for low satisfaction
          (6 - WorkLifeBalance) * 0.20 +
          (YearsSincePromotion / 10) * 0.15 +
          (DistanceFromHome / 50) * 0.10 +
          if_else(OverTime == "Yes", 0.15, 0) +
          (Age > 50) * 0.15  # Age factor
        ),
        risk_category = case_when(
          risk_score >= threshold ~ "High Risk",
          risk_score >= 0.5 ~ "Medium Risk",
          TRUE ~ "Low Risk"
        )
      )
    
    # Validate risk scores
    list(
      risk_distribution = table(high_risk$risk_category),
      avg_risk_by_attrition = high_risk %>%
        group_by(Attrition) %>%
        summarise(avg_risk = mean(risk_score), .groups = "drop"),
      high_risk_employees = high_risk %>%
        filter(risk_category == "High Risk") %>%
        select(EmployeeID, risk_score, risk_category)
    )
  }
  
  risk_result <- identify_high_risk_employees(test_data_large)
  
  # Validate risk identification
  expect_true(sum(risk_result$risk_distribution) == nrow(test_data_large))
  expect_true(all(c("High Risk", "Medium Risk", "Low Risk") %in% 
                 names(risk_result$risk_distribution)))
  expect_true(nrow(risk_result$avg_risk_by_attrition) == 2)
  expect_true(all(risk_result$high_risk_employees$risk_score >= 0.7))
})

# =============================================================================
# 4. SURVIVAL ANALYSIS CORRECTNESS TESTS
# =============================================================================

test_that("Kaplan-Meier survival analysis accuracy", {
  
  perform_survival_analysis <- function(data) {
    # Prepare survival data
    surv_data <- data %>%
      filter(!is.na(TenureDays)) %>%
      mutate(
        time = pmax(TenureDays, 1), # Ensure positive time
        status = Event
      )
    
    # Fit Kaplan-Meier model
    km_fit <- survfit(Surv(time, status) ~ 1, data = surv_data)
    
    # Calculate median survival time
    median_survival <- summary(km_fit)$table["median"]
    
    # Calculate survival probabilities at specific time points
    time_points <- c(365, 730, 1095) # 1, 2, 3 years
    surv_probs <- summary(km_fit, times = time_points)
    
    list(
      km_model = km_fit,
      median_survival = median_survival,
      survival_probabilities = if(length(surv_probs$surv) > 0) {
        data.frame(
          time = surv_probs$time,
          survival_prob = surv_probs$surv,
          std_error = surv_probs$std.err
        )
      } else {
        data.frame(time = numeric(0), survival_prob = numeric(0), std_error = numeric(0))
      },
      total_events = sum(surv_data$status),
      total_observations = nrow(surv_data)
    )
  }
  
  skip_if_not_installed("survival")
  
  surv_result <- perform_survival_analysis(test_data_large)
  
  # Validate survival analysis
  expect_s3_class(surv_result$km_model, "survfit")
  expect_true(surv_result$median_survival > 0 || is.na(surv_result$median_survival))
  expect_true(surv_result$total_events >= 0)
  expect_true(surv_result$total_observations > 0)
  expect_true(surv_result$total_events <= surv_result$total_observations)
  
  if(nrow(surv_result$survival_probabilities) > 0) {
    expect_true(all(surv_result$survival_probabilities$survival_prob >= 0 & 
                   surv_result$survival_probabilities$survival_prob <= 1))
    expect_true(all(surv_result$survival_probabilities$std_error >= 0))
  }
})

test_that("Cox proportional hazards model validation", {
  
  fit_cox_model <- function(data) {
    # Prepare data for Cox regression
    cox_data <- data %>%
      filter(!is.na(TenureDays)) %>%
      mutate(
        time = pmax(TenureDays, 1),
        status = Event,
        overtime_binary = if_else(OverTime == "Yes", 1, 0)
      ) %>%
      select(time, status, Age, Salary, JobSatisfaction, WorkLifeBalance, overtime_binary) %>%
      na.omit()
    
    # Fit Cox model
    cox_model <- coxph(Surv(time, status) ~ Age + Salary + JobSatisfaction + 
                      WorkLifeBalance + overtime_binary, data = cox_data)
    
    # Calculate hazard ratios
    hazard_ratios <- exp(coef(cox_model))
    
    # Test proportional hazards assumption
    ph_test <- cox.zph(cox_model)
    
    list(
      model = cox_model,
      hazard_ratios = hazard_ratios,
      ph_test_pvalues = ph_test$table[,"p"],
      concordance = summary(cox_model)$concordance["C"],
      model_pvalue = summary(cox_model)$logtest["pvalue"]
    )
  }
  
  skip_if_not_installed("survival")
  
  cox_result <- fit_cox_model(test_data_large)
  
  # Validate Cox model
  expect_s3_class(cox_result$model, "coxph")
  expect_true(all(cox_result$hazard_ratios > 0))
  expect_true(cox_result$concordance >= 0.5 && cox_result$concordance <= 1)
  expect_true(cox_result$model_pvalue >= 0 && cox_result$model_pvalue <= 1)
  expect_true(all(cox_result$ph_test_pvalues >= 0 & cox_result$ph_test_pvalues <= 1))
})

# =============================================================================
# 5. CROSS-TABULATION ACCURACY TESTS
# =============================================================================

test_that("Cross-tabulation calculations are accurate", {
  
  create_cross_tabs <- function(data) {
    # Department vs Attrition cross-tab
    dept_attrition <- table(data$Department, data$Attrition)
    
    # Calculate expected frequencies for chi-square test
    expected_freq <- chisq.test(dept_attrition)$expected
    
    # Gender vs Attrition cross-tab
    gender_attrition <- table(data$Gender, data$Attrition)
    
    # OverTime vs Attrition cross-tab
    overtime_attrition <- table(data$OverTime, data$Attrition)
    
    list(
      dept_cross_tab = dept_attrition,
      expected_frequencies = expected_freq,
      gender_cross_tab = gender_attrition,
      overtime_cross_tab = overtime_attrition,
      # Calculate proportions
      dept_proportions = prop.table(dept_attrition, margin = 1),
      gender_proportions = prop.table(gender_attrition, margin = 1)
    )
  }
  
  cross_tab_result <- create_cross_tabs(test_data_large)
  
  # Validate cross-tabulation accuracy
  expect_equal(sum(cross_tab_result$dept_cross_tab), nrow(test_data_large))
  expect_equal(sum(cross_tab_result$gender_cross_tab), nrow(test_data_large))
  expect_equal(sum(cross_tab_result$overtime_cross_tab), nrow(test_data_large))
  
  # Validate proportions sum to 1 across rows
  expect_true(all(abs(rowSums(cross_tab_result$dept_proportions) - 1) < 1e-10))
  expect_true(all(abs(rowSums(cross_tab_result$gender_proportions) - 1) < 1e-10))
  
  # Validate expected frequencies are positive
  expect_true(all(cross_tab_result$expected_frequencies > 0))
})

test_that("Multi-dimensional cross-tabulation accuracy", {
  
  create_multi_cross_tabs <- function(data) {
    # Three-way cross-tabulation: Department x Gender x Attrition
    three_way <- ftable(data$Department, data$Gender, data$Attrition)
    
    # Convert to array for easier manipulation
    three_way_array <- as.array(three_way)
    
    # Calculate marginal totals
    dept_totals <- apply(three_way_array, 1, sum)
    gender_totals <- apply(three_way_array, 2, sum)
    attrition_totals <- apply(three_way_array, 3, sum)
    
    list(
      three_way_table = three_way,
      three_way_array = three_way_array,
      department_marginals = dept_totals,
      gender_marginals = gender_totals,
      attrition_marginals = attrition_totals,
      grand_total = sum(three_way_array)
    )
  }
  
  multi_cross_result <- create_multi_cross_tabs(test_data_large)
  
  # Validate multi-dimensional cross-tabs
  expect_equal(multi_cross_result$grand_total, nrow(test_data_large))
  expect_equal(sum(multi_cross_result$department_marginals), nrow(test_data_large))
  expect_equal(sum(multi_cross_result$gender_marginals), nrow(test_data_large))
  expect_equal(sum(multi_cross_result$attrition_marginals), nrow(test_data_large))
  
  # Validate array dimensions
  expect_equal(length(dim(multi_cross_result$three_way_array)), 3)
})

# =============================================================================
# 6. TREND ANALYSIS VALIDITY TESTS
# =============================================================================

test_that("Time series trend analysis accuracy", {
  
  analyze_attrition_trends <- function(data) {
    # Create monthly attrition time series
    monthly_data <- data %>%
      filter(!is.na(HireDate)) %>%
      mutate(
        hire_year_month = floor_date(HireDate, "month")
      ) %>%
      group_by(hire_year_month) %>%
      summarise(
        hires = n(),
        attritions = sum(Attrition == "Yes"),
        attrition_rate = attritions / hires,
        .groups = "drop"
      ) %>%
      arrange(hire_year_month) %>%
      filter(hires >= 5) # Only months with sufficient data
    
    if(nrow(monthly_data) < 12) {
      return(list(insufficient_data = TRUE))
    }
    
    # Create time series object
    ts_attrition <- ts(monthly_data$attrition_rate, 
                      start = c(year(min(monthly_data$hire_year_month)), 
                               month(min(monthly_data$hire_year_month))),
                      frequency = 12)
    
    # Decompose time series
    decomp <- decompose(ts_attrition, type = "additive")
    
    # Fit linear trend
    time_index <- 1:length(ts_attrition)
    trend_model <- lm(as.numeric(ts_attrition) ~ time_index)
    
    # Calculate trend slope and significance
    trend_slope <- coef(trend_model)[2]
    trend_pvalue <- summary(trend_model)$coefficients[2, 4]
    
    list(
      monthly_data = monthly_data,
      time_series = ts_attrition,
      decomposition = decomp,
      trend_slope = trend_slope,
      trend_pvalue = trend_pvalue,
      trend_direction = case_when(
        trend_pvalue > 0.05 ~ "No significant trend",
        trend_slope > 0 ~ "Increasing trend",
        TRUE ~ "Decreasing trend"
      )
    )
  }
  
  skip_if_not_installed("forecast")
  
  trend_result <- analyze_attrition_trends(test_data_large)
  
  if(!isTRUE(trend_result$insufficient_data)) {
    # Validate trend analysis
    expect_s3_class(trend_result$time_series, "ts")
    expect_s3_class(trend_result$decomposition, "decomposed.ts")
    expect_true(is.numeric(trend_result$trend_slope))
    expect_true(trend_result$trend_pvalue >= 0 && trend_result$trend_pvalue <= 1)
    expect_true(trend_result$trend_direction %in% 
               c("No significant trend", "Increasing trend", "Decreasing trend"))
  }
})

test_that("Seasonal pattern detection accuracy", {
  
  detect_seasonal_patterns <- function(data) {
    # Analyze hiring and attrition by month
    seasonal_analysis <- data %>%
      mutate(
        hire_month = month(HireDate, label = TRUE),
        hire_quarter = quarter(HireDate)
      ) %>%
      group_by(hire_month) %>%
      summarise(
        hires = n(),
        attritions = sum(Attrition == "Yes"),
        attrition_rate = attritions / hires,
        .groups = "drop"
      )
    
    # Test for seasonal differences using ANOVA
    monthly_rates <- seasonal_analysis$attrition_rate
    month_factor <- factor(1:12)
    
    if(length(monthly_rates) == 12) {
      seasonal_test <- aov(monthly_rates ~ month_factor)
      seasonal_pvalue <- summary(seasonal_test)[[1]][["Pr(>F)"]][1]
    } else {
      seasonal_pvalue <- NA
    }
    
    # Calculate seasonal indices
    overall_mean <- mean(seasonal_analysis$attrition_rate)
    seasonal_indices <- seasonal_analysis$attrition_rate / overall_mean
    
    list(
      seasonal_data = seasonal_analysis,
      seasonal_test_pvalue = seasonal_pvalue,
      seasonal_indices = seasonal_indices,
      peak_month = seasonal_analysis$hire_month[which.max(seasonal_analysis$attrition_rate)],
      low_month = seasonal_analysis$hire_month[which.min(seasonal_analysis$attrition_rate)]
    )
  }
  
  seasonal_result <- detect_seasonal_patterns(test_data_large)
  
  # Validate seasonal analysis
  expect_equal(nrow(seasonal_result$seasonal_data), 12)
  expect_true(all(seasonal_result$seasonal_indices > 0))
  expect_true(mean(seasonal_result$seasonal_indices) <= 1.01 && 
             mean(seasonal_result$seasonal_indices) >= 0.99) # Should be ~1
  
  if(!is.na(seasonal_result$seasonal_test_pvalue)) {
    expect_true(seasonal_result$seasonal_test_pvalue >= 0 && 
               seasonal_result$seasonal_test_pvalue <= 1)
  }
})

# =============================================================================
# 7. CORRELATION CALCULATIONS TESTS
# =============================================================================

test_that("Correlation matrix accuracy and significance", {
  
  calculate_correlation_matrix <- function(data) {
    # Select numeric variables for correlation
    numeric_vars <- data %>%
      select(Age, Salary, YearsAtCompany, YearsInRole, YearsSincePromotion,
             DistanceFromHome, JobSatisfaction, WorkLifeBalance, 
             