# Demographics Module Unit Tests
# Atlas Labs HR Analytics Dashboard
# Comprehensive testing for demographics analysis functions

library(testthat)
library(dplyr)
library(tidyr)
library(purrr)

# Mock data generation for testing
generate_mock_employee_data <- function(n = 1000) {
  set.seed(123)
  
  data.frame(
    EmployeeID = 1:n,
    Age = sample(22:65, n, replace = TRUE),
    Gender = sample(c("Male", "Female", "Non-binary", "Prefer not to say"), n, 
                   replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Native American", "Other"), 
                      n, replace = TRUE, prob = c(0.4, 0.2, 0.15, 0.15, 0.05, 0.05)),
    State = sample(state.abb[1:20], n, replace = TRUE),
    Department = sample(c("Engineering", "Sales", "Marketing", "HR", "Finance"), n, 
                       replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    Salary = round(rnorm(n, 75000, 20000)),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.15, 0.85)),
    YearsAtCompany = sample(1:20, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Test data
test_data <- generate_mock_employee_data(1000)
small_data <- generate_mock_employee_data(50)
minimal_data <- generate_mock_employee_data(5)

# =============================================================================
# 1. DIVERSITY METRIC CALCULATIONS
# =============================================================================

test_that("Diversity Index Calculations", {
  
  # Simpson's Diversity Index calculation
  calculate_simpson_diversity <- function(data, column) {
    proportions <- data %>%
      count(!!sym(column)) %>%
      mutate(prop = n / sum(n)) %>%
      pull(prop)
    
    1 - sum(proportions^2)
  }
  
  # Shannon Diversity Index calculation
  calculate_shannon_diversity <- function(data, column) {
    proportions <- data %>%
      count(!!sym(column)) %>%
      mutate(prop = n / sum(n)) %>%
      filter(prop > 0) %>%
      pull(prop)
    
    -sum(proportions * log(proportions))
  }
  
  # Test Simpson's Diversity Index
  gender_simpson <- calculate_simpson_diversity(test_data, "Gender")
  ethnicity_simpson <- calculate_simpson_diversity(test_data, "Ethnicity")
  
  expect_true(gender_simpson >= 0 & gender_simpson <= 1)
  expect_true(ethnicity_simpson >= 0 & ethnicity_simpson <= 1)
  expect_type(gender_simpson, "double")
  expect_type(ethnicity_simpson, "double")
  
  # Test Shannon Diversity Index
  gender_shannon <- calculate_shannon_diversity(test_data, "Gender")
  ethnicity_shannon <- calculate_shannon_diversity(test_data, "Ethnicity")
  
  expect_true(gender_shannon >= 0)
  expect_true(ethnicity_shannon >= 0)
  expect_type(gender_shannon, "double")
  expect_type(ethnicity_shannon, "double")
  
  # Test edge cases
  uniform_data <- data.frame(Category = rep(c("A", "B", "C", "D"), each = 25))
  uniform_simpson <- calculate_simpson_diversity(uniform_data, "Category")
  expect_equal(uniform_simpson, 0.75, tolerance = 0.001)
  
  single_category <- data.frame(Category = rep("A", 100))
  single_simpson <- calculate_simpson_diversity(single_category, "Category")
  expect_equal(single_simpson, 0, tolerance = 0.001)
})

test_that("Representation Ratios", {
  
  calculate_representation_ratio <- function(data, group_col, benchmark_col) {
    group_dist <- data %>%
      count(!!sym(group_col)) %>%
      mutate(group_prop = n / sum(n))
    
    benchmark_dist <- data %>%
      count(!!sym(benchmark_col)) %>%
      mutate(bench_prop = n / sum(n))
    
    # Calculate ratio against expected representation
    expected_prop <- 1 / nrow(benchmark_dist)
    group_dist %>%
      mutate(representation_ratio = group_prop / expected_prop)
  }
  
  gender_rep <- calculate_representation_ratio(test_data, "Gender", "Department")
  
  expect_true(all(gender_rep$representation_ratio > 0))
  expect_true(all(is.finite(gender_rep$representation_ratio)))
  expect_equal(nrow(gender_rep), length(unique(test_data$Gender)))
  
  # Test with small sample
  small_rep <- calculate_representation_ratio(small_data, "Gender", "Department")
  expect_true(nrow(small_rep) > 0)
})

test_that("Diversity Trend Analysis", {
  
  analyze_diversity_trends <- function(data, time_col, group_col) {
    data %>%
      group_by(!!sym(time_col)) %>%
      summarise(
        diversity_index = calculate_simpson_diversity(cur_data(), group_col),
        total_count = n(),
        .groups = "drop"
      ) %>%
      mutate(
        trend_slope = ifelse(row_number() > 1, 
                            diversity_index - lag(diversity_index), 
                            NA)
      )
  }
  
  # Add mock time column
  test_data_time <- test_data %>%
    mutate(HireYear = sample(2018:2023, nrow(.), replace = TRUE))
  
  trends <- analyze_diversity_trends(test_data_time, "HireYear", "Ethnicity")
  
  expect_true(all(trends$diversity_index >= 0))
  expect_true(all(trends$diversity_index <= 1))
  expect_true(all(trends$total_count > 0))
  expect_equal(nrow(trends), length(unique(test_data_time$HireYear)))
})

# =============================================================================
# 2. GEOGRAPHIC DATA PROCESSING
# =============================================================================

test_that("Geographic Distribution Analysis", {
  
  analyze_geographic_distribution <- function(data, geo_col) {
    geo_summary <- data %>%
      count(!!sym(geo_col), name = "employee_count") %>%
      mutate(
        percentage = employee_count / sum(employee_count) * 100,
        rank = rank(-employee_count, ties.method = "min")
      ) %>%
      arrange(desc(employee_count))
    
    list(
      distribution = geo_summary,
      total_locations = nrow(geo_summary),
      concentration_index = max(geo_summary$percentage) / 100,
      top_3_locations = head(geo_summary$State, 3)
    )
  }
  
  geo_analysis <- analyze_geographic_distribution(test_data, "State")
  
  # Test structure
  expect_true(is.list(geo_analysis))
  expect_true(all(c("distribution", "total_locations", "concentration_index", "top_3_locations") %in% names(geo_analysis)))
  
  # Test distribution data
  expect_true(all(geo_analysis$distribution$employee_count > 0))
  expect_equal(sum(geo_analysis$distribution$employee_count), nrow(test_data))
  expect_true(abs(sum(geo_analysis$distribution$percentage) - 100) < 0.001)
  
  # Test concentration index
  expect_true(geo_analysis$concentration_index >= 0)
  expect_true(geo_analysis$concentration_index <= 1)
  
  # Test top locations
  expect_true(length(geo_analysis$top_3_locations) <= 3)
  expect_true(all(geo_analysis$top_3_locations %in% test_data$State))
})

test_that("Geographic Clustering Analysis", {
  
  calculate_geographic_clusters <- function(data, geo_col, threshold = 0.1) {
    geo_dist <- data %>%
      count(!!sym(geo_col)) %>%
      mutate(prop = n / sum(n))
    
    clusters <- list(
      high_concentration = geo_dist %>% filter(prop >= threshold),
      low_concentration = geo_dist %>% filter(prop < threshold),
      herfindahl_index = sum(geo_dist$prop^2)
    )
    
    clusters
  }
  
  clusters <- calculate_geographic_clusters(test_data, "State", 0.1)
  
  expect_true(is.list(clusters))
  expect_true(all(c("high_concentration", "low_concentration", "herfindahl_index") %in% names(clusters)))
  
  # Test Herfindahl Index (measure of concentration)
  expect_true(clusters$herfindahl_index >= 0)
  expect_true(clusters$herfindahl_index <= 1)
  
  # Test cluster completeness
  total_locations <- nrow(clusters$high_concentration) + nrow(clusters$low_concentration)
  expect_equal(total_locations, length(unique(test_data$State)))
})

test_that("Regional Equity Analysis", {
  
  analyze_regional_equity <- function(data, geo_col, metric_col) {
    regional_stats <- data %>%
      group_by(!!sym(geo_col)) %>%
      summarise(
        avg_metric = mean(!!sym(metric_col), na.rm = TRUE),
        median_metric = median(!!sym(metric_col), na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      mutate(
        deviation_from_mean = avg_metric - mean(avg_metric),
        equity_ratio = avg_metric / mean(avg_metric)
      )
    
    list(
      regional_data = regional_stats,
      coefficient_of_variation = sd(regional_stats$avg_metric) / mean(regional_stats$avg_metric),
      equity_range = max(regional_stats$equity_ratio) - min(regional_stats$equity_ratio)
    )
  }
  
  equity_analysis <- analyze_regional_equity(test_data, "State", "Salary")
  
  expect_true(is.list(equity_analysis))
  expect_true(all(c("regional_data", "coefficient_of_variation", "equity_range") %in% names(equity_analysis)))
  
  # Test regional data
  expect_true(all(equity_analysis$regional_data$count > 0))
  expect_true(all(is.finite(equity_analysis$regional_data$avg_metric)))
  expect_true(all(is.finite(equity_analysis$regional_data$equity_ratio)))
  
  # Test coefficient of variation
  expect_true(is.finite(equity_analysis$coefficient_of_variation))
  expect_true(equity_analysis$coefficient_of_variation >= 0)
})

# =============================================================================
# 3. AGE DISTRIBUTION ANALYSIS
# =============================================================================

test_that("Age Distribution Statistics", {
  
  analyze_age_distribution <- function(data, age_col) {
    age_stats <- data %>%
      summarise(
        mean_age = mean(!!sym(age_col), na.rm = TRUE),
        median_age = median(!!sym(age_col), na.rm = TRUE),
        sd_age = sd(!!sym(age_col), na.rm = TRUE),
        min_age = min(!!sym(age_col), na.rm = TRUE),
        max_age = max(!!sym(age_col), na.rm = TRUE),
        q1_age = quantile(!!sym(age_col), 0.25, na.rm = TRUE),
        q3_age = quantile(!!sym(age_col), 0.75, na.rm = TRUE),
        skewness = moments::skewness(!!sym(age_col), na.rm = TRUE),
        kurtosis = moments::kurtosis(!!sym(age_col), na.rm = TRUE)
      )
    
    age_stats
  }
  
  # Mock moments package functions for testing
  if (!require(moments, quietly = TRUE)) {
    skewness <- function(x, na.rm = TRUE) {
      if (na.rm) x <- x[!is.na(x)]
      n <- length(x)
      mean_x <- mean(x)
      sd_x <- sd(x)
      sum((x - mean_x)^3) / (n * sd_x^3)
    }
    
    kurtosis <- function(x, na.rm = TRUE) {
      if (na.rm) x <- x[!is.na(x)]
      n <- length(x)
      mean_x <- mean(x)
      sd_x <- sd(x)
      sum((x - mean_x)^4) / (n * sd_x^4)
    }
  }
  
  age_analysis <- analyze_age_distribution(test_data, "Age")
  
  # Test basic statistics
  expect_true(age_analysis$mean_age >= age_analysis$min_age)
  expect_true(age_analysis$mean_age <= age_analysis$max_age)
  expect_true(age_analysis$median_age >= age_analysis$min_age)
  expect_true(age_analysis$median_age <= age_analysis$max_age)
  expect_true(age_analysis$sd_age >= 0)
  expect_true(age_analysis$q1_age <= age_analysis$median_age)
  expect_true(age_analysis$q3_age >= age_analysis$median_age)
  
  # Test data types
  expect_type(age_analysis$mean_age, "double")
  expect_type(age_analysis$median_age, "double")
  expect_type(age_analysis$sd_age, "double")
})

test_that("Age Cohort Analysis", {
  
  create_age_cohorts <- function(data, age_col) {
    data %>%
      mutate(
        age_cohort = case_when(
          !!sym(age_col) < 25 ~ "Gen Z (Under 25)",
          !!sym(age_col) >= 25 & !!sym(age_col) < 35 ~ "Millennial (25-34)",
          !!sym(age_col) >= 35 & !!sym(age_col) < 45 ~ "Gen X (35-44)",
          !!sym(age_col) >= 45 & !!sym(age_col) < 55 ~ "Baby Boomer (45-54)",
          !!sym(age_col) >= 55 ~ "Senior (55+)",
          TRUE ~ "Unknown"
        )
      ) %>%
      count(age_cohort) %>%
      mutate(
        percentage = n / sum(n) * 100,
        age_cohort = factor(age_cohort, levels = c(
          "Gen Z (Under 25)", "Millennial (25-34)", "Gen X (35-44)", 
          "Baby Boomer (45-54)", "Senior (55+)", "Unknown"
        ))
      ) %>%
      arrange(age_cohort)
  }
  
  cohorts <- create_age_cohorts(test_data, "Age")
  
  expect_true(is.data.frame(cohorts))
  expect_true(all(c("age_cohort", "n", "percentage") %in% names(cohorts)))
  expect_equal(sum(cohorts$n), nrow(test_data))
  expect_true(abs(sum(cohorts$percentage) - 100) < 0.001)
  expect_true(all(cohorts$n > 0))
  expect_true(all(cohorts$percentage > 0))
  expect_true(all(cohorts$percentage <= 100))
})

test_that("Age Distribution Normality Tests", {
  
  test_age_normality <- function(data, age_col) {
    age_vector <- data %>% pull(!!sym(age_col))
    
    # Shapiro-Wilk test (for n <= 5000)
    if (length(age_vector) <= 5000) {
      shapiro_result <- shapiro.test(age_vector)
      shapiro_p <- shapiro_result$p.value
    } else {
      shapiro_p <- NA
    }
    
    # Kolmogorov-Smirnov test
    ks_result <- ks.test(age_vector, "pnorm", mean(age_vector), sd(age_vector))
    
    # Jarque-Bera test alternative (using skewness and kurtosis)
    n <- length(age_vector)
    skew <- skewness(age_vector)
    kurt <- kurtosis(age_vector)
    jb_stat <- n * (skew^2/6 + (kurt - 3)^2/24)
    jb_p <- pchisq(jb_stat, df = 2, lower.tail = FALSE)
    
    list(
      shapiro_p = shapiro_p,
      ks_p = ks_result$p.value,
      jb_p = jb_p,
      is_normal = all(c(shapiro_p, ks_result$p.value, jb_p) > 0.05, na.rm = TRUE)
    )
  }
  
  normality_tests <- test_age_normality(test_data, "Age")
  
  expect_true(is.list(normality_tests))
  expect_true(all(c("shapiro_p", "ks_p", "jb_p", "is_normal") %in% names(normality_tests)))
  
  # Test p-values are valid probabilities
  if (!is.na(normality_tests$shapiro_p)) {
    expect_true(normality_tests$shapiro_p >= 0 & normality_tests$shapiro_p <= 1)
  }
  expect_true(normality_tests$ks_p >= 0 & normality_tests$ks_p <= 1)
  expect_true(normality_tests$jb_p >= 0 & normality_tests$jb_p <= 1)
  expect_type(normality_tests$is_normal, "logical")
})

# =============================================================================
# 4. CROSS-DEMOGRAPHIC CORRELATIONS
# =============================================================================

test_that("Demographic Correlation Analysis", {
  
  calculate_demographic_correlations <- function(data) {
    # Convert categorical to numeric for correlation analysis
    numeric_data <- data %>%
      mutate(
        gender_numeric = as.numeric(as.factor(Gender)),
        ethnicity_numeric = as.numeric(as.factor(Ethnicity)),
        department_numeric = as.numeric(as.factor(Department)),
        attrition_numeric = ifelse(Attrition == "Yes", 1, 0)
      )
    
    # Calculate correlation matrix
    cor_vars <- c("Age", "gender_numeric", "ethnicity_numeric", 
                  "department_numeric", "Salary", "attrition_numeric", "YearsAtCompany")
    
    cor_matrix <- cor(numeric_data[cor_vars], use = "complete.obs")
    
    # Convert to long format for analysis
    cor_long <- cor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
      filter(var1 != var2) %>%
      arrange(desc(abs(correlation)))
    
    list(
      correlation_matrix = cor_matrix,
      correlation_pairs = cor_long,
      strong_correlations = cor_long %>% filter(abs(correlation) > 0.3)
    )
  }
  
  # Mock rownames_to_column if not available
  if (!exists("rownames_to_column")) {
    rownames_to_column <- function(df, var = "rowname") {
      df[[var]] <- rownames(df)
      df[c(var, setdiff(names(df), var))]
    }
  }
  
  correlations <- calculate_demographic_correlations(test_data)
  
  expect_true(is.list(correlations))
  expect_true(all(c("correlation_matrix", "correlation_pairs", "strong_correlations") %in% names(correlations)))
  
  # Test correlation matrix
  expect_true(is.matrix(correlations$correlation_matrix))
  expect_true(all(diag(correlations$correlation_matrix) == 1))
  expect_true(all(correlations$correlation_matrix >= -1 & correlations$correlation_matrix <= 1))
  
  # Test correlation pairs
  expect_true(all(correlations$correlation_pairs$correlation >= -1))
  expect_true(all(correlations$correlation_pairs$correlation <= 1))
  expect_true(all(is.finite(correlations$correlation_pairs$correlation)))
})

test_that("Contingency Table Analysis", {
  
  analyze_contingency_relationships <- function(data, var1, var2) {
    # Create contingency table
    cont_table <- table(data[[var1]], data[[var2]])
    
    # Chi-square test
    chi_test <- chisq.test(cont_table)
    
    # Cramér's V (effect size)
    n <- sum(cont_table)
    cramers_v <- sqrt(chi_test$statistic / (n * (min(dim(cont_table)) - 1)))
    
    # Expected frequencies
    expected_freq <- chi_test$expected
    
    list(
      contingency_table = cont_table,
      chi_square_stat = chi_test$statistic,
      chi_square_p = chi_test$p.value,
      cramers_v = as.numeric(cramers_v),
      expected_frequencies = expected_freq,
      is_significant = chi_test$p.value < 0.05
    )
  }
  
  relationship <- analyze_contingency_relationships(test_data, "Gender", "Department")
  
  expect_true(is.list(relationship))
  expect_true(all(c("contingency_table", "chi_square_stat", "chi_square_p", 
                   "cramers_v", "expected_frequencies", "is_significant") %in% names(relationship)))
  
  # Test chi-square results
  expect_true(relationship$chi_square_stat >= 0)
  expect_true(relationship$chi_square_p >= 0 & relationship$chi_square_p <= 1)
  expect_true(relationship$cramers_v >= 0 & relationship$cramers_v <= 1)
  expect_type(relationship$is_significant, "logical")
  
  # Test table dimensions
  expect_true(sum(relationship$contingency_table) == nrow(test_data))
  expect_true(all(relationship$expected_frequencies > 0))
})

test_that("Interaction Effects Analysis", {
  
  analyze_interaction_effects <- function(data, outcome_var, factor1, factor2) {
    # Create interaction model
    formula_str <- paste(outcome_var, "~", factor1, "*", factor2)
    model <- lm(as.formula(formula_str), data = data)
    
    # ANOVA to test interaction
    anova_result <- anova(model)
    
    # Extract interaction p-value
    interaction_term <- paste(factor1, factor2, sep = ":")
    interaction_p <- anova_result$`Pr(>F)`[grep(interaction_term, rownames(anova_result))]
    
    list(
      model = model,
      anova_table = anova_result,
      interaction_p_value = interaction_p,
      is_significant_interaction = interaction_p < 0.05,
      r_squared = summary(model)$r.squared
    )
  }
  
  interaction_analysis <- analyze_interaction_effects(test_data, "Salary", "Gender", "Department")
  
  expect_true(is.list(interaction_analysis))
  expect_true(all(c("model", "anova_table", "interaction_p_value", 
                   "is_significant_interaction", "r_squared") %in% names(interaction_analysis)))
  
  # Test model results
  expect_s3_class(interaction_analysis$model, "lm")
  expect_true(interaction_analysis$interaction_p_value >= 0)
  expect_true(interaction_analysis$interaction_p_value <= 1)
  expect_true(interaction_analysis$r_squared >= 0)
  expect_true(interaction_analysis$r_squared <= 1)
  expect_type(interaction_analysis$is_significant_interaction, "logical")
})

# =============================================================================
# 5. PRIVACY-COMPLIANT AGGREGATIONS
# =============================================================================

test_that("Minimum Cell Size Enforcement", {
  
  enforce_minimum_cell_size <- function(data, group_vars, min_size = 5) {
    grouped_data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(count = n(), .groups = "drop")
    
    # Identify cells below minimum size
    below_min <- grouped_data %>%
      filter(count < min_size)
    
    # Suppress small cells
    suppressed_data <- grouped_data %>%
      mutate(
        count_suppressed = ifelse(count < min_size, NA, count),
        suppression_flag = count < min_size
      )
    
    list(
      original_data = grouped_data,
      suppressed_data = suppressed_data,
      suppressed_count = nrow(below_min),
      suppression_rate = nrow(below_min) / nrow(grouped_data)
    )
  }
  
  suppression_result <- enforce_minimum_cell_size(test_data, c("Gender", "Department"), 5)
  
  expect_true(is.list(suppression_result))
  expect_true(all(c("original_data", "suppressed_data", "suppressed_count", 
                   "suppression_rate") %in% names(suppression_result)))
  
  # Test suppression logic
  expect_true(all(suppression_result$suppressed_data$count >= 5 | 
                 is.na(suppression_result$suppressed_data$count_suppressed)))
  expect_true(suppression_result$suppression_rate >= 0)
  expect_true(suppression_result$suppression_rate <= 1)
  expect_true(suppression_result$suppressed_count >= 0)
})

test_that("Differential Privacy Implementation", {
  
  add_laplace_noise <- function(data, epsilon = 1.0, sensitivity = 1.0) {
    # Simple Laplace mechanism implementation
    scale <- sensitivity / epsilon
    
    if (is.numeric(data)) {
      noise <- rlaplace(length(data), 0, scale)
      return(data + noise)
    } else {
      # For categorical data, return counts with noise
      counts <- table(data)
      noisy_counts <- counts + rlaplace(length(counts), 0, scale)
      return(pmax(noisy_counts, 0))  # Ensure non-negative counts
    }
  }
  
  # Mock rlaplace function
  rlaplace <- function(n, location = 0, scale = 1) {
    u <- runif(n, -0.5, 0.5)
    location - scale * sign(u) * log(1 - 2 * abs(u))
  }
  
  # Test with numeric data
  original_salary <- test_data$Salary[1:100]
  noisy_salary <- add_laplace_noise(original_salary, epsilon = 1.0)
  
  expect_equal(length(noisy_salary), length(original_salary))
  expect_true(is.numeric(noisy_salary))
  expect_true(all(is.finite(noisy_salary)))
  
  # Test with categorical data
  noisy_gender_counts <- add_laplace_noise(test_data$Gender)
  
  expect_true(is.numeric(noisy_gender_counts))
  expect_true(all(noisy_gender_counts >= 0))
  expect_equal(length(noisy_gender_counts), length(unique(test_data$Gender)))
})

test_that("K-Anonymity Verification", {
  
  verify_k_anonymity <- function(data, quasi_identifiers, k = 5) {
    # Group by quasi-identifiers
    grouped <- data %>%
      group_by(across(all_of(quasi_identifiers))) %>%
      summarise(group_size = n(), .groups = "drop")
    
    # Check k-anonymity
    violations <- grouped %>%
      filter(group_size < k)
    
    list(
      total_groups = nrow(grouped),
      violation_count = nrow(violations),
      k_anonymous = nrow(violations) == 0,
      min_group_size = min(grouped$group_size),
      avg_group_size = mean(grouped$group_size),
      violation_rate = nrow(violations) / nrow(grouped)
    )
  }
  
  # Test with age groups and department
  test_data_age_groups <- test_data %>%
    mutate(age_group = cut(Age, breaks = c(0, 30, 40, 50, 100), 
                          labels = c("Under 30", "30-39", "40-49", "50+")))
  
  k_anon_result <- verify_k_anonymity(test_data_age_groups, c("age_group", "Department"), k = 5)
  
  expect_true(is.list(k_anon_result))
  expect_true(all(c("total_groups", "violation_count", "k_anonymous", 
                   "min_group_size", "avg_group_size", "violation_rate") %in% names(k_anon_result)))
  
  # Test results
  expect_true(k_anon_result$total_groups > 0)
  expect_true(k_anon_result$violation_count >= 0)
  expect_type(k_anon_result$k_anonymous, "logical")
  expect_true(k_anon_result$min_group_size > 0)
  expect_true(k_anon_result$avg_group_size > 0)
  expect_true(k_anon_result$violation_rate >= 0)
  expect_true(k_anon_result$violation_rate <= 1)
})

# =============================================================================
# 6. ANONYMIZATION EFFECTIVENESS
# =============================================================================

test_that("Anonymization Risk Assessment", {
  
  assess_reidentification_risk <- function(data, quasi_identifiers) {
    # Calculate uniqueness
    grouped <- data %>%
      group_by(across(all_of(quasi_identifiers))) %>%
      summarise(count = n(), .groups = "drop")
    
    unique_combinations <- sum(grouped$count == 1)
    rare_combinations <- sum(groupe