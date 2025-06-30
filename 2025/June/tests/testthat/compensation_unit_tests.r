# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPENSATION MODULE UNIT TESTS
# =============================================================================
# Author: akhapwoyaco
# Purpose: Comprehensive unit tests for compensation analysis functions
# Coverage: Pay equity, salary bands, benefits, COLA, market comparison,
#          bonus calculations, stock options, and tax implications
# =============================================================================

library(testthat)
library(tidyverse)
library(lubridate)

# Test Data Setup
setup_test_data <- function() {
  # Employee test data
  employees <- tibble(
    EmployeeID = 1:20,
    Gender = rep(c("Male", "Female"), 10),
    Ethnicity = rep(c("White", "Hispanic", "Black", "Asian"), 5),
    Department = rep(c("Engineering", "Sales", "HR", "Marketing"), 5),
    JobRole = rep(c("Senior", "Junior", "Manager", "Director"), 5),
    State = rep(c("CA", "TX", "NY", "FL"), 5),
    Age = sample(25:60, 20),
    YearsAtCompany = sample(1:15, 20),
    Salary = c(120000, 118000, 125000, 123000, 110000, 108000, 115000, 113000,
               95000, 93000, 98000, 96000, 85000, 83000, 88000, 86000,
               75000, 73000, 78000, 76000),
    StockOptionLevel = sample(0:4, 20, replace = TRUE),
    HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-01-01"), by = "day"), 20)
  )
  
  # Performance data
  performance <- tibble(
    EmployeeID = 1:20,
    ManagerRating = sample(1:5, 20, replace = TRUE, prob = c(0.05, 0.15, 0.6, 0.15, 0.05)),
    SelfRating = sample(1:5, 20, replace = TRUE, prob = c(0.05, 0.15, 0.6, 0.15, 0.05))
  )
  
  # Market data
  market_data <- tibble(
    JobRole = c("Senior", "Junior", "Manager", "Director"),
    Department = rep(c("Engineering", "Sales", "HR", "Marketing"), each = 4),
    MarketSalary_P50 = c(115000, 90000, 105000, 140000),
    MarketSalary_P25 = c(105000, 80000, 95000, 125000),
    MarketSalary_P75 = c(125000, 100000, 115000, 155000)
  )
  
  # COLA data
  cola_data <- tibble(
    State = c("CA", "TX", "NY", "FL"),
    COLA_Index = c(1.25, 0.95, 1.20, 1.05),
    BaseLine_State = "TX"
  )
  
  list(
    employees = employees,
    performance = performance,
    market_data = market_data,
    cola_data = cola_data
  )
}

# =============================================================================
# PAY EQUITY CALCULATIONS TESTS
# =============================================================================

test_that("Pay equity calculations - Gender pay gap", {
  test_data <- setup_test_data()
  
  # Mock function for gender pay gap calculation
  calculate_gender_pay_gap <- function(data) {
    gender_stats <- data %>%
      group_by(Gender, Department, JobRole) %>%
      summarise(
        avg_salary = mean(Salary, na.rm = TRUE),
        median_salary = median(Salary, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = Gender,
        values_from = c(avg_salary, median_salary, count),
        names_sep = "_"
      ) %>%
      mutate(
        pay_gap_avg = round((avg_salary_Male - avg_salary_Female) / avg_salary_Male * 100, 2),
        pay_gap_median = round((median_salary_Male - median_salary_Female) / median_salary_Male * 100, 2),
        statistical_significance = case_when(
          count_Male >= 5 & count_Female >= 5 ~ "Significant",
          TRUE ~ "Insufficient Sample"
        )
      )
    
    return(gender_stats)
  }
  
  result <- calculate_gender_pay_gap(test_data$employees)
  
  # Test structure
  expect_true(all(c("Department", "JobRole", "pay_gap_avg", "pay_gap_median") %in% names(result)))
  expect_true(all(!is.na(result$pay_gap_avg)))
  expect_true(all(is.numeric(result$pay_gap_avg)))
  
  # Test ranges
  expect_true(all(result$pay_gap_avg >= -100 & result$pay_gap_avg <= 100))
  expect_true(all(result$pay_gap_median >= -100 & result$pay_gap_median <= 100))
  
  # Test statistical significance logic
  expect_true(all(result$statistical_significance %in% c("Significant", "Insufficient Sample")))
})

test_that("Pay equity calculations - Ethnicity analysis", {
  test_data <- setup_test_data()
  
  calculate_ethnicity_equity <- function(data) {
    baseline_ethnicity <- "White"
    
    ethnicity_analysis <- data %>%
      group_by(Ethnicity, Department) %>%
      summarise(
        avg_salary = mean(Salary, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      group_by(Department) %>%
      mutate(
        baseline_salary = avg_salary[Ethnicity == baseline_ethnicity],
        equity_ratio = round(avg_salary / baseline_salary, 3),
        equity_gap_pct = round((1 - equity_ratio) * 100, 2)
      ) %>%
      ungroup()
    
    return(ethnicity_analysis)
  }
  
  result <- calculate_ethnicity_equity(test_data$employees)
  
  # Test calculations
  expect_true(all(result$equity_ratio > 0))
  expect_true(all(!is.na(result$equity_ratio)))
  expect_true(all(is.numeric(result$equity_gap_pct)))
  
  # Test baseline calculations
  baseline_entries <- result[result$Ethnicity == "White", ]
  expect_true(all(abs(baseline_entries$equity_ratio - 1.0) < 0.001))
  expect_true(all(abs(baseline_entries$equity_gap_pct) < 0.001))
})

# =============================================================================
# SALARY BAND ANALYSIS TESTS
# =============================================================================

test_that("Salary band analysis - Band creation and positioning", {
  test_data <- setup_test_data()
  
  create_salary_bands <- function(data, band_width = 0.15) {
    salary_bands <- data %>%
      group_by(Department, JobRole) %>%
      summarise(
        median_salary = median(Salary, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        band_min = round(median_salary * (1 - band_width), 0),
        band_mid = round(median_salary, 0),
        band_max = round(median_salary * (1 + band_width), 0),
        band_range = band_max - band_min
      )
    
    # Position employees within bands
    employee_positioning <- data %>%
      left_join(salary_bands, by = c("Department", "JobRole")) %>%
      mutate(
        position_in_band = case_when(
          Salary < band_min ~ "Below Band",
          Salary > band_max ~ "Above Band",
          TRUE ~ "Within Band"
        ),
        band_percentile = pmin(pmax((Salary - band_min) / band_range * 100, 0), 100),
        compa_ratio = round(Salary / band_mid, 3)
      )
    
    list(
      bands = salary_bands,
      positioning = employee_positioning
    )
  }
  
  result <- create_salary_bands(test_data$employees)
  
  # Test band structure
  expect_true(all(result$bands$band_max > result$bands$band_min))
  expect_true(all(result$bands$band_mid >= result$bands$band_min))
  expect_true(all(result$bands$band_mid <= result$bands$band_max))
  expect_true(all(result$bands$band_range > 0))
  
  # Test employee positioning
  expect_true(all(result$positioning$position_in_band %in% c("Below Band", "Within Band", "Above Band")))
  expect_true(all(result$positioning$band_percentile >= 0 & result$positioning$band_percentile <= 100))
  expect_true(all(result$positioning$compa_ratio > 0))
  
  # Test compa-ratio calculations
  within_band <- result$positioning[result$positioning$position_in_band == "Within Band", ]
  expect_true(all(within_band$compa_ratio >= 0.85 & within_band$compa_ratio <= 1.15))
})

test_that("Salary band analysis - Range penetration", {
  test_data <- setup_test_data()
  
  calculate_range_penetration <- function(data) {
    data %>%
      group_by(Department, JobRole) %>%
      summarise(
        min_salary = min(Salary),
        max_salary = max(Salary),
        range_spread = max_salary - min_salary,
        avg_salary = mean(Salary),
        penetration_pct = round((avg_salary - min_salary) / range_spread * 100, 2),
        .groups = "drop"
      ) %>%
      mutate(
        penetration_category = case_when(
          penetration_pct < 25 ~ "Low",
          penetration_pct < 75 ~ "Medium",
          TRUE ~ "High"
        )
      )
  }
  
  result <- calculate_range_penetration(test_data$employees)
  
  # Test penetration calculations
  expect_true(all(result$penetration_pct >= 0 & result$penetration_pct <= 100))
  expect_true(all(result$range_spread >= 0))
  expect_true(all(result$penetration_category %in% c("Low", "Medium", "High")))
  
  # Test edge cases
  expect_true(all(!is.na(result$penetration_pct)))
  expect_true(all(is.finite(result$penetration_pct)))
})

# =============================================================================
# BENEFITS VALUATION TESTS
# =============================================================================

test_that("Benefits valuation - Total compensation calculation", {
  calculate_total_compensation <- function(data) {
    # Benefits as percentage of salary by role level
    benefits_rates <- tibble(
      JobRole = c("Junior", "Senior", "Manager", "Director"),
      health_insurance_pct = c(0.08, 0.09, 0.10, 0.12),
      retirement_match_pct = c(0.03, 0.04, 0.05, 0.06),
      pto_value_pct = c(0.08, 0.10, 0.12, 0.15),
      other_benefits_pct = c(0.02, 0.03, 0.04, 0.05)
    )
    
    total_comp <- data %>%
      left_join(benefits_rates, by = "JobRole") %>%
      mutate(
        health_insurance_value = round(Salary * health_insurance_pct, 0),
        retirement_value = round(Salary * retirement_match_pct, 0),
        pto_value = round(Salary * pto_value_pct, 0),
        other_benefits_value = round(Salary * other_benefits_pct, 0),
        total_benefits_value = health_insurance_value + retirement_value + pto_value + other_benefits_value,
        total_compensation = Salary + total_benefits_value,
        benefits_to_salary_ratio = round(total_benefits_value / Salary, 3)
      )
    
    return(total_comp)
  }
  
  result <- calculate_total_compensation(test_data$employees)
  
  # Test benefit calculations
  expect_true(all(result$total_benefits_value > 0))
  expect_true(all(result$total_compensation > result$Salary))
  expect_true(all(result$benefits_to_salary_ratio > 0 & result$benefits_to_salary_ratio < 1))
  
  # Test component calculations
  expect_true(all(result$health_insurance_value > 0))
  expect_true(all(result$retirement_value > 0))
  expect_true(all(result$pto_value > 0))
  expect_true(all(result$other_benefits_value > 0))
  
  # Test benefits progression by role
  director_benefits <- result[result$JobRole == "Director", "benefits_to_salary_ratio"]
  junior_benefits <- result[result$JobRole == "Junior", "benefits_to_salary_ratio"]
  expect_true(mean(director_benefits[[1]]) > mean(junior_benefits[[1]]))
})

# =============================================================================
# COST-OF-LIVING ADJUSTMENTS TESTS
# =============================================================================

test_that("COLA adjustments - Regional adjustment calculations", {
  test_data <- setup_test_data()
  
  apply_cola_adjustments <- function(employee_data, cola_data) {
    adjusted_data <- employee_data %>%
      left_join(cola_data, by = "State") %>%
      mutate(
        adjusted_salary = round(Salary / COLA_Index, 0),
        cola_adjustment = round(Salary - adjusted_salary, 0),
        cola_adjustment_pct = round((COLA_Index - 1) * 100, 2)
      )
    
    return(adjusted_data)
  }
  
  result <- apply_cola_adjustments(test_data$employees, test_data$cola_data)
  
  # Test COLA calculations
  expect_true(all(!is.na(result$adjusted_salary)))
  expect_true(all(result$adjusted_salary > 0))
  expect_true(all(is.finite(result$cola_adjustment)))
  
  # Test high-cost areas (CA, NY)
  high_cost_states <- result[result$State %in% c("CA", "NY"), ]
  expect_true(all(high_cost_states$adjusted_salary < high_cost_states$Salary))
  expect_true(all(high_cost_states$cola_adjustment_pct > 0))
  
  # Test baseline state (TX)
  baseline_state <- result[result$State == "TX", ]
  expect_true(all(abs(baseline_state$cola_adjustment_pct) < 0.1))
})

test_that("COLA adjustments - Cross-state equity analysis", {
  test_data <- setup_test_data()
  
  analyze_cola_equity <- function(employee_data, cola_data) {
    equity_analysis <- employee_data %>%
      left_join(cola_data, by = "State") %>%
      mutate(adjusted_salary = Salary / COLA_Index) %>%
      group_by(JobRole, Department) %>%
      summarise(
        avg_raw_salary = mean(Salary),
        avg_adjusted_salary = mean(adjusted_salary),
        salary_variance_raw = var(Salary),
        salary_variance_adjusted = var(adjusted_salary),
        equity_improvement = round((salary_variance_raw - salary_variance_adjusted) / salary_variance_raw * 100, 2),
        .groups = "drop"
      )
    
    return(equity_analysis)
  }
  
  result <- analyze_cola_equity(test_data$employees, test_data$cola_data)
  
  # Test equity improvements
  expect_true(all(!is.na(result$equity_improvement)))
  expect_true(all(is.finite(result$equity_improvement)))
  expect_true(all(result$salary_variance_adjusted >= 0))
  expect_true(all(result$salary_variance_raw >= 0))
})

# =============================================================================
# MARKET COMPARISON ACCURACY TESTS
# =============================================================================

test_that("Market comparison - Benchmarking accuracy", {
  test_data <- setup_test_data()
  
  compare_to_market <- function(employee_data, market_data) {
    market_comparison <- employee_data %>%
      left_join(market_data, by = c("JobRole", "Department")) %>%
      mutate(
        market_position_p50 = round((Salary / MarketSalary_P50 - 1) * 100, 2),
        market_position_p25 = round((Salary / MarketSalary_P25 - 1) * 100, 2),
        market_position_p75 = round((Salary / MarketSalary_P75 - 1) * 100, 2),
        market_quartile = case_when(
          Salary < MarketSalary_P25 ~ "Below 25th",
          Salary < MarketSalary_P50 ~ "25th-50th",
          Salary < MarketSalary_P75 ~ "50th-75th",
          TRUE ~ "Above 75th"
        ),
        competitive_position = case_when(
          market_position_p50 < -10 ~ "Below Market",
          market_position_p50 > 10 ~ "Above Market",
          TRUE ~ "Market Competitive"
        )
      )
    
    return(market_comparison)
  }
  
  result <- compare_to_market(test_data$employees, test_data$market_data)
  
  # Test market position calculations
  expect_true(all(!is.na(result$market_position_p50)))
  expect_true(all(is.finite(result$market_position_p50)))
  expect_true(all(result$market_quartile %in% c("Below 25th", "25th-50th", "50th-75th", "Above 75th")))
  expect_true(all(result$competitive_position %in% c("Below Market", "Market Competitive", "Above Market")))
  
  # Test logical consistency
  below_25th <- result[result$market_quartile == "Below 25th", ]
  expect_true(all(below_25th$Salary < below_25th$MarketSalary_P25))
  
  above_75th <- result[result$market_quartile == "Above 75th", ]
  expect_true(all(above_75th$Salary >= above_75th$MarketSalary_P75))
})

# =============================================================================
# BONUS CALCULATION LOGIC TESTS
# =============================================================================

test_that("Bonus calculations - Performance-based bonuses", {
  test_data <- setup_test_data()
  
  calculate_performance_bonus <- function(employee_data, performance_data) {
    # Bonus matrix based on performance ratings
    bonus_matrix <- tibble(
      ManagerRating = c(1, 2, 3, 4, 5),
      bonus_pct = c(0, 0.02, 0.05, 0.08, 0.12)
    )
    
    bonus_data <- employee_data %>%
      left_join(performance_data, by = "EmployeeID") %>%
      left_join(bonus_matrix, by = "ManagerRating") %>%
      mutate(
        target_bonus = round(Salary * bonus_pct, 0),
        # Apply tenure multiplier
        tenure_multiplier = pmin(1 + (YearsAtCompany * 0.01), 1.15),
        final_bonus = round(target_bonus * tenure_multiplier, 0),
        bonus_to_salary_ratio = round(final_bonus / Salary, 4)
      )
    
    return(bonus_data)
  }
  
  result <- calculate_performance_bonus(test_data$employees, test_data$performance_data)
  
  # Test bonus calculations
  expect_true(all(result$final_bonus >= 0))
  expect_true(all(result$bonus_to_salary_ratio >= 0))
  expect_true(all(result$tenure_multiplier >= 1 & result$tenure_multiplier <= 1.15))
  
  # Test performance correlation
  high_performers <- result[result$ManagerRating >= 4, ]
  low_performers <- result[result$ManagerRating <= 2, ]
  expect_true(mean(high_performers$bonus_to_salary_ratio) > mean(low_performers$bonus_to_salary_ratio))
  
  # Test edge cases
  zero_bonus <- result[result$ManagerRating == 1, ]
  expect_true(all(zero_bonus$target_bonus == 0))
})

test_that("Bonus calculations - Department and role adjustments", {
  calculate_role_adjusted_bonus <- function(employee_data, performance_data) {
    # Role-based bonus targets
    role_multipliers <- tibble(
      JobRole = c("Junior", "Senior", "Manager", "Director"),
      role_bonus_multiplier = c(0.8, 1.0, 1.3, 1.6)
    )
    
    # Department performance multipliers
    dept_multipliers <- tibble(
      Department = c("Engineering", "Sales", "HR", "Marketing"),
      dept_performance_multiplier = c(1.1, 1.2, 0.9, 1.0)
    )
    
    adjusted_bonus <- employee_data %>%
      left_join(performance_data, by = "EmployeeID") %>%
      left_join(role_multipliers, by = "JobRole") %>%
      left_join(dept_multipliers, by = "Department") %>%
      mutate(
        base_bonus_pct = case_when(
          ManagerRating == 5 ~ 0.12,
          ManagerRating == 4 ~ 0.08,
          ManagerRating == 3 ~ 0.05,
          ManagerRating == 2 ~ 0.02,
          TRUE ~ 0
        ),
        adjusted_bonus_pct = base_bonus_pct * role_bonus_multiplier * dept_performance_multiplier,
        final_bonus = round(Salary * adjusted_bonus_pct, 0)
      )
    
    return(adjusted_bonus)
  }
  
  result <- calculate_role_adjusted_bonus(test_data$employees, test_data$performance_data)
  
  # Test role adjustments
  directors <- result[result$JobRole == "Director", ]
  juniors <- result[result$JobRole == "Junior", ]
  expect_true(mean(directors$adjusted_bonus_pct) > mean(juniors$adjusted_bonus_pct))
  
  # Test department adjustments
  sales <- result[result$Department == "Sales", ]
  hr <- result[result$Department == "HR", ]
  expect_true(mean(sales$adjusted_bonus_pct) > mean(hr$adjusted_bonus_pct))
})

# =============================================================================
# STOCK OPTION VALUATIONS TESTS
# =============================================================================

test_that("Stock option valuations - Grant value calculations", {
  calculate_stock_option_value <- function(employee_data) {
    # Stock option parameters
    current_stock_price <- 85.50
    strike_price <- 75.00
    vesting_years <- 4
    annual_volatility <- 0.25
    risk_free_rate <- 0.03
    
    # Option value by level
    option_grants <- tibble(
      StockOptionLevel = 0:4,
      shares_granted = c(0, 500, 1000, 2000, 4000)
    )
    
    stock_valuations <- employee_data %>%
      left_join(option_grants, by = "StockOptionLevel") %>%
      mutate(
        # Black-Scholes approximation for simplicity
        intrinsic_value = pmax(current_stock_price - strike_price, 0),
        time_value = pmax(strike_price * annual_volatility * sqrt(vesting_years), 0),
        option_value_per_share = intrinsic_value + time_value,
        total_option_value = round(shares_granted * option_value_per_share, 0),
        vested_shares = case_when(
          YearsAtCompany >= vesting_years ~ shares_granted,
          YearsAtCompany >= 3 ~ shares_granted * 0.75,
          YearsAtCompany >= 2 ~ shares_granted * 0.50,
          YearsAtCompany >= 1 ~ shares_granted * 0.25,
          TRUE ~ 0
        ),
        vested_value = round(vested_shares * option_value_per_share, 0)
      )
    
    return(stock_valuations)
  }
  
  result <- calculate_stock_option_value(test_data$employees)
  
  # Test option value calculations
  expect_true(all(result$option_value_per_share >= 0))
  expect_true(all(result$total_option_value >= 0))
  expect_true(all(result$vested_value >= 0))
  expect_true(all(result$vested_shares <= result$shares_granted))
  
  # Test vesting logic
  fully_vested <- result[result$YearsAtCompany >= 4, ]
  expect_true(all(fully_vested$vested_shares == fully_vested$shares_granted))
  
  new_employees <- result[result$YearsAtCompany < 1, ]
  expect_true(all(new_employees$vested_shares == 0))
  
  # Test option level progression
  level_4 <- result[result$StockOptionLevel == 4, ]
  level_0 <- result[result$StockOptionLevel == 0, ]
  expect_true(all(level_4$total_option_value > level_0$total_option_value))
})

# =============================================================================
# TAX IMPLICATIONS ACCURACY TESTS
# =============================================================================

test_that("Tax implications - Federal tax calculations", {
  calculate_tax_implications <- function(employee_data) {
    # 2024 Federal tax brackets (simplified)
    calculate_federal_tax <- function(income) {
      case_when(
        income <= 11000 ~ income * 0.10,
        income <= 44725 ~ 1100 + (income - 11000) * 0.12,
        income <= 95375 ~ 5147 + (income - 44725) * 0.22,
        income <= 182050 ~ 16290 + (income - 95375) * 0.24,
        income <= 231250 ~ 37104 + (income - 182050) * 0.32,
        income <= 578125 ~ 52832 + (income - 231250) * 0.35,
        TRUE ~ 174238.25 + (income - 578125) * 0.37
      )
    }
    
    # State tax rates (simplified)
    state_tax_rates <- tibble(
      State = c("CA", "TX", "NY", "FL"),
      state_tax_rate = c(0.093, 0.0, 0.0685, 0.0)
    )
    
    tax_calculations <- employee_data %>%
      left_join(state_tax_rates, by = "State") %>%
      mutate(
        # Calculate taxes on salary
        federal_tax = round(map_dbl(Salary, calculate_federal_tax), 0),
        state_tax = round(Salary * state_tax_rate, 0),
        social_security_tax = round(pmin(Salary * 0.062, 160200 * 0.062), 0),
        medicare_tax = round(Salary * 0.0145, 0),
        total_tax = federal_tax + state_tax + social_security_tax + medicare_tax,
        effective_tax_rate = round(total_tax / Salary, 4),
        net_salary = Salary - total_tax,
        # Marginal tax rates
        marginal_federal_rate = case_when(
          Salary <= 11000 ~ 0.10,
          Salary <= 44725 ~ 0.12,
          Salary <= 95375 ~ 0.22,
          Salary <= 182050 ~ 0.24,
          Salary <= 231250 ~ 0.32,
          Salary <= 578125 ~ 0.35,
          TRUE ~ 0.37
        ),
        marginal_total_rate = marginal_federal_rate + state_tax_rate + 0.062 + 0.0145
      )
    
    return(tax_calculations)
  }
  
  result <- calculate_tax_implications(test_data$employees)
  
  # Test tax calculations
  expect_true(all(result$federal_tax >= 0))
  expect_true(all(result$total_tax >= 0))
  expect_true(all(result$effective_tax_rate >= 0 & result$effective_tax_rate <= 1))
  expect_true(all(result$net_salary > 0))
  expect_true(all(result$net_salary < result$Salary))
  
  # Test progressive taxation
  high_earners <- result[result$Salary > 100000, ]
  low_earners <- result[result$Salary < 80000, ]
  expect_true(mean(high_earners$effective_tax_rate) > mean(low_earners$effective_tax_rate))
  
  # Test state tax differences
  ca_employees <- result[result$State == "CA", ]
  tx_employees <- result[result$State == "TX", ]
  expect_true(mean(ca_employees$effective_tax_rate) > mean(tx_employees$effective_tax_rate))
  
  # Test Social Security cap
  high_salary <- result[result$Salary > 160200, ]
  if (nrow(high_salary) > 0) {
    expect_true(all(high_salary$social_security_tax <= 160200 * 0.062 + 1))
  }
})

test_that("Tax implications - Stock option tax calculations", {
  calculate_option_tax_impact <- function(employee_data, stock_data) {
    # Assume stock options are ISOs vs NQSOs
    option_tax_calc <- employee_data %>%
      left_join(stock_data, by = "EmployeeID") %>%
      mutate(
        # ISO vs NQSO determination (simplified)
        option_type = if_else(StockOptionLevel >= 2, "ISO", "NQSO"),
        
        # Exercise scenarios
        exercise_gain = pmax(current_price - strike_price, 0) * shares_exercised,
        
        # Tax treatment
        nqso_ordinary_income = if_else(option_type == "NQSO", exercise_gain, 0),
        iso_amt_adjustment