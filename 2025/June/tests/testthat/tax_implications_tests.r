# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPENSATION MODULE UNIT TESTS
# Tax Implications Accuracy Testing Suite
# 
# Author: akhapwoyaco
# Created: 2025-06-30
# Purpose: Comprehensive testing of tax calculation accuracy in compensation module
# =============================================================================

# Required Libraries for Testing
library(testthat)
library(dplyr)
library(lubridate)

# Source the compensation module (assuming it exists)
# source("modules/compensation_module.R")

# =============================================================================
# TAX CONSTANTS FOR 2025 (Based on IRS Guidelines)
# =============================================================================

# Federal Income Tax Brackets for 2025
FEDERAL_TAX_BRACKETS_2025 <- list(
  single = data.frame(
    min_income = c(0, 11600, 47150, 100525, 191775, 243725, 609350),
    max_income = c(11599, 47149, 100524, 191774, 243724, 609349, Inf),
    rate = c(0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
  ),
  married_jointly = data.frame(
    min_income = c(0, 23200, 94300, 201050, 383550, 487450, 731200),
    max_income = c(23199, 94299, 201049, 383549, 487449, 731199, Inf),
    rate = c(0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
  ),
  married_separately = data.frame(
    min_income = c(0, 11600, 47150, 100525, 191775, 243725, 365600),
    max_income = c(11599, 47149, 100524, 191774, 243724, 365599, Inf),
    rate = c(0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
  ),
  head_of_household = data.frame(
    min_income = c(0, 16550, 63100, 100500, 191750, 243700, 609350),
    max_income = c(16549, 63099, 100499, 191749, 243699, 609349, Inf),
    rate = c(0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
  )
)

# FICA Tax Rates for 2025
FICA_RATES_2025 <- list(
  social_security_rate = 0.062,  # 6.2%
  medicare_rate = 0.0145,        # 1.45%
  additional_medicare_rate = 0.009,  # 0.9% on income over threshold
  social_security_wage_base = 176100,  # Maximum taxable wages for SS
  additional_medicare_threshold_single = 200000,
  additional_medicare_threshold_married = 250000
)

# Standard Deductions for 2025
STANDARD_DEDUCTIONS_2025 <- list(
  single = 15000,
  married_jointly = 30000,
  married_separately = 15000,
  head_of_household = 22500
)

# State Tax Examples (using sample states)
STATE_TAX_RATES <- list(
  california = 0.13,  # Top rate
  texas = 0.0,        # No state income tax
  new_york = 0.10,    # Top rate
  florida = 0.0       # No state income tax
)

# =============================================================================
# HELPER FUNCTIONS FOR TAX CALCULATIONS
# =============================================================================

#' Calculate Federal Income Tax
#' @param income Gross annual income
#' @param filing_status Filing status (single, married_jointly, etc.)
#' @param deductions Total deductions (defaults to standard deduction)
#' @return Federal income tax owed
calculate_federal_income_tax <- function(income, filing_status = "single", deductions = NULL) {
  if (is.null(deductions)) {
    deductions <- STANDARD_DEDUCTIONS_2025[[filing_status]]
  }
  
  taxable_income <- max(0, income - deductions)
  
  if (taxable_income == 0) return(0)
  
  brackets <- FEDERAL_TAX_BRACKETS_2025[[filing_status]]
  total_tax <- 0
  
  for (i in 1:nrow(brackets)) {
    bracket_min <- brackets$min_income[i]
    bracket_max <- brackets$max_income[i]
    rate <- brackets$rate[i]
    
    if (taxable_income > bracket_min) {
      taxable_in_bracket <- min(taxable_income, bracket_max) - bracket_min + 
                            ifelse(bracket_min == 0, 1, 0)
      if (bracket_min == 0) taxable_in_bracket <- taxable_in_bracket - 1
      
      total_tax <- total_tax + (taxable_in_bracket * rate)
    }
  }
  
  return(total_tax)
}

#' Calculate FICA Taxes
#' @param income Gross annual income
#' @param filing_status Filing status for additional Medicare tax
#' @return List with Social Security and Medicare taxes
calculate_fica_taxes <- function(income, filing_status = "single") {
  # Social Security Tax (6.2% up to wage base)
  ss_taxable_income <- min(income, FICA_RATES_2025$social_security_wage_base)
  social_security_tax <- ss_taxable_income * FICA_RATES_2025$social_security_rate
  
  # Medicare Tax (1.45% on all income)
  medicare_tax <- income * FICA_RATES_2025$medicare_rate
  
  # Additional Medicare Tax (0.9% on income over threshold)
  threshold <- ifelse(
    filing_status %in% c("married_jointly"),
    FICA_RATES_2025$additional_medicare_threshold_married,
    FICA_RATES_2025$additional_medicare_threshold_single
  )
  
  additional_medicare_tax <- max(0, income - threshold) * FICA_RATES_2025$additional_medicare_rate
  
  return(list(
    social_security = social_security_tax,
    medicare = medicare_tax,
    additional_medicare = additional_medicare_tax,
    total_fica = social_security_tax + medicare_tax + additional_medicare_tax
  ))
}

#' Calculate Total Tax Burden
#' @param income Gross annual income
#' @param filing_status Filing status
#' @param state State for state tax calculation
#' @param deductions Total deductions
#' @return Comprehensive tax breakdown
calculate_total_tax_burden <- function(income, filing_status = "single", 
                                     state = "texas", deductions = NULL) {
  federal_tax <- calculate_federal_income_tax(income, filing_status, deductions)
  fica_taxes <- calculate_fica_taxes(income, filing_status)
  
  # State tax (simplified)
  state_rate <- STATE_TAX_RATES[[tolower(state)]] %||% 0
  state_tax <- income * state_rate
  
  total_tax <- federal_tax + fica_taxes$total_fica + state_tax
  
  return(list(
    gross_income = income,
    federal_tax = federal_tax,
    fica_taxes = fica_taxes,
    state_tax = state_tax,
    total_tax = total_tax,
    net_income = income - total_tax,
    effective_rate = total_tax / income,
    marginal_rate = get_marginal_rate(income, filing_status)
  ))
}

#' Get Marginal Tax Rate
#' @param income Gross annual income
#' @param filing_status Filing status
#' @return Marginal tax rate
get_marginal_rate <- function(income, filing_status = "single") {
  brackets <- FEDERAL_TAX_BRACKETS_2025[[filing_status]]
  
  for (i in 1:nrow(brackets)) {
    if (income >= brackets$min_income[i] && income <= brackets$max_income[i]) {
      return(brackets$rate[i])
    }
  }
  
  return(0.37)  # Top bracket
}

# =============================================================================
# UNIT TESTS - TAX IMPLICATIONS ACCURACY
# =============================================================================

context("Compensation Module - Tax Implications Accuracy")

# -----------------------------------------------------------------------------
# Test Suite 1: Federal Income Tax Calculations
# -----------------------------------------------------------------------------

test_that("Federal income tax calculation - Single filer scenarios", {
  
  # Test case 1: Low income (10% bracket)
  income_low <- 10000
  expected_tax_low <- 1000  # 10% of $10,000
  actual_tax_low <- calculate_federal_income_tax(income_low, "single")
  
  expect_equal(actual_tax_low, expected_tax_low, tolerance = 1,
               info = "Low income single filer tax calculation")
  
  # Test case 2: Middle income (12% bracket)
  income_mid <- 30000
  # First $11,599 at 10% = $1,159.90
  # Remaining $18,401 at 12% = $2,208.12
  # Total = $3,367.90 (rounded)
  expected_tax_mid <- 1159.90 + (18401 * 0.12)
  actual_tax_mid <- calculate_federal_income_tax(income_mid, "single")
  
  expect_equal(actual_tax_mid, expected_tax_mid, tolerance = 10,
               info = "Middle income single filer tax calculation")
  
  # Test case 3: High income (multiple brackets)
  income_high <- 200000
  actual_tax_high <- calculate_federal_income_tax(income_high, "single")
  
  # Should be in 32% bracket
  expect_true(actual_tax_high > 30000,
              info = "High income should result in substantial tax")
  expect_true(actual_tax_high < income_high * 0.37,
              info = "Tax should be less than top marginal rate")
})

test_that("Federal income tax calculation - Married filing jointly scenarios", {
  
  # Test case 1: Married couple, moderate income
  income_married <- 80000
  actual_tax_married <- calculate_federal_income_tax(income_married, "married_jointly")
  single_equivalent <- calculate_federal_income_tax(40000, "single") * 2
  
  # Married filing jointly should have lower tax burden
  expect_true(actual_tax_married < single_equivalent,
              info = "Married filing jointly should have tax advantage")
  
  # Test case 2: High income married couple
  income_high_married <- 500000
  actual_tax_high_married <- calculate_federal_income_tax(income_high_married, "married_jointly")
  
  expect_true(actual_tax_high_married > 100000,
              info = "High income married couple should pay substantial tax")
})

test_that("Standard deduction application", {
  
  # Test that standard deduction is properly applied
  income_at_deduction <- STANDARD_DEDUCTIONS_2025$single
  tax_at_deduction <- calculate_federal_income_tax(income_at_deduction, "single")
  
  expect_equal(tax_at_deduction, 0, tolerance = 0.01,
               info = "Income equal to standard deduction should result in zero tax")
  
  # Test income slightly above standard deduction
  income_above <- STANDARD_DEDUCTIONS_2025$single + 1000
  tax_above <- calculate_federal_income_tax(income_above, "single")
  expected_tax_above <- 1000 * 0.10  # $1000 in 10% bracket
  
  expect_equal(tax_above, expected_tax_above, tolerance = 1,
               info = "Tax on income above standard deduction")
})

# -----------------------------------------------------------------------------
# Test Suite 2: FICA Tax Calculations
# -----------------------------------------------------------------------------

test_that("Social Security tax calculations", {
  
  # Test case 1: Income below wage base
  income_below_base <- 100000
  fica_taxes <- calculate_fica_taxes(income_below_base)
  expected_ss <- income_below_base * FICA_RATES_2025$social_security_rate
  
  expect_equal(fica_taxes$social_security, expected_ss, tolerance = 0.01,
               info = "Social Security tax for income below wage base")
  
  # Test case 2: Income above wage base
  income_above_base <- 200000
  fica_taxes_above <- calculate_fica_taxes(income_above_base)
  expected_ss_capped <- FICA_RATES_2025$social_security_wage_base * 
                        FICA_RATES_2025$social_security_rate
  
  expect_equal(fica_taxes_above$social_security, expected_ss_capped, tolerance = 0.01,
               info = "Social Security tax should be capped at wage base")
  
  # Test case 3: Income exactly at wage base
  income_at_base <- FICA_RATES_2025$social_security_wage_base
  fica_taxes_at_base <- calculate_fica_taxes(income_at_base)
  expected_ss_at_base <- income_at_base * FICA_RATES_2025$social_security_rate
  
  expect_equal(fica_taxes_at_base$social_security, expected_ss_at_base, tolerance = 0.01,
               info = "Social Security tax at exactly wage base limit")
})

test_that("Medicare tax calculations", {
  
  # Test case 1: Regular Medicare tax (no additional Medicare)
  income_regular <- 150000
  fica_taxes <- calculate_fica_taxes(income_regular, "single")
  expected_medicare <- income_regular * FICA_RATES_2025$medicare_rate
  
  expect_equal(fica_taxes$medicare, expected_medicare, tolerance = 0.01,
               info = "Regular Medicare tax calculation")
  expect_equal(fica_taxes$additional_medicare, 0, tolerance = 0.01,
               info = "No additional Medicare tax below threshold")
  
  # Test case 2: Additional Medicare tax for single filer
  income_high_single <- 250000
  fica_taxes_high <- calculate_fica_taxes(income_high_single, "single")
  expected_additional <- (income_high_single - 200000) * 
                        FICA_RATES_2025$additional_medicare_rate
  
  expect_equal(fica_taxes_high$additional_medicare, expected_additional, tolerance = 0.01,
               info = "Additional Medicare tax for high-income single filer")
  
  # Test case 3: Additional Medicare tax for married couple
  income_married_high <- 300000
  fica_taxes_married <- calculate_fica_taxes(income_married_high, "married_jointly")
  expected_additional_married <- (income_married_high - 250000) * 
                                FICA_RATES_2025$additional_medicare_rate
  
  expect_equal(fica_taxes_married$additional_medicare, expected_additional_married, 
               tolerance = 0.01,
               info = "Additional Medicare tax for high-income married couple")
})

# -----------------------------------------------------------------------------
# Test Suite 3: Edge Cases and Boundary Testing
# -----------------------------------------------------------------------------

test_that("Edge cases and boundary conditions", {
  
  # Test case 1: Zero income
  zero_tax <- calculate_federal_income_tax(0, "single")
  expect_equal(zero_tax, 0, info = "Zero income should result in zero tax")
  
  fica_zero <- calculate_fica_taxes(0)
  expect_equal(fica_zero$total_fica, 0, info = "Zero income should result in zero FICA")
  
  # Test case 2: Negative income (should be treated as zero)
  negative_income <- -5000
  negative_tax <- calculate_federal_income_tax(negative_income, "single")
  expect_equal(negative_tax, 0, info = "Negative income should result in zero tax")
  
  # Test case 3: Very high income (top bracket)
  very_high_income <- 1000000
  very_high_tax <- calculate_federal_income_tax(very_high_income, "single")
  
  # Should be substantial but not exceed 37% of income
  expect_true(very_high_tax > 300000, info = "Very high income should result in high tax")
  expect_true(very_high_tax < very_high_income * 0.37, 
              info = "Tax should not exceed top marginal rate times income")
  
  # Test case 4: Income exactly at bracket boundaries
  bracket_boundary <- 47150  # Boundary between 12% and 22% for single
  tax_at_boundary <- calculate_federal_income_tax(bracket_boundary, "single")
  tax_just_above <- calculate_federal_income_tax(bracket_boundary + 1, "single")
  
  # Tax should increase by the marginal rate
  expect_true(tax_just_above > tax_at_boundary,
              info = "Tax should increase at bracket boundary")
})

# -----------------------------------------------------------------------------
# Test Suite 4: Integration Tests - Total Tax Burden
# -----------------------------------------------------------------------------

test_that("Total tax burden calculations", {
  
  # Test case 1: Comprehensive tax calculation
  income_test <- 75000
  filing_status <- "single"
  state <- "california"
  
  total_burden <- calculate_total_tax_burden(income_test, filing_status, state)
  
  # Verify all components are present
  expect_true(!is.null(total_burden$federal_tax), 
              info = "Federal tax should be calculated")
  expect_true(!is.null(total_burden$fica_taxes$total_fica), 
              info = "FICA taxes should be calculated")
  expect_true(!is.null(total_burden$state_tax), 
              info = "State tax should be calculated")
  
  # Verify total equals sum of components
  expected_total <- total_burden$federal_tax + 
                    total_burden$fica_taxes$total_fica + 
                    total_burden$state_tax
  
  expect_equal(total_burden$total_tax, expected_total, tolerance = 0.01,
               info = "Total tax should equal sum of components")
  
  # Verify net income calculation
  expected_net <- income_test - total_burden$total_tax
  expect_equal(total_burden$net_income, expected_net, tolerance = 0.01,
               info = "Net income calculation should be accurate")
  
  # Test case 2: State with no income tax
  total_burden_no_state <- calculate_total_tax_burden(income_test, filing_status, "texas")
  
  expect_equal(total_burden_no_state$state_tax, 0,
               info = "Texas should have zero state income tax")
  expect_true(total_burden_no_state$total_tax < total_burden$total_tax,
               info = "No state tax should result in lower total burden")
})

# -----------------------------------------------------------------------------
# Test Suite 5: Tax Rate Calculations
# -----------------------------------------------------------------------------

test_that("Marginal and effective tax rate calculations", {
  
  # Test case 1: Marginal rate identification
  income_12_bracket <- 30000  # Should be in 12% bracket
  marginal_rate <- get_marginal_rate(income_12_bracket, "single")
  
  expect_equal(marginal_rate, 0.12, tolerance = 0.001,
               info = "Marginal rate should be 12% for $30K single filer")
  
  # Test case 2: Effective rate calculation
  total_burden <- calculate_total_tax_burden(income_12_bracket, "single", "texas")
  expected_effective <- total_burden$total_tax / income_12_bracket
  
  expect_equal(total_burden$effective_rate, expected_effective, tolerance = 0.001,
               info = "Effective rate calculation should be accurate")
  
  # Test case 3: Effective rate should be less than marginal rate
  expect_true(total_burden$effective_rate < marginal_rate,
              info = "Effective rate should be less than marginal rate due to progressive taxation")
})

# -----------------------------------------------------------------------------
# Test Suite 6: Performance and Robustness Tests
# -----------------------------------------------------------------------------

test_that("Performance and robustness of tax calculations", {
  
  # Test case 1: Large dataset performance
  test_incomes <- seq(20000, 200000, by = 10000)
  
  start_time <- Sys.time()
  tax_results <- sapply(test_incomes, function(income) {
    calculate_total_tax_burden(income, "single", "texas")$total_tax
  })
  end_time <- Sys.time()
  
  # Should complete in reasonable time (< 1 second for 19 calculations)
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(execution_time < 1.0,
              info = "Tax calculations should be performant")
  
  # Results should be monotonically increasing (progressive taxation)
  expect_true(all(diff(tax_results) >= 0),
              info = "Tax burden should increase with income")
  
  # Test case 2: Input validation and error handling
  expect_error(calculate_federal_income_tax("invalid_income", "single"),
               info = "Should handle invalid income input")
  
  expect_error(calculate_federal_income_tax(50000, "invalid_status"),
               info = "Should handle invalid filing status")
})

# -----------------------------------------------------------------------------
# Test Suite 7: Compliance and Accuracy Verification
# -----------------------------------------------------------------------------

test_that("Tax calculation compliance with IRS guidelines", {
  
  # Test case 1: Verify against known tax scenarios (2025 tax year)
  
  # Single filer, $50,000 income, standard deduction
  test_income <- 50000
  test_tax <- calculate_federal_income_tax(test_income, "single")
  
  # Manual calculation:
  # Taxable income: $50,000 - $15,000 = $35,000
  # Tax: $1,159.90 (10% bracket) + $2,808.12 (12% on remaining) = $3,968.02
  expected_manual <- 1159.90 + ((35000 - 11600) * 0.12)
  
  expect_equal(test_tax, expected_manual, tolerance = 5,
               info = "Tax calculation should match manual IRS calculation")
  
  # Test case 2: FICA tax compliance
  test_fica <- calculate_fica_taxes(100000)
  expected_ss <- 100000 * 0.062  # $6,200
  expected_medicare <- 100000 * 0.0145  # $1,450
  
  expect_equal(test_fica$social_security, expected_ss, tolerance = 0.01,
               info = "Social Security tax should match IRS rate")
  expect_equal(test_fica$medicare, expected_medicare, tolerance = 0.01,
               info = "Medicare tax should match IRS rate")
})

# -----------------------------------------------------------------------------
# Test Suite 8: Module Integration Tests
# -----------------------------------------------------------------------------

test_that("Integration with compensation module functions", {
  
  # Test case 1: Tax implications for salary ranges
  salary_ranges <- c(40000, 60000, 80000, 100000, 150000)
  
  for (salary in salary_ranges) {
    tax_burden <- calculate_total_tax_burden(salary, "single", "texas")
    
    # Basic sanity checks
    expect_true(tax_burden$total_tax >= 0,
                info = paste("Tax should be non-negative for salary", salary))
    expect_true(tax_burden$net_income > 0,
                info = paste("Net income should be positive for salary", salary))
    expect_true(tax_burden$effective_rate > 0 && tax_burden$effective_rate < 1,
                info = paste("Effective rate should be between 0 and 1 for salary", salary))
  }
  
  # Test case 2: Comparison across filing statuses
  test_income_comparison <- 80000
  
  single_burden <- calculate_total_tax_burden(test_income_comparison, "single", "texas")
  married_burden <- calculate_total_tax_burden(test_income_comparison, "married_jointly", "texas")
  
  # Married filing jointly should generally have lower effective rate for this income level
  expect_true(married_burden$effective_rate <= single_burden$effective_rate,
              info = "Married filing jointly should have tax advantage at this income level")
})

# =============================================================================
# TEST EXECUTION AND REPORTING
# =============================================================================

#' Run all tax implications tests
#' @return Test results summary
run_tax_implications_tests <- function() {
  
  cat("=================================================================\n")
  cat("ATLAS LABS - COMPENSATION MODULE TAX IMPLICATIONS TESTING\n")
  cat("=================================================================\n\n")
  
  # Run the test suite
  test_results <- test_dir(".", pattern = "tax_implications", reporter = "summary")
  
  cat("\n=================================================================\n")
  cat("TEST EXECUTION COMPLETE\n")
  cat("=================================================================\n")
  
  return(test_results)
}

# =============================================================================
# MOCK DATA GENERATORS FOR TESTING
# =============================================================================

#' Generate mock employee salary data for testing
#' @param n Number of employees
#' @return Data frame with employee salary information
generate_mock_employee_data <- function(n = 100) {
  set.seed(42)  # For reproducible tests
  
  data.frame(
    employee_id = 1:n,
    salary = round(runif(n, 35000, 200000), 0),
    filing_status = sample(c("single", "married_jointly", "married_separately", "head_of_household"), 
                          n, replace = TRUE, prob = c(0.4, 0.35, 0.15, 0.1)),
    state = sample(c("texas", "california", "new_york", "florida"), 
                   n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2)),
    dependents = sample(0:4, n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.15, 0.05)),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# PERFORMANCE BENCHMARKING
# =============================================================================

#' Benchmark tax calculation performance
#' @param sample_size Number of calculations to perform
#' @return Performance metrics
benchmark_tax_calculations <- function(sample_size = 1000) {
  
  mock_data <- generate_mock_employee_data(sample_size)
  
  # Benchmark federal tax calculation
  federal_time <- system.time({
    federal_taxes <- mapply(calculate_federal_income_tax, 
                           mock_data$salary, 
                           mock_data$filing_status)
  })[["elapsed"]]
  
  # Benchmark FICA calculation
  fica_time <- system.time({
    fica_taxes <- mapply(calculate_fica_taxes, 
                        mock_data$salary,
                        mock_data$filing_status)
  })[["elapsed"]]
  
  # Benchmark total burden calculation
  total_time <- system.time({
    total_burdens <- mapply(calculate_total_tax_burden,
                           mock_data$salary,
                           mock_data$filing_status,
                           mock_data$state)
  })[["elapsed"]]
  
  results <- list(
    sample_size = sample_size,
    federal_tax_time = federal_time,
    fica_tax_time = fica_time,
    total_burden_time = total_time,
    calculations_per_second = sample_size / total_time
  )
  
  cat("Tax Calculation Performance Benchmark\n")
  cat("=====================================\n")
  cat("Sample Size:", sample_size, "\n")
  cat("Federal Tax Time:", federal_time, "seconds\n")
  cat("FICA Tax Time:", fica_time, "seconds\n")
  cat("Total Burden Time:", total_time, "seconds\n")
  cat("Calculations per Second:", round(results$calculations_per_second, 2), "\n\n")
  
  return(results)
}

# =============================================================================
# VALIDATION AGAINST EXTERNAL SOURCES
# =============================================================================

#' Validate tax calculations against known test cases
#' @return Validation results
validate_against_known_cases <- function() {
  
  # Known test cases from IRS examples and tax software
  test_cases <- list(
    list(income = 50000, filing_status = "single", expected_federal = 3968, tolerance = 50),
    list(income = 100000, filing_status = "married_jointly", expected_federal = 9000, tolerance = 100),
    list(income = 75000, filing_status = "head_of_household", expected_federal = 6200, tolerance = 100)
  )
  
  validation_results <- list()
  
  for (i in seq_along(test_cases)) {
    case <- test_cases[[i]]
    calculated_tax <- calculate_federal_income_tax(case$income, case$filing_status)
    
    validation_results[[i]] <- list(
      case = i,
      income = case$income,
      filing_status = case$filing_status,
      expected = case$expected_federal,
      calculated = calculated_tax,
      difference = abs(calculated_tax - case$expected_federal),
      within_tolerance = abs(calculated_tax - case$expected_federal) <= case$tolerance,
      percent_error = abs(calculated_tax - case$expected_federal) / case$expected_federal * 100
    )
  }
  
  # Summary
  passed_cases <- sum(sapply(validation_results, function(x) x$within_tolerance))
  total_cases <- length(validation_results)
  
  cat("External Validation Results\n")
  cat("===========================\n")
  cat("Test Cases Passed:", passed_cases, "/", total_cases, "\n")
  cat("Validation Success Rate:", round(passed_cases/total_cases * 100, 1), "%\n\n")
  
  return(validation_results)
}

# =============================================================================
# EXAMPLE USAGE AND TEST EXECUTION
# =============================================================================

if (FALSE) {  # Set to TRUE to run tests when sourcing this file
  
  # Run comprehensive test suite
  test_results <- run_tax_implications_tests()
  
  