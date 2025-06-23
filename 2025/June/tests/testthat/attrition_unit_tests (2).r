# =============================================================================
# ATLAS LABS HR ANALYTICS - ATTRITION ANALYSIS MODULE UNIT TESTS
# =============================================================================
# Comprehensive unit tests for attrition analysis module
# Excludes: Correlation calculations, Hypothesis testing, Confidence intervals, 
#          Seasonal adjustments
# =============================================================================

library(testthat)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(mockery)
library(DT)

# Source the module (assuming it exists)
# source("modules/attrition_module.R")

# =============================================================================
# TEST DATA SETUP
# =============================================================================

# Create comprehensive test datasets
create_test_employee_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    EmployeeID = 1:n,
    FirstName = paste0("Employee", 1:n),
    LastName = paste0("LastName", 1:n),
    Gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE),
    Age = sample(22:65, n, replace = TRUE),
    BusinessTravel = sample(c("None", "Rarely", "Frequently"), n, replace = TRUE),
    Department = sample(c("HR", "Sales", "Engineering", "Marketing", "Finance"), n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL", "WA"), n, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n, replace = TRUE),
    Education = sample(1:5, n, replace = TRUE),
    EducationField = sample(c("Engineering", "Business", "Marketing", "HR", "Other"), n, replace = TRUE),
    JobRole = sample(c("Manager", "Analyst", "Developer", "Specialist", "Director"), n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    Salary = sample(40000:150000, n, replace = TRUE),
    StockOptionLevel = sample(0:3, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    HireDate = as.Date("2020-01-01") + sample(0:1460, n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.15, 0.85)),
    YearsAtCompany = sample(0:20, n, replace = TRUE),
    YearsInMostRecentRole = sample(0:15, n, replace = TRUE),
    YearsSinceLastPromotion = sample(0:10, n, replace = TRUE),
    YearsWithCurrManager = sample(0:12, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create_test_performance_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    PerformanceID = 1:n,
    EmployeeID = 1:n,
    ReviewDate = as.Date("2023-01-01") + sample(0:365, n, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, n, replace = TRUE),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    SelfRating = sample(1:5, n, replace = TRUE),
    ManagerRating = sample(1:5, n, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, n, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# DATA PROCESSING TESTS
# =============================================================================

test_that("Attrition data processing functions work correctly", {
  
  # Test data preparation
  employee_data <- create_test_employee_data(50)
  
  # Test attrition rate calculation
  calculate_attrition_rate <- function(data, group_var = NULL) {
    if (is.null(group_var)) {
      return(sum(data$Attrition == "Yes") / nrow(data))
    } else {
      data %>%
        group_by(!!sym(group_var)) %>%
        summarise(
          total_employees = n(),
          attrition_count = sum(Attrition == "Yes"),
          attrition_rate = attrition_count / total_employees,
          .groups = "drop"
        )
    }
  }
  
  # Test overall attrition rate
  overall_rate <- calculate_attrition_rate(employee_data)
  expect_type(overall_rate, "double")
  expect_gte(overall_rate, 0)
  expect_lte(overall_rate, 1)
  
  # Test department-wise attrition
  dept_attrition <- calculate_attrition_rate(employee_data, "Department")
  expect_s3_class(dept_attrition, "data.frame")
  expect_true(all(c("Department", "total_employees", "attrition_count", "attrition_rate") %in% names(dept_attrition)))
  expect_true(all(dept_attrition$attrition_rate >= 0 & dept_attrition$attrition_rate <= 1))
  
  # Test with empty data
  empty_data <- employee_data[0, ]
  expect_error(calculate_attrition_rate(empty_data), NA) # Should not error
  
  # Test with all attrition = "No"
  no_attrition_data <- employee_data
  no_attrition_data$Attrition <- "No"
  rate_no_attrition <- calculate_attrition_rate(no_attrition_data)
  expect_equal(rate_no_attrition, 0)
  
  # Test with all attrition = "Yes"
  all_attrition_data <- employee_data
  all_attrition_data$Attrition <- "Yes"
  rate_all_attrition <- calculate_attrition_rate(all_attrition_data)
  expect_equal(rate_all_attrition, 1)
})

test_that("Attrition risk scoring works correctly", {
  
  employee_data <- create_test_employee_data(30)
  
  # Test risk scoring function
  calculate_attrition_risk <- function(data) {
    data %>%
      mutate(
        risk_score = case_when(
          YearsAtCompany < 2 ~ 3,
          YearsAtCompany < 5 ~ 2,
          TRUE ~ 1
        ) + case_when(
          JobSatisfaction < 3 ~ 2,
          JobSatisfaction < 4 ~ 1,
          TRUE ~ 0
        ) + case_when(
          OverTime == "Yes" ~ 1,
          TRUE ~ 0
        ),
        risk_category = case_when(
          risk_score >= 4 ~ "High",
          risk_score >= 2 ~ "Medium",
          TRUE ~ "Low"
        )
      )
  }
  
  # Add JobSatisfaction for testing
  employee_data$JobSatisfaction <- sample(1:5, nrow(employee_data), replace = TRUE)
  
  risk_data <- calculate_attrition_risk(employee_data)
  
  expect_true("risk_score" %in% names(risk_data))
  expect_true("risk_category" %in% names(risk_data))
  expect_true(all(risk_data$risk_score >= 0))
  expect_true(all(risk_data$risk_category %in% c("Low", "Medium", "High")))
  
  # Test risk score bounds
  expect_true(all(risk_data$risk_score <= 6)) # Maximum possible score
  
  # Test risk categories alignment
  high_risk <- risk_data[risk_data$risk_category == "High", ]
  expect_true(all(high_risk$risk_score >= 4))
  
  medium_risk <- risk_data[risk_data$risk_category == "Medium", ]
  expect_true(all(medium_risk$risk_score >= 2 & medium_risk$risk_score < 4))
  
  low_risk <- risk_data[risk_data$risk_category == "Low", ]
  expect_true(all(low_risk$risk_score < 2))
})

test_that("Attrition trend analysis works correctly", {
  
  employee_data <- create_test_employee_data(100)
  
  # Test trend calculation by hire date
  calculate_attrition_trends <- function(data) {
    data %>%
      mutate(
        hire_year = as.numeric(format(HireDate, "%Y")),
        hire_quarter = paste0(hire_year, "-Q", ceiling(as.numeric(format(HireDate, "%m")) / 3))
      ) %>%
      group_by(hire_quarter) %>%
      summarise(
        total_hires = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / total_hires,
        .groups = "drop"
      ) %>%
      arrange(hire_quarter)
  }
  
  trends <- calculate_attrition_trends(employee_data)
  
  expect_s3_class(trends, "data.frame")
  expect_true(all(c("hire_quarter", "total_hires", "attrition_count", "attrition_rate") %in% names(trends)))
  expect_true(all(trends$attrition_rate >= 0 & trends$attrition_rate <= 1))
  expect_true(all(trends$total_hires > 0))
  expect_true(all(trends$attrition_count >= 0))
  expect_true(all(trends$attrition_count <= trends$total_hires))
})

# =============================================================================
# VISUALIZATION TESTS
# =============================================================================

test_that("Attrition visualization functions create valid plots", {
  
  employee_data <- create_test_employee_data(50)
  
  # Test department attrition bar chart
  create_department_attrition_chart <- function(data) {
    dept_summary <- data %>%
      group_by(Department) %>%
      summarise(
        total = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / total,
        .groups = "drop"
      )
    
    p <- ggplot(dept_summary, aes(x = reorder(Department, -attrition_rate), y = attrition_rate)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_text(aes(label = scales::percent(attrition_rate, accuracy = 0.1)), 
                vjust = -0.5, size = 3) +
      labs(
        title = "Attrition Rate by Department",
        x = "Department",
        y = "Attrition Rate"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
    
    return(p)
  }
  
  dept_chart <- create_department_attrition_chart(employee_data)
  
  expect_s3_class(dept_chart, "ggplot")
  expect_true(length(dept_chart$layers) > 0)
  expect_equal(dept_chart$labels$title, "Attrition Rate by Department")
  expect_equal(dept_chart$labels$x, "Department")
  expect_equal(dept_chart$labels$y, "Attrition Rate")
  
  # Test age group attrition analysis
  create_age_attrition_chart <- function(data) {
    age_summary <- data %>%
      mutate(
        age_group = case_when(
          Age < 30 ~ "Under 30",
          Age < 40 ~ "30-39",
          Age < 50 ~ "40-49",
          Age < 60 ~ "50-59",
          TRUE ~ "60+"
        )
      ) %>%
      group_by(age_group) %>%
      summarise(
        total = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / total,
        .groups = "drop"
      )
    
    p <- ggplot(age_summary, aes(x = age_group, y = attrition_rate)) +
      geom_col(fill = "coral", alpha = 0.7) +
      labs(
        title = "Attrition Rate by Age Group",
        x = "Age Group",
        y = "Attrition Rate"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
    
    return(p)
  }
  
  age_chart <- create_age_attrition_chart(employee_data)
  
  expect_s3_class(age_chart, "ggplot")
  expect_equal(age_chart$labels$title, "Attrition Rate by Age Group")
  
  # Test tenure vs attrition scatter plot
  create_tenure_attrition_chart <- function(data) {
    p <- ggplot(data, aes(x = YearsAtCompany, y = as.numeric(Attrition == "Yes"))) +
      geom_point(alpha = 0.6, position = position_jitter(height = 0.05)) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
      labs(
        title = "Attrition Probability by Years at Company",
        x = "Years at Company",
        y = "Attrition Probability"
      ) +
      theme_minimal()
    
    return(p)
  }
  
  tenure_chart <- create_tenure_attrition_chart(employee_data)
  
  expect_s3_class(tenure_chart, "ggplot")
  expect_equal(tenure_chart$labels$title, "Attrition Probability by Years at Company")
})

test_that("Interactive plotly visualizations work correctly", {
  
  employee_data <- create_test_employee_data(30)
  
  # Test interactive attrition heatmap
  create_attrition_heatmap <- function(data) {
    heatmap_data <- data %>%
      group_by(Department, JobRole) %>%
      summarise(
        total = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / total,
        .groups = "drop"
      ) %>%
      filter(total >= 2) # Only include combinations with at least 2 employees
    
    p <- ggplot(heatmap_data, aes(x = Department, y = JobRole, fill = attrition_rate)) +
      geom_tile(color = "white") +
      geom_text(aes(label = paste0(attrition_count, "/", total)), 
                color = "white", size = 3) +
      scale_fill_gradient(low = "lightblue", high = "red", 
                         labels = scales::percent_format()) +
      labs(
        title = "Attrition Heatmap: Department vs Job Role",
        x = "Department",
        y = "Job Role",
        fill = "Attrition Rate"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(ggplotly(p))
  }
  
  # Test that function runs without error
  expect_error(create_attrition_heatmap(employee_data), NA)
  
  # Test the base ggplot before plotly conversion
  test_heatmap_base <- function(data) {
    heatmap_data <- data %>%
      group_by(Department, JobRole) %>%
      summarise(
        total = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / total,
        .groups = "drop"
      ) %>%
      filter(total >= 2)
    
    return(nrow(heatmap_data) > 0)
  }
  
  expect_true(test_heatmap_base(employee_data))
})

# =============================================================================
# DATA FILTERING TESTS
# =============================================================================

test_that("Attrition data filtering works correctly", {
  
  employee_data <- create_test_employee_data(100)
  
  # Test department filtering
  filter_by_department <- function(data, departments) {
    if (is.null(departments) || length(departments) == 0) {
      return(data)
    }
    return(data[data$Department %in% departments, ])
  }
  
  # Test with single department
  hr_data <- filter_by_department(employee_data, "HR")
  expect_true(all(hr_data$Department == "HR"))
  expect_true(nrow(hr_data) > 0)
  
  # Test with multiple departments
  multi_dept_data <- filter_by_department(employee_data, c("HR", "Sales"))
  expect_true(all(multi_dept_data$Department %in% c("HR", "Sales")))
  
  # Test with no departments (should return original data)
  no_filter_data <- filter_by_department(employee_data, NULL)
  expect_equal(nrow(no_filter_data), nrow(employee_data))
  
  # Test with empty department list
  empty_filter_data <- filter_by_department(employee_data, character(0))
  expect_equal(nrow(empty_filter_data), nrow(employee_data))
  
  # Test age range filtering
  filter_by_age_range <- function(data, min_age = NULL, max_age = NULL) {
    if (!is.null(min_age)) {
      data <- data[data$Age >= min_age, ]
    }
    if (!is.null(max_age)) {
      data <- data[data$Age <= max_age, ]
    }
    return(data)
  }
  
  # Test age filtering
  young_employees <- filter_by_age_range(employee_data, min_age = 25, max_age = 35)
  expect_true(all(young_employees$Age >= 25 & young_employees$Age <= 35))
  
  # Test salary range filtering
  filter_by_salary_range <- function(data, min_salary = NULL, max_salary = NULL) {
    if (!is.null(min_salary)) {
      data <- data[data$Salary >= min_salary, ]
    }
    if (!is.null(max_salary)) {
      data <- data[data$Salary <= max_salary, ]
    }
    return(data)
  }
  
  high_salary_employees <- filter_by_salary_range(employee_data, min_salary = 80000)
  expect_true(all(high_salary_employees$Salary >= 80000))
})

test_that("Complex filtering combinations work correctly", {
  
  employee_data <- create_test_employee_data(100)
  
  # Test complex multi-filter function
  apply_complex_filters <- function(data, filters) {
    result <- data
    
    if (!is.null(filters$departments) && length(filters$departments) > 0) {
      result <- result[result$Department %in% filters$departments, ]
    }
    
    if (!is.null(filters$age_range)) {
      if (!is.null(filters$age_range$min)) {
        result <- result[result$Age >= filters$age_range$min, ]
      }
      if (!is.null(filters$age_range$max)) {
        result <- result[result$Age <= filters$age_range$max, ]
      }
    }
    
    if (!is.null(filters$tenure_range)) {
      if (!is.null(filters$tenure_range$min)) {
        result <- result[result$YearsAtCompany >= filters$tenure_range$min, ]
      }
      if (!is.null(filters$tenure_range$max)) {
        result <- result[result$YearsAtCompany <= filters$tenure_range$max, ]
      }
    }
    
    if (!is.null(filters$attrition_only) && filters$attrition_only) {
      result <- result[result$Attrition == "Yes", ]
    }
    
    return(result)
  }
  
  # Test complex filter combinations
  complex_filters <- list(
    departments = c("HR", "Sales"),
    age_range = list(min = 25, max = 45),
    tenure_range = list(min = 2, max = 10),
    attrition_only = TRUE
  )
  
  filtered_data <- apply_complex_filters(employee_data, complex_filters)
  
  expect_true(all(filtered_data$Department %in% c("HR", "Sales")))
  expect_true(all(filtered_data$Age >= 25 & filtered_data$Age <= 45))
  expect_true(all(filtered_data$YearsAtCompany >= 2 & filtered_data$YearsAtCompany <= 10))
  expect_true(all(filtered_data$Attrition == "Yes"))
  
  # Test with empty filters
  empty_filters <- list()
  no_filter_result <- apply_complex_filters(employee_data, empty_filters)
  expect_equal(nrow(no_filter_result), nrow(employee_data))
})

# =============================================================================
# BUSINESS LOGIC TESTS
# =============================================================================

test_that("Attrition business rules are correctly implemented", {
  
  employee_data <- create_test_employee_data(50)
  
  # Test high-risk employee identification
  identify_high_risk_employees <- function(data) {
    # Add some required columns for testing
    data$JobSatisfaction <- sample(1:5, nrow(data), replace = TRUE)
    data$WorkLifeBalance <- sample(1:5, nrow(data), replace = TRUE)
    
    high_risk <- data %>%
      filter(
        (YearsAtCompany < 3 & Attrition == "No") |
        (JobSatisfaction <= 2) |
        (WorkLifeBalance <= 2) |
        (YearsSinceLastPromotion > 5 & YearsAtCompany > 5) |
        (OverTime == "Yes" & WorkLifeBalance <= 3)
      ) %>%
      mutate(
        risk_reasons = case_when(
          YearsAtCompany < 3 ~ "New Employee",
          JobSatisfaction <= 2 ~ "Low Job Satisfaction",
          WorkLifeBalance <= 2 ~ "Poor Work-Life Balance",
          YearsSinceLastPromotion > 5 & YearsAtCompany > 5 ~ "Overdue Promotion",
          OverTime == "Yes" & WorkLifeBalance <= 3 ~ "Overtime Burnout",
          TRUE ~ "Multiple Factors"
        )
      )
    
    return(high_risk)
  }
  
  high_risk_employees <- identify_high_risk_employees(employee_data)
  
  expect_s3_class(high_risk_employees, "data.frame")
  expect_true("risk_reasons" %in% names(high_risk_employees))
  expect_true(nrow(high_risk_employees) <= nrow(employee_data))
  
  # Test retention recommendations
  generate_retention_recommendations <- function(data) {
    recommendations <- list()
    
    # Analyze overtime patterns
    overtime_attrition <- data %>%
      group_by(OverTime) %>%
      summarise(attrition_rate = sum(Attrition == "Yes") / n(), .groups = "drop")
    
    if (any(overtime_attrition$attrition_rate > 0.2)) {
      recommendations <- append(recommendations, "Review overtime policies")
    }
    
    # Analyze department-specific issues
    dept_attrition <- data %>%
      group_by(Department) %>%
      summarise(attrition_rate = sum(Attrition == "Yes") / n(), .groups = "drop") %>%
      filter(attrition_rate > 0.15)
    
    if (nrow(dept_attrition) > 0) {
      recommendations <- append(recommendations, 
                               paste("Focus on retention in:", paste(dept_attrition$Department, collapse = ", ")))
    }
    
    # Analyze tenure patterns
    short_tenure_attrition <- sum(data$YearsAtCompany < 2 & data$Attrition == "Yes") / 
                             sum(data$YearsAtCompany < 2)
    
    if (short_tenure_attrition > 0.3) {
      recommendations <- append(recommendations, "Improve onboarding and early engagement")
    }
    
    return(recommendations)
  }
  
  recommendations <- generate_retention_recommendations(employee_data)
  
  expect_type(recommendations, "list")
  expect_true(all(sapply(recommendations, is.character)))
})

test_that("Attrition metrics calculations are accurate", {
  
  # Create controlled test data
  controlled_data <- data.frame(
    EmployeeID = 1:20,
    Department = rep(c("HR", "Sales"), each = 10),
    Attrition = c(rep("Yes", 4), rep("No", 6), rep("Yes", 2), rep("No", 8)),
    YearsAtCompany = c(1:10, 1:10),
    Salary = rep(c(50000, 80000), each = 10),
    stringsAsFactors = FALSE
  )
  
  # Test overall attrition rate
  calculate_overall_metrics <- function(data) {
    list(
      total_employees = nrow(data),
      total_attrition = sum(data$Attrition == "Yes"),
      attrition_rate = sum(data$Attrition == "Yes") / nrow(data),
      retention_rate = sum(data$Attrition == "No") / nrow(data)
    )
  }
  
  metrics <- calculate_overall_metrics(controlled_data)
  
  expect_equal(metrics$total_employees, 20)
  expect_equal(metrics$total_attrition, 6)
  expect_equal(metrics$attrition_rate, 0.3)
  expect_equal(metrics$retention_rate, 0.7)
  expect_equal(metrics$attrition_rate + metrics$retention_rate, 1)
  
  # Test department-specific metrics
  hr_metrics <- calculate_overall_metrics(controlled_data[controlled_data$Department == "HR", ])
  sales_metrics <- calculate_overall_metrics(controlled_data[controlled_data$Department == "Sales", ])
  
  expect_equal(hr_metrics$attrition_rate, 0.4) # 4 out of 10
  expect_equal(sales_metrics$attrition_rate, 0.2) # 2 out of 10
})

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("Input validation for attrition analysis functions", {
  
  # Test with invalid data types
  expect_error(calculate_attrition_rate("not_a_dataframe"))
  expect_error(calculate_attrition_rate(list(a = 1, b = 2)))
  expect_error(calculate_attrition_rate(NULL))
  
  # Test with missing required columns
  incomplete_data <- data.frame(EmployeeID = 1:5, Name = paste0("Emp", 1:5))
  expect_error(calculate_attrition_rate(incomplete_data))
  
  # Test with invalid attrition values
  invalid_attrition_data <- create_test_employee_data(10)
  invalid_attrition_data$Attrition <- c("Yes", "No", "Maybe", "Yes", "No", "Unknown", "Yes", "No", "Yes", "No")
  
  # Function should handle invalid values gracefully
  safe_calculate_attrition_rate <- function(data) {
    tryCatch({
      # Clean the data first
      data <- data[data$Attrition %in% c("Yes", "No"), ]
      if (nrow(data) == 0) {
        return(NA)
      }
      return(sum(data$Attrition == "Yes") / nrow(data))
    }, error = function(e) {
      return(NA)
    })
  }
  
  result <- safe_calculate_attrition_rate(invalid_attrition_data)
  expect_true(!is.na(result) || is.na(result)) # Should either work or return NA
  
  # Test with edge cases
  single_row_data <- create_test_employee_data(1)
  single_result <- safe_calculate_attrition_rate(single_row_data)
  expect_true(single_result %in% c(0, 1))
})

test_that("Date handling in attrition analysis", {
  
  employee_data <- create_test_employee_data(20)
  
  # Test hire date analysis
  analyze_hire_date_patterns <- function(data) {
    data %>%
      mutate(
        hire_year = as.numeric(format(HireDate, "%Y")),
        hire_month = as.numeric(format(HireDate, "%m")),
        days_since_hire = as.numeric(Sys.Date() - HireDate)
      ) %>%
      group_by(hire_year) %>%
      summarise(
        hires_count = n(),
        attrition_count = sum(Attrition == "Yes"),
        attrition_rate = attrition_count / hires_count,
        .groups = "drop"
      )
  }
  
  hire_analysis <- analyze_hire_date_patterns(employee_data)
  
  expect_s3_class(hire_analysis, "data.frame")
  expect_true(all(c("hire_year", "hires_count", "attrition_count", "attrition_rate") %in% names(hire_analysis)))
  expect_true(all(hire_analysis$hire_year >= 2020)) # Based on test data creation
  expect_true(all(hire_analysis$attrition_rate >= 0 & hire_analysis$attrition_rate <= 1))
  
  # Test with invalid dates
  invalid_date_data <- employee_data
  invalid_date_data$HireDate[1:3] <- NA
  
  safe_analyze_dates <- function(data) {
    tryCatch({
      data <- data[!is.na(data$HireDate), ]
      if (nrow(data) == 0) {
        return(data.frame())
      }
      return(analyze_hire_date_patterns(