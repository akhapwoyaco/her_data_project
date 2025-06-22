# Atlas Labs HR Analytics Dashboard - Comprehensive Unit Tests
# Excluding Overview Module specific areas as requested
# Author: akhapwoyaco (GitHub)

# Load required libraries for testing
library(testthat)
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(R6)
library(mockery)

# Source application files (assuming they exist)
source("global.R", local = TRUE)
source("utils.R", local = TRUE)
source("custom_theme.R", local = TRUE)

# Load all modules
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
purrr::walk(module_files, source)

# ============================================================================
# 1. DATA LOADER MODULE TESTS
# ============================================================================

test_that("Data Loader Module - File Validation", {
  
  # Test file existence validation
  expect_error(
    validate_csv_file("nonexistent_file.csv"),
    "File does not exist"
  )
  
  # Test file extension validation
  expect_error(
    validate_csv_file("test_file.txt"),
    "Invalid file extension"
  )
  
  # Mock valid CSV file
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:5, y = letters[1:5]), temp_csv, row.names = FALSE)
  
  expect_silent(validate_csv_file(temp_csv))
  unlink(temp_csv)
})

test_that("Data Loader Module - Data Type Conversion", {
  
  # Create sample data with mixed types
  test_data <- data.frame(
    EmployeeID = c("E001", "E002", "E003"),
    Age = c("25", "30", "35"),
    Salary = c("50000", "60000", "70000"),
    HireDate = c("2020-01-01", "2019-06-15", "2021-03-10"),
    stringsAsFactors = FALSE
  )
  
  # Test data type conversion
  converted_data <- convert_data_types(test_data)
  
  expect_true(is.character(converted_data$EmployeeID))
  expect_true(is.numeric(converted_data$Age))
  expect_true(is.numeric(converted_data$Salary))
  expect_true(inherits(converted_data$HireDate, "Date"))
})

test_that("Data Loader Module - Missing Value Detection", {
  
  # Create data with missing values
  test_data <- data.frame(
    ID = 1:5,
    Name = c("John", NA, "Jane", "Bob", ""),
    Score = c(85, 90, NA, 78, 92)
  )
  
  missing_report <- detect_missing_values(test_data)
  
  expect_equal(missing_report$Name$count, 2)  # NA and empty string
  expect_equal(missing_report$Score$count, 1)  # One NA
  expect_equal(missing_report$ID$count, 0)     # No missing values
})

test_that("Data Loader Module - Data Merge Operations", {
  
  # Create sample datasets
  employees <- data.frame(
    EmployeeID = c("E001", "E002", "E003"),
    Name = c("John", "Jane", "Bob"),
    Department = c("IT", "HR", "Finance")
  )
  
  performance <- data.frame(
    EmployeeID = c("E001", "E002", "E004"),
    Rating = c(4.5, 4.2, 3.8),
    Year = c(2023, 2023, 2023)
  )
  
  # Test inner join
  merged_inner <- merge_employee_data(employees, performance, join_type = "inner")
  expect_equal(nrow(merged_inner), 2)
  
  # Test left join
  merged_left <- merge_employee_data(employees, performance, join_type = "left")
  expect_equal(nrow(merged_left), 3)
  expect_true(is.na(merged_left$Rating[3]))
})

# ============================================================================
# 2. LOGGER MODULE TESTS (R6 Class)
# ============================================================================

test_that("Logger Module - R6 Class Initialization", {
  
  logger <- AtlasLogger$new()
  
  expect_true(R6::is.R6(logger))
  expect_true(inherits(logger, "AtlasLogger"))
  expect_true(is.environment(logger$logs))
  expect_equal(length(logger$logs), 0)
})

test_that("Logger Module - Log Level Functionality", {
  
  logger <- AtlasLogger$new()
  
  # Test different log levels
  logger$log_info("Test info message", "test_module")
  logger$log_warning("Test warning message", "test_module")
  logger$log_error("Test error message", "test_module")
  
  logs <- logger$get_logs()
  
  expect_equal(nrow(logs), 3)
  expect_true(all(c("INFO", "WARNING", "ERROR") %in% logs$level))
  expect_true(all(logs$module == "test_module"))
})

test_that("Logger Module - Performance Tracking", {
  
  logger <- AtlasLogger$new()
  
  # Mock performance data
  perf_data <- list(
    execution_time = 0.5,
    memory_used = 1024,
    cpu_usage = 25.5
  )
  
  logger$log_performance("Operation completed", "performance_module", perf_data)
  
  logs <- logger$get_logs()
  log_entry <- logs[logs$level == "PERFORMANCE", ]
  
  expect_equal(nrow(log_entry), 1)
  expect_equal(log_entry$execution_time, 0.5)
  expect_equal(log_entry$memory_used, 1024)
})

test_that("Logger Module - Memory Usage Tracking", {
  
  logger <- AtlasLogger$new()
  
  # Test memory tracking
  initial_memory <- logger$get_memory_usage()
  
  # Simulate memory-intensive operation
  large_data <- matrix(rnorm(10000), ncol = 100)
  
  final_memory <- logger$get_memory_usage()
  
  expect_true(final_memory >= initial_memory)
  expect_true(is.numeric(initial_memory))
  expect_true(is.numeric(final_memory))
})

test_that("Logger Module - Log Filtering and Search", {
  
  logger <- AtlasLogger$new()
  
  # Add various log entries
  logger$log_info("User login", "auth_module")
  logger$log_warning("High memory usage", "performance_module")
  logger$log_error("Database connection failed", "data_module")
  logger$log_info("Data loaded successfully", "data_module")
  
  # Test filtering by module
  data_logs <- logger$filter_logs_by_module("data_module")
  expect_equal(nrow(data_logs), 2)
  
  # Test filtering by level
  error_logs <- logger$filter_logs_by_level("ERROR")
  expect_equal(nrow(error_logs), 1)
  
  # Test search functionality
  memory_logs <- logger$search_logs("memory")
  expect_equal(nrow(memory_logs), 1)
})

# ============================================================================
# 3. ATTRITION MODULE TESTS
# ============================================================================

test_that("Attrition Module - Attrition Rate Calculation", {
  
  # Create sample employee data
  sample_data <- data.frame(
    EmployeeID = 1:10,
    Department = rep(c("IT", "HR"), each = 5),
    Attrition = c(rep("Yes", 3), rep("No", 7)),
    stringsAsFactors = FALSE
  )
  
  # Test overall attrition rate
  overall_rate <- calculate_attrition_rate(sample_data)
  expect_equal(overall_rate, 0.3)
  
  # Test department-wise attrition
  dept_rates <- calculate_attrition_by_department(sample_data)
  expect_true(is.data.frame(dept_rates))
  expect_true("Department" %in% names(dept_rates))
  expect_true("AttritionRate" %in% names(dept_rates))
})

test_that("Attrition Module - Survival Analysis", {
  
  # Create sample tenure data
  tenure_data <- data.frame(
    EmployeeID = 1:20,
    YearsAtCompany = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 
                       6, 7, 8, 9, 10, 1, 2, 3, 4, 5),
    Attrition = c(rep("Yes", 10), rep("No", 10)),
    stringsAsFactors = FALSE
  )
  
  survival_analysis <- perform_survival_analysis(tenure_data)
  
  expect_true(is.list(survival_analysis))
  expect_true("survival_probability" %in% names(survival_analysis))
  expect_true("risk_factors" %in% names(survival_analysis))
})

test_that("Attrition Module - Risk Factor Analysis", {
  
  # Create comprehensive employee data
  risk_data <- data.frame(
    EmployeeID = 1:50,
    Age = sample(22:65, 50, replace = TRUE),
    YearsAtCompany = sample(1:20, 50, replace = TRUE),
    Salary = sample(30000:120000, 50, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 50, replace = TRUE),
    BusinessTravel = sample(c("Rarely", "Frequently", "Travel_Rarely"), 50, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 50, replace = TRUE, prob = c(0.3, 0.7)),
    stringsAsFactors = FALSE
  )
  
  risk_factors <- analyze_attrition_risk_factors(risk_data)
  
  expect_true(is.data.frame(risk_factors))
  expect_true("factor" %in% names(risk_factors))
  expect_true("importance" %in% names(risk_factors))
  expect_true(nrow(risk_factors) > 0)
})

# ============================================================================
# 4. DEMOGRAPHICS MODULE TESTS
# ============================================================================

test_that("Demographics Module - Age Distribution Analysis", {
  
  # Create sample demographic data
  demo_data <- data.frame(
    EmployeeID = 1:100,
    Age = sample(22:65, 100, replace = TRUE),
    Gender = sample(c("Male", "Female", "Other"), 100, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  age_dist <- analyze_age_distribution(demo_data)
  
  expect_true(is.list(age_dist))
  expect_true("age_groups" %in% names(age_dist))
  expect_true("statistics" %in% names(age_dist))
  expect_true(age_dist$statistics$mean_age > 0)
})

test_that("Demographics Module - Diversity Metrics", {
  
  # Create sample diversity data
  diversity_data <- data.frame(
    EmployeeID = 1:200,
    Gender = sample(c("Male", "Female", "Other"), 200, replace = TRUE, prob = c(0.5, 0.45, 0.05)),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 200, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing", "Operations"), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  diversity_metrics <- calculate_diversity_metrics(diversity_data)
  
  expect_true(is.list(diversity_metrics))
  expect_true("gender_diversity" %in% names(diversity_metrics))
  expect_true("ethnic_diversity" %in% names(diversity_metrics))
  expect_true("department_diversity" %in% names(diversity_metrics))
})

test_that("Demographics Module - Geographic Distribution", {
  
  # Create sample geographic data
  geo_data <- data.frame(
    EmployeeID = 1:150,
    State = sample(c("NY", "CA", "TX", "FL", "IL"), 150, replace = TRUE),
    DistanceFromHome = sample(1:50, 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  geo_analysis <- analyze_geographic_distribution(geo_data)
  
  expect_true(is.data.frame(geo_analysis))
  expect_true("State" %in% names(geo_analysis))
  expect_true("EmployeeCount" %in% names(geo_analysis))
  expect_equal(sum(geo_analysis$EmployeeCount), 150)
})

# ============================================================================
# 5. PERFORMANCE MODULE TESTS
# ============================================================================

test_that("Performance Module - Rating Distribution Analysis", {
  
  # Create sample performance data
  perf_data <- data.frame(
    EmployeeID = 1:100,
    ManagerRating = sample(1:5, 100, replace = TRUE, prob = c(0.05, 0.15, 0.3, 0.35, 0.15)),
    SelfRating = sample(1:5, 100, replace = TRUE, prob = c(0.02, 0.08, 0.25, 0.45, 0.2)),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  rating_dist <- analyze_rating_distribution(perf_data)
  
  expect_true(is.list(rating_dist))
  expect_true("manager_rating_dist" %in% names(rating_dist))
  expect_true("self_rating_dist" %in% names(rating_dist))
  expect_true("rating_correlation" %in% names(rating_dist))
})

test_that("Performance Module - Training Analysis", {
  
  # Create sample training data
  training_data <- data.frame(
    EmployeeID = 1:80,
    TrainingOpportunitiesOffered = sample(1:10, 80, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 80, replace = TRUE),
    ManagerRating = sample(1:5, 80, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Ensure taken <= offered
  training_data$TrainingOpportunitiesTaken <- pmin(
    training_data$TrainingOpportunitiesTaken, 
    training_data$TrainingOpportunitiesOffered
  )
  
  training_analysis <- analyze_training_effectiveness(training_data)
  
  expect_true(is.list(training_analysis))
  expect_true("training_utilization" %in% names(training_analysis))
  expect_true("performance_correlation" %in% names(training_analysis))
})

test_that("Performance Module - Performance Trend Analysis", {
  
  # Create sample performance trend data
  trend_data <- data.frame(
    EmployeeID = rep(1:20, each = 5),
    ReviewDate = rep(seq(as.Date("2020-01-01"), as.Date("2024-01-01"), by = "year"), 20),
    ManagerRating = sample(1:5, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  trend_analysis <- analyze_performance_trends(trend_data)
  
  expect_true(is.data.frame(trend_analysis))
  expect_true("EmployeeID" %in% names(trend_analysis))
  expect_true("trend_direction" %in% names(trend_analysis))
})

# ============================================================================
# 6. COMPENSATION MODULE TESTS
# ============================================================================

test_that("Compensation Module - Salary Distribution Analysis", {
  
  # Create sample compensation data
  comp_data <- data.frame(
    EmployeeID = 1:200,
    Salary = sample(30000:150000, 200, replace = TRUE),
    Gender = sample(c("Male", "Female"), 200, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance", "Marketing"), 200, replace = TRUE),
    YearsAtCompany = sample(1:20, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  salary_dist <- analyze_salary_distribution(comp_data)
  
  expect_true(is.list(salary_dist))
  expect_true("overall_stats" %in% names(salary_dist))
  expect_true("by_gender" %in% names(salary_dist))
  expect_true("by_department" %in% names(salary_dist))
})

test_that("Compensation Module - Pay Equity Analysis", {
  
  # Create sample pay equity data
  equity_data <- data.frame(
    EmployeeID = 1:300,
    Salary = sample(40000:140000, 300, replace = TRUE),
    Gender = sample(c("Male", "Female"), 300, replace = TRUE, prob = c(0.52, 0.48)),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian"), 300, replace = TRUE),
    JobRole = sample(c("Analyst", "Manager", "Director", "VP"), 300, replace = TRUE),
    YearsExperience = sample(1:25, 300, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  pay_equity <- analyze_pay_equity(equity_data)
  
  expect_true(is.list(pay_equity))
  expect_true("gender_pay_gap" %in% names(pay_equity))
  expect_true("ethnicity_pay_gap" %in% names(pay_equity))
  expect_true("adjusted_pay_gap" %in% names(pay_equity))
})

test_that("Compensation Module - Stock Option Analysis", {
  
  # Create sample stock option data
  stock_data <- data.frame(
    EmployeeID = 1:150,
    Salary = sample(50000:120000, 150, replace = TRUE),
    StockOptionLevel = sample(0:3, 150, replace = TRUE),
    JobRole = sample(c("Individual Contributor", "Manager", "Director"), 150, replace = TRUE),
    YearsAtCompany = sample(1:15, 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  stock_analysis <- analyze_stock_options(stock_data)
  
  expect_true(is.data.frame(stock_analysis))
  expect_true("StockOptionLevel" %in% names(stock_analysis))
  expect_true("EmployeeCount" %in% names(stock_analysis))
})

# ============================================================================
# 7. SATISFACTION MODULE TESTS
# ============================================================================

test_that("Satisfaction Module - Satisfaction Score Calculation", {
  
  # Create sample satisfaction data
  satisfaction_data <- data.frame(
    EmployeeID = 1:120,
    JobSatisfaction = sample(1:5, 120, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 120, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 120, replace = TRUE),
    WorkLifeBalance = sample(1:5, 120, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  satisfaction_scores <- calculate_satisfaction_scores(satisfaction_data)
  
  expect_true(is.data.frame(satisfaction_scores))
  expect_true("EmployeeID" %in% names(satisfaction_scores))
  expect_true("OverallSatisfaction" %in% names(satisfaction_scores))
  expect_true(all(satisfaction_scores$OverallSatisfaction >= 1 & 
                  satisfaction_scores$OverallSatisfaction <= 5))
})

test_that("Satisfaction Module - Work-Life Balance Analysis", {
  
  # Create sample work-life balance data
  wlb_data <- data.frame(
    EmployeeID = 1:100,
    WorkLifeBalance = sample(1:5, 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7)),
    BusinessTravel = sample(c("Rarely", "Frequently", "Travel_Rarely"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.25, 0.75)),
    stringsAsFactors = FALSE
  )
  
  wlb_analysis <- analyze_work_life_balance(wlb_data)
  
  expect_true(is.list(wlb_analysis))
  expect_true("balance_by_overtime" %in% names(wlb_analysis))
  expect_true("balance_by_travel" %in% names(wlb_analysis))
  expect_true("balance_attrition_correlation" %in% names(wlb_analysis))
})

test_that("Satisfaction Module - Satisfaction Driver Analysis", {
  
  # Create comprehensive satisfaction data
  driver_data <- data.frame(
    EmployeeID = 1:200,
    JobSatisfaction = sample(1:5, 200, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 200, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 200, replace = TRUE),
    WorkLifeBalance = sample(1:5, 200, replace = TRUE),
    Salary = sample(30000:120000, 200, replace = TRUE),
    YearsAtCompany = sample(1:20, 200, replace = TRUE),
    ManagerRating = sample(1:5, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  driver_analysis <- analyze_satisfaction_drivers(driver_data)
  
  expect_true(is.data.frame(driver_analysis))
  expect_true("factor" %in% names(driver_analysis))
  expect_true("correlation" %in% names(driver_analysis))
  expect_true("importance" %in% names(driver_analysis))
})

# ============================================================================
# 8. REPORT MODULE TESTS
# ============================================================================

test_that("Report Module - Parameter Validation", {
  
  # Test valid parameters
  valid_params <- list(
    data_summary = list(total_employees = 100),
    selected_filters = list(department = c("IT", "HR")),
    analysis_date = Sys.Date()
  )
  
  expect_true(validate_report_parameters(valid_params))
  
  # Test invalid parameters
  invalid_params <- list(
    data_summary = NULL,
    selected_filters = "invalid_filter"
  )
  
  expect_false(validate_report_parameters(invalid_params))
})

test_that("Report Module - Report Generation", {
  
  # Create sample report data
  report_data <- list(
    kpi_metrics = list(
      total_employees = 500,
      attrition_rate = 0.15,
      avg_satisfaction = 3.8,
      avg_salary = 75000
    ),
    selected_filters = list(
      department = c("IT", "Finance"),
      years_at_company = c(1, 5)
    ),
    analysis_date = Sys.Date()
  )
  
  # Test report generation (mock)
  report_result <- generate_report(report_data, format = "html")
  
  expect_true(is.list(report_result))
  expect_true("status" %in% names(report_result))
  expect_equal(report_result$status, "success")
})

test_that("Report Module - Export Functionality", {
  
  # Create sample export data
  export_data <- data.frame(
    Department = c("IT", "HR", "Finance"),
    EmployeeCount = c(50, 25, 30),
    AttritionRate = c(0.12, 0.08, 0.15)
  )
  
  # Test CSV export
  temp_file <- tempfile(fileext = ".csv")
  export_result <- export_data_to_csv(export_data, temp_file)
  
  expect_true(export_result)
  expect_true(file.exists(temp_file))
  
  # Verify exported data
  imported_data <- read.csv(temp_file)
  expect_equal(nrow(imported_data), 3)
  expect_equal(names(imported_data), names(export_data))
  
  unlink(temp_file)
})

# ============================================================================
# 9. UTILITY FUNCTIONS TESTS
# ============================================================================

test_that("Utility Functions - Data Validation", {
  
  # Test numeric validation
  expect_true(is_valid_numeric("123.45"))
  expect_false(is_valid_numeric("abc"))
  expect_false(is_valid_numeric(""))
  
  # Test date validation
  expect_true(is_valid_date("2023-12-25"))
  expect_false(is_valid_date("2023-13-25"))
  expect_false(is_valid_date("invalid_date"))
  
  # Test email validation
  expect_true(is_valid_email("test@example.com"))
  expect_false(is_valid_email("invalid_email"))
  expect_false(is_valid_email(""))
})

test_that("Utility Functions - String Manipulation", {
  
  # Test string cleaning
  expect_equal(clean_string("  Test String  "), "Test String")
  expect_equal(clean_string("Test\nString"), "Test String")
  expect_equal(clean_string("Test\t\tString"), "Test String")
  
  # Test title case conversion
  expect_equal(to_title_case("hello world"), "Hello World")
  expect_equal(to_title_case("HELLO WORLD"), "Hello World")
  
  # Test string truncation
  expect_equal(truncate_string("This is a long string", 10), "This is a...")
  expect_equal(truncate_string("Short", 10), "Short")
})

test_that("Utility Functions - Number Formatting", {
  
  # Test currency formatting
  expect_equal(format_currency(50000), "$50,000")
  expect_equal(format_currency(1234.56), "$1,235")
  expect_equal(format_currency(0), "$0")
  
  # Test percentage formatting
  expect_equal(format_percentage(0.25), "25.0%")
  expect_equal(format_percentage(0.1234), "12.3%")
  expect_equal(format_percentage(1), "100.0%")
  
  # Test number abbreviation
  expect_equal(abbreviate_number(1500), "1.5K")
  expect_equal(abbreviate_number(1500000), "1.5M")
  expect_equal(abbreviate_number(1500000000), "1.5B")
})

# ============================================================================
# 10. CUSTOM THEME TESTS
# ============================================================================

test_that("Custom Theme - Color Palette", {
  
  # Test Atlas color palette
  atlas_colors <- get_atlas_colors()
  
  expect_true(is.character(atlas_colors))
  expect_true(length(atlas_colors) > 0)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", atlas_colors)))
  
  # Test color accessibility
  expect_true(check_color_contrast(atlas_colors[1], "#FFFFFF"))
  expect_true(check_color_contrast(atlas_colors[1], "#000000"))
})

test_that("Custom Theme - ggplot2 Theme", {
  
  # Test custom ggplot2 theme
  custom_theme <- theme_atlas()
  
  expect_true(inherits(custom_theme, "theme"))
  expect_true(inherits(custom_theme, "gg"))
  
  # Test theme components
  theme_elements <- custom_theme$axis.text$colour
  expect_true(!is.null(theme_elements))
})

test_that("Custom Theme - Plotly Theme", {
  
  # Test Plotly theme configuration
  plotly_config <- get_plotly_config()
  
  expect_true(is.list(plotly_config))
  expect_true("displayModeBar" %in% names(plotly_config))
  expect_true("responsive" %in% names(plotly_config))
})

# ============================================================================
# 11. SIDEBAR MODULE TESTS
# ============================================================================

test_that("Sidebar Module - Filter Generation", {
  
  # Create sample data for filter generation
  filter_data <- data.frame(
    Department = c("IT", "HR", "Finance", "IT", "HR"),
    Gender = c("Male", "Female", "Male", "Female", "Male"),
    Age = c(25, 30, 35, 28, 42),
    stringsAsFactors = FALSE
  )
  
  # Test filter options generation
  filter_options <- generate_filter_options(filter_data)
  
  expect_true(is.list(filter_options))
  expect_true("Department" %in% names(filter_options))
  expect_true("Gender" %in% names(filter_options))
  expect_equal(length(filter_options$Department), 3)
  expect_equal(length(filter_options$Gender), 2)
})

test_that("Sidebar Module - Filter Application", {
  
  # Create sample data
  test_data <- data.frame(
    EmployeeID = 1:10,
    Department = rep(c("IT", "HR"), each = 5),
    Gender = rep(c("Male", "Female"), 5),
    Age = sample(25:45, 10),
    stringsAsFactors = FALSE
  )
  
  # Test filter application
  filters <- list(
    Department = c("IT"),
    Gender = c("Male", "Female")
  )
  
  filtered_data <- apply_filters(test_data, filters)
  
  expect_equal(nrow(filtered_data), 5)
  expect_true(all(filtered_data$Department == "IT"))
})

# ============================================================================
# 12. FOOTER MODULE TESTS
# ============================================================================

test_that("Footer Module - Credit Information", {
  
  # Test footer content generation
  footer_content <- generate_footer_content()
  
  expect_true(is.character(footer_content))
  expect_true(grepl("akhapwoyaco", footer_content))
  expect_true(grepl("github", footer_content, ignore.case = TRUE))
  expect_true(grepl("herdataproject", footer_content))
})

test_that("Footer Module - Version Information", {
  
  # Test version display
  version_info <- get_app_version()
  
  expect_true(is.character(version_info))
  expect_true(grepl("\\d+\\.\\d+\\.\\d+", version_info))
})

# ============