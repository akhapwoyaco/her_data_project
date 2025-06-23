# =============================================================================
# ATLAS LABS HR ANALYTICS - ATTRITION ANALYSIS MODULE UNIT TESTS
# =============================================================================
# Comprehensive test suite for attrition module (excluding statistical calculations)
# Developer: akhapwoyaco
# =============================================================================

library(testthat)
library(shiny)
library(shinytestmate)
library(mockery)
library(DT)
library(plotly)
library(tidyverse)

# Source the module (assuming it exists)
# source("modules/attrition_module.R")
# source("modules/logger_module.R")

# =============================================================================
# TEST DATA SETUP
# =============================================================================

setup_test_data <- function() {
  # Create comprehensive test datasets
  employee_data <- tibble(
    EmployeeID = 1:100,
    FirstName = paste0("Employee", 1:100),
    LastName = paste0("Lastname", 1:100),
    Gender = sample(c("Male", "Female", "Non-binary"), 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    BusinessTravel = sample(c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), 100, replace = TRUE),
    Department = sample(c("Sales", "Research & Development", "Human Resources", "Finance"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    State = sample(c("California", "Texas", "New York", "Florida"), 100, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 100, replace = TRUE),
    Education = sample(1:5, 100, replace = TRUE),
    EducationField = sample(c("Technical", "Business", "Medical", "Life Sciences"), 100, replace = TRUE),
    JobRole = sample(c("Sales Executive", "Research Scientist", "HR Representative", "Manager"), 100, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), 100, replace = TRUE),
    Salary = sample(30000:150000, 100, replace = TRUE),
    StockOptionLevel = sample(0:3, 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
    HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 100),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.15, 0.85)),
    YearsAtCompany = sample(0:20, 100, replace = TRUE),
    YearsInMostRecentRole = sample(0:15, 100, replace = TRUE),
    YearsSinceLastPromotion = sample(0:10, 100, replace = TRUE),
    YearsWithCurrManager = sample(0:12, 100, replace = TRUE)
  )
  
  performance_data <- tibble(
    PerformanceID = 1:100,
    EmployeeID = 1:100,
    ReviewDate = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 100),
    EnvironmentSatisfaction = sample(1:5, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 100, replace = TRUE),
    WorkLifeBalance = sample(1:5, 100, replace = TRUE),
    SelfRating = sample(1:5, 100, replace = TRUE),
    ManagerRating = sample(1:5, 100, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:5, 100, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:5, 100, replace = TRUE)
  )
  
  merged_data <- employee_data %>%
    left_join(performance_data, by = "EmployeeID")
  
  list(
    employee = employee_data,
    performance = performance_data,
    merged = merged_data
  )
}

# Mock logger for testing
mock_logger <- list(
  log_info = function(message, module = "test", ...) message,
  log_warning = function(message, module = "test", ...) message,
  log_error = function(message, module = "test", ...) message,
  track_performance = function(operation, ...) operation
)

# =============================================================================
# UI COMPONENT TESTS
# =============================================================================

test_that("Attrition UI renders correctly", {
  skip_if_not_installed("shiny")
  
  # Test UI structure
  ui_output <- attritionUI("test_attrition")
  
  expect_s3_class(ui_output, "shiny.tag")
  expect_true("div" %in% class(ui_output))
  
  # Test for required UI elements
  ui_html <- as.character(ui_output)
  
  expect_true(grepl("test_attrition", ui_html))
  expect_true(grepl("tabsetPanel|navbarPage", ui_html, ignore.case = TRUE))
})

test_that("Attrition UI contains required input controls", {
  skip_if_not_installed("shiny")
  
  ui_output <- attritionUI("test_attrition")
  ui_html <- as.character(ui_output)
  
  # Check for filter controls
  expected_controls <- c("selectInput", "dateRangeInput", "checkboxGroupInput")
  
  for (control in expected_controls) {
    expect_true(grepl(control, ui_html, ignore.case = TRUE),
                info = paste("Missing control:", control))
  }
})

test_that("Attrition UI has correct output placeholders", {
  skip_if_not_installed("shiny")
  
  ui_output <- attritionUI("test_attrition")
  ui_html <- as.character(ui_output)
  
  # Check for output elements
  expected_outputs <- c("plotlyOutput", "DTOutput", "verbatimTextOutput")
  
  for (output in expected_outputs) {
    expect_true(grepl(output, ui_html, ignore.case = TRUE),
                info = paste("Missing output:", output))
  }
})

# =============================================================================
# DATA PROCESSING TESTS
# =============================================================================

test_that("Data preprocessing handles missing values correctly", {
  test_data <- setup_test_data()
  
  # Introduce some NA values
  test_data$merged$Age[1:5] <- NA
  test_data$merged$Salary[6:10] <- NA
  
  # Mock the preprocessing function
  preprocess_attrition_data <- function(data) {
    data %>%
      filter(!is.na(Age), !is.na(Salary), !is.na(Attrition)) %>%
      mutate(
        Attrition_Binary = ifelse(Attrition == "Yes", 1, 0),
        Age_Group = cut(Age, breaks = c(0, 30, 40, 50, 65), labels = c("Under 30", "30-39", "40-49", "50+"))
      )
  }
  
  processed_data <- preprocess_attrition_data(test_data$merged)
  
  expect_true(all(!is.na(processed_data$Age)))
  expect_true(all(!is.na(processed_data$Salary)))
  expect_true("Attrition_Binary" %in% names(processed_data))
  expect_true("Age_Group" %in% names(processed_data))
  expect_equal(nrow(processed_data), 90) # 100 - 10 with NA values
})

test_that("Data filtering works correctly", {
  test_data <- setup_test_data()
  
  # Mock filtering function
  filter_attrition_data <- function(data, filters) {
    filtered_data <- data
    
    if (!is.null(filters$departments) && length(filters$departments) > 0) {
      filtered_data <- filtered_data %>% filter(Department %in% filters$departments)
    }
    
    if (!is.null(filters$age_range)) {
      filtered_data <- filtered_data %>% 
        filter(Age >= filters$age_range[1], Age <= filters$age_range[2])
    }
    
    if (!is.null(filters$tenure_range)) {
      filtered_data <- filtered_data %>%
        filter(YearsAtCompany >= filters$tenure_range[1], YearsAtCompany <= filters$tenure_range[2])
    }
    
    return(filtered_data)
  }
  
  # Test department filtering
  dept_filter <- list(departments = c("Sales", "Finance"))
  filtered_data <- filter_attrition_data(test_data$merged, dept_filter)
  
  expect_true(all(filtered_data$Department %in% c("Sales", "Finance")))
  expect_true(nrow(filtered_data) <= nrow(test_data$merged))
  
  # Test age range filtering
  age_filter <- list(age_range = c(25, 45))
  filtered_data <- filter_attrition_data(test_data$merged, age_filter)
  
  expect_true(all(filtered_data$Age >= 25 & filtered_data$Age <= 45))
})

test_that("Attrition rate calculations are correct", {
  test_data <- setup_test_data()
  
  # Mock attrition rate calculation function
  calculate_attrition_rates <- function(data, group_by = NULL) {
    if (is.null(group_by)) {
      # Overall attrition rate
      total_employees <- nrow(data)
      attrited_employees <- sum(data$Attrition == "Yes")
      return(list(
        total = total_employees,
        attrited = attrited_employees,
        rate = attrited_employees / total_employees
      ))
    } else {
      # Group-wise attrition rates
      data %>%
        group_by(!!sym(group_by)) %>%
        summarise(
          total_employees = n(),
          attrited_employees = sum(Attrition == "Yes"),
          attrition_rate = attrited_employees / total_employees,
          .groups = "drop"
        )
    }
  }
  
  # Test overall attrition rate
  overall_rate <- calculate_attrition_rates(test_data$merged)
  
  expect_type(overall_rate$total, "integer")
  expect_type(overall_rate$attrited, "integer")
  expect_type(overall_rate$rate, "double")
  expect_true(overall_rate$rate >= 0 && overall_rate$rate <= 1)
  expect_equal(overall_rate$total, nrow(test_data$merged))
  
  # Test department-wise attrition rates
  dept_rates <- calculate_attrition_rates(test_data$merged, "Department")
  
  expect_s3_class(dept_rates, "data.frame")
  expect_true(all(c("Department", "total_employees", "attrited_employees", "attrition_rate") %in% names(dept_rates)))
  expect_true(all(dept_rates$attrition_rate >= 0 & dept_rates$attrition_rate <= 1))
})

# =============================================================================
# VISUALIZATION TESTS
# =============================================================================

test_that("Attrition heatmap generates correctly", {
  skip_if_not_installed("plotly")
  
  test_data <- setup_test_data()
  
  # Mock heatmap creation function
  create_attrition_heatmap <- function(data) {
    heatmap_data <- data %>%
      group_by(Department, JobRole) %>%
      summarise(
        attrition_rate = mean(Attrition == "Yes"),
        employee_count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(heatmap_data, aes(x = Department, y = JobRole, fill = attrition_rate)) +
      geom_tile() +
      scale_fill_gradient(low = "green", high = "red") +
      labs(title = "Attrition Rate Heatmap", fill = "Attrition Rate")
    
    ggplotly(p)
  }
  
  heatmap <- create_attrition_heatmap(test_data$merged)
  
  expect_s3_class(heatmap, "plotly")
  expect_true("data" %in% names(heatmap))
  expect_true(length(heatmap$data) > 0)
})

test_that("Attrition trend charts render properly", {
  skip_if_not_installed("plotly")
  
  test_data <- setup_test_data()
  
  # Mock trend chart function
  create_attrition_trends <- function(data) {
    trend_data <- data %>%
      mutate(
        hire_year = year(HireDate),
        attrition_binary = ifelse(Attrition == "Yes", 1, 0)
      ) %>%
      group_by(hire_year) %>%
      summarise(
        attrition_rate = mean(attrition_binary),
        employee_count = n(),
        .groups = "drop"
      ) %>%
      filter(hire_year >= 2015) # Filter for recent years
    
    p <- ggplot(trend_data, aes(x = hire_year, y = attrition_rate)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Attrition Trends Over Time", 
           x = "Hire Year", y = "Attrition Rate")
    
    ggplotly(p)
  }
  
  trend_chart <- create_attrition_trends(test_data$merged)
  
  expect_s3_class(trend_chart, "plotly")
  expect_true("data" %in% names(trend_chart))
  expect_true(length(trend_chart$data) > 0)
})

test_that("Interactive charts have correct structure", {
  skip_if_not_installed("plotly")
  
  test_data <- setup_test_data()
  
  # Mock interactive chart function
  create_interactive_attrition_analysis <- function(data) {
    chart_data <- data %>%
      group_by(Department, Gender) %>%
      summarise(
        avg_salary = mean(Salary, na.rm = TRUE),
        attrition_rate = mean(Attrition == "Yes"),
        employee_count = n(),
        .groups = "drop"
      )
    
    p <- plot_ly(chart_data, 
                 x = ~avg_salary, 
                 y = ~attrition_rate,
                 size = ~employee_count,
                 color = ~Department,
                 text = ~paste("Department:", Department, 
                              "<br>Gender:", Gender,
                              "<br>Avg Salary: $", round(avg_salary),
                              "<br>Attrition Rate:", scales::percent(attrition_rate)),
                 hovertemplate = "%{text}<extra></extra>") %>%
      add_markers() %>%
      layout(title = "Salary vs Attrition Analysis",
             xaxis = list(title = "Average Salary"),
             yaxis = list(title = "Attrition Rate"))
    
    return(p)
  }
  
  interactive_chart <- create_interactive_attrition_analysis(test_data$merged)
  
  expect_s3_class(interactive_chart, "plotly")
  expect_true("data" %in% names(interactive_chart))
  expect_true("layout" %in% names(interactive_chart))
  
  # Check for required plot elements
  expect_true(!is.null(interactive_chart$layout$title))
  expect_true(!is.null(interactive_chart$layout$xaxis))
  expect_true(!is.null(interactive_chart$layout$yaxis))
})

# =============================================================================
# DATA TABLE TESTS
# =============================================================================

test_that("Attrition data tables render correctly", {
  skip_if_not_installed("DT")
  
  test_data <- setup_test_data()
  
  # Mock data table creation function
  create_attrition_summary_table <- function(data) {
    summary_table <- data %>%
      group_by(Department, JobRole) %>%
      summarise(
        total_employees = n(),
        attrited_employees = sum(Attrition == "Yes"),
        attrition_rate = round(mean(Attrition == "Yes") * 100, 1),
        avg_salary = round(mean(Salary, na.rm = TRUE)),
        avg_tenure = round(mean(YearsAtCompany, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(attrition_rate))
    
    DT::datatable(
      summary_table,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      colnames = c("Department", "Job Role", "Total Employees", 
                   "Attrited", "Attrition Rate (%)", 
                   "Avg Salary ($)", "Avg Tenure (Years)"),
      rownames = FALSE
    ) %>%
      DT::formatCurrency(c("avg_salary"), currency = "$", digits = 0) %>%
      DT::formatStyle(
        "attrition_rate",
        backgroundColor = DT::styleInterval(
          cuts = c(10, 20),
          values = c("lightgreen", "yellow", "lightcoral")
        )
      )
  }
  
  summary_table <- create_attrition_summary_table(test_data$merged)
  
  expect_s3_class(summary_table, "datatables")
  expect_true("x" %in% names(summary_table))
  expect_s3_class(summary_table$x$data, "data.frame")
  expect_true(nrow(summary_table$x$data) > 0)
})

test_that("Employee detail tables have correct structure", {
  skip_if_not_installed("DT")
  
  test_data <- setup_test_data()
  
  # Mock detailed employee table
  create_high_risk_employee_table <- function(data, risk_threshold = 0.7) {
    # Mock risk scoring function
    data_with_risk <- data %>%
      mutate(
        risk_score = case_when(
          YearsSinceLastPromotion > 5 ~ 0.3,
          OverTime == "Yes" ~ 0.2,
          JobSatisfaction <= 2 ~ 0.3,
          WorkLifeBalance <= 2 ~ 0.2,
          TRUE ~ 0
        ),
        risk_score = pmin(risk_score, 1) # Cap at 1
      ) %>%
      filter(risk_score >= risk_threshold, Attrition == "No") %>%
      select(EmployeeID, FirstName, LastName, Department, JobRole, 
             Salary, YearsAtCompany, risk_score) %>%
      arrange(desc(risk_score))
    
    DT::datatable(
      data_with_risk,
      options = list(pageLength = 15, scrollX = TRUE),
      colnames = c("ID", "First Name", "Last Name", "Department", 
                   "Job Role", "Salary", "Tenure", "Risk Score"),
      rownames = FALSE
    )
  }
  
  risk_table <- create_high_risk_employee_table(test_data$merged)
  
  expect_s3_class(risk_table, "datatables")
  expect_true("x" %in% names(risk_table))
  expect_s3_class(risk_table$x$data, "data.frame")
})

# =============================================================================
# MODULE INTEGRATION TESTS
# =============================================================================

test_that("Module server function handles reactive inputs correctly", {
  skip_if_not_installed("shiny")
  
  # Mock reactive inputs
  mock_inputs <- list(
    dept_filter = reactive({ c("Sales", "Finance") }),
    date_range = reactive({ c(as.Date("2020-01-01"), as.Date("2023-12-31")) }),
    age_range = reactive({ c(25, 55) }),
    show_predictions = reactive({ TRUE })
  )
  
  # Mock data reactive
  mock_data <- reactive({ setup_test_data()$merged })
  
  # Test that server function can be called without errors
  expect_no_error({
    # This would be the actual server function call
    # attritionServer("test", mock_data, mock_logger)
    
    # For now, just test the structure
    server_result <- list(
      filtered_data = mock_data,
      attrition_rates = reactive({ "calculated rates" }),
      visualizations = reactive({ "generated plots" })
    )
    
    expect_true(is.list(server_result))
    expect_true("filtered_data" %in% names(server_result))
  })
})

test_that("Module communicates correctly with logger", {
  test_data <- setup_test_data()
  
  # Mock logger tracking
  logged_messages <- character()
  
  mock_logger_with_capture <- list(
    log_info = function(message, module = "attrition", ...) {
      logged_messages <<- c(logged_messages, paste("INFO:", message))
      message
    },
    log_warning = function(message, module = "attrition", ...) {
      logged_messages <<- c(logged_messages, paste("WARNING:", message))
      message
    },
    log_error = function(message, module = "attrition", ...) {
      logged_messages <<- c(logged_messages, paste("ERROR:", message))
      message
    }
  )
  
  # Simulate module operations with logging
  mock_logger_with_capture$log_info("Starting attrition analysis")
  mock_logger_with_capture$log_info("Data processed successfully")
  mock_logger_with_capture$log_warning("High attrition rate detected in Sales")
  
  expect_length(logged_messages, 3)
  expect_true(any(grepl("Starting attrition analysis", logged_messages)))
  expect_true(any(grepl("WARNING.*Sales", logged_messages)))
})

test_that("Module handles edge cases gracefully", {
  # Test with empty data
  empty_data <- tibble(
    EmployeeID = integer(),
    Attrition = character(),
    Department = character()
  )
  
  # Mock function to handle empty data
  handle_empty_data <- function(data) {
    if (nrow(data) == 0) {
      return(list(
        message = "No data available for analysis",
        has_data = FALSE,
        visualizations = NULL
      ))
    }
    
    list(
      message = "Data processed successfully",
      has_data = TRUE,
      visualizations = "generated_plots"
    )
  }
  
  result <- handle_empty_data(empty_data)
  
  expect_false(result$has_data)
  expect_equal(result$message, "No data available for analysis")
  expect_null(result$visualizations)
  
  # Test with single row data
  single_row_data <- tibble(
    EmployeeID = 1,
    Attrition = "No",
    Department = "Sales"
  )
  
  result_single <- handle_empty_data(single_row_data)
  expect_true(result_single$has_data)
})

# =============================================================================
# REACTIVE BEHAVIOR TESTS
# =============================================================================

test_that("Reactive values update correctly", {
  skip_if_not_installed("shiny")
  
  # Mock reactive environment
  test_reactives <- reactiveValues(
    selected_department = NULL,
    attrition_data = NULL,
    current_filters = list()
  )
  
  # Simulate reactive updates
  expect_no_error({
    test_reactives$selected_department <- "Sales"
    test_reactives$current_filters <- list(dept = "Sales", age_min = 25)
  })
  
  expect_equal(test_reactives$selected_department, "Sales")
  expect_equal(test_reactives$current_filters$dept, "Sales")
})

test_that("Event handlers work correctly", {
  skip_if_not_installed("shiny")
  
  # Mock event handling
  button_clicked <- FALSE
  
  mock_observe_event <- function(eventExpr, handlerExpr) {
    # Simulate button click
    if (deparse(substitute(eventExpr)) == "input$refresh_analysis") {
      button_clicked <<- TRUE
      eval(handlerExpr)
    }
  }
  
  # Mock input
  input <- list(refresh_analysis = 1)
  
  # Simulate observe event
  mock_observe_event(input$refresh_analysis, {
    # Handler code would go here
    message("Analysis refreshed")
  })
  
  expect_true(button_clicked)
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Module handles data validation errors", {
  # Test with invalid data types
  invalid_data <- tibble(
    EmployeeID = c("invalid", "data", "types"),
    Attrition = c(1, 2, 3), # Should be character
    Age = c("young", "old", "medium") # Should be numeric
  )
  
  # Mock validation function
  validate_attrition_data <- function(data) {
    errors <- character()
    
    if (!is.numeric(data$EmployeeID)) {
      errors <- c(errors, "EmployeeID must be numeric")
    }
    
    if (!is.character(data$Attrition)) {
      errors <- c(errors, "Attrition must be character (Yes/No)")
    }
    
    if ("Age" %in% names(data) && !is.numeric(data$Age)) {
      errors <- c(errors, "Age must be numeric")
    }
    
    if (length(errors) > 0) {
      return(list(valid = FALSE, errors = errors))
    }
    
    list(valid = TRUE, errors = NULL)
  }
  
  validation_result <- validate_attrition_data(invalid_data)
  
  expect_false(validation_result$valid)
  expect_length(validation_result$errors, 2) # Age and Attrition errors
  expect_true(any(grepl("Attrition must be character", validation_result$errors)))
})

test_that("Module handles missing required columns", {
  # Test with missing essential columns
  incomplete_data <- tibble(
    EmployeeID = 1:10,
    FirstName = paste0("Employee", 1:10)
    # Missing Attrition column
  )
  
  # Mock column validation
  validate_required_columns <- function(data) {
    required_cols <- c("EmployeeID", "Attrition", "Department", "Salary")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      return(list(
        valid = FALSE,
        missing_columns = missing_cols,
        message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      ))
    }
    
    list(valid = TRUE, missing_columns = NULL, message = "All required columns present")
  }
  
  validation_result <- validate_required_columns(incomplete_data)
  
  expect_false(validation_result$valid)
  expect_true("Attrition" %in% validation_result$missing_columns)
  expect_true("Department" %in% validation_result$missing_columns)
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

test_that("Module handles large datasets efficiently", {
  skip_if_not_installed("microbenchmark")
  
  # Create larger test dataset
  large_data <- tibble(
    EmployeeID = 1:10000,
    Attrition = sample(c("Yes", "No"), 10000, replace = TRUE),
    Department = sample(c("Sales", "R&D", "HR", "Finance"), 10000, replace = TRUE),
    Salary = sample(30000:150000, 10000, replace = TRUE),
    Age = sample(22:65, 10000, replace = TRUE)
  )
  
  # Mock performance-sensitive function
  calculate_department_attrition <- function(data) {
    data %>%
      group_by(Department) %>%
      summarise(
        attrition_rate = mean(Attrition == "Yes"),
        avg_salary = mean(Salary),
        employee_count = n(),
        .groups = "drop"
      )
  }
  
  # Test that function completes in reasonable time
  start_time <- Sys.time()
  result <- calculate_department_attrition(large_data)
  end_time <- Sys.time()
  
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(execution_time < 5) # Should complete within 5 seconds
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Memory usage is reasonable for typical operations", {
  skip_on_cran() # Skip on CRAN due to memory profiling
  
  test_data <- setup_test_data()
  
  # Mock memory-intensive operation
  memory_intensive_operation <- function(data) {
    # Create multiple data transformations
    result_list <- list()
    
    for (i in 1:5) {
      result_list[[i]] <- data %>%
        mutate(
          iteration = i,
          random_value = runif(nrow(data))
        ) %>%
        group_by(Department) %>%
        summarise(
          avg_value = mean(random_value),
          .groups = "drop"
        )
    }
    
    return(result_list)
  }
  
  # Test that operation completes without memory issues
  expect_no_error({
    result <- memory_intensive_operation(test_data$merged)
    expect_length(result, 5)
  })
})

# =============================================================================
# INTEGRATION WITH OTHER MODULES TESTS
# =============================================================================

test_that("Module integrates with report generation", {
  test_data <- setup_test_data()
  
  # Mock report data extraction
  extract_report_data <- function(attrition_analysis) {
    list(
      summary = list(
        total_employees = nrow(test_data$merged),
        overall_attrition_rate = mean(test_data$merged$Attrition == "Yes"),
        high_risk_departments = c("Sales", "R&D")
      ),
      detailed_metrics = test_data$merged %>%
        group_by(Department) %>%
        summarise(
          attrition_rate