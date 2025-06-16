# ============================================================================
# utils.R - Atlas Labs HR Analytics Dashboard
# Utility Functions and Helpers
# 
# Author: akhapwoyaco
# GitHub: https://github.com/akhapwoyaco
# Data Source: https://herdataproject.gumroad.com/l/hr-analytics-tableau
# ============================================================================

# Data Validation Utilities ----

#' Validate numeric columns for outliers and missing values
#' @param data Data frame to validate
#' @param numeric_cols Character vector of numeric column names
#' @return List with validation results
validate_numeric_data <- function(data, numeric_cols) {
  results <- list()
  
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      col_data <- data[[col]]
      results[[col]] <- list(
        missing_count = sum(is.na(col_data)),
        missing_pct = round(sum(is.na(col_data)) / length(col_data) * 100, 2),
        outliers = length(boxplot.stats(col_data, na.rm = TRUE)$out),
        range = range(col_data, na.rm = TRUE)
      )
    }
  }
  results
}

#' Check data integrity across related tables
#' @param employee_data Employee master data
#' @param performance_data Performance ratings data
#' @param education_data Education levels data
#' @return List with integrity check results
check_data_integrity <- function(employee_data, performance_data, education_data) {
  list(
    employee_ids_match = all(performance_data$EmployeeID %in% employee_data$EmployeeID),
    education_ids_match = all(employee_data$Education %in% education_data$EducationLevelID),
    duplicate_employees = sum(duplicated(employee_data$EmployeeID)),
    duplicate_performance = sum(duplicated(performance_data$PerformanceID)),
    data_completeness = round(sum(complete.cases(employee_data)) / nrow(employee_data) * 100, 2)
  )
}

# Data Processing Utilities ----

#' Calculate age groups for demographic analysis
#' @param ages Numeric vector of ages
#' @return Factor vector of age groups
create_age_groups <- function(ages) {
  breaks <- c(0, 25, 35, 45, 55, 100)
  labels <- c("Under 25", "25-34", "35-44", "45-54", "55+")
  cut(ages, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
}

#' Calculate tenure groups for analysis
#' @param years Numeric vector of years
#' @return Factor vector of tenure groups
create_tenure_groups <- function(years) {
  breaks <- c(0, 1, 3, 5, 10, 100)
  labels <- c("< 1 year", "1-2 years", "3-4 years", "5-9 years", "10+ years")
  cut(years, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
}

#' Calculate salary bands for compensation analysis
#' @param salaries Numeric vector of salaries
#' @return Factor vector of salary bands
create_salary_bands <- function(salaries) {
  quantiles <- quantile(salaries, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  labels <- c("Q1 (Bottom 25%)", "Q2 (25-50%)", "Q3 (50-75%)", "Q4 (Top 25%)")
  cut(salaries, breaks = quantiles, labels = labels, include.lowest = TRUE)
}

#' Standardize satisfaction ratings for comparison
#' @param rating Numeric vector of ratings
#' @param scale_max Maximum value of the rating scale
#' @return Numeric vector of standardized ratings (0-100)
standardize_rating <- function(rating, scale_max = 5) {
  (rating / scale_max) * 100
}

# Statistical Analysis Utilities ----

#' Calculate attrition rate with confidence intervals
#' @param data Data frame with Attrition column
#' @param group_var Optional grouping variable
#' @return Data frame with attrition statistics
calculate_attrition_rate <- function(data, group_var = NULL) {
  if (is.null(group_var)) {
    # Overall attrition rate
    attrition_count <- sum(data$Attrition == "Yes", na.rm = TRUE)
    total_count <- nrow(data)
    rate <- attrition_count / total_count
    
    # Wilson confidence interval for proportion
    z <- qnorm(0.975)  # 95% CI
    p <- rate
    n <- total_count
    ci_lower <- (p + z^2/(2*n) - z*sqrt((p*(1-p) + z^2/(4*n))/n)) / (1 + z^2/n)
    ci_upper <- (p + z^2/(2*n) + z*sqrt((p*(1-p) + z^2/(4*n))/n)) / (1 + z^2/n)
    
    data.frame(
      Group = "Overall",
      AttritionCount = attrition_count,
      TotalCount = total_count,
      AttritionRate = round(rate * 100, 2),
      CI_Lower = round(ci_lower * 100, 2),
      CI_Upper = round(ci_upper * 100, 2)
    )
  } else {
    # Grouped attrition rate
    data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        AttritionCount = sum(Attrition == "Yes", na.rm = TRUE),
        TotalCount = n(),
        AttritionRate = round((AttritionCount / TotalCount) * 100, 2),
        .groups = "drop"
      ) %>%
      rename(Group = !!sym(group_var))
  }
}

#' Calculate correlation matrix for satisfaction metrics
#' @param data Data frame with satisfaction columns
#' @param satisfaction_cols Character vector of satisfaction column names
#' @return Correlation matrix
calculate_satisfaction_correlations <- function(data, satisfaction_cols) {
  satisfaction_data <- data %>%
    select(all_of(satisfaction_cols)) %>%
    filter(complete.cases(.))
  
  if (nrow(satisfaction_data) > 0) {
    cor(satisfaction_data, use = "complete.obs", method = "pearson")
  } else {
    matrix(NA, nrow = length(satisfaction_cols), ncol = length(satisfaction_cols),
           dimnames = list(satisfaction_cols, satisfaction_cols))
  }
}

#' Perform pay equity analysis
#' @param data Data frame with salary and demographic data
#' @param demographic_var Demographic variable for comparison
#' @return List with pay equity statistics
analyze_pay_equity <- function(data, demographic_var) {
  equity_stats <- data %>%
    filter(!is.na(!!sym(demographic_var)), !is.na(Salary)) %>%
    group_by(!!sym(demographic_var)) %>%
    summarise(
      Count = n(),
      MedianSalary = median(Salary, na.rm = TRUE),
      MeanSalary = mean(Salary, na.rm = TRUE),
      SalarySD = sd(Salary, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate pay gaps relative to highest-paid group
  if (nrow(equity_stats) > 1) {
    max_median <- max(equity_stats$MedianSalary, na.rm = TRUE)
    equity_stats$PayGap <- round((max_median - equity_stats$MedianSalary) / max_median * 100, 2)
  } else {
    equity_stats$PayGap <- 0
  }
  
  equity_stats
}

# Visualization Utilities ----

#' Generate color palette for Atlas Labs branding
#' @param n Number of colors needed
#' @param type Type of palette ("sequential", "diverging", "qualitative")
#' @return Character vector of hex colors
atlas_color_palette <- function(n, type = "qualitative") {
  # Atlas Labs brand colors
  primary_colors <- c("#2C3E50", "#3498DB", "#E74C3C", "#F39C12", "#27AE60", "#9B59B6")
  
  if (type == "sequential") {
    colorRampPalette(c("#ECF0F1", "#2C3E50"))(n)
  } else if (type == "diverging") {
    colorRampPalette(c("#E74C3C", "#ECF0F1", "#3498DB"))(n)
  } else {
    # Qualitative - cycle through brand colors
    rep_len(primary_colors, n)
  }
}

#' Format numbers for display in UI
#' @param x Numeric vector
#' @param type Format type ("currency", "percent", "number", "decimal")
#' @return Character vector of formatted numbers
format_number <- function(x, type = "number") {
  switch(type,
    "currency" = paste0("$", scales::comma(x, accuracy = 1)),
    "percent" = scales::percent(x, accuracy = 0.1),
    "decimal" = scales::number(x, accuracy = 0.01),
    "number" = scales::comma(x, accuracy = 1),
    scales::comma(x)
  )
}

#' Create value box data for KPI displays
#' @param value Numeric value to display
#' @param title Title for the value box
#' @param subtitle Optional subtitle
#' @param icon Optional icon name
#' @param color Color for the value box
#' @param format_type Type of number formatting
#' @return List with value box parameters
create_value_box_data <- function(value, title, subtitle = NULL, icon = NULL, 
                                 color = "#3498DB", format_type = "number") {
  list(
    value = format_number(value, format_type),
    title = title,
    subtitle = subtitle,
    icon = icon,
    color = color
  )
}

# Performance Utilities ----

#' Track function execution time
#' @param expr Expression to time
#' @param description Description of the operation
#' @return Result of expression with timing info
track_performance <- function(expr, description = "Operation") {
  start_time <- Sys.time()
  result <- force(expr)
  end_time <- Sys.time()
  
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  attr(result, "performance") <- list(
    description = description,
    duration_seconds = duration,
    start_time = start_time,
    end_time = end_time
  )
  
  result
}

#' Get memory usage information
#' @return Named list with memory statistics
get_memory_usage <- function() {
  gc_info <- gc(verbose = FALSE)
  list(
    used_mb = sum(gc_info[, "used"]),
    max_used_mb = sum(gc_info[, "max used"]),
    available_mb = sum(gc_info[, "available"]) %||% NA
  )
}

#' Optimize data frame for memory efficiency
#' @param data Data frame to optimize
#' @return Optimized data frame
optimize_dataframe <- function(data) {
  # Convert character columns with few unique values to factors
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      unique_count <- length(unique(data[[col]]))
      total_count <- nrow(data)
      
      # Convert to factor if less than 10% unique values
      if (unique_count / total_count < 0.1 && unique_count > 1) {
        data[[col]] <- as.factor(data[[col]])
      }
    }
  }
  
  data
}

# Error Handling Utilities ----

#' Safely execute expressions with error handling
#' @param expr Expression to execute
#' @param default Default value if expression fails
#' @param error_message Custom error message
#' @return Result of expression or default value
safe_execute <- function(expr, default = NULL, error_message = "Operation failed") {
  tryCatch({
    expr
  }, error = function(e) {
    warning(paste(error_message, ":", e$message))
    default
  })
}

#' Validate required columns exist in data frame
#' @param data Data frame to check
#' @param required_cols Character vector of required column names
#' @return Logical indicating if all columns exist
validate_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }
  TRUE
}

# Date/Time Utilities ----

#' Convert date strings to Date objects with multiple format attempts
#' @param date_strings Character vector of date strings
#' @return Date vector
parse_dates_flexible <- function(date_strings) {
  formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y")
  
  result <- rep(as.Date(NA), length(date_strings))
  
  for (fmt in formats) {
    if (any(is.na(result))) {
      parsed <- as.Date(date_strings, format = fmt)
      result[is.na(result) & !is.na(parsed)] <- parsed[is.na(result) & !is.na(parsed)]
    }
  }
  
  result
}

#' Calculate business days between dates
#' @param start_date Start date
#' @param end_date End date
#' @return Numeric vector of business days
calculate_business_days <- function(start_date, end_date) {
  total_days <- as.numeric(end_date - start_date)
  weeks <- floor(total_days / 7)
  remaining_days <- total_days %% 7
  
  # Approximate business days (5 days per week)
  business_days <- weeks * 5 + pmin(remaining_days, 5)
  pmax(business_days, 0)
}

# Filter Utilities ----

#' Apply multiple filters to data frame
#' @param data Data frame to filter
#' @param filters Named list of filter values
#' @return Filtered data frame
apply_filters <- function(data, filters) {
  filtered_data <- data
  
  for (filter_name in names(filters)) {
    filter_values <- filters[[filter_name]]
    
    if (!is.null(filter_values) && length(filter_values) > 0 && filter_name %in% names(filtered_data)) {
      if (is.numeric(filter_values) && length(filter_values) == 2) {
        # Range filter for numeric values
        filtered_data <- filtered_data %>%
          filter(!!sym(filter_name) >= filter_values[1] & !!sym(filter_name) <= filter_values[2])
      } else {
        # Categorical filter
        filtered_data <- filtered_data %>%
          filter(!!sym(filter_name) %in% filter_values)
      }
    }
  }
  
  filtered_data
}

#' Get unique filter options for categorical variables
#' @param data Data frame
#' @param column_name Column to get options for
#' @return Character vector of unique values
get_filter_options <- function(data, column_name) {
  if (column_name %in% names(data)) {
    unique_vals <- unique(data[[column_name]])
    unique_vals[!is.na(unique_vals)]
  } else {
    character(0)
  }
}

# Export Utilities ----

#' Prepare data for export with proper formatting
#' @param data Data frame to export
#' @param format_dates Logical, whether to format date columns
#' @return Data frame ready for export
prepare_export_data <- function(data, format_dates = TRUE) {
  export_data <- data
  
  # Format date columns
  if (format_dates) {
    date_cols <- sapply(export_data, function(x) inherits(x, "Date"))
    export_data[date_cols] <- lapply(export_data[date_cols], format, "%Y-%m-%d")
  }
  
  # Convert factors to character for better export compatibility
  factor_cols <- sapply(export_data, is.factor)
  export_data[factor_cols] <- lapply(export_data[factor_cols], as.character)
  
  export_data
}

# Module Communication Utilities ----

#' Create reactive trigger for module communication
#' @return Reactive trigger function
create_module_trigger <- function() {
  trigger_val <- reactiveVal(0)
  
  list(
    trigger = reactive({ trigger_val() }),
    fire = function() { trigger_val(trigger_val() + 1) }
  )
}

#' Validate shared reactive values structure
#' @param shared_values ReactiveValues object
#' @param required_keys Character vector of required keys
#' @return Logical indicating validity
validate_shared_values <- function(shared_values, required_keys) {
  current_keys <- names(reactiveValuesToList(shared_values))
  missing_keys <- setdiff(required_keys, current_keys)
  
  if (length(missing_keys) > 0) {
    warning("Missing shared values: ", paste(missing_keys, collapse = ", "))
    return(FALSE)
  }
  TRUE
}

# ============================================================================
# End of utils.R
# ============================================================================