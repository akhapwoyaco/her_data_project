# ================================================================================
# Atlas Labs HR Analytics - Advanced Edge Case & Boundary Testing Suite
# Author: akhapwoyaco
# Focus: Edge cases, boundary conditions, and corner cases not covered in manual testing
# ================================================================================

library(testthat)
library(shiny)
library(lubridate)
library(stringr)

# ================================================================================
# 11. BOUNDARY VALUE TESTING
# ================================================================================

test_that("Boundary Value Analysis - Edge Cases", {
  
  # Test 11.1: Date boundary conditions
  test_that("handles date edge cases correctly", {
    edge_dates <- list(
      leap_year = as.Date("2024-02-29"),
      year_2000 = as.Date("2000-01-01"),
      far_future = as.Date("2099-12-31"),
      epoch_start = as.Date("1970-01-01"),
      february_28 = as.Date("2023-02-28"),
      december_31 = as.Date("2023-12-31")
    )
    
    for(date_type in names(edge_dates)) {
      test_data <- create_test_dataset(100)
      test_data$HireDate <- edge_dates[[date_type]]
      
      expect_silent({
        tenure_calc <- calculate_tenure(test_data)
        date_filters <- apply_date_filters(test_data, edge_dates[[date_type]], edge_dates[[date_type]])
      })
      
      expect_true(all(!is.na(tenure_calc$years_at_company)))
      expect_gte(nrow(date_filters), 0)
    }
  })
  
  # Test 11.2: Numeric boundary extremes
  test_that("handles numeric boundary extremes", {
    boundary_cases <- list(
      max_salary = 999999999,
      min_salary = 0,
      max_age = 100,
      min_age = 16,
      zero_tenure = 0,
      max_tenure = 50,
      negative_values = -1,
      float_precision = 123456.789012345
    )
    
    test_data <- create_test_dataset(10)
    
    # Test each boundary case
    test_data$Salary[1] <- boundary_cases$max_salary
    test_data$Salary[2] <- boundary_cases$min_salary
    test_data$Age[3] <- boundary_cases$max_age
    test_data$Age[4] <- boundary_cases$min_age
    
    expect_silent({
      salary_stats <- calculate_salary_statistics(test_data)
      age_distribution <- analyze_age_distribution(test_data)
    })
    
    # Should handle extreme values gracefully
    expect_true(is.finite(salary_stats$mean_salary))
    expect_true(salary_stats$max_salary <= 999999999)
    expect_gte(salary_stats$min_salary, 0)
  })
  
  # Test 11.3: String length boundaries
  test_that("handles string length edge cases", {
    test_data <- create_test_dataset(5)
    
    # Test various string lengths
    test_data$FirstName[1] <- "" # Empty string
    test_data$FirstName[2] <- "A" # Single character
    test_data$FirstName[3] <- paste(rep("X", 255), collapse = "") # Very long name
    test_data$FirstName[4] <- "José-María" # Special characters
    test_data$FirstName[5] <- "李小明" # Unicode characters
    
    expect_silent({
      name_analysis <- analyze_employee_names(test_data)
      search_results <- search_employees_by_name(test_data, "José")
    })
    
    expect_gte(nrow(search_results), 0)
    expect_true(all(nchar(name_analysis$processed_names) >= 0))
  })
})

# ================================================================================
# 12. DATA CORRUPTION & RECOVERY TESTING
# ================================================================================

test_that("Data Corruption & Recovery - Edge Cases", {
  
  # Test 12.1: Malformed CSV handling
  test_that("recovers from malformed CSV data", {
    # Create malformed CSV scenarios
    malformed_scenarios <- list(
      extra_commas = "ID,Name,Age,Salary\n1,John,,50000,extra\n2,Jane,30,60000\n",
      missing_quotes = "ID,Name,Age,Salary\n1,John Smith,25,50000\n2,Jane \"Doe,30,60000\n",
      mixed_encodings = "ID,Name,Age,Salary\n1,José,25,50000\n2,François,30,60000\n",
      unicode_bom = "\uFEFFID,Name,Age,Salary\n1,John,25,50000\n"
    )
    
    for(scenario_name in names(malformed_scenarios)) {
      temp_file <- tempfile(fileext = ".csv")
      writeLines(malformed_scenarios[[scenario_name]], temp_file, useBytes = TRUE)
      
      expect_silent({
        result <- safely_load_csv(temp_file)
      })
      
      expect_true(result$success || !is.null(result$recovered_data))
      unlink(temp_file)
    }
  })
  
  # Test 12.2: Partial data corruption recovery
  test_that("recovers from partial data corruption", {
    test_data <- create_test_dataset(1000)
    
    # Introduce various corruption patterns
    corrupted_data <- test_data
    corrupted_data$Salary[sample(1000, 50)] <- "CORRUPTED"
    corrupted_data$Age[sample(1000, 30)] <- -999
    corrupted_data$HireDate[sample(1000, 20)] <- "invalid-date"
    corrupted_data$Department[sample(1000, 10)] <- ""
    
    expect_silent({
      cleaned_data <- clean_and_recover_data(corrupted_data)
      validation_result <- validate_data_integrity(cleaned_data)
    })
    
    expect_true(validation_result$is_valid)
    expect_gt(nrow(cleaned_data), 800) # Should recover most records
    expect_true(all(is.numeric(cleaned_data$Salary[!is.na(cleaned_data$Salary)])))
  })
  
  # Test 12.3: Memory corruption simulation
  test_that("detects and handles memory corruption", {
    large_data <- create_large_test_dataset(10000)
    
    # Simulate memory corruption scenarios
    memory_scenarios <- list(
      null_injection = function(data) { data[sample(nrow(data), 100), ] <- NA; data },
      type_corruption = function(data) { data$Age <- as.character(data$Age); data },
      structure_damage = function(data) { attr(data, "class") <- NULL; data }
    )
    
    for(scenario_name in names(memory_scenarios)) {
      corrupted_data <- memory_scenarios[[scenario_name]](large_data)
      
      expect_silent({
        recovery_result <- attempt_data_recovery(corrupted_data)
      })
      
      expect_true(recovery_result$recovery_attempted)
      if(recovery_result$recovery_successful) {
        expect_true(is.data.frame(recovery_result$recovered_data))
      }
    }
  })
})

# ================================================================================
# 13. CONCURRENCY & RACE CONDITION TESTING
# ================================================================================

test_that("Concurrency & Race Conditions - Edge Cases", {
  
  # Test 13.1: Simultaneous data updates
  test_that("handles simultaneous data updates safely", {
    shared_data <- create_test_dataset(1000)
    
    # Simulate concurrent data modifications
    update_functions <- list(
      function(data) { data$Salary <- data$Salary * 1.05; data },
      function(data) { data$Age <- data$Age + 1; data },
      function(data) { data[nrow(data) + 1, ] <- data[1, ]; data },
      function(data) { data <- data[-sample(nrow(data), 10), ]; data }
    )
    
    results <- parallel::mclapply(update_functions, function(update_fn) {
      tryCatch({
        updated_data <- update_fn(shared_data)
        list(success = TRUE, data = updated_data)
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })
    }, mc.cores = 4)
    
    # At least some updates should succeed
    successful_updates <- sum(sapply(results, function(x) x$success))
    expect_gt(successful_updates, 0)
  })
  
  # Test 13.2: Resource contention handling
  test_that("manages resource contention gracefully", {
    # Simulate multiple processes accessing same resources
    resource_tasks <- replicate(10, {
      function() {
        data <- load_employee_data()
        processed <- expensive_computation(data)
        save_results(processed)
        return(nrow(processed))
      }
    }, simplify = FALSE)
    
    start_time <- Sys.time()
    results <- parallel::mclapply(resource_tasks, function(task) {
      tryCatch({
        task()
      }, error = function(e) {
        return(-1) # Error indicator
      })
    }, mc.cores = 5)
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    successful_tasks <- sum(results != -1)
    
    expect_gt(successful_tasks, 5) # At least half should succeed
    expect_lt(execution_time, 30) # Should not hang indefinitely
  })
  
  # Test 13.3: Deadlock prevention
  test_that("prevents deadlock scenarios", {
    # Create potential deadlock scenario with shared resources
    resource_a <- new.env()
    resource_b <- new.env()
    
    task_1 <- function() {
      lock_resource(resource_a)
      Sys.sleep(0.1)
      lock_resource(resource_b)
      unlock_resource(resource_b)
      unlock_resource(resource_a)
      return("task_1_complete")
    }
    
    task_2 <- function() {
      lock_resource(resource_b)
      Sys.sleep(0.1)
      lock_resource(resource_a)
      unlock_resource(resource_a)
      unlock_resource(resource_b)
      return("task_2_complete")
    }
    
    # Run tasks with timeout to detect deadlocks
    start_time <- Sys.time()
    results <- tryCatch({
      parallel::mclapply(list(task_1, task_2), function(task) {
        task()
      }, mc.cores = 2)
    }, error = function(e) {
      list("error", "error")
    })
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should not hang (indicating deadlock prevention works)
    expect_lt(execution_time, 5)
  })
})

# ================================================================================
# 14. INTERNATIONALIZATION & LOCALIZATION EDGE CASES
# ================================================================================

test_that("Internationalization & Localization - Edge Cases", {
  
  # Test 14.1: Unicode and special character handling
  test_that("handles international characters correctly", {
    international_data <- data.frame(
      EmployeeID = 1:10,
      FirstName = c("José", "François", "李小明", "محمد", "Ğünther", "Østergård", "Пётр", "Καλλιόπη", "Rajesh", "Åse"),
      LastName = c("García", "Müller", "王", "الرحمن", "Schäfer", "Hansen", "Иванов", "Παπαδοπούλου", "Śrivaştava", "Øberg"),
      Department = c("Sales", "Engineering", "HR", "Marketing", "Finance", "Operations", "IT", "Legal", "Research", "Admin"),
      stringsAsFactors = FALSE
    )
    
    expect_silent({
      search_results <- search_employees_by_name(international_data, "José")
      filtered_data <- filter_by_department(international_data, "Sales")
      sorted_data <- sort_employees_by_name(international_data)
    })
    
    expect_gt(nrow(search_results), 0)
    expect_true(all(nchar(sorted_data$FirstName) > 0))
  })
  
  # Test 14.2: Right-to-left language support
  test_that("handles RTL languages correctly", {
    rtl_data <- data.frame(
      EmployeeID = 1:5,
      FirstName = c("محمد", "فاطمة", "علي", "خديجة", "حسن"),
      LastName = c("الأحمد", "الزهراء", "الحسيني", "البتول", "العلوي"),
      Department = c("مبيعات", "هندسة", "موارد بشرية", "تسويق", "مالية"),
      stringsAsFactors = FALSE
    )
    
    expect_silent({
      formatted_names <- format_employee_names(rtl_data)
      department_summary <- summarize_by_department(rtl_data)
    })
    
    expect_equal(nrow(formatted_names), 5)
    expect_gt(nrow(department_summary), 0)
  })
  
  # Test 14.3: Mixed encoding scenarios
  test_that("handles mixed character encodings", {
    mixed_encodings <- list(
      utf8 = "José García",
      latin1 = iconv("José García", to = "latin1"),
      ascii = "Jose Garcia"
    )
    
    for(encoding_name in names(mixed_encodings)) {
      test_data <- create_test_dataset(3)
      test_data$FirstName[1] <- mixed_encodings[[encoding_name]]
      
      expect_silent({
        normalized_data <- normalize_text_encoding(test_data)
        search_result <- fuzzy_search_names(test_data, "Jose")
      })
      
      expect_true(validUTF8(normalized_data$FirstName[1]))
    }
  })
})

# ================================================================================
# 15. EXTREME DATA VOLUME TESTING
# ================================================================================

test_that("Extreme Data Volume - Scalability Edge Cases", {
  
  # Test 15.1: Very large dataset handling
  test_that("handles extremely large datasets", {
    skip_if_not(interactive(), "Skipping large dataset test in non-interactive mode")
    
    # Test with 1 million records
    large_dataset_size <- 1000000
    
    expect_silent({
      large_data <- create_large_test_dataset(large_dataset_size)
    })
    
    # Memory usage should be reasonable
    object_size <- object.size(large_data)
    expect_lt(as.numeric(object_size), 1e9) # Less than 1GB
    
    # Basic operations should still work
    start_time <- Sys.time()
    summary_stats <- calculate_basic_statistics(large_data)
    end_time <- Sys.time()
    
    processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(processing_time, 60) # Should process within 1 minute
  })
  
  # Test 15.2: Wide dataset (many columns) handling
  test_that("handles datasets with many columns", {
    # Create dataset with 500 columns
    wide_data <- create_test_dataset(1000)
    
    # Add many additional columns
    for(i in 1:497) {
      wide_data[[paste0("ExtraCol_", i)]] <- sample(1:100, 1000, replace = TRUE)
    }
    
    expect_equal(ncol(wide_data), 500)
    
    expect_silent({
      column_summary <- summarize_all_columns(wide_data)
      correlation_matrix <- calculate_correlation_matrix(wide_data[, sapply(wide_data, is.numeric)])
    })
    
    expect_equal(length(column_summary), 500)
  })
  
  # Test 15.3: Sparse data handling
  test_that("handles sparse datasets efficiently", {
    # Create dataset with 90% missing values
    sparse_data <- create_test_dataset(10000)
    
    # Make data sparse
    for(col in names(sparse_data)) {
      if(is.numeric(sparse_data[[col]])) {
        sparse_data[[col]][sample(10000, 9000)] <- NA
      }
    }
    
    expect_silent({
      sparse_summary <- analyze_sparse_data(sparse_data)
      imputed_data <- smart_imputation(sparse_data)
    })
    
    expect_true(sparse_summary$sparsity_ratio > 0.8)
    expect_lt(sum(is.na(imputed_data)), sum(is.na(sparse_data)))
  })
})

# ================================================================================
# 16. TIME-BASED EDGE CASES
# ================================================================================

test_that("Time-Based Processing - Edge Cases", {
  
  # Test 16.1: Timezone handling edge cases
  test_that("handles timezone transitions correctly", {
    # Test around daylight saving time transitions
    dst_dates <- c(
      "2024-03-10 02:00:00", # Spring forward (doesn't exist)
      "2024-11-03 01:30:00", # Fall back (happens twice)
      "2024-12-31 23:59:59"  # Year boundary
    )
    
    for(date_str in dst_dates) {
      test_data <- create_test_dataset(10)
      test_data$HireDate <- as.POSIXct(date_str, tz = "America/New_York")
      
      expect_silent({
        tenure_calc <- calculate_tenure_with_timezone(test_data)
        time_series <- create_hiring_timeline(test_data)
      })
      
      expect_true(all(!is.na(tenure_calc$days_employed)))
    }
  })
  
  # Test 16.2: Leap year calculations
  test_that("handles leap year calculations correctly", {
    leap_year_scenarios <- list(
      leap_year_hire = as.Date("2024-02-29"),
      leap_year_boundary = as.Date("2024-02-28"),
      non_leap_year = as.Date("2023-02-28"),
      century_year = as.Date("2000-02-29") # 2000 is leap year
    )
    
    for(scenario in names(leap_year_scenarios)) {
      test_data <- create_test_dataset(100)
      test_data$HireDate <- leap_year_scenarios[[scenario]]
      
      expect_silent({
        anniversary_dates <- calculate_work_anniversaries(test_data)
        tenure_years <- calculate_exact_tenure_years(test_data)
      })
      
      expect_true(all(anniversary_dates >= test_data$HireDate))
      expect_true(all(tenure_years >= 0))
    }
  })
  
  # Test 16.3: Future date handling
  test_that("handles future dates gracefully", {
    test_data <- create_test_dataset(10)
    
    # Set some hire dates in the future
    test_data$HireDate[1:3] <- Sys.Date() + c(30, 60, 90)
    
    expect_silent({
      validation_result <- validate_hire_dates(test_data)
      filtered_data <- filter_valid_employees(test_data)
    })
    
    expect_true(validation_result$has_future_dates)
    expect_lt(nrow(filtered_data), nrow(test_data))
  })
})

# ================================================================================
# 17. STATISTICAL EDGE CASES
# ================================================================================

test_that("Statistical Analysis - Edge Cases", {
  
  # Test 17.1: Division by zero scenarios
  test_that("handles division by zero in statistics", {
    # Create scenarios that could lead to division by zero
    edge_case_data <- create_test_dataset(100)
    
    # All employees have same salary (zero variance)
    edge_case_data$Salary <- 50000
    
    # All employees in same department
    edge_case_data$Department <- "Engineering"
    
    expect_silent({
      salary_stats <- calculate_salary_statistics(edge_case_data)
      dept_analysis <- analyze_department_distribution(edge_case_data)
      correlation_analysis <- calculate_correlations(edge_case_data)
    })
    
    # Should handle zero variance gracefully
    expect_true(is.finite(salary_stats$mean))
    expect_true(salary_stats$std_dev == 0 || is.na(salary_stats$std_dev))
  })
  
  # Test 17.2: Extreme outliers
  test_that("handles extreme statistical outliers", {
    test_data <- create_test_dataset(1000)
    
    # Introduce extreme outliers
    test_data$Salary[1] <- 10000000 # 10M salary
    test_data$Salary[2] <- 1 # $1 salary
    test_data$Age[3] <- 16 # Very young
    test_data$Age[4] <- 99 # Very old
    
    expect_silent({
      outlier_detection <- detect_outliers(test_data)
      robust_stats <- calculate_robust_statistics(test_data)
      cleaned_data <- remove_extreme_outliers(test_data)
    })
    
    expect_gt(nrow(outlier_detection), 0)
    expect_true(robust_stats$median_salary < robust_stats$mean_salary)
    expect_lt(nrow(cleaned_data), nrow(test_data))
  })
  
  # Test 17.3: Perfect correlations
  test_that("handles perfect correlations correctly", {
    test_data <- create_test_dataset(100)
    
    # Create perfect correlation
    test_data$PerfectCorr <- test_data$Salary * 2
    
    # Create perfect negative correlation
    test_data$NegativeCorr <- -test_data$Age
    
    expect_silent({
      correlation_matrix <- cor(test_data[sapply(test_data, is.numeric)], use = "complete.obs")
      correlation_analysis <- analyze_correlation_strength(correlation_matrix)
    })
    
    expect_equal(correlation_matrix["Salary", "PerfectCorr"], 1)
    expect_equal(correlation_matrix["Age", "NegativeCorr"], -1)
    expect_true(correlation_analysis$has_perfect_correlations)
  })
})

# ================================================================================
# 18. VISUALIZATION EDGE CASES
# ================================================================================

test_that("Visualization Rendering - Edge Cases", {
  
  # Test 18.1: Empty data visualization
  test_that("handles empty datasets in visualizations", {
    empty_data <- create_test_dataset(0)
    
    expect_silent({
      empty_plot <- create_salary_distribution_plot(empty_data)
      empty_heatmap <- create_correlation_heatmap(empty_data)
    })
    
    # Should create valid plot objects even with empty data
    expect_true(ggplot2::is.ggplot(empty_plot) || is.null(empty_plot))
  })
  
  # Test 18.2: Single data point visualization
  test_that("handles single data point visualizations", {
    single_point_data <- create_test_dataset(1)
    
    expect_silent({
      single_plot <- create_attrition_analysis_plot(single_point_data)
      single_scatter <- create_age_salary_scatter(single_point_data)
    })
    
    expect_true(ggplot2::is.ggplot(single_plot) || is.null(single_plot))
  })
  
  # Test 18.3: Extreme aspect ratios
  test_that("handles extreme visualization aspect ratios", {
    test_data <- create_test_dataset(1000)
    
    # Test very wide and very tall plots
    extreme_dimensions <- list(
      very_wide = list(width = 2000, height = 100),
      very_tall = list(width = 100, height = 2000),
      tiny = list(width = 50, height = 50)
    )
    
    for(dim_name in names(extreme_dimensions)) {
      dims <- extreme_dimensions[[dim_name]]
      
      expect_silent({
        plot_obj <- create_department_breakdown_plot(test_data, 
                                                   width = dims$width, 
                                                   height = dims$height)
      })
      
      expect_true(ggplot2::is.ggplot(plot_obj) || is.null(plot_obj))
    }
  })
})

# ================================================================================
# HELPER FUNCTIONS FOR EDGE CASE TESTS
# ================================================================================

safely_load_csv <- function(file_path) {
  tryCatch({
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    list(success = TRUE, data = data, recovered_data = NULL)
  }, error = function(e) {
    # Attempt recovery
    recovered <- tryCatch({
      readLines(file_path) %>%
        gsub(",,+", ",NA,", .) %>%
        paste(collapse = "\n") %>%
        textConnection() %>%
        read.csv(stringsAsFactors = FALSE)
    }, error = function(e2) NULL)
    
    list(success = FALSE, data = NULL, recovered_data = recovered, error = e$message)
  })
}

clean_and_recover_data <- function(data) {
  # Clean corrupted data
  cleaned <- data
  
  # Fix salary corruption
  cleaned$Salary[cleaned$Salary == "CORRUPTED"] <- NA
  cleaned$Salary <- as.numeric(cleaned$Salary)
  
  # Fix age corruption
  cleaned$Age[cleaned$Age < 0] <- NA
  
  # Fix date corruption
  cleaned$HireDate <- as.Date(cleaned$HireDate)
  
  # Remove empty departments
  cleaned <- cleaned[cleaned$Department != "", ]
  
  return(cleaned)
}

validate_data_integrity <- function(data) {
  checks <- list(
    has_required_columns = all(c("EmployeeID", "Age", "Salary") %in% names(data)),
    no_duplicate_ids = length(unique(data$EmployeeID)) == nrow(data),
    valid_ages = all(data$Age >= 16 & data$Age <= 100, na.rm = TRUE),
    valid_salaries = all(data$Salary >= 0, na.rm = TRUE),
    valid_dates = all(!is.na(as.Date(data$HireDate)), na.rm = TRUE)
  )
  
  list(
    is_valid = all(unlist(checks)),
    checks = checks,
    issues_found = sum(!unlist(checks))
  )
}

attempt_data_recovery <- function(corrupted_data) {
  recovery_attempted <- TRUE
  recovery_successful <- FALSE
  recovered_data <- NULL
  
  tryCatch({
    # Attempt various recovery strategies
    if(!is.data.frame(corrupted_data)) {
      recovered_data <- as.data.frame(corrupted_data)
    } else {
      recovered_data <- corrupted_data
    }
    
    # Fix data types
    numeric_cols <- c("Age", "Salary", "YearsAtCompany")
    for(col in numeric_cols) {
      if(col %in% names(recovered_data)) {
        recovered_data[[col]] <- as.numeric(recovered_data[[col]])
      }
    }
    
    recovery_successful <- TRUE
  }, error = function(e) {
    recovery_successful <<- FALSE
  })
  
  list(
    recovery_attempted = recovery_attempted,
    recovery_successful = recovery_successful,
    recovered_data = recovered_data
  )
}

expensive_computation <- function(data) {
  # Simulate expensive computation
  Sys.sleep(runif(1, 0.1, 0.5))
  return(data[sample(nrow(data), min(100, nrow(data))), ])
}

save_results <- function(data) {
  # Simulate saving results
  temp_file <- tempfile()
  write.csv(data, temp_file)
  unlink(temp_file)
  return(TRUE)
}

lock_resource <- function(resource) {
  # Simulate resource locking
  resource$locked <- TRUE
  resource$lock_time <- Sys.time()
}

unlock_resource <- function(resource) {
  # Simulate resource unlocking
  resource$locked <- FALSE
  resource$lock_time <- NULL
}

# Additional helper functions for statistical edge cases
detect_outliers <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  outliers <- data.frame()
  
  for(col in names(data)[numeric_cols]) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    outlier_indices <- which(data[[col]] < (Q1 - 1.5 * IQR) | data[[col]] > (Q3 + 1.5 * IQR))
    
    if(length(outlier_indices) > 0) {
      outliers <- rbind(outliers, data.frame(
        row = outlier_indices,
        column = col,
        value = data[[col]][outlier_indices]
      ))
    }
  }
  
  return(outliers)
}

calculate_robust_statistics <- function(data) {
  list(
    median_salary = median(data$Salary, na.rm = TRUE),
    mean_salary = mean(data$Salary, na.rm = TRUE),
    mad_salary = mad(data$Salary, na.rm = TRUE),
    iqr_salary = IQR(data$Salary, na.rm = TRUE)
  )
}

analyze_correlation_strength <- function(cor_matrix) {
  # Remove diagonal (perfect self-correlation)
  cor_matrix[is.na(cor_matrix)] <- 0
  diag(cor_matrix) <- 0
  
  perfect_correlations <- sum(abs(cor_matrix) == 1)
  strong_correlations <- sum(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1)
  
  list(
    has_perfect_correlations = perfect_correlations > 0,
    perfect_count = perfect_correlations,
    strong_count = strong_correlations
  )
}

cat("Atlas Labs HR Analytics - Advanced Edge Case Test Suite Loaded\n")
cat("================================================================\n")
cat("Test Coverage:\n")
cat("- Boundary Value Analysis\n")
cat("- Data Corruption & Recovery\n") 
cat("- Concurrency & Race Conditions\n")
cat("- Internationalization Edge Cases\n")
cat("- Extreme Data Volume Scenarios\n")
cat("- Time-based Processing Edge Cases\n")
cat("- Statistical Analysis Edge Cases\n")
cat("- Visualization Rendering Edge Cases\n")
cat("================================================================\n")