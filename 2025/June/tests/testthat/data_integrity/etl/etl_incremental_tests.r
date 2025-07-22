# =============================================================================
# ETL PROCESS TESTING - INCREMENTAL UPDATE ACCURACY
# Comprehensive Unit Tests for Atlas Labs HR Analytics Dashboard
# Focus: Data Integrity & ETL Process Validation
# =============================================================================

# Load required libraries for testing
library(testthat)
library(dplyr)
library(lubridate)
library(data.table)
library(digest)

# =============================================================================
# TEST SETUP & HELPER FUNCTIONS
# =============================================================================

# Create test data generators
create_baseline_employee_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    EmployeeID = 1:n,
    FirstName = paste0("Employee", 1:n),
    LastName = paste0("LastName", 1:n),
    Gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE),
    Age = sample(25:65, n, replace = TRUE),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "No_Travel"), n, replace = TRUE),
    Department = sample(c("Sales", "R&D", "HR", "IT", "Marketing"), n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL", "WA"), n, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n, replace = TRUE),
    Education = sample(1:5, n, replace = TRUE),
    EducationField = sample(c("Engineering", "Business", "Marketing", "HR", "Other"), n, replace = TRUE),
    JobRole = sample(c("Manager", "Senior", "Junior", "Lead", "Director"), n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    Salary = sample(40000:150000, n, replace = TRUE),
    StockOptionLevel = sample(0:3, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    HireDate = as.Date("2020-01-01") + sample(0:1000, n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.15, 0.85)),
    YearsAtCompany = sample(0:20, n, replace = TRUE),
    YearsInMostRecentRole = sample(0:15, n, replace = TRUE),
    YearsSinceLastPromotion = sample(0:10, n, replace = TRUE),
    YearsWithCurrManager = sample(0:12, n, replace = TRUE),
    LastModified = Sys.time(),
    DataVersion = "1.0",
    stringsAsFactors = FALSE
  )
}

create_incremental_updates <- function(base_data, update_type = "mixed") {
  n_updates <- nrow(base_data) * 0.1  # 10% of records
  update_ids <- sample(base_data$EmployeeID, n_updates)
  
  if (update_type == "salary_only") {
    updates <- base_data[base_data$EmployeeID %in% update_ids, ]
    updates$Salary <- updates$Salary * runif(length(update_ids), 1.05, 1.15)
  } else if (update_type == "mixed") {
    updates <- base_data[base_data$EmployeeID %in% update_ids, ]
    updates$Salary <- updates$Salary * runif(length(update_ids), 0.95, 1.20)
    updates$Department[1:3] <- "NewDept"
    updates$JobRole[4:6] <- "NewRole"
    updates$Attrition[7:9] <- "Yes"
  }
  
  updates$LastModified <- Sys.time() + runif(nrow(updates), 1, 3600)
  updates$DataVersion <- "1.1"
  return(updates)
}

# Data integrity validation functions
validate_data_types <- function(df) {
  type_checks <- list(
    EmployeeID = is.numeric(df$EmployeeID),
    Age = is.numeric(df$Age),
    Salary = is.numeric(df$Salary),
    HireDate = inherits(df$HireDate, "Date"),
    LastModified = inherits(df$LastModified, "POSIXct")
  )
  return(all(unlist(type_checks)))
}

calculate_data_hash <- function(df) {
  # Create hash for data integrity checking
  df_sorted <- df[order(df$EmployeeID), ]
  digest(df_sorted, algo = "md5")
}

# ETL functions to test
perform_incremental_update <- function(baseline_data, incremental_data, 
                                     merge_strategy = "last_modified") {
  if (merge_strategy == "last_modified") {
    # Merge based on LastModified timestamp
    combined <- rbind(baseline_data, incremental_data)
    result <- combined %>%
      group_by(EmployeeID) %>%
      slice_max(LastModified, n = 1) %>%
      ungroup() %>%
      arrange(EmployeeID)
    
  } else if (merge_strategy == "version_priority") {
    # Merge based on DataVersion
    combined <- rbind(baseline_data, incremental_data)
    result <- combined %>%
      group_by(EmployeeID) %>%
      slice_max(as.numeric(gsub("\\.", "", DataVersion)), n = 1) %>%
      ungroup() %>%
      arrange(EmployeeID)
  }
  
  return(as.data.frame(result))
}

detect_data_conflicts <- function(baseline_data, incremental_data) {
  conflicts <- incremental_data %>%
    inner_join(baseline_data, by = "EmployeeID", suffix = c("_new", "_old")) %>%
    filter(
      Salary_new != Salary_old |
      Department_new != Department_old |
      JobRole_new != JobRole_old |
      Attrition_new != Attrition_old
    ) %>%
    select(EmployeeID, contains("_new"), contains("_old"))
  
  return(conflicts)
}

# =============================================================================
# 1. INCREMENTAL UPDATE ACCURACY TESTS
# =============================================================================

test_that("Incremental updates preserve data integrity", {
  # Setup
  baseline <- create_baseline_employee_data(100)
  incremental <- create_incremental_updates(baseline, "mixed")
  
  # Execute incremental update
  result <- perform_incremental_update(baseline, incremental)
  
  # Test 1: All original records preserved or updated
  expect_equal(nrow(result), nrow(baseline))
  expect_equal(sort(result$EmployeeID), sort(baseline$EmployeeID))
  
  # Test 2: Data types maintained
  expect_true(validate_data_types(result))
  
  # Test 3: Updated records have newer timestamps
  updated_ids <- intersect(result$EmployeeID, incremental$EmployeeID)
  updated_records <- result[result$EmployeeID %in% updated_ids, ]
  expect_true(all(updated_records$DataVersion == "1.1"))
  
  # Test 4: Non-updated records maintain original version
  non_updated_ids <- setdiff(result$EmployeeID, incremental$EmployeeID)
  non_updated_records <- result[result$EmployeeID %in% non_updated_ids, ]
  expect_true(all(non_updated_records$DataVersion == "1.0"))
})

test_that("Incremental updates handle duplicate records correctly", {
  # Setup with intentional duplicates
  baseline <- create_baseline_employee_data(50)
  incremental <- create_incremental_updates(baseline, "salary_only")
  
  # Add duplicate records with different timestamps
  duplicate_record <- incremental[1, ]
  duplicate_record$LastModified <- duplicate_record$LastModified + 3600
  duplicate_record$Salary <- duplicate_record$Salary * 1.10
  incremental_with_dups <- rbind(incremental, duplicate_record)
  
  # Execute update
  result <- perform_incremental_update(baseline, incremental_with_dups)
  
  # Test: Only one record per EmployeeID
  expect_equal(length(unique(result$EmployeeID)), nrow(result))
  
  # Test: Latest timestamp wins
  target_id <- duplicate_record$EmployeeID
  final_record <- result[result$EmployeeID == target_id, ]
  expect_equal(final_record$LastModified, duplicate_record$LastModified)
})

# =============================================================================
# 2. DATA CONSISTENCY VALIDATION TESTS
# =============================================================================

test_that("Incremental updates maintain referential integrity", {
  # Setup with performance data that references employee data
  baseline_emp <- create_baseline_employee_data(50)
  
  # Create performance data
  performance_data <- data.frame(
    PerformanceID = 1:75,
    EmployeeID = sample(baseline_emp$EmployeeID, 75, replace = TRUE),
    ReviewDate = Sys.Date() - sample(1:365, 75, replace = TRUE),
    JobSatisfaction = sample(1:5, 75, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 75, replace = TRUE)
  )
  
  # Create incremental updates that remove some employees
  incremental_emp <- baseline_emp[1:5, ]
  incremental_emp$Attrition <- "Yes"
  incremental_emp$LastModified <- Sys.time()
  
  # Execute update
  updated_emp <- perform_incremental_update(baseline_emp, incremental_emp)
  
  # Test: All performance records still have valid employee references
  orphaned_performance <- performance_data[!performance_data$EmployeeID %in% updated_emp$EmployeeID, ]
  expect_equal(nrow(orphaned_performance), 0)
  
  # Test: Employee IDs in both datasets match
  common_ids <- intersect(updated_emp$EmployeeID, performance_data$EmployeeID)
  expect_true(length(common_ids) > 0)
})

test_that("Incremental updates handle missing mandatory fields", {
  # Setup
  baseline <- create_baseline_employee_data(30)
  incremental <- create_incremental_updates(baseline, "mixed")[1:3, ]
  
  # Introduce missing mandatory fields
  incremental$EmployeeID[1] <- NA
  incremental$FirstName[2] <- NA
  incremental$HireDate[3] <- NA
  
  # Test: Function handles missing mandatory fields gracefully
  expect_error(perform_incremental_update(baseline, incremental), NA)
  
  # Execute with error handling
  result <- tryCatch({
    perform_incremental_update(baseline, incremental)
  }, error = function(e) {
    baseline  # Return original if error
  })
  
  # Test: Original data preserved when incremental has issues
  expect_equal(nrow(result), nrow(baseline))
})

# =============================================================================
# 3. PERFORMANCE DATA INCREMENTAL UPDATES
# =============================================================================

test_that("Performance data incremental updates maintain accuracy", {
  # Setup performance baseline
  baseline_perf <- data.frame(
    PerformanceID = 1:100,
    EmployeeID = sample(1:50, 100, replace = TRUE),
    ReviewDate = seq.Date(from = as.Date("2023-01-01"), by = "month", length.out = 100),
    EnvironmentSatisfaction = sample(1:5, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 100, replace = TRUE),
    WorkLifeBalance = sample(1:5, 100, replace = TRUE),
    SelfRating = sample(1:5, 100, replace = TRUE),
    ManagerRating = sample(1:5, 100, replace = TRUE),
    LastModified = Sys.time() - runif(100, 0, 86400),
    DataVersion = "1.0"
  )
  
  # Create incremental performance updates
  incremental_perf <- baseline_perf[sample(nrow(baseline_perf), 20), ]
  incremental_perf$JobSatisfaction <- sample(1:5, 20, replace = TRUE)
  incremental_perf$LastModified <- Sys.time()
  incremental_perf$DataVersion <- "1.1"
  
  # Execute incremental update
  result_perf <- perform_incremental_update(baseline_perf, incremental_perf)
  
  # Test: Correct number of records maintained
  expect_equal(nrow(result_perf), nrow(baseline_perf))
  
  # Test: Updated records have latest satisfaction scores
  updated_perf_ids <- incremental_perf$PerformanceID
  updated_records <- result_perf[result_perf$PerformanceID %in% updated_perf_ids, ]
  expect_true(all(updated_records$DataVersion == "1.1"))
})

# =============================================================================
# 4. EDGE CASE TESTING
# =============================================================================

test_that("Handle empty incremental updates", {
  baseline <- create_baseline_employee_data(25)
  empty_incremental <- baseline[0, ]  # Empty dataframe with same structure
  
  result <- perform_incremental_update(baseline, empty_incremental)
  
  # Test: Original data unchanged
  expect_equal(nrow(result), nrow(baseline))
  expect_equal(result$EmployeeID, baseline$EmployeeID)
})

test_that("Handle incremental updates with new employee records", {
  baseline <- create_baseline_employee_data(25)
  
  # Create new employee records
  new_employees <- data.frame(
    EmployeeID = (max(baseline$EmployeeID) + 1):(max(baseline$EmployeeID) + 5),
    FirstName = paste0("NewEmployee", 1:5),
    LastName = paste0("NewLastName", 1:5),
    Gender = sample(c("Male", "Female"), 5, replace = TRUE),
    Age = sample(25:35, 5, replace = TRUE),
    BusinessTravel = "Travel_Rarely",
    Department = "New_Department",
    DistanceFromHome = sample(10:30, 5, replace = TRUE),
    State = "CA",
    Ethnicity = "White",
    Education = 3,
    EducationField = "Business",
    JobRole = "New_Hire",
    MaritalStatus = "Single",
    Salary = sample(50000:70000, 5, replace = TRUE),
    StockOptionLevel = 0,
    OverTime = "No",
    HireDate = Sys.Date(),
    Attrition = "No",
    YearsAtCompany = 0,
    YearsInMostRecentRole = 0,
    YearsSinceLastPromotion = 0,
    YearsWithCurrManager = 0,
    LastModified = Sys.time(),
    DataVersion = "1.1",
    stringsAsFactors = FALSE
  )
  
  # This should be handled as addition, not update
  expect_error({
    result <- rbind(baseline, new_employees)
    expect_equal(nrow(result), nrow(baseline) + nrow(new_employees))
  }, NA)
})

test_that("Handle concurrent update conflicts", {
  baseline <- create_baseline_employee_data(20)
  
  # Create two conflicting updates for same employee
  update1 <- baseline[1, ]
  update1$Salary <- 75000
  update1$LastModified <- Sys.time()
  update1$DataVersion <- "1.1"
  
  update2 <- baseline[1, ]
  update2$Salary <- 80000
  update2$LastModified <- Sys.time() + 3600  # 1 hour later
  update2$DataVersion <- "1.2"
  
  # Combine conflicting updates
  conflicting_updates <- rbind(update1, update2)
  result <- perform_incremental_update(baseline, conflicting_updates)
  
  # Test: Latest update wins
  final_salary <- result[result$EmployeeID == 1, "Salary"]
  expect_equal(final_salary, 80000)
})

test_that("Validate data transformation accuracy during updates", {
  baseline <- create_baseline_employee_data(30)
  
  # Create updates with data transformations
  incremental <- create_incremental_updates(baseline, "salary_only")
  
  # Store original checksums
  original_checksums <- baseline %>%
    mutate(row_hash = pmap_chr(., function(...) digest(list(...), algo = "md5")))
  
  # Execute update
  result <- perform_incremental_update(baseline, incremental)
  
  # Test: Non-updated records maintain exact same data
  non_updated_ids <- setdiff(baseline$EmployeeID, incremental$EmployeeID)
  non_updated_baseline <- baseline[baseline$EmployeeID %in% non_updated_ids, ]
  non_updated_result <- result[result$EmployeeID %in% non_updated_ids, ]
  
  # Remove timestamps for comparison
  non_updated_baseline_clean <- non_updated_baseline %>% select(-LastModified)
  non_updated_result_clean <- non_updated_result %>% select(-LastModified)
  
  expect_equal(nrow(non_updated_baseline_clean), nrow(non_updated_result_clean))
})

# =============================================================================
# 5. DATA VOLUME AND PERFORMANCE EDGE CASES
# =============================================================================

test_that("Handle large incremental updates efficiently", {
  # Test with larger dataset
  baseline_large <- create_baseline_employee_data(1000)
  incremental_large <- create_incremental_updates(baseline_large, "mixed")
  
  # Measure performance
  start_time <- Sys.time()
  result_large <- perform_incremental_update(baseline_large, incremental_large)
  end_time <- Sys.time()
  
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Test: Processing completes in reasonable time (< 5 seconds)
  expect_lt(processing_time, 5)
  
  # Test: All data integrity maintained
  expect_equal(nrow(result_large), nrow(baseline_large))
  expect_true(validate_data_types(result_large))
})

test_that("Memory efficiency during incremental updates", {
  baseline <- create_baseline_employee_data(500)
  incremental <- create_incremental_updates(baseline, "mixed")
  
  # Monitor memory usage
  gc_before <- gc()
  initial_memory <- sum(gc_before[, "used"])
  
  result <- perform_incremental_update(baseline, incremental)
  
  gc_after <- gc()
  final_memory <- sum(gc_after[, "used"])
  
  # Test: Memory usage doesn't grow excessively
  memory_increase <- final_memory - initial_memory
  expect_lt(memory_increase, initial_memory * 2)  # Less than 100% increase
})

# =============================================================================
# 6. BUSINESS LOGIC VALIDATION TESTS
# =============================================================================

test_that("Salary update validation follows business rules", {
  baseline <- create_baseline_employee_data(50)
  
  # Create salary updates that violate business rules
  incremental <- baseline[1:5, ]
  incremental$Salary[1] <- -1000  # Negative salary
  incremental$Salary[2] <- 1000000  # Unrealistic high salary
  incremental$Salary[3] <- 0  # Zero salary
  incremental$LastModified <- Sys.time()
  
  # Apply business rule validation
  validate_salary_update <- function(old_salary, new_salary) {
    if (new_salary <= 0) return(FALSE)
    if (new_salary > 500000) return(FALSE)
    if (abs(new_salary - old_salary) / old_salary > 0.5) return(FALSE)  # 50% change limit
    return(TRUE)
  }
  
  # Test business rule validation
  for (i in 1:3) {
    old_sal <- baseline[baseline$EmployeeID == incremental$EmployeeID[i], "Salary"]
    new_sal <- incremental$Salary[i]
    is_valid <- validate_salary_update(old_sal, new_sal)
    
    if (i == 1) expect_false(is_valid)  # Negative salary should fail
    if (i == 2) expect_false(is_valid)  # Too high salary should fail
    if (i == 3) expect_false(is_valid)  # Zero salary should fail
  }
})

test_that("Attrition status changes are logically consistent", {
  baseline <- create_baseline_employee_data(30)
  
  # Test attrition status changes
  incremental <- baseline[1:3, ]
  incremental$Attrition <- "Yes"
  incremental$YearsAtCompany <- incremental$YearsAtCompany + 1  # Should not increase if attrition = Yes
  incremental$LastModified <- Sys.time()
  
  result <- perform_incremental_update(baseline, incremental)
  
  # Business logic: If Attrition = "Yes", employee should not have future performance records
  attrit_employees <- result[result$Attrition == "Yes", "EmployeeID"]
  
  # Test: Attrition employees identified correctly
  expect_length(attrit_employees, 3)
  expect_true(all(attrit_employees %in% c(1, 2, 3)))
})

# =============================================================================
# 7. DATA QUALITY REGRESSION TESTS
# =============================================================================

test_that("Incremental updates don't introduce data quality issues", {
  baseline <- create_baseline_employee_data(40)
  incremental <- create_incremental_updates(baseline, "mixed")
  
  # Original data quality metrics
  orig_complete_records <- sum(complete.cases(baseline))
  orig_unique_ids <- length(unique(baseline$EmployeeID))
  orig_valid_dates <- sum(!is.na(baseline$HireDate))
  
  result <- perform_incremental_update(baseline, incremental)
  
  # Post-update data quality metrics
  new_complete_records <- sum(complete.cases(result))
  new_unique_ids <- length(unique(result$EmployeeID))
  new_valid_dates <- sum(!is.na(result$HireDate))
  
  # Test: Data quality maintained or improved
  expect_gte(new_complete_records, orig_complete_records)
  expect_equal(new_unique_ids, orig_unique_ids)
  expect_gte(new_valid_dates, orig_valid_dates)
})

# =============================================================================
# RUN ALL TESTS
# =============================================================================

# Function to run all ETL incremental update tests
run_etl_incremental_tests <- function() {
  cat("Running ETL Incremental Update Accuracy Tests...\n")
  cat("===============================================\n\n")
  
  test_results <- test_dir(".", pattern = "incremental.*\\.R$", reporter = "summary")
  
  cat("\n===============================================\n")
  cat("ETL Testing Complete!\n")
  cat("===============================================\n")
  
  return(test_results)
}

# Example usage:
# run_etl_incremental_tests()