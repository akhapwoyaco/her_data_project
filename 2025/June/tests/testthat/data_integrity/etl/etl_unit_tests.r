# ============================================================================
# ATLAS LABS HR ANALYTICS - ETL PROCESS TESTING SUITE
# Comprehensive Unit Tests for Data Integrity and ETL Operations
# Author: akhapwoyaco
# Focus: Error Handling, Data Lineage, Version Control, Rollback, Updates
# ============================================================================

library(testthat)
library(dplyr)
library(purrr)
library(lubridate)
library(digest)
library(R6)

# ============================================================================
# 5.2.1 ERROR HANDLING ROBUSTNESS TESTING
# ============================================================================

test_that("ETL Error Handling - File Access Errors", {
  
  # Test non-existent file handling
  expect_error(
    load_employee_data("non_existent_file.csv"),
    regexp = "File does not exist|cannot open",
    class = "file_not_found_error"
  )
  
  # Test corrupted file handling
  temp_corrupted <- tempfile(fileext = ".csv")
  writeLines(c("header1,header2", "value1", "incomplete_row,"), temp_corrupted)
  
  expect_warning(
    result <- load_employee_data(temp_corrupted),
    regexp = "Data integrity issues detected"
  )
  
  # Test permission denied scenarios
  if (.Platform$OS.type == "unix") {
    temp_readonly <- tempfile(fileext = ".csv")
    write.csv(mtcars, temp_readonly)
    Sys.chmod(temp_readonly, mode = "000")
    
    expect_error(
      load_employee_data(temp_readonly),
      regexp = "Permission denied|Access denied",
      class = "permission_error"
    )
    
    # Cleanup
    Sys.chmod(temp_readonly, mode = "644")
    unlink(temp_readonly)
  }
  
  unlink(temp_corrupted)
})

test_that("ETL Error Handling - Data Format Validation", {
  
  # Test invalid CSV structure
  temp_invalid <- tempfile(fileext = ".csv")
  writeLines(c("EmployeeID,Name", '1,"John"Doe"', "2,Jane,Smith,Extra"), temp_invalid)
  
  expect_error(
    validate_data_structure(temp_invalid),
    regexp = "Invalid CSV structure|Parsing error",
    class = "csv_format_error"
  )
  
  # Test missing required columns
  temp_missing_cols <- tempfile(fileext = ".csv")
  write.csv(data.frame(ID = 1:3, Name = letters[1:3]), temp_missing_cols)
  
  expect_error(
    validate_employee_schema(read.csv(temp_missing_cols)),
    regexp = "Missing required columns.*EmployeeID",
    class = "schema_validation_error"
  )
  
  # Test incorrect data types
  temp_wrong_types <- tempfile(fileext = ".csv")
  wrong_data <- data.frame(
    EmployeeID = c("ABC", "DEF", "GHI"),  # Should be numeric
    Age = c("twenty", "thirty", "forty"),  # Should be numeric
    Salary = c("high", "medium", "low"),   # Should be numeric
    stringsAsFactors = FALSE
  )
  write.csv(wrong_data, temp_wrong_types, row.names = FALSE)
  
  expect_error(
    validate_data_types(read.csv(temp_wrong_types)),
    regexp = "Data type validation failed",
    class = "data_type_error"
  )
  
  # Cleanup
  unlink(c(temp_invalid, temp_missing_cols, temp_wrong_types))
})

test_that("ETL Error Handling - Memory and Resource Limits", {
  
  # Test large file handling
  create_large_test_file <- function(size_mb = 100) {
    temp_large <- tempfile(fileext = ".csv")
    large_data <- data.frame(
      EmployeeID = 1:(size_mb * 1000),
      Name = replicate(size_mb * 1000, paste(sample(letters, 10), collapse = "")),
      Department = sample(c("HR", "IT", "Finance"), size_mb * 1000, replace = TRUE),
      stringsAsFactors = FALSE
    )
    write.csv(large_data, temp_large, row.names = FALSE)
    temp_large
  }
  
  # Mock memory limit check
  expect_warning(
    check_memory_availability(file_size_mb = 500),
    regexp = "Large file detected.*memory usage",
    class = "memory_warning"
  )
  
  # Test timeout scenarios
  expect_error(
    with_timeout(Sys.sleep(10), timeout = 2),
    regexp = "Operation timed out",
    class = "timeout_error"
  )
})

test_that("ETL Error Handling - Network and External Dependencies", {
  
  # Test network connectivity issues (mock)
  mock_network_error <- function() {
    stop("Connection timeout", class = c("network_error", "error"))
  }
  
  expect_error(
    tryCatch(mock_network_error(), error = handle_network_error),
    regexp = "Network connectivity issue",
    class = "handled_network_error"
  )
  
  # Test external API failures
  expect_error(
    validate_external_data_source("http://invalid-api-endpoint.com"),
    regexp = "External data source unavailable",
    class = "external_source_error"
  )
})

# ============================================================================
# 5.2.2 DATA LINEAGE TRACKING TESTING
# ============================================================================

test_that("Data Lineage - Source Tracking", {
  
  # Create test data with lineage metadata
  employee_data <- data.frame(
    EmployeeID = 1:5,
    Name = paste("Employee", 1:5),
    Department = c("HR", "IT", "Finance", "IT", "HR"),
    stringsAsFactors = FALSE
  )
  
  # Test lineage creation
  lineage <- create_data_lineage(
    data = employee_data,
    source_file = "employee.csv",
    load_timestamp = Sys.time(),
    transformation_steps = c("load", "validate", "clean")
  )
  
  expect_true("lineage_id" %in% names(lineage))
  expect_true("source_metadata" %in% names(lineage))
  expect_equal(lineage$source_metadata$file_name, "employee.csv")
  expect_true(is.POSIXct(lineage$source_metadata$load_timestamp))
  
  # Test lineage inheritance in transformations
  transformed_data <- employee_data %>%
    mutate(Department_Clean = toupper(Department)) %>%
    add_transformation_lineage(
      parent_lineage_id = lineage$lineage_id,
      transformation = "department_standardization",
      columns_affected = "Department_Clean"
    )
  
  expect_true(has_lineage_tracking(transformed_data))
  expect_equal(get_lineage_parent(transformed_data), lineage$lineage_id)
})

test_that("Data Lineage - Transformation Tracking", {
  
  # Test complex transformation lineage
  base_data <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Salary = c(50000, 60000, 55000)
  )
  
  lineage_tracker <- DataLineageTracker$new()
  
  # Track initial load
  step1_id <- lineage_tracker$track_operation(
    operation = "initial_load",
    input_sources = "employee.csv",
    output_description = "Raw employee data",
    row_count = nrow(base_data),
    column_count = ncol(base_data)
  )
  
  # Track data cleaning
  cleaned_data <- base_data %>%
    filter(!is.na(Salary)) %>%
    mutate(FullName = paste(FirstName, LastName))
  
  step2_id <- lineage_tracker$track_operation(
    operation = "data_cleaning",
    input_sources = step1_id,
    output_description = "Cleaned employee data with full names",
    row_count = nrow(cleaned_data),
    column_count = ncol(cleaned_data),
    transformation_logic = "Remove NA salaries, create FullName column"
  )
  
  # Track aggregation
  salary_summary <- cleaned_data %>%
    group_by(substr(FullName, 1, 1)) %>%
    summarise(avg_salary = mean(Salary), .groups = "drop")
  
  step3_id <- lineage_tracker$track_operation(
    operation = "aggregation",
    input_sources = step2_id,
    output_description = "Salary summary by first letter",
    row_count = nrow(salary_summary),
    column_count = ncol(salary_summary)
  )
  
  # Validate complete lineage chain
  full_lineage <- lineage_tracker$get_full_lineage(step3_id)
  expect_equal(length(full_lineage), 3)
  expect_true(all(c("initial_load", "data_cleaning", "aggregation") %in% 
                  sapply(full_lineage, function(x) x$operation)))
})

test_that("Data Lineage - Column-Level Tracking", {
  
  # Test column-level lineage tracking
  source_data <- data.frame(
    emp_id = 1:4,
    first_nm = c("John", "Jane", "Bob", "Alice"),
    last_nm = c("Doe", "Smith", "Johnson", "Brown"),
    dept_cd = c("HR", "IT", "FIN", "IT"),
    sal_amt = c(50000, 60000, 55000, 65000)
  )
  
  column_lineage <- ColumnLineageTracker$new()
  
  # Track column renaming
  renamed_data <- source_data %>%
    rename(
      EmployeeID = emp_id,
      FirstName = first_nm,
      LastName = last_nm,
      Department = dept_cd,
      Salary = sal_amt
    )
  
  column_lineage$track_column_mapping(
    source_columns = names(source_data),
    target_columns = names(renamed_data),
    operation = "column_standardization"
  )
  
  # Track derived columns
  final_data <- renamed_data %>%
    mutate(
      FullName = paste(FirstName, LastName),
      SalaryBand = case_when(
        Salary < 55000 ~ "Low",
        Salary < 62000 ~ "Medium",
        TRUE ~ "High"
      )
    )
  
  column_lineage$track_derived_columns(
    derived_columns = c("FullName", "SalaryBand"),
    source_columns = list(
      FullName = c("FirstName", "LastName"),
      SalaryBand = c("Salary")
    ),
    transformation_logic = list(
      FullName = "CONCAT(FirstName, ' ', LastName)",
      SalaryBand = "CASE WHEN Salary < 55000 THEN 'Low' ..."
    )
  )
  
  # Validate column lineage
  fullname_lineage <- column_lineage$get_column_lineage("FullName")
  expect_equal(fullname_lineage$source_columns, c("FirstName", "LastName"))
  expect_equal(fullname_lineage$transformation_type, "concatenation")
})

# ============================================================================
# 5.2.3 VERSION CONTROL VALIDATION TESTING
# ============================================================================

test_that("Version Control - Schema Version Validation", {
  
  # Test schema version compatibility
  current_schema <- list(
    version = "2.1.0",
    tables = list(
      employee = list(
        columns = c("EmployeeID", "FirstName", "LastName", "Department", "Salary"),
        required = c("EmployeeID", "FirstName", "LastName"),
        types = list(EmployeeID = "integer", Salary = "numeric")
      )
    )
  )
  
  # Test backward compatibility
  old_data_v1 <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Dept = c("HR", "IT", "Finance")  # Old column name
  )
  
  expect_true(
    validate_schema_compatibility(old_data_v1, current_schema, target_version = "2.1.0")
  )
  
  # Test forward compatibility (should fail)
  future_data_v3 <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Department = c("HR", "IT", "Finance"),
    NewColumn_v3 = c("A", "B", "C")  # Future column
  )
  
  expect_warning(
    validate_schema_compatibility(future_data_v3, current_schema, target_version = "2.1.0"),
    regexp = "Schema version mismatch detected"
  )
})

test_that("Version Control - Data Version Tracking", {
  
  # Test data versioning system
  version_manager <- DataVersionManager$new()
  
  # Create initial data version
  initial_data <- data.frame(
    EmployeeID = 1:5,
    Name = paste("Employee", 1:5),
    Department = c("HR", "IT", "Finance", "IT", "HR"),
    Salary = c(50000, 60000, 55000, 65000, 52000)
  )
  
  v1_id <- version_manager$create_version(
    data = initial_data,
    version_tag = "v1.0.0",
    description = "Initial employee data load",
    author = "etl_process"
  )
  
  expect_true(version_manager$version_exists(v1_id))
  expect_equal(version_manager$get_version_tag(v1_id), "v1.0.0")
  
  # Create updated version
  updated_data <- initial_data %>%
    mutate(Salary = ifelse(EmployeeID == 1, 55000, Salary)) %>%
    add_row(EmployeeID = 6, Name = "Employee 6", Department = "Marketing", Salary = 58000)
  
  v2_id <- version_manager$create_version(
    data = updated_data,
    version_tag = "v1.1.0",
    description = "Updated salary for Employee 1, added new employee",
    author = "etl_process",
    parent_version = v1_id
  )
  
  # Test version comparison
  changes <- version_manager$compare_versions(v1_id, v2_id)
  expect_true("modified_rows" %in% names(changes))
  expect_true("added_rows" %in% names(changes))
  expect_equal(length(changes$added_rows), 1)
  expect_equal(length(changes$modified_rows), 1)
})

test_that("Version Control - Migration Testing", {
  
  # Test data migration between versions
  migration_engine <- DataMigrationEngine$new()
  
  # Define migration from v1.0 to v2.0
  migration_v1_to_v2 <- function(data) {
    data %>%
      rename(DepartmentCode = Department) %>%
      mutate(
        Department = case_when(
          DepartmentCode == "HR" ~ "Human Resources",
          DepartmentCode == "IT" ~ "Information Technology",
          DepartmentCode == "Finance" ~ "Finance",
          TRUE ~ DepartmentCode
        ),
        CreatedDate = Sys.Date()
      ) %>%
      select(-DepartmentCode)
  }
  
  migration_engine$register_migration(
    from_version = "1.0.0",
    to_version = "2.0.0",
    migration_function = migration_v1_to_v2,
    rollback_function = function(data) {
      data %>%
        select(-CreatedDate) %>%
        mutate(Department = case_when(
          Department == "Human Resources" ~ "HR",
          Department == "Information Technology" ~ "IT",
          TRUE ~ Department
        ))
    }
  )
  
  # Test forward migration
  v1_data <- data.frame(
    EmployeeID = 1:3,
    Name = c("John", "Jane", "Bob"),
    Department = c("HR", "IT", "Finance")
  )
  
  migrated_data <- migration_engine$migrate_data(
    data = v1_data,
    from_version = "1.0.0",
    to_version = "2.0.0"
  )
  
  expect_equal(migrated_data$Department[1], "Human Resources")
  expect_true("CreatedDate" %in% names(migrated_data))
  
  # Test rollback migration
  rolled_back_data <- migration_engine$rollback_migration(
    data = migrated_data,
    from_version = "2.0.0",
    to_version = "1.0.0"
  )
  
  expect_equal(rolled_back_data$Department[1], "HR")
  expect_false("CreatedDate" %in% names(rolled_back_data))
})

# ============================================================================
# 5.2.4 ROLLBACK CAPABILITY TESTING
# ============================================================================

test_that("Rollback - Transaction Rollback", {
  
  # Test transactional ETL operations
  transaction_manager <- ETLTransactionManager$new()
  
  test_data <- data.frame(
    EmployeeID = 1:5,
    Name = paste("Employee", 1:5),
    Department = c("HR", "IT", "Finance", "IT", "HR"),
    Salary = c(50000, 60000, 55000, 65000, 52000)
  )
  
  # Begin transaction
  transaction_id <- transaction_manager$begin_transaction("test_etl_process")
  
  # Simulate successful operations
  transaction_manager$log_operation(
    transaction_id = transaction_id,
    operation = "data_load",
    status = "success",
    checkpoint_data = list(data = test_data, row_count = nrow(test_data))
  )
  
  # Simulate data transformation
  transformed_data <- test_data %>%
    mutate(SalaryBand = cut(Salary, breaks = c(0, 55000, 62000, Inf), 
                           labels = c("Low", "Medium", "High")))
  
  transaction_manager$log_operation(
    transaction_id = transaction_id,
    operation = "data_transform",
    status = "success",
    checkpoint_data = list(data = transformed_data, row_count = nrow(transformed_data))
  )
  
  # Simulate validation failure
  expect_error({
    if (any(is.na(transformed_data$SalaryBand))) {
      stop("Validation failed: NULL values in SalaryBand")
    }
    transaction_manager$log_operation(
      transaction_id = transaction_id,
      operation = "data_validation",
      status = "failed",
      error_message = "Validation failed"
    )
  })
  
  # Test rollback
  rollback_result <- transaction_manager$rollback_transaction(transaction_id)
  expect_true(rollback_result$success)
  expect_equal(rollback_result$restored_checkpoint, "data_load")
})

test_that("Rollback - Point-in-Time Recovery", {
  
  # Test point-in-time data recovery
  recovery_manager <- PointInTimeRecovery$new()
  
  # Create snapshots at different time points
  t1 <- Sys.time()
  snapshot1_data <- data.frame(
    EmployeeID = 1:3,
    Name = c("John", "Jane", "Bob"),
    Status = c("Active", "Active", "Active")
  )
  
  recovery_manager$create_snapshot(
    snapshot_id = "snapshot_t1",
    data = snapshot1_data,
    timestamp = t1,
    description = "Initial state"
  )
  
  Sys.sleep(1)
  t2 <- Sys.time()
  snapshot2_data <- snapshot1_data %>%
    mutate(Status = ifelse(EmployeeID == 2, "Inactive", Status))
  
  recovery_manager$create_snapshot(
    snapshot_id = "snapshot_t2",
    data = snapshot2_data,
    timestamp = t2,
    description = "Employee 2 deactivated"
  )
  
  Sys.sleep(1)
  t3 <- Sys.time()
  snapshot3_data <- snapshot2_data %>%
    filter(EmployeeID != 3)  # Employee 3 removed
  
  recovery_manager$create_snapshot(
    snapshot_id = "snapshot_t3",
    data = snapshot3_data,
    timestamp = t3,
    description = "Employee 3 removed"
  )
  
  # Test recovery to specific point in time
  recovered_data <- recovery_manager$recover_to_timestamp(t2 + 0.5)
  expect_equal(nrow(recovered_data), 3)  # Should have all 3 employees
  expect_equal(recovered_data$Status[2], "Inactive")  # Employee 2 should be inactive
  
  # Test recovery to earliest point
  earliest_data <- recovery_manager$recover_to_timestamp(t1 + 0.5)
  expect_equal(nrow(earliest_data), 3)
  expect_true(all(earliest_data$Status == "Active"))
})

test_that("Rollback - Cascading Rollback", {
  
  # Test cascading rollback across dependent operations
  dependency_manager <- DependencyRollbackManager$new()
  
  # Define dependency chain: A -> B -> C
  dependency_manager$define_dependency("operation_A", "operation_B")
  dependency_manager$define_dependency("operation_B", "operation_C")
  
  # Simulate successful execution
  dependency_manager$mark_operation_complete("operation_A", 
    result = data.frame(id = 1:5, value_a = letters[1:5]))
  
  dependency_manager$mark_operation_complete("operation_B",
    result = data.frame(id = 1:5, value_b = LETTERS[1:5]))
  
  dependency_manager$mark_operation_complete("operation_C",
    result = data.frame(id = 1:5, value_c = 1:5))
  
  # Simulate failure in operation B requiring rollback
  rollback_plan <- dependency_manager$create_rollback_plan("operation_B")
  
  expect_true("operation_C" %in% rollback_plan$operations_to_rollback)
  expect_true("operation_B" %in% rollback_plan$operations_to_rollback)
  expect_false("operation_A" %in% rollback_plan$operations_to_rollback)
  
  # Execute cascading rollback
  rollback_result <- dependency_manager$execute_rollback_plan(rollback_plan)
  expect_true(rollback_result$success)
  expect_equal(length(rollback_result$rolled_back_operations), 2)
})

# ============================================================================
# 5.2.5 INCREMENTAL UPDATE ACCURACY TESTING
# ============================================================================

test_that("Incremental Updates - Delta Detection", {
  
  # Test accurate delta detection between datasets
  delta_processor <- DeltaProcessor$new()
  
  # Original dataset
  original_data <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("John", "Jane", "Bob", "Alice", "Charlie"),
    LastName = c("Doe", "Smith", "Johnson", "Brown", "Wilson"),
    Department = c("HR", "IT", "Finance", "IT", "HR"),
    Salary = c(50000, 60000, 55000, 65000, 52000),
    LastModified = as.POSIXct("2023-01-01 10:00:00")
  )
  
  # Updated dataset with changes
  updated_data <- data.frame(
    EmployeeID = c(1:5, 6),  # Added employee 6
    FirstName = c("John", "Jane", "Robert", "Alice", "Charlie", "David"),  # Bob -> Robert
    LastName = c("Doe", "Smith", "Johnson", "Brown", "Wilson", "Miller"),
    Department = c("IT", "IT", "Finance", "IT", "Marketing", "HR"),  # John: HR->IT, Charlie: HR->Marketing
    Salary = c(55000, 60000, 55000, 68000, 52000, 48000),  # John: +5000, Alice: +3000
    LastModified = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:00:00", 
                               "2023-01-15 14:30:00", "2023-01-10 09:15:00",
                               "2023-01-12 16:45:00", "2023-01-20 11:00:00"))
  )
  
  # Detect changes
  delta_result <- delta_processor$detect_changes(
    original_data = original_data,
    updated_data = updated_data,
    key_column = "EmployeeID",
    exclude_columns = "LastModified"
  )
  
  # Validate delta detection
  expect_equal(nrow(delta_result$inserted), 1)  # Employee 6
  expect_equal(delta_result$inserted$EmployeeID, 6)
  
  expect_equal(nrow(delta_result$updated), 4)  # Employees 1, 3, 4, 5
  expect_true(1 %in% delta_result$updated$EmployeeID)  # John
  expect_true(3 %in% delta_result$updated$EmployeeID)  # Robert (Bob)
  expect_true(4 %in% delta_result$updated$EmployeeID)  # Alice
  expect_true(5 %in% delta_result$updated$EmployeeID)  # Charlie
  
  expect_equal(nrow(delta_result$unchanged), 1)  # Employee 2 (Jane)
  expect_equal(delta_result$unchanged$EmployeeID, 2)
  
  expect_equal(nrow(delta_result$deleted), 0)  # No deletions
})

test_that("Incremental Updates - Change Data Capture", {
  
  # Test change data capture functionality
  cdc_processor := ChangeDataCaptureProcessor$new()
  
  # Setup CDC tracking
  base_data <- data.frame(
    EmployeeID = 1:4,
    Name = c("John", "Jane", "Bob", "Alice"),
    Salary = c(50000, 60000, 55000, 65000),
    Version = 1,
    CreatedAt = Sys.time(),
    UpdatedAt = Sys.time()
  )
  
  cdc_processor$initialize_tracking(base_data, key_column = "EmployeeID")
  
  # Simulate changes over time
  Sys.sleep(1)
  
  # Change 1: Update John's salary
  change1 <- data.frame(
    EmployeeID = 1,
    Name = "John",
    Salary = 55000,  # Increased by 5000
    Version = 2,
    CreatedAt = base_data$CreatedAt[1],
    UpdatedAt = Sys.time()
  )
  
  cdc_result1 <- cdc_processor$capture_changes(change1)
  expect_equal(cdc_result1$change_type, "UPDATE")
  expect_equal(cdc_result1$changed_columns, "Salary")
  expect_equal(cdc_result1$old_values$Salary, 50000)
  expect_equal(cdc_result1$new_values$Salary, 55000)
  
  # Change 2: Insert new employee
  change2 <- data.frame(
    EmployeeID = 5,
    Name = "Eve",
    Salary = 58000,
    Version = 1,
    CreatedAt = Sys.time(),
    UpdatedAt = Sys.time()
  )
  
  cdc_result2 <- cdc_processor$capture_changes(change2)
  expect_equal(cdc_result2$change_type, "INSERT")
  expect_true(is.null(cdc_result2$old_values))
  expect_equal(cdc_result2$new_values$Name, "Eve")
  
  # Change 3: Delete employee
  cdc_result3 <- cdc_processor$capture_deletion(EmployeeID = 3)
  expect_equal(cdc_result3$change_type, "DELETE")
  expect_equal(cdc_result3$old_values$Name, "Bob")
  expect_true(is.null(cdc_result3$new_values))
})

test_that("Incremental Updates - Merge Strategy Validation", {
  
  # Test different merge strategies for incremental updates
  merge_processor <- IncrementalMergeProcessor$new()
  
  target_data <- data.frame(
    EmployeeID = 1:4,
    Name = c("John", "Jane", "Bob", "Alice"),
    Department = c("HR", "IT", "Finance", "IT"),
    Salary = c(50000, 60000, 55000, 65000),
    LastUpdated = as.POSIXct("2023-01-01 10:00:00")
  )
  
  source_data <- data.frame(
    EmployeeID = c(2, 3, 4, 5),
    Name = c("Jane", "Robert", "Alice", "Charlie"),  # Bob -> Robert
    Department = c("IT", "Finance", "Marketing", "HR"),  # Alice: IT -> Marketing
    Salary = c(62000, 55000, 68000, 52000),  # Jane: +2000, Alice: +3000
    LastUpdated = as.POSIXct("2023-01-15 14:30:00")
  )
  
  # Test UPSERT strategy
  upsert_result <- merge_processor$execute_merge(
    target_data = target_data,
    source_data = source_data,
    key_column = "EmployeeID",
    strategy = "UPSERT"
  )
  
  expect_equal(nrow(upsert_result), 5)  # 4 original + 1 new
  expect_true(5 %in% upsert_result$EmployeeID)  # Charlie added
  expect_equal(upsert_result$Name[upsert_result$EmployeeID == 3], "Robert")  # Bob -> Robert
  expect_equal(upsert_result$Department[upsert_result$EmployeeID == 4], "Marketing")  # Alice dept change
  
  # Test