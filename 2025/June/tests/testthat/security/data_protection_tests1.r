# ==============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - DATA PROTECTION UNIT TESTS
# ==============================================================================
# Comprehensive unit tests for GDPR compliance, data retention, secure disposal,
# and cross-border data transfer compliance
# 
# Author: akhapwoyaco
# Version: 1.0.0
# ==============================================================================

# Load required libraries for testing
library(testthat)
library(mockery)
library(lubridate)
library(digest)
library(sodium)
library(openssl)
library(DBI)
library(RSQLite)

# ==============================================================================
# 1. GDPR COMPLIANCE VALIDATION TESTS
# ==============================================================================

context("GDPR Compliance Validation")

# Mock GDPR compliance class
GDPRCompliance <- R6::R6Class("GDPRCompliance",
  public = list(
    initialize = function() {
      private$lawful_basis <- c("consent", "contract", "legal_obligation", 
                               "vital_interests", "public_task", "legitimate_interests")
      private$special_categories <- c("racial_ethnic", "political_opinions", 
                                     "religious_beliefs", "trade_union", 
                                     "genetic", "biometric", "health", "sex_life")
      private$data_subjects <- data.frame()
      private$consent_records <- data.frame()
      private$processing_log <- data.frame()
    },
    
    validate_lawful_basis = function(purpose, basis) {
      if (!(basis %in% private$lawful_basis)) {
        stop("Invalid lawful basis for processing")
      }
      return(TRUE)
    },
    
    check_special_category = function(data_field) {
      special_patterns <- c("ethnic", "religion", "health", "genetic", "biometric")
      return(any(sapply(special_patterns, function(x) grepl(x, tolower(data_field)))))
    },
    
    validate_consent = function(subject_id, purpose, timestamp = Sys.time()) {
      consent_key <- paste(subject_id, purpose, sep = "_")
      return(consent_key %in% private$consent_records$consent_key)
    },
    
    log_processing_activity = function(purpose, data_categories, legal_basis, 
                                      recipients = NULL, retention_period = NULL) {
      activity <- data.frame(
        timestamp = Sys.time(),
        purpose = purpose,
        data_categories = paste(data_categories, collapse = ";"),
        legal_basis = legal_basis,
        recipients = ifelse(is.null(recipients), "", paste(recipients, collapse = ";")),
        retention_period = ifelse(is.null(retention_period), "", retention_period),
        stringsAsFactors = FALSE
      )
      private$processing_log <- rbind(private$processing_log, activity)
      return(TRUE)
    },
    
    validate_data_minimization = function(collected_fields, required_fields) {
      excess_fields <- setdiff(collected_fields, required_fields)
      if (length(excess_fields) > 0) {
        warning(paste("Potential data minimization violation. Excess fields:", 
                     paste(excess_fields, collapse = ", ")))
        return(FALSE)
      }
      return(TRUE)
    },
    
    check_data_accuracy = function(employee_data) {
      # Check for data quality issues that could affect accuracy
      accuracy_issues <- list()
      
      # Check for missing critical fields
      critical_fields <- c("EmployeeID", "FirstName", "LastName", "HireDate")
      missing_critical <- critical_fields[!critical_fields %in% names(employee_data)]
      if (length(missing_critical) > 0) {
        accuracy_issues$missing_critical <- missing_critical
      }
      
      # Check for impossible dates
      if ("HireDate" %in% names(employee_data)) {
        future_dates <- employee_data$HireDate > Sys.Date()
        if (any(future_dates, na.rm = TRUE)) {
          accuracy_issues$future_hire_dates <- sum(future_dates, na.rm = TRUE)
        }
      }
      
      # Check for negative ages
      if ("Age" %in% names(employee_data)) {
        negative_ages <- employee_data$Age < 0
        if (any(negative_ages, na.rm = TRUE)) {
          accuracy_issues$negative_ages <- sum(negative_ages, na.rm = TRUE)
        }
      }
      
      return(length(accuracy_issues) == 0)
    }
  ),
  
  private = list(
    lawful_basis = NULL,
    special_categories = NULL,
    data_subjects = NULL,
    consent_records = NULL,
    processing_log = NULL
  )
)

# Test GDPR lawful basis validation
test_that("GDPR lawful basis validation works correctly", {
  gdpr <- GDPRCompliance$new()
  
  # Test valid lawful basis
  expect_true(gdpr$validate_lawful_basis("HR Analytics", "legitimate_interests"))
  expect_true(gdpr$validate_lawful_basis("Payroll", "contract"))
  expect_true(gdpr$validate_lawful_basis("Safety Training", "legal_obligation"))
  
  # Test invalid lawful basis
  expect_error(gdpr$validate_lawful_basis("Marketing", "invalid_basis"))
  expect_error(gdpr$validate_lawful_basis("Profiling", ""))
  expect_error(gdpr$validate_lawful_basis("Analytics", "personal_interest"))
})

# Test special category data detection
test_that("Special category data detection works correctly", {
  gdpr <- GDPRCompliance$new()
  
  # Test fields that should be flagged as special categories
  expect_true(gdpr$check_special_category("Ethnicity"))
  expect_true(gdpr$check_special_category("Religious_Beliefs"))
  expect_true(gdpr$check_special_category("Health_Status"))
  expect_true(gdpr$check_special_category("Genetic_Information"))
  expect_true(gdpr$check_special_category("Biometric_Data"))
  
  # Test fields that should NOT be flagged
  expect_false(gdpr$check_special_category("FirstName"))
  expect_false(gdpr$check_special_category("Department"))
  expect_false(gdpr$check_special_category("Salary"))
  expect_false(gdpr$check_special_category("JobRole"))
  
  # Test edge cases
  expect_true(gdpr$check_special_category("ETHNIC_ORIGIN"))  # Case insensitive
  expect_true(gdpr$check_special_category("employee_health_record"))
  expect_false(gdpr$check_special_category(""))  # Empty string
  expect_false(gdpr$check_special_category(NULL))  # NULL input
})

# Test data minimization validation
test_that("Data minimization validation works correctly", {
  gdpr <- GDPRCompliance$new()
  
  # Test compliant data collection
  required_fields <- c("EmployeeID", "FirstName", "LastName", "Department", "HireDate")
  collected_fields <- c("EmployeeID", "FirstName", "LastName", "Department", "HireDate")
  expect_true(gdpr$validate_data_minimization(collected_fields, required_fields))
  
  # Test subset collection (should pass)
  collected_subset <- c("EmployeeID", "FirstName", "LastName", "Department")
  expect_true(gdpr$validate_data_minimization(collected_subset, required_fields))
  
  # Test excessive data collection (should fail with warning)
  collected_excess <- c("EmployeeID", "FirstName", "LastName", "Department", 
                       "HireDate", "PersonalPhone", "HomeAddress", "SocialMedia")
  expect_warning(gdpr$validate_data_minimization(collected_excess, required_fields))
  expect_false(suppressWarnings(gdpr$validate_data_minimization(collected_excess, required_fields)))
  
  # Test edge cases
  expect_true(gdpr$validate_data_minimization(c(), c()))  # Empty sets
  expect_true(gdpr$validate_data_minimization(c(), required_fields))  # No collection
})

# Test data accuracy validation
test_that("Data accuracy validation works correctly", {
  gdpr <- GDPRCompliance$new()
  
  # Test accurate data
  accurate_data <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("John", "Jane", "Bob", "Alice", "Charlie"),
    LastName = c("Doe", "Smith", "Johnson", "Brown", "Davis"),
    HireDate = as.Date(c("2020-01-15", "2019-03-22", "2021-07-01", "2018-11-30", "2022-02-14")),
    Age = c(30, 25, 35, 28, 32),
    stringsAsFactors = FALSE
  )
  expect_true(gdpr$check_data_accuracy(accurate_data))
  
  # Test data with missing critical fields
  missing_critical <- data.frame(
    FirstName = c("John", "Jane"),
    LastName = c("Doe", "Smith"),
    stringsAsFactors = FALSE
  )
  expect_false(gdpr$check_data_accuracy(missing_critical))
  
  # Test data with future hire dates
  future_dates <- data.frame(
    EmployeeID = 1:2,
    FirstName = c("John", "Jane"),
    LastName = c("Doe", "Smith"),
    HireDate = as.Date(c("2020-01-15", "2025-12-31")),  # Future date
    Age = c(30, 25),
    stringsAsFactors = FALSE
  )
  expect_false(gdpr$check_data_accuracy(future_dates))
  
  # Test data with negative ages
  negative_ages <- data.frame(
    EmployeeID = 1:2,
    FirstName = c("John", "Jane"),
    LastName = c("Doe", "Smith"),
    HireDate = as.Date(c("2020-01-15", "2019-03-22")),
    Age = c(30, -5),  # Negative age
    stringsAsFactors = FALSE
  )
  expect_false(gdpr$check_data_accuracy(negative_ages))
})

# Test processing activity logging
test_that("Processing activity logging works correctly", {
  gdpr <- GDPRCompliance$new()
  
  # Test logging valid activity
  expect_true(gdpr$log_processing_activity(
    purpose = "HR Analytics",
    data_categories = c("Employee Demographics", "Performance Data"),
    legal_basis = "legitimate_interests",
    recipients = c("HR Team", "Management"),
    retention_period = "7 years"
  ))
  
  # Test logging with minimal parameters
  expect_true(gdpr$log_processing_activity(
    purpose = "Payroll Processing",
    data_categories = c("Salary Information"),
    legal_basis = "contract"
  ))
  
  # Test multiple logging activities
  expect_true(gdpr$log_processing_activity("Training", "Training Records", "legal_obligation"))
  expect_true(gdpr$log_processing_activity("Benefits", "Benefits Data", "contract"))
  
  # Verify log structure
  log_data <- gdpr$.__enclos_env__$private$processing_log
  expect_true(nrow(log_data) >= 4)
  expect_true(all(c("timestamp", "purpose", "data_categories", "legal_basis") %in% names(log_data)))
})

# ==============================================================================
# 2. DATA RETENTION POLICY ENFORCEMENT TESTS
# ==============================================================================

context("Data Retention Policy Enforcement")

# Mock Data Retention Policy class
DataRetentionPolicy <- R6::R6Class("DataRetentionPolicy",
  public = list(
    initialize = function() {
      private$retention_rules <- data.frame(
        data_category = c("employee_active", "employee_terminated", "performance_reviews", 
                         "training_records", "disciplinary_records", "payroll_records"),
        retention_period_years = c(NA, 7, 5, 3, 10, 7),
        trigger_event = c("employment_end", "termination_date", "review_date", 
                         "training_completion", "incident_date", "payment_date"),
        stringsAsFactors = FALSE
      )
      private$retention_log <- data.frame()
    },
    
    get_retention_period = function(data_category) {
      rule <- private$retention_rules[private$retention_rules$data_category == data_category, ]
      if (nrow(rule) == 0) {
        stop(paste("No retention rule found for category:", data_category))
      }
      return(rule$retention_period_years)
    },
    
    calculate_deletion_date = function(data_category, trigger_date) {
      retention_years <- self$get_retention_period(data_category)
      if (is.na(retention_years)) {
        return(NA)  # No automatic deletion for active employees
      }
      return(trigger_date + years(retention_years))
    },
    
    identify_expired_records = function(data_records, category_field = "category", 
                                       date_field = "trigger_date") {
      expired_records <- data.frame()
      
      for (i in seq_len(nrow(data_records))) {
        record <- data_records[i, ]
        category <- record[[category_field]]
        trigger_date <- as.Date(record[[date_field]])
        
        deletion_date <- self$calculate_deletion_date(category, trigger_date)
        
        if (!is.na(deletion_date) && deletion_date <= Sys.Date()) {
          expired_records <- rbind(expired_records, record)
        }
      }
      
      return(expired_records)
    },
    
    validate_retention_compliance = function(data_records, category_field = "category", 
                                           date_field = "trigger_date") {
      expired_records <- self$identify_expired_records(data_records, category_field, date_field)
      
      compliance_report <- list(
        total_records = nrow(data_records),
        expired_records = nrow(expired_records),
        compliance_rate = (nrow(data_records) - nrow(expired_records)) / nrow(data_records),
        expired_data = expired_records
      )
      
      return(compliance_report)
    },
    
    log_retention_action = function(action_type, record_ids, category, reason = "") {
      log_entry <- data.frame(
        timestamp = Sys.time(),
        action_type = action_type,
        record_count = length(record_ids),
        category = category,
        record_ids = paste(record_ids, collapse = ";"),
        reason = reason,
        stringsAsFactors = FALSE
      )
      private$retention_log <- rbind(private$retention_log, log_entry)
      return(TRUE)
    },
    
    schedule_automated_deletion = function(data_records, category_field = "category", 
                                         date_field = "trigger_date") {
      expired_records <- self$identify_expired_records(data_records, category_field, date_field)
      
      if (nrow(expired_records) > 0) {
        # Log the scheduled deletion
        self$log_retention_action(
          action_type = "scheduled_deletion",
          record_ids = expired_records$record_id,
          category = paste(unique(expired_records[[category_field]]), collapse = ";"),
          reason = "Automatic deletion due to retention policy expiration"
        )
        
        return(list(
          scheduled_count = nrow(expired_records),
          scheduled_records = expired_records$record_id
        ))
      }
      
      return(list(scheduled_count = 0, scheduled_records = c()))
    }
  ),
  
  private = list(
    retention_rules = NULL,
    retention_log = NULL
  )
)

# Test retention period calculation
test_that("Retention period calculation works correctly", {
  retention_policy <- DataRetentionPolicy$new()
  
  # Test valid categories
  expect_equal(retention_policy$get_retention_period("employee_terminated"), 7)
  expect_equal(retention_policy$get_retention_period("performance_reviews"), 5)
  expect_equal(retention_policy$get_retention_period("training_records"), 3)
  expect_equal(retention_policy$get_retention_period("disciplinary_records"), 10)
  expect_equal(retention_policy$get_retention_period("payroll_records"), 7)
  
  # Test active employees (no automatic deletion)
  expect_true(is.na(retention_policy$get_retention_period("employee_active")))
  
  # Test invalid category
  expect_error(retention_policy$get_retention_period("invalid_category"))
  expect_error(retention_policy$get_retention_period(""))
})

# Test deletion date calculation
test_that("Deletion date calculation works correctly", {
  retention_policy <- DataRetentionPolicy$new()
  
  # Test with specific dates
  termination_date <- as.Date("2015-12-31")
  expected_deletion <- as.Date("2022-12-31")
  actual_deletion <- retention_policy$calculate_deletion_date("employee_terminated", termination_date)
  expect_equal(actual_deletion, expected_deletion)
  
  # Test with performance reviews
  review_date <- as.Date("2018-06-15")
  expected_deletion <- as.Date("2023-06-15")
  actual_deletion <- retention_policy$calculate_deletion_date("performance_reviews", review_date)
  expect_equal(actual_deletion, expected_deletion)
  
  # Test with active employees (should return NA)
  hire_date <- as.Date("2020-01-01")
  expect_true(is.na(retention_policy$calculate_deletion_date("employee_active", hire_date)))
  
  # Test edge cases
  expect_error(retention_policy$calculate_deletion_date("invalid_category", Sys.Date()))
})

# Test expired records identification
test_that("Expired records identification works correctly", {
  retention_policy <- DataRetentionPolicy$new()
  
  # Create test data with mixed expired/valid records
  test_data <- data.frame(
    record_id = 1:6,
    category = c("employee_terminated", "employee_terminated", "performance_reviews", 
                "training_records", "disciplinary_records", "payroll_records"),
    trigger_date = as.Date(c("2010-01-01", "2020-01-01", "2015-06-15", 
                           "2018-03-22", "2005-12-31", "2021-12-31")),
    stringsAsFactors = FALSE
  )
  
  # Test expired records identification
  expired_records <- retention_policy$identify_expired_records(test_data)
  
  # Should identify records 1, 3, 4, 5 as expired (based on 2024 current date assumption)
  expect_true(nrow(expired_records) > 0)
  expect_true(all(expired_records$record_id %in% c(1, 3, 4, 5)))
  
  # Test with no expired records
  future_data <- data.frame(
    record_id = 1:2,
    category = c("employee_terminated", "performance_reviews"),
    trigger_date = as.Date(c("2023-01-01", "2023-06-15")),
    stringsAsFactors = FALSE
  )
  
  future_expired <- retention_policy$identify_expired_records(future_data)
  expect_equal(nrow(future_expired), 0)
  
  # Test with empty data
  empty_data <- data.frame(
    record_id = integer(0),
    category = character(0),
    trigger_date = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
  
  empty_expired <- retention_policy$identify_expired_records(empty_data)
  expect_equal(nrow(empty_expired), 0)
})

# Test retention compliance validation
test_that("Retention compliance validation works correctly", {
  retention_policy <- DataRetentionPolicy$new()
  
  # Test data with compliance issues
  test_data <- data.frame(
    record_id = 1:8,
    category = c("employee_terminated", "employee_terminated", "performance_reviews", 
                "training_records", "disciplinary_records", "payroll_records",
                "employee_active", "employee_active"),
    trigger_date = as.Date(c("2010-01-01", "2020-01-01", "2015-06-15", 
                           "2018-03-22", "2005-12-31", "2021-12-31",
                           "2020-01-01", "2022-01-01")),
    stringsAsFactors = FALSE
  )
  
  compliance_report <- retention_policy$validate_retention_compliance(test_data)
  
  # Verify report structure
  expect_true(all(c("total_records", "expired_records", "compliance_rate", "expired_data") %in% 
                 names(compliance_report)))
  expect_equal(compliance_report$total_records, 8)
  expect_true(compliance_report$expired_records > 0)
  expect_true(compliance_report$compliance_rate < 1.0)
  expect_true(compliance_report$compliance_rate >= 0.0)
  
  # Test with fully compliant data
  compliant_data <- data.frame(
    record_id = 1:3,
    category = c("employee_active", "employee_terminated", "performance_reviews"),
    trigger_date = as.Date(c("2020-01-01", "2022-01-01", "2023-01-01")),
    stringsAsFactors = FALSE
  )
  
  compliant_report <- retention_policy$validate_retention_compliance(compliant_data)
  expect_equal(compliant_report$expired_records, 0)
  expect_equal(compliant_report$compliance_rate, 1.0)
})

# Test retention action logging
test_that("Retention action logging works correctly", {
  retention_policy <- DataRetentionPolicy$new()
  
  # Test logging deletion action
  expect_true(retention_policy$log_retention_action(
    action_type = "deletion",
    record_ids = c(1, 2, 3),
    category = "employee_terminated",
    reason = "Retention period expired"
  ))
  
  # Test logging archive action
  expect_true(retention_policy$log_retention_action(
    action_type = "archive",
    record_ids = c(4, 5),
    category = "performance_reviews",
    reason = "Scheduled archival"
  ))
  
  # Test logging with single record
  expect_true(retention_policy$log_retention_action(
    action_type = "manual_deletion",
    record_ids = c(6),
    category = "disciplinary_records",
    reason = "Data subject request"
  ))
  
  # Verify log structure
  log_data <- retention_policy$.__enclos_env__$private$retention_log
  expect_true(nrow(log_data) == 3)
  expect_true(all(c("timestamp", "action_type", "record_count", "category", "record_ids", "reason") %in% 
                 names(log_data)))
  
  # Test edge cases
  expect_true(retention_policy$log_retention_action("test", c(), "test_category", ""))
})

# Test automated deletion scheduling
test_that("Automated deletion scheduling works correctly", {
  retention_policy <- DataRetentionPolicy$new()
  
  # Test data with expired records
  test_data <- data.frame(
    record_id = 1:5,
    category = c("employee_terminated", "performance_reviews", "training_records", 
                "employee_active", "payroll_records"),
    trigger_date = as.Date(c("2010-01-01", "2015-06-15", "2018-03-22", 
                           "2020-01-01", "2012-12-31")),
    stringsAsFactors = FALSE
  )
  
  schedule_result <- retention_policy$schedule_automated_deletion(test_data)
  
  # Should schedule deletions for expired records
  expect_true(schedule_result$scheduled_count > 0)
  expect_true(length(schedule_result$scheduled_records) > 0)
  expect_true(all(schedule_result$scheduled_records %in% test_data$record_id))
  
  # Test with no expired records
  current_data <- data.frame(
    record_id = 1:2,
    category = c("employee_active", "employee_terminated"),
    trigger_date = as.Date(c("2020-01-01", "2023-01-01")),
    stringsAsFactors = FALSE
  )
  
  current_schedule <- retention_policy$schedule_automated_deletion(current_data)
  expect_equal(current_schedule$scheduled_count, 0)
  expect_equal(length(current_schedule$scheduled_records), 0)
  
  # Verify logging occurred
  log_data <- retention_policy$.__enclos_env__$private$retention_log
  expect_true(nrow(log_data) > 0)
  expect_true(any(log_data$action_type == "scheduled_deletion"))
})

# ==============================================================================
# 3. SECURE DATA DISPOSAL TESTS
# ==============================================================================

context("Secure Data Disposal")

# Mock Secure Data Disposal class
SecureDataDisposal <- R6::R6Class("SecureDataDisposal",
  public = list(
    initialize = function() {
      private$disposal_log <- data.frame()
      private$disposal_methods <- c("secure_delete", "crypto_shred", "overwrite", "physical_destruction")
      private$verification_required <- TRUE
    },
    
    secure_delete_record = function(record_id, table_name, method = "secure_delete") {
      if (!(method %in% private$disposal_methods)) {
        stop(paste("Invalid disposal method:", method))
      }
      
      # Simulate secure deletion process
      deletion_steps <- self$execute_secure_deletion(record_id, table_name, method)
      
      # Log the disposal
      self$log_disposal_action(
        record_id = record_id,
        table_name = table_name,
        method = method,
        verification_hash = deletion_steps$verification_hash,
        status = "completed"
      )
      
      return(deletion_steps)
    },
    
    execute_secure_deletion = function(record_id, table_name, method) {
      # Simulate multi-pass overwrite for secure deletion
      passes <- switch(method,
        "secure_delete" = 3,
        "crypto_shred" = 1,
        "overwrite" = 7,
        "physical_destruction" = 1
      )
      
      # Generate verification hash before deletion
      pre_deletion_hash <- digest::digest(paste(record_id, table_name, Sys.time()), 
                                         algo = "sha256")
      
      # Simulate overwrite passes
      overwrite_hashes <- character(passes)
      for (i in seq_len(passes)) {
        # Simulate random overwrite
        random_data <- paste(sample(c(0:9, letters, LETTERS), 100, replace = TRUE), 
                           collapse = "")
        overwrite_hashes[i] <- digest::digest(random_data, algo = "sha256")
      }
      
      # Generate post-deletion verification hash
      post_deletion_hash <- digest::digest(paste(overwrite_hashes, collapse = ""), 
                                         algo = "sha256")
      
      return(list(
        pre_deletion_hash = pre_deletion_hash,
        overwrite_passes = passes,
        overwrite_hashes = overwrite_hashes,
        post_deletion_hash = post_deletion_hash,
        verification_hash = post_deletion_hash,
        timestamp = Sys.time()
      ))
    },
    
    crypto_shred_data = function(data_vector, encryption_key = NULL) {
      if (is.null(encryption_key)) {
        # Generate random key for crypto shredding
        encryption_key <- sodium::random(32)
      }
      
      # Encrypt data
      encrypted_data <- sodium::simple_encrypt(serialize(data_vector, NULL), encryption_key)
      
      # "Shred" by overwriting the key
      shredded_key <- sodium::random(32)  # New random key
      
      # Attempt to decrypt with shredded key (should fail)
      decryption_attempt <- tryCatch({
        sodium::simple_decrypt(encrypted_data, shredded_key)
        FALSE  # If decryption succeeds, shredding failed
      }, error = function(e) {
        TRUE  # If decryption fails, shredding succeeded
      })
      
      return(list(
        original_key_hash = digest::digest(encryption_key, algo = "sha256"),
        shredded_key_hash = digest::digest(shredded_key, algo = "sha256"),
        crypto_shred_successful = decryption_attempt,
        encrypted_data_hash = digest::digest(encrypted_data, algo = "sha256"),
        timestamp = Sys.time()
      ))
    },
    
    verify_deletion_completeness = function(record_id, table_name, expected_hash) {
      # Simulate verification by checking if record exists
      # In real implementation, this would query the actual database
      
      # Generate current state hash
      current_state_hash <- digest::digest(paste("deleted", record_id, table_name, Sys.time()), 
                                          algo = "sha256")
      
      # Check if deletion was successful
      deletion_verified <- current_state_hash != expected_hash
      
      return(list(
        record_id = record_id,
        table_name = table_name,
        expected_hash = expected_hash,
        current_state_hash = current_state_hash,
        verification_successful = deletion_verified,
        verification_timestamp = Sys.time()
      ))
    },
    
    log_disposal_action = function(record_id, table_name, method, verification_hash, 
                                  status, reason = "") {
      log_entry <- data.frame(
        timestamp = Sys.time(),
        record_id = record_id,
        table_name = table_name,
        disposal_method = method,
        verification_hash = verification_hash,
        status = status,
        reason = reason,
        stringsAsFactors = FALSE
      )
      private$disposal_log <- rbind(private$disposal_log, log_entry)
      return(TRUE)
    },
    
    bulk_secure_delete = function(record_ids, table_name, method = "secure_delete") {
      if (length(record_ids) == 0) {
        return(list(successful = 0, failed = 0, results = list()))
      }
      
      results <- list()
      successful <- 0
      failed <- 0
      
      for (record_id in record_ids) {
        tr