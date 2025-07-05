# =============================================================================
# Atlas Labs HR Analytics Dashboard - Data Protection Unit Tests
# 
# Comprehensive test suite for data protection compliance covering:
# 1. PII Data Masking
# 2. Encryption at Rest Validation
# 3. Encryption in Transit Verification
# 4. Data Anonymization Effectiveness
# 5. GDPR Compliance Validation
# 6. Data Retention Policy Enforcement
# 7. Secure Data Disposal
# 8. Cross-border Data Transfer Compliance
#
# Author: akhapwoyaco
# =============================================================================

# Load required libraries
library(testthat)
library(digest)
library(openssl)
library(httr)
library(lubridate)
library(stringr)
library(dplyr)
library(mockery)

# =============================================================================
# 1. PII DATA MASKING TESTS
# =============================================================================

test_that("PII Data Masking - Basic Functionality", {
  # Test data with PII
  test_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    FirstName = c("John", "Jane", "Robert"),
    LastName = c("Doe", "Smith", "Johnson"),
    Email = c("john.doe@company.com", "jane.smith@company.com", "robert.johnson@company.com"),
    SSN = c("123-45-6789", "987-65-4321", "555-44-3333"),
    Phone = c("555-123-4567", "555-987-6543", "555-555-5555"),
    Address = c("123 Main St", "456 Oak Ave", "789 Pine Rd"),
    Salary = c(75000, 85000, 95000),
    stringsAsFactors = FALSE
  )
  
  # Mock masking function
  mask_pii_data <- function(data) {
    masked_data <- data
    masked_data$FirstName <- str_replace_all(masked_data$FirstName, "(?<=.{1}).", "*")
    masked_data$LastName <- str_replace_all(masked_data$LastName, "(?<=.{1}).", "*")
    masked_data$Email <- str_replace_all(masked_data$Email, "(?<=.{2})[^@]*(?=@)", "***")
    masked_data$SSN <- str_replace_all(masked_data$SSN, "\\d(?=\\d{4})", "*")
    masked_data$Phone <- str_replace_all(masked_data$Phone, "\\d(?=\\d{4})", "*")
    masked_data$Address <- str_replace_all(masked_data$Address, "\\d+", "XXX")
    return(masked_data)
  }
  
  masked_data <- mask_pii_data(test_data)
  
  # Test FirstName masking
  expect_equal(masked_data$FirstName[1], "J***")
  expect_equal(masked_data$FirstName[2], "J***")
  expect_equal(masked_data$FirstName[3], "R*****")
  
  # Test LastName masking
  expect_equal(masked_data$LastName[1], "D**")
  expect_equal(masked_data$LastName[2], "S****")
  expect_equal(masked_data$LastName[3], "J******")
  
  # Test Email masking
  expect_true(all(str_detect(masked_data$Email, "\\*\\*\\*@")))
  expect_false(any(str_detect(masked_data$Email, "john|jane|robert")))
  
  # Test SSN masking
  expect_true(all(str_detect(masked_data$SSN, "\\*\\*\\*-\\*\\*-\\d{4}")))
  expect_false(any(str_detect(masked_data$SSN, "123|987|555")))
  
  # Test Phone masking
  expect_true(all(str_detect(masked_data$Phone, "\\*\\*\\*-\\*\\*\\*-\\d{4}")))
  
  # Test Address masking
  expect_true(all(str_detect(masked_data$Address, "XXX")))
  expect_false(any(str_detect(masked_data$Address, "123|456|789")))
  
  # Test non-PII data remains unchanged
  expect_equal(masked_data$EmployeeID, test_data$EmployeeID)
  expect_equal(masked_data$Salary, test_data$Salary)
})

test_that("PII Data Masking - Edge Cases", {
  # Test empty data
  empty_data <- data.frame(
    FirstName = character(0),
    LastName = character(0),
    Email = character(0),
    stringsAsFactors = FALSE
  )
  
  mask_pii_data <- function(data) {
    if (nrow(data) == 0) return(data)
    # Apply masking logic
    return(data)
  }
  
  result <- mask_pii_data(empty_data)
  expect_equal(nrow(result), 0)
  
  # Test NULL values
  null_data <- data.frame(
    FirstName = c("John", NA, ""),
    LastName = c(NA, "Smith", ""),
    Email = c("", NA, "test@example.com"),
    stringsAsFactors = FALSE
  )
  
  mask_with_nulls <- function(data) {
    data$FirstName <- ifelse(is.na(data$FirstName) | data$FirstName == "", 
                           data$FirstName, 
                           str_replace_all(data$FirstName, "(?<=.{1}).", "*"))
    return(data)
  }
  
  result <- mask_with_nulls(null_data)
  expect_true(is.na(result$FirstName[2]))
  expect_equal(result$FirstName[3], "")
  expect_equal(result$FirstName[1], "J***")
  
  # Test special characters in names
  special_data <- data.frame(
    FirstName = c("Jean-Pierre", "Mary-Ann", "O'Connor"),
    LastName = c("D'Artagnan", "Smith-Jones", "MacArthur"),
    stringsAsFactors = FALSE
  )
  
  mask_special <- function(data) {
    data$FirstName <- str_replace_all(data$FirstName, "(?<=.{1})[a-zA-Z]", "*")
    data$LastName <- str_replace_all(data$LastName, "(?<=.{1})[a-zA-Z]", "*")
    return(data)
  }
  
  result <- mask_special(special_data)
  expect_true(all(str_detect(result$FirstName, "^[a-zA-Z]")))
  expect_true(all(str_detect(result$LastName, "^[a-zA-Z]")))
})

test_that("PII Data Masking - Performance and Memory", {
  # Test with large dataset
  large_data <- data.frame(
    FirstName = replicate(10000, paste0(sample(letters, 8, replace = TRUE), collapse = "")),
    LastName = replicate(10000, paste0(sample(letters, 8, replace = TRUE), collapse = "")),
    Email = replicate(10000, paste0(sample(letters, 8, replace = TRUE), collapse = "", "@test.com")),
    stringsAsFactors = FALSE
  )
  
  start_time <- Sys.time()
  start_memory <- pryr::mem_used()
  
  mask_large_data <- function(data) {
    data$FirstName <- str_replace_all(data$FirstName, "(?<=.{1}).", "*")
    data$LastName <- str_replace_all(data$LastName, "(?<=.{1}).", "*")
    data$Email <- str_replace_all(data$Email, "(?<=.{2})[^@]*(?=@)", "***")
    return(data)
  }
  
  result <- mask_large_data(large_data)
  
  end_time <- Sys.time()
  end_memory <- pryr::mem_used()
  
  # Performance should be reasonable (< 5 seconds for 10k records)
  expect_true(as.numeric(end_time - start_time) < 5)
  
  # Memory usage should be reasonable
  memory_increase <- as.numeric(end_memory - start_memory)
  expect_true(memory_increase < 100 * 1024 * 1024) # Less than 100MB increase
  
  # Verify masking worked
  expect_true(all(str_count(result$FirstName, "\\*") >= 7))
  expect_true(all(str_count(result$LastName, "\\*") >= 7))
})

# =============================================================================
# 2. ENCRYPTION AT REST VALIDATION TESTS
# =============================================================================

test_that("Encryption at Rest - File Encryption", {
  # Test data
  test_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002"),
    Salary = c(75000, 85000),
    stringsAsFactors = FALSE
  )
  
  # Mock encryption functions
  encrypt_file <- function(data, key) {
    # Simulate AES encryption
    serialized <- serialize(data, NULL)
    encrypted <- aes_cbc_encrypt(serialized, key)
    return(encrypted)
  }
  
  decrypt_file <- function(encrypted_data, key) {
    # Simulate AES decryption
    decrypted <- aes_cbc_decrypt(encrypted_data, key)
    return(unserialize(decrypted))
  }
  
  # Generate encryption key
  key <- rand_bytes(32) # 256-bit key
  
  # Test encryption
  encrypted_data <- encrypt_file(test_data, key)
  expect_true(is.raw(encrypted_data))
  expect_true(length(encrypted_data) > 0)
  
  # Test decryption
  decrypted_data <- decrypt_file(encrypted_data, key)
  expect_equal(decrypted_data, test_data)
  
  # Test wrong key fails
  wrong_key <- rand_bytes(32)
  expect_error(decrypt_file(encrypted_data, wrong_key))
})

test_that("Encryption at Rest - Database Encryption", {
  # Mock database encryption validation
  validate_db_encryption <- function(db_config) {
    # Check if encryption is enabled
    required_settings <- c("ssl_mode", "encryption_key", "cipher_algorithm")
    missing_settings <- setdiff(required_settings, names(db_config))
    
    if (length(missing_settings) > 0) {
      return(list(valid = FALSE, missing = missing_settings))
    }
    
    # Validate encryption strength
    valid_ciphers <- c("AES-256-GCM", "AES-256-CBC", "ChaCha20-Poly1305")
    if (!db_config$cipher_algorithm %in% valid_ciphers) {
      return(list(valid = FALSE, error = "Weak cipher algorithm"))
    }
    
    # Validate key length
    if (length(db_config$encryption_key) < 32) {
      return(list(valid = FALSE, error = "Key too short"))
    }
    
    return(list(valid = TRUE))
  }
  
  # Test valid configuration
  valid_config <- list(
    ssl_mode = "require",
    encryption_key = rand_bytes(32),
    cipher_algorithm = "AES-256-GCM"
  )
  
  result <- validate_db_encryption(valid_config)
  expect_true(result$valid)
  
  # Test invalid configuration - missing SSL
  invalid_config1 <- list(
    encryption_key = rand_bytes(32),
    cipher_algorithm = "AES-256-GCM"
  )
  
  result <- validate_db_encryption(invalid_config1)
  expect_false(result$valid)
  expect_true("ssl_mode" %in% result$missing)
  
  # Test weak cipher
  invalid_config2 <- list(
    ssl_mode = "require",
    encryption_key = rand_bytes(32),
    cipher_algorithm = "DES"
  )
  
  result <- validate_db_encryption(invalid_config2)
  expect_false(result$valid)
  expect_equal(result$error, "Weak cipher algorithm")
})

test_that("Encryption at Rest - Key Management", {
  # Test key rotation
  rotate_encryption_key <- function(old_key, new_key, data) {
    # Decrypt with old key
    decrypted <- aes_cbc_decrypt(data, old_key)
    # Encrypt with new key
    encrypted <- aes_cbc_encrypt(decrypted, new_key)
    return(encrypted)
  }
  
  # Test data
  original_data <- serialize(data.frame(test = "data"), NULL)
  old_key <- rand_bytes(32)
  new_key <- rand_bytes(32)
  
  # Encrypt with old key
  encrypted_old <- aes_cbc_encrypt(original_data, old_key)
  
  # Rotate key
  encrypted_new <- rotate_encryption_key(old_key, new_key, encrypted_old)
  
  # Verify new encryption works
  decrypted_new <- aes_cbc_decrypt(encrypted_new, new_key)
  expect_equal(decrypted_new, original_data)
  
  # Verify old key no longer works
  expect_error(aes_cbc_decrypt(encrypted_new, old_key))
})

# =============================================================================
# 3. ENCRYPTION IN TRANSIT VERIFICATION TESTS
# =============================================================================

test_that("Encryption in Transit - HTTPS Enforcement", {
  # Mock HTTPS validation
  validate_https <- function(url) {
    return(startsWith(url, "https://"))
  }
  
  # Test valid HTTPS URLs
  expect_true(validate_https("https://api.example.com/data"))
  expect_true(validate_https("https://secure.atlaslab.com/hr"))
  
  # Test invalid HTTP URLs
  expect_false(validate_https("http://api.example.com/data"))
  expect_false(validate_https("ftp://files.example.com/data"))
  
  # Test malformed URLs
  expect_false(validate_https("https:/"))
  expect_false(validate_https("htps://example.com"))
})

test_that("Encryption in Transit - TLS Version Validation", {
  # Mock TLS version check
  check_tls_version <- function(connection_info) {
    min_tls_version <- "1.2"
    current_version <- connection_info$tls_version
    
    version_priority <- list("1.0" = 1, "1.1" = 2, "1.2" = 3, "1.3" = 4)
    
    if (is.null(version_priority[[current_version]]) || 
        is.null(version_priority[[min_tls_version]])) {
      return(list(valid = FALSE, error = "Unknown TLS version"))
    }
    
    if (version_priority[[current_version]] < version_priority[[min_tls_version]]) {
      return(list(valid = FALSE, error = "TLS version too old"))
    }
    
    return(list(valid = TRUE, version = current_version))
  }
  
  # Test valid TLS versions
  expect_true(check_tls_version(list(tls_version = "1.2"))$valid)
  expect_true(check_tls_version(list(tls_version = "1.3"))$valid)
  
  # Test invalid TLS versions
  expect_false(check_tls_version(list(tls_version = "1.0"))$valid)
  expect_false(check_tls_version(list(tls_version = "1.1"))$valid)
  
  # Test unknown version
  result <- check_tls_version(list(tls_version = "2.0"))
  expect_false(result$valid)
  expect_equal(result$error, "Unknown TLS version")
})

test_that("Encryption in Transit - Certificate Validation", {
  # Mock certificate validation
  validate_ssl_certificate <- function(cert_info) {
    current_date <- Sys.Date()
    
    # Check expiration
    if (cert_info$expires < current_date) {
      return(list(valid = FALSE, error = "Certificate expired"))
    }
    
    # Check if expires soon (within 30 days)
    if (cert_info$expires < current_date + 30) {
      return(list(valid = TRUE, warning = "Certificate expires soon"))
    }
    
    # Check issuer
    trusted_issuers <- c("Let's Encrypt", "DigiCert", "Comodo", "GlobalSign")
    if (!cert_info$issuer %in% trusted_issuers) {
      return(list(valid = FALSE, error = "Untrusted certificate issuer"))
    }
    
    # Check key strength
    if (cert_info$key_size < 2048) {
      return(list(valid = FALSE, error = "Key size too small"))
    }
    
    return(list(valid = TRUE))
  }
  
  # Test valid certificate
  valid_cert <- list(
    expires = Sys.Date() + 365,
    issuer = "Let's Encrypt",
    key_size = 2048
  )
  
  result <- validate_ssl_certificate(valid_cert)
  expect_true(result$valid)
  
  # Test expired certificate
  expired_cert <- list(
    expires = Sys.Date() - 1,
    issuer = "Let's Encrypt",
    key_size = 2048
  )
  
  result <- validate_ssl_certificate(expired_cert)
  expect_false(result$valid)
  expect_equal(result$error, "Certificate expired")
  
  # Test certificate expiring soon
  expiring_cert <- list(
    expires = Sys.Date() + 15,
    issuer = "Let's Encrypt",
    key_size = 2048
  )
  
  result <- validate_ssl_certificate(expiring_cert)
  expect_true(result$valid)
  expect_equal(result$warning, "Certificate expires soon")
})

# =============================================================================
# 4. DATA ANONYMIZATION EFFECTIVENESS TESTS
# =============================================================================

test_that("Data Anonymization - K-Anonymity", {
  # Test k-anonymity implementation
  test_data <- data.frame(
    Age = c(25, 26, 27, 45, 46, 47),
    Gender = c("M", "M", "M", "F", "F", "F"),
    Department = c("IT", "IT", "IT", "HR", "HR", "HR"),
    Salary = c(50000, 52000, 51000, 60000, 62000, 61000),
    stringsAsFactors = FALSE
  )
  
  # Check k-anonymity
  check_k_anonymity <- function(data, quasi_identifiers, k = 2) {
    # Group by quasi-identifiers
    grouped <- data %>%
      group_by(across(all_of(quasi_identifiers))) %>%
      summarise(count = n(), .groups = "drop")
    
    # Check if all groups have at least k members
    min_group_size <- min(grouped$count)
    
    return(list(
      k_anonymous = min_group_size >= k,
      min_group_size = min_group_size,
      groups = nrow(grouped)
    ))
  }
  
  quasi_identifiers <- c("Age", "Gender", "Department")
  
  # Test original data (should not be k-anonymous for k=2)
  result <- check_k_anonymity(test_data, quasi_identifiers, k = 2)
  expect_true(result$k_anonymous) # Each group has exactly 3 members
  
  # Test with k=4 (should fail)
  result <- check_k_anonymity(test_data, quasi_identifiers, k = 4)
  expect_false(result$k_anonymous)
  expect_equal(result$min_group_size, 3)
})

test_that("Data Anonymization - L-Diversity", {
  # Test l-diversity implementation
  test_data <- data.frame(
    Age = c(25, 26, 27, 45, 46, 47),
    Gender = c("M", "M", "M", "F", "F", "F"),
    Department = c("IT", "IT", "IT", "HR", "HR", "HR"),
    Salary = c(50000, 52000, 51000, 60000, 62000, 61000),
    Performance = c("High", "Medium", "High", "Low", "High", "Medium"),
    stringsAsFactors = FALSE
  )
  
  check_l_diversity <- function(data, quasi_identifiers, sensitive_attribute, l = 2) {
    # Group by quasi-identifiers
    grouped <- data %>%
      group_by(across(all_of(quasi_identifiers))) %>%
      summarise(
        distinct_values = n_distinct(!!sym(sensitive_attribute)),
        .groups = "drop"
      )
    
    # Check if all groups have at least l distinct values
    min_diversity <- min(grouped$distinct_values)
    
    return(list(
      l_diverse = min_diversity >= l,
      min_diversity = min_diversity,
      groups = nrow(grouped)
    ))
  }
  
  quasi_identifiers <- c("Age", "Gender", "Department")
  
  # Test l-diversity for Performance attribute
  result <- check_l_diversity(test_data, quasi_identifiers, "Performance", l = 2)
  expect_true(result$l_diverse)
  
  # Test with higher l value (should fail)
  result <- check_l_diversity(test_data, quasi_identifiers, "Performance", l = 4)
  expect_false(result$l_diverse)
})

test_that("Data Anonymization - Differential Privacy", {
  # Test differential privacy implementation
  add_laplace_noise <- function(value, epsilon, sensitivity = 1) {
    # Add Laplace noise for differential privacy
    scale <- sensitivity / epsilon
    noise <- extraDistr::rlaplace(1, 0, scale)
    return(value + noise)
  }
  
  # Test noise addition
  original_value <- 100
  epsilon <- 0.1
  
  noisy_values <- replicate(1000, add_laplace_noise(original_value, epsilon))
  
  # Test that noise is added
  expect_true(any(noisy_values != original_value))
  
  # Test that mean is approximately preserved
  expect_true(abs(mean(noisy_values) - original_value) < 5)
  
  # Test privacy budget tracking
  privacy_budget <- list(total = 1.0, used = 0.0)
  
  use_privacy_budget <- function(budget, epsilon_needed) {
    if (budget$used + epsilon_needed > budget$total) {
      return(list(success = FALSE, error = "Privacy budget exceeded"))
    }
    
    budget$used <- budget$used + epsilon_needed
    return(list(success = TRUE, remaining = budget$total - budget$used))
  }
  
  # Test budget usage
  result <- use_privacy_budget(privacy_budget, 0.5)
  expect_true(result$success)
  expect_equal(result$remaining, 0.5)
  
  # Test budget exhaustion
  result <- use_privacy_budget(privacy_budget, 0.6)
  expect_false(result$success)
  expect_equal(result$error, "Privacy budget exceeded")
})

# =============================================================================
# 5. GDPR COMPLIANCE VALIDATION TESTS
# =============================================================================

test_that("GDPR Compliance - Data Subject Rights", {
  # Mock data subject rights implementation
  process_data_subject_request <- function(request_type, subject_id, data_store) {
    switch(request_type,
      "access" = {
        # Right to access
        subject_data <- data_store[data_store$subject_id == subject_id, ]
        return(list(success = TRUE, data = subject_data))
      },
      "rectification" = {
        # Right to rectification
        return(list(success = TRUE, message = "Data updated"))
      },
      "erasure" = {
        # Right to erasure (right to be forgotten)
        remaining_data <- data_store[data_store$subject_id != subject_id, ]
        return(list(success = TRUE, data = remaining_data))
      },
      "portability" = {
        # Right to data portability
        subject_data <- data_store[data_store$subject_id == subject_id, ]
        return(list(success = TRUE, data = subject_data, format = "JSON"))
      },
      "objection" = {
        # Right to object
        return(list(success = TRUE, message = "Processing stopped"))
      },
      {
        return(list(success = FALSE, error = "Invalid request type"))
      }
    )
  }
  
  # Test data
  test_data_store <- data.frame(
    subject_id = c("S001", "S002", "S003"),
    name = c("John Doe", "Jane Smith", "Bob Johnson"),
    email = c("john@example.com", "jane@example.com", "bob@example.com"),
    stringsAsFactors = FALSE
  )
  
  # Test right to access
  result <- process_data_subject_request("access", "S001", test_data_store)
  expect_true(result$success)
  expect_equal(nrow(result$data), 1)
  expect_equal(result$data$subject_id, "S001")
  
  # Test right to erasure
  result <- process_data_subject_request("erasure", "S001", test_data_store)
  expect_true(result$success)
  expect_equal(nrow(result$data), 2)
  expect_false("S001" %in% result$data$subject_id)
  
  # Test invalid request type
  result <- process_data_subject_request("invalid", "S001", test_data_store)
  expect_false(result$success)
  expect_equal(result$error, "Invalid request type")
})

test_that("GDPR Compliance - Consent Management", {
  # Mock consent management system
  consent_manager <- function() {
    list(
      consents = data.frame(
        subject_id = character(0),
        purpose = character(0),
        granted = logical(0),
        timestamp = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    )
  }
  
  add_consent <- function(manager, subject_id, purpose, granted = TRUE) {
    new_consent <- data.frame(
      subject_id = subject_id,
      purpose = purpose,
      granted = granted,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    # Remove existing consent for same subject/purpose
    manager$consents <- manager$consents[
      !(manager$consents$subject_id == subject_id & 
        manager$consents$purpose == purpose), 
    ]
    
    # Add new consent
    manager$consents <- rbind(manager$consents, new_consent)
    return(manager)
  }
  
  check_consent <- function(manager, subject_id, purpose) {
    consent_record <- manager$consents[
      manager$consents$subject_id == subject_id & 
      manager$consents$purpose == purpose, 
    ]
    
    if (nrow(consent_record) == 0) {
      return(list(has_consent = FALSE, error = "No consent record found"))
    }
    
    return(list(
      has_consent = consent_record$granted[1],
      timestamp = consent_record$timestamp[1]
    ))
  }
  
  # Test consent management
  manager <- consent_manager()
  
  # Add consent
  manager <- add_consent(manager, "S001", "analytics", TRUE)
  
  # Check consent
  result <- check_consent(manager, "S001", "analytics")
  expect_true(result$has_consent)
  expect_true(is.POSIXct(result$timestamp))
  
  # Test no consent
  result <- check_consent(manager, "S002", "analytics")
  expect_false(result$has_consent)
  expect_equal(result$error, "No consent record found")
  
  # Test consent withdrawal
  manager <- add_consent(manager, "S001", "analytics", FALSE)
  result <- check_consent(manager, "S001", "analytics")
  expect_false(result$has_consent)
})

test_that("GDPR Compliance - Privacy Impact Assessment", {
  # Mock PIA implementation
  conduct_pia <- function(processing_activity) {
    risk_score <- 0
    recommendations <- character(0)
    
    # Check for high-risk processing
    if (processing_activity$involves_special_categories) {
      risk_score <- risk_score + 3
      recommendations <- c(recommendations, "Implement additional safeguards for special category data")
    }
    
    if (processing_activity$large_scale) {
      risk_score <- risk_score + 2
      recommendations <- c(recommendations, "Implement data minimization techniques")
    }
    
    if (processing_activity$profiling) {
      risk_score <- risk_score + 2
      recommendations <- c(recommendations, "Provide opt-out mechanisms for profiling")
    }
    
    if (processing_activity$automated_decision_making) {
      risk_score <- risk_score + 3
      recommendations <- c(recommendations, "Implement human review processes")
    }
    
    if (processing_activity$cross_border_transfer) {
      risk_score <- risk_score + 2
      recommendations <- c(recommendations, "Ensure adequate safeguards for international transfers")
    }
    
    # Determine risk level
    risk_level <- case_when(
      risk_score >= 8 ~ "High",
      risk_score >= 4 ~ "Medium",
      TRUE ~ "Low"
    )
    
    return(list(
      risk_level = risk_level,
      risk_score = risk_score,
      recommendations = recommendations,
      dpo_consultation_required = risk_level == "High"
    ))
  }
  
  # Test high-risk processing
  high_risk_activity <- list(
    involves_special_categories = TRUE,
    large_scale = TRUE,
    profiling = TRUE,
    automated_decision_making = TRUE,
    cross_border_transfer = TRUE
  )
  
  result <- conduct_pia(high_risk_activity)
  expect_equal(result$risk_level, "High")
  expect_true(result$dpo_consultation_required)
  expect_true(length(result$recommendations) > 0)
  
  # Test low-risk processing
  low_risk_activity <- list(
    involves_special_categories = FALSE,
    large_scale = FALSE,
    profiling = FALSE,
    automated_decision_making = FALSE,
    cross_border_transfer = FALSE
  )
  
  result <- conduct_pia(low_risk_activity)
  expect_equal(result$risk_level, "Low")
  expect_false