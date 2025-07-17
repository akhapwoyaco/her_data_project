# ==============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - CYBERSECURITY COMPLIANCE UNIT TESTS
# ==============================================================================
# Focus: Comprehensive cybersecurity compliance testing with edge cases
# Exclusions: SOC 2, HIPAA, PCI DSS, ISO 27001, NIST, industry-specific, 
#            international compliance, audit trail completeness
# ==============================================================================

# Required libraries for testing
library(testthat)
library(shiny)
library(httr)
library(stringr)
library(digest)
library(jsonlite)
library(DT)
library(plotly)

# Source the application modules (assuming they exist)
source("global.R")
source("utils.R")
source("custom_theme.R")

# ==============================================================================
# 1. DATA PROTECTION & PRIVACY COMPLIANCE TESTING
# ==============================================================================

test_that("Data Protection - PII Identification and Handling", {
  
  # Test 1.1: PII Detection in Employee Data
  mock_employee_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Johnson"),
    Email = c("john.doe@company.com", "jane.smith@company.com", "bob.j@company.com"),
    SSN = c("123-45-6789", "987-65-4321", "456-78-9012"),
    DateOfBirth = as.Date(c("1990-01-01", "1985-06-15", "1992-12-30")),
    Salary = c(75000, 85000, 65000),
    stringsAsFactors = FALSE
  )
  
  # Test PII column identification
  pii_columns <- identify_pii_columns(mock_employee_data)
  expect_true("FirstName" %in% pii_columns)
  expect_true("LastName" %in% pii_columns)
  expect_true("Email" %in% pii_columns)
  expect_true("SSN" %in% pii_columns)
  expect_true("DateOfBirth" %in% pii_columns)
  expect_false("EmployeeID" %in% pii_columns)
  
  # Test 1.2: Data Masking for Display
  masked_data <- mask_sensitive_data(mock_employee_data)
  expect_true(all(grepl("\\*{3}", masked_data$SSN)))
  expect_true(all(grepl("\\*{2,}", masked_data$FirstName)))
  expect_true(all(grepl("\\*{2,}", masked_data$LastName)))
  
  # Test 1.3: Edge Case - Empty Data
  empty_data <- data.frame()
  expect_silent(identify_pii_columns(empty_data))
  expect_equal(length(identify_pii_columns(empty_data)), 0)
  
  # Test 1.4: Edge Case - NULL values in PII columns
  null_pii_data <- mock_employee_data
  null_pii_data$SSN[1] <- NA
  null_pii_data$Email[2] <- ""
  masked_null <- mask_sensitive_data(null_pii_data)
  expect_true(is.na(masked_null$SSN[1]) | masked_null$SSN[1] == "")
  expect_true(masked_null$Email[2] == "" | is.na(masked_null$Email[2]))
})

test_that("Data Protection - Anonymization Compliance", {
  
  # Test 2.1: K-Anonymity Implementation
  test_data <- data.frame(
    Age = c(25, 26, 25, 35, 36, 35),
    Department = c("IT", "IT", "HR", "IT", "HR", "HR"),
    Salary = c(50000, 52000, 48000, 75000, 73000, 77000),
    Gender = c("M", "F", "M", "F", "M", "F")
  )
  
  k_anon_result <- apply_k_anonymity(test_data, k = 2, quasi_identifiers = c("Age", "Department"))
  expect_true(check_k_anonymity(k_anon_result, k = 2, quasi_identifiers = c("Age", "Department")))
  
  # Test 2.2: L-Diversity Implementation
  l_div_result <- apply_l_diversity(test_data, l = 2, sensitive_attr = "Salary", quasi_identifiers = c("Age", "Department"))
  expect_true(check_l_diversity(l_div_result, l = 2, sensitive_attr = "Salary", quasi_identifiers = c("Age", "Department")))
  
  # Test 2.3: Edge Case - Insufficient data for anonymization
  small_data <- data.frame(Age = c(25), Department = c("IT"), Salary = c(50000))
  expect_warning(apply_k_anonymity(small_data, k = 2, quasi_identifiers = c("Age", "Department")))
  
  # Test 2.4: Edge Case - All identical quasi-identifiers
  identical_data <- data.frame(
    Age = rep(25, 5),
    Department = rep("IT", 5),
    Salary = c(50000, 52000, 48000, 55000, 53000)
  )
  identical_result <- apply_k_anonymity(identical_data, k = 2, quasi_identifiers = c("Age", "Department"))
  expect_true(check_k_anonymity(identical_result, k = 2, quasi_identifiers = c("Age", "Department")))
})

test_that("Data Protection - Consent Management", {
  
  # Test 3.1: Consent Recording
  consent_record <- record_data_consent(
    user_id = "USER001",
    consent_type = "data_processing",
    consent_given = TRUE,
    timestamp = Sys.time(),
    purposes = c("analytics", "reporting")
  )
  
  expect_true(consent_record$consent_given)
  expect_equal(consent_record$user_id, "USER001")
  expect_true(all(c("analytics", "reporting") %in% consent_record$purposes))
  
  # Test 3.2: Consent Withdrawal
  withdrawal_record <- withdraw_consent("USER001", "analytics")
  expect_true(withdrawal_record$withdrawn)
  expect_equal(withdrawal_record$purpose, "analytics")
  
  # Test 3.3: Edge Case - Invalid consent type
  expect_error(record_data_consent("USER001", "invalid_type", TRUE, Sys.time(), "analytics"))
  
  # Test 3.4: Edge Case - Consent for non-existent user
  expect_error(withdraw_consent("NONEXISTENT", "analytics"))
})

# ==============================================================================
# 2. ACCESS CONTROL & AUTHENTICATION COMPLIANCE TESTING
# ==============================================================================

test_that("Access Control - Role-Based Access Control (RBAC)", {
  
  # Test 4.1: Role Definition and Assignment
  roles <- define_user_roles()
  expect_true("hr_admin" %in% names(roles))
  expect_true("hr_analyst" %in% names(roles))
  expect_true("employee" %in% names(roles))
  expect_true("manager" %in% names(roles))
  
  # Test 4.2: Permission Matrix Validation
  permissions <- get_role_permissions("hr_admin")
  expect_true("view_all_employees" %in% permissions)
  expect_true("export_data" %in% permissions)
  expect_true("modify_settings" %in% permissions)
  
  analyst_permissions <- get_role_permissions("hr_analyst")
  expect_true("view_analytics" %in% analyst_permissions)
  expect_false("modify_settings" %in% analyst_permissions)
  
  # Test 4.3: Access Control Enforcement
  user_session <- create_mock_session(user_id = "USER001", role = "hr_analyst")
  expect_true(check_access_permission(user_session, "view_analytics"))
  expect_false(check_access_permission(user_session, "modify_settings"))
  
  # Test 4.4: Edge Case - Invalid role assignment
  expect_error(get_role_permissions("invalid_role"))
  
  # Test 4.5: Edge Case - Session without role
  empty_session <- create_mock_session(user_id = "USER002", role = NULL)
  expect_false(check_access_permission(empty_session, "view_analytics"))
})

test_that("Access Control - Session Management", {
  
  # Test 5.1: Session Creation and Validation
  session <- create_secure_session("USER001", "hr_admin")
  expect_true(validate_session(session))
  expect_equal(session$user_id, "USER001")
  expect_equal(session$role, "hr_admin")
  expect_true(!is.null(session$token))
  
  # Test 5.2: Session Expiration
  expired_session <- create_secure_session("USER001", "hr_admin", expires_in = -1)
  expect_false(validate_session(expired_session))
  
  # Test 5.3: Session Invalidation
  valid_session <- create_secure_session("USER001", "hr_admin")
  invalidate_session(valid_session$token)
  expect_false(validate_session(valid_session))
  
  # Test 5.4: Concurrent Session Limits
  sessions <- replicate(10, create_secure_session("USER001", "hr_admin"), simplify = FALSE)
  active_sessions <- count_active_sessions("USER001")
  expect_true(active_sessions <= get_max_concurrent_sessions())
  
  # Test 5.5: Edge Case - Session hijacking attempt
  hijacked_session <- session
  hijacked_session$user_agent <- "Different User Agent"
  expect_false(validate_session(hijacked_session))
})

test_that("Access Control - Multi-Factor Authentication", {
  
  # Test 6.1: MFA Setup Validation
  mfa_setup <- setup_mfa("USER001", method = "totp")
  expect_true(mfa_setup$success)
  expect_true(!is.null(mfa_setup$secret))
  expect_true(!is.null(mfa_setup$qr_code))
  
  # Test 6.2: MFA Token Validation
  valid_token <- "123456"  # Mock TOTP token
  expect_true(validate_mfa_token("USER001", valid_token))
  
  invalid_token <- "000000"
  expect_false(validate_mfa_token("USER001", invalid_token))
  
  # Test 6.3: MFA Backup Codes
  backup_codes <- generate_mfa_backup_codes("USER001")
  expect_equal(length(backup_codes), 10)
  expect_true(all(nchar(backup_codes) == 8))
  
  # Test 6.4: Edge Case - MFA for non-existent user
  expect_error(setup_mfa("NONEXISTENT", "totp"))
  
  # Test 6.5: Edge Case - Invalid MFA method
  expect_error(setup_mfa("USER001", "invalid_method"))
})

# ==============================================================================
# 3. INPUT VALIDATION & SANITIZATION COMPLIANCE TESTING
# ==============================================================================

test_that("Input Validation - SQL Injection Prevention", {
  
  # Test 7.1: SQL Injection Detection
  malicious_inputs <- c(
    "'; DROP TABLE employees; --",
    "1' OR '1'='1",
    "admin'--",
    "' UNION SELECT * FROM users--",
    "1; DELETE FROM performance_rating; --"
  )
  
  for (input in malicious_inputs) {
    expect_true(detect_sql_injection(input))
    expect_true(is_malicious_input(input))
  }
  
  # Test 7.2: Safe Input Validation
  safe_inputs <- c("John Doe", "123", "valid@email.com", "HR Department")
  for (input in safe_inputs) {
    expect_false(detect_sql_injection(input))
    expect_false(is_malicious_input(input))
  }
  
  # Test 7.3: Input Sanitization
  sanitized <- sanitize_input("'; DROP TABLE employees; --")
  expect_false(detect_sql_injection(sanitized))
  expect_true(nchar(sanitized) > 0)
  
  # Test 7.4: Edge Case - Empty input
  expect_false(detect_sql_injection(""))
  expect_equal(sanitize_input(""), "")
  
  # Test 7.5: Edge Case - Very long input
  long_input <- paste(rep("a", 10000), collapse = "")
  expect_true(nchar(sanitize_input(long_input)) <= get_max_input_length())
})

test_that("Input Validation - XSS Prevention", {
  
  # Test 8.1: XSS Detection
  xss_inputs <- c(
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')",
    "<iframe src='javascript:alert(\"XSS\")'></iframe>",
    "<svg onload=alert('XSS')>"
  )
  
  for (input in xss_inputs) {
    expect_true(detect_xss(input))
    expect_true(is_malicious_input(input))
  }
  
  # Test 8.2: XSS Sanitization
  for (input in xss_inputs) {
    sanitized <- sanitize_html(input)
    expect_false(detect_xss(sanitized))
  }
  
  # Test 8.3: Safe HTML Preservation
  safe_html <- "<p>This is safe content</p>"
  sanitized_safe <- sanitize_html(safe_html, allow_safe_tags = TRUE)
  expect_true(grepl("<p>", sanitized_safe))
  expect_false(detect_xss(sanitized_safe))
  
  # Test 8.4: Edge Case - Nested XSS attempts
  nested_xss <- "<div><script>alert('nested')</script></div>"
  sanitized_nested <- sanitize_html(nested_xss)
  expect_false(detect_xss(sanitized_nested))
  
  # Test 8.5: Edge Case - URL-encoded XSS
  encoded_xss <- "%3Cscript%3Ealert%28%27XSS%27%29%3C%2Fscript%3E"
  decoded_input <- url_decode(encoded_xss)
  expect_true(detect_xss(decoded_input))
})

test_that("Input Validation - File Upload Security", {
  
  # Test 9.1: File Type Validation
  valid_files <- c("employee_data.csv", "performance.xlsx", "report.pdf")
  invalid_files <- c("malware.exe", "script.js", "config.php", "data.sql")
  
  for (file in valid_files) {
    expect_true(validate_file_type(file))
  }
  
  for (file in invalid_files) {
    expect_false(validate_file_type(file))
  }
  
  # Test 9.2: File Size Validation
  expect_true(validate_file_size(1024 * 1024))  # 1MB
  expect_false(validate_file_size(100 * 1024 * 1024))  # 100MB
  
  # Test 9.3: File Content Validation
  csv_content <- "EmployeeID,FirstName,LastName\n1,John,Doe\n2,Jane,Smith"
  expect_true(validate_csv_content(csv_content))
  
  malicious_content <- "<?php system($_GET['cmd']); ?>"
  expect_false(validate_file_content(malicious_content))
  
  # Test 9.4: Edge Case - Empty file
  expect_false(validate_file_content(""))
  
  # Test 9.5: Edge Case - File with no extension
  expect_false(validate_file_type("filename"))
})

# ==============================================================================
# 4. SECURE COMMUNICATION COMPLIANCE TESTING
# ==============================================================================

test_that("Secure Communication - HTTPS Enforcement", {
  
  # Test 10.1: HTTP to HTTPS Redirect
  http_request <- list(HTTP_HOST = "example.com", REQUEST_SCHEME = "http")
  expect_true(requires_https_redirect(http_request))
  
  https_request <- list(HTTP_HOST = "example.com", REQUEST_SCHEME = "https")
  expect_false(requires_https_redirect(https_request))
  
  # Test 10.2: Secure Headers Validation
  headers <- get_security_headers()
  expect_true("Strict-Transport-Security" %in% names(headers))
  expect_true("X-Frame-Options" %in% names(headers))
  expect_true("X-Content-Type-Options" %in% names(headers))
  expect_true("X-XSS-Protection" %in% names(headers))
  
  # Test 10.3: HSTS Header Validation
  hsts_header <- headers[["Strict-Transport-Security"]]
  expect_true(grepl("max-age=", hsts_header))
  expect_true(grepl("includeSubDomains", hsts_header))
  
  # Test 10.4: Edge Case - Invalid scheme
  invalid_request <- list(HTTP_HOST = "example.com", REQUEST_SCHEME = "ftp")
  expect_true(requires_https_redirect(invalid_request))
})

test_that("Secure Communication - Certificate Validation", {
  
  # Test 11.1: SSL Certificate Validation
  valid_cert <- create_mock_certificate(
    subject = "CN=atlas-labs.com",
    issuer = "CN=Valid CA",
    expiry = Sys.time() + 365 * 24 * 3600,
    signature_valid = TRUE
  )
  
  expect_true(validate_ssl_certificate(valid_cert))
  
  # Test 11.2: Expired Certificate Detection
  expired_cert <- create_mock_certificate(
    subject = "CN=atlas-labs.com",
    issuer = "CN=Valid CA",
    expiry = Sys.time() - 24 * 3600,
    signature_valid = TRUE
  )
  
  expect_false(validate_ssl_certificate(expired_cert))
  
  # Test 11.3: Self-signed Certificate Detection
  self_signed_cert <- create_mock_certificate(
    subject = "CN=atlas-labs.com",
    issuer = "CN=atlas-labs.com",
    expiry = Sys.time() + 365 * 24 * 3600,
    signature_valid = FALSE
  )
  
  expect_false(validate_ssl_certificate(self_signed_cert))
  
  # Test 11.4: Edge Case - Certificate with wrong domain
  wrong_domain_cert <- create_mock_certificate(
    subject = "CN=wrong-domain.com",
    issuer = "CN=Valid CA",
    expiry = Sys.time() + 365 * 24 * 3600,
    signature_valid = TRUE
  )
  
  expect_false(validate_ssl_certificate(wrong_domain_cert, expected_domain = "atlas-labs.com"))
})

# ==============================================================================
# 5. API SECURITY COMPLIANCE TESTING
# ==============================================================================

test_that("API Security - Rate Limiting", {
  
  # Test 12.1: Rate Limit Configuration
  rate_limits <- get_rate_limits()
  expect_true("requests_per_minute" %in% names(rate_limits))
  expect_true("requests_per_hour" %in% names(rate_limits))
  expect_true("requests_per_day" %in% names(rate_limits))
  
  # Test 12.2: Rate Limit Enforcement
  client_id <- "TEST_CLIENT"
  
  # Make requests within limit
  for (i in 1:10) {
    expect_true(check_rate_limit(client_id, "minute"))
  }
  
  # Exceed rate limit
  for (i in 1:100) {
    check_rate_limit(client_id, "minute")
  }
  expect_false(check_rate_limit(client_id, "minute"))
  
  # Test 12.3: Rate Limit Reset
  reset_rate_limit(client_id, "minute")
  expect_true(check_rate_limit(client_id, "minute"))
  
  # Test 12.4: Edge Case - Multiple clients
  client_a <- "CLIENT_A"
  client_b <- "CLIENT_B"
  
  for (i in 1:50) {
    check_rate_limit(client_a, "minute")
  }
  
  expect_false(check_rate_limit(client_a, "minute"))
  expect_true(check_rate_limit(client_b, "minute"))
})

test_that("API Security - Authentication Token Validation", {
  
  # Test 13.1: JWT Token Generation
  token <- generate_jwt_token("USER001", "hr_admin", expires_in = 3600)
  expect_true(is_valid_jwt_format(token))
  
  # Test 13.2: JWT Token Validation
  payload <- validate_jwt_token(token)
  expect_equal(payload$user_id, "USER001")
  expect_equal(payload$role, "hr_admin")
  
  # Test 13.3: Expired Token Detection
  expired_token <- generate_jwt_token("USER001", "hr_admin", expires_in = -1)
  expect_null(validate_jwt_token(expired_token))
  
  # Test 13.4: Tampered Token Detection
  tampered_token <- paste0(substr(token, 1, nchar(token) - 10), "tampered123")
  expect_null(validate_jwt_token(tampered_token))
  
  # Test 13.5: Edge Case - Malformed token
  malformed_tokens <- c("", "invalid", "header.payload", "a.b.c.d")
  for (malformed in malformed_tokens) {
    expect_false(is_valid_jwt_format(malformed))
    expect_null(validate_jwt_token(malformed))
  }
})

# ==============================================================================
# 6. LOGGING & MONITORING COMPLIANCE TESTING
# ==============================================================================

test_that("Logging & Monitoring - Security Event Logging", {
  
  # Test 14.1: Security Event Recording
  security_logger <- AtlasSecurityLogger$new()
  
  # Test login event
  login_event <- security_logger$log_security_event(
    event_type = "login_attempt",
    user_id = "USER001",
    success = TRUE,
    ip_address = "192.168.1.100",
    user_agent = "Mozilla/5.0..."
  )
  
  expect_true(login_event$logged)
  expect_equal(login_event$event_type, "login_attempt")
  
  # Test failed login event
  failed_login <- security_logger$log_security_event(
    event_type = "login_failure",
    user_id = "USER001",
    success = FALSE,
    ip_address = "192.168.1.100",
    details = "Invalid password"
  )
  
  expect_true(failed_login$logged)
  expect_false(failed_login$success)
  
  # Test 14.2: Suspicious Activity Detection
  # Multiple failed logins
  for (i in 1:5) {
    security_logger$log_security_event(
      event_type = "login_failure",
      user_id = "USER001",
      success = FALSE,
      ip_address = "192.168.1.100"
    )
  }
  
  suspicious_activity <- security_logger$detect_suspicious_activity("USER001")
  expect_true(suspicious_activity$detected)
  expect_true("multiple_failed_logins" %in% suspicious_activity$indicators)
  
  # Test 14.3: Log Integrity Verification
  logs <- security_logger$get_security_logs()
  expect_true(verify_log_integrity(logs))
  
  # Test 14.4: Edge Case - Concurrent logging
  concurrent_logs <- parallel::mclapply(1:10, function(i) {
    security_logger$log_security_event(
      event_type = "data_access",
      user_id = paste0("USER", i),
      success = TRUE,
      ip_address = "192.168.1.100"
    )
  })
  
  expect_equal(length(concurrent_logs), 10)
  expect_true(all(sapply(concurrent_logs, function(x) x$logged)))
})

test_that("Logging & Monitoring - Real-time Monitoring", {
  
  # Test 15.1: Real-time Alert Configuration
  alerts <- configure_security_alerts()
  expect_true("failed_login_threshold" %in% names(alerts))
  expect_true("suspicious_ip_detection" %in% names(alerts))
  expect_true("privilege_escalation" %in% names(alerts))
  
  # Test 15.2: Alert Triggering
  monitor <- SecurityMonitor$new()
  
  # Trigger failed login alert
  for (i in 1:alerts$failed_login_threshold + 1) {
    monitor$record_event("login_failure", "USER001", "192.168.1.100")
  }
  
  triggered_alerts <- monitor$get_triggered_alerts()
  expect_true(any(sapply(triggered_alerts, function(x) x$type == "multiple_failed_logins")))
  
  # Test 15.3: Alert Escalation
  critical_alert <- monitor$trigger_alert(
    type = "privilege_escalation",
    severity = "critical",
    user_id = "USER001",
    details = "Unauthorized admin access attempt"
  )
  
  expect_equal(critical_alert$severity, "critical")
  expect_true(critical_alert$escalated)
  
  # Test 15.4: Edge Case - Alert flooding prevention
  for (i in 1:100) {
    monitor$trigger_alert(
      type = "login_failure",
      severity = "low",
      user_id = "USER001"
    )
  }
  
  recent_alerts <- monitor$get_recent_alerts(minutes = 5)
  expect_true(length(recent_alerts) <= get_max_alerts_per_period())
})

# ==============================================================================
# 7. CONFIGURATION SECURITY COMPLIANCE TESTING
# ==============================================================================

test_that("Configuration Security - Secure Defaults", {
  
  # Test 16.1: Default Security Settings
  default_config <- get_default_security_config()
  
  expect_true(default_config$https_only)
  expect_true(default_config$secure_cookies)
  expect_false(default_config$debug_mode)
  expect_true(default_config$csrf_protection)
  expect_true(default_config$xss_protection)
  
  # Test 16.2: Configuration Validation
  valid_config <- list(
    session_timeout = 1800,
    max_login_attempts = 5,
    password_complexity = TRUE,
    mfa_required = TRUE
  )
  
  expect_true(validate_security_config(valid_config))
  
  # Test 16.3: Invalid Configuration Detection
  invalid_config <- list(
    session_timeout = -1,
    max_login_attempts = 0,
    password_complexity = FALSE,
    mfa_required = FALSE
  )
  
  expect_false(validate_security_config(invalid_config))
  
  # Test 16.4: Edge Case - Missing required configuration
  incomplete_config <- list(
    session_timeout = 1800
    # Missing other required fields
  )
  
  expect_false(validate_security_config(incomplete_config))
})

test_that("Configuration Security - Environment Variable Protection", {
  
  # Test 17.1: Sensitive Environment Variables
  sensitive_vars <- c(
    "DB_PASSWORD", "JWT_SECRET", "ENCRYPTION_KEY", 
    "API_KEY", "OAUTH_CLIENT_SECRET"
  )
  
  for (var in sensitive_vars) {
    expect_true(is_sensitive_env_var(var))
  }
  
  # Test 17.2: Environment Variable Validation
  env_vars <- get_environment_variables()
  
  for (var in sensitive_vars) {
    if (var %in% names(env_vars)) {
      expect_true(is_properly_secured(env_vars[[var]]))
    }
  }
  
  # Test 17.3: Default Value Prevention
  default_values <- c("password", "secret", "changeme", "admin", "default")
  
  for (var in sensitive_vars) {
    if (var %in% names(env_vars)) {
      expect_false(tolower(env_vars[[var]]) %in% default_values)
    }
  }
  
  # Test 17.4: Edge Case - Environment variable exposure
  exposed_vars <- check_environment_exposure()
  expect_equal(length(exposed_vars), 0)
})

# ==============================================================================
# 8. ERROR HANDLING & INFORMATION DISCLOSURE COMPLIANCE TESTING
# ==============================================================================

test_that("Error Handling - Information Disclosure Prevention", {
  
  # Test 18.1: Generic Error Messages
  error_handler <- SecurityErrorHandler$new()
  
  # Database error
  db_error <- simpleError("ORA-00942: table or view does not exist")
  generic_error <- error_handler$handle_error(db_error, "database")
  expect_false(grepl("ORA-", generic_error$message))
  expect_true(grepl("database error", tolower(generic_error$message)))
  
  # File system error
  fs_error <- simpleError("No such file or directory: /etc/passwd")
  generic_fs_error <- error_handler$handle_error(fs_error, "file_system")
  expect_false(grepl("/etc/passwd", generic_fs_error$message))
  
  # Test 18.2: Stack Trace Sanitization
  stack_trace <- "Error in function_name() at line 42 of /app/secret/module.R"
  sanitized_trace <- sanitize_stack_trace(stack_trace)
  expect_false(grepl("/app/secret/", sanitized_trace))
  expect_false(grepl("line 42", sanitized_trace))
  
  # Test 18.3: Debug Information Filtering
  debug_info <- list(
    user_input = "SELECT * FROM users",
    file_path = "/home/user/app/config.R",
    environment = "production",
    session_data = list(user_id = "USER001", role = "admin")
  )
  
  filtered_debug <- filter_debug_information(debug_info)
  expect_null(filtered_debug$user_input)
  expect_null(filtered_debug$file_path)
  expect_null(filtered_debug$session_data)
  
  # Test 18.4: Edge Case - Nested error objects
  nested_error <- simpleError("Outer error", call = quote(inner_function()))