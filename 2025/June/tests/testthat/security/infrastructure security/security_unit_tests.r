# Atlas Labs HR Analytics - Infrastructure Security Unit Tests
# Test Suite for: Intrusion Detection, Vulnerability Scanning, Patch Management, Backup Security
# Author: Atlas Labs Security Team
# Version: 1.0.0

# Required libraries for testing
library(testthat)
library(shiny)
library(DT)
library(httr)
library(jsonlite)
library(digest)
library(openssl)
library(sodium)
library(base64enc)
library(curl)
library(processx)

# =============================================================================
# 1. INTRUSION DETECTION TESTING
# =============================================================================

# Test Context: Intrusion Detection System
context("Infrastructure Security - Intrusion Detection Testing")

# Security monitoring utility functions
source("utils/security_utils.R")

test_that("Intrusion Detection - SQL Injection Attempts", {
  # Test SQL injection patterns in user inputs
  malicious_inputs <- c(
    "'; DROP TABLE employees; --",
    "1' OR '1'='1",
    "UNION SELECT * FROM performance_rating",
    "admin'--",
    "1; DELETE FROM employee WHERE 1=1",
    "' OR 1=1 /*",
    "' UNION SELECT password FROM users --"
  )
  
  # Mock input validation function
  validate_input <- function(input) {
    sql_patterns <- c(
      "(?i)(union|select|insert|update|delete|drop|create|alter|exec|execute)",
      "(?i)(script|javascript|vbscript|onload|onerror)",
      "(?i)(--|/\\*|\\*/|;|'|\")",
      "(?i)(or\\s+1=1|and\\s+1=1)"
    )
    
    for (pattern in sql_patterns) {
      if (grepl(pattern, input, perl = TRUE)) {
        return(list(valid = FALSE, threat = "SQL_INJECTION", pattern = pattern))
      }
    }
    return(list(valid = TRUE, threat = NULL, pattern = NULL))
  }
  
  # Test each malicious input
  for (input in malicious_inputs) {
    result <- validate_input(input)
    expect_false(result$valid, 
                 info = paste("SQL injection not detected for:", input))
    expect_equal(result$threat, "SQL_INJECTION",
                 info = paste("Threat type incorrect for:", input))
  }
  
  # Test legitimate inputs
  legitimate_inputs <- c("John Doe", "Software Engineer", "HR Department", "2023-01-15")
  for (input in legitimate_inputs) {
    result <- validate_input(input)
    expect_true(result$valid,
                info = paste("Legitimate input flagged as threat:", input))
  }
})

test_that("Intrusion Detection - XSS Attack Patterns", {
  # Test Cross-Site Scripting patterns
  xss_payloads <- c(
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')",
    "<svg onload=alert('XSS')>",
    "';alert('XSS');//",
    "<iframe src=javascript:alert('XSS')></iframe>",
    "<body onload=alert('XSS')>",
    "<%2fscript%2f>alert('XSS')<%2fscript%2f>"
  )
  
  detect_xss <- function(input) {
    xss_patterns <- c(
      "(?i)<script[^>]*>.*?</script>",
      "(?i)<.*?on\\w+\\s*=.*?>",
      "(?i)javascript:",
      "(?i)<iframe[^>]*src\\s*=\\s*['\"]?javascript:",
      "(?i)<svg[^>]*onload\\s*=",
      "(?i)<body[^>]*onload\\s*=",
      "(?i)<%2fscript%2f>"
    )
    
    for (pattern in xss_patterns) {
      if (grepl(pattern, input, perl = TRUE)) {
        return(list(detected = TRUE, threat = "XSS", pattern = pattern))
      }
    }
    return(list(detected = FALSE, threat = NULL, pattern = NULL))
  }
  
  # Test XSS detection
  for (payload in xss_payloads) {
    result <- detect_xss(payload)
    expect_true(result$detected,
                info = paste("XSS not detected for:", payload))
    expect_equal(result$threat, "XSS",
                 info = paste("Threat type incorrect for:", payload))
  }
})

test_that("Intrusion Detection - Session Hijacking Protection", {
  # Test session token validation
  generate_secure_token <- function() {
    paste0(
      format(Sys.time(), "%Y%m%d%H%M%S"),
      sample(letters, 32, replace = TRUE) %>% paste(collapse = ""),
      digest(runif(1), algo = "sha256") %>% substr(1, 16)
    )
  }
  
  validate_session_token <- function(token, user_agent, ip_address) {
    # Basic validation checks
    if (is.null(token) || nchar(token) < 32) {
      return(list(valid = FALSE, reason = "TOKEN_TOO_SHORT"))
    }
    
    # Check for suspicious patterns
    if (grepl("[<>\"'%;()&+]", token)) {
      return(list(valid = FALSE, reason = "SUSPICIOUS_CHARACTERS"))
    }
    
    # Validate token format (timestamp + random + hash)
    if (!grepl("^[0-9]{14}[a-z]{32}[a-f0-9]{16}$", token)) {
      return(list(valid = FALSE, reason = "INVALID_FORMAT"))
    }
    
    return(list(valid = TRUE, reason = NULL))
  }
  
  # Test legitimate tokens
  valid_token <- generate_secure_token()
  result <- validate_session_token(valid_token, "Mozilla/5.0", "192.168.1.100")
  expect_true(result$valid, info = "Valid token rejected")
  
  # Test malicious tokens
  malicious_tokens <- c(
    "short",
    "<script>alert('xss')</script>",
    "'; DROP TABLE sessions; --",
    "admin123",
    NULL,
    ""
  )
  
  for (token in malicious_tokens) {
    result <- validate_session_token(token, "Mozilla/5.0", "192.168.1.100")
    expect_false(result$valid,
                 info = paste("Malicious token accepted:", token))
  }
})

test_that("Intrusion Detection - Brute Force Attack Detection", {
  # Mock login attempt tracking
  login_attempts <- data.frame(
    ip_address = character(0),
    timestamp = as.POSIXct(character(0)),
    success = logical(0),
    user_agent = character(0),
    stringsAsFactors = FALSE
  )
  
  track_login_attempt <- function(ip, success, user_agent = "Unknown") {
    login_attempts <<- rbind(login_attempts, data.frame(
      ip_address = ip,
      timestamp = Sys.time(),
      success = success,
      user_agent = user_agent,
      stringsAsFactors = FALSE
    ))
  }
  
  detect_brute_force <- function(ip_address, time_window = 300) { # 5 minutes
    recent_attempts <- login_attempts[
      login_attempts$ip_address == ip_address &
      login_attempts$timestamp > (Sys.time() - time_window),
    ]
    
    failed_attempts <- sum(!recent_attempts$success)
    
    # Alert if more than 5 failed attempts in time window
    if (failed_attempts > 5) {
      return(list(
        threat_detected = TRUE,
        threat_level = "HIGH",
        failed_attempts = failed_attempts,
        action = "BLOCK_IP"
      ))
    }
    
    return(list(
      threat_detected = FALSE,
      threat_level = "LOW",
      failed_attempts = failed_attempts,
      action = "MONITOR"
    ))
  }
  
  # Simulate brute force attack
  attack_ip <- "192.168.1.666"
  for (i in 1:10) {
    track_login_attempt(attack_ip, FALSE, "AttackBot/1.0")
    Sys.sleep(0.1) # Small delay to simulate real attempts
  }
  
  # Test brute force detection
  result <- detect_brute_force(attack_ip)
  expect_true(result$threat_detected, info = "Brute force attack not detected")
  expect_equal(result$threat_level, "HIGH", info = "Threat level incorrect")
  expect_equal(result$action, "BLOCK_IP", info = "Action incorrect")
  expect_gte(result$failed_attempts, 5, info = "Failed attempts count incorrect")
  
  # Test legitimate user
  legitimate_ip <- "192.168.1.100"
  track_login_attempt(legitimate_ip, TRUE, "Mozilla/5.0")
  track_login_attempt(legitimate_ip, FALSE, "Mozilla/5.0")
  track_login_attempt(legitimate_ip, TRUE, "Mozilla/5.0")
  
  result <- detect_brute_force(legitimate_ip)
  expect_false(result$threat_detected, info = "Legitimate user flagged")
  expect_equal(result$threat_level, "LOW", info = "Threat level should be LOW")
})

# =============================================================================
# 2. VULNERABILITY SCANNING
# =============================================================================

context("Infrastructure Security - Vulnerability Scanning")

test_that("Vulnerability Scanning - Package Dependencies", {
  # Get installed packages
  installed_packages <- as.data.frame(installed.packages())
  
  # Mock vulnerability database
  vulnerable_packages <- data.frame(
    package = c("shiny", "DT", "httr", "jsonlite", "openssl"),
    vulnerable_version = c("1.6.0", "0.15", "1.4.0", "1.6.0", "1.4.0"),
    severity = c("HIGH", "MEDIUM", "HIGH", "LOW", "CRITICAL"),
    cve = c("CVE-2021-1234", "CVE-2021-5678", "CVE-2021-9012", "CVE-2021-3456", "CVE-2021-7890"),
    stringsAsFactors = FALSE
  )
  
  scan_package_vulnerabilities <- function(package_name, package_version) {
    vuln <- vulnerable_packages[vulnerable_packages$package == package_name, ]
    
    if (nrow(vuln) == 0) {
      return(list(vulnerable = FALSE, severity = "NONE"))
    }
    
    # Compare versions (simplified)
    if (package_version <= vuln$vulnerable_version) {
      return(list(
        vulnerable = TRUE,
        severity = vuln$severity,
        cve = vuln$cve,
        vulnerable_version = vuln$vulnerable_version
      ))
    }
    
    return(list(vulnerable = FALSE, severity = "NONE"))
  }
  
  # Test critical packages
  critical_packages <- c("shiny", "DT", "httr", "jsonlite", "openssl")
  
  for (pkg in critical_packages) {
    if (pkg %in% installed_packages$Package) {
      pkg_version <- installed_packages[installed_packages$Package == pkg, "Version"]
      result <- scan_package_vulnerabilities(pkg, pkg_version)
      
      # Log vulnerability findings
      if (result$vulnerable) {
        warning(paste("VULNERABILITY FOUND:", pkg, "version", pkg_version, 
                     "- Severity:", result$severity, "- CVE:", result$cve))
      }
      
      # Assert that critical vulnerabilities are addressed
      expect_false(result$vulnerable && result$severity == "CRITICAL",
                   info = paste("Critical vulnerability in", pkg))
    }
  }
})

test_that("Vulnerability Scanning - Input Validation Gaps", {
  # Test input validation functions
  validate_employee_id <- function(id) {
    if (is.null(id) || is.na(id)) return(FALSE)
    if (!is.numeric(id) && !grepl("^[0-9]+$", id)) return(FALSE)
    if (as.numeric(id) < 1 || as.numeric(id) > 999999) return(FALSE)
    return(TRUE)
  }
  
  validate_date_input <- function(date_str) {
    if (is.null(date_str) || nchar(date_str) == 0) return(FALSE)
    
    # Check for injection patterns
    if (grepl("[<>\"'%;()&+]", date_str)) return(FALSE)
    
    # Validate date format
    parsed_date <- tryCatch({
      as.Date(date_str)
    }, error = function(e) NULL)
    
    if (is.null(parsed_date) || is.na(parsed_date)) return(FALSE)
    
    # Check reasonable date range
    if (parsed_date < as.Date("1900-01-01") || parsed_date > Sys.Date()) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Test employee ID validation
  valid_ids <- c(1, 123, 999999, "456", "789")
  for (id in valid_ids) {
    expect_true(validate_employee_id(id),
                info = paste("Valid employee ID rejected:", id))
  }
  
  invalid_ids <- c(0, -1, 1000000, "abc", "'; DROP TABLE", NULL, NA, "")
  for (id in invalid_ids) {
    expect_false(validate_employee_id(id),
                 info = paste("Invalid employee ID accepted:", id))
  }
  
  # Test date validation
  valid_dates <- c("2023-01-15", "2022-12-31", "2020-06-15")
  for (date in valid_dates) {
    expect_true(validate_date_input(date),
                info = paste("Valid date rejected:", date))
  }
  
  invalid_dates <- c("2025-01-01", "1899-12-31", "invalid", "'; DROP TABLE", "", NULL)
  for (date in invalid_dates) {
    expect_false(validate_date_input(date),
                 info = paste("Invalid date accepted:", date))
  }
})

test_that("Vulnerability Scanning - File Upload Security", {
  # Test file upload validation
  validate_file_upload <- function(filename, content_type, file_size) {
    # Check file extension
    allowed_extensions <- c(".csv", ".xlsx", ".xls")
    file_ext <- tools::file_ext(filename)
    if (!paste0(".", tolower(file_ext)) %in% allowed_extensions) {
      return(list(valid = FALSE, reason = "INVALID_EXTENSION"))
    }
    
    # Check MIME type
    allowed_mime_types <- c(
      "text/csv",
      "application/vnd.ms-excel",
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    if (!content_type %in% allowed_mime_types) {
      return(list(valid = FALSE, reason = "INVALID_MIME_TYPE"))
    }
    
    # Check file size (max 50MB)
    if (file_size > 50 * 1024 * 1024) {
      return(list(valid = FALSE, reason = "FILE_TOO_LARGE"))
    }
    
    # Check for suspicious filenames
    if (grepl("\\.\\.|\\/|\\\\|<|>|\\|", filename)) {
      return(list(valid = FALSE, reason = "SUSPICIOUS_FILENAME"))
    }
    
    return(list(valid = TRUE, reason = NULL))
  }
  
  # Test valid files
  valid_files <- list(
    list(filename = "employees.csv", content_type = "text/csv", size = 1024),
    list(filename = "data.xlsx", content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", size = 2048),
    list(filename = "report.xls", content_type = "application/vnd.ms-excel", size = 512)
  )
  
  for (file in valid_files) {
    result <- validate_file_upload(file$filename, file$content_type, file$size)
    expect_true(result$valid,
                info = paste("Valid file rejected:", file$filename))
  }
  
  # Test invalid files
  invalid_files <- list(
    list(filename = "malware.exe", content_type = "application/x-executable", size = 1024),
    list(filename = "script.js", content_type = "text/javascript", size = 512),
    list(filename = "../../../etc/passwd", content_type = "text/plain", size = 256),
    list(filename = "huge_file.csv", content_type = "text/csv", size = 100 * 1024 * 1024),
    list(filename = "data<script>.csv", content_type = "text/csv", size = 1024)
  )
  
  for (file in invalid_files) {
    result <- validate_file_upload(file$filename, file$content_type, file$size)
    expect_false(result$valid,
                 info = paste("Invalid file accepted:", file$filename))
  }
})

test_that("Vulnerability Scanning - API Endpoint Security", {
  # Mock API endpoint security validation
  validate_api_endpoint <- function(endpoint, method, headers, body) {
    vulnerabilities <- c()
    
    # Check for path traversal
    if (grepl("\\.\\.|\\/\\/|\\\\", endpoint)) {
      vulnerabilities <- c(vulnerabilities, "PATH_TRAVERSAL")
    }
    
    # Check for missing authentication
    if (!"Authorization" %in% names(headers)) {
      vulnerabilities <- c(vulnerabilities, "MISSING_AUTH")
    }
    
    # Check for SQL injection in parameters
    if (grepl("(?i)(union|select|insert|update|delete|drop)", body)) {
      vulnerabilities <- c(vulnerabilities, "SQL_INJECTION")
    }
    
    # Check for XSS in body
    if (grepl("(?i)<script|javascript:", body)) {
      vulnerabilities <- c(vulnerabilities, "XSS")
    }
    
    # Check for insecure methods
    if (method %in% c("TRACE", "TRACK", "DEBUG")) {
      vulnerabilities <- c(vulnerabilities, "INSECURE_METHOD")
    }
    
    return(list(
      secure = length(vulnerabilities) == 0,
      vulnerabilities = vulnerabilities
    ))
  }
  
  # Test secure endpoint
  secure_result <- validate_api_endpoint(
    endpoint = "/api/employees",
    method = "GET",
    headers = list(Authorization = "Bearer token123"),
    body = '{"filter": "active"}'
  )
  expect_true(secure_result$secure, info = "Secure endpoint flagged as vulnerable")
  
  # Test vulnerable endpoints
  vulnerable_tests <- list(
    list(
      endpoint = "/api/../../../etc/passwd",
      method = "GET",
      headers = list(),
      body = "",
      expected_vuln = "PATH_TRAVERSAL"
    ),
    list(
      endpoint = "/api/employees",
      method = "POST",
      headers = list(),
      body = '{"query": "SELECT * FROM users"}',
      expected_vuln = "SQL_INJECTION"
    ),
    list(
      endpoint = "/api/data",
      method = "TRACE",
      headers = list(Authorization = "Bearer token123"),
      body = "",
      expected_vuln = "INSECURE_METHOD"
    )
  )
  
  for (test in vulnerable_tests) {
    result <- validate_api_endpoint(test$endpoint, test$method, test$headers, test$body)
    expect_false(result$secure,
                 info = paste("Vulnerable endpoint not detected:", test$endpoint))
    expect_true(test$expected_vuln %in% result$vulnerabilities,
                info = paste("Expected vulnerability not found:", test$expected_vuln))
  }
})

# =============================================================================
# 3. PATCH MANAGEMENT VALIDATION
# =============================================================================

context("Infrastructure Security - Patch Management Validation")

test_that("Patch Management - R Version Validation", {
  # Check R version against known vulnerabilities
  current_r_version <- R.Version()$version.string
  
  # Mock vulnerable R versions
  vulnerable_r_versions <- c(
    "3.6.0", "3.6.1", "3.6.2", "3.6.3",
    "4.0.0", "4.0.1", "4.0.2",
    "4.1.0", "4.1.1"
  )
  
  check_r_version_security <- function(version_string) {
    # Extract version number
    version_match <- regexpr("\\d+\\.\\d+\\.\\d+", version_string)
    if (version_match == -1) return(list(secure = FALSE, reason = "INVALID_VERSION"))
    
    version_num <- regmatches(version_string, version_match)
    
    # Check against vulnerable versions
    if (version_num %in% vulnerable_r_versions) {
      return(list(
        secure = FALSE,
        reason = "VULNERABLE_VERSION",
        current_version = version_num,
        recommendation = "Update to latest R version"
      ))
    }
    
    # Check if version is too old (older than 2 years)
    version_parts <- as.numeric(strsplit(version_num, "\\.")[[1]])
    if (version_parts[1] < 4 || (version_parts[1] == 4 && version_parts[2] < 2)) {
      return(list(
        secure = FALSE,
        reason = "OUTDATED_VERSION",
        current_version = version_num,
        recommendation = "Update to supported R version"
      ))
    }
    
    return(list(secure = TRUE, reason = NULL, current_version = version_num))
  }
  
  # Test current R version
  result <- check_r_version_security(current_r_version)
  
  # Log findings
  if (!result$secure) {
    warning(paste("R Version Security Issue:", result$reason, "-", result$recommendation))
  }
  
  # For testing, we'll accept warnings but not failures for outdated versions
  if (result$reason == "VULNERABLE_VERSION") {
    expect_true(result$secure, info = paste("R version has known vulnerabilities:", result$current_version))
  }
})

test_that("Patch Management - Package Update Status", {
  # Check if critical packages are up to date
  check_package_updates <- function(packages) {
    # Mock function to check available updates
    # In real implementation, this would check CRAN
    
    update_info <- data.frame(
      package = character(0),
      installed_version = character(0),
      available_version = character(0),
      update_available = logical(0),
      security_update = logical(0),
      stringsAsFactors = FALSE
    )
    
    # Mock data for testing
    mock_updates <- data.frame(
      package = c("shiny", "DT", "httr", "jsonlite", "openssl"),
      installed_version = c("1.7.0", "0.20", "1.4.3", "1.8.0", "2.0.0"),
      available_version = c("1.7.1", "0.21", "1.4.4", "1.8.1", "2.0.1"),
      update_available = c(TRUE, TRUE, TRUE, TRUE, TRUE),
      security_update = c(TRUE, FALSE, TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
    
    return(mock_updates[mock_updates$package %in% packages, ])
  }
  
  # Test critical packages
  critical_packages <- c("shiny", "DT", "httr", "jsonlite", "openssl")
  update_status <- check_package_updates(critical_packages)
  
  # Check for security updates
  security_updates <- update_status[update_status$security_update, ]
  
  if (nrow(security_updates) > 0) {
    for (i in 1:nrow(security_updates)) {
      pkg <- security_updates[i, ]
      warning(paste("SECURITY UPDATE REQUIRED:", pkg$package, 
                   "- Current:", pkg$installed_version, 
                   "- Available:", pkg$available_version))
    }
  }
  
  # Test that we're tracking updates properly
  expect_true(nrow(update_status) > 0, info = "No package update information found")
  expect_true(all(c("package", "installed_version", "available_version", 
                   "update_available", "security_update") %in% names(update_status)),
              info = "Update status data structure incomplete")
})

test_that("Patch Management - System Dependencies", {
  # Check system-level dependencies
  check_system_dependencies <- function() {
    dependencies <- list()
    
    # Check OpenSSL version
    if (Sys.which("openssl") != "") {
      openssl_version <- tryCatch({
        system("openssl version", intern = TRUE)
      }, error = function(e) "Unknown")
      
      dependencies$openssl <- list(
        available = TRUE,
        version = openssl_version,
        secure = !grepl("1.0.|1.1.0", openssl_version) # Vulnerable versions
      )
    } else {
      dependencies$openssl <- list(available = FALSE, secure = FALSE)
    }
    
    # Check curl version
    if (Sys.which("curl") != "") {
      curl_version <- tryCatch({
        system("curl --version", intern = TRUE)[1]
      }, error = function(e) "Unknown")
      
      dependencies$curl <- list(
        available = TRUE,
        version = curl_version,
        secure = !grepl("curl 7\\.[0-6]", curl_version) # Old versions
      )
    } else {
      dependencies$curl <- list(available = FALSE, secure = FALSE)
    }
    
    # Check Git version (for development environments)
    if (Sys.which("git") != "") {
      git_version <- tryCatch({
        system("git --version", intern = TRUE)
      }, error = function(e) "Unknown")
      
      dependencies$git <- list(
        available = TRUE,
        version = git_version,
        secure = !grepl("git version 2\\.[0-9]\\.", git_version) # Check for old versions
      )
    } else {
      dependencies$git <- list(available = FALSE, secure = TRUE) # Not critical
    }
    
    return(dependencies)
  }
  
  # Test system dependencies
  sys_deps <- check_system_dependencies()
  
  # Verify critical dependencies are available and secure
  critical_deps <- c("openssl", "curl")
  
  for (dep in critical_deps) {
    if (dep %in% names(sys_deps)) {
      expect_true(sys_deps[[dep]]$available,
                  info = paste("Critical dependency not available:", dep))
      
      if (sys_deps[[dep]]$available) {
        expect_true(sys_deps[[dep]]$secure,
                    info = paste("Critical dependency has security issues:", dep, 
                               "- Version:", sys_deps[[dep]]$version))
      }
    }
  }
})

test_that("Patch Management - Configuration Security", {
  # Test security configuration settings
  check_security_config <- function() {
    config <- list()
    
    # Check session configuration
    config$session_timeout <- getOption("shiny.session.timeout", 0)
    config$max_request_size <- getOption("shiny.maxRequestSize", 5 * 1024^2)
    config$sanitize_errors <- getOption("shiny.sanitize.errors", FALSE)
    
    # Check environment variables
    config$https_only <- Sys.getenv("FORCE_HTTPS", "false") == "true"
    config$debug_mode <- Sys.getenv("SHINY_DEBUG", "false") == "true"
    
    return(config)
  }
  
  validate_security_config <- function(config) {
    issues <- c()
    
    # Session timeout should be set (not 0)
    if (config$session_timeout == 0) {
      issues <- c(issues, "SESSION_TIMEOUT_DISABLED")
    }
    
    # Request size should be limited
    if (config$max_request_size > 50 * 1024^2) { # 50MB
      issues <- c(issues, "REQUEST_SIZE_TOO_LARGE")
    }
    
    # Errors should be sanitized in production
    if (!config$sanitize_errors) {
      issues <- c(issues, "ERROR_SANITIZATION_DISABLED")
    }
    
    # HTTPS should be enforced
    if (!config$https_only) {
      issues <- c(issues, "HTTPS_NOT_ENFORCED")
    }
    
    # Debug mode should be disabled in production
    if (config$debug_mode) {
      issues <- c(issues, "DEBUG_MODE_ENABLED")
    }
    
    return(list(
      secure = length(issues) == 0,
      issues = issues
    ))
  }
  
  # Test security configuration
  config <- check_security_config()
  validation <- validate_security_config(config)
  
  # Log configuration issues
  if (!validation$secure) {
    for (issue in validation$issues) {
      warning(paste("SECURITY CONFIGURATION ISSUE:", issue))
    }
  }
  
  # Test specific security requirements
  expect_true(config$session_timeout > 0 || validation$secure,
              info = "Session timeout should be configured")
  expect_true(config$max_request_size <= 50 * 1024^2,
              info = "Request size limit too high")
})

# =============================================================================
# 4. BACKUP SECURITY VERIFICATION
# =============================================================================

context("Infrastructure Security - Backup Security Verification")

test_that("Backup Security - Data Encryption", {
  # Test backup encryption functionality
  
  # Mock encryption functions
  encrypt_backup_data <- function(data, key) {
    if (is.null(key) || nchar(key) < 32) {
      stop("Encryption key too short")
    }
    
    # Simulate encryption (in real implementation, use proper encryption)
    encrypted <- paste0("ENCRYPTED_", digest(paste(data, key), algo = "sha256"))
    
    return(list(
      encrypted_data = encrypted,
      checksum = digest(data, algo = "md5"),
      encryption_method = "AES-256-GCM",
      key_id = digest(key, algo = "sha256") %>% substr(1, 8)
    ))
  }
  
  decrypt_backup_data <- function(encrypted_data, key, checksum) {
    if (is.null(key) || nchar(key) < 32