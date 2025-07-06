# Atlas Labs HR Analytics Dashboard - Security Unit Tests
# Comprehensive testing for application security components
# Author: akhapwoyaco
# Version: 1.0.0

# Load required libraries for testing
library(testthat)
library(shiny)
library(httr)
library(jsonlite)
library(xml2)
library(rvest)
library(openssl)
library(curl)

# Source the security configuration module
source("modules/security_config.R")

# Test Suite: Application Security
describe("Atlas Labs HR Analytics - Application Security Tests", {
  
  # =============================================================================
  # 1. SECURE HEADERS VALIDATION
  # =============================================================================
  
  describe("Secure Headers Validation", {
    
    # Test basic security headers presence
    test_that("Essential security headers are present", {
      # Mock HTTP response with security headers
      mock_response <- list(
        headers = list(
          `x-frame-options` = "DENY",
          `x-content-type-options` = "nosniff",
          `x-xss-protection` = "1; mode=block",
          `strict-transport-security` = "max-age=31536000; includeSubDomains",
          `content-security-policy` = "default-src 'self'",
          `referrer-policy` = "strict-origin-when-cross-origin"
        )
      )
      
      # Validate each header
      expect_equal(mock_response$headers$`x-frame-options`, "DENY")
      expect_equal(mock_response$headers$`x-content-type-options`, "nosniff")
      expect_equal(mock_response$headers$`x-xss-protection`, "1; mode=block")
      expect_match(mock_response$headers$`strict-transport-security`, "max-age=\\d+")
      expect_match(mock_response$headers$`content-security-policy`, "default-src")
      expect_true(!is.null(mock_response$headers$`referrer-policy`))
    })
    
    # Test X-Frame-Options header variations
    test_that("X-Frame-Options header validation", {
      valid_options <- c("DENY", "SAMEORIGIN", "ALLOW-FROM https://example.com")
      invalid_options <- c("", "INVALID", "deny", "sameorigin", "allow-from http://evil.com")
      
      for (option in valid_options) {
        expect_true(validate_x_frame_options(option))
      }
      
      for (option in invalid_options) {
        expect_false(validate_x_frame_options(option))
      }
    })
    
    # Test X-Content-Type-Options header
    test_that("X-Content-Type-Options header validation", {
      expect_true(validate_content_type_options("nosniff"))
      expect_false(validate_content_type_options(""))
      expect_false(validate_content_type_options("sniff"))
      expect_false(validate_content_type_options("NOSNIFF"))
    })
    
    # Test X-XSS-Protection header
    test_that("X-XSS-Protection header validation", {
      valid_xss <- c("1; mode=block", "0", "1", "1; report=https://example.com/report")
      invalid_xss <- c("", "2", "1; mode=filter", "1; mode=")
      
      for (xss in valid_xss) {
        expect_true(validate_xss_protection(xss))
      }
      
      for (xss in invalid_xss) {
        expect_false(validate_xss_protection(xss))
      }
    })
    
    # Test Strict-Transport-Security header
    test_that("HSTS header validation", {
      valid_hsts <- c(
        "max-age=31536000",
        "max-age=31536000; includeSubDomains",
        "max-age=31536000; includeSubDomains; preload"
      )
      invalid_hsts <- c(
        "",
        "max-age=",
        "max-age=0",
        "max-age=-1",
        "max-age=abc",
        "includeSubDomains"
      )
      
      for (hsts in valid_hsts) {
        expect_true(validate_hsts_header(hsts))
      }
      
      for (hsts in invalid_hsts) {
        expect_false(validate_hsts_header(hsts))
      }
    })
    
    # Test Referrer-Policy header
    test_that("Referrer-Policy header validation", {
      valid_policies <- c(
        "no-referrer",
        "no-referrer-when-downgrade",
        "origin",
        "origin-when-cross-origin",
        "same-origin",
        "strict-origin",
        "strict-origin-when-cross-origin",
        "unsafe-url"
      )
      invalid_policies <- c("", "invalid-policy", "no-referrer-invalid")
      
      for (policy in valid_policies) {
        expect_true(validate_referrer_policy(policy))
      }
      
      for (policy in invalid_policies) {
        expect_false(validate_referrer_policy(policy))
      }
    })
    
    # Test header injection prevention
    test_that("Header injection prevention", {
      malicious_headers <- c(
        "value\r\nX-Injected: malicious",
        "value\nX-Injected: malicious",
        "value\r\n\r\n<script>alert('xss')</script>",
        "value\x00\x0a\x0dX-Injected: malicious"
      )
      
      for (header in malicious_headers) {
        expect_false(validate_header_value(header))
      }
    })
  })
  
  # =============================================================================
  # 2. CSRF PROTECTION TESTING
  # =============================================================================
  
  describe("CSRF Protection Testing", {
    
    # Test CSRF token generation
    test_that("CSRF token generation", {
      token1 <- generate_csrf_token()
      token2 <- generate_csrf_token()
      
      expect_true(nchar(token1) >= 32)
      expect_true(nchar(token2) >= 32)
      expect_false(token1 == token2)
      expect_match(token1, "^[a-zA-Z0-9]+$")
    })
    
    # Test CSRF token validation
    test_that("CSRF token validation", {
      # Mock session with CSRF token
      mock_session <- list(csrf_token = "valid_token_123")
      
      # Valid token
      expect_true(validate_csrf_token("valid_token_123", mock_session))
      
      # Invalid tokens
      expect_false(validate_csrf_token("invalid_token", mock_session))
      expect_false(validate_csrf_token("", mock_session))
      expect_false(validate_csrf_token(NULL, mock_session))
      expect_false(validate_csrf_token("valid_token_123", list()))
    })
    
    # Test CSRF middleware
    test_that("CSRF middleware protection", {
      # Mock request without CSRF token
      unsafe_request <- list(
        method = "POST",
        headers = list(),
        body = list(action = "delete_employee")
      )
      
      # Mock request with valid CSRF token
      safe_request <- list(
        method = "POST",
        headers = list(`X-CSRF-Token` = "valid_token_123"),
        body = list(action = "delete_employee")
      )
      
      # Mock session
      mock_session <- list(csrf_token = "valid_token_123")
      
      expect_false(csrf_middleware(unsafe_request, mock_session))
      expect_true(csrf_middleware(safe_request, mock_session))
    })
    
    # Test CSRF protection for state-changing operations
    test_that("CSRF protection for state-changing operations", {
      state_changing_actions <- c(
        "update_employee",
        "delete_employee", 
        "change_password",
        "update_settings",
        "generate_report"
      )
      
      for (action in state_changing_actions) {
        request <- list(
          method = "POST",
          body = list(action = action)
        )
        expect_true(requires_csrf_protection(request))
      }
      
      # Read-only operations should not require CSRF
      safe_actions <- c("view_dashboard", "export_data", "search_employees")
      for (action in safe_actions) {
        request <- list(
          method = "GET",
          body = list(action = action)
        )
        expect_false(requires_csrf_protection(request))
      }
    })
    
    # Test CSRF token expiration
    test_that("CSRF token expiration", {
      # Create token with timestamp
      token_with_time <- list(
        token = "test_token_123",
        created_at = Sys.time() - 3600  # 1 hour ago
      )
      
      # Test expired token
      expect_false(is_csrf_token_valid(token_with_time, max_age = 1800))  # 30 min max age
      
      # Test valid token
      fresh_token <- list(
        token = "test_token_456",
        created_at = Sys.time() - 900  # 15 minutes ago
      )
      expect_true(is_csrf_token_valid(fresh_token, max_age = 1800))
    })
  })
  
  # =============================================================================
  # 3. CLICKJACKING PREVENTION
  # =============================================================================
  
  describe("Clickjacking Prevention", {
    
    # Test X-Frame-Options implementation
    test_that("X-Frame-Options prevents clickjacking", {
      # Test DENY policy
      expect_true(prevents_clickjacking("DENY"))
      expect_true(prevents_clickjacking("SAMEORIGIN"))
      expect_false(prevents_clickjacking(""))
      expect_false(prevents_clickjacking(NULL))
    })
    
    # Test frame-ancestors CSP directive
    test_that("CSP frame-ancestors directive", {
      csp_policies <- c(
        "frame-ancestors 'none'",
        "frame-ancestors 'self'",
        "frame-ancestors 'self' https://trusted.com",
        "default-src 'self'; frame-ancestors 'none'"
      )
      
      for (policy in csp_policies) {
        expect_true(has_frame_ancestors_directive(policy))
      }
      
      # Test policies without frame-ancestors
      invalid_policies <- c(
        "default-src 'self'",
        "script-src 'self'",
        ""
      )
      
      for (policy in invalid_policies) {
        expect_false(has_frame_ancestors_directive(policy))
      }
    })
    
    # Test clickjacking protection validation
    test_that("Comprehensive clickjacking protection", {
      # Strong protection
      strong_headers <- list(
        `x-frame-options` = "DENY",
        `content-security-policy` = "frame-ancestors 'none'"
      )
      expect_true(has_clickjacking_protection(strong_headers))
      
      # Moderate protection
      moderate_headers <- list(
        `x-frame-options` = "SAMEORIGIN"
      )
      expect_true(has_clickjacking_protection(moderate_headers))
      
      # No protection
      no_protection <- list()
      expect_false(has_clickjacking_protection(no_protection))
    })
    
    # Test conflicting frame policies
    test_that("Conflicting frame policies detection", {
      conflicting_headers <- list(
        `x-frame-options` = "SAMEORIGIN",
        `content-security-policy` = "frame-ancestors 'none'"
      )
      
      expect_true(has_conflicting_frame_policies(conflicting_headers))
      
      consistent_headers <- list(
        `x-frame-options` = "DENY",
        `content-security-policy` = "frame-ancestors 'none'"
      )
      
      expect_false(has_conflicting_frame_policies(consistent_headers))
    })
  })
  
  # =============================================================================
  # 4. CONTENT SECURITY POLICY
  # =============================================================================
  
  describe("Content Security Policy", {
    
    # Test CSP header parsing
    test_that("CSP header parsing", {
      csp_header <- "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'"
      parsed_csp <- parse_csp_header(csp_header)
      
      expect_equal(parsed_csp$`default-src`, c("'self'"))
      expect_equal(parsed_csp$`script-src`, c("'self'", "'unsafe-inline'"))
      expect_equal(parsed_csp$`style-src`, c("'self'", "'unsafe-inline'"))
    })
    
    # Test CSP directive validation
    test_that("CSP directive validation", {
      # Valid directives
      valid_directives <- c(
        "default-src",
        "script-src",
        "style-src",
        "img-src",
        "font-src",
        "connect-src",
        "frame-ancestors",
        "form-action"
      )
      
      for (directive in valid_directives) {
        expect_true(is_valid_csp_directive(directive))
      }
      
      # Invalid directives
      invalid_directives <- c("invalid-src", "script", "style")
      for (directive in invalid_directives) {
        expect_false(is_valid_csp_directive(directive))
      }
    })
    
    # Test CSP source validation
    test_that("CSP source validation", {
      valid_sources <- c(
        "'self'",
        "'unsafe-inline'",
        "'unsafe-eval'",
        "'none'",
        "https://cdnjs.cloudflare.com",
        "https://*.example.com",
        "data:",
        "blob:"
      )
      
      for (source in valid_sources) {
        expect_true(is_valid_csp_source(source))
      }
      
      # Invalid sources
      invalid_sources <- c(
        "http://unsecure.com",
        "javascript:",
        "vbscript:",
        "about:",
        "file://"
      )
      
      for (source in invalid_sources) {
        expect_false(is_valid_csp_source(source))
      }
    })
    
    # Test restrictive CSP policy
    test_that("Restrictive CSP policy validation", {
      restrictive_csp <- "default-src 'self'; script-src 'self' https://cdnjs.cloudflare.com; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self' https://fonts.gstatic.com; connect-src 'self'; frame-ancestors 'none'; form-action 'self'"
      
      expect_true(is_restrictive_csp(restrictive_csp))
      
      # Permissive CSP
      permissive_csp <- "default-src *; script-src * 'unsafe-inline' 'unsafe-eval'"
      expect_false(is_restrictive_csp(permissive_csp))
    })
    
    # Test CSP nonce validation
    test_that("CSP nonce validation", {
      # Valid nonces
      valid_nonces <- c(
        "nonce-abc123def456",
        "nonce-" %+% base64_encode(openssl::rand_bytes(16))
      )
      
      for (nonce in valid_nonces) {
        expect_true(is_valid_csp_nonce(nonce))
      }
      
      # Invalid nonces
      invalid_nonces <- c(
        "nonce-short",
        "nonce-",
        "abc123def456",
        "nonce-<script>"
      )
      
      for (nonce in invalid_nonces) {
        expect_false(is_valid_csp_nonce(nonce))
      }
    })
    
    # Test CSP violation reporting
    test_that("CSP violation reporting", {
      csp_with_report <- "default-src 'self'; report-uri /csp-violation-report"
      expect_true(has_csp_reporting(csp_with_report))
      
      csp_without_report <- "default-src 'self'"
      expect_false(has_csp_reporting(csp_without_report))
    })
  })
  
  # =============================================================================
  # 5. SECURE COOKIE CONFIGURATION
  # =============================================================================
  
  describe("Secure Cookie Configuration", {
    
    # Test cookie security attributes
    test_that("Cookie security attributes", {
      secure_cookie <- list(
        name = "session_id",
        value = "abc123def456",
        secure = TRUE,
        httponly = TRUE,
        samesite = "Strict",
        path = "/",
        domain = "atlaslabs.com"
      )
      
      expect_true(is_secure_cookie(secure_cookie))
      
      # Insecure cookie
      insecure_cookie <- list(
        name = "session_id",
        value = "abc123def456",
        secure = FALSE,
        httponly = FALSE,
        samesite = "None"
      )
      
      expect_false(is_secure_cookie(insecure_cookie))
    })
    
    # Test SameSite attribute validation
    test_that("SameSite attribute validation", {
      valid_samesite <- c("Strict", "Lax", "None")
      for (value in valid_samesite) {
        expect_true(is_valid_samesite(value))
      }
      
      invalid_samesite <- c("", "Invalid", "strict", "lax")
      for (value in invalid_samesite) {
        expect_false(is_valid_samesite(value))
      }
    })
    
    # Test cookie expiration
    test_that("Cookie expiration validation", {
      # Valid expiration times
      future_time <- Sys.time() + 3600  # 1 hour from now
      expect_true(is_valid_cookie_expiration(future_time))
      
      # Invalid expiration times
      past_time <- Sys.time() - 3600  # 1 hour ago
      expect_false(is_valid_cookie_expiration(past_time))
    })
    
    # Test session cookie configuration
    test_that("Session cookie configuration", {
      session_config <- list(
        name = "JSESSIONID",
        secure = TRUE,
        httponly = TRUE,
        samesite = "Strict",
        max_age = 3600,
        path = "/",
        domain = ".atlaslabs.com"
      )
      
      expect_true(is_secure_session_config(session_config))
      
      # Test session timeout
      expect_true(session_config$max_age <= 86400)  # Max 24 hours
    })
    
    # Test cookie domain validation
    test_that("Cookie domain validation", {
      valid_domains <- c(
        "atlaslabs.com",
        ".atlaslabs.com",
        "subdomain.atlaslabs.com"
      )
      
      for (domain in valid_domains) {
        expect_true(is_valid_cookie_domain(domain))
      }
      
      invalid_domains <- c(
        "evil.com",
        ".evil.com",
        "127.0.0.1",
        "localhost"
      )
      
      for (domain in invalid_domains) {
        expect_false(is_valid_cookie_domain(domain))
      }
    })
  })
  
  # =============================================================================
  # 6. HTTP SECURITY HEADERS
  # =============================================================================
  
  describe("HTTP Security Headers", {
    
    # Test complete security headers presence
    test_that("Complete security headers validation", {
      complete_headers <- list(
        `strict-transport-security` = "max-age=31536000; includeSubDomains",
        `x-content-type-options` = "nosniff",
        `x-frame-options` = "DENY",
        `x-xss-protection` = "1; mode=block",
        `content-security-policy` = "default-src 'self'",
        `referrer-policy` = "strict-origin-when-cross-origin",
        `permissions-policy` = "camera=(), microphone=(), geolocation=()"
      )
      
      expect_true(has_complete_security_headers(complete_headers))
      
      # Missing critical headers
      incomplete_headers <- list(
        `x-content-type-options` = "nosniff"
      )
      
      expect_false(has_complete_security_headers(incomplete_headers))
    })
    
    # Test Permissions-Policy header
    test_that("Permissions-Policy header validation", {
      valid_permissions <- c(
        "camera=(), microphone=(), geolocation=()",
        "camera=('self'), microphone=('none')",
        "geolocation=('self' 'https://maps.googleapis.com')"
      )
      
      for (permission in valid_permissions) {
        expect_true(is_valid_permissions_policy(permission))
      }
      
      invalid_permissions <- c(
        "",
        "camera=*",
        "invalid-feature=()"
      )
      
      for (permission in invalid_permissions) {
        expect_false(is_valid_permissions_policy(permission))
      }
    })
    
    # Test security header values
    test_that("Security header values validation", {
      # Test various header combinations
      headers_test_cases <- list(
        list(
          headers = list(`x-frame-options` = "DENY"),
          expected = TRUE
        ),
        list(
          headers = list(`x-frame-options` = "ALLOWALL"),
          expected = FALSE
        ),
        list(
          headers = list(`x-content-type-options` = "nosniff"),
          expected = TRUE
        ),
        list(
          headers = list(`x-content-type-options` = "sniff"),
          expected = FALSE
        )
      )
      
      for (test_case in headers_test_cases) {
        result <- validate_security_headers(test_case$headers)
        expect_equal(result, test_case$expected)
      }
    })
    
    # Test header case sensitivity
    test_that("Header case sensitivity", {
      # Headers should be case-insensitive
      upper_headers <- list(
        `X-FRAME-OPTIONS` = "DENY",
        `X-CONTENT-TYPE-OPTIONS` = "nosniff"
      )
      
      lower_headers <- list(
        `x-frame-options` = "DENY",
        `x-content-type-options` = "nosniff"
      )
      
      expect_true(normalize_headers(upper_headers) == normalize_headers(lower_headers))
    })
  })
  
  # =============================================================================
  # 7. SSL/TLS CONFIGURATION
  # =============================================================================
  
  describe("SSL/TLS Configuration", {
    
    # Test TLS version validation
    test_that("TLS version validation", {
      valid_versions <- c("TLSv1.2", "TLSv1.3")
      for (version in valid_versions) {
        expect_true(is_valid_tls_version(version))
      }
      
      invalid_versions <- c("SSLv3", "TLSv1.0", "TLSv1.1")
      for (version in invalid_versions) {
        expect_false(is_valid_tls_version(version))
      }
    })
    
    # Test cipher suite validation
    test_that("Cipher suite validation", {
      secure_ciphers <- c(
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES128-GCM-SHA256",
        "ECDHE-RSA-AES256-SHA384",
        "ECDHE-RSA-AES128-SHA256"
      )
      
      for (cipher in secure_ciphers) {
        expect_true(is_secure_cipher(cipher))
      }
      
      weak_ciphers <- c(
        "DES-CBC-SHA",
        "RC4-SHA",
        "NULL-SHA",
        "ADH-AES256-SHA"
      )
      
      for (cipher in weak_ciphers) {
        expect_false(is_secure_cipher(cipher))
      }
    })
    
    # Test SSL configuration
    test_that("SSL configuration validation", {
      ssl_config <- list(
        tls_version = "TLSv1.2",
        ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256",
        hsts_enabled = TRUE,
        redirect_http = TRUE,
        perfect_forward_secrecy = TRUE
      )
      
      expect_true(is_secure_ssl_config(ssl_config))
      
      # Insecure configuration
      insecure_config <- list(
        tls_version = "TLSv1.0",
        ciphers = "DES-CBC-SHA",
        hsts_enabled = FALSE,
        redirect_http = FALSE
      )
      
      expect_false(is_secure_ssl_config(insecure_config))
    })
    
    # Test HSTS configuration
    test_that("HSTS configuration validation", {
      hsts_configs <- list(
        list(
          max_age = 31536000,
          include_subdomains = TRUE,
          preload = TRUE,
          expected = TRUE
        ),
        list(
          max_age = 86400,
          include_subdomains = FALSE,
          preload = FALSE,
          expected = TRUE
        ),
        list(
          max_age = 0,
          include_subdomains = TRUE,
          preload = TRUE,
          expected = FALSE
        )
      )
      
      for (config in hsts_configs) {
        result <- is_valid_hsts_config(config)
        expect_equal(result, config$expected)
      }
    })
  })
  
  # =============================================================================
  # 8. CERTIFICATE VALIDATION
  # =============================================================================
  
  describe("Certificate Validation", {
    
    # Test certificate format validation
    test_that("Certificate format validation", {
      # Mock valid certificate
      valid_cert <- "-----BEGIN CERTIFICATE-----\nMIIEpDCCA4ygAwIBAgIJAKfVHHfVTAV...\n-----END CERTIFICATE-----"
      expect_true(is_valid_certificate_format(valid_cert))
      
      # Invalid certificate formats
      invalid_certs <- c(
        "",
        "invalid certificate",
        "-----BEGIN CERTIFICATE-----\ninvalid\n-----END CERTIFICATE-----"
      )
      
      for (cert in invalid_certs) {
        expect_false(is_valid_certificate_format(cert))
      }
    })
    
    # Test certificate expiration
    test_that("Certificate expiration validation", {
      # Mock certificate with expiration dates
      future_cert <- list(
        not_before = Sys.time() - 86400,  # 1 day ago
        not_after = Sys.time() + 86400 * 30  # 30 days from now
      )
      
      expect_true(is_certificate_valid(future_cert))
      
      # Expired certificate
      expired_cert <- list(
        not_before = Sys.time() - 86400 * 365,  # 1 year ago
        not_after = Sys.time() - 86400  # 1 day ago
      )
      
      expect_false(is_certificate_valid(expired_cert))
    })
    
    # Test certificate chain validation
    test_that("Certificate chain validation", {
      # Mock certificate chain
      cert_chain <- list(
        leaf = list(
          subject = "CN=atlaslabs.com",
          issuer = "CN=Intermediate CA",
          valid = TRUE
        ),
        intermediate = list(
          subject = "CN=Intermediate CA",
          issuer = "CN=Root CA",
          valid = TRUE
        ),
        root = list(
          subject = "CN=Root CA",
          issuer = "CN=Root CA",
          valid = TRUE
        )
      )
      
      expect_true(is_valid_certificate_chain(cert_chain))
      
      # Broken chain
      broken_chain <- list(
        leaf = list(
          subject = "CN=atlaslabs.com",
          issuer = "CN=Unknown CA",
          valid = TRUE
        )
      )
      
      expect_false(is_valid_certificate_chain(broken_chain))
    })
    
    # Test certificate subject validation
    test_that("Certificate subject validation", {
      valid_subjects <- c(
        "CN=atlaslabs.com",
        "CN=*.atlaslabs.com",
        "CN=app.atlaslabs.com, O=Atlas Labs, C=US"
      )
      
      for (subject in valid_subjects) {
        expect_true(is_valid_certificate_subject(subject, "atlaslabs.com"))
      }
      
      invalid_subjects <- c(
        "CN=evil.com",
        "CN=*.evil.com",
        ""
      )
      
      for (subject in invalid_subjects) {
        expect_false(is_valid_certificate_subject(subject, "atlaslabs.com"))
      }
    })
    
    # Test certificate key strength
    test_that("Certificate key strength validation", {
      # Strong keys
      strong_keys <- list(
        list(algorithm = "RSA", key_size = 2048),
        list(algorithm = "RSA", key_size = 4096),
        list(algorithm = "ECDSA", key_size = 256)
      )
      
      for (key in strong_keys) {
        expect_true(is_strong_key(key))
      }
      
      # Weak keys
      weak_keys <- list(
        list(algorithm = "RSA", key_size = 1024),
        list(algorithm = "DSA", key_size = 1024),
        list(algorithm = "RSA", key_size = 512)
      )
      
      for (key in weak_keys) {
        expect_false(is_strong_key(key))
      }
    })
    
    # Test certificate pinning
    test_that("Certificate pinning validation", {
      # Mock pinned certificates
      pinned_hashes <- c(
        "sha256/abc123def456...",
        "sha256/def456ghi789..."
      )
      
      current_hash <- "sha256/abc123def456..."
      expect_true(is_certificate_pinned(current_hash, pinned_hashes))
      
      # Unpinned certificate
      unpinned_hash <- "sha256/malicious123..."
      expect_false(is_certificate_pinned(unpinned_hash, pinned_hashes))
    })
  })
  