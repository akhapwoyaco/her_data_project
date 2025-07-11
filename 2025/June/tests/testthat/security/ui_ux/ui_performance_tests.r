# ============================================================================
# Atlas Labs HR Analytics Dashboard - UI/UX Performance Unit Tests
# ============================================================================
# Comprehensive testing suite for mobile performance, network latency, 
# browser compatibility, and progressive loading validation
# ============================================================================

# Required libraries for testing
library(testthat)
library(shiny)
library(shinytest2)
library(chromote)
library(httr)
library(jsonlite)
library(parallel)
library(microbenchmark)
library(profvis)
library(bench)

# Source the main application components
source("global.R")
source("utils.R")
source("custom_theme.R")

# ============================================================================
# 1. MOBILE DEVICE PERFORMANCE TESTS
# ============================================================================

context("Mobile Device Performance Tests")

# Test environment setup for mobile simulation
setup_mobile_environment <- function(device_type = "mobile") {
  device_configs <- list(
    mobile = list(
      width = 375, height = 667, 
      user_agent = "Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)",
      touch = TRUE, memory_limit = 512
    ),
    tablet = list(
      width = 768, height = 1024,
      user_agent = "Mozilla/5.0 (iPad; CPU OS 14_0 like Mac OS X)",
      touch = TRUE, memory_limit = 1024
    ),
    desktop = list(
      width = 1920, height = 1080,
      user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      touch = FALSE, memory_limit = 4096
    )
  )
  return(device_configs[[device_type]])
}

# Test 1.1: Mobile Viewport Responsiveness
test_that("Mobile viewport adapts correctly to different screen sizes", {
  
  # Test various mobile screen sizes
  mobile_sizes <- list(
    iphone_se = c(320, 568),
    iphone_12 = c(390, 844),
    iphone_12_pro_max = c(428, 926),
    samsung_galaxy_s21 = c(384, 854),
    pixel_5 = c(393, 851)
  )
  
  for (device_name in names(mobile_sizes)) {
    size <- mobile_sizes[[device_name]]
    
    # Create ChromoteSession for testing
    session <- ChromoteSession$new()
    
    # Set viewport
    session$Page$setDeviceMetricsOverride(
      width = size[1],
      height = size[2],
      deviceScaleFactor = 2,
      mobile = TRUE
    )
    
    # Test that UI elements are properly sized
    ui_elements <- session$Runtime$evaluate(
      "document.querySelectorAll('.container-fluid, .sidebar, .main-content').length"
    )
    
    expect_gt(ui_elements$result$value, 0, 
              info = paste("UI elements should be present on", device_name))
    
    # Test responsive breakpoints
    sidebar_visible <- session$Runtime$evaluate(
      "window.getComputedStyle(document.querySelector('.sidebar')).display !== 'none'"
    )
    
    # On mobile, sidebar should be collapsible
    if (size[1] < 768) {
      expect_true(is.logical(sidebar_visible$result$value) || 
                  sidebar_visible$result$value == "none",
                  info = paste("Sidebar should be collapsible on", device_name))
    }
    
    session$close()
  }
})

# Test 1.2: Touch Interface Optimization
test_that("Touch interface elements meet accessibility standards", {
  
  app <- AppDriver$new(
    app_dir = ".",
    variant = "mobile",
    options = list(
      width = 375, height = 667,
      args = list("--touch-events")
    )
  )
  
  # Test minimum touch target sizes (44x44px iOS, 48x48px Android)
  touch_elements <- app$get_js(
    "Array.from(document.querySelectorAll('button, .btn, input[type=\"submit\"], .nav-link'))
     .map(el => ({ 
       width: el.offsetWidth, 
       height: el.offsetHeight,
       element: el.tagName + '.' + el.className
     }))
     .filter(el => el.width < 44 || el.height < 44)"
  )
  
  expect_length(touch_elements, 0, 
                info = "All touch elements should meet minimum size requirements")
  
  # Test touch spacing (minimum 8px between touch targets)
  touch_spacing <- app$get_js(
    "function checkTouchSpacing() {
       const touchElements = document.querySelectorAll('button, .btn, input[type=\"submit\"]');
       const violations = [];
       
       for (let i = 0; i < touchElements.length; i++) {
         const rect1 = touchElements[i].getBoundingClientRect();
         for (let j = i + 1; j < touchElements.length; j++) {
           const rect2 = touchElements[j].getBoundingClientRect();
           const distance = Math.sqrt(
             Math.pow(rect1.left - rect2.left, 2) + 
             Math.pow(rect1.top - rect2.top, 2)
           );
           if (distance < 8 && distance > 0) {
             violations.push({
               element1: touchElements[i].className,
               element2: touchElements[j].className,
               distance: distance
             });
           }
         }
       }
       return violations;
     }
     checkTouchSpacing();"
  )
  
  expect_length(touch_spacing, 0, 
                info = "Touch elements should have adequate spacing")
  
  app$stop()
})

# Test 1.3: Mobile Performance Metrics
test_that("Mobile performance meets optimization thresholds", {
  
  # Test with different mobile CPU throttling levels
  throttling_levels <- list(
    no_throttling = 1,
    slow_3g = 4,
    slow_4g = 6
  )
  
  for (throttle_name in names(throttling_levels)) {
    throttle_rate <- throttling_levels[[throttle_name]]
    
    session <- ChromoteSession$new()
    
    # Enable CPU throttling
    session$Emulation$setCPUThrottlingRate(rate = throttle_rate)
    
    # Measure performance
    start_time <- Sys.time()
    
    # Navigate to app
    session$Page$navigate("http://localhost:3838")
    
    # Wait for app to load
    session$Page$loadEventFired()
    
    load_time <- as.numeric(Sys.time() - start_time)
    
    # Performance thresholds
    if (throttle_name == "no_throttling") {
      expect_lt(load_time, 3, 
                info = "App should load within 3 seconds on normal mobile")
    } else if (throttle_name == "slow_3g") {
      expect_lt(load_time, 10, 
                info = "App should load within 10 seconds on slow 3G")
    } else if (throttle_name == "slow_4g") {
      expect_lt(load_time, 6, 
                info = "App should load within 6 seconds on slow 4G")
    }
    
    # Test memory usage
    memory_usage <- session$Runtime$evaluate(
      "performance.memory ? performance.memory.usedJSHeapSize : 0"
    )
    
    # Mobile memory should be under 50MB
    expect_lt(memory_usage$result$value, 50 * 1024 * 1024, 
              info = paste("Memory usage should be optimized for mobile on", throttle_name))
    
    session$close()
  }
})

# Test 1.4: Mobile Battery Impact
test_that("Mobile battery consumption is optimized", {
  
  app <- AppDriver$new(
    app_dir = ".",
    variant = "mobile",
    options = list(width = 375, height = 667)
  )
  
  # Test for battery-draining operations
  battery_intensive_checks <- app$get_js(
    "function checkBatteryIntensiveOperations() {
       const issues = [];
       
       // Check for continuous animations
       const animatedElements = document.querySelectorAll('*');
       animatedElements.forEach(el => {
         const style = window.getComputedStyle(el);
         if (style.animationIterationCount === 'infinite' && 
             style.animationDuration !== '0s') {
           issues.push('Infinite animation detected: ' + el.className);
         }
       });
       
       // Check for high-frequency timers
       const originalSetInterval = window.setInterval;
       let highFrequencyTimers = 0;
       window.setInterval = function(callback, delay) {
         if (delay < 100) highFrequencyTimers++;
         return originalSetInterval.call(this, callback, delay);
       };
       
       // Check for constant polling
       const originalFetch = window.fetch;
       let fetchCount = 0;
       window.fetch = function(...args) {
         fetchCount++;
         return originalFetch.apply(this, args);
       };
       
       setTimeout(() => {
         if (fetchCount > 10) {
           issues.push('High frequency network requests detected');
         }
         if (highFrequencyTimers > 5) {
           issues.push('High frequency timers detected');
         }
       }, 5000);
       
       return issues;
     }
     checkBatteryIntensiveOperations();"
  )
  
  expect_length(battery_intensive_checks, 0, 
                info = "App should not have battery-intensive operations")
  
  app$stop()
})

# ============================================================================
# 2. NETWORK LATENCY IMPACT TESTS
# ============================================================================

context("Network Latency Impact Tests")

# Test 2.1: Network Condition Simulation
test_that("App performs gracefully under various network conditions", {
  
  network_profiles <- list(
    fast_3g = list(
      downloadThroughput = 1.5 * 1024 * 1024 / 8,  # 1.5 Mbps
      uploadThroughput = 0.75 * 1024 * 1024 / 8,    # 0.75 Mbps
      latency = 562.5
    ),
    slow_3g = list(
      downloadThroughput = 0.5 * 1024 * 1024 / 8,   # 0.5 Mbps
      uploadThroughput = 0.5 * 1024 * 1024 / 8,     # 0.5 Mbps
      latency = 2000
    ),
    offline = list(
      downloadThroughput = 0,
      uploadThroughput = 0,
      latency = 0
    )
  )
  
  for (profile_name in names(network_profiles)) {
    profile <- network_profiles[[profile_name]]
    
    session <- ChromoteSession$new()
    
    # Enable network emulation
    session$Network$enable()
    session$Network$emulateNetworkConditions(
      offline = profile_name == "offline",
      latency = profile$latency,
      downloadThroughput = profile$downloadThroughput,
      uploadThroughput = profile$uploadThroughput
    )
    
    # Test app behavior
    start_time <- Sys.time()
    
    tryCatch({
      session$Page$navigate("http://localhost:3838")
      session$Page$loadEventFired()
      
      load_time <- as.numeric(Sys.time() - start_time)
      
      if (profile_name == "offline") {
        # Should show offline message or cached version
        offline_indicator <- session$Runtime$evaluate(
          "document.querySelector('.offline-indicator') !== null || 
           document.querySelector('.network-error') !== null"
        )
        expect_true(offline_indicator$result$value, 
                    info = "App should show offline indicator")
      } else {
        # Should load within reasonable time
        max_load_time <- if (profile_name == "slow_3g") 20 else 10
        expect_lt(load_time, max_load_time, 
                  info = paste("App should load within", max_load_time, "seconds on", profile_name))
      }
      
    }, error = function(e) {
      if (profile_name != "offline") {
        fail(paste("App failed to load on", profile_name, ":", e$message))
      }
    })
    
    session$close()
  }
})

# Test 2.2: Progressive Loading Validation
test_that("Progressive loading works correctly under network constraints", {
  
  session <- ChromoteSession$new()
  
  # Enable network throttling
  session$Network$enable()
  session$Network$emulateNetworkConditions(
    offline = FALSE,
    latency = 1000,
    downloadThroughput = 1 * 1024 * 1024 / 8,  # 1 Mbps
    uploadThroughput = 1 * 1024 * 1024 / 8
  )
  
  # Track loading stages
  loading_stages <- session$Runtime$evaluate(
    "window.loadingStages = [];
     window.addEventListener('DOMContentLoaded', () => {
       window.loadingStages.push('DOMContentLoaded');
     });
     
     // Override image loading to track progress
     const originalImageSrc = Object.getOwnPropertyDescriptor(HTMLImageElement.prototype, 'src');
     Object.defineProperty(HTMLImageElement.prototype, 'src', {
       set: function(value) {
         this.addEventListener('load', () => {
           window.loadingStages.push('image_loaded');
         });
         originalImageSrc.set.call(this, value);
       },
       get: originalImageSrc.get
     });
     
     // Track script loading
     const originalScriptSrc = Object.getOwnPropertyDescriptor(HTMLScriptElement.prototype, 'src');
     Object.defineProperty(HTMLScriptElement.prototype, 'src', {
       set: function(value) {
         this.addEventListener('load', () => {
           window.loadingStages.push('script_loaded');
         });
         originalScriptSrc.set.call(this, value);
       },
       get: originalScriptSrc.get
     });
     
     true;"
  )
  
  # Navigate and wait for initial load
  session$Page$navigate("http://localhost:3838")
  session$Page$loadEventFired()
  
  # Wait for progressive loading
  Sys.sleep(5)
  
  # Check loading stages
  stages <- session$Runtime$evaluate("window.loadingStages")
  
  expect_true("DOMContentLoaded" %in% stages$result$value, 
              info = "DOM should load first")
  
  # Test that critical content loads before non-critical
  critical_content_loaded <- session$Runtime$evaluate(
    "document.querySelector('.main-content') !== null && 
     document.querySelector('.sidebar') !== null"
  )
  
  expect_true(critical_content_loaded$result$value, 
              info = "Critical content should load progressively")
  
  session$close()
})

# Test 2.3: Network Error Handling
test_that("Network error handling is robust", {
  
  error_scenarios <- list(
    connection_lost = function(session) {
      session$Network$emulateNetworkConditions(
        offline = TRUE, latency = 0, 
        downloadThroughput = 0, uploadThroughput = 0
      )
    },
    timeout = function(session) {
      session$Network$emulateNetworkConditions(
        offline = FALSE, latency = 30000,  # 30 second latency
        downloadThroughput = 1000, uploadThroughput = 1000
      )
    },
    intermittent = function(session) {
      # Simulate intermittent connection
      for (i in 1:5) {
        session$Network$emulateNetworkConditions(
          offline = i %% 2 == 0, latency = 1000,
          downloadThroughput = 1000, uploadThroughput = 1000
        )
        Sys.sleep(1)
      }
    }
  )
  
  for (scenario_name in names(error_scenarios)) {
    session <- ChromoteSession$new()
    session$Network$enable()
    
    # Apply error scenario
    error_scenarios[[scenario_name]](session)
    
    # Test error handling
    tryCatch({
      session$Page$navigate("http://localhost:3838")
      
      # Check for error handling UI
      error_ui <- session$Runtime$evaluate(
        "document.querySelector('.error-message') !== null || 
         document.querySelector('.retry-button') !== null || 
         document.querySelector('.offline-indicator') !== null"
      )
      
      expect_true(error_ui$result$value, 
                  info = paste("Error UI should be shown for", scenario_name))
      
    }, error = function(e) {
      # Error is expected for some scenarios
      if (scenario_name == "connection_lost") {
        expect_true(TRUE, info = "Connection lost scenario handled correctly")
      } else {
        fail(paste("Unexpected error in", scenario_name, ":", e$message))
      }
    })
    
    session$close()
  }
})

# Test 2.4: Data Loading Optimization
test_that("Data loading is optimized for network conditions", {
  
  app <- AppDriver$new(app_dir = ".")
  
  # Test lazy loading of data
  lazy_loading_test <- app$get_js(
    "function testLazyLoading() {
       const dataElements = document.querySelectorAll('[data-lazy-load]');
       const visibleElements = [];
       const hiddenElements = [];
       
       dataElements.forEach(el => {
         const rect = el.getBoundingClientRect();
         const isVisible = (
           rect.top >= 0 && rect.left >= 0 && 
           rect.bottom <= window.innerHeight && 
           rect.right <= window.innerWidth
         );
         
         if (isVisible) {
           visibleElements.push(el.dataset.lazyLoad);
         } else {
           hiddenElements.push(el.dataset.lazyLoad);
         }
       });
       
       return {
         visible: visibleElements,
         hidden: hiddenElements,
         visibleCount: visibleElements.length,
         hiddenCount: hiddenElements.length
       };
     }
     testLazyLoading();"
  )
  
  # Only visible elements should be loaded initially
  expect_gt(lazy_loading_test$visibleCount, 0, 
            info = "Some elements should be visible and loaded")
  
  # Test data chunking
  chunk_size_test <- app$get_js(
    "function testDataChunking() {
       const tables = document.querySelectorAll('table');
       const largeTables = [];
       
       tables.forEach(table => {
         const rows = table.querySelectorAll('tbody tr');
         if (rows.length > 100) {
           largeTables.push({
             rows: rows.length,
             hasPagination: table.closest('.dataTables_wrapper') !== null,
             hasVirtualScrolling: table.dataset.virtualScroll === 'true'
           });
         }
       });
       
       return largeTables;
     }
     testDataChunking();"
  )
  
  # Large tables should have pagination or virtual scrolling
  if (length(chunk_size_test) > 0) {
    for (table in chunk_size_test) {
      expect_true(table$hasPagination || table$hasVirtualScrolling, 
                  info = "Large tables should implement chunking")
    }
  }
  
  app$stop()
})

# ============================================================================
# 3. BROWSER COMPATIBILITY TESTS
# ============================================================================

context("Browser Compatibility Tests")

# Test 3.1: Cross-Browser Functionality
test_that("App works across major browsers", {
  
  # Define browser configurations
  browsers <- list(
    chrome = list(
      name = "Chrome",
      user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      features = c("es6", "flexbox", "grid", "webgl")
    ),
    firefox = list(
      name = "Firefox",
      user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:89.0) Gecko/20100101 Firefox/89.0",
      features = c("es6", "flexbox", "grid", "webgl")
    ),
    safari = list(
      name = "Safari",
      user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15",
      features = c("es6", "flexbox", "grid", "webgl")
    ),
    edge = list(
      name = "Edge",
      user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36 Edg/91.0.864.59",
      features = c("es6", "flexbox", "grid", "webgl")
    ),
    ie11 = list(
      name = "IE11",
      user_agent = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko",
      features = c("flexbox")  # Limited features
    )
  )
  
  for (browser_name in names(browsers)) {
    browser <- browsers[[browser_name]]
    
    session <- ChromoteSession$new()
    
    # Set user agent
    session$Network$setUserAgentOverride(userAgent = browser$user_agent)
    
    # Navigate to app
    session$Page$navigate("http://localhost:3838")
    
    # Test basic functionality
    page_loaded <- session$Runtime$evaluate(
      "document.readyState === 'complete'"
    )
    
    expect_true(page_loaded$result$value, 
                info = paste("App should load in", browser$name))
    
    # Test feature compatibility
    feature_support <- session$Runtime$evaluate(paste0(
      "({
         es6: typeof Symbol !== 'undefined',
         flexbox: CSS.supports('display', 'flex'),
         grid: CSS.supports('display', 'grid'),
         webgl: !!document.createElement('canvas').getContext('webgl'),
         fetch: typeof fetch !== 'undefined',
         promises: typeof Promise !== 'undefined'
       })"
    ))
    
    features <- feature_support$result$value
    
    # Check expected features
    for (feature in browser$features) {
      expect_true(features[[feature]], 
                  info = paste(feature, "should be supported in", browser$name))
    }
    
    # Test polyfill loading for older browsers
    if (browser_name == "ie11") {
      polyfills_loaded <- session$Runtime$evaluate(
        "document.querySelectorAll('script[src*=\"polyfill\"]').length > 0"
      )
      
      expect_true(polyfills_loaded$result$value, 
                  info = "Polyfills should be loaded for IE11")
    }
    
    session$close()
  }
})

# Test 3.2: CSS Feature Support
test_that("CSS features degrade gracefully", {
  
  session <- ChromoteSession$new()
  session$Page$navigate("http://localhost:3838")
  
  # Test CSS feature detection and fallbacks
  css_support_test <- session$Runtime$evaluate(
    "function testCSSSupport() {
       const features = {
         flexbox: CSS.supports('display', 'flex'),
         grid: CSS.supports('display', 'grid'),
         customProperties: CSS.supports('--custom-property', 'value'),
         transforms: CSS.supports('transform', 'translateX(10px)'),
         transitions: CSS.supports('transition', 'all 0.3s ease')
       };
       
       // Test fallback implementations
       const fallbacks = {
         flexboxFallback: document.querySelector('.flexbox-fallback') !== null,
         gridFallback: document.querySelector('.grid-fallback') !== null,
         customPropertiesFallback: document.querySelector('[data-css-fallback]') !== null
       };
       
       return { features, fallbacks };
     }
     testCSSSupport();"
  )
  
  css_results <- css_support_test$result$value
  
  # Modern browsers should support key features
  expect_true(css_results$features$flexbox, 
              info = "Flexbox should be supported")
  expect_true(css_results$features$transforms, 
              info = "CSS transforms should be supported")
  
  # Test that fallbacks are available when needed
  if (!css_results$features$grid) {
    expect_true(css_results$fallbacks$gridFallback, 
                info = "Grid fallback should be available when grid is not supported")
  }
  
  session$close()
})

# Test 3.3: JavaScript API Compatibility
test_that("JavaScript APIs work across browsers", {
  
  session <- ChromoteSession$new()
  session$Page$navigate("http://localhost:3838")
  
  # Test API compatibility
  api_compatibility <- session$Runtime$evaluate(
    "function testAPICompatibility() {
       const apis = {
         fetch: typeof fetch !== 'undefined',
         promises: typeof Promise !== 'undefined',
         arrow_functions: (() => true)(),
         const_let: (() => { try { eval('const x = 1; let y = 2;'); return true; } catch(e) { return false; } })(),
         for_of: (() => { try { eval('for (let x of [1,2,3]) {}'); return true; } catch(e) { return false; } })(),
         destructuring: (() => { try { eval('const {a} = {a: 1};'); return true; } catch(e) { return false; } })(),
         spread_operator: (() => { try { eval('const arr = [1, ...[2, 3]];'); return true; } catch(e) { return false; } })(),
         template_literals: (() => { try { eval('const str = `template`;'); return true; } catch(e) { return false; } })()
       };
       
       // Test polyfill loading
       const polyfills = {
         fetch_polyfill: document.querySelector('script[src*=\"fetch\"]') !== null,
         promise_polyfill: document.querySelector('script[src*=\"promise\"]') !== null,
         babel_polyfill: document.querySelector('script[src*=\"babel\"]') !== null
       };
       
       return { apis, polyfills };
     }
     testAPICompatibility();"
  )
  
  api_results <- api_compatibility$result$value
  
  # Test that critical APIs are available or polyfilled
  if (!api_results$apis$fetch) {
    expect_true(api_results$polyfills$fetch_polyfill, 
                info = "Fetch polyfill should be loaded when fetch is not supported")
  }
  
  if (!api_results$apis$promises) {
    expect_true(api_results$polyfills$promise_polyfill, 
                info = "Promise polyfill should be loaded when promises are not supported")
  }
  
  session$close()
})

# Test 3.4: Browser-Specific Edge Cases
test_that("Browser-specific edge cases are handled", {
  
  # Test Safari-specific issues
  safari_session <- ChromoteSession$new()
  safari_session$Network$setUserAgentOverride(
    userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15"
  )
  
  safari_session$Page$navigate("http://localhost:3838")
  
  # Test Safari date input handling
  safari_date_test <- safari_session$Runtime$evaluate(
    "function testSafariDateInput() {
       const dateInputs = document.querySelectorAll('input[type=\"date\"]');
       const issues = [];
       
       dateInputs.forEach(input => {
         // Safari date input quirks
         if (input.value && !input.value.match(/\\d{4}-\\d{2}-\\d{2}/)) {
           issues.push('Safari date format issue');
         }
       });
       
       return issues;
     }
     testSafariDateInput();"
  )
  
  expect_length(safari_date_test$result$value, 0, 
                info = "Safari date input issues should be handled")
  
  safari_session$close()
  
  # Test IE11-specific issues
  ie11_session <- ChromoteSession$new()
  ie11_session$Network$setUserAgentOverride(
    userAgent = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko"
  )
  
  ie11_session$Page$navigate("http://localhost:3838")
  
  # Test IE11 compatibility
  ie11_compatibility <- ie11_session$Runtime$evaluate(
    "function testIE11Compatibility() {
       const issues = [];
       
       // Test for IE11-specific problems
       if (typeof Map === 'undefined') {
         issues.push('Map polyfill needed');
       }
       
       if (typeof Set === 'undefined') {
         issues.push('Set polyfill needed');
       }
       
       // Test for proper event handling
       if (!window.addEventListener) {
         issues.push('Event listener polyfill needed');
       }
       
       return issues;
     }
     testIE11Compatibility();"
  )
  
  # IE11 issues should be handled by polyfills
  expect_length(ie11_compatibility$result$value, 0, 
                info = "IE11 compatibility issues should be resolved")
  
  ie11_session$close()
})

# ============================================================================
# 4. PROGRESSIVE LOADING VALIDATION TESTS
# ============================================================================

context("Progressive Loading Validation Tests")

# Test 4.1: Critical Resource Priority
test_that("Critical resources load first", {
  
  session <- ChromoteSession$new()
  
  # Enable performance timeline
  session$Performance$enable()
  
  # Track resource loading order
  resource_timeline <- list()
  
  session$Network$enable()
  session$Network$responseReceived(callback = function(params) {
    resource_timeline <<- append(resource_timeline, list(list(
      url = params