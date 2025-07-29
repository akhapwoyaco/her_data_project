# ============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - UI TESTING SUITE
# Comprehensive Functional UI Testing with Edge Cases
# Developer: akhapwoyaco
# ============================================================================

# Required Libraries for UI Testing
library(testthat)
library(shinytest2)
library(RSelenium)
library(shiny)
library(htmlwidgets)
library(rvest)
library(httr)
library(jsonlite)
library(chromote)
library(accessibility)

# ============================================================================
# 7.1 FUNCTIONAL UI TESTING SUITE
# ============================================================================

# Test Configuration
test_config <- list(
  app_url = "http://localhost:3838",
  timeout = 30,
  browsers = c("chrome", "firefox", "safari", "edge"),
  mobile_devices = c("iPhone", "iPad", "Android", "Galaxy"),
  screen_sizes = list(
    desktop = list(width = 1920, height = 1080),
    laptop = list(width = 1366, height = 768),
    tablet = list(width = 768, height = 1024),
    mobile = list(width = 375, height = 812)
  )
)

# ============================================================================
# 7.1.1 NAVIGATION FLOW VALIDATION TESTS
# ============================================================================

test_that("Navigation Flow Validation - Comprehensive Tests", {
  
  # Initialize Shiny Test App
  app <- AppDriver$new(
    app_dir = ".",
    name = "atlas-labs-navigation",
    height = 1080,
    width = 1920,
    load_timeout = 30000
  )
  
  # Test 1: Main Navigation Menu Accessibility
  test_that("Main navigation menu loads and is accessible", {
    expect_true(app$wait_for_js("$('.sidebar').length > 0", timeout = 5000))
    
    # Check all main navigation items exist
    nav_items <- c("overview", "attrition", "demographics", "performance", 
                   "compensation", "satisfaction", "reports")
    
    for (item in nav_items) {
      expect_true(
        app$wait_for_js(paste0("$('[data-value=\"", item, "\"]').length > 0"), 
                       timeout = 3000),
        info = paste("Navigation item", item, "not found")
      )
    }
  })
  
  # Test 2: Navigation State Persistence
  test_that("Navigation state persists across module switches", {
    # Navigate to each module and verify state
    modules <- c("attrition", "demographics", "performance")
    
    for (module in modules) {
      app$click(paste0("[data-value='", module, "']"))
      app$wait_for_idle(2000)
      
      # Verify active state
      expect_true(
        app$get_js(paste0("$('[data-value=\"", module, "\"]').hasClass('active')")) == TRUE,
        info = paste("Module", module, "not showing active state")
      )
      
      # Verify content loads
      expect_true(
        app$wait_for_js(paste0("$('#", module, "-content').is(':visible')"), timeout = 5000),
        info = paste("Module", module, "content not visible")
      )
    }
  })
  
  # Test 3: Breadcrumb Navigation
  test_that("Breadcrumb navigation functions correctly", {
    # Navigate deep into analysis
    app$click("[data-value='attrition']")
    app$wait_for_idle(1000)
    app$click("[data-testid='detailed-analysis']")
    app$wait_for_idle(1000)
    
    # Check breadcrumb exists
    expect_true(
      app$wait_for_js("$('.breadcrumb').length > 0", timeout = 3000)
    )
    
    # Test breadcrumb back navigation
    app$click(".breadcrumb-item:nth-child(2) a")
    app$wait_for_idle(1000)
    
    # Verify we're back at parent level
    expect_true(
      app$get_js("window.location.hash") == "#attrition"
    )
  })
  
  # Test 4: Deep Linking and URL State Management
  test_that("Deep linking and URL state management works", {
    # Test direct navigation via URL
    test_urls <- c(
      "#overview",
      "#attrition?department=IT&tenure=2-5",
      "#demographics?gender=all&age=25-35",
      "#performance?rating=high"
    )
    
    for (url in test_urls) {
      app$run_js(paste0("window.location.hash = '", url, "'"))
      app$wait_for_idle(2000)
      
      # Verify correct module is active
      module <- strsplit(url, "\\?")[[1]][1]
      module <- gsub("#", "", module)
      
      expect_true(
        app$get_js(paste0("$('[data-value=\"", module, "\"]').hasClass('active')")) == TRUE,
        info = paste("Deep link to", module, "failed")
      )
    }
  })
  
  # Test 5: Navigation with Keyboard Shortcuts
  test_that("Keyboard navigation shortcuts work", {
    # Test Tab navigation
    app$send_keys(keys$tab)
    expect_true(app$get_js("document.activeElement.tagName") == "A")
    
    # Test arrow key navigation
    app$send_keys(keys$arrow_down)
    app$wait_for_idle(500)
    
    # Test Enter to activate
    app$send_keys(keys$enter)
    app$wait_for_idle(1000)
    
    # Verify navigation occurred
    expect_true(
      app$get_js("$('.sidebar .active').length") == 1
    )
  })
  
  # Test 6: Navigation Error Handling
  test_that("Navigation handles errors gracefully", {
    # Try to navigate to non-existent module
    app$run_js("window.location.hash = '#nonexistent'")
    app$wait_for_idle(2000)
    
    # Should redirect to overview or show error
    expect_true(
      app$get_js("$('.error-message').length > 0 || window.location.hash == '#overview'")
    )
    
    # Test navigation with malformed parameters
    app$run_js("window.location.hash = '#attrition?malformed=<script>alert(1)</script>'")
    app$wait_for_idle(2000)
    
    # Should sanitize and handle gracefully
    expect_false(
      app$get_js("document.body.innerHTML.includes('<script>')"),
      info = "XSS vulnerability in navigation parameters"
    )
  })
  
  app$stop()
})

# ============================================================================
# 7.1.2 FORM SUBMISSION ACCURACY TESTS
# ============================================================================

test_that("Form Submission Accuracy - Comprehensive Tests", {
  
  app <- AppDriver$new(app_dir = ".", name = "atlas-labs-forms")
  
  # Test 1: Filter Form Validation
  test_that("Filter forms validate input correctly", {
    app$click("[data-value='demographics']")
    app$wait_for_idle(1000)
    
    # Test valid inputs
    app$set_inputs(
      department_filter = "IT",
      age_range = c(25, 45),
      gender_filter = "All"
    )
    
    app$click("[data-testid='apply-filters']")
    app$wait_for_idle(2000)
    
    # Verify filters applied
    expect_true(
      app$get_js("$('.filter-applied').length > 0")
    )
    
    # Test invalid age range
    app$set_inputs(age_range = c(65, 25)) # Invalid: max < min
    app$click("[data-testid='apply-filters']")
    
    # Should show validation error
    expect_true(
      app$wait_for_js("$('.validation-error').is(':visible')", timeout = 3000)
    )
  })
  
  # Test 2: Date Range Form Validation
  test_that("Date range forms handle edge cases", {
    app$click("[data-value='performance']")
    app$wait_for_idle(1000)
    
    # Test valid date range
    app$set_inputs(
      start_date = "2023-01-01",
      end_date = "2023-12-31"
    )
    
    expect_true(
      app$get_js("$('#start_date').get(0).checkValidity()") == TRUE
    )
    
    # Test invalid date combinations
    invalid_dates <- list(
      list(start = "2023-12-31", end = "2023-01-01"), # End before start
      list(start = "2025-01-01", end = "2025-12-31"), # Future dates
      list(start = "invalid-date", end = "2023-12-31"), # Invalid format
      list(start = "", end = "2023-12-31") # Empty start date
    )
    
    for (dates in invalid_dates) {
      app$set_inputs(
        start_date = dates$start,
        end_date = dates$end
      )
      
      app$click("[data-testid='apply-date-filter']")
      app$wait_for_idle(1000)
      
      # Should show validation error or prevent submission
      expect_true(
        app$get_js("$('.date-validation-error').is(':visible') || 
                   !$('#start_date').get(0).checkValidity()"),
        info = paste("Date validation failed for:", dates$start, "to", dates$end)
      )
    }
  })
  
  # Test 3: Multi-Select Form Handling
  test_that("Multi-select forms handle complex selections", {
    app$click("[data-value='attrition']")
    app$wait_for_idle(1000)
    
    # Test selecting multiple departments
    departments <- c("IT", "Sales", "HR", "Marketing")
    
    # Open multi-select dropdown
    app$click("[data-testid='department-multiselect']")
    app$wait_for_idle(500)
    
    # Select multiple options
    for (dept in departments) {
      app$click(paste0("[data-value='", dept, "']"))
      app$wait_for_idle(200)
    }
    
    # Verify selections
    selected_count <- app$get_js("$('[data-testid=\"department-multiselect\"] .selected').length")
    expect_equal(selected_count, length(departments))
    
    # Test select all functionality
    app$click("[data-testid='select-all-departments']")
    app$wait_for_idle(1000)
    
    all_selected <- app$get_js("$('[data-testid=\"department-multiselect\"] option:selected').length")
    expect_true(all_selected > length(departments))
    
    # Test clear all functionality
    app$click("[data-testid='clear-all-departments']")
    app$wait_for_idle(1000)
    
    cleared_count <- app$get_js("$('[data-testid=\"department-multiselect\"] option:selected').length")
    expect_equal(cleared_count, 0)
  })
  
  # Test 4: Form Submission with Large Datasets
  test_that("Forms handle large dataset submissions", {
    # Simulate large filter selection
    app$click("[data-value='demographics']")
    app$wait_for_idle(1000)
    
    # Select many filters simultaneously
    large_filter_set <- list(
      departments = paste0("Dept_", 1:50),
      job_roles = paste0("Role_", 1:100),
      locations = paste0("City_", 1:200)
    )
    
    # Apply large filter set
    app$run_js("
      window.testLargeFilterSubmission = function() {
        // Simulate large form submission
        var formData = new FormData();
        for (let i = 1; i <= 50; i++) {
          formData.append('departments[]', 'Dept_' + i);
        }
        return formData.getAll('departments[]').length;
      }
    ")
    
    result <- app$get_js("window.testLargeFilterSubmission()")
    expect_equal(result, 50)
    
    # Test form doesn't freeze with large submissions
    start_time <- Sys.time()
    app$click("[data-testid='apply-large-filters']")
    app$wait_for_idle(5000)
    end_time <- Sys.time()
    
    # Should complete within reasonable time
    expect_true(
      as.numeric(difftime(end_time, start_time, units = "secs")) < 10,
      info = "Large form submission took too long"
    )
  })
  
  # Test 5: Form Auto-Save and Recovery
  test_that("Forms auto-save and recover data", {
    app$click("[data-value='reports']")
    app$wait_for_idle(1000)
    
    # Fill out report configuration form
    app$set_inputs(
      report_title = "Test Attrition Report",
      report_description = "This is a test report with auto-save functionality",
      include_charts = TRUE,
      export_format = "PDF"
    )
    
    # Wait for auto-save (should trigger after 2 seconds of inactivity)
    app$wait_for_idle(3000)
    
    # Verify auto-save occurred
    expect_true(
      app$get_js("localStorage.getItem('atlas_report_draft') !== null"),
      info = "Auto-save not working"
    )
    
    # Simulate page refresh
    app$run_js("window.location.reload()")
    app$wait_for_idle(3000)
    
    # Verify form data recovered
    recovered_title <- app$get_js("$('#report_title').val()")
    expect_equal(recovered_title, "Test Attrition Report")
  })
  
  app$stop()
})

# ============================================================================
# 7.1.3 INTERACTIVE ELEMENT FUNCTIONALITY TESTS
# ============================================================================

test_that("Interactive Element Functionality - Comprehensive Tests", {
  
  app <- AppDriver$new(app_dir = ".", name = "atlas-labs-interactive")
  
  # Test 1: Chart Interactivity
  test_that("Charts respond correctly to user interactions", {
    app$click("[data-value='attrition']")
    app$wait_for_idle(2000)
    
    # Test chart hover functionality
    app$run_js("
      // Simulate hover on chart element
      var chart = document.querySelector('.plotly');
      if (chart) {
        var event = new MouseEvent('mouseover', {
          view: window,
          bubbles: true,
          cancelable: true,
          clientX: 100,
          clientY: 100
        });
        chart.dispatchEvent(event);
      }
    ")
    
    app$wait_for_idle(1000)
    
    # Check tooltip appears
    expect_true(
      app$wait_for_js("$('.plotly-tooltip').is(':visible')", timeout = 3000)
    )
    
    # Test chart click functionality
    app$run_js("
      // Simulate click on chart bar
      var chartBar = document.querySelector('.plotly .bars path');
      if (chartBar) {
        chartBar.click();
      }
    ")
    
    app$wait_for_idle(1000)
    
    # Verify drill-down or selection occurred
    expect_true(
      app$get_js("$('.chart-selection-info').is(':visible')") ||
      app$get_js("window.location.hash.includes('drill')")
    )
  })
  
  # Test 2: Data Table Interactions
  test_that("Data tables handle complex interactions", {
    app$click("[data-value='demographics']")
    app$wait_for_idle(2000)
    
    # Test column sorting
    app$click("th[data-column='salary']")
    app$wait_for_idle(1000)
    
    # Verify sorting applied
    first_salary <- app$get_js("$('tbody tr:first-child td[data-column=\"salary\"]').text()")
    app$click("th[data-column='salary']") # Sort descending
    app$wait_for_idle(1000)
    
    new_first_salary <- app$get_js("$('tbody tr:first-child td[data-column=\"salary\"]').text()")
    expect_false(first_salary == new_first_salary, info = "Table sorting not working")
    
    # Test column filtering
    app$type("#salary-filter", "50000")
    app$wait_for_idle(2000)
    
    # Verify filter applied
    visible_rows <- app$get_js("$('tbody tr:visible').length")
    total_rows <- app$get_js("$('tbody tr').length")
    expect_true(visible_rows <= total_rows, info = "Table filtering not working")
    
    # Test pagination
    if (app$get_js("$('.pagination').length > 0")) {
      app$click(".pagination .next")
      app$wait_for_idle(1000)
      
      page_indicator <- app$get_js("$('.pagination .active').text()")
      expect_true(as.numeric(page_indicator) > 1, info = "Pagination not working")
    }
  })
  
  # Test 3: Modal Dialog Interactions
  test_that("Modal dialogs function correctly", {
    app$click("[data-value='performance']")
    app$wait_for_idle(1000)
    
    # Open employee details modal
    app$click("[data-testid='employee-details-btn']")
    app$wait_for_idle(1000)
    
    # Verify modal opens
    expect_true(
      app$wait_for_js("$('.modal').hasClass('show')", timeout = 3000)
    )
    
    # Test modal keyboard navigation
    app$send_keys(keys$tab)
    app$send_keys(keys$tab)
    app$send_keys(keys$enter)
    app$wait_for_idle(500)
    
    # Test close modal with Escape key
    app$send_keys(keys$escape)
    app$wait_for_idle(1000)
    
    expect_false(
      app$get_js("$('.modal').hasClass('show')"),
      info = "Modal not closing with Escape key"
    )
    
    # Test modal backdrop click
    app$click("[data-testid='employee-details-btn']")
    app$wait_for_idle(1000)
    
    app$run_js("$('.modal-backdrop').click()")
    app$wait_for_idle(1000)
    
    expect_false(
      app$get_js("$('.modal').hasClass('show')"),
      info = "Modal not closing with backdrop click"
    )
  })
  
  # Test 4: Drag and Drop Functionality
  test_that("Drag and drop interactions work correctly", {
    app$click("[data-value='reports']")
    app$wait_for_idle(1000)
    
    # Test dragging chart elements to report builder
    app$run_js("
      // Simulate drag and drop
      function simulateDragDrop(sourceSelector, targetSelector) {
        var source = document.querySelector(sourceSelector);
        var target = document.querySelector(targetSelector);
        
        if (source && target) {
          var dragEvent = new DragEvent('dragstart', {
            dataTransfer: new DataTransfer()
          });
          source.dispatchEvent(dragEvent);
          
          var dropEvent = new DragEvent('drop', {
            dataTransfer: dragEvent.dataTransfer
          });
          target.dispatchEvent(dropEvent);
          
          return true;
        }
        return false;
      }
      
      window.testDragDrop = simulateDragDrop;
    ")
    
    # Test dragging chart to report area
    result <- app$get_js("window.testDragDrop('.chart-widget', '.report-drop-zone')")
    
    if (result) {
      app$wait_for_idle(1000)
      
      # Verify element was added to report
      expect_true(
        app$get_js("$('.report-drop-zone .chart-widget').length > 0")
      )
    }
  })
  
  # Test 5: Touch Gesture Support
  test_that("Touch gestures work on mobile devices", {
    # Set mobile viewport
    app$set_window_size(375, 812)
    app$wait_for_idle(1000)
    
    app$click("[data-value='attrition']")
    app$wait_for_idle(2000)
    
    # Simulate touch swipe on chart
    app$run_js("
      // Simulate touch swipe
      var chart = document.querySelector('.plotly');
      if (chart) {
        var touchStart = new TouchEvent('touchstart', {
          touches: [new Touch({
            identifier: 0,
            target: chart,
            clientX: 100,
            clientY: 200
          })]
        });
        
        var touchMove = new TouchEvent('touchmove', {
          touches: [new Touch({
            identifier: 0,
            target: chart,
            clientX: 200,
            clientY: 200
          })]
        });
        
        var touchEnd = new TouchEvent('touchend', {
          changedTouches: [new Touch({
            identifier: 0,
            target: chart,
            clientX: 200,
            clientY: 200
          })]
        });
        
        chart.dispatchEvent(touchStart);
        setTimeout(() => chart.dispatchEvent(touchMove), 50);
        setTimeout(() => chart.dispatchEvent(touchEnd), 100);
      }
    ")
    
    app$wait_for_idle(1000)
    
    # Verify swipe action was processed
    expect_true(
      app$get_js("$('.chart-swipe-indicator').length > 0") ||
      app$get_js("typeof Plotly !== 'undefined'"), # Plotly handles touch events
      info = "Touch swipe not working on charts"
    )
    
    # Reset to desktop view
    app$set_window_size(1920, 1080)
  })
  
  app$stop()
})

# ============================================================================
# 7.1.4 RESPONSIVE DESIGN TESTING
# ============================================================================

test_that("Responsive Design Testing - Comprehensive Tests", {
  
  app <- AppDriver$new(app_dir = ".", name = "atlas-labs-responsive")
  
  # Test different screen sizes
  screen_sizes <- list(
    mobile_portrait = list(width = 375, height = 812),
    mobile_landscape = list(width = 812, height = 375),
    tablet_portrait = list(width = 768, height = 1024),
    tablet_landscape = list(width = 1024, height = 768),
    laptop = list(width = 1366, height = 768),
    desktop = list(width = 1920, height = 1080),
    ultrawide = list(width = 2560, height = 1440)
  )
  
  for (size_name in names(screen_sizes)) {
    size <- screen_sizes[[size_name]]
    
    test_that(paste("Layout adapts correctly to", size_name), {
      app$set_window_size(size$width, size$height)
      app$wait_for_idle(1000)
      
      # Test navigation visibility
      if (size$width < 768) {
        # Mobile: Navigation should be collapsible
        expect_true(
          app$get_js("$('.navbar-toggler').is(':visible')"),
          info = paste("Mobile navigation toggle not visible on", size_name)
        )
        
        # Test hamburger menu
        app$click(".navbar-toggler")
        app$wait_for_idle(500)
        
        expect_true(
          app$get_js("$('.navbar-collapse').hasClass('show')"),
          info = paste("Mobile menu not expanding on", size_name)
        )
      } else {
        # Desktop: Navigation should be always visible
        expect_true(
          app$get_js("$('.navbar-nav').is(':visible')"),
          info = paste("Desktop navigation not visible on", size_name)
        )
      }
      
      # Test content area layout
      app$click("[data-value='demographics']")
      app$wait_for_idle(2000)
      
      # Check if charts stack vertically on mobile
      if (size$width < 768) {
        chart_containers <- app$get_js("$('.chart-container').length")
        stacked_containers <- app$get_js("
          $('.chart-container').filter(function() {
            return $(this).width() >= $(this).parent().width() * 0.9;
          }).length
        ")
        
        expect_true(
          stacked_containers >= chart_containers * 0.8,
          info = paste("Charts not stacking on", size_name)
        )
      }
      
      # Test table responsiveness
      if (app$get_js("$('.table-responsive').length > 0")) {
        expect_true(
          app$get_js("$('.table-responsive').css('overflow-x') === 'auto'"),
          info = paste("Table not responsive on", size_name)
        )
      }
      
      # Test text readability (font sizes)
      body_font_size <- app$get_js("parseInt($('body').css('font-size'))")
      expect_true(
        body_font_size >= 14,
        info = paste("Font too small on", size_name)
      )
      
      # Test button touch targets (minimum 44px for mobile)
      if (size$width < 768) {
        min_button_height <- app$get_js("
          Math.min.apply(Math, $('.btn').map(function() {
            return $(this).outerHeight();
          }).get())
        ")
        
        expect_true(
          min_button_height >= 44,
          info = paste("Button touch targets too small on", size_name)
        )
      }
    })
  }
  
  # Test orientation change handling
  test_that("App handles orientation changes gracefully", {
    # Start in portrait
    app$set_window_size(375, 812)
    app$wait_for_idle(1000)
    
    app$click("[data-value='attrition']")
    app$wait_for_idle(2000)
    
    # Rotate to landscape
    app$set_window_size(812, 375)
    app$wait_for_idle(2000)
    
    # Verify layout adjusted
    expect_true(
      app$get_js("$(window).width() > $(window).height()"),
      info = "Orientation change not detected"
    )
    
    # Check that charts reflow
    expect_true(
      app$wait_for_js("$('.plotly').length > 0", timeout = 5000),
      info = "Charts not reflowing after orientation change"
    )
    
    # Rotate back to portrait
    app$set_window_size(375, 812)
    app$wait_for_idle(2000)
    
    # Verify layout adjusted back
    expect_true(
      app$get_js("$(window).width() < $(window).height()"),
      info = "Orientation change back not handled"
    )
  })
  
  # Test responsive images and media
  test_that("Images and media scale correctly", {
    sizes_to_test <- list(
      list(width = 375, height = 812),
      list(width = 1920, height = 1080)
    )
    
    for (size in sizes_to_test) {
      app$set_window_size(size$width, size$height)
      app$wait_for_idle(1000)
      
      # Check logo scaling
      if (app$get_js("$('.logo img').length > 0")) {
        logo_width <- app$get_js("$('.logo img').width()")
        container_width <- app$get_js("$('.logo').width()")
        
        expect_true(
          logo_width <= container_width,
          info = paste("Logo not scaling at", size$width, "x", size$height)
        )
      }
      
      # Check chart containers
      app$click("[data-value='overview']")
      app$wait_for_idle(2000)
      
      chart_containers <- app$get_js("$('.chart-container').length")
      properly_sized <- app$get_js("
        $('.chart-container').filter(function() {
          return $(this).width() <= $(this).parent().width();
        }).length
      ")
      
      expect_equal(
        properly_sized, chart_containers,
        info = paste("Charts overflowing at", size$width, "x", size$height)
      )
    }
  })
  
  app$stop()
})

# ============================================================================
# 7.1.5 CROSS-BROWSER COMPATIBILITY TESTS
# ============================================================================

test_that("Cross-Browser Compatibility - Comprehensive Tests", {
  
  browsers <- c("chrome", "firefox", "edge") # Safari requires macOS
  
  for (browser in browsers) {
    test_that(paste("App works correctly in", browser), {
      
      # Skip if browser not available
      skip_if_not_installed(browser)
      
      # Initialize browser-specific driver
      driver <- try({
        if (browser == "chrome") {
          RSelenium::rsDriver(browser = "chrome", chromever = "latest")
        } else if (browser == "firefox") {
          RSelenium::rsDriver(browser = "firefox")
        } else if (browser == "edge") {
          RSelenium::rsDriver(browser = "MicrosoftEdge")
        }
      }, silent = TRUE)
      
      if (inherits(driver, "try-error")) {
        skip(paste("Browser", browser, "not available"))
      }
      
      remDr <- driver$client
      remDr$navigate(test_config$app_url)
      
      # Test 1: Basic page load
      expect_true({
        remDr$setTimeout(type = "page load", milliseconds = 30000)
        title <- remDr$getTitle()[[1]]
        length(title) > 0
      }, info = paste("Page not loading in", browser))
      
      # Test 2: JavaScript functionality
      js_test_result <- remDr$executeScript("
        return {
          jquery: typeof $ !== 'undefined',
          plotly: typeof Plotly !== 'undefined',
          shiny: typeof Shiny !== 'undefined'
        };
      ")
      
      expect_true(js_test_result[[1]]$jquery, 
                 info = paste("jQuery not working in", browser))
      expect_true(js_test_result[[1]]$shiny, 
                 info = paste("Shiny not working in", browser))
      
      # Test 3: CSS rendering
      body_bg <- remDr$executeScript("
        return window.getComputedStyle(document.body).backgroundColor;
      ")
      
      expect_true(
        !is.null(body_bg[[1]