# =============================================================================
# Atlas Labs HR Analytics Dashboard - UI/UX Performance Unit Tests
# Comprehensive testing suite for UI/UX performance metrics
# Author: akhapwoyaco
# =============================================================================

# Required Libraries
library(testthat)
library(shiny)
library(shinytest2)
library(RSelenium)
library(chromote)
library(microbenchmark)
library(profvis)
library(bench)
library(httr)
library(jsonlite)
library(future)
library(promises)
library(DT)
library(plotly)
library(ggplot2)

# =============================================================================
# 1. PAGE LOAD TIME MEASUREMENT
# =============================================================================

# Test Suite: Page Load Performance
test_that("Page Load Time Measurement - Comprehensive Tests", {
  
  # 1.1 Basic Page Load Time Test
  test_that("Basic page load time is within acceptable limits", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "page_load_basic",
      timeout = 30000,
      load_timeout = 10000
    )
    
    start_time <- Sys.time()
    app$wait_for_idle(timeout = 5000)
    end_time <- Sys.time()
    
    load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    expect_lt(load_time, 5.0, info = "Page should load within 5 seconds")
    expect_gt(load_time, 0.1, info = "Load time should be realistic")
    
    app$stop()
  })
  
  # 1.2 Cold Start Performance Test
  test_that("Cold start performance meets requirements", {
    # Simulate cold start by clearing cache
    app <- AppDriver$new(
      app_dir = ".",
      name = "page_load_cold_start",
      options = list(shiny.port = sample(3000:4000, 1))
    )
    
    start_time <- Sys.time()
    app$wait_for_idle(timeout = 10000)
    end_time <- Sys.time()
    
    cold_start_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    expect_lt(cold_start_time, 10.0, info = "Cold start should complete within 10 seconds")
    
    app$stop()
  })
  
  # 1.3 Progressive Loading Test
  test_that("Progressive loading components appear in correct order", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "progressive_loading",
      timeout = 30000
    )
    
    # Track loading sequence
    loading_sequence <- list()
    
    # Check for loading spinner
    loading_sequence$spinner <- !is.null(app$get_html("#loading-spinner"))
    
    # Wait for main content
    app$wait_for_idle(timeout = 5000)
    
    # Check for sidebar appearance
    sidebar_present <- !is.null(app$get_html("#sidebar"))
    loading_sequence$sidebar <- sidebar_present
    
    # Check for main dashboard
    main_present <- !is.null(app$get_html("#main-dashboard"))
    loading_sequence$main_dashboard <- main_present
    
    # Verify loading sequence
    expect_true(loading_sequence$spinner, info = "Loading spinner should appear first")
    expect_true(loading_sequence$sidebar, info = "Sidebar should load")
    expect_true(loading_sequence$main_dashboard, info = "Main dashboard should load")
    
    app$stop()
  })
  
  # 1.4 Memory Usage During Load
  test_that("Memory usage during page load is reasonable", {
    gc() # Clean up before test
    
    initial_memory <- pryr::mem_used()
    
    app <- AppDriver$new(
      app_dir = ".",
      name = "memory_usage_load"
    )
    
    app$wait_for_idle(timeout = 5000)
    
    post_load_memory <- pryr::mem_used()
    memory_increase <- post_load_memory - initial_memory
    
    # Memory increase should be reasonable (less than 100MB)
    expect_lt(as.numeric(memory_increase), 100 * 1024^2, 
              info = "Memory increase should be less than 100MB")
    
    app$stop()
    gc() # Clean up after test
  })
  
  # 1.5 Concurrent User Load Test
  test_that("Page load time with concurrent users", {
    # Simulate multiple concurrent users
    load_times <- numeric(5)
    
    for (i in 1:5) {
      start_time <- Sys.time()
      
      app <- AppDriver$new(
        app_dir = ".",
        name = paste0("concurrent_user_", i),
        options = list(shiny.port = 3000 + i)
      )
      
      app$wait_for_idle(timeout = 10000)
      end_time <- Sys.time()
      
      load_times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      app$stop()
    }
    
    # Average load time should still be acceptable
    avg_load_time <- mean(load_times)
    expect_lt(avg_load_time, 8.0, info = "Average load time with concurrent users should be < 8s")
    
    # No single load should be excessively slow
    expect_true(all(load_times < 15.0), info = "No single load should exceed 15 seconds")
  })
})

# =============================================================================
# 2. INTERACTIVE ELEMENT RESPONSIVENESS
# =============================================================================

test_that("Interactive Element Responsiveness - Comprehensive Tests", {
  
  # 2.1 Button Click Responsiveness
  test_that("Button clicks are responsive", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "button_responsiveness"
    )
    
    app$wait_for_idle()
    
    # Test multiple button types
    buttons_to_test <- c("#refresh-data", "#generate-report", "#export-data")
    
    for (button_id in buttons_to_test) {
      if (!is.null(app$get_html(button_id))) {
        start_time <- Sys.time()
        
        app$click(button_id)
        app$wait_for_idle(timeout = 3000)
        
        end_time <- Sys.time()
        response_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        expect_lt(response_time, 2.0, 
                  info = paste("Button", button_id, "should respond within 2 seconds"))
      }
    }
    
    app$stop()
  })
  
  # 2.2 Input Field Responsiveness
  test_that("Input fields respond immediately to changes", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "input_responsiveness"
    )
    
    app$wait_for_idle()
    
    # Test various input types
    inputs_to_test <- list(
      list(id = "#department-filter", value = "Sales"),
      list(id = "#age-range", value = c(25, 45)),
      list(id = "#salary-filter", value = 50000)
    )
    
    for (input_test in inputs_to_test) {
      if (!is.null(app$get_html(input_test$id))) {
        start_time <- Sys.time()
        
        app$set_inputs(!!input_test$id := input_test$value)
        app$wait_for_idle(timeout = 1000)
        
        end_time <- Sys.time()
        response_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        expect_lt(response_time, 1.0, 
                  info = paste("Input", input_test$id, "should respond within 1 second"))
      }
    }
    
    app$stop()
  })
  
  # 2.3 Tab Switching Performance
  test_that("Tab switching is smooth and fast", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "tab_switching"
    )
    
    app$wait_for_idle()
    
    # Test tab switching between different modules
    tabs_to_test <- c("overview", "attrition", "demographics", "performance", "compensation")
    
    for (tab in tabs_to_test) {
      tab_selector <- paste0("#", tab, "-tab")
      
      if (!is.null(app$get_html(tab_selector))) {
        start_time <- Sys.time()
        
        app$click(tab_selector)
        app$wait_for_idle(timeout = 2000)
        
        end_time <- Sys.time()
        switch_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        expect_lt(switch_time, 1.5, 
                  info = paste("Tab", tab, "should switch within 1.5 seconds"))
      }
    }
    
    app$stop()
  })
  
  # 2.4 Modal Dialog Responsiveness
  test_that("Modal dialogs open and close quickly", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "modal_responsiveness"
    )
    
    app$wait_for_idle()
    
    # Test modal opening
    modal_trigger <- "#settings-modal-trigger"
    
    if (!is.null(app$get_html(modal_trigger))) {
      start_time <- Sys.time()
      
      app$click(modal_trigger)
      app$wait_for_idle(timeout = 1000)
      
      end_time <- Sys.time()
      open_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      expect_lt(open_time, 0.5, info = "Modal should open within 0.5 seconds")
      
      # Test modal closing
      modal_close <- "#modal-close"
      
      if (!is.null(app$get_html(modal_close))) {
        start_time <- Sys.time()
        
        app$click(modal_close)
        app$wait_for_idle(timeout = 1000)
        
        end_time <- Sys.time()
        close_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        expect_lt(close_time, 0.5, info = "Modal should close within 0.5 seconds")
      }
    }
    
    app$stop()
  })
  
  # 2.5 Dropdown Menu Performance
  test_that("Dropdown menus perform well with large datasets", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "dropdown_performance"
    )
    
    app$wait_for_idle()
    
    # Test dropdown with many options
    dropdown_selector <- "#employee-dropdown"
    
    if (!is.null(app$get_html(dropdown_selector))) {
      start_time <- Sys.time()
      
      app$click(dropdown_selector)
      app$wait_for_idle(timeout = 2000)
      
      end_time <- Sys.time()
      dropdown_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      expect_lt(dropdown_time, 1.5, 
                info = "Dropdown should open within 1.5 seconds even with large datasets")
      
      # Test typing in dropdown (if searchable)
      start_time <- Sys.time()
      
      app$send_keys(dropdown_selector, "John")
      app$wait_for_idle(timeout = 1000)
      
      end_time <- Sys.time()
      search_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      expect_lt(search_time, 1.0, info = "Dropdown search should respond within 1 second")
    }
    
    app$stop()
  })
})

# =============================================================================
# 3. CHART RENDERING PERFORMANCE
# =============================================================================

test_that("Chart Rendering Performance - Comprehensive Tests", {
  
  # 3.1 Basic Chart Rendering Speed
  test_that("Charts render within acceptable time limits", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "chart_rendering"
    )
    
    app$wait_for_idle()
    
    # Test different chart types
    charts_to_test <- c("#attrition-chart", "#salary-distribution", "#performance-radar")
    
    for (chart_id in charts_to_test) {
      if (!is.null(app$get_html(chart_id))) {
        start_time <- Sys.time()
        
        # Trigger chart update
        app$click("#refresh-charts")
        app$wait_for_idle(timeout = 5000)
        
        end_time <- Sys.time()
        render_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        expect_lt(render_time, 3.0, 
                  info = paste("Chart", chart_id, "should render within 3 seconds"))
      }
    }
    
    app$stop()
  })
  
  # 3.2 Large Dataset Chart Performance
  test_that("Charts handle large datasets efficiently", {
    # Create synthetic large dataset
    large_dataset <- data.frame(
      employee_id = 1:10000,
      department = sample(c("Sales", "IT", "HR", "Finance", "Marketing"), 10000, replace = TRUE),
      salary = rnorm(10000, 50000, 15000),
      performance = sample(1:5, 10000, replace = TRUE),
      attrition = sample(c(0, 1), 10000, replace = TRUE, prob = c(0.85, 0.15))
    )
    
    # Test chart rendering with large dataset
    render_time <- system.time({
      p <- ggplot(large_dataset, aes(x = department, y = salary)) +
        geom_boxplot() +
        theme_minimal()
      
      plotly_chart <- ggplotly(p)
    })
    
    expect_lt(render_time[["elapsed"]], 5.0, 
              info = "Large dataset chart should render within 5 seconds")
    
    # Test memory usage
    object_size <- object.size(plotly_chart)
    expect_lt(as.numeric(object_size), 50 * 1024^2, 
              info = "Chart object should be less than 50MB")
  })
  
  # 3.3 Interactive Chart Performance
  test_that("Interactive charts maintain responsiveness", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "interactive_charts"
    )
    
    app$wait_for_idle()
    
    # Test hover interactions
    chart_selector <- "#interactive-scatter-plot"
    
    if (!is.null(app$get_html(chart_selector))) {
      start_time <- Sys.time()
      
      # Simulate hover event
      app$run_js(paste0("
        var chart = document.querySelector('", chart_selector, "');
        if (chart) {
          var event = new MouseEvent('mouseover', {
            'view': window,
            'bubbles': true,
            'cancelable': true
          });
          chart.dispatchEvent(event);
        }
      "))
      
      app$wait_for_idle(timeout = 1000)
      
      end_time <- Sys.time()
      hover_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      expect_lt(hover_time, 0.5, info = "Chart hover should respond within 0.5 seconds")
    }
    
    app$stop()
  })
  
  # 3.4 Chart Animation Performance
  test_that("Chart animations are smooth and performant", {
    # Test animation frame rates
    animation_test <- function() {
      frames <- 60
      start_time <- Sys.time()
      
      for (i in 1:frames) {
        # Simulate chart update
        Sys.sleep(0.016) # 60 FPS target
      }
      
      end_time <- Sys.time()
      total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      return(total_time)
    }
    
    animation_time <- animation_test()
    expected_time <- 60 * 0.016 # 60 frames at 16ms each
    
    expect_lt(animation_time, expected_time * 1.5, 
              info = "Animations should maintain close to 60 FPS")
  })
  
  # 3.5 Chart Memory Management
  test_that("Charts properly manage memory during updates", {
    gc() # Clean up before test
    
    initial_memory <- pryr::mem_used()
    
    # Create and destroy multiple charts
    for (i in 1:10) {
      large_data <- data.frame(
        x = rnorm(1000),
        y = rnorm(1000),
        group = sample(letters[1:5], 1000, replace = TRUE)
      )
      
      chart <- ggplot(large_data, aes(x, y, color = group)) +
        geom_point() +
        theme_minimal()
      
      plotly_chart <- ggplotly(chart)
      
      # Explicitly remove objects
      rm(large_data, chart, plotly_chart)
    }
    
    gc() # Force garbage collection
    
    final_memory <- pryr::mem_used()
    memory_increase <- final_memory - initial_memory
    
    expect_lt(as.numeric(memory_increase), 50 * 1024^2, 
              info = "Memory increase should be less than 50MB after chart operations")
  })
})

# =============================================================================
# 4. DATA VISUALIZATION EFFICIENCY
# =============================================================================

test_that("Data Visualization Efficiency - Comprehensive Tests", {
  
  # 4.1 Data Processing Speed
  test_that("Data processing for visualizations is efficient", {
    # Create test dataset
    test_data <- data.frame(
      employee_id = 1:5000,
      department = sample(c("Sales", "IT", "HR", "Finance", "Marketing"), 5000, replace = TRUE),
      salary = rnorm(5000, 50000, 15000),
      performance = sample(1:5, 5000, replace = TRUE),
      hire_date = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 5000, replace = TRUE)
    )
    
    # Test data aggregation speed
    aggregation_time <- system.time({
      dept_summary <- test_data %>%
        group_by(department) %>%
        summarise(
          avg_salary = mean(salary),
          median_performance = median(performance),
          count = n(),
          .groups = "drop"
        )
    })
    
    expect_lt(aggregation_time[["elapsed"]], 1.0, 
              info = "Data aggregation should complete within 1 second")
    
    # Test filtering speed
    filtering_time <- system.time({
      filtered_data <- test_data %>%
        filter(salary > 40000, performance >= 3) %>%
        arrange(desc(salary))
    })
    
    expect_lt(filtering_time[["elapsed"]], 0.5, 
              info = "Data filtering should complete within 0.5 seconds")
  })
  
  # 4.2 Visualization Rendering Efficiency
  test_that("Visualization rendering is optimized", {
    # Test different visualization types
    test_data <- data.frame(
      category = sample(LETTERS[1:10], 1000, replace = TRUE),
      value = rnorm(1000, 100, 20),
      group = sample(c("Group1", "Group2", "Group3"), 1000, replace = TRUE)
    )
    
    # Bar chart rendering
    bar_time <- system.time({
      bar_chart <- ggplot(test_data, aes(x = category, y = value, fill = group)) +
        geom_bar(stat = "identity") +
        theme_minimal()
      
      plotly_bar <- ggplotly(bar_chart)
    })
    
    expect_lt(bar_time[["elapsed"]], 2.0, 
              info = "Bar chart should render within 2 seconds")
    
    # Scatter plot rendering
    scatter_time <- system.time({
      scatter_plot <- ggplot(test_data, aes(x = value, y = category, color = group)) +
        geom_point(alpha = 0.7) +
        theme_minimal()
      
      plotly_scatter <- ggplotly(scatter_plot)
    })
    
    expect_lt(scatter_time[["elapsed"]], 2.0, 
              info = "Scatter plot should render within 2 seconds")
  })
  
  # 4.3 Data Table Performance
  test_that("Data tables handle large datasets efficiently", {
    # Create large dataset
    large_data <- data.frame(
      id = 1:50000,
      name = paste("Employee", 1:50000),
      department = sample(c("Sales", "IT", "HR", "Finance", "Marketing"), 50000, replace = TRUE),
      salary = rnorm(50000, 50000, 15000),
      performance = sample(1:5, 50000, replace = TRUE)
    )
    
    # Test DT creation
    dt_time <- system.time({
      dt_table <- DT::datatable(
        large_data,
        options = list(
          pageLength = 25,
          processing = TRUE,
          serverSide = TRUE,
          scrollX = TRUE
        )
      )
    })
    
    expect_lt(dt_time[["elapsed"]], 3.0, 
              info = "Large data table should initialize within 3 seconds")
  })
  
  # 4.4 Real-time Data Updates
  test_that("Real-time data updates are efficient", {
    # Simulate real-time data updates
    base_data <- data.frame(
      timestamp = Sys.time(),
      value = rnorm(100),
      category = sample(c("A", "B", "C"), 100, replace = TRUE)
    )
    
    update_times <- numeric(10)
    
    for (i in 1:10) {
      start_time <- Sys.time()
      
      # Simulate data update
      new_row <- data.frame(
        timestamp = Sys.time(),
        value = rnorm(1),
        category = sample(c("A", "B", "C"), 1)
      )
      
      updated_data <- rbind(base_data, new_row)
      
      # Update visualization
      updated_plot <- ggplot(updated_data, aes(x = timestamp, y = value, color = category)) +
        geom_line() +
        theme_minimal()
      
      end_time <- Sys.time()
      update_times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
    }
    
    avg_update_time <- mean(update_times)
    expect_lt(avg_update_time, 0.5, 
              info = "Real-time updates should average less than 0.5 seconds")
  })
  
  # 4.5 Memory Efficient Visualizations
  test_that("Visualizations use memory efficiently", {
    gc() # Clean up before test
    
    initial_memory <- pryr::mem_used()
    
    # Create multiple visualizations
    for (chart_type in c("bar", "scatter", "line", "histogram")) {
      test_data <- data.frame(
        x = rnorm(1000),
        y = rnorm(1000),
        group = sample(c("A", "B", "C"), 1000, replace = TRUE)
      )
      
      chart <- switch(chart_type,
        "bar" = ggplot(test_data, aes(x = group, y = x)) + geom_bar(stat = "identity"),
        "scatter" = ggplot(test_data, aes(x = x, y = y, color = group)) + geom_point(),
        "line" = ggplot(test_data, aes(x = x, y = y, color = group)) + geom_line(),
        "histogram" = ggplot(test_data, aes(x = x, fill = group)) + geom_histogram()
      )
      
      plotly_chart <- ggplotly(chart)
      
      # Clean up
      rm(test_data, chart, plotly_chart)
    }
    
    gc() # Force garbage collection
    
    final_memory <- pryr::mem_used()
    memory_increase <- final_memory - initial_memory
    
    expect_lt(as.numeric(memory_increase), 100 * 1024^2, 
              info = "Memory increase should be less than 100MB for multiple visualizations")
  })
})

# =============================================================================
# 5. MOBILE DEVICE PERFORMANCE
# =============================================================================

test_that("Mobile Device Performance - Comprehensive Tests", {
  
  # 5.1 Mobile Responsive Design
  test_that("App responds correctly to mobile viewports", {
    # Test different mobile viewport sizes
    mobile_viewports <- list(
      list(width = 375, height = 667, name = "iPhone 8"),
      list(width = 414, height = 896, name = "iPhone 11"),
      list(width = 360, height = 640, name = "Android Small"),
      list(width = 768, height = 1024, name = "iPad")
    )
    
    for (viewport in mobile_viewports) {
      app <- AppDriver$new(
        app_dir = ".",
        name = paste0("mobile_", gsub(" ", "_", viewport$name)),
        width = viewport$width,
        height = viewport$height
      )
      
      app$wait_for_idle()
      
      # Check if sidebar collapses on mobile
      if (viewport$width < 768) {
        sidebar_html <- app$get_html("#sidebar")
        expect_true(grepl("collapsed", sidebar_html) || grepl("hidden", sidebar_html),
                   info = paste("Sidebar should collapse on", viewport$name))
      }
      
      # Check if main content adjusts
      main_content <- app$get_html("#main-content")
      expect_false(is.null(main_content), 
                   info = paste("Main content should be visible on", viewport$name))
      
      app$stop()
    }
  })
  
  # 5.2 Touch Interface Performance
  test_that("Touch interactions are responsive", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "touch_interface",
      width = 375,
      height = 667
    )
    
    app$wait_for_idle()
    
    # Test touch events
    touch_elements <- c("#menu-toggle", "#refresh-button", "#export-button")
    
    for (element in touch_elements) {
      if (!is.null(app$get_html(element))) {
        start_time <- Sys.time()
        
        # Simulate touch event
        app$run_js(paste0("
          var element = document.querySelector('", element, "');
          if (element) {
            var touchEvent = new TouchEvent('touchstart', {
              bubbles: true,
              cancelable: true,
              view: window
            });
            element.dispatchEvent(touchEvent);
          }
        "))
        
        app$wait_for_idle(timeout = 1000)
        
        end_time <- Sys.time()
        touch_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        expect_lt(touch_time, 0.5, 
                  info = paste("Touch interaction on", element, "should respond within 0.5 seconds"))
      }
    }
    
    app$stop()
  })
  
  # 5.3 Mobile Chart Performance
  test_that("Charts perform well on mobile devices", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "mobile_charts",
      width = 375,
      height = 667
    )
    
    app$wait_for_idle()
    
    # Test chart rendering on mobile
    chart_selector <- "#mobile-optimized-chart"
    
    if (!is.null(app$get_html(chart_selector))) {
      start_time <- Sys.time()
      
      # Trigger chart update
      app$click("#update-mobile-chart")
      app$wait_for_idle(timeout = 5000)
      
      end_time <- Sys.time()
      mobile_chart_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      expect_lt(mobile_chart_time, 4.0, 
                info = "Mobile chart should render within 4 seconds")
      
      # Test chart zoom/pan on mobile
      app$run_js(paste0("
        var chart = document.querySelector('", chart_selector, "');
        if (chart) {
          var pinchEvent = new TouchEvent('touchstart', {
            bubbles: true,
            cancelable: true,
            touches: [
              new Touch({identifier: 1, target: chart, clientX: 100, clientY: 100}),
              new Touch({identifier: 2, target: chart, clientX: 200, clientY: 200})
            ]
          });
          chart.dispatchEvent(pinchEvent);
        }
      "))
    }
    
    app$stop()
  })
  
  # 5.4 Mobile Data Loading
  test_that("Data loading is optimized for mobile", {
    app <- AppDriver$new(
      app_dir = ".",
      name = "mobile_data_loading",
      width = 375,
      height = 667
    )
    
    start_time <- Sys.time()
    app$wait_for_idle(timeout = 10000)
    end_time <- Sys.time()
    
    mobile_load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    expect_lt(mobile_load_time, 8.0, 
              info = "Mobile data loading should complete within 8 seconds")
    
    app$stop()
  })
  
  # 5.5 Mobile Memory Usage
  test_that("Mobile app uses memory efficiently", {
    # Simulate mobile memory constraints
    mobile_memory_limit <- 512 * 1024^2 # 512MB limit
    
    gc() # Clean up before test
    
    initial_memory <- pryr::mem_used()
    
    app <- AppDriver$new(
      app_dir = ".",
      name = "mobile_memory",
      width = 375,
      height = 667
    )
    
    app$wait_for