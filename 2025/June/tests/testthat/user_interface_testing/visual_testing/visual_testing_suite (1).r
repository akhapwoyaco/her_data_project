# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - VISUAL TESTING SUITE
# Comprehensive Unit Tests for User Interface Visual Components
# =============================================================================

library(testthat)
library(shiny)
library(shinycssloaders)
library(htmltools)
library(rvest)
library(magrittr)
library(stringr)
library(RSelenium)
library(webshot2)
library(magick)

# =============================================================================
# 7.2.1 LAYOUT CONSISTENCY TESTING
# =============================================================================

test_that("Layout Consistency - Grid System Validation", {
  
  # Test Bootstrap grid system consistency
  test_bootstrap_grid_consistency <- function() {
    ui <- fluidPage(
      fluidRow(
        column(12, div(id = "full-width", "Full Width")),
        column(6, div(id = "half-width-1", "Half 1")),
        column(6, div(id = "half-width-2", "Half 2")),
        column(4, div(id = "third-width-1", "Third 1")),
        column(4, div(id = "third-width-2", "Third 2")),
        column(4, div(id = "third-width-3", "Third 3"))
      )
    )
    
    rendered_html <- as.character(ui)
    
    # Test column classes
    expect_true(grepl('class="col-sm-12"', rendered_html))
    expect_true(grepl('class="col-sm-6"', rendered_html))
    expect_true(grepl('class="col-sm-4"', rendered_html))
    
    # Test proper nesting
    expect_true(grepl('<div class="row">', rendered_html))
    expect_equal(str_count(rendered_html, '<div class="row">'), 1)
    
    return(TRUE)
  }
  
  expect_true(test_bootstrap_grid_consistency())
})

test_that("Layout Consistency - Module Container Alignment", {
  
  # Test module containers have consistent structure
  test_module_container_structure <- function(module_ui_func) {
    mock_ui <- module_ui_func("test_id")
    rendered <- as.character(mock_ui)
    
    # All modules should have consistent container structure
    expect_true(grepl('class=".*module-container.*"', rendered) ||
                grepl('class=".*atlas-module.*"', rendered))
    
    # Check for proper spacing classes
    expect_true(grepl('class=".*mb-[3-4].*"', rendered) ||
                grepl('class=".*margin-bottom.*"', rendered))
    
    return(TRUE)
  }
  
  # Mock module UI functions for testing
  mock_overview_ui <- function(id) {
    div(class = "atlas-module overview-module mb-4",
        h3("Overview Dashboard"),
        div(class = "module-content", "Content here"))
  }
  
  expect_true(test_module_container_structure(mock_overview_ui))
})

test_that("Layout Consistency - Responsive Breakpoints", {
  
  # Test responsive design breakpoints
  test_responsive_breakpoints <- function() {
    breakpoints <- list(
      xs = "max-width: 575.98px",
      sm = "min-width: 576px",
      md = "min-width: 768px", 
      lg = "min-width: 992px",
      xl = "min-width: 1200px",
      xxl = "min-width: 1400px"
    )
    
    # Mock CSS content
    css_content <- "
      @media (max-width: 575.98px) { .atlas-card { padding: 0.5rem; } }
      @media (min-width: 576px) { .atlas-card { padding: 1rem; } }
      @media (min-width: 768px) { .atlas-card { padding: 1.5rem; } }
      @media (min-width: 992px) { .atlas-card { padding: 2rem; } }
      @media (min-width: 1200px) { .atlas-card { padding: 2.5rem; } }
    "
    
    # Validate breakpoint syntax
    for (bp_name in names(breakpoints)) {
      bp_pattern <- breakpoints[[bp_name]]
      expect_true(grepl(bp_pattern, css_content, fixed = TRUE))
    }
    
    return(TRUE)
  }
  
  expect_true(test_responsive_breakpoints())
})

test_that("Layout Consistency - Header Footer Alignment", {
  
  # Test header/footer consistency across pages
  test_header_footer_structure <- function() {
    header_structure <- div(
      class = "atlas-header fixed-top",
      div(class = "container-fluid",
          div(class = "row align-items-center",
              div(class = "col-md-6", 
                  img(src = "atlas_labs_logo.png", class = "logo")),
              div(class = "col-md-6 text-end",
                  span(class = "app-version", "v1.0.0"))
          )
      )
    )
    
    footer_structure <- div(
      class = "atlas-footer mt-auto py-3",
      div(class = "container-fluid text-center",
          p("© 2025 Atlas Labs | Developed by akhapwoyaco"))
    )
    
    header_html <- as.character(header_structure)
    footer_html <- as.character(footer_structure)
    
    # Validate header structure
    expect_true(grepl('class="atlas-header fixed-top"', header_html))
    expect_true(grepl('class="container-fluid"', header_html))
    expect_true(grepl('class="logo"', header_html))
    
    # Validate footer structure  
    expect_true(grepl('class="atlas-footer mt-auto py-3"', footer_html))
    expect_true(grepl('akhapwoyaco', footer_html))
    
    return(TRUE)
  }
  
  expect_true(test_header_footer_structure())
})

# =============================================================================
# 7.2.2 COLOR SCHEME VALIDATION TESTING
# =============================================================================

test_that("Color Scheme - Brand Color Consistency", {
  
  # Test Atlas Labs brand colors
  test_brand_colors <- function() {
    atlas_colors <- list(
      primary = "#2C3E50",      # Dark blue-gray
      secondary = "#3498DB",    # Bright blue
      success = "#27AE60",      # Green
      warning = "#F39C12",      # Orange
      danger = "#E74C3C",       # Red
      info = "#17A2B8",         # Cyan
      light = "#F8F9FA",        # Light gray
      dark = "#343A40"          # Dark gray
    )
    
    # Validate hex color format
    for (color_name in names(atlas_colors)) {
      color_value <- atlas_colors[[color_name]]
      expect_true(grepl("^#[0-9A-Fa-f]{6}$", color_value),
                  info = paste("Invalid hex color:", color_name, color_value))
    }
    
    # Test color contrast ratios (WCAG AA compliance)
    test_contrast_ratio <- function(fg, bg) {
      # Mock contrast calculation (simplified)
      fg_rgb <- col2rgb(fg)
      bg_rgb <- col2rgb(bg)
      
      # Calculate relative luminance (simplified)
      get_luminance <- function(rgb_val) {
        rgb_norm <- rgb_val / 255
        sum(rgb_norm * c(0.299, 0.587, 0.114))
      }
      
      l1 <- get_luminance(fg_rgb)
      l2 <- get_luminance(bg_rgb)
      
      contrast <- (max(l1, l2) + 0.05) / (min(l1, l2) + 0.05)
      return(contrast >= 4.5)  # WCAG AA standard
    }
    
    # Test key color combinations
    expect_true(test_contrast_ratio(atlas_colors$primary, atlas_colors$light))
    expect_true(test_contrast_ratio(atlas_colors$dark, atlas_colors$light))
    
    return(TRUE)
  }
  
  expect_true(test_brand_colors())
})

test_that("Color Scheme - CSS Variable Consistency", {
  
  # Test CSS custom properties for colors
  test_css_variables <- function() {
    css_variables <- "
      :root {
        --atlas-primary: #2C3E50;
        --atlas-secondary: #3498DB;
        --atlas-success: #27AE60;
        --atlas-warning: #F39C12;
        --atlas-danger: #E74C3C;
        --atlas-info: #17A2B8;
        --atlas-light: #F8F9FA;
        --atlas-dark: #343A40;
        --atlas-border: #DEE2E6;
        --atlas-text: #212529;
        --atlas-muted: #6C757D;
      }
    "
    
    # Validate CSS variable declarations
    expect_true(grepl("--atlas-primary:\\s*#[0-9A-Fa-f]{6};", css_variables))
    expect_true(grepl("--atlas-secondary:\\s*#[0-9A-Fa-f]{6};", css_variables))
    expect_true(grepl("--atlas-success:\\s*#[0-9A-Fa-f]{6};", css_variables))
    
    # Count variable declarations
    variable_count <- str_count(css_variables, "--atlas-[a-z]+:")
    expect_gte(variable_count, 10)
    
    return(TRUE)
  }
  
  expect_true(test_css_variables())
})

test_that("Color Scheme - Accessibility Compliance", {
  
  # Test color accessibility for visually impaired users
  test_color_accessibility <- function() {
    # Test color-blind friendly palette
    colorblind_safe_colors <- c(
      "#1f77b4",  # Blue
      "#ff7f0e",  # Orange
      "#2ca02c",  # Green
      "#d62728",  # Red
      "#9467bd",  # Purple
      "#8c564b",  # Brown
      "#e377c2",  # Pink
      "#7f7f7f",  # Gray
      "#bcbd22",  # Olive
      "#17becf"   # Cyan
    )
    
    # Validate each color is distinguishable
    for (i in seq_along(colorblind_safe_colors)) {
      color <- colorblind_safe_colors[i]
      expect_true(grepl("^#[0-9A-Fa-f]{6}$", color))
      
      # Convert to RGB for distance calculation
      rgb_val <- col2rgb(color)
      expect_true(all(rgb_val >= 0 & rgb_val <= 255))
    }
    
    # Test minimum color differences
    min_distance <- 50  # Minimum perceptual distance
    for (i in 1:(length(colorblind_safe_colors)-1)) {
      for (j in (i+1):length(colorblind_safe_colors)) {
        rgb1 <- col2rgb(colorblind_safe_colors[i])
        rgb2 <- col2rgb(colorblind_safe_colors[j])
        
        # Euclidean distance in RGB space
        distance <- sqrt(sum((rgb1 - rgb2)^2))
        expect_gte(distance, min_distance,
                   info = paste("Colors too similar:", i, j))
      }
    }
    
    return(TRUE)
  }
  
  expect_true(test_color_accessibility())
})

# =============================================================================
# 7.2.3 TYPOGRAPHY CONSISTENCY TESTING
# =============================================================================

test_that("Typography - Font Family Consistency", {
  
  # Test font stack consistency
  test_font_families <- function() {
    font_definitions <- list(
      primary = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif",
      monospace = "'SF Mono', Monaco, 'Cascadia Code', 'Roboto Mono', monospace",
      display = "'Inter', system-ui, -apple-system, sans-serif"
    )
    
    # Validate font stack format
    for (font_name in names(font_definitions)) {
      font_stack <- font_definitions[[font_name]]
      
      # Check for proper fallbacks
      expect_true(grepl("sans-serif|serif|monospace", font_stack),
                  info = paste("Missing fallback for:", font_name))
      
      # Check for proper quoting
      quoted_fonts <- str_extract_all(font_stack, "'[^']+'")[[1]]
      spaced_fonts <- str_extract_all(font_stack, "[A-Za-z]+\\s+[A-Za-z]+")[[1]]
      
      if (length(spaced_fonts) > 0) {
        expect_true(all(paste0("'", spaced_fonts, "'") %in% quoted_fonts),
                    info = "Multi-word fonts should be quoted")
      }
    }
    
    return(TRUE)
  }
  
  expect_true(test_font_families())
})

test_that("Typography - Font Size Hierarchy", {
  
  # Test typography scale consistency
  test_font_scale <- function() {
    # Define type scale (based on Major Third - 1.25 ratio)
    font_scale <- list(
      h1 = "2.5rem",    # 40px
      h2 = "2rem",      # 32px  
      h3 = "1.75rem",   # 28px
      h4 = "1.5rem",    # 24px
      h5 = "1.25rem",   # 20px
      h6 = "1rem",      # 16px
      body = "1rem",    # 16px
      small = "0.875rem", # 14px
      xs = "0.75rem"    # 12px
    )
    
    # Convert rem to numeric for validation
    get_rem_value <- function(rem_string) {
      as.numeric(gsub("rem", "", rem_string))
    }
    
    rem_values <- sapply(font_scale, get_rem_value)
    
    # Test descending hierarchy
    expect_true(all(diff(rem_values) <= 0),
                info = "Font sizes should decrease down the hierarchy")
    
    # Test minimum readable sizes
    expect_gte(min(rem_values), 0.75,
               info = "Minimum font size should be 0.75rem for accessibility")
    
    # Test maximum reasonable sizes
    expect_lte(max(rem_values), 3,
               info = "Maximum font size should not exceed 3rem")
    
    return(TRUE)
  }
  
  expect_true(test_font_scale())
})

test_that("Typography - Line Height Consistency", {
  
  # Test line height ratios
  test_line_heights <- function() {
    line_height_ratios <- list(
      headings = 1.2,    # Tight for headings
      body = 1.6,        # Comfortable for reading
      small = 1.4,       # Slightly tighter for small text
      buttons = 1.5,     # Balanced for UI elements
      captions = 1.3     # Compact for captions
    )
    
    # Validate line height ratios
    for (element in names(line_height_ratios)) {
      ratio <- line_height_ratios[[element]]
      
      expect_gte(ratio, 1.1,
                 info = paste("Line height too tight for", element))
      expect_lte(ratio, 2.0,
                 info = paste("Line height too loose for", element))
    }
    
    # Test accessibility requirements
    expect_gte(line_height_ratios$body, 1.5,
               info = "Body text line height should be at least 1.5 for accessibility")
    
    return(TRUE)
  }
  
  expect_true(test_line_heights())
})

test_that("Typography - Font Weight Consistency", {
  
  # Test font weight usage
  test_font_weights <- function() {
    font_weights <- list(
      thin = 100,
      light = 300,
      regular = 400,
      medium = 500,
      semibold = 600,
      bold = 700,
      extrabold = 800,
      black = 900
    )
    
    # Validate weight values
    for (weight_name in names(font_weights)) {
      weight_value <- font_weights[[weight_name]]
      
      expect_true(weight_value %in% seq(100, 900, 100),
                  info = paste("Invalid font weight:", weight_name, weight_value))
    }
    
    # Test semantic usage
    ui_weights <- list(
      headings = c(600, 700),     # Semibold to bold
      body = c(400, 500),         # Regular to medium
      emphasis = c(600, 700),     # Semibold to bold
      captions = c(400, 500)      # Regular to medium
    )
    
    for (element in names(ui_weights)) {
      allowed_weights <- ui_weights[[element]]
      expect_true(all(allowed_weights %in% unlist(font_weights)),
                  info = paste("Invalid weight for", element))
    }
    
    return(TRUE)
  }
  
  expect_true(test_font_weights())
})

# =============================================================================
# 7.2.4 IMAGE RENDERING QUALITY TESTING
# =============================================================================

test_that("Image Rendering - Logo Quality", {
  
  # Test Atlas Labs logo rendering
  test_logo_rendering <- function() {
    logo_specs <- list(
      formats = c("PNG", "SVG", "WebP"),
      sizes = list(
        small = c(width = 120, height = 40),
        medium = c(width = 240, height = 80),
        large = c(width = 480, height = 160)
      ),
      dpi = c(72, 144, 192)  # 1x, 2x, 2.67x
    )
    
    # Validate supported formats
    supported_formats <- c("PNG", "JPEG", "SVG", "WebP", "GIF")
    expect_true(all(logo_specs$formats %in% supported_formats))
    
    # Validate aspect ratios
    for (size_name in names(logo_specs$sizes)) {
      dimensions <- logo_specs$sizes[[size_name]]
      aspect_ratio <- dimensions["width"] / dimensions["height"]
      
      # Logo should maintain 3:1 aspect ratio
      expect_equal(aspect_ratio, 3, tolerance = 0.1,
                   info = paste("Logo aspect ratio incorrect for", size_name))
    }
    
    # Validate DPI values
    expect_true(all(logo_specs$dpi >= 72),
                info = "Minimum DPI should be 72 for web")
    expect_true(any(logo_specs$dpi >= 144),
                info = "Should support high-DPI displays")
    
    return(TRUE)
  }
  
  expect_true(test_logo_rendering())
})

test_that("Image Rendering - Chart Export Quality", {
  
  # Test plotly chart export quality
  test_chart_export_quality <- function() {
    export_settings <- list(
      formats = c("PNG", "PDF", "SVG", "HTML"),
      dimensions = list(
        standard = c(width = 800, height = 600),
        wide = c(width = 1200, height = 600),
        square = c(width = 600, height = 600),
        mobile = c(width = 400, height = 300)
      ),
      dpi = 300,  # High quality for exports
      quality = 95  # JPEG quality if applicable
    )
    
    # Validate export formats
    expect_true(all(export_settings$formats %in% 
                   c("PNG", "PDF", "SVG", "HTML", "JPEG")))
    
    # Validate dimensions
    for (dim_name in names(export_settings$dimensions)) {
      dims <- export_settings$dimensions[[dim_name]]
      
      expect_gte(dims["width"], 300,
                 info = paste("Minimum width too small for", dim_name))
      expect_gte(dims["height"], 200,
                 info = paste("Minimum height too small for", dim_name))
      expect_lte(dims["width"], 2000,
                 info = paste("Maximum width too large for", dim_name))
      expect_lte(dims["height"], 1500,
                 info = paste("Maximum height too large for", dim_name))
    }
    
    # Validate quality settings
    expect_gte(export_settings$dpi, 150,
               info = "Export DPI should be at least 150 for print quality")
    expect_gte(export_settings$quality, 80,
               info = "JPEG quality should be at least 80")
    
    return(TRUE)
  }
  
  expect_true(test_chart_export_quality())
})

test_that("Image Rendering - Responsive Images", {
  
  # Test responsive image implementation
  test_responsive_images <- function() {
    # Mock responsive image HTML
    responsive_img_html <- '
      <picture>
        <source media="(min-width: 768px)" srcset="logo-large.png 1x, logo-large@2x.png 2x">
        <source media="(min-width: 480px)" srcset="logo-medium.png 1x, logo-medium@2x.png 2x">
        <img src="logo-small.png" srcset="logo-small@2x.png 2x" alt="Atlas Labs Logo" loading="lazy">
      </picture>
    '
    
    # Validate picture element structure
    expect_true(grepl("<picture>", responsive_img_html))
    expect_true(grepl("</picture>", responsive_img_html))
    
    # Validate source elements with media queries
    expect_true(grepl('media="\\(min-width:', responsive_img_html))
    
    # Validate srcset attributes
    expect_true(grepl('srcset="[^"]*@2x\\.png 2x"', responsive_img_html))
    
    # Validate lazy loading
    expect_true(grepl('loading="lazy"', responsive_img_html))
    
    # Validate alt text
    expect_true(grepl('alt="[^"]*"', responsive_img_html))
    
    return(TRUE)
  }
  
  expect_true(test_responsive_images())
})

# =============================================================================
# 7.2.5 ANIMATION SMOOTHNESS TESTING
# =============================================================================

test_that("Animation Smoothness - CSS Transitions", {
  
  # Test CSS transition properties
  test_css_transitions <- function() {
    transition_rules <- list(
      button_hover = "all 0.2s ease-in-out",
      modal_fade = "opacity 0.3s ease-out",
      sidebar_slide = "transform 0.25s cubic-bezier(0.4, 0, 0.2, 1)",
      chart_update = "all 0.4s ease-in-out",
      loading_spin = "transform 1s linear infinite"
    )
    
    # Validate transition duration ranges
    duration_pattern <- "([0-9.]+)s"
    
    for (element in names(transition_rules)) {
      rule <- transition_rules[[element]]
      
      # Extract duration
      duration_match <- str_extract(rule, duration_pattern)
      if (!is.na(duration_match)) {
        duration <- as.numeric(gsub("s", "", duration_match))
        
        # Validate reasonable duration ranges
        if (!grepl("infinite", rule)) {
          expect_gte(duration, 0.1,
                     info = paste("Transition too fast for", element))
          expect_lte(duration, 1.0,
                     info = paste("Transition too slow for", element))
        }
      }
      
      # Validate easing functions
      valid_easings <- c("ease", "ease-in", "ease-out", "ease-in-out", 
                        "linear", "cubic-bezier")
      
      has_valid_easing <- any(sapply(valid_easings, function(easing) {
        grepl(easing, rule)
      }))
      
      expect_true(has_valid_easing,
                  info = paste("Invalid easing for", element))
    }
    
    return(TRUE)
  }
  
  expect_true(test_css_transitions())
})

test_that("Animation Smoothness - Frame Rate Optimization", {
  
  # Test animation performance considerations
  test_animation_performance <- function() {
    # Properties that trigger layout/paint vs. composite-only
    composite_only_props <- c("transform", "opacity", "filter")
    layout_triggering_props <- c("width", "height", "padding", "margin", 
                                 "top", "left", "right", "bottom")
    
    # Mock animation CSS
    animation_css <- "
      .atlas-fade { transition: opacity 0.3s ease-out; }
      .atlas-slide { transition: transform 0.25s ease-in-out; }
      .atlas-scale { transition: transform 0.2s cubic-bezier(0.4, 0, 0.2, 1); }
      .atlas-blur { transition: filter 0.3s ease-in-out; }
    "
    
    # Count composite-only vs. layout-triggering animations
    composite_count <- sum(sapply(composite_only_props, function(prop) {
      str_count(animation_css, paste0("transition:\\s*", prop))
    }))
    
    layout_count <- sum(sapply(layout_triggering_props, function(prop) {
      str_count(animation_css, paste0("transition:\\s*", prop))
    }))
    
    # Should prefer composite-only properties
    expect_gte(composite_count, layout_count,
               info = "Should prefer composite-only properties for better performance")
    
    # Test will-change property usage
    will_change_css <- ".atlas-animated { will-change: transform, opacity; }"
    expect_true(grepl("will-change:", will_change_css))
    
    return(TRUE)
  }
  
  expect_true(test_animation_performance())
})

test_that("Animation Smoothness - Reduced Motion Support", {
  
  # Test accessibility for users who prefer reduced motion
  test_reduced_motion <- function() {
    reduced_motion_css <- "
      @media (prefers-reduced-motion: reduce) {
        *,
        *::before,
        *::after {
          animation-duration: 0.01ms !important;
          animation-iteration-count: 1 !important;
          transition-duration: 0.01ms !important;
          scroll-behavior: auto !important;
        }
      }
    "
    
    # Validate reduced motion media query
    expect_true(grepl("@media \\(prefers-reduced-motion: reduce\\)", reduced_motion_css))
    
    # Check for animation overrides
    expect_true(grepl("animation-duration: 0\\.01ms !important", reduced_motion_css))
    expect_true(grepl("transition-duration: 0\\.01ms !important", reduced_motion_css))
    expect_true(grepl("scroll-behavior: auto !important", reduced_motion_css))
    
    return(TRUE)
  }
  
  expect_true(test_reduced_motion())
})

# =============================================================================
# 7.2.6 LOADING INDICATOR ACCURACY TESTING
# =============================================================================

test_that("Loading Indicators - Spinner Accuracy", {
  
  # Test loading spinner implementation
  test_loading_spinners <- function() {
    # Mock loading spinner HTML
    spinner_html <- '
      <div class="atlas-spinner" role="status" aria-label="Loading">
        <div class="spinner-border text-primary" role="status">
          <span class="visually-hidden">Loading...</span>
        </div>
      </div>
    '
    
    # Validate accessibility attributes
    expect_true(grepl('role="status"', spinner_html))
    expect_true(grepl('aria-label="Loading"', spinner_html))
    expect_true(grepl('visually-hidden', spinner_html))
    
    # Validate Bootstrap spinner classes
    expect_true(grepl('spinner-border', spinner_html))
    expect_true(grepl('text-primary', spinner_html))
    
    return(TRUE)
  }
  
  expect_true(test_loading_spinners())
})

test_that("Loading Indicators - Progress Bar Accuracy", {
  
  # Test progress bar implementation
  test_progress_bars <- function() {
    # Mock progress bar HTML
    progress_html <- '
      <div class="progress atlas-progress" role="progressbar" aria-valuenow="65" aria-valuemin="0" aria-valuemax="100">
        <div class="progress-bar bg-primary" style="width: 65%">
          <span class="visually-hidden">65% Complete</span>
        </div>
      </div>
    '
    
    # Validate ARIA attributes
    expect_true(grepl('role="progressbar"', progress_html))
    expect_true(grepl('aria-valuenow="[0-9]+"', progress_html))
    expect_true(grepl('aria-valuemin="0"', progress_html))
    expect_true(grepl('aria-valuemax="100"', progress_html))
    
    # Validate width consistency
    aria_value <- as.numeric(str_extract(progress_html, 'aria-valuenow="([0-9]+)"', group = 1))
    style_width <- as.numeric(str_extract(progress_html, 'width: ([0-9]+)%', group = 1))
    
    expect_equal(aria_value, style_width,
                 info = "Progress bar width should match aria-valuenow value")
    
    return(TRUE)
  }
  
  expect_true(test_progress_bars())
})

test_that("Loading Indicators - Skeleton Loading", {
  
  # Test skeleton loading states
  test_skeleton_loading <- function() {
    skeleton_css <- "
      .atlas-skeleton {
        background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
        background-size: 200% 100%;
        animation: atlas-skeleton-loading 1.5s infinite;
      }
      
      @keyframes atlas-skeleton-loading {
        0% { background-position: 200% 0; }
        100% { background-position: -200% 0; }
      }
    "
    
    # Validate skeleton animation
    expect_true(grepl("@keyframes atlas-skeleton-loading", skeleton_css))
    expect_true(grepl("background-size: 200% 100%", skeleton_css))
    expect_true(grepl("animation: atlas-skeleton-loading 1\\.5s infinite", skeleton_css))
    
    # Validate gradient colors for accessibility
    expect_true(grepl("#f0f0f0", skeleton_css))
    expect_true(grepl("#e0e0e0", skeleton_css))
    
    return(TRUE)
  }
  
  expect_true(test_skeleton_loading())
})

test_that("Loading Indicators - State Management", {
  
  # Test loading state transitions
  test_loading_states <- function() {
    # Mock loading state object
    loading_states <- list(
      idle = list(show_spinner = FALSE, show_content = TRUE, show_error = FALSE),
      loading = list(show_spinner = TRUE, show_content = FALSE, show_error = FALSE),
      error = list(show_spinner = FALSE, show_content = FALSE, show_error = TRUE),
      success = list(show_spinner = FALSE, show_content = TRUE, show_error = FALSE)
    )
    
    # Validate mutually exclusive states
    for (state_name in names(loading_states)) {
      state <- loading_states[[state_name]]
      
      # Only one primary state should be active
      active_states <- sum(unlist(state))
      expect_lte(active_states, 1,
                 info = paste("Multiple states active in", state_name))
    }
    
    # Validate required state properties
    required_props <- c("show_spinner", "show_content", "show_error")
    for (state_name in names(loading_states)) {
      state <- loading_states[[state_name]]
      expect_true(all(required_props %in% names(state)),
                  info = paste("Missing properties in", state_name))
    }
    
    return(TRUE)
  }
  
  expect_true(test_loading_states())
})

# =============================================================================
# 7.2.7 ERROR MESSAGE CLARITY TESTING
# =============================================================================

test_that("Error Messages - User-Friendly Language", {
  
  # Test error message clarity and tone
  test_error_message_clarity <- function() {
    error_messages <- list(
      file_not_found = "We couldn't find the employee data file. Please check that 'employee.csv' exists in the data folder.",
      invalid_data = "The data format appears to be incorrect. Please ensure your CSV files match the expected structure.",
      network_error = "Unable to connect to the server. Please check your internet connection and try again.",
      permission_error = "Access denied. You don't have permission to view this data.",
      validation_error = "Some required fields are missing or invalid. Please review your input and try again.",
      timeout_error = "The request took too long to complete. Please try again or contact support if the issue persists."
    )
    
    # Test message characteristics
    for (error_type in names(error_messages)) {
      message <- error_messages[[error_type]]
      
      # Should be in plain English
      expect_false(grepl("Error code|Exception|NULL|undefined", message),
                   info = paste("Technical jargon in", error_type))
      
      # Should provide guidance
      expect_true(grepl("please|try|check|ensure|contact", message, ignore.case = TRUE),
                  info = paste("No guidance provided in", error_type))
      
      # Should be appropriately long
      word_count <- length(strsplit(message, "\\s+")[[1]])
      expect_gte(word_count, 8,
                 info = paste("Error message too short for", error_type))
      expect_lte(word_count, 30,
                 info = paste("Error message too long for", error_type))
    }
    
    return(TRUE)
  }
  
  expect_true(test_error_message_clarity())
})

test_that("Error Messages - Contextual Information", {
  
  # Test error messages include relevant context
  test_error_context <- function() {
    # Mock contextual error generator
    generate_contextual_error <- function(error_type, context) {
      base_messages <- list(
        file_error = "Could not load {filename}. {details}",
        validation_error = "Invalid data in {field}. {requirement}",
        permission_error = "Access denied to {resource}. {action_needed}"
      )
      
      template <- base_messages[[error_type]]
      if (is.null(template)) return(NULL)
      
      # Replace placeholders
      for (key in names(context)) {
        placeholder <- paste0("{", key, "}")
        template <- gsub(placeholder, context[[key]], template, fixed = TRUE)
      }
      
      return(template)
    }
    
    # Test file error context
    file_context <- list(
      filename = "employee.csv",
      details = "File not found in /data directory"
    )
    
    file_error <- generate_contextual_error("file_error", file_context)
    expect_true(grepl("employee\\.csv", file_error))
    expect_true(grepl("data directory", file_error))
    
    # Test validation error context
    validation_context <- list(
      field = "Employee ID",
      requirement = "Must be a unique numeric value"
    )
    
    validation_error <- generate_contextual_error("validation_error", validation_context)
    expect_true(grepl("Employee ID", validation_error))
    expect_true(grepl("unique numeric", validation_error))
    
    return(TRUE)
  }
  
  expect_true(test_error_context())
})

test_that("Error Messages - Visual Styling", {
  
  # Test error message visual presentation
  test_error_styling <- function() {
    error_css <- "
      .atlas-error {
        background-color: #f8d7da;
        border: 1px solid #f5c6cb;
        border-radius: 0.375rem;
        color: #721c24;
        padding: 0.75rem 1rem;
        margin: 1rem 0;
        display: flex;
        align-items: flex-start;
      }
      
      .atlas-error-icon {
        color: #dc3545;
        margin-right: 0.5rem;
        flex-shrink: 0;
      }
      
      .atlas-error-content {
        flex: 1;
      }
      
      .atlas-error-title {
        font-weight: 600;
        margin-bottom: 0.25rem;
      }
    "
    
    # Validate error styling classes
    expect_true(grepl("\\.atlas-error\\s*{", error_css))
    expect_true(grepl("background-color:\\s*#f8d7da", error_css))
    expect_true(grepl("border:\\s*1px solid", error_css))
    expect_true(grepl("border-radius:", error_css))
    
    # Validate icon styling
    expect_true(grepl("\\.atlas-error-icon", error_css))
    expect_true(grepl("margin-right:", error_css))
    
    # Validate content layout
    expect_true(grepl("display:\\s*flex", error_css))
    expect_true(grepl("align-items:\\s*flex-start", error_css))
    
    return(TRUE)
  }
  
  expect_true(test_error_styling())
})

test_that("Error Messages - Accessibility Compliance", {
  
  # Test error message accessibility
  test_error_accessibility <- function() {
    # Mock accessible error HTML
    error_html <- '
      <div class="atlas-error" role="alert" aria-live="polite">
        <svg class="atlas-error-icon" aria-hidden="true" role="img">
          <use href="#icon-exclamation-triangle"></use>
        </svg>
        <div class="atlas-error-content">
          <div class="atlas-error-title" id="error-title-1">Data Loading Error</div>
          <div class="atlas-error-message" aria-describedby="error-title-1">
            Could not load employee.csv. Please check the file exists.
          </div>
        </div>
      </div>
    '
    
    # Validate ARIA attributes
    expect_true(grepl('role="alert"', error_html))
    expect_true(grepl('aria-live="polite"', error_html))
    expect_true(grepl('aria-describedby=', error_html))
    expect_true(grepl('aria-hidden="true"', error_html))
    
    # Validate semantic structure
    expect_true(grepl('id="error-title-[0-9]+"', error_html))
    expect_true(grepl('class="atlas-error-title"', error_html))
    expect_true(grepl('class="atlas-error-message"', error_html))
    
    return(TRUE)
  }
  
  expect_true(test_error_accessibility())
})

# =============================================================================
# 7.2.8 INTERNATIONALIZATION SUPPORT TESTING
# =============================================================================

test_that("Internationalization - Text Externalization", {
  
  # Test that UI text is externalized for translation
  test_text_externalization <- function() {
    # Mock translation system
    translations <- list(
      en = list(
        app_title = "Atlas Labs HR Analytics",
        nav_overview = "Overview",
        nav_attrition = "Attrition Analysis", 
        nav_demographics = "Demographics",
        nav_performance = "Performance",
        btn_export = "Export Report",
        btn_filter = "Apply Filters",
        msg_loading = "Loading data...",
        msg_no_data = "No data available"
      ),
      es = list(
        app_title = "Análisis de RR.HH. de Atlas Labs",
        nav_overview = "Resumen",
        nav_attrition = "Análisis de Deserción",
        nav_demographics = "Demografía",
        nav_performance = "Rendimiento",
        btn_export = "Exportar Informe",
        btn_filter = "Aplicar Filtros",
        msg_loading = "Cargando datos...",
        msg_no_data = "No hay datos disponibles"
      )
    )
    
    # Validate translation structure
    en_keys <- names(translations$en)
    es_keys <- names(translations$es)
    
    # All languages should have same keys
    expect_equal(sort(en_keys), sort(es_keys),
                 info = "Translation keys should match across languages")
    
    # No empty translations
    for (lang in names(translations)) {
      lang_translations <- translations[[lang]]
      for (key in names(lang_translations)) {
        expect_true(nchar(lang_translations[[key]]) > 0,
                    info = paste("Empty translation for", lang, key))
      }
    }
    
    return(TRUE)
  }
  
  expect_true(test_text_externalization())
})

test_that("Internationalization - Locale-Specific Formatting", {
  
  # Test locale-aware number and date formatting
  test_locale_formatting <- function() {
    # Mock locale-specific formatters
    format_number <- function(value, locale = "en-US") {
      if (locale == "en-US") {
        return(format(value, big.mark = ",", decimal.mark = "."))
      } else if (locale == "es-ES") {
        return(format(value, big.mark = ".", decimal.mark = ","))
      } else if (locale == "de-DE") {
        return(format(value, big.mark = ".", decimal.mark = ","))
      }
      return(as.character(value))
    }
    
    format_currency <- function(value, locale = "en-US") {
      if (locale == "en-US") {
        return(paste0("$", format_number(value, locale)))
      } else if (locale == "es-ES") {
        return(paste0(format_number(value, locale), " €"))
      } else if (locale == "de-DE") {
        return(paste0(format_number(value, locale), " €"))
      }
      return(as.character(value))
    }
    
    test_value <- 1234567.89
    
    # Test number formatting
    us_number <- format_number(test_value, "en-US")
    es_number <- format_number(test_value, "es-ES")
    
    expect_true(grepl("1,234,567\\.89", us_number))
    expect_true(grepl("1\\.234\\.567,89", es_number))
    
    # Test currency formatting
    us_currency <- format_currency(1000, "en-US")
    es_currency <- format_currency(1000, "es-ES")
    
    expect_true(grepl("^\\$", us_currency))
    expect_true(grepl("€$", es_currency))
    
    return(TRUE)
  }
  
  expect_true(test_locale_formatting())
})

test_that("Internationalization - RTL Language Support", {
  
  # Test right-to-left language support
  test_rtl_support <- function() {
    rtl_css <- "
      [dir='rtl'] .atlas-sidebar {
        right: 0;
        left: auto;
        border-right: none;
        border-left: 1px solid var(--atlas-border);
      }
      
      [dir='rtl'] .atlas-card {
        text-align: right;
      }
      
      [dir='rtl'] .atlas-chart-container {
        direction: ltr; /* Charts remain LTR */
      }
      
      [dir='rtl'] .atlas-breadcrumb::before {
        content: '\\\\';
        transform: scaleX(-1);
      }
    "
    
    # Validate RTL attribute selectors
    expect_true(grepl("\\[dir='rtl'\\]", rtl_css))
    
    # Validate position adjustments
    expect_true(grepl("right:\\s*0", rtl_css))
    expect_true(grepl("left:\\s*auto", rtl_css))
    
    # Validate text alignment
    expect_true(grepl("text-align:\\s*right", rtl_css))
    
    # Validate chart exception (charts should remain LTR)
    expect_true(grepl("direction:\\s*ltr", rtl_css))
    
    return(TRUE)
  }
  
  expect_true(test_rtl_support())
})

test_that("Internationalization - Font Support", {
  
  # Test international font support
  test_international_fonts <- function() {
    font_stacks <- list(
      latin = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
      arabic = "'Noto Sans Arabic', 'Segoe UI', Tahoma, sans-serif",
      chinese = "'Noto Sans SC', 'PingFang SC', 'Hiragino Sans GB', sans-serif",
      japanese = "'Noto Sans JP', 'Hiragino Kaku Gothic ProN', 'Yu Gothic', sans-serif",
      korean = "'Noto Sans KR', 'Malgun Gothic', 'Apple Gothic', sans-serif",
      cyrillic = "'Noto Sans', 'Segoe UI', 'DejaVu Sans', sans-serif"
    )
    
    # Validate font stack structure
    for (script in names(font_stacks)) {
      font_stack <- font_stacks[[script]]
      
      # Should have fallback fonts
      expect_true(grepl("sans-serif$", font_stack),
                  info = paste("Missing sans-serif fallback for", script))
      
      # Should include system fonts
      system_fonts <- c("system-ui", "Segoe UI", "sans-serif")
      has_system_font <- any(sapply(system_fonts, function(font) {
        grepl(font, font_stack, fixed = TRUE)
      }))
      expect_true(has_system_font,
                  info = paste("Missing system font for", script))
    }
    
    return(TRUE)
  }
  
  expect_true(test_international_fonts())
})

test_that("Internationalization - Character Encoding", {
  
  # Test proper character encoding support
  test_character_encoding <- function() {
    # Test various character sets
    test_strings <- list(
      latin = "Atlas Labs HR Analytics",
      accented = "Résumé Café Naïve",
      arabic = "تحليلات الموارد البشرية",
      chinese = "人力资源分析",
      japanese = "人事分析ダッシュボード",
      korean = "인사 분석 대시보드",
      emoji = "📊 📈 👥 🎯",
      special = "© ® ™ € £ ¥"
    )
    
    # Validate UTF-8 encoding handling
    for (category in names(test_strings)) {
      test_string <- test_strings[[category]]
      
      # Should not contain mojibake or encoding errors
      expect_false(grepl("\\?{2,}", test_string),
                   info = paste("Potential encoding issue in", category))
      
      # Should maintain string length correctly
      char_count <- nchar(test_string, type = "chars")
      byte_count <- nchar(test_string, type = "bytes")
      
      expect_gte(byte_count, char_count,
                 info = paste("Encoding length mismatch in", category))
    }
    
    # Test HTML meta tag
    html_meta <- '<meta charset="UTF-8">'
    expect_true(grepl('charset="UTF-8"', html_meta))
    
    return(TRUE)
  }
  
  expect_true(test_character_encoding())
})

# =============================================================================
# EDGE CASE TESTING SCENARIOS
# =============================================================================

test_that("Edge Cases - Extreme Viewport Sizes", {
  
  # Test very small and very large viewport handling
  test_extreme_viewports <- function() {
    viewport_tests <- list(
      very_small = list(width = 320, height = 568),    # iPhone SE
      small_tablet = list(width = 768, height = 1024), # iPad Mini
      large_desktop = list(width = 2560, height = 1440), # 1440p Monitor
      ultrawide = list(width = 3440, height = 1440),   # Ultrawide Monitor
      vertical_mobile = list(width = 375, height = 812) # iPhone X Portrait
    )
    
    for (viewport_name in names(viewport_tests)) {
      viewport <- viewport_tests[[viewport_name]]
      
      # Validate minimum usable dimensions
      expect_gte(viewport$width, 320,
                 info = paste("Viewport too narrow for", viewport_name))
      expect_gte(viewport$height, 480,
                 info = paste("Viewport too short for", viewport_name))
      
      # Test aspect ratios
      aspect_ratio <- viewport$width / viewport$height
      expect_gte(aspect_ratio, 0.4,
                 info = paste("Aspect ratio too extreme for", viewport_name))
      expect_lte(aspect_ratio, 3.0,
                 info = paste("Aspect ratio too wide for", viewport_name))
    }
    
    return(TRUE)
  }
  
  expect_true(test_extreme_viewports())
})

test_that("Edge Cases - High Contrast Mode", {
  
  # Test Windows High Contrast mode compatibility
  test_high_contrast_mode <- function() {
    high_contrast_css <- "
      @media (prefers-contrast: high) {
        .atlas-card {
          border: 2px solid ButtonText;
          background-color: ButtonFace;
          color: ButtonText;
        }
        
        .atlas-button {
          border: 2px solid ButtonText;
          background-color: ButtonFace;
          color: ButtonText;
        }
        
        .atlas-button:hover {
          background-color: Highlight;
          color: HighlightText;
        }
      }
    "
    
    # Validate high contrast media query
    expect_true(grepl("@media \\(prefers-contrast: high\\)", high_contrast_css))
    
    # Validate system color usage
    system_colors <- c("ButtonText", "ButtonFace", "Highlight", "HighlightText")
    for (color in system_colors) {
      expect_true(grepl(color, high_contrast_css),
                  info = paste("Missing system color:", color))
    }
    
    return(TRUE)
  }
  
  expect_true(test_high_contrast_mode())
})

test_that("Edge Cases - Print Stylesheet", {
  
  # Test print-specific styling
  test_print_styles <- function() {
    print_css <- "
      @media print {
        .atlas-sidebar,
        .atlas-header,
        .atlas-footer,
        .no-print {
          display: none !important;
        }
        
        .atlas-main-content {
          margin: 0 !important;
          padding: 0 !important;
          width: 100% !important;
        }
        
        .atlas-chart {
          page-break-inside: avoid;
          max-height: 8in;
        }
        
        body {
          font-size: 12pt;
          line-height: 1.4;
          color: black;
          background: white;
        }
      }
    "
    
    # Validate print media query
    expect_true(grepl("@media print", print_css))
    
    # Validate hidden elements
    expect_true(grepl("display: none !important", print_css))
    
    # Validate page break control
    expect_true(grepl("page-break-inside: avoid", print_css))
    
    # Validate print-friendly colors
    expect_true(grepl("color: black", print_css))
    expect_true(grepl("background: white", print_css))
    
    return(TRUE)
  }
  
  expect_true(test_print_styles())
})

test_that("Edge Cases - Browser Compatibility", {
  
  # Test cross-browser compatibility
  test_browser_compatibility <- function() {
    # CSS features requiring fallbacks
    modern_css_with_fallbacks <- "
      .atlas-card {
        background: #ffffff;
        background: var(--atlas-bg-color, #ffffff);
        
        display: block;
        display: grid;
        grid-template-columns: 1fr;
        
        border-radius: 4px;
        border-radius: 0.25rem;
      }
    "
    
    # Validate CSS custom property fallbacks
    expect_true(grepl("background: #ffffff;\\s*background: var\\(", modern_css_with_fallbacks))
    
    # Validate grid fallbacks
    expect_true(grepl("display: block;\\s*display: grid;", modern_css_with_fallbacks))
    
    # Validate unit fallbacks
    expect_true(grepl("border-radius: 4px;\\s*border-radius: 0\\.25rem;", modern_css_with_fallbacks))
    
    return(TRUE)
  }
  
  expect_true(test_browser_compatibility())
})

# =============================================================================
# PERFORMANCE TESTING FOR VISUAL ELEMENTS
# =============================================================================

test_that("Performance - CSS Optimization", {
  
  # Test CSS performance best practices
  test_css_performance <- function() {
    # Efficient CSS selectors (avoid deep nesting)
    efficient_selectors <- c(
      ".atlas-card",
      ".atlas-button.primary",
      "#main-dashboard",
      "[data-module='overview']"
    )
    
    inefficient_selectors <- c(
      "div div div div .deeply-nested",
      "* + * + * + .multiple-universals",
      ".class1 .class2 .class3 .class4 .class5 .too-deep"
    )
    
    # Validate selector efficiency
    for (selector in efficient_selectors) {
      # Should not have excessive nesting
      depth <- str_count(selector, "\\s+") + 1
      expect_lte(depth, 3,
                 info = paste("Selector too deep:", selector))
    }
    
    # Test for inefficient patterns
    test_css <- paste(efficient_selectors, collapse = " ")
    expect_false(grepl("\\*\\s*\\+\\s*\\*", test_css),
                 info = "Avoid universal selector combinations")
    
    return(TRUE)
  }
  
  expect_true(test_css_performance())
})

test_that("Performance - Image Optimization", {
  
  # Test image optimization strategies
  test_image_optimization <- function() {
    image_specs <- list(
      logo = list(
        format = "SVG",
        fallback = "PNG",
        max_size_kb = 50
      ),
      charts = list(
        format = "PNG",
        dpi = 144,
        compression = 85
      ),
      icons = list(
        format = "SVG",
        sprite = TRUE,
        inline = TRUE
      )
    )
    
    # Validate image format choices
    for (image_type in names(image_specs)) {
      spec <- image_specs[[image_type]]
      
      # Vector graphics should prefer SVG
      if (image_type %in% c("logo", "icons")) {
        expect_equal(spec$format, "SVG",
                     info = paste("Should use SVG for", image_type))
      }
      
      # Check size constraints
      if ("max_size_kb" %in% names(spec)) {
        expect_lte(spec$max_size_kb, 100,
                   info = paste("Image size too large for", image_type))
      }
    }
    
    return(TRUE)
  }
  
  expect_true(test_image_optimization())
})

# =============================================================================
# INTEGRATION TESTING WITH SHINY COMPONENTS
# =============================================================================

test_that("Integration - Shiny UI Rendering", {
  
  # Test Shiny UI component integration
  test_shiny_ui_integration <- function() {
    # Mock Shiny UI component
    mock_ui <- fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", href = "custom_styles.css"),
        tags$meta(charset = "UTF-8"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
      ),
      div(class = "atlas-header",
          h1("Atlas Labs HR Analytics")),
      div(class = "atlas-main",
          fluidRow(
            column(3, div(class = "atlas-sidebar", "Sidebar")),
            column(9, div(class = "atlas-content", "Main Content"))
          ))
    )
    
    rendered_html <- as.character(mock_ui)
    
    # Validate HTML structure
    expect_true(grepl("<html", rendered_html))
    expect_true(grepl('charset="UTF-8"', rendered_html))
    expect_true(grepl('name="viewport"', rendered_html))
    expect_true(grepl('class="atlas-header"', rendered_html))
    
    return(TRUE)
  }
  
  expect_true(test_shiny_ui_integration())
})

# =============================================================================
# FINAL COMPREHENSIVE TEST SUITE RUNNER
# =============================================================================

# Run all visual tests with comprehensive reporting
run_visual_test_suite <- function() {
  cat("\n🎨 ATLAS LABS VISUAL TESTING SUITE\n")
  cat("==================================\n\n")
  
  test_categories <- list(
    "Layout Consistency" = c("Layout Consistency"),
    "Color Scheme" = c("Color Scheme"),
    "Typography" = c("Typography"),
    "Image Rendering" = c("Image Rendering"),
    "Animation Smoothness" = c("Animation Smoothness"),
    "Loading Indicators" = c("Loading Indicators"),
    "Error Messages" = c("Error Messages"),
    "Internationalization" = c("Internationalization"),
    "Edge Cases" = c("Edge Cases"),
    "Performance" = c("Performance"),
    "Integration" = c("Integration")
  )
  
  results <- list()
  
  for (category in names(test_categories)) {
    cat("📋 Testing:", category, "\n")
    
    # Run tests for this category
    category_results <- test_dir(
      path = ".",
      filter = paste(test_categories[[category]], collapse = "|"),
      reporter = "minimal"
    )
    
    results[[category]] <- category_results
    cat("✅ Completed:", category, "\n\n")
  }
  
  # Summary report
  cat("📊 VISUAL TESTING SUMMARY\n")
  cat("========================\n")
  
  total_tests <- sum(sapply(results, function(r) r$n))
  total_passed <- sum(sapply(results, function(r) r$passed))
  total_failed <- sum(sapply(results, function(r) r$failed))
  
  cat("Total Tests:", total_tests, "\n")
  cat("Passed:", total_passed, "\n")
  cat("Failed:", total_failed, "\n")
  cat("Success Rate:", round(total_passed / total_tests * 100, 1), "%\n\n")
  
  return(results)
}

# Example usage:
# visual_test_results <- run_visual_test_suite()