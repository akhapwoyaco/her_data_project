# =============================================================================
# Atlas Labs HR Analytics Dashboard - Custom Theme Configuration
# File: custom_theme.R
# Author: akhapwoyaco (GitHub)
# Purpose: Professional ggplot2 themes and UI styling for Atlas Labs
# =============================================================================

# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(showtext)) install.packages("showtext")
if (!require(scales)) install.packages("scales")

library(ggplot2)
library(showtext)
library(scales)

# =============================================================================
# COLOR PALETTES
# =============================================================================

# Atlas Labs Brand Colors (Professional & Modern)
ATLAS_COLORS <- list(
  # Primary brand colors
  primary = "#2E86AB",          # Professional Blue
  secondary = "#A23B72",        # Accent Purple
  tertiary = "#F18F01",         # Warm Orange
  
  # Supporting colors
  success = "#28A745",          # Green
  warning = "#FFC107",          # Yellow
  danger = "#DC3545",           # Red
  info = "#17A2B8",            # Cyan
  
  # Neutral colors
  dark = "#2C3E50",            # Dark Blue-Gray
  medium = "#6C757D",          # Medium Gray
  light = "#F8F9FA",           # Light Gray
  white = "#FFFFFF",           # Pure White
  
  # Extended palette for visualizations
  extended = c("#2E86AB", "#A23B72", "#F18F01", "#28A745", "#FFC107", 
               "#DC3545", "#17A2B8", "#6F42C1", "#FD7E14", "#20C997")
)

# Color palettes for different chart types
ATLAS_PALETTES <- list(
  # Categorical data (qualitative)
  categorical = c("#2E86AB", "#A23B72", "#F18F01", "#28A745", "#17A2B8", 
                  "#6F42C1", "#FD7E14", "#DC3545", "#20C997", "#6C757D"),
  
  # Sequential data (quantitative)
  sequential_blue = c("#E3F2FD", "#BBDEFB", "#90CAF9", "#64B5F6", 
                      "#42A5F5", "#2196F3", "#1E88E5", "#1976D2", 
                      "#1565C0", "#0D47A1"),
  
  # Diverging data (positive/negative)
  diverging = c("#DC3545", "#E74C3C", "#F39C12", "#F1C40F", "#FFF", 
                "#3498DB", "#2980B9", "#2E86AB", "#1ABC9C", "#28A745"),
  
  # Attrition specific
  attrition = c("#28A745", "#DC3545"),  # Stay (Green), Leave (Red)
  
  # Performance ratings
  performance = c("#DC3545", "#FFC107", "#F18F01", "#17A2B8", "#28A745"),
  
  # Satisfaction levels
  satisfaction = c("#DC3545", "#E74C3C", "#F39C12", "#2E86AB", "#28A745")
)

# =============================================================================
# TYPOGRAPHY CONFIGURATION
# =============================================================================

# Google Fonts integration
font_add_google("Inter", "inter")         # Modern sans-serif
font_add_google("Source Sans Pro", "source_sans")  # Clean alternative
font_add_google("Roboto", "roboto")       # Professional choice
showtext_auto()

# Font specifications
ATLAS_FONTS <- list(
  title = "inter",              # Main titles
  subtitle = "source_sans",     # Subtitles
  body = "roboto",             # Body text
  axis = "source_sans",        # Axis labels
  caption = "roboto"           # Captions
)

# =============================================================================
# CUSTOM GGPLOT2 THEMES
# =============================================================================

#' Atlas Labs Base Theme
#' Professional theme with consistent styling
theme_atlas_base <- function(base_size = 12, base_family = ATLAS_FONTS$body) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot background
      plot.background = element_rect(fill = ATLAS_COLORS$white, color = NA),
      panel.background = element_rect(fill = ATLAS_COLORS$white, color = NA),
      panel.grid.major = element_line(color = "#E0E0E0", size = 0.3),
      panel.grid.minor = element_line(color = "#F0F0F0", size = 0.2),
      
      # Titles and labels
      plot.title = element_text(
        size = base_size * 1.4,
        color = ATLAS_COLORS$dark,
        face = "bold",
        family = ATLAS_FONTS$title,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = base_size * 1.1,
        color = ATLAS_COLORS$medium,
        family = ATLAS_FONTS$subtitle,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        color = ATLAS_COLORS$medium,
        family = ATLAS_FONTS$caption,
        hjust = 0,
        margin = margin(t = 15)
      ),
      
      # Axes
      axis.title = element_text(
        size = base_size * 1.0,
        color = ATLAS_COLORS$dark,
        family = ATLAS_FONTS$axis,
        face = "bold"
      ),
      axis.text = element_text(
        size = base_size * 0.9,
        color = ATLAS_COLORS$medium,
        family = ATLAS_FONTS$axis
      ),
      axis.line = element_line(color = ATLAS_COLORS$medium, size = 0.5),
      axis.ticks = element_line(color = ATLAS_COLORS$medium, size = 0.3),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(
        size = base_size * 0.9,
        color = ATLAS_COLORS$dark,
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.8,
        color = ATLAS_COLORS$medium
      ),
      legend.box.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      
      # Strips (for facets)
      strip.background = element_rect(
        fill = ATLAS_COLORS$light,
        color = ATLAS_COLORS$medium,
        size = 0.5
      ),
      strip.text = element_text(
        size = base_size * 0.9,
        color = ATLAS_COLORS$dark,
        face = "bold"
      ),
      
      # Margins
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Atlas Executive Theme
#' Clean theme for executive dashboards
theme_atlas_executive <- function(base_size = 14) {
  theme_atlas_base(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "#F0F0F0", size = 0.2),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "top",
      plot.title = element_text(size = base_size * 1.6, face = "bold"),
      plot.subtitle = element_text(size = base_size * 1.2)
    )
}

#' Atlas Detailed Theme
#' Theme for detailed analytical charts
theme_atlas_detailed <- function(base_size = 11) {
  theme_atlas_base(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "#E5E5E5", size = 0.3),
      panel.grid.minor = element_line(color = "#F5F5F5", size = 0.2),
      legend.position = "right",
      strip.background = element_rect(fill = ATLAS_COLORS$primary, color = NA),
      strip.text = element_text(color = ATLAS_COLORS$white, face = "bold")
    )
}

#' Atlas Presentation Theme
#' Theme optimized for presentations and reports
theme_atlas_presentation <- function(base_size = 16) {
  theme_atlas_base(base_size = base_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = ATLAS_COLORS$dark, size = 0.8),
      axis.ticks = element_line(color = ATLAS_COLORS$dark, size = 0.5),
      legend.position = "bottom",
      plot.title = element_text(size = base_size * 1.8, face = "bold"),
      plot.subtitle = element_text(size = base_size * 1.3),
      text = element_text(size = base_size)
    )
}

# =============================================================================
# CUSTOM COLOR SCALES
# =============================================================================

#' Atlas Discrete Color Scale
scale_color_atlas <- function(palette = "categorical", ...) {
  discrete_scale("colour", "atlas", 
                 palette = function(n) ATLAS_PALETTES[[palette]][1:n], ...)
}

#' Atlas Fill Scale
scale_fill_atlas <- function(palette = "categorical", ...) {
  discrete_scale("fill", "atlas", 
                 palette = function(n) ATLAS_PALETTES[[palette]][1:n], ...)
}

#' Atlas Gradient Scale (Continuous)
scale_color_atlas_gradient <- function(low = ATLAS_COLORS$light, 
                                       high = ATLAS_COLORS$primary, ...) {
  scale_color_gradient(low = low, high = high, ...)
}

scale_fill_atlas_gradient <- function(low = ATLAS_COLORS$light, 
                                      high = ATLAS_COLORS$primary, ...) {
  scale_fill_gradient(low = low, high = high, ...)
}

#' Atlas Diverging Scale
scale_color_atlas_diverging <- function(low = ATLAS_COLORS$danger,
                                        mid = ATLAS_COLORS$white,
                                        high = ATLAS_COLORS$primary, ...) {
  scale_color_gradient2(low = low, mid = mid, high = high, ...)
}

scale_fill_atlas_diverging <- function(low = ATLAS_COLORS$danger,
                                       mid = ATLAS_COLORS$white,
                                       high = ATLAS_COLORS$primary, ...) {
  scale_fill_gradient2(low = low, mid = mid, high = high, ...)
}

# =============================================================================
# SHINY UI STYLING FUNCTIONS
# =============================================================================

#' Atlas Value Box Styling
atlas_value_box <- function(value, subtitle, icon = NULL, color = "primary", 
                           width = 3, href = NULL) {
  
  color_code <- switch(color,
    "primary" = ATLAS_COLORS$primary,
    "secondary" = ATLAS_COLORS$secondary,
    "success" = ATLAS_COLORS$success,
    "warning" = ATLAS_COLORS$warning,
    "danger" = ATLAS_COLORS$danger,
    "info" = ATLAS_COLORS$info,
    ATLAS_COLORS$primary
  )
  
  div(
    class = paste0("col-md-", width),
    div(
      class = "atlas-value-box",
      style = paste0(
        "background: linear-gradient(135deg, ", color_code, " 0%, ", 
        adjustcolor(color_code, alpha.f = 0.8), " 100%);",
        "color: white; padding: 20px; border-radius: 10px; text-align: center;",
        "box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: transform 0.2s;",
        "margin-bottom: 20px;"
      ),
      if (!is.null(icon)) {
        div(style = "font-size: 2em; margin-bottom: 10px;", icon)
      },
      div(
        style = "font-size: 2.5em; font-weight: bold; margin-bottom: 5px;",
        value
      ),
      div(
        style = "font-size: 1.1em; opacity: 0.9;",
        subtitle
      ),
      if (!is.null(href)) {
        tags$script(paste0(
          "$('.atlas-value-box').click(function() { window.location = '", 
          href, "'; });"
        ))
      }
    )
  )
}

#' Atlas Card Wrapper
atlas_card <- function(..., title = NULL, status = "primary", 
                      solidHeader = FALSE, collapsible = FALSE) {
  
  header_color <- switch(status,
    "primary" = ATLAS_COLORS$primary,
    "secondary" = ATLAS_COLORS$secondary,
    "success" = ATLAS_COLORS$success,
    "warning" = ATLAS_COLORS$warning,
    "danger" = ATLAS_COLORS$danger,
    ATLAS_COLORS$primary
  )
  
  div(
    class = "atlas-card",
    style = paste0(
      "background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      "margin-bottom: 20px; overflow: hidden;"
    ),
    if (!is.null(title)) {
      div(
        class = "atlas-card-header",
        style = paste0(
          "background: ", if (solidHeader) header_color else ATLAS_COLORS$light, ";",
          "color: ", if (solidHeader) "white" else ATLAS_COLORS$dark, ";",
          "padding: 15px 20px; border-bottom: 1px solid #E0E0E0;",
          "font-weight: bold; font-size: 1.1em;"
        ),
        title
      )
    },
    div(
      class = "atlas-card-body",
      style = "padding: 20px;",
      ...
    )
  )
}

#' Atlas Button Styling
atlas_button <- function(inputId, label, style = "primary", size = "md", 
                        block = FALSE, ...) {
  
  button_color <- switch(style,
    "primary" = ATLAS_COLORS$primary,
    "secondary" = ATLAS_COLORS$secondary,
    "success" = ATLAS_COLORS$success,
    "warning" = ATLAS_COLORS$warning,
    "danger" = ATLAS_COLORS$danger,
    "info" = ATLAS_COLORS$info,
    ATLAS_COLORS$primary
  )
  
  size_class <- switch(size,
    "sm" = "btn-sm",
    "lg" = "btn-lg",
    ""
  )
  
  actionButton(
    inputId = inputId,
    label = label,
    class = paste("atlas-btn", size_class, if (block) "btn-block"),
    style = paste0(
      "background: ", button_color, "; border: none; color: white;",
      "border-radius: 6px; transition: all 0.2s ease;",
      "font-weight: 500; padding: 8px 16px;"
    ),
    ...
  )
}

# =============================================================================
# PLOTTING HELPER FUNCTIONS
# =============================================================================

#' Format numbers for Atlas charts
atlas_format_number <- function(x, prefix = "", suffix = "", accuracy = 1) {
  scales::number(x, prefix = prefix, suffix = suffix, accuracy = accuracy,
                 big.mark = ",")
}

#' Format percentages for Atlas charts
atlas_format_percent <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy)
}

#' Format currency for Atlas charts
atlas_format_currency <- function(x, accuracy = 1) {
  scales::dollar(x, accuracy = accuracy, prefix = "$")
}

#' Add Atlas branding to plots
add_atlas_branding <- function(plot, caption = NULL) {
  default_caption <- "Atlas Labs HR Analytics | github.com/akhapwoyaco"
  final_caption <- if (is.null(caption)) default_caption else paste(caption, "|", default_caption)
  
  plot + labs(caption = final_caption)
}

# =============================================================================
# CSS STYLES FOR SHINY UI
# =============================================================================

#' Generate Atlas CSS styles
atlas_css <- function() {
  tags$style(HTML(paste0("
    /* Atlas Labs Custom CSS */
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&display=swap');
    
    :root {
      --atlas-primary: ", ATLAS_COLORS$primary, ";
      --atlas-secondary: ", ATLAS_COLORS$secondary, ";
      --atlas-tertiary: ", ATLAS_COLORS$tertiary, ";
      --atlas-dark: ", ATLAS_COLORS$dark, ";
      --atlas-medium: ", ATLAS_COLORS$medium, ";
      --atlas-light: ", ATLAS_COLORS$light, ";
    }
    
    body {
      font-family: 'Inter', sans-serif;
      background-color: #F8F9FA;
    }
    
    .atlas-value-box:hover {
      transform: translateY(-2px);
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
    }
    
    .atlas-btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      opacity: 0.9;
    }
    
    .atlas-card:hover {
      box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    }
    
    .sidebar {
      background: linear-gradient(135deg, var(--atlas-primary) 0%, var(--atlas-secondary) 100%);
      color: white;
    }
    
    .sidebar .sidebar-menu > li > a {
      color: rgba(255,255,255,0.9);
      transition: all 0.3s ease;
    }
    
    .sidebar .sidebar-menu > li > a:hover {
      background-color: rgba(255,255,255,0.1);
      color: white;
    }
    
    .content-wrapper {
      background-color: #F8F9FA;
    }
    
    .nav-tabs .nav-link.active {
      background-color: var(--atlas-primary);
      border-color: var(--atlas-primary);
      color: white;
    }
    
    .nav-tabs .nav-link {
      color: var(--atlas-medium);
      border: 1px solid transparent;
      transition: all 0.2s ease;
    }
    
    .nav-tabs .nav-link:hover {
      border-color: var(--atlas-primary);
      color: var(--atlas-primary);
    }
    
    /* Plotly customization */
    .plotly .plotly-graph-div {
      border-radius: 8px;
      overflow: hidden;
    }
    
    /* DataTable customization */
    .dataTables_wrapper .dataTables_filter input {
      border-radius: 6px;
      border: 1px solid #E0E0E0;
    }
    
    .dataTables_wrapper .dataTables_length select {
      border-radius: 6px;
      border: 1px solid #E0E0E0;
    }
    
    /* Loading spinner */
    .atlas-spinner {
      border: 3px solid rgba(46, 134, 171, 0.3);
      border-top: 3px solid var(--atlas-primary);
      border-radius: 50%;
      width: 40px;
      height: 40px;
      animation: spin 1s linear infinite;
      margin: 20px auto;
    }
    
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
    
    /* Responsive design */
    @media (max-width: 768px) {
      .atlas-value-box {
        margin-bottom: 15px;
      }
      
      .atlas-card-body {
        padding: 15px;
      }
    }
  ")))
}

# =============================================================================
# THEME VALIDATION AND SETUP
# =============================================================================

#' Set Atlas theme as default
set_atlas_theme <- function(theme_name = "base") {
  theme_function <- switch(theme_name,
    "base" = theme_atlas_base,
    "executive" = theme_atlas_executive,
    "detailed" = theme_atlas_detailed,
    "presentation" = theme_atlas_presentation,
    theme_atlas_base
  )
  
  theme_set(theme_function())
  
  # Set default color scales
  options(
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill = "viridis",
    ggplot2.discrete.colour = ATLAS_PALETTES$categorical,
    ggplot2.discrete.fill = ATLAS_PALETTES$categorical
  )
}

# =============================================================================
# EXPORT CONFIGURATION
# =============================================================================

# Set default theme on load
set_atlas_theme("base")

# Export main objects for use in modules
ATLAS_THEME_EXPORTS <- list(
  colors = ATLAS_COLORS,
  palettes = ATLAS_PALETTES,
  fonts = ATLAS_FONTS,
  themes = list(
    base = theme_atlas_base,
    executive = theme_atlas_executive,
    detailed = theme_atlas_detailed,
    presentation = theme_atlas_presentation
  ),
  scales = list(
    color = scale_color_atlas,
    fill = scale_fill_atlas,
    gradient = scale_color_atlas_gradient,
    diverging = scale_color_atlas_diverging
  ),
  ui = list(
    value_box = atlas_value_box,
    card = atlas_card,
    button = atlas_button,
    css = atlas_css
  ),
  formatters = list(
    number = atlas_format_number,
    percent = atlas_format_percent,
    currency = atlas_format_currency
  )
)

# Print confirmation message
message("✅ Atlas Labs custom theme loaded successfully!")
message("📊 Available themes: base, executive, detailed, presentation")
message("🎨 Color palettes: categorical, sequential_blue, diverging, attrition, performance, satisfaction")
message("🔧 Use set_atlas_theme('theme_name') to change default theme")