# =============================================================================
# Atlas Labs HR Analytics Dashboard - Global Configuration
# Developer: akhapwoyaco (GitHub)
# Data Source: https://herdataproject.gumroad.com/l/hr-analytics-tableau
# =============================================================================

# =============================================================================
# CORE LIBRARIES
# =============================================================================

# Essential Shiny ecosystem
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

# Data manipulation and analysis
library(tidyverse)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(scales)

# Visualization libraries
library(plotly)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(viridis)

# Data presentation
library(DT)
library(knitr)
library(kableExtra)
library(formattable)

# Advanced functionality
library(R6)
library(jsonlite)
library(htmltools)
library(htmlwidgets)

# Performance and utilities
library(memoise)
library(future)
library(promises)

# Report generation
library(rmarkdown)
library(flexdashboard)

# =============================================================================
# ATLAS LABS BRAND CONFIGURATION
# =============================================================================

# Primary brand colors (Professional palette inspired by Tableau)
ATLAS_COLORS <- c(
  primary = "#1f4e79",      # Deep blue (primary brand)
  secondary = "#2e86ab",    # Medium blue (secondary)
  accent = "#a23b72",       # Magenta accent
  success = "#2ecc71",      # Green (positive metrics)
  warning = "#f39c12",      # Orange (warnings)
  danger = "#e74c3c",       # Red (negative metrics)
  info = "#3498db",         # Light blue (information)
  light = "#ecf0f1",        # Light gray (backgrounds)
  dark = "#2c3e50",         # Dark gray (text)
  white = "#ffffff"         # Pure white
)

# Extended color palette for visualizations
ATLAS_PALETTE <- c(
  "#1f4e79", "#2e86ab", "#a23b72", "#f18f01", "#c73e1d",
  "#5d737e", "#64a6bd", "#90a955", "#ee6c4d", "#3d5a80",
  "#98c1d9", "#ee6c4d", "#293241", "#f2cc8f", "#81b29a"
)

# Demographic analysis colors
DEMOGRAPHIC_COLORS <- c(
  male = "#2e86ab",
  female = "#a23b72", 
  other = "#5d737e",
  white = "#1f4e79",
  black = "#f18f01",
  hispanic = "#c73e1d",
  asian = "#64a6bd",
  other_ethnicity = "#90a955"
)

# Department colors
DEPARTMENT_COLORS <- c(
  "Human Resources" = "#a23b72",
  "Sales" = "#2e86ab", 
  "Technology" = "#1f4e79",
  "Marketing" = "#f18f01",
  "Finance" = "#2ecc71",
  "Operations" = "#c73e1d",
  "Legal" = "#5d737e"
)

# =============================================================================
# APPLICATION CONFIGURATION
# =============================================================================

# Application metadata
APP_VERSION <- "1.0.0"
APP_NAME <- "Atlas Labs HR Analytics Dashboard"
APP_DESCRIPTION <- "Data-driven insights for strategic HR decisions"
DEVELOPER_CREDIT <- "akhapwoyaco"
GITHUB_URL <- "https://github.com/akhapwoyaco"
DATA_SOURCE_URL <- "https://herdataproject.gumroad.com/l/hr-analytics-tableau"

# Performance settings
PERFORMANCE_CONFIG <- list(
  max_file_size = 50 * 1024^2,  # 50MB max file size
  cache_timeout = 3600,         # 1 hour cache timeout
  log_retention_days = 30,      # Keep logs for 30 days
  max_concurrent_users = 100,   # Maximum concurrent users
  memory_threshold = 1024,      # Memory warning threshold (MB)
  refresh_interval = 5000       # Performance refresh interval (ms)
)

# Data validation rules
DATA_VALIDATION <- list(
  required_columns = list(
    employee = c("EmployeeID", "FirstName", "LastName", "Age", "Department", 
                "JobRole", "Salary", "Attrition", "HireDate"),
    performance = c("PerformanceID", "EmployeeID", "ReviewDate", 
                   "JobSatisfaction", "WorkLifeBalance"),
    education = c("Education Level ID", "Education Level")
  ),
  date_format = "%Y-%m-%d",
  salary_range = c(30000, 500000),
  age_range = c(18, 80),
  rating_range = c(1, 5)
)

# =============================================================================
# UI/UX CONFIGURATION
# =============================================================================

# Layout constants
UI_CONFIG <- list(
  sidebar_width = 3,
  content_width = 9,
  header_height = "80px",
  footer_height = "60px",
  card_min_height = "300px",
  chart_height = "400px",
  table_page_length = 25
)

# Animation and transition settings
ANIMATION_CONFIG <- list(
  fade_duration = 300,
  slide_duration = 400,
  chart_animation = 1000,
  loading_delay = 500
)

# Responsive breakpoints
BREAKPOINTS <- list(
  mobile = 768,
  tablet = 1024,
  desktop = 1200,
  large = 1440
)

# =============================================================================
# LOGGING CONFIGURATION
# =============================================================================

# Log levels and settings
LOG_CONFIG <- list(
  levels = c("DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"),
  console_colors = list(
    DEBUG = "\033[36m",    # Cyan
    INFO = "\033[32m",     # Green
    WARNING = "\033[33m",  # Yellow
    ERROR = "\033[31m",    # Red
    CRITICAL = "\033[35m", # Magenta
    RESET = "\033[0m"      # Reset
  ),
  max_log_entries = 1000,
  log_file_path = "logs/atlas_hr_analytics.log",
  performance_log_interval = 10  # seconds
)

# =============================================================================
# DATA PROCESSING CONFIGURATION
# =============================================================================

# Analysis constants
ANALYSIS_CONFIG <- list(
  attrition_threshold = 0.15,     # 15% attrition rate threshold
  high_performer_threshold = 4.0,  # Performance rating threshold
  satisfaction_threshold = 3.5,    # Satisfaction threshold
  tenure_categories = c(0, 1, 3, 5, 10, Inf),
  tenure_labels = c("0-1 years", "1-3 years", "3-5 years", "5-10 years", "10+ years"),
  salary_percentiles = c(0.25, 0.5, 0.75, 0.9, 0.95),
  age_groups = c(18, 25, 35, 45, 55, 80),
  age_labels = c("18-24", "25-34", "35-44", "45-54", "55+")
)

# Statistical significance levels
STATS_CONFIG <- list(
  alpha = 0.05,              # Significance level
  confidence_level = 0.95,   # Confidence interval
  min_sample_size = 30,      # Minimum sample for analysis
  correlation_threshold = 0.3 # Minimum correlation to report
)

# =============================================================================
# VISUALIZATION CONFIGURATION
# =============================================================================

# Chart defaults
CHART_CONFIG <- list(
  default_theme = "minimal",
  font_family = "Arial, sans-serif",
  title_size = 16,
  axis_title_size = 12,
  axis_text_size = 10,
  legend_position = "bottom",
  grid_color = "#f0f0f0",
  background_color = "#ffffff"
)

# Plotly configuration
PLOTLY_CONFIG <- list(
  displayModeBar = TRUE,
  modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
  displaylogo = FALSE,
  toImageButtonOptions = list(
    format = "png",
    filename = "atlas_labs_chart",
    height = 600,
    width = 800,
    scale = 2
  )
)

# =============================================================================
# FILE PATHS AND DIRECTORIES
# =============================================================================

# Application directories
APP_DIRS <- list(
  data = "data/",
  modules = "modules/",
  www = "www/",
  reports = "reports/",
  logs = "logs/",
  temp = "temp/"
)

# Ensure required directories exist
purrr::walk(APP_DIRS, ~{
  if (!dir.exists(.x)) {
    dir.create(.x, recursive = TRUE, showWarnings = FALSE)
  }
})

# Data file paths
DATA_FILES <- list(
  employee = file.path(APP_DIRS$data, "employee.csv"),
  performance = file.path(APP_DIRS$data, "performance_rating.csv"),
  education = file.path(APP_DIRS$data, "education_level.csv")
)

# =============================================================================
# UTILITY FUNCTIONS (Global Scope)
# =============================================================================

# Color utility functions
get_atlas_color <- function(name) {
  ATLAS_COLORS[[name]] %||% ATLAS_COLORS$primary
}

get_status_color <- function(value, thresholds = c(low = 0.3, high = 0.7)) {
  case_when(
    value < thresholds[["low"]] ~ ATLAS_COLORS$danger,
    value > thresholds[["high"]] ~ ATLAS_COLORS$success,
    TRUE ~ ATLAS_COLORS$warning
  )
}

# Format utility functions
format_currency <- function(x) {
  scales::dollar(x, accuracy = 1, scale = 1e-3, suffix = "K")
}

format_percentage <- function(x, digits = 1) {
  scales::percent(x, accuracy = 10^(-digits))
}

format_number <- function(x, digits = 0) {
  scales::comma(x, accuracy = 10^(-digits))
}

# Validation functions
is_valid_employee_id <- function(id) {
  !is.na(id) & is.numeric(id) & id > 0
}

is_valid_date <- function(date_string) {
  !is.na(lubridate::ymd(date_string))
}

is_valid_rating <- function(rating) {
  !is.na(rating) & rating >= ANALYSIS_CONFIG$rating_range[1] & 
  rating <= ANALYSIS_CONFIG$rating_range[2]
}

# =============================================================================
# SHINY OPTIONS AND SETTINGS
# =============================================================================

# Shiny application options
options(
  shiny.maxRequestSize = PERFORMANCE_CONFIG$max_file_size,
  shiny.usecairo = TRUE,
  shiny.useragg = TRUE,
  shiny.autoreload = FALSE,
  shiny.error = function() {
    cat(paste0(LOG_CONFIG$console_colors$ERROR, 
               "[ERROR] Shiny application error occurred at ", 
               Sys.time(), LOG_CONFIG$console_colors$RESET, "\n"))
  }
)

# DT options for consistent table styling
options(
  DT.options = list(
    pageLength = UI_CONFIG$table_page_length,
    lengthMenu = c(10, 25, 50, 100),
    dom = 'Bfrtip',
    scrollX = TRUE,
    responsive = TRUE,
    language = list(
      search = "Filter records:",
      lengthMenu = "Show _MENU_ entries per page"
    )
  )
)

# ggplot2 theme defaults
theme_set(theme_minimal(base_family = CHART_CONFIG$font_family))

# =============================================================================
# DEVELOPMENT AND DEBUG SETTINGS
# =============================================================================

# Development flags
DEV_CONFIG <- list(
  debug_mode = FALSE,
  show_performance_panel = TRUE,
  enable_easter_eggs = TRUE,
  mock_data_mode = FALSE,
  verbose_logging = TRUE
)

# Console startup message
cat(paste0(
  LOG_CONFIG$console_colors$INFO,
  "🚀 ", APP_NAME, " v", APP_VERSION, " initialized successfully!\n",
  "📊 Ready for HR analytics and insights\n",
  "👨‍💻 Developed by: ", DEVELOPER_CREDIT, "\n",
  "🔗 Data source: ", DATA_SOURCE_URL, "\n",
  LOG_CONFIG$console_colors$RESET
))

# =============================================================================
# GLOBAL ENVIRONMENT CLEANUP
# =============================================================================

# Clean up temporary variables (keep only essential globals)
rm(list = setdiff(ls(), c(
  # Core configuration objects
  "ATLAS_COLORS", "ATLAS_PALETTE", "DEMOGRAPHIC_COLORS", "DEPARTMENT_COLORS",
  "APP_VERSION", "APP_NAME", "APP_DESCRIPTION", "DEVELOPER_CREDIT", 
  "GITHUB_URL", "DATA_SOURCE_URL",
  
  # Configuration lists
  "PERFORMANCE_CONFIG", "DATA_VALIDATION", "UI_CONFIG", "ANIMATION_CONFIG",
  "BREAKPOINTS", "LOG_CONFIG", "ANALYSIS_CONFIG", "STATS_CONFIG", 
  "CHART_CONFIG", "PLOTLY_CONFIG", "APP_DIRS", "DATA_FILES", "DEV_CONFIG",
  
  # Utility functions
  "get_atlas_color", "get_status_color", "format_currency", 
  "format_percentage", "format_number", "is_valid_employee_id",
  "is_valid_date", "is_valid_rating"
)))

# Memory optimization
gc(verbose = FALSE)