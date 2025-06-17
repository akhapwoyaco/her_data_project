# =============================================================================
# Atlas Labs HR Analytics Dashboard - Main Application
# Developer: akhapwoyaco (GitHub)
# Data Source: https://herdataproject.gumroad.com/l/hr-analytics-tableau
# =============================================================================

# Load core dependencies and configurations
source("global.R")
source("utils.R") 
source("custom_theme.R")

# Load all modules dynamically
purrr::walk(list.files("modules", full.names = TRUE, pattern = "\\.r$"), source)

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- fluidPage(
  # Custom styling and meta tags
  tags$head(
    includeCSS("www/custom_styles.css"),
    # includeScript("www/scripts.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Atlas Labs - HR Analytics Dashboard"),
    tags$link(rel = "icon", type = "image/png", href = "atlas_labs_logo.png")
  ),
  
  # Application header
  div(class = "app-header",
      fluidRow(
        column(3,
               div(class = "logo-container",
                   img(src = "atlas_labs_logo.png", height = "40px", alt = "Atlas Labs"),
                   h4("HR Analytics", class = "header-title")
               )
        ),
        column(6,
               div(class = "header-center",
                   h2("Employee Analytics Dashboard", class = "main-title"),
                   p("Data-driven insights for strategic HR decisions", class = "subtitle")
               )
        ),
        column(3,
               div(class = "header-controls",
                   # Real-time status indicator
                   div(id = "app-status", class = "status-indicator online", "●"),
                   span("Online", class = "status-text"),
                   # Performance monitor toggle
                   actionButton("toggle_performance", "", 
                                icon = icon("tachometer-alt"),
                                class = "btn-sm btn-outline-secondary ms-2",
                                title = "Performance Monitor")
               )
        )
      ),
      hr(class = "header-divider")
  ),
  
  # Main application layout
  div(class = "main-container",
      fluidRow(
        # Sidebar navigation
        column(3,
               div(class = "sidebar-container",
                   sidebarUI("sidebar"),
                   # Logger output panel (collapsible)
                   conditionalPanel(
                     condition = "input.show_logs",
                     div(class = "logger-panel mt-3",
                         h6("System Logs", class = "panel-title"),
                         div(id = "logger-output", class = "logger-display")
                     )
                   )
               )
        ),
        
        # Main content area
        column(9,
               div(class = "content-container",
                   # Performance indicator bar
                   conditionalPanel(
                     condition = "input.toggle_performance % 2 == 1",
                     div(class = "performance-bar",
                         fluidRow(
                           column(3, div(class = "perf-metric", 
                                         span("Memory: "), 
                                         span(id = "memory-usage", "0 MB", class = "metric-value")
                           )),
                           column(3, div(class = "perf-metric",
                                         span("Load Time: "),
                                         span(id = "load-time", "0ms", class = "metric-value")
                           )),
                           column(3, div(class = "perf-metric",
                                         span("Active Module: "),
                                         span(id = "active-module", "Overview", class = "metric-value")
                           )),
                           column(3, div(class = "perf-metric",
                                         span("Users: "),
                                         span(id = "user-count", "1", class = "metric-value")
                           ))
                         )
                     )
                   ),
                   
                   # Dynamic content based on navigation
                   div(id = "main-content",
                       # Overview tab (default)
                       conditionalPanel(
                         condition = "input.selected_tab == 'overview'",
                         overviewUI("overview")
                       ),
                       
                       # Attrition analysis tab
                       conditionalPanel(
                         condition = "input.selected_tab == 'attrition'",
                         attritionUI("attrition")
                       ),
                       
                       # Demographics tab
                       conditionalPanel(
                         condition = "input.selected_tab == 'demographics'",
                         demographicsUI("demographics")
                       ),
                       
                       # Performance tab
                       conditionalPanel(
                         condition = "input.selected_tab == 'performance'",
                         performanceUI("performance")
                       ),

                       # Compensation tab
                       conditionalPanel(
                         condition = "input.selected_tab == 'compensation'",
                         compensationUI("compensation")
                       ),

                       # Satisfaction tab
                       conditionalPanel(
                         condition = "input.selected_tab == 'satisfaction'",
                         satisfactionUI("satisfaction")
                       )#,
                       # 
                       # # Reports tab
                       # conditionalPanel(
                       #   condition = "input.selected_tab == 'reports'",
                       #   reportModuleUI("reports")
                       # )
                   )
               )
        )
      )
  ),
  
  # Application footer
  footerUI("footer"),
  
  # Hidden elements for JavaScript communication
  tags$script("
    // Easter egg: Konami code
    var konamiCode = [38,38,40,40,37,39,37,39,66,65];
    var konamiIndex = 0;
    
    $(document).keydown(function(e) {
      if (e.keyCode === konamiCode[konamiIndex++]) {
        if (konamiIndex === konamiCode.length) {
          $('#easter-egg').show();
          konamiIndex = 0;
        }
      } else {
        konamiIndex = 0;
      }
    });
    
    // Performance monitoring
    setInterval(function() {
      Shiny.setInputValue('performance_check', Math.random());
    }, 5000);
  "),
  
  # Easter egg modal
  div(id = "easter-egg", style = "display: none;",
      div(class = "easter-egg-content",
          h3("🎉 Atlas Labs Developer Mode Activated!"),
          p("You found the secret! Developed with ❤️ by akhapwoyaco"),
          actionButton("close_easter", "Close", class = "btn-primary")
      )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  session_vals <- reactive({
    print(session$token)
    print(session$clientData$url_hostname)
    
    list(
      token = session$token,
      url_hostname = session$clientData$url_hostname
    )
  })
  
  # Initialize logger
  atlas_logger <- AtlasLogger$new()
  atlas_logger$log_info("Application started", "app.R", list(
    timestamp = Sys.time(),
    session_id = 1, #session_vals()$token,
    user_agent = ""#session_vals()$url_hostname
  ))
  
  # Shared reactive values for cross-module communication
  shared_values <- reactiveValues(
    employee_data = NULL,
    performance_data = NULL,
    education_data = NULL,
    filtered_data = NULL,
    selected_filters = list(),
    current_analysis = NULL,
    kpi_metrics = NULL,
    app_state = "loading",
    performance_metrics = list(
      memory_usage = 0,
      load_time = 0,
      active_users = 1
    )
  )
  
  # Performance monitoring
  observe({
    input$performance_check
    
    # Update performance metrics
    memory_info <- gc(verbose = FALSE)
    memory_used <- sum(memory_info[, 2])
    
    shared_values$performance_metrics$memory_usage <- memory_used
    shared_values$performance_metrics$load_time <- as.numeric(Sys.time() - session$started, units = "secs") * 1000
    
    # Update UI elements via JavaScript
    session$sendCustomMessage("updatePerformance", list(
      memory = paste0(round(memory_used, 1), " MB"),
      loadTime = paste0(round(shared_values$performance_metrics$load_time, 0), "ms"),
      activeModule = input$selected_tab %||% "overview"
    ))
  })
  
  # Initialize data loader module
  data_results <- dataLoaderServer("data_loader", atlas_logger)
  
  # Update shared values when data is loaded
  observe({
    if (!is.null(data_results$employee_data())) {
      shared_values$employee_data <- data_results$employee_data()
      shared_values$performance_data <- data_results$performance_data()
      shared_values$education_data <- data_results$education_data()
      shared_values$app_state <- "ready"
      
      atlas_logger$log_info("Data successfully loaded", "app.R", list(
        employee_count = nrow(shared_values$employee_data),
        performance_records = nrow(shared_values$performance_data),
        education_levels = nrow(shared_values$education_data)
      ))
    }
  })
  
  # # Initialize sidebar module for navigation and filtering
  sidebar_results <- sidebarServer("sidebar", shared_values, atlas_logger)
  # 
  # Update filtered data based on sidebar selections
  observe({
    if (!is.null(sidebar_results$filtered_data())) {
      shared_values$filtered_data <- sidebar_results$filtered_data()
      shared_values$selected_filters <- sidebar_results$active_filters()

      atlas_logger$log_info("Filters applied", "app.R", list(
        filters = names(shared_values$selected_filters),
        filtered_count = nrow(shared_values$filtered_data)
      ))
    }
  })
  
  # Initialize analysis modules
  overview_results <- overviewServer("overview", shared_values, atlas_logger)
  attrition_results <- attritionServer("attrition", shared_values, atlas_logger)
  demographics_results <- demographicsServer("demographics", shared_values, atlas_logger)
  performance_results <- performanceServer("performance", shared_values, atlas_logger)
  compensation_results <- compensationServer("compensation", shared_values, atlas_logger)
  satisfaction_results <- satisfactionServer("satisfaction", shared_values, atlas_logger)
  
  # Initialize report module with cross-module data
  # report_data <- reactive({
  #   list(
  #     kpi_metrics = overview_results$kpi_data(),
  #     attrition_analysis = attrition_results$analysis_data(),
  #     performance_data = performance_results$analysis_data(),
  #     satisfaction_metrics = satisfaction_results$analysis_data(),
  #     compensation_analysis = compensation_results$analysis_data(),
  #     demographics_summary = demographics_results$analysis_data(),
  #     selected_filters = shared_values$selected_filters,
  #     analysis_date = Sys.Date(),
  #     data_summary = list(
  #       total_employees = nrow(shared_values$employee_data %||% data.frame()),
  #       data_quality = data_results$quality_metrics()
  #     )
  #   )
  # })
  # 
  # reportModuleServer("reports", report_data, atlas_logger)
  # 
  # # Initialize footer module
  footer_status <- footerServer("footer", logger = atlas_logger)
  
  # Logger output to UI
  output$logger_output <- renderUI({
    if (!is.null(atlas_logger)) {
      logs <- atlas_logger$get_recent_logs(20)
      if (length(logs) > 0) {
        log_items <- map(logs, ~{
          div(class = paste0("log-entry log-", .x$level),
              span(class = "log-time", format(.x$timestamp, "%H:%M:%S")),
              span(class = "log-module", paste0("[", .x$module, "]")),
              span(class = "log-message", .x$message)
          )
        })
        div(log_items)
      } else {
        p("No logs available", class = "text-muted")
      }
    }
  })
  
  # Handle navigation changes
  observe({
    current_tab <- input$selected_tab %||% "overview"
    
    # Log navigation
    atlas_logger$log_info(paste("Navigated to", current_tab), "app.R", list(
      previous_tab = isolate(shared_values$current_analysis),
      timestamp = Sys.time()
    ))
    
    shared_values$current_analysis <- current_tab
  })
  
  # Easter egg handler
  observeEvent(input$close_easter, {
    runjs("$('#easter-egg').hide();")
    atlas_logger$log_info("Easter egg discovered!", "app.R", list(
      user_session = session$token,
      timestamp = Sys.time()
    ))
  })
  
  # Application shutdown logging
  session$onSessionEnded(function() {
    atlas_logger$log_info("Application session ended", "app.R", list(
      session_duration = as.numeric(Sys.time() - session$started, units = "mins"),
      final_memory_usage = shared_values$performance_metrics$memory_usage
    ))
  })
  
  # Error handling
  options(shiny.error = function() {
    atlas_logger$log_error("Application error occurred", "app.R", list(
      error_time = Sys.time(),
      session_info = sessionInfo()
    ))
  })
}

# =============================================================================
# APPLICATION LAUNCH
# =============================================================================

# Run the application
shinyApp(ui = ui, server = server)