# =============================================================================
# Performance Analysis Module - Atlas Labs HR Analytics Dashboard
# File: modules/performance_module.R
# 
# Description: Comprehensive performance analytics module for employee 
#              performance ratings, training analysis, and trend visualization
# 
# Author: akhapwoyaco (GitHub)
# Version: 1.0.0
# =============================================================================

# UI Module ===================================================================
performanceUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Custom CSS for performance module
    tags$head(
      tags$style(HTML("
        .performance-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
          color: white;
          box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);
        }
        .metric-value {
          font-size: 2.5em;
          font-weight: bold;
          text-align: center;
        }
        .metric-label {
          font-size: 1.1em;
          text-align: center;
          opacity: 0.9;
        }
        .performance-section {
          margin: 25px 0;
          padding: 20px;
          background: #f8f9fa;
          border-radius: 10px;
          border-left: 4px solid #667eea;
        }
      "))
    ),
    
    # Header Section
    div(class = "performance-section",
      fluidRow(
        column(12,
          h2("Performance Analytics", 
             style = "color: #2c3e50; margin-bottom: 20px;"),
          p("Comprehensive analysis of employee performance ratings, training utilization, and development trends.",
            style = "color: #666; font-size: 1.1em;")
        )
      )
    ),
    
    # KPI Cards Row
    fluidRow(
      column(3,
        div(class = "performance-card",
          div(class = "metric-value", textOutput(ns("avg_self_rating"))),
          div(class = "metric-label", "Avg Self Rating")
        )
      ),
      column(3,
        div(class = "performance-card",
          div(class = "metric-value", textOutput(ns("avg_manager_rating"))),
          div(class = "metric-label", "Avg Manager Rating")
        )
      ),
      column(3,
        div(class = "performance-card",
          div(class = "metric-value", textOutput(ns("training_utilization"))),
          div(class = "metric-label", "Training Utilization")
        )
      ),
      column(3,
        div(class = "performance-card",
          div(class = "metric-value", textOutput(ns("high_performers"))),
          div(class = "metric-label", "High Performers")
        )
      )
    ),
    
    # Filters Section
    div(class = "performance-section",
      fluidRow(
        column(4,
          selectInput(ns("dept_filter"), "Filter by Department:",
                     choices = NULL, multiple = TRUE)
        ),
        column(4,
          selectInput(ns("role_filter"), "Filter by Job Role:",
                     choices = NULL, multiple = TRUE)
        ),
        column(4,
          dateRangeInput(ns("date_range"), "Review Date Range:",
                        start = NULL, end = NULL)
        )
      )
    ),
    
    # Main Visualizations
    fluidRow(
      # Performance Rating Distribution
      column(6,
        div(class = "performance-section",
          h4("Performance Rating Distribution", style = "color: #34495e;"),
          plotlyOutput(ns("rating_distribution"), height = "400px")
        )
      ),
      
      # Manager vs Self Rating Comparison
      column(6,
        div(class = "performance-section",
          h4("Manager vs Self Rating Analysis", style = "color: #34495e;"),
          plotlyOutput(ns("rating_comparison"), height = "400px")
        )
      )
    ),
    
    fluidRow(
      # Training Opportunities Analysis
      column(6,
        div(class = "performance-section",
          h4("Training Opportunities Analysis", style = "color: #34495e;"),
          plotlyOutput(ns("training_analysis"), height = "400px")
        )
      ),
      
      # Performance Trends Over Time
      column(6,
        div(class = "performance-section",
          h4("Performance Trends Over Time", style = "color: #34495e;"),
          plotlyOutput(ns("performance_trends"), height = "400px")
        )
      )
    ),
    
    # Detailed Analysis Section
    div(class = "performance-section",
      fluidRow(
        column(12,
          h4("Performance Insights & Recommendations", style = "color: #34495e;"),
          verbatimTextOutput(ns("performance_insights"))
        )
      )
    ),
    
    # Data Table
    div(class = "performance-section",
      fluidRow(
        column(12,
          h4("Detailed Performance Data", style = "color: #34495e;"),
          DT::dataTableOutput(ns("performance_table"))
        )
      )
    )
  )
}

# Server Module ===============================================================
performanceServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Log module initialization
    logger$log_info("Performance module initialized", "performance_module")
    
    # Reactive data processing
    performance_data <- reactive({
      req(data())
      
      start_time <- Sys.time()
      logger$log_info("Processing performance data", "performance_module")
      
      # Merge employee and performance data
      merged_data <- data()$employees %>%
        left_join(data()$performance_ratings, by = "EmployeeID") %>%
        left_join(data()$education_levels, by = c("Education" = "Education Level ID"))
      
      # Apply filters
      filtered_data <- merged_data
      
      if (!is.null(input$dept_filter) && length(input$dept_filter) > 0) {
        filtered_data <- filtered_data %>%
          filter(Department %in% input$dept_filter)
      }
      
      if (!is.null(input$role_filter) && length(input$role_filter) > 0) {
        filtered_data <- filtered_data %>%
          filter(JobRole %in% input$role_filter)
      }
      
      if (!is.null(input$date_range)) {
        filtered_data <- filtered_data %>%
          filter(ReviewDate >= input$date_range[1] & ReviewDate <= input$date_range[2])
      }
      
      # Log performance metrics
      end_time <- Sys.time()
      processing_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
      logger$log_info(paste("Performance data processed in", processing_time, "seconds"), 
                     "performance_module")
      
      filtered_data
    })
    
    # Initialize filter choices
    observe({
      req(data())
      
      dept_choices <- sort(unique(data()$employees$Department))
      role_choices <- sort(unique(data()$employees$JobRole))
      
      updateSelectInput(session, "dept_filter", choices = dept_choices)
      updateSelectInput(session, "role_filter", choices = role_choices)
      
      # Set default date range
      if (!is.null(data()$performance_ratings$ReviewDate)) {
        date_range <- range(data()$performance_ratings$ReviewDate, na.rm = TRUE)
        updateDateRangeInput(session, "date_range", 
                           start = date_range[1], end = date_range[2])
      }
    })
    
    # Calculate performance metrics
    performance_metrics <- reactive({
      req(performance_data())
      calculate_performance_metrics(performance_data())
    })
    
    # KPI Outputs
    output$avg_self_rating <- renderText({
      metrics <- performance_metrics()
      round(metrics$avg_self_rating, 1)
    })
    
    output$avg_manager_rating <- renderText({
      metrics <- performance_metrics()
      round(metrics$avg_manager_rating, 1)
    })
    
    output$training_utilization <- renderText({
      metrics <- performance_metrics()
      paste0(round(metrics$training_utilization * 100, 1), "%")
    })
    
    output$high_performers <- renderText({
      metrics <- performance_metrics()
      paste0(round(metrics$high_performers_pct * 100, 1), "%")
    })
    
    # Performance Rating Distribution
    output$rating_distribution <- renderPlotly({
      req(performance_data())
      
      p <- performance_data() %>%
        select(SelfRating, ManagerRating) %>%
        pivot_longer(cols = everything(), names_to = "RatingType", values_to = "Rating") %>%
        filter(!is.na(Rating)) %>%
        mutate(RatingType = case_when(
          RatingType == "SelfRating" ~ "Self Rating",
          RatingType == "ManagerRating" ~ "Manager Rating"
        )) %>%
        ggplot(aes(x = factor(Rating), fill = RatingType)) +
        geom_bar(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("Self Rating" = "#667eea", "Manager Rating" = "#764ba2")) +
        atlas_theme() +
        labs(title = "Performance Rating Distribution",
             x = "Rating Score", y = "Count of Employees",
             fill = "Rating Type") +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("fill", "x", "y")) %>%
        layout(showlegend = TRUE)
    })
    
    # Manager vs Self Rating Comparison
    output$rating_comparison <- renderPlotly({
      req(performance_data())
      
      p <- performance_data() %>%
        filter(!is.na(SelfRating) & !is.na(ManagerRating)) %>%
        ggplot(aes(x = SelfRating, y = ManagerRating)) +
        geom_point(alpha = 0.6, color = "#667eea", size = 2) +
        geom_smooth(method = "lm", color = "#764ba2", se = TRUE) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", alpha = 0.7) +
        scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
        scale_y_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
        atlas_theme() +
        labs(title = "Manager vs Self Rating Correlation",
             x = "Self Rating", y = "Manager Rating",
             caption = "Red line = Perfect Agreement")
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(showlegend = FALSE)
    })
    
    # Training Analysis
    output$training_analysis <- renderPlotly({
      req(performance_data())
      
      training_summary <- performance_data() %>%
        filter(!is.na(TrainingOpportunitiesWithinYear) & !is.na(TrainingOpportunitiesTaken)) %>%
        group_by(Department) %>%
        summarise(
          Offered = mean(TrainingOpportunitiesWithinYear, na.rm = TRUE),
          Taken = mean(TrainingOpportunitiesTaken, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Offered, Taken), names_to = "Type", values_to = "Count")
      
      p <- training_summary %>%
        ggplot(aes(x = reorder(Department, Count), y = Count, fill = Type)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("Offered" = "#667eea", "Taken" = "#764ba2")) +
        coord_flip() +
        atlas_theme() +
        labs(title = "Training Opportunities by Department",
             x = "Department", y = "Average Training Count",
             fill = "Training Type")
      
      ggplotly(p, tooltip = c("fill", "y", "x"))
    })
    
    # Performance Trends Over Time
    output$performance_trends <- renderPlotly({
      req(performance_data())
      
      trend_data <- performance_data() %>%
        filter(!is.na(ReviewDate) & !is.na(ManagerRating)) %>%
        mutate(YearMonth = floor_date(ReviewDate, "month")) %>%
        group_by(YearMonth) %>%
        summarise(
          AvgRating = mean(ManagerRating, na.rm = TRUE),
          Count = n(),
          .groups = "drop"
        ) %>%
        filter(Count >= 5)  # Only include months with sufficient data
      
      p <- trend_data %>%
        ggplot(aes(x = YearMonth, y = AvgRating)) +
        geom_line(color = "#667eea", size = 1.2) +
        geom_point(color = "#764ba2", size = 2) +
        scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
        atlas_theme() +
        labs(title = "Performance Rating Trends Over Time",
             x = "Review Period", y = "Average Manager Rating")
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # Performance Insights
    output$performance_insights <- renderText({
      req(performance_metrics())
      
      metrics <- performance_metrics()
      insights <- analyze_rating_patterns(performance_data())
      
      paste(
        "=== PERFORMANCE ANALYSIS INSIGHTS ===\n",
        sprintf("• Average Self Rating: %.1f/5.0", metrics$avg_self_rating), "\n",
        sprintf("• Average Manager Rating: %.1f/5.0", metrics$avg_manager_rating), "\n",
        sprintf("• Rating Correlation: %.3f", insights$rating_correlation), "\n",
        sprintf("• Training Utilization Rate: %.1f%%", metrics$training_utilization * 100), "\n",
        sprintf("• High Performers (4+ rating): %.1f%%", metrics$high_performers_pct * 100), "\n\n",
        "=== KEY FINDINGS ===\n",
        if (insights$self_rating_bias > 0.2) "• Employees tend to rate themselves higher than managers\n" else "",
        if (metrics$training_utilization < 0.5) "• Low training utilization - consider engagement strategies\n" else "",
        if (insights$rating_correlation < 0.6) "• Moderate correlation between self and manager ratings\n" else "",
        "\n=== RECOMMENDATIONS ===\n",
        "• Implement regular calibration sessions between managers\n",
        "• Develop targeted training programs for underperforming areas\n",
        "• Consider 360-degree feedback to complement manager ratings\n",
        "• Focus on departments with low training utilization rates"
      )
    })
    
    # Performance Data Table
    output$performance_table <- DT::renderDataTable({
      req(performance_data())
      
      table_data <- performance_data() %>%
        select(
          Employee = paste(FirstName, LastName),
          Department, JobRole, ReviewDate,
          SelfRating, ManagerRating,
          TrainingOffered = TrainingOpportunitiesWithinYear,
          TrainingTaken = TrainingOpportunitiesTaken,
          JobSatisfaction, EnvironmentSatisfaction
        ) %>%
        filter(!is.na(SelfRating) | !is.na(ManagerRating))
      
      DT::datatable(
        table_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = 4:9)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatRound(columns = c("SelfRating", "ManagerRating", "JobSatisfaction", "EnvironmentSatisfaction"), digits = 1)
    })
    
    # Return reactive values for other modules
    list(
      performance_data = performance_data,
      performance_metrics = performance_metrics
    )
  })
}

# Helper Functions ============================================================

#' Calculate comprehensive performance metrics
#' @param data Merged performance data
#' @return List of calculated metrics
calculate_performance_metrics <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      avg_self_rating = 0,
      avg_manager_rating = 0,
      training_utilization = 0,
      high_performers_pct = 0
    ))
  }
  
  metrics <- list(
    avg_self_rating = mean(data$SelfRating, na.rm = TRUE),
    avg_manager_rating = mean(data$ManagerRating, na.rm = TRUE),
    training_utilization = mean(
      data$TrainingOpportunitiesTaken / pmax(data$TrainingOpportunitiesWithinYear, 1), 
      na.rm = TRUE
    ),
    high_performers_pct = mean(data$ManagerRating >= 4, na.rm = TRUE)
  )
  
  # Handle NaN values
  metrics <- lapply(metrics, function(x) ifelse(is.nan(x), 0, x))
  
  return(metrics)
}

#' Analyze rating patterns and correlations
#' @param data Performance data
#' @return List of pattern analysis results
analyze_rating_patterns <- function(data) {
  if (nrow(data) == 0 || sum(!is.na(data$SelfRating) & !is.na(data$ManagerRating)) < 3) {
    return(list(
      rating_correlation = 0,
      self_rating_bias = 0,
      department_variance = 0
    ))
  }
  
  # Filter for complete cases
  complete_ratings <- data %>%
    filter(!is.na(SelfRating) & !is.na(ManagerRating))
  
  if (nrow(complete_ratings) < 3) {
    return(list(
      rating_correlation = 0,
      self_rating_bias = 0,
      department_variance = 0
    ))
  }
  
  # Calculate correlation
  correlation <- cor(complete_ratings$SelfRating, complete_ratings$ManagerRating, 
                    use = "complete.obs")
  
  # Calculate self-rating bias (positive = self-rating higher)
  bias <- mean(complete_ratings$SelfRating - complete_ratings$ManagerRating, na.rm = TRUE)
  
  # Calculate department variance in ratings
  dept_variance <- data %>%
    filter(!is.na(ManagerRating)) %>%
    group_by(Department) %>%
    summarise(dept_avg = mean(ManagerRating, na.rm = TRUE), .groups = "drop") %>%
    pull(dept_avg) %>%
    var(na.rm = TRUE)
  
  list(
    rating_correlation = ifelse(is.na(correlation), 0, correlation),
    self_rating_bias = ifelse(is.na(bias), 0, bias),
    department_variance = ifelse(is.na(dept_variance), 0, dept_variance)
  )
}