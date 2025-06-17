# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPENSATION MODULE
# File: modules/compensation_module.R
# Author: akhapwoyaco (GitHub)
# Description: Comprehensive compensation analysis with pay equity insights
# =============================================================================

# UI Function ----------------------------------------------------------------
compensationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for compensation module
    tags$style(HTML("
      .comp-card { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }
      .equity-alert { border-left: 4px solid #e74c3c; background: #fdf2f2; }
      .equity-good { border-left: 4px solid #27ae60; background: #f0fdf4; }
      .salary-band { background: rgba(102, 126, 234, 0.1); border-radius: 8px; padding: 10px; }
    ")),
    
    fluidRow(
      # Filter Controls
      column(12,
        wellPanel(
          style = "background: #f8f9fa; border: 1px solid #dee2e6;",
          h4("Compensation Analysis Filters", style = "color: #2c3e50; margin-top: 0;"),
          fluidRow(
            column(3, selectInput(ns("dept_filter"), "Department:", choices = NULL, multiple = TRUE)),
            column(3, selectInput(ns("gender_filter"), "Gender:", choices = NULL, multiple = TRUE)),
            column(3, selectInput(ns("role_filter"), "Job Role:", choices = NULL, multiple = TRUE)),
            column(3, br(), actionButton(ns("reset_filters"), "Reset Filters", class = "btn-outline-secondary"))
          )
        )
      )
    ),
    
    # KPI Cards Row
    fluidRow(
      column(3, div(class = "comp-card text-white text-center p-3 rounded mb-3",
        h3(textOutput(ns("avg_salary")), style = "margin: 0;"),
        p("Average Salary", style = "margin: 0; opacity: 0.9;")
      )),
      column(3, div(class = "comp-card text-white text-center p-3 rounded mb-3",
        h3(textOutput(ns("median_salary")), style = "margin: 0;"),
        p("Median Salary", style = "margin: 0; opacity: 0.9;")
      )),
      column(3, div(class = "comp-card text-white text-center p-3 rounded mb-3",
        h3(textOutput(ns("pay_gap")), style = "margin: 0;"),
        p("Gender Pay Gap", style = "margin: 0; opacity: 0.9;")
      )),
      column(3, div(class = "comp-card text-white text-center p-3 rounded mb-3",
        h3(textOutput(ns("stock_coverage")), style = "margin: 0;"),
        p("Stock Option Coverage", style = "margin: 0; opacity: 0.9;")
      ))
    ),
    
    # Main Analysis Tabs
    tabsetPanel(
      id = ns("comp_tabs"),
      type = "pills",
      
      # Salary Distribution Tab
      tabPanel("Salary Distribution",
        fluidRow(
          column(8, 
            div(style = "height: 500px;", plotlyOutput(ns("salary_dist_plot"), height = "100%"))
          ),
          column(4,
            h4("Salary Bands", style = "color: #34495e;"),
            div(id = ns("salary_bands_container"), uiOutput(ns("salary_bands"))),
            br(),
            h4("Distribution Insights", style = "color: #34495e;"),
            uiOutput(ns("salary_insights"))
          )
        )
      ),
      
      # Pay Equity Tab  
      tabPanel("Pay Equity Analysis",
        fluidRow(
          column(6, div(style = "height: 400px;", plotlyOutput(ns("pay_equity_plot"), height = "100%"))),
          column(6, div(style = "height: 400px;", plotlyOutput(ns("pay_gap_trends"), height = "100%")))
        ),
        fluidRow(
          column(12,
            h4("Pay Equity Alerts", style = "color: #34495e; margin-top: 20px;"),
            uiOutput(ns("equity_alerts"))
          )
        )
      ),
      
      # Stock Options Tab
      tabPanel("Stock Options",
        fluidRow(
          column(8, div(style = "height: 450px;", plotlyOutput(ns("stock_analysis"), height = "100%"))),
          column(4,
            h4("Stock Option Levels", style = "color: #34495e;"),
            DT::dataTableOutput(ns("stock_summary")),
            br(),
            uiOutput(ns("stock_insights"))
          )
        )
      ),
      
      # Performance Correlation Tab
      tabPanel("Performance vs Compensation",
        fluidRow(
          column(12, div(style = "height: 500px;", plotlyOutput(ns("perf_comp_correlation"), height = "100%")))
        ),
        fluidRow(
          column(6, 
            h4("Correlation Analysis", style = "color: #34495e;"),
            verbatimTextOutput(ns("correlation_stats"))
          ),
          column(6,
            h4("Performance-Pay Insights", style = "color: #34495e;"),
            uiOutput(ns("perf_pay_insights"))
          )
        )
      )
    )
  )
}

# Server Function ------------------------------------------------------------
compensationServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Log module initialization
    logger$log_info("Compensation module initialized", "compensation_module")
    
    # Reactive Values
    values <- reactiveValues(
      filtered_data = NULL,
      pay_equity_analysis = NULL,
      salary_bands = NULL,
      performance_correlation = NULL
    )
    
    # Data Processing --------------------------------------------------------
    observe({
      req(data())
      start_time <- Sys.time()
      
      tryCatch({
        # Merge employee and performance data
        comp_data <- data()$employee %>%
          left_join(data()$performance, by = "EmployeeID") %>%
          left_join(data()$education, by = c("Education" = "Education Level ID")) %>%
          filter(!is.na(Salary), Salary > 0)
        
        values$filtered_data <- comp_data
        
        # Update filter choices
        updateSelectInput(session, "dept_filter", 
                         choices = sort(unique(comp_data$Department)),
                         selected = NULL)
        updateSelectInput(session, "gender_filter",
                         choices = sort(unique(comp_data$Gender)),
                         selected = NULL)
        updateSelectInput(session, "role_filter",
                         choices = sort(unique(comp_data$JobRole)),
                         selected = NULL)
        
        # Performance tracking
        end_time <- Sys.time()
        execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        memory_usage <- object.size(comp_data)
        
        logger$log_info(
          sprintf("Data processed: %d records in %.2f seconds", nrow(comp_data), execution_time),
          "compensation_module",
          list(execution_time = execution_time, memory_usage = memory_usage, records = nrow(comp_data))
        )
        
      }, error = function(e) {
        logger$log_error(paste("Data processing failed:", e$message), "compensation_module")
      })
    })
    
    # Apply Filters ----------------------------------------------------------
    filtered_data <- reactive({
      req(values$filtered_data)
      
      data_filtered <- values$filtered_data
      
      if (!is.null(input$dept_filter) && length(input$dept_filter) > 0) {
        data_filtered <- data_filtered %>% filter(Department %in% input$dept_filter)
      }
      if (!is.null(input$gender_filter) && length(input$gender_filter) > 0) {
        data_filtered <- data_filtered %>% filter(Gender %in% input$gender_filter)
      }
      if (!is.null(input$role_filter) && length(input$role_filter) > 0) {
        data_filtered <- data_filtered %>% filter(JobRole %in% input$role_filter)
      }
      
      logger$log_info(sprintf("Filters applied, %d records remaining", nrow(data_filtered)), "compensation_module")
      data_filtered
    })
    
    # Reset Filters
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "dept_filter", selected = character(0))
      updateSelectInput(session, "gender_filter", selected = character(0))
      updateSelectInput(session, "role_filter", selected = character(0))
      logger$log_info("Filters reset", "compensation_module")
    })
    
    # KPI Calculations -------------------------------------------------------
    output$avg_salary <- renderText({
      req(filtered_data())
      scales::dollar(mean(filtered_data()$Salary, na.rm = TRUE), accuracy = 1)
    })
    
    output$median_salary <- renderText({
      req(filtered_data())
      scales::dollar(median(filtered_data()$Salary, na.rm = TRUE), accuracy = 1)
    })
    
    output$pay_gap <- renderText({
      req(filtered_data())
      gap_analysis <- analyze_pay_equity(filtered_data())
      paste0(round(gap_analysis$gender_gap * 100, 1), "%")
    })
    
    output$stock_coverage <- renderText({
      req(filtered_data())
      coverage <- filtered_data() %>%
        summarise(pct = mean(StockOptionLevel > 0, na.rm = TRUE)) %>%
        pull(pct)
      scales::percent(coverage, accuracy = 0.1)
    })
    
    # Salary Distribution Visualization --------------------------------------
    output$salary_dist_plot <- renderPlotly({
      req(filtered_data())
      
      tryCatch({
        p <- filtered_data() %>%
          ggplot(aes(x = Salary, fill = Gender)) +
          geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
          scale_fill_manual(values = c("Female" = "#e74c3c", "Male" = "#3498db", "Other" = "#95a5a6")) +
          scale_x_continuous(labels = scales::dollar_format()) +
          labs(
            title = "Salary Distribution by Gender",
            x = "Annual Salary",
            y = "Count",
            fill = "Gender"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
            axis.text = element_text(size = 11),
            legend.position = "top"
          )
        
        ggplotly(p, tooltip = c("x", "y", "fill")) %>%
          config(displayModeBar = FALSE) %>%
          layout(showlegend = TRUE)
        
      }, error = function(e) {
        logger$log_error(paste("Salary distribution plot failed:", e$message), "compensation_module")
        plotly_empty() %>% layout(title = "Error loading visualization")
      })
    })
    
    # Salary Bands -----------------------------------------------------------
    output$salary_bands <- renderUI({
      req(filtered_data())
      
      bands <- create_salary_bands(filtered_data())
      values$salary_bands <- bands
      
      band_ui <- map(1:nrow(bands), ~{
        band <- bands[.x, ]
        div(class = "salary-band mb-2",
          strong(band$band_name),
          br(),
          span(paste("Range:", scales::dollar(band$min_salary), "-", scales::dollar(band$max_salary))),
          br(),
          span(paste("Employees:", band$employee_count), style = "color: #666;")
        )
      })
      
      tagList(band_ui)
    })
    
    # Salary Insights --------------------------------------------------------
    output$salary_insights <- renderUI({
      req(filtered_data())
      
      insights <- filtered_data() %>%
        summarise(
          cv = sd(Salary, na.rm = TRUE) / mean(Salary, na.rm = TRUE),
          q75_q25_ratio = quantile(Salary, 0.75, na.rm = TRUE) / quantile(Salary, 0.25, na.rm = TRUE),
          top_10_pct = quantile(Salary, 0.9, na.rm = TRUE)
        )
      
      insight_level <- case_when(
        insights$cv < 0.3 ~ "Low",
        insights$cv < 0.5 ~ "Moderate", 
        TRUE ~ "High"
      )
      
      tagList(
        p(strong("Salary Spread:"), insight_level, style = "margin-bottom: 8px;"),
        p(strong("75th/25th Percentile Ratio:"), round(insights$q75_q25_ratio, 2), style = "margin-bottom: 8px;"),
        p(strong("Top 10% Threshold:"), scales::dollar(insights$top_10_pct), style = "margin-bottom: 8px;")
      )
    })
    
    # Pay Equity Analysis ----------------------------------------------------
    output$pay_equity_plot <- renderPlotly({
      req(filtered_data())
      
      tryCatch({
        equity_data <- filtered_data() %>%
          group_by(Department, Gender) %>%
          summarise(
            avg_salary = mean(Salary, na.rm = TRUE),
            count = n(),
            .groups = "drop"
          ) %>%
          filter(count >= 3)  # Minimum sample size
        
        p <- equity_data %>%
          ggplot(aes(x = Department, y = avg_salary, fill = Gender)) +
          geom_col(position = "dodge", alpha = 0.8) +
          scale_fill_manual(values = c("Female" = "#e74c3c", "Male" = "#3498db", "Other" = "#95a5a6")) +
          scale_y_continuous(labels = scales::dollar_format()) +
          labs(
            title = "Average Salary by Department & Gender",
            x = "Department",
            y = "Average Salary"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        ggplotly(p, tooltip = c("x", "y", "fill")) %>%
          config(displayModeBar = FALSE)
        
      }, error = function(e) {
        logger$log_error(paste("Pay equity plot failed:", e$message), "compensation_module")
        plotly_empty() %>% layout(title = "Error loading visualization")
      })
    })
    
    # Pay Gap Trends ---------------------------------------------------------
    output$pay_gap_trends <- renderPlotly({
      req(filtered_data())
      
      tryCatch({
        gap_data <- filtered_data() %>%
          group_by(JobRole) %>%
          summarise(
            male_avg = mean(Salary[Gender == "Male"], na.rm = TRUE),
            female_avg = mean(Salary[Gender == "Female"], na.rm = TRUE),
            gap = (male_avg - female_avg) / male_avg * 100,
            .groups = "drop"
          ) %>%
          filter(!is.na(gap), !is.infinite(gap)) %>%
          arrange(desc(gap)) %>%
          head(10)
        
        p <- gap_data %>%
          ggplot(aes(x = reorder(JobRole, gap), y = gap)) +
          geom_col(aes(fill = gap > 0), alpha = 0.8) +
          scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#27ae60"), guide = "none") +
          coord_flip() +
          labs(
            title = "Gender Pay Gap by Job Role",
            x = "Job Role",
            y = "Pay Gap (%)"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
        
        ggplotly(p, tooltip = c("x", "y")) %>%
          config(displayModeBar = FALSE)
        
      }, error = function(e) {
        logger$log_error(paste("Pay gap trends plot failed:", e$message), "compensation_module")
        plotly_empty() %>% layout(title = "Error loading visualization")
      })
    })
    
    # Equity Alerts ----------------------------------------------------------
    output$equity_alerts <- renderUI({
      req(filtered_data())
      
      equity_analysis <- analyze_pay_equity(filtered_data())
      values$pay_equity_analysis <- equity_analysis
      
      alerts <- list()
      
      # Gender pay gap alert
      if (equity_analysis$gender_gap > 0.05) {
        alerts <- append(alerts, list(
          div(class = "equity-alert p-3 mb-2",
            strong("Pay Gap Alert: "), 
            sprintf("Gender pay gap of %.1f%% detected across organization", equity_analysis$gender_gap * 100)
          )
        ))
      } else {
        alerts <- append(alerts, list(
          div(class = "equity-good p-3 mb-2",
            strong("Pay Equity Good: "), 
            "Gender pay gap is within acceptable range (< 5%)"
          )
        ))
      }
      
      # Department-specific alerts
      if (length(equity_analysis$high_gap_depts) > 0) {
        alerts <- append(alerts, list(
          div(class = "equity-alert p-3 mb-2",
            strong("Department Alert: "), 
            sprintf("High pay gaps detected in: %s", paste(equity_analysis$high_gap_depts, collapse = ", "))
          )
        ))
      }
      
      tagList(alerts)
    })
    
    # Stock Options Analysis -------------------------------------------------
    output$stock_analysis <- renderPlotly({
      req(filtered_data())
      
      tryCatch({
        stock_data <- filtered_data() %>%
          filter(!is.na(StockOptionLevel)) %>%
          count(Department, StockOptionLevel) %>%
          group_by(Department) %>%
          mutate(pct = n / sum(n) * 100)
        
        p <- stock_data %>%
          ggplot(aes(x = Department, y = pct, fill = factor(StockOptionLevel))) +
          geom_col(position = "stack", alpha = 0.8) +
          scale_fill_viridis_d(name = "Stock Option\nLevel") +
          labs(
            title = "Stock Option Distribution by Department",
            x = "Department",
            y = "Percentage"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        ggplotly(p, tooltip = c("x", "y", "fill")) %>%
          config(displayModeBar = FALSE)
        
      }, error = function(e) {
        logger$log_error(paste("Stock analysis plot failed:", e$message), "compensation_module")
        plotly_empty() %>% layout(title = "Error loading visualization")
      })
    })
    
    # Stock Summary Table ----------------------------------------------------
    output$stock_summary <- DT::renderDataTable({
      req(filtered_data())
      
      stock_summary <- filtered_data() %>%
        group_by(StockOptionLevel) %>%
        summarise(
          Count = n(),
          `Avg Salary` = scales::dollar(mean(Salary, na.rm = TRUE)),
          `% of Total` = scales::percent(n() / nrow(filtered_data())),
          .groups = "drop"
        ) %>%
        arrange(StockOptionLevel)
      
      DT::datatable(
        stock_summary,
        options = list(
          dom = 't',
          pageLength = 10,
          searching = FALSE,
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(columns = 1:4, fontSize = '12px')
    })
    
    # Stock Insights ---------------------------------------------------------
    output$stock_insights <- renderUI({
      req(filtered_data())
      
      stock_stats <- filtered_data() %>%
        summarise(
          coverage = mean(StockOptionLevel > 0, na.rm = TRUE),
          avg_level = mean(StockOptionLevel, na.rm = TRUE),
          max_level = max(StockOptionLevel, na.rm = TRUE)
        )
      
      tagList(
        p(strong("Stock Coverage:"), scales::percent(stock_stats$coverage)),
        p(strong("Average Level:"), round(stock_stats$avg_level, 1)),
        p(strong("Maximum Level:"), stock_stats$max_level)
      )
    })
    
    # Performance vs Compensation Correlation -------------------------------
    output$perf_comp_correlation <- renderPlotly({
      req(filtered_data())
      
      tryCatch({
        perf_comp_data <- filtered_data() %>%
          filter(!is.na(ManagerRating), !is.na(Salary)) %>%
          mutate(
            PerformanceScore = (ManagerRating + SelfRating) / 2,
            SalaryBand = cut(Salary, breaks = 5, labels = c("Low", "Lower-Mid", "Mid", "Upper-Mid", "High"))
          )
        
        p <- perf_comp_data %>%
          ggplot(aes(x = PerformanceScore, y = Salary, color = Department)) +
          geom_point(alpha = 0.7, size = 2) +
          geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
          scale_y_continuous(labels = scales::dollar_format()) +
          scale_color_viridis_d() +
          labs(
            title = "Performance vs Compensation Correlation",
            x = "Average Performance Rating",
            y = "Annual Salary",
            color = "Department"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            legend.position = "right"
          )
        
        ggplotly(p, tooltip = c("x", "y", "colour")) %>%
          config(displayModeBar = FALSE)
        
      }, error = function(e) {
        logger$log_error(paste("Performance correlation plot failed:", e$message), "compensation_module")
        plotly_empty() %>% layout(title = "Error loading visualization")
      })
    })
    
    # Correlation Statistics -------------------------------------------------
    output$correlation_stats <- renderText({
      req(filtered_data())
      
      tryCatch({
        cor_data <- filtered_data() %>%
          filter(!is.na(ManagerRating), !is.na(Salary), !is.na(SelfRating)) %>%
          mutate(PerformanceScore = (ManagerRating + SelfRating) / 2)
        
        correlations <- cor_data %>%
          summarise(
            perf_salary_cor = cor(PerformanceScore, Salary, use = "complete.obs"),
            manager_salary_cor = cor(ManagerRating, Salary, use = "complete.obs"),
            self_salary_cor = cor(SelfRating, Salary, use = "complete.obs")
          )
        
        values$performance_correlation <- correlations
        
        paste0(
          "Performance-Salary Correlation: ", round(correlations$perf_salary_cor, 3), "\n",
          "Manager Rating-Salary: ", round(correlations$manager_salary_cor, 3), "\n",
          "Self Rating-Salary: ", round(correlations$self_salary_cor, 3), "\n\n",
          "Sample Size: ", nrow(cor_data), " employees"
        )
        
      }, error = function(e) {
        logger$log_error(paste("Correlation calculation failed:", e$message), "compensation_module")
        "Error calculating correlations"
      })
    })
    
    # Performance-Pay Insights -----------------------------------------------
    output$perf_pay_insights <- renderUI({
      req(values$performance_correlation)
      
      cor_strength <- case_when(
        abs(values$performance_correlation$perf_salary_cor) < 0.3 ~ "Weak",
        abs(values$performance_correlation$perf_salary_cor) < 0.7 ~ "Moderate",
        TRUE ~ "Strong"
      )
      
      cor_direction <- ifelse(values$performance_correlation$perf_salary_cor > 0, "Positive", "Negative")
      
      tagList(
        p(strong("Correlation Strength:"), cor_strength),
        p(strong("Correlation Direction:"), cor_direction),
        if (values$performance_correlation$perf_salary_cor < 0.5) {
          p("⚠️ Low performance-pay correlation may indicate compensation review needed", 
            style = "color: #e74c3c;")
        } else {
          p("✅ Good alignment between performance and compensation", 
            style = "color: #27ae60;")
        }
      )
    })
    
    # Return reactive values for inter-module communication
    return(reactive({
      list(
        filtered_data = filtered_data(),
        pay_equity_analysis = values$pay_equity_analysis,
        salary_bands = values$salary_bands,
        performance_correlation = values$performance_correlation
      )
    }))
  })
}

# Helper Functions -----------------------------------------------------------

#' Analyze pay equity across demographics
#' @param data Filtered employee data
#' @return List with equity analysis results
analyze_pay_equity <- function(data) {
  
  # Gender pay gap analysis
  gender_summary <- data %>%
    group_by(Gender) %>%
    summarise(avg_salary = mean(Salary, na.rm = TRUE), .groups = "drop")
  
  if ("Male" %in% gender_summary$Gender && "Female" %in% gender_summary$Gender) {
    male_avg <- gender_summary$avg_salary[gender_summary$Gender == "Male"]
    female_avg <- gender_summary$avg_salary[gender_summary$Gender == "Female"]
    gender_gap <- (male_avg - female_avg) / male_avg
  } else {
    gender_gap <- 0
  }
  
  # Department-level analysis
  dept_gaps <- data %>%
    group_by(Department, Gender) %>%
    summarise(avg_salary = mean(Salary, na.rm = TRUE), count = n(), .groups = "drop") %>%
    filter(count >= 3) %>%
    pivot_wider(names_from = Gender, values_from = avg_salary, names_prefix = "avg_") %>%
    mutate(
      gap = ifelse(!is.na(avg_Male) & !is.na(avg_Female), 
                   (avg_Male - avg_Female) / avg_Male, NA)
    ) %>%
    filter(!is.na(gap))
  
  high_gap_depts <- dept_gaps$Department[dept_gaps$gap > 0.1]
  
  # Ethnicity analysis
  ethnicity_analysis <- data %>%
    group_by(Ethincity) %>%
    summarise(
      avg_salary = mean(Salary, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    filter(count >= 5) %>%
    arrange(desc(avg_salary))
  
  return(list(
    gender_gap = gender_gap,
    high_gap_depts = high_gap_depts,
    dept_gaps = dept_gaps,
    ethnicity_analysis = ethnicity_analysis
  ))
}

#' Create salary bands for organization
#' @param data Employee data
#' @return Data frame with salary band information
create_salary_bands <- function(data) {
  
  salary_quantiles <- quantile(data$Salary, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
  
  bands <- tibble(
    band_name = c("Entry Level", "Professional", "Senior", "Leadership", "Executive"),
    min_salary = salary_quantiles[1:5],
    max_salary = salary_quantiles[2:6]
  ) %>%
    mutate(
      employee_count = map2_int(min_salary, max_salary, ~{
        sum(data$Salary >= .x & data$Salary < .y, na.rm = TRUE)
      }),
      avg_salary = map2_dbl(min_salary, max_salary, ~{
        mean(data$Salary[data$Salary >= .x & data$Salary < .y], na.rm = TRUE)
      })
    )
  
  # Fix the last band to include the maximum
  bands$employee_count[nrow(bands)] <- sum(data$Salary >= bands$min_salary[nrow(bands)], na.rm = TRUE)
  bands$avg_salary[nrow(bands)] <- mean(data$Salary[data$Salary >= bands$min_salary[nrow(bands)]], na.rm = TRUE)
  
  return(bands)
}