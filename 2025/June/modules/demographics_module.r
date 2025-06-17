# ===============================================================================
# DEMOGRAPHICS MODULE - ATLAS LABS HR ANALYTICS DASHBOARD
# ===============================================================================
# File: modules/demographics_module.R
# Purpose: Comprehensive demographic analysis and diversity metrics
# Author: akhapwoyaco (GitHub)
# Maintains: Blueprint architecture with minimal code and bidirectional communication
# ===============================================================================

# UI FUNCTION ===================================================================
demographicsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for demographics styling
    tags$head(
      tags$style(HTML("
        .demo-card { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
          border-radius: 12px; 
          padding: 20px; 
          margin: 10px 0; 
          color: white; 
          text-align: center; 
        }
        .demo-metric { font-size: 2.5em; font-weight: bold; }
        .demo-label { font-size: 1.1em; margin-top: 5px; }
        .chart-container { 
          background: white; 
          border-radius: 8px; 
          padding: 15px; 
          box-shadow: 0 2px 10px rgba(0,0,0,0.1); 
          margin: 15px 0; 
        }
      "))
    ),
    
    # Page Header
    fluidRow(
      column(12,
        div(class = "page-header",
          h2(icon("users"), "Workforce Demographics", 
             style = "color: #2c3e50; margin-bottom: 20px;"),
          p("Comprehensive analysis of workforce diversity and demographic patterns",
            style = "color: #7f8c8d; font-size: 1.1em;")
        )
      )
    ),
    
    # KPI Cards Row
    fluidRow(
      column(3, div(class = "demo-card",
        div(class = "demo-metric", textOutput(ns("total_employees"))),
        div(class = "demo-label", "Total Employees")
      )),
      column(3, div(class = "demo-card",
        div(class = "demo-metric", textOutput(ns("gender_diversity"))),
        div(class = "demo-label", "Gender Balance")
      )),
      column(3, div(class = "demo-card",
        div(class = "demo-metric", textOutput(ns("avg_age"))),
        div(class = "demo-label", "Average Age")
      )),
      column(3, div(class = "demo-card",
        div(class = "demo-metric", textOutput(ns("diversity_index"))),
        div(class = "demo-label", "Diversity Index")
      ))
    ),
    
    # Interactive Filters
    fluidRow(
      column(12,
        wellPanel(
          fluidRow(
            column(3, selectInput(ns("dept_filter"), "Department:",
              choices = NULL, multiple = TRUE#, 
              # options = list(placeholder = "All Departments")
              )),
            column(3, selectInput(ns("age_filter"), "Age Group:",
              choices = c("All Ages" = "all", "18-30" = "young", 
                         "31-45" = "mid", "46+" = "senior"),
              selected = "all")),
            column(3, selectInput(ns("state_filter"), "State:",
              choices = NULL, multiple = TRUE#,
              # options = list(placeholder = "All States")
              )
              ),
            column(3, div(style = "margin-top: 25px;",
              actionButton(ns("reset_filters"), "Reset Filters", 
                          class = "btn-outline-secondary btn-sm")))
          )
        )
      )
    ),
    
    # Main Visualization Tabs
    tabsetPanel(id = ns("demo_tabs"), type = "pills",
      
      # Age & Gender Analysis Tab
      tabPanel("Age & Gender", 
        fluidRow(
          column(6, div(class = "chart-container",
            h4("Age Distribution by Gender", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("age_gender_plot"), height = "400px")
          )),
          column(6, div(class = "chart-container",
            h4("Gender Distribution", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("gender_pie"), height = "400px")
          ))
        ),
        fluidRow(
          column(12, div(class = "chart-container",
            h4("Age vs Tenure Analysis", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("age_tenure_scatter"), height = "350px")
          ))
        )
      ),
      
      # Ethnicity & Diversity Tab
      tabPanel("Ethnicity & Diversity",
        fluidRow(
          column(8, div(class = "chart-container",
            h4("Ethnicity Breakdown", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("ethnicity_bar"), height = "400px")
          )),
          column(4, div(class = "chart-container",
            h4("Diversity Metrics", style = "margin-bottom: 15px;"),
            DT::dataTableOutput(ns("diversity_table"))
          ))
        ),
        fluidRow(
          column(12, div(class = "chart-container",
            h4("Diversity Heatmap by Department", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("diversity_heatmap"), height = "350px")
          ))
        )
      ),
      
      # Geographic Distribution Tab
      tabPanel("Geographic Analysis",
        fluidRow(
          column(6, div(class = "chart-container",
            h4("Employee Distribution by State", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("state_map"), height = "450px")
          )),
          column(6, div(class = "chart-container",
            h4("Distance from Home Analysis", style = "margin-bottom: 15px;"),
            plotlyOutput(ns("distance_dist"), height = "450px")
          ))
        )
      ),
      
      # Cross-Demographic Analysis Tab
      tabPanel("Cross-Analysis",
        fluidRow(
          column(12, div(class = "chart-container",
            h4("Multi-Dimensional Analysis", style = "margin-bottom: 15px;"),
            fluidRow(
              column(4, selectInput(ns("x_var"), "X-Axis:", 
                choices = c("Age" = "Age", "Gender" = "Gender", 
                           "Ethnicity" = "Ethincity", "Department" = "Department"))),
              column(4, selectInput(ns("y_var"), "Y-Axis:",
                choices = c("Salary" = "Salary", "Years at Company" = "YearsAtCompany",
                           "Distance from Home" = "DistanceFromHome"))),
              column(4, selectInput(ns("color_var"), "Color By:",
                choices = c("Gender" = "Gender", "Ethnicity" = "Ethincity",
                           "Department" = "Department", "Education" = "EducationField")))
            ),
            plotlyOutput(ns("cross_analysis"), height = "400px")
          ))
        )
      )
    )
  )
}

# SERVER FUNCTION ===============================================================
demographicsServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Log module initialization
    logger$log_info("Demographics module initialized", "demographics_module", 
                    list(memory = as.numeric(object.size(environment()))))
    
    # Reactive filtered data
    filtered_data <- reactive({
      start_time <- Sys.time()
      
      req(data())
      df <- data()
      
      # Apply department filter
      if (!is.null(input$dept_filter) && length(input$dept_filter) > 0) {
        df <- df %>% filter(Department %in% input$dept_filter)
      }
      
      # Apply age filter
      if (input$age_filter != "all") {
        df <- df %>% 
          filter(
            case_when(
              input$age_filter == "young" ~ Age <= 30,
              input$age_filter == "mid" ~ Age > 30 & Age <= 45,
              input$age_filter == "senior" ~ Age > 45,
              TRUE ~ TRUE
            )
          )
      }
      
      # Apply state filter
      if (!is.null(input$state_filter) && length(input$state_filter) > 0) {
        df <- df %>% filter(State %in% input$state_filter)
      }
      
      execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      logger$log_info(paste("Data filtered:", nrow(df), "records"), 
                      "demographics_module", 
                      list(execution_time = execution_time))
      
      df
    })
    
    # Update filter choices
    observe({
      req(data())
      df <- data()
      
      updateSelectInput(session, "dept_filter", 
                       choices = sort(unique(df$Department)))
      updateSelectInput(session, "state_filter",
                       choices = sort(unique(df$State)))
    })
    
    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "dept_filter", selected = character(0))
      updateSelectInput(session, "state_filter", selected = character(0))
      updateSelectInput(session, "age_filter", selected = "all")
      logger$log_info("Filters reset", "demographics_module")
    })
    
    # Demographic patterns analysis
    demo_patterns <- reactive({
      analyze_demographic_patterns(filtered_data(), logger)
    })
    
    # Diversity metrics
    diversity_metrics <- reactive({
      create_diversity_metrics(filtered_data(), logger)
    })
    
    # KPI Outputs
    output$total_employees <- renderText({
      scales::comma(nrow(filtered_data()))
    })
    
    output$gender_diversity <- renderText({
      df <- filtered_data()
      gender_counts <- table(df$Gender)
      ratio <- min(gender_counts) / max(gender_counts)
      paste0(scales::percent(ratio, accuracy = 1))
    })
    
    output$avg_age <- renderText({
      round(mean(filtered_data()$Age, na.rm = TRUE), 1)
    })
    
    output$diversity_index <- renderText({
      diversity_metrics()$overall_index
    })
    
    # Age & Gender Visualizations
    output$age_gender_plot <- renderPlotly({
      df <- filtered_data()
      
      p <- df %>%
        ggplot(aes(x = Age, fill = Gender)) +
        geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
        labs(title = "Age Distribution by Gender", x = "Age", y = "Count") +
        theme_atlas() +
        scale_fill_manual(values = ATLAS_COLORS[1:2])
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(hovermode = "x unified")
    })
    
    output$gender_pie <- renderPlotly({
      df <- filtered_data()
      gender_summary <- df %>%
        count(Gender) %>%
        mutate(percentage = n / sum(n) * 100)
      
      plot_ly(gender_summary, labels = ~Gender, values = ~n, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              marker = list(colors = ATLAS_COLORS[1:nrow(gender_summary)],
                           line = list(color = '#FFFFFF', width = 2))) %>%
        layout(showlegend = TRUE, 
               title = list(text = "Gender Distribution", font = list(size = 16)))
    })
    
    output$age_tenure_scatter <- renderPlotly({
      df <- filtered_data()
      
      p <- df %>%
        ggplot(aes(x = Age, y = YearsAtCompany, color = Gender, size = Salary)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
        labs(title = "Age vs Tenure Relationship", 
             x = "Age", y = "Years at Company") +
        theme_atlas() +
        scale_color_manual(values = ATLAS_COLORS[1:2]) +
        scale_size_continuous(range = c(2, 8), guide = "none")
      
      ggplotly(p, tooltip = c("x", "y", "colour", "size"))
    })
    
    # Ethnicity Visualizations
    output$ethnicity_bar <- renderPlotly({
      df <- filtered_data()
      
      ethnicity_summary <- df %>%
        count(Ethincity, sort = TRUE) %>%
        mutate(percentage = n / sum(n) * 100)
      
      p <- ethnicity_summary %>%
        ggplot(aes(x = reorder(Ethincity, n), y = n, fill = Ethincity)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        labs(title = "Employee Count by Ethnicity", 
             x = "Ethnicity", y = "Number of Employees") +
        theme_atlas() +
        theme(legend.position = "none") +
        scale_fill_viridis_d()
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    output$diversity_table <- DT::renderDataTable({
      diversity_metrics()$by_category
    }, options = list(dom = 't', pageLength = 10), 
    rownames = FALSE, class = 'cell-border stripe compact')
    
    output$diversity_heatmap <- renderPlotly({
      df <- filtered_data()
      
      diversity_by_dept <- df %>%
        group_by(Department, Ethincity) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(Department) %>%
        mutate(percentage = count / sum(count) * 100)
      
      p <- diversity_by_dept %>%
        ggplot(aes(x = Department, y = Ethincity, fill = percentage)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", 
                           name = "Percentage") +
        labs(title = "Ethnic Diversity by Department", 
             x = "Department", y = "Ethnicity") +
        theme_atlas() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
    })
    
    # Geographic Visualizations
    output$state_map <- renderPlotly({
      df <- filtered_data()
      
      state_summary <- df %>%
        count(State, sort = TRUE) %>%
        mutate(percentage = n / sum(n) * 100)
      
      p <- state_summary %>%
        ggplot(aes(x = reorder(State, n), y = n, fill = n)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        labs(title = "Employee Distribution by State", 
             x = "State", y = "Number of Employees") +
        theme_atlas() +
        scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
        theme(legend.position = "none")
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    output$distance_dist <- renderPlotly({
      df <- filtered_data()
      
      p <- df %>%
        ggplot(aes(x = `DistanceFromHome (KM)`, fill = BusinessTravel)) +
        geom_histogram(alpha = 0.7, bins = 25) +
        facet_wrap(~BusinessTravel, scales = "free_y") +
        labs(title = "Distance from Home by Travel Frequency", 
             x = "Distance from Home (KM)", y = "Count") +
        theme_atlas() +
        scale_fill_manual(values = ATLAS_COLORS[1:3]) +
        theme(legend.position = "none")
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
    })
    
    # Cross-Analysis Visualization
    output$cross_analysis <- renderPlotly({
      df <- filtered_data()
      
      # Dynamic plot based on selections
      if (input$x_var %in% c("Age", "Salary", "YearsAtCompany", "DistanceFromHome")) {
        # Numeric x-axis
        p <- df %>%
          ggplot(aes_string(x = input$x_var, y = input$y_var, 
                           color = input$color_var)) +
          geom_point(alpha = 0.6, size = 2) +
          geom_smooth(method = "lm", se = FALSE, alpha = 0.3) +
          labs(title = paste("Relationship:", input$x_var, "vs", input$y_var),
               x = input$x_var, y = input$y_var) +
          theme_atlas() +
          scale_color_viridis_d()
      } else {
        # Categorical x-axis
        p <- df %>%
          ggplot(aes_string(x = input$x_var, y = input$y_var, 
                           fill = input$color_var)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Distribution:", input$y_var, "by", input$x_var),
               x = input$x_var, y = input$y_var) +
          theme_atlas() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_viridis_d()
      }
      
      ggplotly(p, tooltip = c("x", "y", "colour", "fill"))
    })
    
    # Return reactive data for other modules
    return(
      list(
        filtered_data = filtered_data,
        demographic_patterns = demo_patterns,
        diversity_metrics = diversity_metrics
      )
    )
  })
}

# ANALYSIS FUNCTIONS ============================================================

analyze_demographic_patterns <- function(data, logger) {
  start_time <- Sys.time()
  
  tryCatch({
    patterns <- list(
      age_distribution = data %>%
        group_by(age_group = cut(Age, breaks = c(0, 30, 45, 60, 100), 
                                labels = c("18-30", "31-45", "46-60", "60+"))) %>%
        summarise(count = n(), avg_salary = mean(Salary, na.rm = TRUE), .groups = "drop"),
      
      gender_by_dept = data %>%
        group_by(Department, Gender) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(percentage = count / sum(count) * 100),
      
      ethnicity_trends = data %>%
        group_by(Ethincity) %>%
        summarise(
          count = n(),
          avg_age = mean(Age, na.rm = TRUE),
          avg_salary = mean(Salary, na.rm = TRUE),
          avg_tenure = mean(YearsAtCompany, na.rm = TRUE),
          .groups = "drop"
        ),
      
      geographic_patterns = data %>%
        group_by(State) %>%
        summarise(
          count = n(),
          avg_distance = mean(`DistanceFromHome (KM)`, na.rm = TRUE),
          avg_salary = mean(Salary, na.rm = TRUE),
          .groups = "drop"
        )
    )
    
    execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    logger$log_info("Demographic patterns analyzed successfully", 
                    "demographics_module", 
                    list(execution_time = execution_time, 
                         patterns_count = length(patterns)))
    
    return(patterns)
    
  }, error = function(e) {
    logger$log_error(paste("Error analyzing demographic patterns:", e$message), 
                     "demographics_module")
    return(list())
  })
}

create_diversity_metrics <- function(data, logger) {
  start_time <- Sys.time()
  
  tryCatch({
    # Calculate Shannon Diversity Index for each category
    calculate_shannon <- function(x) {
      props <- table(x) / length(x)
      -sum(props * log(props, base = 2))
    }
    
    # Calculate Simpson's Diversity Index
    calculate_simpson <- function(x) {
      props <- table(x) / length(x)
      1 - sum(props^2)
    }
    
    diversity_metrics <- list(
      by_category = data.frame(
        Category = c("Gender", "Ethnicity", "Department", "Education Field"),
        Shannon_Index = c(
          calculate_shannon(data$Gender),
          calculate_shannon(data$Ethincity),
          calculate_shannon(data$Department),
          calculate_shannon(data$EducationField)
        ),
        Simpson_Index = c(
          calculate_simpson(data$Gender),
          calculate_simpson(data$Ethincity),
          calculate_simpson(data$Department),
          calculate_simpson(data$EducationField)
        ),
        Categories_Count = c(
          length(unique(data$Gender)),
          length(unique(data$Ethincity)),
          length(unique(data$Department)),
          length(unique(data$EducationField))
        )
      ),
      
      overall_index = paste0(
        round(mean(c(
          calculate_shannon(data$Gender),
          calculate_shannon(data$Ethincity)
        )) / log(2, base = 2) * 100, 1), "%"
      ),
      
      representation_ratios = list(
        gender_ratio = min(table(data$Gender)) / max(table(data$Gender)),
        ethnic_representation = table(data$Ethincity) / nrow(data) * 100
      )
    )
    
    execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    logger$log_info("Diversity metrics calculated successfully", 
                    "demographics_module",
                    list(execution_time = execution_time))
    
    return(diversity_metrics)
    
  }, error = function(e) {
    logger$log_error(paste("Error calculating diversity metrics:", e$message), 
                     "demographics_module")
    return(list(
      by_category = data.frame(),
      overall_index = "0%",
      representation_ratios = list()
    ))
  })
}