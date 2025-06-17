# =============================================================================
# Footer Module for Atlas Labs HR Analytics Dashboard
# File: modules/footer_module.R
# 
# Description: Professional footer module with credits, attribution, and app info
# Developer: akhapwoyaco
# =============================================================================

# Footer UI Module ----
footerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for footer styling
    tags$style(HTML("
      .atlas-footer {
        background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
        color: #ecf0f1;
        padding: 25px 0;
        margin-top: 30px;
        border-top: 3px solid #3498db;
        box-shadow: 0 -2px 10px rgba(0,0,0,0.1);
      }
      
      .footer-section {
        padding: 10px 15px;
      }
      
      .footer-title {
        color: #3498db;
        font-weight: 600;
        font-size: 14px;
        margin-bottom: 8px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .footer-content {
        font-size: 13px;
        line-height: 1.6;
        color: #bdc3c7;
      }
      
      .footer-link {
        color: #3498db;
        text-decoration: none;
        transition: color 0.3s ease;
      }
      
      .footer-link:hover {
        color: #5dade2;
        text-decoration: underline;
      }
      
      .github-icon {
        margin-right: 5px;
        font-size: 16px;
      }
      
      .version-badge {
        background: #27ae60;
        color: white;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: 500;
      }
      
      .data-source-badge {
        background: #e74c3c;
        color: white;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: 500;
      }
      
      .footer-divider {
        border-top: 1px solid #34495e;
        margin: 15px 0;
      }
      
      .footer-copyright {
        text-align: center;
        font-size: 12px;
        color: #95a5a6;
        padding-top: 10px;
      }
      
      .footer-stats {
        font-size: 11px;
        color: #7f8c8d;
        margin-top: 5px;
      }
      
      @media (max-width: 768px) {
        .atlas-footer {
          padding: 20px 0;
        }
        .footer-section {
          padding: 8px 10px;
          text-align: center;
        }
      }
    ")),
    
    # Footer container
    div(class = "atlas-footer",
      div(class = "container-fluid",
        fluidRow(
          # Developer Credits Section
          column(4,
            div(class = "footer-section",
              div(class = "footer-title", 
                icon("code"), " Developer"
              ),
              div(class = "footer-content",
                p(
                  icon("github", class = "github-icon"),
                  a("akhapwoyaco", 
                    href = "https://github.com/akhapwoyaco",
                    class = "footer-link",
                    target = "_blank",
                    title = "Visit GitHub Profile"
                  )
                ),
                div(class = "footer-stats",
                  textOutput(ns("build_info"))
                )
              )
            )
          ),
          
          # Data Source Section
          column(4,
            div(class = "footer-section",
              div(class = "footer-title",
                icon("database"), " Data Source"
              ),
              div(class = "footer-content",
                p(
                  span(class = "data-source-badge", "HR Analytics"),
                  br(),
                  a("HerDataProject Gumroad",
                    href = "https://herdataproject.gumroad.com/l/hr-analytics-tableau",
                    class = "footer-link",
                    target = "_blank",
                    title = "Visit Data Source"
                  )
                ),
                div(class = "footer-stats",
                  textOutput(ns("data_info"))
                )
              )
            )
          ),
          
          # App Information Section
          column(4,
            div(class = "footer-section",
              div(class = "footer-title",
                icon("info-circle"), " Application"
              ),
              div(class = "footer-content",
                p(
                  "Atlas Labs HR Dashboard ",
                  span(class = "version-badge", 
                    textOutput(ns("app_version"), inline = TRUE)
                  )
                ),
                div(class = "footer-stats",
                  textOutput(ns("session_info"))
                )
              )
            )
          )
        ),
        
        # Divider
        div(class = "footer-divider"),
        
        # Copyright and additional info
        div(class = "footer-copyright",
          fluidRow(
            column(12,
              p(
                paste("©", format(Sys.Date(), "%Y"), "Atlas Labs HR Analytics Dashboard."),
                "Built with", 
                icon("heart", style = "color: #e74c3c;"),
                "using R Shiny"
              ),
              div(class = "footer-stats",
                textOutput(ns("performance_stats"))
              )
            )
          )
        )
      )
    )
  )
}

# Footer Server Module ----
footerServer <- function(id, logger = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Log module initialization
    if (!is.null(logger)) {
      logger$log_info("Footer module initialized", "footer_module")
    }
    
    # App version output
    output$app_version <- renderText({
      version <- ifelse(exists("APP_VERSION", envir = .GlobalEnv), 
                       APP_VERSION, "1.0.0")
      paste("v", version, sep = "")
    })
    
    # Build information
    output$build_info <- renderText({
      build_date <- format(Sys.Date(), "%B %d, %Y")
      paste("Built on", build_date)
    })
    
    # Data information
    output$data_info <- renderText({
      "Employee & Performance Data"
    })
    
    # Session information
    output$session_info <- renderText({
      r_version <- paste(R.version$major, R.version$minor, sep = ".")
      shiny_version <- as.character(packageVersion("shiny"))
      paste("R", r_version, "• Shiny", shiny_version)
    })
    
    # Performance statistics (if logger available)
    output$performance_stats <- renderText({
      if (!is.null(logger)) {
        tryCatch({
          # Get memory usage
          mem_usage <- format(object.size(ls(envir = .GlobalEnv)), units = "MB")
          
          # Get session uptime
          if (exists("app_start_time", envir = .GlobalEnv)) {
            uptime <- difftime(Sys.time(), app_start_time, units = "mins")
            uptime_text <- if (uptime < 60) {
              paste(round(uptime, 1), "minutes")
            } else {
              paste(round(uptime/60, 1), "hours")
            }
          } else {
            uptime_text <- "Unknown"
          }
          
          paste("Memory:", mem_usage, "• Uptime:", uptime_text)
        }, error = function(e) {
          "Performance stats unavailable"
        })
      } else {
        paste("Powered by tidyverse & plotly")
      }
    })
    
    # Easter egg: Konami code detection (optional)
    observeEvent(input$konami_code, {
      if (!is.null(logger)) {
        logger$log_info("Konami code activated! 🎮", "footer_module")
      }
      
      showModal(modalDialog(
        title = "🎮 Easter Egg Activated!",
        HTML(paste(
          "<div style='text-align: center; padding: 20px;'>",
          "<h3>🏆 Achievement Unlocked!</h3>",
          "<p>You found the secret developer easter egg!</p>",
          "<p><strong>Developer:</strong> akhapwoyaco</p>",
          "<p><strong>GitHub:</strong> <a href='https://github.com/akhapwoyaco' target='_blank'>github.com/akhapwoyaco</a></p>",
          "<p>🚀 Thanks for exploring the Atlas Labs dashboard!</p>",
          "</div>"
        )),
        footer = modalButton("Awesome!"),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    
    # Return footer module information for parent app
    return(reactive({
      list(
        module_name = "footer",
        status = "active",
        version = ifelse(exists("APP_VERSION", envir = .GlobalEnv), 
                        APP_VERSION, "1.0.0"),
        last_updated = Sys.time()
      )
    }))
  })
}

# Helper Functions ----

#' Get system information for footer display
#' @return named list with system info
get_system_info <- function() {
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    shiny_version = as.character(packageVersion("shiny")),
    memory_usage = format(object.size(ls(envir = .GlobalEnv)), units = "MB")
  )
}

#' Format application uptime
#' @param start_time POSIXct start time
#' @return formatted uptime string
format_uptime <- function(start_time) {
  if (missing(start_time) || is.null(start_time)) {
    return("Unknown")
  }
  
  uptime <- difftime(Sys.time(), start_time, units = "secs")
  
  if (uptime < 60) {
    paste(round(uptime, 0), "seconds")
  } else if (uptime < 3600) {
    paste(round(uptime/60, 1), "minutes")
  } else if (uptime < 86400) {
    paste(round(uptime/3600, 1), "hours")
  } else {
    paste(round(uptime/86400, 1), "days")
  }
}

#' Generate footer JavaScript for enhanced interactivity
#' @return HTML script tag with JavaScript
footer_js <- function() {
  tags$script(HTML("
    // Konami Code Implementation
    let konamiCode = [38, 38, 40, 40, 37, 39, 37, 39, 66, 65];
    let userInput = [];
    
    document.addEventListener('keydown', function(e) {
      userInput.push(e.keyCode);
      
      if (userInput.length > konamiCode.length) {
        userInput.shift();
      }
      
      if (JSON.stringify(userInput) === JSON.stringify(konamiCode)) {
        Shiny.setInputValue('footer-konami_code', Math.random());
        userInput = [];
      }
    });
    
    // Add subtle animation to footer links
    $(document).ready(function() {
      $('.footer-link').hover(
        function() {
          $(this).css('transform', 'translateY(-1px)');
        },
        function() {
          $(this).css('transform', 'translateY(0)');
        }
      );
    });
  "))
}