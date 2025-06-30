# =============================================================================
# ATLAS LABS HR ANALYTICS - REPORT MODULE UNIT TESTS
# =============================================================================
# Comprehensive unit tests for Report Module functionality
# Developer: akhapwoyaco
# Focus Areas: Dynamic generation, Parameter accuracy, Template rendering,
#              Export integrity, Data serialization, Cross-reference validation,
#              Formatting consistency, Multi-language support
# =============================================================================

# Load required libraries for testing
library(testthat)
library(shiny)
library(rmarkdown)
library(knitr)
library(jsonlite)
library(xml2)
library(withr)
library(mockery)
library(here)

# Source the report module (assuming it exists)
# source("modules/report_module.R")

# =============================================================================
# TEST SETUP AND UTILITIES
# =============================================================================

setup_test_environment <- function() {
  # Create temporary directory for test files
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "atlas_tests")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Create subdirectories
  dir.create(file.path(test_dir, "reports"), showWarnings = FALSE)
  dir.create(file.path(test_dir, "templates"), showWarnings = FALSE)
  dir.create(file.path(test_dir, "data"), showWarnings = FALSE)
  
  return(test_dir)
}

create_mock_data <- function() {
  list(
    kpi_metrics = list(
      total_employees = 1250,
      attrition_rate = 0.157,
      avg_satisfaction = 3.8,
      avg_salary = 75000,
      high_performers = 234
    ),
    attrition_analysis = list(
      by_department = data.frame(
        Department = c("Sales", "HR", "Engineering", "Marketing"),
        AttritionRate = c(0.18, 0.12, 0.09, 0.15),
        EmployeeCount = c(300, 50, 400, 150)
      ),
      risk_factors = c("Low Satisfaction", "Long Commute", "No Promotion")
    ),
    performance_data = data.frame(
      EmployeeID = 1:100,
      ManagerRating = sample(1:5, 100, replace = TRUE),
      SelfRating = sample(1:5, 100, replace = TRUE),
      TrainingTaken = sample(0:5, 100, replace = TRUE)
    ),
    satisfaction_metrics = list(
      job_satisfaction = 3.7,
      environment_satisfaction = 4.1,
      work_life_balance = 3.5,
      relationship_satisfaction = 4.0
    ),
    compensation_analysis = list(
      salary_by_gender = data.frame(
        Gender = c("Male", "Female", "Other"),
        MedianSalary = c(78000, 76500, 77200),
        Count = c(650, 580, 20)
      )
    ),
    selected_filters = list(
      department = c("Sales", "Engineering"),
      age_range = "25-45",
      tenure = ">2 years"
    ),
    analysis_date = Sys.Date(),
    recommendations = list(
      list(
        title = "Improve Work-Life Balance",
        description = "Implement flexible working arrangements",
        priority = "High"
      ),
      list(
        title = "Enhance Career Development",
        description = "Create clear promotion pathways",
        priority = "Medium"
      )
    )
  )
}

create_test_template <- function(file_path) {
  template_content <- '---
title: "Test Report"
output: html_document
params:
  test_param: NULL
  numeric_param: 0
  data_param: NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# Test Report

Test parameter: `r params$test_param`
Numeric parameter: `r params$numeric_param`

```{r}
if (!is.null(params$data_param)) {
  knitr::kable(head(params$data_param))
}
```
'
  writeLines(template_content, file_path)
  return(file_path)
}

# =============================================================================
# 1. DYNAMIC REPORT GENERATION TESTS
# =============================================================================

test_that("Dynamic report generation creates reports successfully", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "test_template.Rmd")
  output_path <- file.path(test_dir, "reports", "test_output.html")
  
  create_test_template(template_path)
  mock_data <- create_mock_data()
  
  # Test basic report generation
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        test_param = "Dynamic Generation Test",
        numeric_param = 42,
        data_param = mock_data$performance_data
      ),
      quiet = TRUE
    )
  })
  
  # Verify output file exists
  expect_true(file.exists(output_path))
  
  # Verify file size is reasonable (not empty)
  expect_gt(file.size(output_path), 1000)
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("Dynamic report generation handles multiple formats", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "multi_format.Rmd")
  
  # Create template with multiple output formats
  multi_format_template <- '---
title: "Multi-Format Test"
output:
  html_document: default
  pdf_document: default
params:
  test_data: NULL
---

# Multi-Format Report
Data rows: `r if(!is.null(params$test_data)) nrow(params$test_data) else 0`
'
  writeLines(multi_format_template, template_path)
  mock_data <- create_mock_data()
  
  # Test HTML generation
  html_output <- file.path(test_dir, "reports", "multi_test.html")
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_format = "html_document",
      output_file = html_output,
      params = list(test_data = mock_data$performance_data),
      quiet = TRUE
    )
  })
  expect_true(file.exists(html_output))
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("Dynamic report generation handles large datasets", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "large_data.Rmd")
  create_test_template(template_path)
  
  # Create large dataset
  large_data <- data.frame(
    ID = 1:10000,
    Value = rnorm(10000),
    Category = sample(LETTERS[1:10], 10000, replace = TRUE)
  )
  
  output_path <- file.path(test_dir, "reports", "large_data_test.html")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        test_param = "Large Data Test",
        numeric_param = nrow(large_data),
        data_param = large_data
      ),
      quiet = TRUE
    )
  })
  
  expect_true(file.exists(output_path))
  expect_gt(file.size(output_path), 5000)
  
  unlink(test_dir, recursive = TRUE)
})

# =============================================================================
# 2. PARAMETER PASSING ACCURACY TESTS
# =============================================================================

test_that("Parameter passing maintains data type accuracy", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "param_test.Rmd")
  
  # Create template that checks parameter types
  param_template <- '---
title: "Parameter Type Test"
output: html_document
params:
  string_param: ""
  numeric_param: 0
  logical_param: TRUE
  list_param: NULL
  date_param: NULL
  dataframe_param: NULL
---

```{r param-checks, include=FALSE}
# Type checks
string_check <- is.character(params$string_param)
numeric_check <- is.numeric(params$numeric_param)
logical_check <- is.logical(params$logical_param)
list_check <- is.list(params$list_param)
date_check <- inherits(params$date_param, "Date")
dataframe_check <- is.data.frame(params$dataframe_param)

# Store results for verification
param_results <- list(
  string_check = string_check,
  numeric_check = numeric_check,
  logical_check = logical_check,
  list_check = list_check,
  date_check = date_check,
  dataframe_check = dataframe_check
)

# Save to file for testing
saveRDS(param_results, file.path(dirname(getwd()), "param_results.rds"))
```

# Parameter Type Verification
All parameters passed correctly: `r all(unlist(param_results))`
'
  writeLines(param_template, template_path)
  
  mock_data <- create_mock_data()
  test_date <- as.Date("2024-01-15")
  
  output_path <- file.path(test_dir, "reports", "param_test.html")
  results_path <- file.path(test_dir, "param_results.rds")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        string_param = "Test String",
        numeric_param = 123.45,
        logical_param = FALSE,
        list_param = mock_data$kpi_metrics,
        date_param = test_date,
        dataframe_param = mock_data$performance_data
      ),
      quiet = TRUE
    )
  })
  
  # Verify parameter types were preserved
  if (file.exists(results_path)) {
    results <- readRDS(results_path)
    expect_true(results$string_check)
    expect_true(results$numeric_check)
    expect_true(results$logical_check)
    expect_true(results$list_check)
    expect_true(results$date_check)
    expect_true(results$dataframe_check)
  }
  
  unlink(test_dir, recursive = TRUE)
})

test_that("Parameter passing handles NULL and missing values", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "null_test.Rmd")
  
  null_template <- '---
title: "NULL Parameter Test"
output: html_document
params:
  null_param: NULL
  optional_param: NULL
---

```{r null-handling}
null_handled <- is.null(params$null_param)
optional_handled <- is.null(params$optional_param)
result <- list(null_handled = null_handled, optional_handled = optional_handled)
saveRDS(result, "null_test_results.rds")
```

NULL parameters handled: `r null_handled && optional_handled`
'
  writeLines(null_template, template_path)
  
  output_path <- file.path(test_dir, "reports", "null_test.html")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        null_param = NULL,
        optional_param = NULL
      ),
      quiet = TRUE
    )
  })
  
  expect_true(file.exists(output_path))
  unlink(test_dir, recursive = TRUE)
})

test_that("Parameter passing handles complex nested structures", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "complex_params.Rmd")
  
  complex_template <- '---
title: "Complex Parameter Test"
output: html_document
params:
  nested_list: NULL
  matrix_param: NULL
  factor_param: NULL
---

```{r complex-handling}
nested_ok <- is.list(params$nested_list) && length(params$nested_list) > 0
matrix_ok <- is.matrix(params$matrix_param)
factor_ok <- is.factor(params$factor_param)

complex_results <- list(
  nested_ok = nested_ok,
  matrix_ok = matrix_ok, 
  factor_ok = factor_ok
)
saveRDS(complex_results, file.path(dirname(getwd()), "complex_results.rds"))
```

Complex parameters: `r all(unlist(complex_results))`
'
  writeLines(complex_template, template_path)
  
  # Create complex parameters
  nested_structure <- list(
    level1 = list(
      level2 = list(
        data = data.frame(x = 1:5, y = letters[1:5]),
        meta = list(created = Sys.time(), version = "1.0")
      )
    ),
    arrays = list(
      numeric_array = array(1:24, dim = c(2, 3, 4)),
      char_array = array(letters[1:12], dim = c(3, 4))
    )
  )
  
  test_matrix <- matrix(1:20, nrow = 4)
  test_factor <- factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C"))
  
  output_path <- file.path(test_dir, "reports", "complex_test.html")
  results_path <- file.path(test_dir, "complex_results.rds")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        nested_list = nested_structure,
        matrix_param = test_matrix,
        factor_param = test_factor
      ),
      quiet = TRUE
    )
  })
  
  if (file.exists(results_path)) {
    results <- readRDS(results_path)
    expect_true(results$nested_ok)
    expect_true(results$matrix_ok)
    expect_true(results$factor_ok)
  }
  
  unlink(test_dir, recursive = TRUE)
})

# =============================================================================
# 3. TEMPLATE RENDERING CORRECTNESS TESTS
# =============================================================================

test_that("Template rendering produces valid HTML structure", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "html_structure.Rmd")
  
  html_template <- '---
title: "HTML Structure Test"
output: 
  html_document:
    toc: true
    theme: flatly
params:
  title_text: "Default Title"
  content_data: NULL
---

# `r params$title_text`

## Section 1
This is section 1 content.

## Section 2  
```{r}
if (!is.null(params$content_data)) {
  plot(params$content_data$x, params$content_data$y)
}
```

## Section 3
Final section with table:
```{r}
if (!is.null(params$content_data)) {
  knitr::kable(head(params$content_data))
}
```
'
  writeLines(html_template, template_path)
  
  test_data <- data.frame(x = 1:10, y = rnorm(10))
  output_path <- file.path(test_dir, "reports", "html_structure.html")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        title_text = "Template Rendering Test",
        content_data = test_data
      ),
      quiet = TRUE
    )
  })
  
  # Verify HTML structure
  expect_true(file.exists(output_path))
  html_content <- readLines(output_path)
  html_text <- paste(html_content, collapse = "\n")
  
  # Check for essential HTML elements
  expect_true(grepl("<html", html_text))
  expect_true(grepl("<head>", html_text))
  expect_true(grepl("<body>", html_text))
  expect_true(grepl("Template Rendering Test", html_text))
  expect_true(grepl("<h1", html_text))
  expect_true(grepl("<h2", html_text))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("Template rendering handles code chunk errors gracefully", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "error_handling.Rmd")
  
  error_template <- '---
title: "Error Handling Test"
output: html_document
params:
  good_data: NULL
  bad_operation: FALSE
---

# Error Handling Test

## Good Section
```{r good-chunk}
if (!is.null(params$good_data)) {
  summary(params$good_data)
}
```

## Potentially Problematic Section  
```{r error-chunk, error=TRUE}
if (params$bad_operation) {
  # This will cause an error
  result <- nonexistent_function(params$good_data)
} else {
  result <- "No error occurred"
}
cat("Result:", result)
```

## Recovery Section
Document continues after error handling.
'
  writeLines(error_template, template_path)
  
  output_path <- file.path(test_dir, "reports", "error_handling.html")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        good_data = data.frame(x = 1:5, y = 1:5),
        bad_operation = TRUE
      ),
      quiet = TRUE
    )
  })
  
  expect_true(file.exists(output_path))
  
  # Verify document was still generated despite errors
  html_content <- readLines(output_path)
  html_text <- paste(html_content, collapse = "\n")
  expect_true(grepl("Recovery Section", html_text))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("Template rendering preserves formatting and styles", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "formatting_test.Rmd")
  
  formatting_template <- '---
title: "Formatting Test"
output: 
  html_document:
    css: |
      .custom-box { 
        background-color: #f0f0f0; 
        padding: 10px; 
        border-radius: 5px; 
      }
      .highlight { color: #ff0000; font-weight: bold; }
params:
  styled_content: "Default Content"
---

<style>
.test-style {
  background-color: lightblue;
  padding: 15px;
  margin: 10px 0;
}
</style>

# Formatting Test

<div class="custom-box">
This is custom styled content: **`r params$styled_content`**
</div>

<div class="test-style">
<span class="highlight">Highlighted text</span> in styled box.
</div>

## Table Formatting
```{r}
library(knitr)
test_table <- data.frame(
  Column1 = c("A", "B", "C"),
  Column2 = c(1, 2, 3),
  Column3 = c("X", "Y", "Z")
)
kable(test_table, format = "html", table.attr = "class=\"table table-striped\"")
```
'
  writeLines(formatting_template, template_path)
  
  output_path <- file.path(test_dir, "reports", "formatting_test.html")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(styled_content = "Formatted Content Test"),
      quiet = TRUE
    )
  })
  
  html_content <- readLines(output_path)
  html_text <- paste(html_content, collapse = "\n")
  
  # Check for CSS preservation
  expect_true(grepl("custom-box", html_text))
  expect_true(grepl("test-style", html_text))
  expect_true(grepl("highlight", html_text))
  expect_true(grepl("Formatted Content Test", html_text))
  
  unlink(test_dir, recursive = TRUE)
})

# =============================================================================
# 4. EXPORT FORMAT INTEGRITY TESTS
# =============================================================================

test_that("Export format integrity for HTML outputs", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "html_export.Rmd")
  create_test_template(template_path)
  
  mock_data <- create_mock_data()
  html_output <- file.path(test_dir, "reports", "html_export.html")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_format = "html_document",
      output_file = html_output,
      params = list(
        test_param = "HTML Export Test",
        numeric_param = 99,
        data_param = mock_data$performance_data
      ),
      quiet = TRUE
    )
  })
  
  expect_true(file.exists(html_output))
  
  # Validate HTML structure
  html_doc <- xml2::read_html(html_output)
  expect_true(length(xml2::xml_find_all(html_doc, "//html")) > 0)
  expect_true(length(xml2::xml_find_all(html_doc, "//head")) > 0)
  expect_true(length(xml2::xml_find_all(html_doc, "//body")) > 0)
  
  # Check for content integrity
  html_text <- xml2::xml_text(html_doc)
  expect_true(grepl("HTML Export Test", html_text))
  expect_true(grepl("99", html_text))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("Export format integrity for PDF outputs", {
  skip_if_not_installed("tinytex")
  skip_if(!tinytex::is_tinytex())
  
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "pdf_export.Rmd")
  
  pdf_template <- '---
title: "PDF Export Test"
output: pdf_document
params:
  test_content: "Default"
  test_number: 0
---

# PDF Export Test

Content: `r params$test_content`
Number: `r params$test_number`

```{r}
plot(1:10, main = "Test Plot")
```
'
  writeLines(pdf_template, template_path)
  
  pdf_output <- file.path(test_dir, "reports", "pdf_export.pdf")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = pdf_output,
      params = list(
        test_content = "PDF Content Test",
        test_number = 42
      ),
      quiet = TRUE
    )
  })
  
  expect_true(file.exists(pdf_output))
  expect_gt(file.size(pdf_output), 1000)  # PDF should have reasonable size
  
  # Basic PDF validation - check magic bytes
  pdf_bytes <- readBin(pdf_output, "raw", n = 4)
  expect_equal(as.character(pdf_bytes[1:4]), c("25", "50", "44", "46"))  # %PDF
  
  unlink(test_dir, recursive = TRUE)
})

test_that("Export format integrity maintains file associations", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "association_test.Rmd")
  create_test_template(template_path)
  
  mock_data <- create_mock_data()
  
  # Test multiple export formats
  formats <- list(
    "html" = list(format = "html_document", extension = ".html"),
    "word" = list(format = "word_document", extension = ".docx")
  )
  
  for (format_name in names(formats)) {
    format_info <- formats[[format_name]]
    output_file <- file.path(test_dir, "reports", 
                           paste0("association_test_", format_name, format_info$extension))
    
    expect_no_error({
      rmarkdown::render(
        input = template_path,
        output_format = format_info$format,
        output_file = output_file,
        params = list(
          test_param = paste("Association Test", format_name),
          numeric_param = 123,
          data_param = mock_data$performance_data
        ),
        quiet = TRUE
      )
    })
    
    expect_true(file.exists(output_file))
    expect_gt(file.size(output_file), 500)
  }
  
  unlink(test_dir, recursive = TRUE)
})

# =============================================================================
# 5. DATA SERIALIZATION SAFETY TESTS
# =============================================================================

test_that("Data serialization handles large datasets safely", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "serialization_test.Rmd")
  
  serialization_template <- '---
title: "Data Serialization Test"
output: html_document
params:
  large_dataset: NULL
  serialized_data: NULL
---

```{r data-checks}
large_data_ok <- !is.null(params$large_dataset) && nrow(params$large_dataset) > 0
serialized_ok <- !is.null(params$serialized_data)

# Test data integrity
if (large_data_ok) {
  data_summary <- list(
    rows = nrow(params$large_dataset),
    cols = ncol(params$large_dataset),
    classes = sapply(params$large_dataset, class),
    memory_size = object.size(params$large_dataset)
  )
} else {
  data_summary <- list(error = "Large dataset not received")
}

saveRDS(data_summary, file.path(dirname(getwd()), "serialization_results.rds"))
```

# Serialization Test Results

Large dataset received: `r large_data_ok`
Serialized data received: `r serialized_ok`

Dataset summary:
- Rows: `r if(large_data_ok) nrow(params$large_dataset) else "N/A"`
- Memory: `r if(large_data_ok) format(object.size(params$large_dataset), units="Mb") else "N/A"`
'
  writeLines(serialization_template, template_path)
  
  # Create large dataset
  large_data <- data.frame(
    id = 1:50000,
    value1 = rnorm(50000),
    value2 = sample(letters, 50000, replace = TRUE),
    value3 = sample(1:1000, 50000, replace = TRUE),
    timestamp = Sys.time() + sample(1:50000, 50000)
  )
  
  # Create serialized data
  serialized_data <- list(
    processed_results = summary(large_data),
    metadata = list(created = Sys.time(), rows = nrow(large_data))
  )
  
  output_path <- file.path(test_dir, "reports", "serialization_test.html")
  results_path <- file.path(test_dir, "serialization_results.rds")
  
  expect_no_error({
    rmarkdown::render(
      input = template_path,
      output_file = output_path,
      params = list(
        large_dataset = large_data,
        serialized_data = serialized_data
      ),
      quiet = TRUE
    )
  })
  
  expect_true(file.exists(output_path))
  
  if (file.exists(results_path)) {
    results <- readRDS(results_path)
    expect_equal(results$rows, 50000)
    expect_equal(results$cols, 5)
    expect_true(results$memory_size > 1000000)  # Should be substantial
  }
  
  unlink(test_dir, recursive = TRUE)
})

test_that("Data serialization preserves data types and structures", {
  test_dir <- setup_test_environment()
  template_path <- file.path(test_dir, "templates", "type_preservation.Rmd")
  
  type_template <- '---
title: "Type Preservation Test"
output: html_document
params:
  mixed_data: NULL
---

```{r type-verification}
if (!is.null(params$mixed_data)) {
  type_results <- list(
    numeric_preserved = is.numeric(params$mixed_data$numeric_col),
    factor_preserved = is.factor(params$mixed_data$factor_col),
    date_preserved = inherits(params$mixed_data$date_col, "Date"),
    logical_preserved = is.logical(params$mixed_data$logical_col),
    character_preserved = is.character(params$mixed_data$char_col),
    factor_levels = levels(params$mixed_data$factor_col),
    date_range = range(params$mixed_data$date_col)
  )
} else {
  type_results <- list(error = "No data received")
}

saveRDS(type_results, file.path(dirname(getwd()), "type_results.rds"))
```

# Type Preservation Results
All types preserved: `r if(!is.null(params$mixed_data)) all(unlist(type_results[1:5])) else FALSE`
'
  writeLines(type_template, template_path)
  
  # Create mixed-type dataset
  mixed_data <- data.frame(
    numeric_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
    factor_col = factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C", "D")),
    date_col = seq(as.Date("2024-01-01"), by = "day", length.out = 5),
    logical_col = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    char_col = c("apple", "banana", "cherry", "date", "elderberry"),
    stringsAsFactors = FALSE
  )
  
  output_path <- file.path(test_dir, "reports", "type_preservation.html")
  results_path <- file.path(test_dir, "type_results.rds")
  
  expect_