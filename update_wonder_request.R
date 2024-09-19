# update_wonder_request.R

.libPaths(c("~/R_libs", .libPaths()))
library(httr)
library(xml2)
library(logger)

# Set up logging
log_threshold(INFO)

create_wonder_request <- function(years) {
  # Create the base XML structure
  xml_doc <- xml_new_document()
  xml_doc <- xml_add_child(xml_doc, "request-parameters")
  xml_string <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n', as.character(xml_doc))

  # Add parameters to match user-provided XML structure
  parameters <- list(
    list(name = "B_1", value = "D76.V1-level1"),
    list(name = "B_2", value = "*None*"),
    list(name = "B_3", value = "*None*"),
    list(name = "B_4", value = "*None*"),
    list(name = "B_5", value = "*None*"),
    list(name = "F_D76.V1", value = as.character(years)),
    list(name = "F_D76.V10", value = "*All*"),
    list(name = "F_D76.V2", value = "*All*"),
    list(name = "F_D76.V25", value = "*All*"),
    list(name = "F_D76.V27", value = "*All*"),
    list(name = "F_D76.V9", value = "*All*"),
    list(name = "I_D76.V1", value = paste(years, "(", years, ")", collapse = "\n")),
    list(name = "I_D76.V10", value = "*All* (The United States)"),
    list(name = "I_D76.V2", value = "*All* (All Causes of Death)"),
    list(name = "I_D76.V25", value = "*All* (All Urbanization Categories)"),
    list(name = "I_D76.V27", value = "*All* (All Weekdays)"),
    list(name = "I_D76.V9", value = "*All* (All ICD-10 Codes)"),
    list(name = "M_1", value = "D76.M1"),
    list(name = "M_2", value = "D76.M2"),
    list(name = "M_3", value = "D76.M3"),
    list(name = "O_V10_fmode", value = "freg"),
    list(name = "O_V1_fmode", value = "freg"),
    list(name = "O_V25_fmode", value = "freg"),
    list(name = "O_V27_fmode", value = "freg"),
    list(name = "O_V9_fmode", value = "freg"),
    list(name = "O_aar_pop", value = "0000"),
    list(name = "O_aar_strat", value = "D76.V51"),
    list(name = "O_age", value = "D76.V5"),
    list(name = "O_age_adjust", value = "0"),
    list(name = "O_precision", value = "1"),
    list(name = "O_rate_per", value = "100000"),
    list(name = "O_show_totals", value = "true"),
    list(name = "O_timeout", value = "300"),
    list(name = "O_title", value = "Mortality Data Request"),
    list(name = "O_ucd", value = "D76.V2"),
    list(name = "V_D76.V1", value = "*All*"),
    list(name = "V_D76.V10", value = "*All*"),
    list(name = "V_D76.V2", value = "*All*"),
    list(name = "V_D76.V25", value = "*All*"),
    list(name = "V_D76.V27", value = "*All*"),
    list(name = "V_D76.V5", value = "*All*"),
    list(name = "V_D76.V51", value = "*All*"),
    list(name = "V_D76.V9", value = "*All*"),
    list(name = "action-Send", value = "Send"),
    list(name = "finder-stage-D76.V1", value = "codeset"),
    list(name = "finder-stage-D76.V2", value = "codeset"),
    list(name = "finder-stage-D76.V25", value = "codeset"),
    list(name = "finder-stage-D76.V27", value = "codeset"),
    list(name = "finder-stage-D76.V51", value = "codeset"),
    list(name = "finder-stage-D76.V9", value = "codeset"),
    list(name = "stage", value = "request"),
    list(name = "saved_id", value = ""),
    list(name = "accept_datause_restrictions", value = "true")
  )

  # Add parameters to XML
  for (param in parameters) {
    param_node <- xml_add_child(xml_doc, "parameter")
    xml_add_child(param_node, "name", param$name)
    if (length(param$value) > 1) {
      for (val in param$value) {
        xml_add_child(param_node, "value", val)
      }
    } else {
      xml_add_child(param_node, "value", param$value)
    }
  }

  # Convert to character
  xml_string <- as.character(xml_doc)

  return(xml_string)
}

query_cdc_wonder <- function(years) {
  url <- "https://wonder.cdc.gov/controller/datarequest/D140"

  # Create XML request
  xml_request <- create_wonder_request(years)

  # Save XML request to file
  writeLines(xml_request, "wonder_request.xml")

  # Log the request
  log_info("Sending request to CDC Wonder API")
  log_debug("Request XML: {xml_request}")

  # Add parameters to XML
  for (param in parameters) {
    param_node <- xml_add_child(root, "parameter")
    xml_add_child(param_node, "name", param$name)
    if (length(param$value) > 1) {
      for (val in param$value) {
        xml_add_child(param_node, "value", val)
      }
    } else {
      xml_add_child(param_node, "value", param$value)
    }
  }

  # Convert to character
  xml_string <- as.character(xml_doc)

  return(xml_string)
}

query_cdc_wonder <- function(years) {
  url <- "https://wonder.cdc.gov/controller/datarequest/D76"

  # Create XML request
  xml_request <- create_wonder_request(years)

  # Save XML request to file
  writeLines(xml_request, "wonder_request.xml")

  # Log the request
  log_info("Sending request to CDC Wonder API")
  log_debug("Request XML: {xml_request}")

  # Send request
  response <- tryCatch({
    POST(url,
         body = list(request_xml = xml_request),
         encode = "form",
         add_headers("Content-Type" = "application/x-www-form-urlencoded",
                     "Accept" = "application/xml"),
         verbose())
  }, error = function(e) {
    log_error("Error sending request: {e$message}")
    return(NULL)
  })

  if (is.null(response)) {
    return(NULL)
  }

  # Check response status
  if (status_code(response) != 200) {
    log_error("API request failed with status code: {status_code(response)}")
    return(NULL)
  }

  # Parse response
  content <- content(response, "text")
  log_info("Received response from CDC Wonder API")
  log_debug("Response content: {content}")

  # Verify API response
  parsed_xml <- xml2::read_xml(content)

  # Extract total deaths for each year
  deaths_2002 <- as.numeric(xml2::xml_text(xml2::xml_find_first(parsed_xml, "//year[@year='2002']/deaths")))
  deaths_2003 <- as.numeric(xml2::xml_text(xml2::xml_find_first(parsed_xml, "//year[@year='2003']/deaths")))
  deaths_2004 <- as.numeric(xml2::xml_text(xml2::xml_find_first(parsed_xml, "//year[@year='2004']/deaths")))

  # Compare with expected values
  expected_deaths <- c(2443387, 2448288, 2397615)
  actual_deaths <- c(deaths_2002, deaths_2003, deaths_2004)

  if (all(actual_deaths == expected_deaths)) {
    log_info("API response matches expected death counts for 2002-2004")
  } else {
    log_warn("API response does not match expected death counts")
    log_debug("Expected: {expected_deaths}")
    log_debug("Actual: {actual_deaths}")
  }

  return(parsed_xml)
}

# Test the function
test_api_request <- function() {
  years <- 2002:2004
  result <- query_cdc_wonder(years)

  if (is.null(result)) {
    log_error("API request failed")
    return(FALSE)
  }

  # Basic checks on the response
  if (grepl("error", result, ignore.case = TRUE)) {
    log_error("API response contains an error message")
    return(FALSE)
  }

  if (!all(sapply(years, function(y) grepl(as.character(y), result)))) {
    log_error("Response does not contain all requested years")
    return(FALSE)
  }

  log_info("API request successful")
  return(TRUE)
}

# Run the test
test_result <- test_api_request()
if (test_result) {
  cat("API request test passed successfully.\n")
} else {
  cat("API request test failed. Check the logs for more information.\n")
}
