rate_limiter <- new.env(parent = emptyenv())
page_cache <- new.env(parent = emptyenv())

rate_limiter$call_timestamps <- numeric()  
rate_limiter$max_calls <- 10        
rate_limiter$time_window <- 60    

Page <- function(config) {
  cache <- new.env(parent = emptyenv())
  page <- list(
    config = config,
    fetch_table = function(identifier, dynamic_values = list(), index = 1) {
      if (index > nrow(config) || index < 1) {
        stop("Invalid index: Please provide a valid row index.")
      }
      url <- config$url[index]
      if (!is.null(dynamic_values) && length(dynamic_values) > 0) {
        for (key in names(dynamic_values)) {
          placeholder <- paste0("\\{", key, "\\}")
          url <- gsub(placeholder, dynamic_values[[key]], url)
        }
      }
      if (!exists(url, envir = page_cache)) {
        enforce_rate_limit()
        message("Fetching page: ", url)
        page_content <- read_html(url)
        assign(url, page_content, envir = page_cache)
      } else {
        message("Using cached page: ", url)
      }
      page_content <- get(url, envir = page_cache)
      table_content <- page_content %>%
        html_element(identifier)
      return(table_content)
    }
  )
  structure(page, class = "Page")
}

enforce_rate_limit <- function() {
  current_time <- as.numeric(Sys.time())
  valid_timestamps <- rate_limiter$call_timestamps[
    rate_limiter$call_timestamps > (current_time - rate_limiter$time_window)
  ]
  rate_limiter$call_timestamps <- valid_timestamps
  if (length(rate_limiter$call_timestamps) < rate_limiter$max_calls) {
    rate_limiter$call_timestamps <- c(rate_limiter$call_timestamps, current_time)
  } else {
    wait_time <- rate_limiter$time_window - (current_time - min(valid_timestamps))
    if (wait_time > 0) {
      message("Rate limit reached. Waiting for ", wait_time, " seconds...")
      Sys.sleep(wait_time) 
    }
    enforce_rate_limit()
  }
}


get_page_node <- function(page) {
  UseMethod("get_page_node")
}

get_page_node_stats <- function(page) {
  UseMethod("get_page_node_stats")
}

get_page_multi_node_stats <- function(page, base_nodes) {
  UseMethod("get_page_multi_node_stats")
}