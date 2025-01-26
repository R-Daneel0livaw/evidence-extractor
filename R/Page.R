rate_limiter <- new.env(parent = emptyenv())
page_cache <- new.env(parent = emptyenv())

rate_limiter$call_timestamps <- numeric()  
rate_limiter$max_calls <- 10        
rate_limiter$time_window <- 60    

Page <- function(config) {
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

get_page_node <- function(page) {
  UseMethod("get_page_node")
}

get_page_node_stats <- function(page) {
  UseMethod("get_page_node_stats")
}

get_page_multi_node_stats <- function(page, base_nodes) {
  UseMethod("get_page_multi_node_stats")
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

base_get_page_node <- function(page, clean_fn, join_fn, mutate_fn, filter_fn = NULL, select_cols = NULL) {
  view <- page$fetch_table(page$config$table_identifier)
  
  identifier <- extract_identifier(
    view = view,
    identifier = page$config$key_data_identifier,
    names = c("id", "season"), 
    id = str_extract(id, page$config$id_extract_regex, 1)
  )
  
  if (!is.null(filter_fn)) {
    identifier <- filter_fn(identifier)
  }
  
  identifier_table <- join_fn(clean_fn(view), identifier)
  
  result <- mutate_fn(identifier_table)
  
  if (!is.null(select_cols)) {
    if (all(grepl("^-", select_cols))) {
      exclude_cols <- gsub("^-", "", select_cols)
      result <- result %>% select(-any_of(exclude_cols))
    } else {
      result <- result %>% select(any_of(select_cols))
    }
  }
  
  return(result)
}
