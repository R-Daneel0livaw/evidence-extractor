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

base_get_page_node <- function(page,
                               config = NULL,
                               orig_data = NULL,
                               clean_fn = NULL,
                               join_fn = NULL,
                               mutate_fn,
                               filter_fn = NULL,
                               select_cols = NULL,
                               rename_fn = NULL,
                               stats_fn = NULL) {
  
  config <- config %||% page$config 
  
  view <- do.call(page$fetch_table, build_fetch_args(config))
  
  identifier <- extract_identifier(
    view = view,
    identifier = config$key_data_identifier,
    names = unlist(config$id_extract_names), 
    id = str_extract(id, config$id_extract_regex, 1)
  )
  
  if (!is.null(filter_fn)) {
    identifier <- filter_fn(identifier)
  }
  
  result <- orig_data %||% get_cleaned_view(clean_fn, view, config)
  
  if (!is.null(join_fn)) {
    result <- join_fn(result, identifier) %>%
      mutate_fn() %>%
      apply_column_selection(select_cols)
  }
  
  if (!is.null(rename_fn)) {
    result <- rename_fn(result, config$suffix, config$rename_start, config$end)
  }
  
  if (!is.null(stats_fn)) {
    result <- stats_fn(result, config)
  }
  
  return(result)
}

build_fetch_args <- function(config) {
  fetch_args <- list(identifier = config$table_identifier)
  
  if (!is.null(config[["stat"]])) {
    fetch_args$dynamic_values <- list(node = config[["stat"]])
  }
  if (!is.null(config[["index"]])) {
    fetch_args$index <- config[["index"]]
  }
  
  fetch_args
}

get_cleaned_view <- function(clean_fn, view, config) {
  extra_params <- config[c("multi_row_header", "dummy_header")]
  extra_params <- extra_params[extra_params == TRUE]
  
  if (length(extra_params) > 0) {
    do.call(clean_fn, c(list(view), extra_params))
  } else {
    clean_fn(view)
  }
}

apply_column_selection <- function(data, select_cols) {
  if (!is.null(select_cols)) {
    expanded_cols <- unlist(lapply(select_cols, function(col) {
      if (grepl(":", col)) {
        col <- gsub("^-", "", col) 
        col_range <- strsplit(col, ":")[[1]]
        
        col_names <- names(data)
        col_indices <- tidyselect::eval_select(expr(c(all_of(col_range[1]):all_of(col_range[2]))), data)
        return(col_names[col_indices])
      }
      return(col)
    }))
    
    exclude_cols <- expanded_cols[grepl("^-", select_cols)] %>% gsub("^-", "", .)
    include_cols <- expanded_cols[!grepl("^-", select_cols)]
    
    if (length(exclude_cols) > 0) {
      return(data %>% select(-any_of(exclude_cols)))
    } else if (length(include_cols) > 0) {
      return(data %>% select(any_of(include_cols)))
    }
  }
  
  data  
}

base_get_config_rows <- function(
    page, 
    base_nodes, 
    join_fn, 
    mutate_fn = NULL, 
    map_fn, 
    select_cols = NULL
) {
  stats_table <- join_fn(page, base_nodes)
  
  if (!is.null(mutate_fn)) {
    stats_table <- mutate_fn(stats_table)
  }
  
  if (!is.null(select_cols)) {
    stats_table <- stats_table %>% select(any_of(select_cols))
  }
  
  config_rows <- stats_table %>%
    transpose()
  
  result <- map_dfr(config_rows, \(config_row) map_fn(page, config_row))
  
  return(result)
}

