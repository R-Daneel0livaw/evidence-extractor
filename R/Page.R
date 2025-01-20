Page <- function(config) {
  page <- list(
    config = config,
    fetch_table = function(identifier, dynamic_values = list()) {
      url <- config$url
      if (!is.null(dynamic_values) && length(dynamic_values) > 0) {
        for (key in names(dynamic_values)) {
          placeholder <- paste0("{{", key, "}}")  
          url <- gsub(placeholder, dynamic_values[[key]], url)
        }
      }
      message("Fetching page: ", url)
      page_content <- read_html(url) %>%
        html_element(identifier)
      return(page_content)
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