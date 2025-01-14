Page <- function(config) {
  page <- list(
    config = config,
    fetch_table = function(identifier) {
      message("Fetching page: ", config$url)
      page_content <- read_html(config$url) %>%
        html_element(identifier)
      return(page_content)
    }
  )
  structure(page, class = "Page")
}

get_page_node <- function(page) {
  UseMethod("get_page_nodes")
}

get_page_node_stats <- function(page) {
  UseMethod("get_page_node_stats")
}

get_page_multi_node_stats <- function(page) {
  UseMethod("get_page_multi_node_stats")
}