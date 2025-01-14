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

discover_page <- function(page, identifier) {
  UseMethod("discover_page")
}

discover_page.Page <- function(page, identifier) {
  page$fetch_table(identifier)
}
