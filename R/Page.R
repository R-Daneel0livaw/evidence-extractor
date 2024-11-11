Page <- function(config) {
  structure(
    list(config = config),
    class = "Page"
  )
}

discover_page <- function(page) {
  UseMethod("discover_page")
}

discover_page.Page <- function(page) {
  message("Fetching page: ", page$config$url)
  page_content <- "HTML_CONTENT"  
  return(page_content)
}