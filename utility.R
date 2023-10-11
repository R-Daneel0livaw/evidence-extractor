
discover_page <- function(url) {
    function(identifier) {
      read_html(url) %>%
        html_element(identifier)
    }
}