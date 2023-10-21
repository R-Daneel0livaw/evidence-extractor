discover_page <- function(url) {
    function(identifier) {
      read_html(url) %>%
        html_element(identifier)
    }
}

convert_to_stats <- function(stat_data, stats_start) {
  filtered_df <- select_until_end(stat_data, stats_start)
  
  stats <-
    map2(
      names(filtered_df),
      filtered_df,
      \(name, value, connector_id, connector_type) data.frame(
        name,
        value = as.character(value),
        connector_id,
        connector_type,
        type = "STAT"
      ),
      stat_data$id,
      stat_data$type
    ) %>%
    bind_rows() %>%
    filter(nzchar(value)) %>% 
    relocate(type)
  
  stats
}

select_until_end <- function(data, start) {
  filtered_df <-
    data %>%
    select((which(names(.) == start)):last_col())
  
  filtered_df
}