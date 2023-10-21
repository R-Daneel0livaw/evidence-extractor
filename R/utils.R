discover_page <- function(url) {
    function(identifier) {
      read_html(url) %>%
        html_element(identifier)
    }
}

get_clean_table <- function(view, include_row_to_names = FALSE) {
  clean_table <-
    view %>%
    html_table()
  
  if (include_row_to_names) {
    clean_table <- clean_table %>% row_to_names(row_number = 1)
  }
  
  clean_table <- clean_table %>% clean_names()
  
  clean_table
}

select_until_end <- function(data, start) {
  filtered_df <-
    data %>%
    select((which(names(.) == start)):last_col())
  
  filtered_df
}

extract_identifier <- function(view, identifier, names, add_text = TRUE, ...) {
  extracted_identifier <-
    view %>%
    html_elements(identifier) %>%
    html_attrs_dfr(add_text = add_text) %>% 
    rename_all(~ names) %>% 
    mutate(...)
  
  extracted_identifier
}

join_identifier <- function(initial_table, identifier, ...) {
  joined_table <-
    initial_table %>%
    left_join(identifier, by = join_by(...))
  
  joined_table
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