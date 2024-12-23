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
    clean_table <-
      clean_table %>% {
        suppressWarnings(row_to_names(., row_number = 1))
      }
  }
  
  clean_table <- clean_table %>% clean_names()
  
  clean_table
}

get_column_names <- function(view, identifier) {
  cloummn_names <-
    extract_identifier(view = view,
                       identifier = identifier,
                       names = c("data_stat"),
                       attrs = "data-stat",
                       add_text = FALSE)
  
  names <- cloummn_names$data_stat
  
  names
}

get_uuid <- function(amount) {
  uuids <- UUIDgenerate(n = amount)
  
  uuids
}

select_until_end <- function(data, start) {
  filtered_df <-
    data %>%
    select((which(names(.) == start)):last_col())
  
  filtered_df
}

extract_value <- function(view, identifier, name, ...) {
  extracted_value <-
    view %>%
    html_elements(identifier) %>%
    html_text() %>% 
    as.data.frame() %>% 
    setNames(name) %>% 
    mutate(...)
  
  extracted_value
}

extract_identifier <- function(view, identifier, names, 
                               attrs = NULL, add_text = TRUE, ...) {
  extracted_identifier <-
    view %>%
    html_elements(identifier) %>%
    html_attrs_dfr(attrs = attrs, add_text = add_text) %>%
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

join_config_stat <- function(config, stat) {
  joined_table <- expand_grid(config, stat = stat)
  
  joined_table
}

duplicate_stats <-
  function(base_stats, duplicate_type, duplicate_id) {
    stats <-
      base_stats %>%
      bind_rows(base_stats %>% mutate(connector_type = duplicate_type, connector_id = duplicate_id)) %>%
      arrange(id)
    
    stats
  }

rename_stats <- function(stats_table, suffix, start, end) {
  stats <- stats_table
  
  if (nzchar(suffix)) {
    stats <-
      stats_table %>%
      rename_with( ~ paste0(.x, "_", suffix),
                   .cols = starts_with(start):starts_with(end))
  }
  stats
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