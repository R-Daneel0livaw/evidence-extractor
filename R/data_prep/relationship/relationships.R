generate_relationships <- function(stat_table, relationship_type) {
  processed_data <- stat_table %>%
    group_by(id) %>%
    summarise(
      pairs = list(t(combn(connector_id, 2))),
      types = list(t(combn(connector_type, 2))),
      .groups = "drop"
    ) %>%
    unnest(c(pairs, types)) %>%
    mutate(
      from = ifelse(types[, 1] < types[, 2], pairs[, 2], pairs[, 1]),
      to = ifelse(types[, 1] < types[, 2], pairs[, 1], pairs[, 2])
    )
  
  finalize_relationships(processed_data, relationship_type)
}

generate_simple_relationships <- function(simple_df, relationship_type) {
  processed_data <- simple_df %>%
    rename(from = a, to = b)
  
  finalize_relationships(processed_data, relationship_type)
}

finalize_relationships <- function(data, relationship_type) {
  data %>%
    mutate(type = "RELATIONSHIP") %>%
    select(type, from, to) %>%
    distinct() %>%
    mutate(value = relationship_type)
}