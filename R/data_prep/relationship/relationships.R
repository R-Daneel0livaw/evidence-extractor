generate_relationships <- function(stat_table, relationship_type) {
  stat_table %>%
    group_by(id) %>%
    summarise(
      pairs = list(t(combn(connector_id, 2))),
      types = list(t(combn(connector_type, 2))),
      .groups = "drop"
    ) %>%
    unnest(c(pairs, types)) %>%
    mutate(
      type = "RELATIONSHIP",
      from = ifelse(types[, 1] < types[, 2], pairs[, 2], pairs[, 1]),
      to = ifelse(types[, 1] < types[, 2], pairs[, 1], pairs[, 2]),
    ) %>%
    distinct(to, from, type) %>%
    mutate(value = relationship_type) %>%
    select(type, from, to, value)
}