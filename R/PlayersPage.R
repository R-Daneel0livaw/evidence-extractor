PlayersPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("PlayersPage", class(page)))
}

get_page_node.PlayersPage <- function(page) {
  # base_get_page_node(
  #   page = page,
  #   clean_fn = get_clean_teams_table,
  #   join_fn = function(view, identifier) {
  #     join_teams_identifier(view, identifier)
  #   },
  #   mutate_fn = function(data) {
  #     data %>%
  #       join_teams_alternative_names(get_teams_alternative_names(data)) %>%
  #       mutate(type = page$config$type) %>%
  #       relocate(type, id, team, alternative_names)
  #   },
  #   filter_fn = function(data) {
  #     data %>% mutate(current = TRUE)
  #   },
  #   select_cols =  c("-current:level", "-lg")
  # )
}