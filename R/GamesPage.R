GamesPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("GamesPage", class(page)))
}

get_page_node.GamesPage <- function(page) {
  # params_grid <- join_config_stat(page$config, letters[1:3]) %>% transpose()
  # params_grid %>% 
  #   map_dfr(\(config_row) {
  #     base_get_page_node(
  #       page = page,
  #       config = config_row,
  #       clean_fn = get_clean_players_table,
  #       join_fn = function(view, identifier) {
  #         join_players_identifier(view, identifier)
  #       },
  #       mutate_fn = function(data, view) {
  #         data %>%
  #           join_players_active(get_players_active(view)) %>%
  #           join_players_college(get_players_college(view)) %>%
  #           mutate(type = page$config$type) %>%
  #           relocate(type, id, active)
  #       },
  #       filter_fn = function(data) {
  #         data %>% mutate(row_number = row_number())
  #       }
  #     )
  # })
}