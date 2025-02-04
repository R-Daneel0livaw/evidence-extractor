TeamsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("TeamsPage", class(page)))
}

get_page_node.TeamsPage <- function(page) {
  # base_get_page_node(
  #   page = page,
  #   clean_fn = get_clean_seasons_table,  
  #   join_fn = function(view, identifier) {
  #     join_identifier(view, identifier, season)
  #   },
  #   mutate_fn = function(data) {
  #     data %>%
  #       mutate(
  #         start = as.numeric(str_replace(season, "-.*", "")),
  #         end = start + 1,
  #         type = page$config$type
  #       )
  #   },
  #   filter_fn = function(data) {
  #     data %>% filter(str_detect(id, "NBA"))
  #   },
  #   select_cols = c("type", "id", "season", "start", "end")
  # )
}
