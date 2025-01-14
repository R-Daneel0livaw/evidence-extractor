SeasonsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("SeasonsPage", class(page)))
}

get_page_node.SeasonsPage <- function(page) {
  # return(nodes)
}

get_page_node_stats.SeasonsPage <- function(page) {
  # return(nodes)
}

get_page_multi_node_stats.SeasonsPage <- function(page) {
  # return(nodes)
}


# get_season_df.SeasonsPage <- function(page) {
#   seasons_view <- page$fetch_table("table#stats")
#   
#   identifier <- extract_identifier(
#     view = seasons_view,
#     identifier = page$config$identifier,
#     names = c("id", "season"),
#     id = str_extract(page$config$id_pattern, ".*/([A-Z]+_\\d+).html", 1)
#   ) %>% filter(str_detect(id, "NBA"))
#   
#   seasons_identifier_table <- identifier # Placeholder for actual join logic
#   
#   seasons_table <- seasons_identifier_table %>%
#     mutate(
#       start = as.numeric(str_replace(season, "-.*", "")),
#       end = start + 1,
#       type = "SEASON"
#     ) %>% select(type, id, season, start, end)
#   
#   return(seasons_table)
# }
