get_season_node_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_regex,
    "SEASON", "https://www.basketball-reference.com/leagues/", "table#stats", "tr th[data-stat='season'] a", "",  "", "", "", FALSE, FALSE, c("id", "season"), ".*/([A-Z]+_\\d+).html"
  ) 
  data
}

get_season_stats_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_regex,
    "SEASON", "https://www.basketball-reference.com/leagues/NBA_stats_per_game.html", "table#stats-Regular-Season", "tr td[data-stat='season'] a", "",  "age", "", "", FALSE, FALSE, c("id", "season"), ".*/([A-Z]+_\\d+).html"
  )
  data
}

get_season_team_stats_config <- function() {
  data <- tribble(
    ~type, ~secondary_type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_regex,
    "SEASON", "TEAM", "https://www.basketball-reference.com/leagues/{node}.html", "table#per_game-team", "tr td[data-stat='team'] a", "per_g",  "g", "pts", "mp", FALSE, FALSE, c("id", "team"), ".*/teams/([^/]+)/.*",
    "SEASON", "TEAM", "https://www.basketball-reference.com/leagues/{node}.html", "table#totals-team", "tr td[data-stat='team'] a", "",  "mp", "pts", "", FALSE, FALSE, c("id", "team"), ".*/teams/([^/]+)/.*",
    "SEASON", "TEAM", "https://www.basketball-reference.com/leagues/{node}.html", "table#advanced-team", "tr td[data-stat='team'] a", "",  "age", "pts", "", TRUE, TRUE, c("id", "team"), ".*/teams/([^/]+)/.*"
  )
  data
}

get_page_config <- function(page_type) {
  config <- get_config()
  page_config <- config %>% filter(page_type == page_type)
  if (nrow(page_config) == 0) {
    stop("No configuration found for page type: ", page_type)
  }
  return(page_config)
}