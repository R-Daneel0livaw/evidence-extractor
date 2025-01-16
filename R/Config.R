get_season_node_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header,
    "SEASON", "https://www.basketball-reference.com/leagues/", "table#stats", "tr th[data-stat='season'] a", "",  "", "", "", FALSE, FALSE,
  )
  data
}

get_season_stats_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header,
    "SEASON", "https://www.basketball-reference.com/leagues/NBA_stats_per_game.html", "table#stats-Regular-Season", "tr td[data-stat='season'] a", "",  "age", "", "", FALSE, FALSE,
  )
  data
}