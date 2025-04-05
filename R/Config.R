get_page_config <- function(page_type, secondary_type = NULL) {
  config_functions <- list(
    "SEASON" = get_season_node_config,
    "SEASON_STATS" = get_season_stats_config,
    "SEASON_TEAM_STATS" = get_season_team_stats_config,
    "TEAM" = get_team_node_config,
    "TEAM_STATS" = get_team_stats_config,
    "PLAYER" = get_player_node_config,
    "PLAYER_STATS" = get_player_stats_config,
    "GAME" = get_game_node_config,
    "GAME_STATS" = get_game_stats_config,
    "GAME_PLAYER_STATS" = get_game_player_stats_config
  )
  
  if (!page_type %in% names(config_functions)) {
    stop("Invalid page type: ", page_type)
  }
  
  config_data <- config_functions[[page_type]]()
  
  if (!is.null(secondary_type) && "secondary_type" %in% colnames(config_data)) {
    config_data <- config_data %>% filter(secondary_type == !!secondary_type)
  }
  
  if (nrow(config_data) == 0) {
    stop("No configuration found for page type: ", page_type, 
         if (!is.null(secondary_type)) paste(" and secondary type: ", secondary_type))
  }
  
  return(config_data)
}

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
    "SEASON", "TEAM", "https://www.basketball-reference.com/leagues/{node1}.html", "table#per_game-team", "tr td[data-stat='team'] a", "per_g",  "g", "pts", "mp", FALSE, FALSE, c("id", "team"), ".*/teams/([^/]+)/.*",
    "SEASON", "TEAM", "https://www.basketball-reference.com/leagues/{node1}.html", "table#totals-team", "tr td[data-stat='team'] a", "",  "mp", "pts", "", FALSE, FALSE, c("id", "team"), ".*/teams/([^/]+)/.*",
    "SEASON", "TEAM", "https://www.basketball-reference.com/leagues/{node1}.html", "table#advanced-team", "tr td[data-stat='team'] a", "",  "age", "pts", "", TRUE, TRUE, c("id", "team"), ".*/teams/([^/]+)/.*"
  )
  data
}

get_team_node_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_regex,
    "TEAM", "https://www.basketball-reference.com/teams/", "table#teams_active", "tr th[data-stat$='name'] a", "",  "", "", "", FALSE, FALSE, c("id", "team"), ".*/([^/]+)/$"
  ) 
  data
}

get_team_stats_config <- function() {
  data <- tribble(
    ~start,
    "g" 
  ) 
  data
}

get_player_node_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_regex,
    "PLAYER", "https://www.basketball-reference.com/players/{node1}/", "table#players", "tr th[data-stat='player'] a", "",  "", "", "", FALSE, FALSE, c("id", "player"), "([^/]+)(?=\\.html$)"
  ) 
  data
}

get_player_stats_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_attrs, ~id_extract_regex,
    "PLAYER", "https://www.basketball-reference.com/players/{node1}/{node2}.html", "table#per_game_stats", "tfoot tr[id] > *", "", "games",  "pts_per_g", "", FALSE, FALSE, c("data_stat", "text"), "data-stat", "",
    "PLAYER", "https://www.basketball-reference.com/players/{node1}/{node2}.html", "table#totals_stats", "tfoot tr:nth-child(1) > *", "", "fg",  "tpl_dbl", "", FALSE, TRUE, c("data_stat", "text"), "data-stat", "",
    "PLAYER", "https://www.basketball-reference.com/players/{node1}/{node2}.html", "table#advanced", "tfoot tr[id] > *", "", "games",  "vorp", "", FALSE, FALSE, c("data_stat", "text"), "data-stat", ""
  )
  data
}

get_game_node_config <- function() {
  data <- tribble(
    ~type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~add_text, ~id_extract_names, ~id_extract_regex,
    "GAME", "https://www.basketball-reference.com/leagues/{node1}_games-{node2}.html", "table#schedule", "tr td[data-stat='box_score_text'] a", "",  "", "", "", FALSE, FALSE, FALSE, c("id"), ".*/([^.]+)\\.html$"
  ) 
  data
}

get_game_stats_config <- function() {
  data <- tribble(
    ~start,
    "visitor_pts" 
  ) 
  data
}

get_game_player_stats_config <- function() {
  data <- tribble(
    ~type, ~secondary_type, ~url, ~table_identifier, ~key_data_identifier, ~suffix,  ~start, ~end, ~rename_start, ~multi_row_header, ~dummy_header, ~id_extract_names, ~id_extract_regex,
    # "GAME", "PLAYER", "https://www.basketball-reference.com/boxscores/{node1}.html", "table#box-{{DYNAMIC}}-game-basic", "tr th[data-stat='player'] a", "",  "mp", "plus_minus", "", TRUE, FALSE, c("id", "player"), "(?<=/players/[a-z]/)[a-z0-9]+",
    # "GAME", "PLAYER", "https://www.basketball-reference.com/boxscores/{node1}.html", "table#box-{{DYNAMIC}}-game-advanced", "tr th[data-stat='player'] a", "",  "ts_pct", "bpm", "", TRUE, FALSE, c("id", "player"), "(?<=/players/[a-z]/)[a-z0-9]+",
    # "GAME", "PLAYER", "https://www.basketball-reference.com/boxscores/{node1}.html", "table#box-{{DYNAMIC}}-game-basic", "tr th[data-stat='player'] a", "",  "mp", "plus_minus", "", TRUE, FALSE, c("id", "player"), "(?<=/players/[a-z]/)[a-z0-9]+"
    # "GAME", "PLAYER", "https://www.basketball-reference.com/boxscores/{node1}.html", "table#box-{{DYNAMIC}}-game-advanced", "tr th[data-stat='player'] a", "",  "ts_pct", "bpm", "", TRUE, FALSE, c("id", "player"), "(?<=/players/[a-z]/)[a-z0-9]+"
  )
  data
}