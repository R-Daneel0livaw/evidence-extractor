get_game_df <- function() {
  games_table <-
    month.name[10:12] %>%
    map2_dfr(2023, \(month, season) get_games_group(str_to_lower(month), season))
  
  games_table
}

m_get_game_df <- memoise(get_game_df)

get_games_group <- function(month, season) {
  games_page <- discover_page(paste0("https://www.basketball-reference.com/leagues/NBA_", season,
    "_games-", month, ".html"))
  games_view <- games_page("table#schedule")

  identifier <-
    extract_identifier(view = games_view,
                       identifier = "tr td[data-stat='box_score_text'] a",
                       names = c("id"),
                       add_text = FALSE,
                       id = str_extract(id, ".*/([^.]+)\\.html$", 1)) %>%
    mutate(row_number = row_number())

  games_table <-
    join_games_identifier(get_clean_games_table(games_view), identifier) %>% 
    mutate(type = "GAME") %>%
    relocate(type, id, date, start_et, ot, arena, attend) %>%
    select(!c(row_number, notes, home_pts, visitor_pts))
  
  games_table
}

get_game_player_stats <- function() {
  games_players_stats_table <-  
    get_game_df()$id[1] %>% 
    map_dfr(\(game) get_games_group(str_to_lower(month), season))
    
    
    
    
    # join_config_stat(get_season_team_config(), m_get_season_df()$id[2:3]) %>%  
    # mutate(stat_sort = as.numeric(str_extract(stat, ".+_(\\d+)", 1))) %>%
    # arrange(stat_sort, desc(stat_sort)) %>% 
    # select(-stat_sort) %>% 
    # transpose() %>% 
    # map_dfr(\(config_row) get_seasons_teams_stats_group(config_row))
  
  games_players_stats_table
}

m_get_game_player_stats <- memoise(get_game_player_stats)

get_games_players_stats_group <- function(game) {
  games_players_stats_page <- discover_page(paste0("https://www.basketball-reference.com/boxscores/", game, ".html"))
  view <- games_players_stats_page("")
}

get_clean_games_table <- function(view) {
  games_initial_table <-
    view %>%
    get_clean_table() %>% 
    rename(
      visitor = visitor_neutral,
      home = home_neutral,
      home_pts = pts_2,
      visitor_pts = pts,
      ot = x_2
    ) %>%
    mutate(
      row_number = row_number(),
      ot = ifelse(nzchar(ot), TRUE, FALSE),
      date = as.Date(date, format = "%a, %b %d, %Y")
    ) %>%
    select(!x)
  
  games_initial_table
}

join_games_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, row_number)
  
  joined_table
}