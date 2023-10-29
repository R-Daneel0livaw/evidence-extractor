get_season_df <- function() {
  seasons_page <- discover_page("https://www.basketball-reference.com/leagues/")
  seasons_view <- seasons_page("table#stats")

  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = "tr th[data-stat='season'] a",
                       names = c("id", "season"),
                       id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1)) %>% 
    filter(str_detect(id, "NBA"))
  
  seasons_identifier_table <-
    join_seasons_identifier(get_clean_seasons_table(seasons_view), identifier)

  seasons_table <-
    seasons_identifier_table %>%
    mutate(start = as.numeric(str_replace(season, "-.*", "")),
           end = start + 1,
           type = "SEASON") %>% 
    select(type, id, season, start, end)

  seasons_table
}

m_get_season_df <- memoise(get_season_df)

get_season_top_stats <- function() {
  seasons_stats_page <- discover_page("https://www.basketball-reference.com/leagues/NBA_stats_per_game.html")
  seasons_view <- seasons_stats_page("table#stats")

  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = "tr td[data-stat='season'] a",
                       name = c("id", "season"),
                       id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1))
  
  seasons_identifier_table <-
    join_identifier(initial_table = get_clean_seasons_stats_table(seasons_view),
                    identifier = identifier,
                    season)
  
  seasons_stats_table <-
    seasons_identifier_table %>%
    mutate(type = "SEASON") %>%
    relocate(type, id) %>%
    select(-season)
  
  seasons_stats <- convert_to_stats(seasons_stats_table, "age")
    
  seasons_stats
}

m_get_season_top_stats <- memoise(get_season_top_stats)

get_season_team_stats <- function() {
  seasons_team_stats_page <- discover_page("https://www.basketball-reference.com/leagues/NBA_2023.html")
  seasons_view <- seasons_team_stats_page("table#per_game-team")
  
  season_team_stats_table <- get_clean_seasons_teams_stats_table(seasons_view)
  
  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = "tr td[data-stat='team'] a",
                       name = c("id", "team"),
                       id = str_extract(id, ".*/teams/([^/]+)/.*", 1))

  seasons_identifier_table <-
    join_identifier(initial_table = season_team_stats_table,
                    identifier = identifier,
                    team) %>%
    filter(!is.na(id))

  teams_stats_table <-
    seasons_identifier_table %>%
    mutate(type = "TEAM") %>%
    relocate(type, id) %>%
    select(-team, -ranker)
  
  teams_stats <- convert_to_stats(teams_stats_table, "g") %>% 
    mutate(id = get_uuid(nrow(.))) %>% 
    relocate(type, id)
  
  seasons_teams_stats <-
    teams_stats %>%
    bind_rows(teams_stats %>% mutate(connector_type = "SEASON", connector_id = "NBA_2023")) %>% 
    arrange(id)

  seasons_teams_stats
}


get_clean_seasons_table <- function(view) {
  seasons_initial_table <-
    view %>%
    get_clean_table(TRUE) %>% 
    filter(lg == "NBA")
  
  seasons_initial_table
}

get_clean_seasons_stats_table <- function(view) {
  seasons_initial_table <- 
    view %>%
    get_clean_table(TRUE) %>% 
    filter(lg == "NBA", g > 0) %>% 
    select(-c(rk, lg))
  
  seasons_initial_table
}

get_clean_seasons_teams_stats_table <- function(view) {
  seasons_initial_table <- 
    get_clean_table(view) %>% 
    mutate(team = str_replace_all(team, "\\*", ""))
  
  cloummn_names <-
    extract_identifier(view = view,
                       identifier = "tfoot tr:nth-child(1) > *",
                       names = c("data_stat"),
                       attrs = "data-stat",
                       add_text = FALSE)
  
  colnames(seasons_initial_table) <- cloummn_names$data_stat
  
  seasons_initial_table
}

join_seasons_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, season)
  
  joined_table
}