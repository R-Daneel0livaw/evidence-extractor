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
  seasons_teams_stats_table <-  
    join_config_stat(get_season_team_config(), m_get_season_df()$id[2:3]) %>%  
    mutate(stat_sort = as.numeric(str_extract(stat, ".+_(\\d+)", 1))) %>%
    arrange(stat_sort, desc(stat_sort)) %>% 
    select(-stat_sort) %>% 
    transpose() %>% 
    map_dfr(\(config_row) get_seasons_teams_stats_group(config_row))
  
  
  seasons_teams_stats_table
}

m_get_season_team_stats <- memoise(get_season_team_stats)

get_seasons_teams_stats_group <- function(config_row) {
  seasons_team_stats_page <- discover_page(paste0("https://www.basketball-reference.com/leagues/", config_row$stat, ".html"))
  get_individual_seasons_teams_stats_group(config_row, seasons_team_stats_page(config_row$view))
}

get_individual_seasons_teams_stats_group <- function(config_row, view) {
  identifier <-
    extract_identifier(view = view,
                       identifier = "tr td[data-stat='team'] a",
                       name = c("id", "team"),
                       id = str_extract(id, ".*/teams/([^/]+)/.*", 1))

  seasons_identifier_table <-
    join_identifier(initial_table = get_clean_seasons_teams_stats_table(view, config_row$multi_row_header,
                                                                        config_row$dummy_header),
                    identifier = identifier,
                    team) %>%
    filter(!is.na(id))

  teams_stats_table <-
    seasons_identifier_table %>%
    mutate(type = "TEAM") %>%
    relocate(type, id) %>%
    select(-team,-ranker) %>%
    rename_stats(config_row$stat_suffix, config_row$rename_start, config_row$stats_end) 

  teams_stats <- convert_to_stats(teams_stats_table, config_row$stats_start) %>%
    mutate(id = get_uuid(nrow(.))) %>%
    relocate(type, id)

  seasons_teams_stats <- duplicate_stats(teams_stats, "SEASON", config_row$stat)
  
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

get_clean_seasons_teams_stats_table <- function(view, multi_row_header = FALSE, 
                                                dummy_header = FALSE) {
  seasons_initial_table <- 
    get_clean_table(view, multi_row_header) %>% 
    mutate(team = str_replace_all(team, "\\*", ""))

  colnames(seasons_initial_table) <- get_column_names(view, "tfoot tr:nth-child(1) > *")
 
  if(dummy_header) {
    seasons_initial_table <-
      seasons_initial_table %>% 
      select(-c(DUMMY))
  } 
  
  seasons_initial_table
}

get_season_team_config <- function() {
  data <- tribble(
    ~view, ~stat_suffix,  ~stats_start, ~stats_end, ~rename_start, ~multi_row_header, ~dummy_header,
    "table#per_game-team", "per_g",  "g", "pts", "mp", FALSE, FALSE,
    "table#totals-team", "",  "mp", "pts", "", FALSE, FALSE,
    "table#advanced-team", "",  "age", "pts", "", TRUE, TRUE
  )
  
  data
}

join_seasons_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, season)
  
  joined_table
}