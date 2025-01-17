SeasonsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("SeasonsPage", class(page)))
}

get_page_node.SeasonsPage <- function(page) {
  seasons_view <- page$fetch_table(page$config$table_identifier)
  
  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = page$config$key_data_identifier,
                       names = c("id", "season"),
                       id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1)) %>% 
    filter(str_detect(id, "NBA"))
  
  seasons_identifier_table <-
    join_seasons_identifier(get_clean_seasons_table(seasons_view), identifier)
  
  seasons <-
    seasons_identifier_table %>%
    mutate(start = as.numeric(str_replace(season, "-.*", "")),
           end = start + 1,
           type = page$config$type) %>% 
    select(type, id, season, start, end)
  
  return(seasons)
}

get_page_node_stats.SeasonsPage <- function(page) {
  seasons_view <- page$fetch_table(page$config$table_identifier)

  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = page$config$key_data_identifier,
                       name = c("id", "season"),
                       id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1))

  seasons_identifier_table <-
    join_identifier(initial_table = get_clean_seasons_stats_table(seasons_view),
                    identifier = identifier,
                    season)

  seasons_stats_table <-
    seasons_identifier_table %>%
    mutate(type = page$config$type) %>%
    relocate(type, id) %>%
    select(-season)

  seasons_stats <- convert_to_stats(seasons_stats_table, "age")

  return(seasons_stats)
}

get_page_multi_node_stats.SeasonsPage <- function(page, base_nodes) {
  seasons_teams_stats <-  
    join_config_stat(page$config, base_nodes$id) %>%  
    mutate(stat_sort = as.numeric(str_extract(stat, ".+_(\\d+)", 1))) %>%
    arrange(stat_sort, desc(stat_sort)) %>% 
    select(-stat_sort) %>% 
    transpose() %>% 
    map_dfr(\(config_row) get_seasons_teams_stats_group(config_row))
  
  # seasons_teams_stats <-  
  #   join_config_stat(get_season_team_config(), m_get_season_df()$id[2:3]) %>%  
  #   mutate(stat_sort = as.numeric(str_extract(stat, ".+_(\\d+)", 1))) %>%
  #   arrange(stat_sort, desc(stat_sort)) %>% 
  #   select(-stat_sort) %>% 
  #   transpose() %>% 
  #   map_dfr(\(config_row) get_seasons_teams_stats_group(config_row))
  
  return(seasons_teams_stats)
}

get_seasons_teams_stats_group <- function(config_row) {
  seasons_team_stats_page <- discover_page(paste0(config_row$url, config_row$stat, ".html"))
  get_individual_seasons_teams_stats_group(config_row, page$fetch_table(page$config$table_identifier))
  # Need to somehow get the config_row$stat into the page fetch_table function as it won't be complete based off the url only
  
  # seasons_team_stats_page <- discover_page(paste0("https://www.basketball-reference.com/leagues/", config_row$stat, ".html"))
  # get_individual_seasons_teams_stats_group(config_row, seasons_team_stats_page(config_row$view))
}

get_individual_seasons_teams_stats_group <- function(config_row, view) {
  identifier <-
    extract_identifier(view = view,
                       identifier = page$config$key_data_identifier,
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
    mutate(type = config_row$secondary_type) %>%
    relocate(type, id) %>%
    select(-team,-ranker) %>%
    rename_stats(config_row$suffix, config_row$rename_start, config_row$end) 

  teams_stats <- convert_to_stats(teams_stats_table, config_row$start) %>%
    mutate(id = get_uuid(nrow(.))) %>%
    relocate(type, id)

  seasons_teams_stats <- duplicate_stats(teams_stats, config_row$type, config_row$stat)
  
  return(seasons_teams_stats)
}

join_seasons_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, season)
  
  joined_table
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
