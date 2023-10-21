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
    join_identifier(get_clean_seasons_stats_table(seasons_view),
                            identifier,
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

join_seasons_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table, identifier, season)
  
  joined_table
}