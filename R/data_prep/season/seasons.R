get_season_df <- function() {
  seasons_page <- discover_page("https://www.basketball-reference.com/leagues/")
  seasons_view <- seasons_page("table#stats")

  seasons_identifier_table <-
    join_identifier_columns(
      seasons_view,
      get_clean_seasons_table(seasons_view),
      "tr th[data-stat='season'] a",
      ".*/([A-Z]+_\\d+).html"
    )

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

  seasons_identifier_table <-
    join_identifier_columns(
      seasons_view,
      get_clean_seasons_stats_table(seasons_view),
      "tr td[data-stat='season'] a",
      ".*/([A-Z]+_\\d+).html"
    )
  
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
    html_table() %>% 
    row_to_names(row_number = 1) %>%
    clean_names() %>% 
    filter(lg == "NBA")
  
  seasons_initial_table
}

get_clean_seasons_stats_table <- function(view) {
  seasons_initial_table <- 
    view %>%
    html_table() %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    filter(lg == "NBA", g > 0) %>% 
    select(-c(rk, lg))
  
  seasons_initial_table
}

join_identifier_columns <- function(view, initial_table,
                                     identifier, extract) {
  seasons_identifier <-
    view %>%
    html_elements(identifier) %>%
    html_attrs_dfr() %>% 
    rename_all(~ c("id", "season")) %>% 
    mutate(id = str_extract(id, extract, 1)) %>% 
    filter(str_detect(id, "NBA"))
  
  seasons_identifier_table <-
    initial_table %>%
    left_join(seasons_identifier, by = join_by(season))
  
  seasons_identifier_table
}