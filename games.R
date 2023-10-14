get_game_df <- function() {
  games_table <-
    month.name[10:12] %>%
    map2_dfr(2023, \(month, season) get_games_group(str_to_lower(month), season))
  
  games_table
}

get_games_group <- function(month, season) {
  games_page <- discover_page(paste0("https://www.basketball-reference.com/leagues/NBA_", season,
    "_games-", month, ".html"))
  games_view <- games_page("table#schedule")

  games_identifier <- get_games_identifier(games_view)

  games_table <-
    get_clean_games_table(games_view) %>%
    left_join(games_identifier, by = join_by(row_number)) %>%
    mutate(type = "GAME") %>%
    select(!row_number)
  
  games_table
}

m_get_game_df <- memoise(get_game_df)

get_clean_games_table <- function(view) {
  games_initial_table <-
    view %>%
    html_table() %>%
    clean_names() %>%
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

get_games_identifier <- function(view) {
  games_identifier <-
    view %>%
    html_elements("tr td[data-stat='box_score_text'] a") %>%
    html_attrs_dfr(add_text = FALSE) %>% 
    rename_all(~ c("id")) %>% 
    mutate(id = str_extract(id, ".*/([^.]+)\\.html$", 1),
           row_number = row_number())
  
  games_identifier
}