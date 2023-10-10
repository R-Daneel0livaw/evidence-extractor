

get_game_df <- function() {
  games_table <-
    month.name[10:12] %>%
    map2_dfr(2023, \(month, season) scrape_games(str_to_lower(month), season))
  
  games_table
}

scrape_games <- function(month, season) {
  games_url <-
    paste0(
      "https://www.basketball-reference.com/leagues/NBA_",
      season,
      "_games-",
      month,
      ".html"
    )
  
  games_page <-
    read_html(games_url) %>%
    html_element("table#schedule")
  
  games_initial_table <-
    games_page %>%
    html_table() %>%
    clean_names() %>%
    rename(all_of(
      c(
        visitor = "visitor_neutral",
        home = "home_neutral",
        home_pts = "pts_2",
        visitor_pts = "pts",
        ot = "x_2"
      )
    )) %>%
    mutate(
      row_number = row_number(),
      ot = ifelse(nzchar(ot), TRUE, FALSE),
      date = as.Date(date, format = "%a, %b %d, %Y")
    ) %>%
    select(!x)
  
  games_id <-
    games_page %>%
    html_elements("tr td[data-stat='box_score_text'] a") %>%
    html_attr("href") %>%
    str_extract(".*/([^.]+)\\.html$", 1)
  
  games_identifier <-
    tibble(id = games_id) %>%
    mutate(row_number = row_number())
  
  games_table <-
    games_initial_table %>%
    left_join(games_identifier, by = join_by(row_number)) %>%
    mutate(type = "GAME") %>%
    select(!row_number)
  
  games_table
}