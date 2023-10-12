


get_player_df <- function() {
  players_table <-
    letters[1:3] %>%
    map_dfr(\(letter) scrape_players(letter))
  
  players_table
}

get_college_df <- function(players_table) {
  colleges_table <-
    players_table %>%
    select(colleges) %>%
    mutate(type = "COLLEGE") %>%
    separate_rows(colleges, sep = ", ") %>%
    filter(nzchar(colleges)) %>%
    separate_wider_delim(colleges, names = c("name", "id"), delim = "/") %>%
    distinct() %>%
    arrange(name)
  
  colleges_table
}

scrape_players <- function(letter) {
  players_page <- discover_page(paste0("https://www.basketball-reference.com/players/", letter, "/"))
  players_view <- players_page("table#players")

  players_initial_table <-
    players_view %>%
    html_table() %>%
    mutate(Player = str_replace_all(Player, "\\*", ""),
           row_number = row_number())
  
  players_identifier <- 
    players_view %>%
    html_elements("tr th[data-stat='player'] a") %>%
    html_attrs_dfr() %>% 
    rename_all(~ c("id", "player")) %>% 
    mutate(id = str_extract(id, "[^/]+(?=\\.html$)"),
           row_number = row_number())

  players_identifier_table <-
    players_initial_table %>%
    left_join(players_identifier, by = c("Player" = "player", "row_number"))
  
  players_active_identifier <-
    players_view %>%
    html_elements("tr th[data-stat='player'] strong a") %>%
    html_attrs_dfr(add_text = FALSE) %>% 
    rename_all(~ c("id")) %>% 
    mutate(id = str_extract(id, "[^/]+(?=\\.html$)"),
           active = TRUE)

  players_table <-
    players_identifier_table %>%
    left_join(players_active_identifier, by = join_by(id)) %>%
    mutate(
      active = !is.na(active),
      `Birth Date` = as.Date(`Birth Date`, format = "%B %d, %Y"),
      type = "PLAYER"
    ) %>%
    select(!row_number) %>%
    clean_names()
  
  players_college_identifier <-
    players_view %>%
    html_elements("tr td[data-stat='colleges'] a") %>%
    html_attrs_dfr() %>% 
    rename_all(~ c("college_id", "college_name")) %>% 
    mutate(id = str_extract(college_id, "(?<=college=).*")) %>% 
    distinct(college_id, .keep_all = TRUE)

  # look into library(fuzzyjoin) to eliminate need to split on comma which is causing NAs.
  players_table <- 
    players_table %>%
    separate_longer_delim(colleges, ", ") %>%
    left_join(
      players_college_identifier,
      by = c("colleges" = "college_name"),
      multiple = "first"
    ) %>%
    mutate(colleges = ifelse(
      nzchar(colleges),
      paste0(colleges, "/", college_id),
      colleges
    )) %>%
    mutate(colleges = str_c(colleges, collapse = ", "),
           .by = id) %>%
    distinct(player, id, .keep_all = TRUE) %>%
    select(!college_id)
  
  players_table
}