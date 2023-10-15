get_player_df <- function() {
  players_table <-
    letters[1:3] %>%
    map_dfr(\(letter) get_players_group(letter))
  
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
    arrange(name) %>% 
    relocate(type, id)
  
  colleges_table
}

m_get_player_df <- memoise(get_player_df)
m_get_college_df <- memoise(get_college_df)

get_players_group <- function(letter) {
  players_page <- discover_page(paste0("https://www.basketball-reference.com/players/", letter, "/"))
  players_view <- players_page("table#players")

  players_identifier_table <-
    join_identifier_columns(players_view, get_clean_players_table(players_view))

  players_active_identifier <- get_players_active(players_view)
  players_college_identifier <- get_players_college(players_view)
    
  players_table <-
    players_identifier_table %>% 
    join_players_active(players_active_identifier) %>%
    join_players_college(players_college_identifier) %>% 
    relocate(type, id, active)

  players_table
}

get_clean_players_table <- function(view) {
  players_initial_table <-
    view %>%
    html_table() %>%
    clean_names() %>%
    mutate(player = str_replace_all(player, "\\*", ""),
           row_number = row_number())
  
  players_initial_table
}

join_identifier_columns <- function(view, initial_table) {
  players_identifier <- 
    view %>%
    html_elements("tr th[data-stat='player'] a") %>%
    html_attrs_dfr() %>%
    rename_all( ~ c("id", "player")) %>%
    mutate(id = str_extract(id, "[^/]+(?=\\.html$)"),
           row_number = row_number())
  
  players_identifier_table <-
    initial_table %>%
    left_join(players_identifier, by = c("player", "row_number"))
  
  players_identifier_table
}

get_players_active <- function(view) {
  players_active_identifier <-
    view %>%
    html_elements("tr th[data-stat='player'] strong a") %>%
    html_attrs_dfr(add_text = FALSE) %>% 
    rename_all(~ c("id")) %>% 
    mutate(id = str_extract(id, "[^/]+(?=\\.html$)"),
           active = TRUE)
  
  players_active_identifier
}

join_players_active <- function(players_identifier_table, players_active_identifier) {
  players_table <-
    players_identifier_table %>%
    left_join(players_active_identifier, by = join_by(id)) %>%
    mutate(
      active = !is.na(active),
      birth_date = as.Date(birth_date, format = "%B %d, %Y"),
      type = "PLAYER"
    ) %>%
    select(!row_number)
  
  players_table
}

get_players_college <- function(view) {
  players_college_identifier <-
    view %>%
    html_elements("tr td[data-stat='colleges'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("college_id", "college_name")) %>%
    mutate(college_id = str_extract(college_id, "(?<=college=).*")) %>%
    distinct(college_id, .keep_all = TRUE)
  
  players_college_identifier
}

# look into library(fuzzyjoin) to eliminate need to split on comma which is causing NAs.
join_players_college <- function(players_table, players_college_identifier) {
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