get_player_df <- function() {
  players_table <-
    letters[1:3] %>%
    map_dfr(\(letter) get_players_group(letter))
  
  players_table
}

m_get_player_df <- memoise(get_player_df)

get_player_top_stats <- function() {
  players_stats_table <-
    # m_get_player_df()$id[120] %>%
    # m_get_player_df()$id[1:3] %>%
    join_config_stat(get_player_top_stats_config(), m_get_player_df()$id[120]) %>%
    mutate(stat_sort = stat) %>%
    arrange(stat_sort, desc(stat_sort)) %>%
    select(-stat_sort) %>%
    map_dfr(\(id) get_players_stats_group(id))
  
  players_stats_table
}

m_get_player_top_stats <- memoise(get_player_top_stats)

get_college_df <- function() {
  colleges_table <-
    m_get_player_df() %>%
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

m_get_college_df <- memoise(get_college_df)

get_players_group <- function(letter) {
  players_page <- discover_page(paste0("https://www.basketball-reference.com/players/", letter, "/"))
  players_view <- players_page("table#players")

  identifier <-
    extract_identifier(view = players_view,
                       identifier = "tr th[data-stat='player'] a",
                       names = c("id", "player"),
                       id = str_extract(id, "[^/]+(?=\\.html$)")) %>% 
    mutate(row_number = row_number())
  
  players_identifier_table <-
    join_players_identifier(get_clean_players_table(players_view), identifier)

  players_table <-
    players_identifier_table %>% 
    join_players_active(get_players_active(players_view)) %>%
    join_players_college(get_players_college(players_view)) %>% 
    mutate(type = "PLAYER") %>%
    relocate(type, id, active)

  players_table
}

get_players_stats_group <- function(player) {
  players_page <- discover_page(paste0("https://www.basketball-reference.com/players/",
                                       str_sub(player, 1, 1), "/", player, ".html"))
  views <- c("table#per_game_stats",
             # , "table#totals", 
             "table#advanced"
             )
  
  players_stats <-
    views %>%
    map_dfr(\(id) get_individual_players_stats_group(player, players_page(id))) %>%
    distinct(name, connector_id, .keep_all = TRUE)

  players_stats
}

get_individual_players_stats_group <- function(player, view) {
  identifier <-
    extract_identifier(view = view,
                       identifier = "tfoot tr[id] > *",
                       # identifier = "tfoot tr:nth-child(1) > *",
                       names = c("data_stat", "text"),
                       attrs = "data-stat",
                       id = player) %>% 
    filter(data_stat != "DUMMY")
  
  players_stats_table <-
    identifier %>%
    pivot_wider(names_from = data_stat, values_from = text) %>%
    mutate(type = "PLAYER") %>%
    relocate(type, id) %>%
    select(!(year_id:pos))
  
    players_stats <- convert_to_stats(players_stats_table, "games")
  
    players_stats
}

get_clean_players_table <- function(view) {
  players_initial_table <-
    view %>%
    get_clean_table() %>% 
    mutate(player = str_replace_all(player, "\\*", ""),
           row_number = row_number())
  
  players_initial_table
}

join_players_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, player, row_number)
  
  joined_table
}

get_players_active <- function(view) {
  players_active_identifier <-
    extract_identifier(
      view = view,
      identifier = "tr th[data-stat='player'] strong a",
      names = c("id"),
      add_text = FALSE,
      id = str_extract(id, "[^/]+(?=\\.html$)")
    ) %>% 
    mutate(active = TRUE)
  
  players_active_identifier
}

join_players_active <- function(players_identifier_table, players_active_identifier) {
  players_table <-
    join_identifier(players_identifier_table, players_active_identifier, id) %>%
    mutate(
      active = !is.na(active),
      birth_date = as.Date(birth_date, format = "%B %d, %Y")
    ) %>%
    select(!row_number)
  
  players_table
}

get_players_college <- function(view) {
  players_college_identifier <-
    extract_identifier(
      view = view,
      identifier = "tr td[data-stat='colleges'] a",
      names = c("college_id", "college_name"),
      college_id = str_extract(college_id, "(?<=college=).*")
    ) %>% 
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

get_player_top_stats_config <- function() {
  data <- tribble(
    ~view, ~stat_suffix,  ~stats_start, ~stats_end, ~rename_start, ~multi_row_header, ~dummy_header, ~identifier,
    "table#per_game_stats", "",  "games", "pts", "", FALSE, FALSE, "tfoot tr[id] > *",
    # "table#totals", "",  "mp", "pts", "", FALSE, FALSE, "tfoot tr:nth-child(1) > *",
    "table#advanced", "",  "games", "pts", "", FALSE, FALSE, "tfoot tr[id] > *"
  )
  
  data
}
