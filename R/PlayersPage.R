PlayersPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("PlayersPage", class(page)))
}

get_page_node.PlayersPage <- function(page) {
  params_grid <- expand_grid(page$config, stat = letters[1:3]) %>% transpose()
  params_grid %>% 
    map_dfr(\(config_row) {
      base_get_page_node(
        page = page,
        config = config_row,
        clean_fn = get_clean_players_table,
        join_fn = function(view, identifier) {
          join_players_identifier(view, identifier)
        },
        mutate_fn = function(data, view) {
          data %>%
            join_players_active(get_players_active(view)) %>%
            join_players_college(get_players_college(view)) %>%
            mutate(type = page$config$type) %>%
            relocate(type, id, active)
        },
        filter_fn = function(data) {
          data %>% mutate(row_number = row_number())
        }
      )
  })
}

get_page_node_stats.PlayersPage <- function(page, base_nodes = NULL) {
  params_grid <- expand_grid(page$config, stat = base_nodes$id[120]) %>%
    mutate(stat_sort = stat) %>%
    arrange(stat_sort, desc(stat_sort)) %>%
    select(-stat_sort) %>%
    transpose()
  params_grid %>%
    map_dfr(\(config_row) {
      print(config_row)
      # base_get_page_node(
      #   page = page,
      #   config = config_row,
      #   clean_fn = get_clean_players_table,
      #   join_fn = function(view, identifier) {
      #     join_players_identifier(view, identifier)
      #   },
      #   mutate_fn = function(data, view) {
      #     data %>%
      #       join_players_active(get_players_active(view)) %>%
      #       join_players_college(get_players_college(view)) %>%
      #       mutate(type = page$config$type) %>%
      #       relocate(type, id, active)
      #   },
      #   filter_fn = function(data) {
      #     data %>% mutate(row_number = row_number())
      #   }
      # )
    }) 
  # %>%
  #   distinct(name, connector_id, .keep_all = TRUE)
  
  # players_stats_table <-
  #   join_config_stat(page$config, base_nodes$id[120]) %>%
  #   mutate(stat_sort = stat) %>%
  #   arrange(stat_sort, desc(stat_sort)) %>%
  #   select(-stat_sort) %>%
  #   transpose() %>% 
  #   map_dfr(\(config_row) get_players_stats_group(config_row)) %>%
  #   distinct(name, connector_id, .keep_all = TRUE)
}

get_clean_players_table <- function(view) {
  players_initial_table <-
    view %>%
    get_clean_table() %>% 
    mutate(player = str_replace_all(player, "\\*", ""),
           row_number = row_number())
  
  players_initial_table
}

join_players_identifier <- function(view, identifier) {
  joined_table <-
    join_identifier(initial_table = view, identifier = identifier, player, row_number)
  
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