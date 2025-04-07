GamesPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("GamesPage", class(page)))
}

get_page_node.GamesPage <- function(page, dependent_nodes) {
  params_grid <- join_config_stat(page$config, dependent_nodes$id[2], str_to_lower(month.name[10:12])) %>% transpose()
  params_grid %>%
    map_dfr(\(config_row) {
      base_get_page_node(
        page = page,
        config = config_row,
        clean_fn = get_clean_games_table,
        join_fn = function(view, identifier) {
          join_games_identifier(view, identifier) 
        },
        mutate_fn = function(data, view) {
          data %>%
            join_games_identifier(get_visitor(view)) %>%
            join_games_identifier(get_home(view)) %>% 
            mutate(type = page$config$type,
                   season = config_row$stat1) %>%
            relocate(type, id, date, start_et, ot, arena, attend)
        },
        filter_fn = function(data) {
          data %>% mutate(row_number = row_number())
        },
        select_cols =  c("-row_number", "-notes", "visitor", "home")
      )
  })
}

get_page_node_stats.GamesPage <- function(page, base_nodes = NULL) {
  base_get_page_node(
    orig_data = base_nodes,
    config = page$config,
    stats_fn = function(data, config) {
        convert_to_stats(data, config$start) %>% 
        filter(name %in% c("visitor_pts", "home_pts"))
    }
  )
}

get_page_multi_node_stats.GamesPage <- function(page, base_nodes) {
  join_fn <- function(page, base_nodes) {
    join_config_stat(add_index_column(page$config), base_nodes$id[1]) %>%
      left_join(get_game_df(), by = c("stat" = "id"))
  }
  # 
  # mutate_fn <- function(stats_table) {
  #   stats_table %>%
  #     mutate(stat_sort = as.numeric(str_extract(.data[[names(.)[str_detect(names(.), "^stat\\d+$")][1]]], ".+_(\\d+)", 1))) %>%
  #     arrange(stat_sort, desc(stat_sort)) %>%
  #     select(-stat_sort)
  # }
  # 
  # map_fn <- function(page, config_row) {
  #   get_seasons_teams_stats(config_row, page)
  # }
  # 
  # base_get_config_rows(
  #   page = page, 
  #   base_nodes = base_nodes, 
  #   join_fn = join_fn, 
  #   mutate_fn = mutate_fn, 
  #   map_fn = map_fn
  # )
}

get_games_players_stats <- function(config_row, page) {
  # base_get_page_node(
  #   page = page,
  #   config = config_row,
  #   clean_fn = get_clean_seasons_teams_stats_table,
  #   join_fn = function(view, identifier) {
  #     join_identifier(view, identifier, team) %>%
  #       filter(!is.na(id))
  #   },
  #   mutate_fn = function(data, view) {
  #     data %>% mutate(type = config_row$secondary_type) %>% 
  #       relocate(type, id)
  #   },
  #   select_cols = c("-team", "-ranker"),
  #   rename_fn = rename_stats,
  #   stats_fn = function(data, config) {
  #     data %>%
  #       convert_to_stats(config$start) %>%
  #       mutate(id = get_uuid(nrow(.))) %>%
  #       relocate(type, id) %>%
  #       duplicate_stats(config$type, config$stat)
  #   }
  # )
}

get_clean_games_table <- function(view) {
  games_initial_table <-
    view %>%
    get_clean_table() %>% 
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

join_games_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, row_number)
  
  joined_table
}

get_visitor <- function(view) {
  visitor_identifier <-
    extract_identifier(view = view,
                       identifier = "tr td[data-stat='visitor_team_name'] a",
                       names = c("visitor_id"),
                       add_text = FALSE,
                       visitor_id = str_extract(visitor_id,  ".*/teams/([^/]+)/.*", 1)) %>%
    mutate(row_number = row_number())
  
  visitor_identifier
}

get_home <- function(view) {
  home_identifier <-
    extract_identifier(view = view,
                       identifier = "tr td[data-stat='home_team_name'] a",
                       names = c("home_id"),
                       add_text = FALSE,
                       home_id = str_extract(home_id,  ".*/teams/([^/]+)/.*", 1)) %>%
    mutate(row_number = row_number())
  
  home_identifier
}