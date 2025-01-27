SeasonsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("SeasonsPage", class(page)))
}

get_page_node.SeasonsPage <- function(page) {
  base_get_page_node(
    page = page,
    clean_fn = get_clean_seasons_table,  
    join_fn = join_seasons_identifier, 
    mutate_fn = function(data) {
      data %>%
        mutate(
          start = as.numeric(str_replace(season, "-.*", "")),
          end = start + 1,
          type = page$config$type
        )
    },
    filter_fn = function(data) {
      data %>% filter(str_detect(id, "NBA"))
    },
    select_cols = c("type", "id", "season", "start", "end")
  )
}

get_page_node_stats.SeasonsPage <- function(page) {
  base_get_page_node(
    page = page,
    clean_fn = get_clean_seasons_stats_table, 
    join_fn = join_seasons_identifier,
    mutate_fn = function(data) {
      data %>%
        mutate(type = page$config$type) %>%
        relocate(type, id) %>%
        convert_to_stats(page$config$start) 
    },
    select_cols = c("-season") 
  )
}

get_page_multi_node_stats.SeasonsPage <- function(page, base_nodes) {
  join_fn <- function(base_nodes) {
    join_config_stat(add_index_column(page$config), base_nodes$id[2:3])
  }
  
  mutate_fn <- function(stats_table) {
    stats_table %>%
      mutate(stat_sort = as.numeric(str_extract(stat, ".+_(\\d+)", 1))) %>%
      arrange(stat_sort, desc(stat_sort)) %>%
      select(-stat_sort)
  }
  
  map_fn <- function(config_row) {
    get_seasons_teams_stats_group(config_row, page)
  }
  
  base_get_config_rows(
    page = page, 
    base_nodes = base_nodes, 
    join_fn = join_fn, 
    mutate_fn = mutate_fn, 
    map_fn = map_fn
  )
}

get_seasons_teams_stats_group <- function(config_row, page) {
  get_individual_seasons_teams_stats_group(config_row, page$fetch_table(identifier = config_row$table_identifier, 
                                                                        dynamic_values = list(season = config_row$stat),
                                                                        index = config_row$index), page)
}

get_individual_seasons_teams_stats_group <- function(config_row, view, page) {
  identifier <-
    extract_identifier(view = view,
                       identifier = config_row$key_data_identifier,
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
