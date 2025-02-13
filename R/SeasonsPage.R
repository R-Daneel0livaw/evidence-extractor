SeasonsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("SeasonsPage", class(page)))
}

get_page_node.SeasonsPage <- function(page) {
  base_get_page_node(
    page = page,
    clean_fn = get_clean_seasons_table,  
    join_fn = function(view, identifier) {
      join_identifier(view, identifier, season)
    },
    mutate_fn = function(data, view) {
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
    join_fn = function(view, identifier) {
      join_identifier(view, identifier, season)
    },
    mutate_fn = function(data, view) {
      data %>%
        mutate(type = page$config$type) %>%
        relocate(type, id)
    },
    select_cols = c("-season"),
    stats_fn = function(data, config) {
        convert_to_stats(data, config$start) 
    }
  )
}

get_page_multi_node_stats.SeasonsPage <- function(page, base_nodes) {
  join_fn <- function(page, base_nodes) {
    join_config_stat(add_index_column(page$config), base_nodes$id[2:3])
  }
  
  mutate_fn <- function(stats_table) {
    stats_table %>%
      mutate(stat_sort = as.numeric(str_extract(stat, ".+_(\\d+)", 1))) %>%
      arrange(stat_sort, desc(stat_sort)) %>%
      select(-stat_sort)
  }
  
  map_fn <- function(page, config_row) {
    get_seasons_teams_stats(config_row, page)
  }
  
  base_get_config_rows(
    page = page, 
    base_nodes = base_nodes, 
    join_fn = join_fn, 
    mutate_fn = mutate_fn, 
    map_fn = map_fn
  )
}

get_seasons_teams_stats <- function(config_row, page) {
  base_get_page_node(
    page = page,
    config = config_row,
    clean_fn = get_clean_seasons_teams_stats_table,
    join_fn = function(view, identifier) {
      join_identifier(view, identifier, team) %>%
        filter(!is.na(id))
    },
    mutate_fn = function(data, view) {
      data %>% mutate(type = config_row$secondary_type) %>% 
        relocate(type, id)
    },
    select_cols = c("-team", "-ranker"),
    rename_fn = rename_stats,
    stats_fn = function(data, config) {
      data %>%
        convert_to_stats(config$start) %>%
        mutate(id = get_uuid(nrow(.))) %>%
        relocate(type, id) %>%
        duplicate_stats(config$type, config$stat)
    }
  )
}

get_clean_seasons_table <- function(view) {
  get_clean_seasons_table_base(view, filter_nba = TRUE)
}

get_clean_seasons_stats_table <- function(view) {
  get_clean_seasons_table_base(view, filter_nba = TRUE, filter_games = TRUE, drop_columns = c("rk", "lg"))
}

get_clean_seasons_teams_stats_table <- function(view, multi_row_header = FALSE, dummy_header = FALSE) {
  cleaned_table <- get_clean_seasons_table_base(view, include_row_to_names = multi_row_header, clean_team_names = TRUE)
  
  colnames(cleaned_table) <- get_column_names(view, "tfoot tr:nth-child(1) > *")
  
  if(dummy_header) {
    cleaned_table <-
      cleaned_table %>%
      select(-c(DUMMY))
  }
  
  return(cleaned_table)
}

get_clean_seasons_table_base <- function(view, 
                                         include_row_to_names = TRUE,
                                         filter_nba = FALSE, 
                                         filter_games = FALSE, 
                                         drop_columns = NULL, 
                                         clean_team_names = FALSE) {
  
  cleaned_table <- get_clean_table(view, include_row_to_names)
  
  if (filter_nba) {
    cleaned_table <- cleaned_table %>% filter(lg == "NBA")
  }
  
  if (filter_games) {
    cleaned_table <- cleaned_table %>% filter(g > 0)
  }
  
  if (!is.null(drop_columns)) {
    cleaned_table <- cleaned_table %>% select(-all_of(drop_columns))
  }
  
  if (clean_team_names) {
    cleaned_table <- cleaned_table %>% mutate(team = str_replace_all(team, "\\*", ""))
  }
  
  return(cleaned_table)
}
