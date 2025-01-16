SeasonsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("SeasonsPage", class(page)))
}

get_page_node.SeasonsPage <- function(page) {
  seasons_view <- page$fetch_table(page$config$table_identifier)
  
  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = page$config$key_data_identifier,
                       names = c("id", "season"),
                       id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1)) %>% 
    filter(str_detect(id, "NBA"))
  
  seasons_identifier_table <-
    join_seasons_identifier(get_clean_seasons_table(seasons_view), identifier)
  
  seasons_table <-
    seasons_identifier_table %>%
    mutate(start = as.numeric(str_replace(season, "-.*", "")),
           end = start + 1,
           type = page$config$type) %>% 
    select(type, id, season, start, end)
  
  return(seasons_table)
}

get_page_node_stats.SeasonsPage <- function(page) {
  seasons_view <- page$fetch_table(page$config$table_identifier)

  identifier <-
    extract_identifier(view = seasons_view,
                       identifier = page$config$key_data_identifier,
                       name = c("id", "season"),
                       id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1))

  seasons_identifier_table <-
    join_identifier(initial_table = get_clean_seasons_stats_table(seasons_view),
                    identifier = identifier,
                    season)

  seasons_stats_table <-
    seasons_identifier_table %>%
    mutate(type = page$config$type) %>%
    relocate(type, id) %>%
    select(-season)

  seasons_stats <- convert_to_stats(seasons_stats_table, "age")

  return(seasons_stats)
}

get_page_multi_node_stats.SeasonsPage <- function(page) {
  # return(nodes)
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
