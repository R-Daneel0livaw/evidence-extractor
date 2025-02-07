TeamsPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("TeamsPage", class(page)))
}

get_page_node.TeamsPage <- function(page) {
  base_get_page_node(
    page = page,
    clean_fn = get_clean_teams_table,
    join_fn = function(view, identifier) {
      join_teams_identifier(view, identifier)
    },
    mutate_fn = function(data) {
      data %>%
        join_teams_alternative_names(get_teams_alternative_names(data)) %>%
        mutate(type = page$config$type) %>%
        relocate(type, id, team, alternative_names)
    },
    filter_fn = function(data) {
      data %>% mutate(current = TRUE)
    },
    select_cols =  c("-current:level", "-lg")
  )
}

get_page_node_stats.TeamsPage <- function(page) {
  base_get_page_node(
    page = page,
    # clean_fn = get_clean_seasons_stats_table, 
    # join_fn = function(view, identifier) {
    #   join_identifier(view, identifier, season)
    # },
    # mutate_fn = function(data) {
    #   data %>%
    #     mutate(type = page$config$type) %>%
    #     relocate(type, id)
    # },
    # select_cols = c("-season"),
    stats_fn = function(data, config) {
        convert_to_stats(data, config$start) 
    }
  )
}

get_clean_teams_table <- function(view) {
  seasons_initial_table <-
    view %>% 
    get_clean_table() %>% 
    rename(team = franchise)
  
  seasons_initial_table
}

join_teams_identifier <- function(view, identifier) {
  joined_table <-
    join_identifier(view, identifier, team) %>%
    fill(id) %>%
    replace_na(list(current = FALSE)) %>%
    mutate(level = ifelse(!duplicated(team) &
                            current, "FRANCHISE", "TEAM"))
  
  joined_table
}

get_teams_alternative_names <- function(teams_identifier_table) {
  teams_alts <-
    teams_identifier_table %>%
    group_by(id) %>%
    summarise(alternative_names = str_c(unique(team), collapse = ", "))
  
  teams_alts
}

join_teams_alternative_names <- function(teams_identifier_table, teams_alts) {
  teams_table <-
    join_identifier(teams_identifier_table, teams_alts, id) %>%
    filter(level == "FRANCHISE")
}
