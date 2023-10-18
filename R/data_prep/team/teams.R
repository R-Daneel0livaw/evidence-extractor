get_team_df <- function() {
  teams_page <- discover_page("https://www.basketball-reference.com/teams/")
  teams_view <- teams_page("table#teams_active")

  teams_identifier_table <-
    join_identifier_columns(teams_view, get_clean_teams_table(teams_view))

  teams_alts <- get_teams_alternative_names(teams_identifier_table)

  teams_table <-
    join_teams_alternative_names(teams_identifier_table, teams_alts) %>% 
    mutate(type = "TEAM") %>%
    relocate(type, id, franchise, alternative_names) %>% 
    select(!c(current:level, lg))

  teams_table
}

m_get_team_df <- memoise(get_team_df)

get_team_stats <- function() {
  teams_table <- m_get_team_df()
  
  filtered_df <-
    teams_table %>%
    select((which(names(.) == "yrs")):last_col())
  
  teams_stats <-
    map2(
      names(filtered_df),
      filtered_df,
      \(name, value, connector_id, connector_type) data.frame(
        name,
        value = as.character(value),
        connector_id,
        connector_type,
        type = "STAT"
      ),
      teams_table$id,
      teams_table$type
    ) %>%
    bind_rows() %>%
    relocate(type)
  
  teams_stats
}

m_get_team_stats <- memoise(get_team_stats)

get_clean_teams_table <- function(view) {
  teams_initial_table <-
    view %>%
    html_table() %>% 
    clean_names()
  
  teams_initial_table
}

join_identifier_columns <- function(view, initial_table) {
  teams_identifier <-
    view %>% 
    html_elements("tr th[data-stat$='name'] a") %>%
    html_attrs_dfr() %>% 
    rename_all(~ c("id", "team")) %>% 
    mutate(id = str_extract(id, ".*/([^/]+)/$", 1),
           current = TRUE)
  
  teams_identifier_table <-
    initial_table %>%
    left_join(teams_identifier, by = c("franchise" = "team")) %>%
    fill(id) %>%
    replace_na(list(current = FALSE)) %>%
    mutate(level = ifelse(!duplicated(franchise) &
                            current, "FRANCHISE", "TEAM"))
  
  teams_identifier_table
}

get_teams_alternative_names <- function(teams_identifier_table) {
  teams_alts <-
    teams_identifier_table %>%
    group_by(id) %>%
    summarise(alternative_names = str_c(unique(franchise), collapse = ", "))
  
  teams_alts
}

join_teams_alternative_names <- function(teams_identifier_table, teams_alts) {
  teams_table <-
    teams_identifier_table %>%
    left_join(teams_alts, by = join_by(id)) %>%
    filter(level == "FRANCHISE")
}