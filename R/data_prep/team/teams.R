get_team_df <- function() {
  teams_page <- discover_page("https://www.basketball-reference.com/teams/")
  teams_view <- teams_page("table#teams_active")

  identifier <-
    extract_identifier(teams_view,
                       "tr th[data-stat$='name'] a",
                       ".*/([^/]+)/$",
                       "team") %>%
    mutate(current = TRUE)
  
  teams_identifier_table <-
    join_teams_identifier(get_clean_teams_table(teams_view), identifier)

  teams_alts <- get_teams_alternative_names(teams_identifier_table)

  teams_table <-
    join_teams_alternative_names(teams_identifier_table, teams_alts) %>% 
    mutate(type = "TEAM") %>%
    relocate(type, id, team, alternative_names) %>% 
    select(!c(current:level, lg))

  teams_table
}

m_get_team_df <- memoise(get_team_df)

get_team_top_stats <- function() {
  teams_stats <- convert_to_stats(m_get_team_df(), "g")
    
  teams_stats
}

m_get_team_top_stats <- memoise(get_team_top_stats)

get_clean_teams_table <- function(view) {
  seasons_initial_table <-
    view %>% 
    get_clean_table() %>% 
    rename(team = franchise)
  
  seasons_initial_table
}

join_teams_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table, identifier, team) %>%
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