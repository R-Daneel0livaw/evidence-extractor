get_team_df <- function() {
  teams_page <- discover_page("https://www.basketball-reference.com/teams/")
  teams_view <- teams_page("table#teams_active")

  teams_initial_table <-
    teams_view %>%
    html_table() %>% 
    clean_names()
  
  teams_identifier <-
    teams_view %>% 
    html_elements("tr th[data-stat$='name'] a") %>%
    html_attrs_dfr() %>% 
    rename_all(~ c("id", "team")) %>% 
    mutate(id = str_extract(id, ".*/([^/]+)/$", 1),
           current = TRUE)

  teams_identifier_table <-
    teams_initial_table %>%
    left_join(teams_identifier, by = c("franchise" = "team")) %>%
    fill(id) %>%
    replace_na(list(current = FALSE)) %>%
    mutate(level = ifelse(!duplicated(franchise) &
                            current, "FRANCHISE", "TEAM"))

  teams_alts <-
    teams_identifier_table %>%
    group_by(id) %>%
    summarise(alternative_names = str_c(unique(franchise), collapse = ", "))

  teams_table <-
    teams_identifier_table %>%
    left_join(teams_alts, by = join_by(id)) %>%
    filter(level == "FRANCHISE") %>%
    mutate(type = "TEAM") %>%
    select(!(current:level))

  teams_table
}