

get_team_df <- function() {
  teams_page <- discover_page("https://www.basketball-reference.com/teams/")
  teams_view <- teams_page("table#teams_active")

  teams_initial_table <-
    teams_view %>%
    html_table()
  
  teams_name <-
    teams_view %>%
    html_elements("tr th[data-stat$='name'] a") %>%
    html_text2()
  
  teams_id <-
    teams_view %>%
    html_elements("tr th[data-stat$='name'] a") %>%
    html_attr("href") %>%
    str_extract(".*/([^/]+)/$", 1)
  
  teams_identifier <-
    tibble(team = teams_name,
           id = teams_id,
           current = TRUE)
  
  teams_identifier_table <-
    teams_initial_table %>%
    left_join(teams_identifier, by = c("Franchise" = "team")) %>%
    fill(id) %>%
    replace_na(list(current = FALSE)) %>%
    mutate(level = ifelse(!duplicated(Franchise) &
                            current, "FRANCHISE", "TEAM")) %>%
    clean_names()
  
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