get_season_df <- function() {
  seasons_page <- discover_page("https://www.basketball-reference.com/leagues/")
  seasons_view <- seasons_page("table#stats")

  seasons_initial_table <-
    seasons_view %>%
    html_table() %>% 
    row_to_names(row_number = 1) %>%
    clean_names() %>% 
    filter(lg == "NBA")
  
  seasons_identifier <-
    seasons_view %>%
    html_elements("tr th[data-stat='season'] a") %>%
    html_attrs_dfr() %>% 
    rename_all(~ c("id", "season")) %>% 
    mutate(id = str_extract(id, ".*/([A-Z]+_\\d+).html", 1)) %>% 
    filter(str_detect(id, "NBA"))

  seasons_identifier_table <-
    seasons_initial_table %>%
    left_join(seasons_identifier, by = join_by(season))

  seasons_table <-
    seasons_identifier_table %>%
    mutate(start = as.numeric(str_replace(season, "-.*", "")),
           end = start + 1,
           type = "SEASON") 

  seasons_table
}

m_get_season_df <- memoise(get_season_df)