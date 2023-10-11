
get_season_df <- function() {
  seasons_page <- discover_page("https://www.basketball-reference.com/leagues/")
  seasons_view <- seasons_page("table#stats")

  seasons_initial_table <-
    seasons_view %>%
    html_table() %>% 
    row_to_names(row_number = 1) %>%
    filter(Lg == "NBA")

  seasons_name <-
    seasons_view %>%
    html_elements("tr th[data-stat='season'] a") %>%
    html_text2()

  seasons_id <-
    seasons_view %>%
    html_elements("tr th[data-stat='season'] a") %>%
    html_attr("href") %>%
    str_extract(".*/([A-Z]+_\\d+).html", 1)

  seasons_identifier <-
    tibble(season = seasons_name, id = seasons_id) %>%
    filter(str_detect(id, "NBA"))

  seasons_identifier_table <-
    seasons_initial_table %>%
    left_join(seasons_identifier, by = c("Season" = "season"))

  seasons_table <-
    seasons_identifier_table %>%
    mutate(start = as.numeric(str_replace(Season, "-.*", "")),
           end = start + 1,
           type = "SEASON") %>%
    clean_names()

  seasons_table
}