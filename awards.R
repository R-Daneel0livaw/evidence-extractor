# Have some duplicates due to ties. need to account for both but not duplicate others in row.
# 1952 is an example.
get_awards_df <- function() {
  playoffs_page <- discover_page("https://www.basketball-reference.com/playoffs/")
  mvp_page <- discover_page("https://www.basketball-reference.com/awards/mvp.html")
  roy_page <- discover_page("https://www.basketball-reference.com/awards/roy.html")
  pts_page <- discover_page("https://www.basketball-reference.com/leaders/pts_yearly.html")
  asts_page <- discover_page("https://www.basketball-reference.com/leaders/ast_yearly.html")
  trbs_page <- discover_page("https://www.basketball-reference.com/leaders/trb_yearly.html")

  awards_table <-
    get_all_awards(playoffs_page, mvp_page, roy_page, 
                   pts_page, asts_page, trbs_page) %>%
    pivot_longer(cols = -season,
                 names_to = "award",
                 values_to = "value") %>%
    filter(complete.cases(.)) %>%
    separate_wider_delim(
      value,
      names = c("value", "extra"),
      delim = "/",
      too_few = "align_start"
    ) %>%
    mutate(type = "AWARD", id = paste(season, award, sep = "_")) %>% 
    relocate(type, id)
}

m_get_awards_df <- memoise(get_awards_df)

get_all_awards <- function(playoffs_page, mvp_page, roy_page, 
                           pts_page, asts_page, trbs_page) {
  playoffs_view <- playoffs_page("table#champions_index")
  mvp_view <- mvp_page("table#mvp_NBA")
  roy_view <- roy_page("table#roy_NBA")
  pts_view <- pts_page("table#leaders")
  asts_view <- asts_page("table#leaders")
  trbs_view <- trbs_page("table#leaders")
  
  awards_all_identifier_table <-
    join_identifier_columns(playoffs_view, 
                            get_clean_champions_awards_table(playoffs_view)) %>%
    join_award_champions(get_award_champions(playoffs_view)) %>%
    join_award_mvps(get_clean_player_awards_table(mvp_view),
                    get_award_mvps(mvp_view)) %>%
    join_award_roys(get_clean_player_awards_table(roy_view),
                    get_award_roys(roy_view)) %>%
    join_award_pts(get_clean_total_awards_table(pts_view),
                   get_award_pts(pts_view)) %>%
    join_award_asts(get_clean_total_awards_table(asts_view),
                    get_award_asts(asts_view)) %>%
    join_award_trbs(get_clean_total_awards_table(trbs_view),
                    get_award_trbs(trbs_view)) %>%
    rename(all_of(c(mvp = "mvps_id", roy = "roys_id", pts_leader = "pts_id",
                    asts_leader = "asts_id", trbs_leader = "trbs_id")))
  
  awards_all_identifier_table
}

get_clean_champions_awards_table <- function(view) {
  awards_initial_table <-
    view %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>% 
    select(where(is_character)) %>%
    filter(nzchar(year)) %>%
    mutate(row_number = row_number())
  
  awards_initial_table
}

get_clean_player_awards_table <- function(view) {
  player_awards_initial_table <-
    view %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    mutate(row_number = row_number())
  
  player_awards_initial_table
}

get_clean_total_awards_table <- function(view) {
  total_awards_initial_table <-
    view %>%
    html_table() %>%
    mutate(row_number = row_number())
  
  total_awards_initial_table
}

join_identifier_columns <- function(view, initial_table) {
  seasons_identifier <-
    view %>%
    html_elements("tr th[data-stat='year_id'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("seasons_id", "seasons_name")) %>%
    mutate(seasons_id = str_extract(seasons_id, ".*/([A-Z]+_\\d+).html", 1),
           row_number = row_number())
  
  awards_seasons_identifier_table <-
    initial_table %>%
    left_join(seasons_identifier,
              by = c("year" = "seasons_name", "row_number"))
  
  awards_seasons_identifier_table
}

get_award_champions <- function(view) {
  champions_identifier <-
    view %>%
    html_elements("tr td[data-stat='champion'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("champions_id", "champions_name")) %>%
    mutate(champions_id = str_extract(champions_id, ".*/teams/([^/]+)/.*", 1),
           row_number = row_number())
  
  champions_identifier
}

get_award_mvps <- function(view) {
  mvps_identifier <-
    view %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("mvps_id", "mvps_name")) %>%
    mutate(mvps_id = str_extract(mvps_id, "[^/]+(?=\\.html$)"),
           row_number = row_number())
  
  mvps_identifier
}

get_award_roys <- function(view) {
  roys_identifier <-
    view %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("roys_id", "roys_name")) %>%
    mutate(roys_id = str_extract(roys_id, "[^/]+(?=\\.html$)"),
           row_number = row_number())
  
  roys_identifier
}

get_award_pts <- function(view) {
  pts_value <-
    view %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()
  
  pts_identifier <-
    view %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("pts_id", "pts_name")) %>%
    mutate(pts_id = str_extract(pts_id, "[^/]+(?=\\.html$)"),
           pts_value = pts_value,
           row_number = row_number())
  
  pts_identifier
}

get_award_asts <- function(view) {
  asts_value <-
    view %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()
  
  asts_identifier <-
    view %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("asts_id", "asts_name")) %>%
    mutate(asts_id = str_extract(asts_id, "[^/]+(?=\\.html$)"),
           asts_value = asts_value,
           row_number = row_number())
  
  asts_identifier
}

get_award_trbs <- function(view) {
  trbs_value <-
    view %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()
  
  trbs_identifier <-
    view %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("trbs_id", "trbs_name")) %>%
    mutate(trbs_id = str_extract(trbs_id, "[^/]+(?=\\.html$)"),
           trbs_value = trbs_value,
           row_number = row_number())
  
  trbs_identifier
}

join_award_champions <- function(awards_table, champions_identifier) {
  awards_champions_identifier_table <-
    awards_table %>%
    left_join(champions_identifier,
              by = c("champion" = "champions_name", "row_number")) %>%
    filter(lg == "NBA") %>%
    select(seasons_id, champions_id) %>%
    rename(all_of(c(
      season = "seasons_id",
      champion = "champions_id"
    )))
  
  awards_champions_identifier_table
}

join_award_mvps <- function(main_awards_table, temp_awards_table, mvps_identifier) {
  mvps_identifier_table <-
    temp_awards_table %>%
    left_join(mvps_identifier, by = c("Player" = "mvps_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1)) %>%
    select(season, mvps_id)
  
  full_awards_table <- 
    main_awards_table %>%
    left_join(mvps_identifier_table, by = join_by(season))

  full_awards_table
}

join_award_roys <- function(main_awards_table, temp_awards_table, roys_identifier) {
  roys_identifier_table <-
    temp_awards_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\(Tie\\)|\\*", ""))) %>%
    left_join(roys_identifier, by = c("Player" = "roys_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1)) %>%
    select(season, roys_id)
  
  full_awards_table <- 
    main_awards_table %>%
    left_join(roys_identifier_table,
               by = join_by(season),
               multiple = "all")
  
  full_awards_table
}

join_award_pts <- function(main_awards_table, temp_awards_table, pts_identifier) {
  pts_identifier_table <-
    temp_awards_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(pts_identifier, by = c("Player" = "pts_name", "row_number")) %>%
    mutate(
      season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
      pts_id = paste0(pts_id, "/", pts_value)
    ) %>%
    filter(season != "NBA_NA") %>%
    select(season, pts_id)
  
  full_awards_table <- 
    main_awards_table %>%
    left_join(pts_identifier_table,
              by = join_by(season),
              multiple = "all")
  
  full_awards_table
}

join_award_asts <- function(main_awards_table, temp_awards_table, asts_identifier) {
  asts_identifier_table <-
    temp_awards_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(asts_identifier, by = c("Player" = "asts_name", "row_number")) %>%
    mutate(
      season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
      asts_id = paste0(asts_id, "/", asts_value)
    ) %>%
    filter(season != "NBA_NA") %>%
    select(season, asts_id)
  
  full_awards_table <-
    main_awards_table %>%
    left_join(asts_identifier_table,
              by = join_by(season),
              multiple = "all")

  full_awards_table
}

join_award_trbs <- function(main_awards_table, temp_awards_table, trbs_identifier) {
  trbs_identifier_table <-
    temp_awards_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(trbs_identifier, by = c("Player" = "trbs_name", "row_number")) %>%
    mutate(
      season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
      trbs_id = paste0(trbs_id, "/", trbs_value)
    ) %>%
    filter(season != "NBA_NA") %>%
    select(season, trbs_id)
  
  full_awards_table <-
    main_awards_table %>%
    left_join(trbs_identifier_table,
              by = join_by(season),
              multiple = "all")
  
  full_awards_table
}