get_awards_df <- function() {
  playoffs_page <- discover_page("https://www.basketball-reference.com/playoffs/")
  playoffs_view <- playoffs_page("table#champions_index")

  playoffs_initial_table <-
    playoffs_view %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>% 
    select(where(is_character)) %>%
    filter(nzchar(year)) %>%
    mutate(row_number = row_number())

  seasons_identifier <-
    playoffs_view %>%
    html_elements("tr th[data-stat='year_id'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("seasons_id", "seasons_name")) %>%
    mutate(seasons_id = str_extract(seasons_id, ".*/([A-Z]+_\\d+).html", 1),
           row_number = row_number())

  awards_seasons_identifier_table <-
    playoffs_initial_table %>%
    left_join(seasons_identifier,
              by = c("year" = "seasons_name", "row_number"))

  champions_identifier <-
    playoffs_view %>%
    html_elements("tr td[data-stat='champion'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("champions_id", "champions_name")) %>%
    mutate(champions_id = str_extract(champions_id, ".*/teams/([^/]+)/.*", 1),
           row_number = row_number())

  awards_champions_identifier_table <-
    awards_seasons_identifier_table %>%
    left_join(champions_identifier,
              by = c("champion" = "champions_name", "row_number")) %>%
    filter(lg == "NBA") %>%
    select(seasons_id, champions_id) %>%
    rename(all_of(c(
      season = "seasons_id",
      champion = "champions_id"
    )))

  mvp_page <- discover_page("https://www.basketball-reference.com/awards/mvp.html")
  mvp_view <- mvp_page("table#mvp_NBA")

  mvp_initial_table <-
    mvp_view %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    mutate(row_number = row_number())

  mvps_identifier <-
    mvp_view %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("mvps_id", "mvps_name")) %>%
    mutate(mvps_id = str_extract(mvps_id, "[^/]+(?=\\.html$)"),
           row_number = row_number())

  mvps_identifier_table <-
    mvp_initial_table %>%
    left_join(mvps_identifier, by = c("Player" = "mvps_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1)) %>%
    select(season, mvps_id)

  awards_mvp_identifier_table <-
    awards_champions_identifier_table %>%
    left_join(mvps_identifier_table, by = join_by(season)) %>%
    rename(all_of(c(mvp = "mvps_id")))

  roy_page <- discover_page("https://www.basketball-reference.com/awards/roy.html")
  roy_view <- roy_page("table#roy_NBA")

  roy_initial_table <-
    roy_view %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    mutate(row_number = row_number())

  roys_identifier <-
    roy_view %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("roys_id", "roys_name")) %>%
    mutate(roys_id = str_extract(roys_id, "[^/]+(?=\\.html$)"),
           row_number = row_number())

  roys_identifier_table <-
    roy_initial_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\(Tie\\)|\\*", ""))) %>%
    left_join(roys_identifier, by = c("Player" = "roys_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1)) %>%
    select(season, roys_id)

  awards_roy_identifier_table <-
    awards_mvp_identifier_table %>%
    left_join(roys_identifier_table,
              by = join_by(season),
              multiple = "all") %>%
    rename(all_of(c(roy = "roys_id")))

  pts_page <- discover_page("https://www.basketball-reference.com/leaders/pts_yearly.html")
  pts_view <- pts_page("table#leaders")

  pts_initial_table <-
    pts_view %>%
    html_table() %>%
    mutate(row_number = row_number())

  pts_value <-
    pts_view %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()

  pts_identifier <-
    pts_view %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("pts_id", "pts_name")) %>%
    mutate(pts_id = str_extract(pts_id, "[^/]+(?=\\.html$)"),
           pts_value = pts_value,
           row_number = row_number())

  pts_identifier_table <-
    pts_initial_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(pts_identifier, by = c("Player" = "pts_name", "row_number")) %>%
    mutate(
      season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
      pts_id = paste0(pts_id, "/", pts_value)
    ) %>%
    filter(season != "NBA_NA") %>%
    select(season, pts_id)

  awards_pts_identifier_table <-
    awards_roy_identifier_table %>%
    left_join(pts_identifier_table,
              by = join_by(season),
              multiple = "all") %>%
    rename(all_of(c(pts_leader = "pts_id")))

  asts_page <- discover_page("https://www.basketball-reference.com/leaders/ast_yearly.html")
  asts_view <- asts_page("table#leaders")

  asts_initial_table <-
    asts_view %>%
    html_table() %>%
    mutate(row_number = row_number())

  asts_value <-
    asts_view %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()

  asts_identifier <-
    asts_view %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("asts_id", "asts_name")) %>%
    mutate(asts_id = str_extract(asts_id, "[^/]+(?=\\.html$)"),
           asts_value = asts_value,
           row_number = row_number())

  asts_identifier_table <-
    asts_initial_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(asts_identifier, by = c("Player" = "asts_name", "row_number")) %>%
    mutate(
      season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
      asts_id = paste0(asts_id, "/", asts_value)
    ) %>%
    filter(season != "NBA_NA") %>%
    select(season, asts_id)

  awards_asts_identifier_table <-
    awards_pts_identifier_table %>%
    left_join(asts_identifier_table,
              by = join_by(season),
              multiple = "all") %>%
    rename(all_of(c(asts_leader = "asts_id")))

  trbs_page <- discover_page("https://www.basketball-reference.com/leaders/trb_yearly.html")
  trbs_view <- trbs_page("table#leaders")

  trbs_initial_table <-
    trbs_view %>%
    html_table() %>%
    mutate(row_number = row_number())

  trbs_value <-
    trbs_view %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()

  trbs_identifier <-
    trbs_view %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attrs_dfr() %>%
    rename_all(~ c("trbs_id", "trbs_name")) %>%
    mutate(trbs_id = str_extract(trbs_id, "[^/]+(?=\\.html$)"),
           trbs_value = trbs_value,
           row_number = row_number())

  trbs_identifier_table <-
    trbs_initial_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(trbs_identifier, by = c("Player" = "trbs_name", "row_number")) %>%
    mutate(
      season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
      trbs_id = paste0(trbs_id, "/", trbs_value)
    ) %>%
    filter(season != "NBA_NA") %>%
    select(season, trbs_id)

  awards_trbs_identifier_table <-
    awards_asts_identifier_table %>%
    left_join(trbs_identifier_table,
              by = join_by(season),
              multiple = "all") %>%
    rename(all_of(c(trbs_leader = "trbs_id")))

  awards_trbs_identifier_table

  awards_table <-
    awards_trbs_identifier_table %>%
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
    mutate(type = "AWARD")
  
  # Have some duplicates due to ties. need to account for both but not duplicate others in row.
  # 1952 is an example.
}