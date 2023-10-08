library(tidyverse)
library(rvest)
library(jsonlite)
library(janitor)


# SEASON

get_season_df <- function() {
  seasons_url <- "https://www.basketball-reference.com/leagues/"
  
  seasons_page <-
    read_html(seasons_url) %>%
    html_element("table#stats")
  
  seasons_initial_table <-
    seasons_page %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    filter(Lg == "NBA")
  
  seasons_name <-
    seasons_page %>%
    html_elements("tr th[data-stat='season'] a") %>%
    html_text2()

  seasons_id <-
    seasons_page %>%
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

seasons_table <- get_season_df()



# TEAM

get_team_df <- function() {
  teams_url <- "https://www.basketball-reference.com/teams/"
  
  teams_page <-
    read_html(teams_url) %>%
    html_element("table#teams_active")
  
  teams_initial_table <-
    teams_page %>%
    html_table()
  
  teams_name <-
    teams_page %>%
    html_elements("tr th[data-stat$='name'] a") %>%
    html_text2()
  
  teams_id <-
    teams_page %>%
    html_elements("tr th[data-stat$='name'] a") %>%
    html_attr("href") %>%
    str_extract(".*/([^/]+)/$", 1)
  
  teams_identifier <-
    tibble(team = teams_name, id = teams_id, current = TRUE)
  
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

teams_table <- get_team_df()


# PLAYER

get_player_df <- function(letter) {
  players_url <-
    paste0("https://www.basketball-reference.com/players/", letter, "/")
  
  players_page <-
    read_html(players_url) %>%
    html_element("table#players")
  
  players_initial_table <-
    players_page %>%
    html_table() %>%
    mutate(Player = str_replace_all(Player, "\\*", ""),
           row_number = row_number())
  
  players_name <-
    players_page %>%
    html_elements("tr th[data-stat='player'] a") %>%
    html_text2()
  
  players_id <-
    players_page %>%
    html_elements("tr th[data-stat='player'] a") %>%
    html_attr("href") %>%
    str_extract("[^/]+(?=\\.html$)")
  
  players_identifier <-
    tibble(player = players_name, id = players_id) %>%
    mutate(row_number = row_number())
  
  players_identifier_table <-
    players_initial_table %>%
    left_join(players_identifier, by = c("Player" = "player", "row_number"))
  
  players_active_id <-
    players_page %>%
    html_elements("tr th[data-stat='player'] strong a") %>%
    html_attr("href") %>%
    str_extract("[^/]+(?=\\.html$)")
  
  players_active_identifier <-
    tibble(id = players_active_id, active = TRUE)
  
  players_table <-
    players_identifier_table %>%
    left_join(players_active_identifier, by = join_by(id)) %>%
    mutate(
      active = !is.na(active),
      `Birth Date` = as.Date(`Birth Date`, format = "%B %d, %Y"),
      type = "PLAYER"
    ) %>%
    select(!row_number) %>%
    clean_names()
  
  players_college_name <-
    players_page %>%
    html_elements("tr td[data-stat='colleges'] a") %>%
    html_text2() 
  # %>%
  # str_replace(",", "#")
  
  players_college_id <-
    players_page %>%
    html_elements("tr td[data-stat='colleges'] a") %>%
    html_attr("href") %>%
    str_extract("(?<=college=).*")
  
  players_college_identifier <-
    tibble(college_name = players_college_name, college_id = players_college_id) %>%
    distinct(college_id, .keep_all = TRUE)
  
  # look into library(fuzzyjoin) to eliminate need to split on comma which is causing NAs.   
  players_table <- players_table %>%
    separate_longer_delim(colleges, ", ") %>%
    left_join(
      players_college_identifier,
      by = c("colleges" = "college_name"),
      multiple = "first"
    ) %>%
    mutate(colleges = ifelse(nzchar(colleges), paste0(colleges, "/", college_id), colleges)) %>%
    mutate(colleges = str_c(colleges, collapse = ", "), .by = id) %>%
    distinct(player, id, .keep_all = TRUE) %>%
    select(!college_id)
  
  players_table
}

players_table <-
  letters[1:3] %>%
  map_dfr(\(letter) get_player_df(letter))



# COLLEGE

colleges_table <-
  players_table %>%
  select(colleges) %>% 
  mutate(type = "COLLEGE") %>% 
  separate_rows(colleges, sep = ", ") %>%
  filter(nzchar(colleges)) %>% 
  separate_wider_delim(colleges, names = c("name", "id"), delim = "/") %>% 
  distinct() %>% 
  arrange(name)



# GAME

get_game_df <- function(month, season) {
  games_url <-
    paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_games-", month, ".html")
  
  games_page <-
    read_html(games_url) %>%
    html_element("table#schedule")
  
  games_initial_table <-
    games_page %>%
    html_table() %>%
    clean_names() %>%
    rename(all_of(c(
      visitor = "visitor_neutral",
      home = "home_neutral",
      home_pts = "pts_2",
      visitor_pts = "pts",
      ot = "x_2"
    ))) %>% 
    mutate(row_number = row_number(),
           ot = ifelse(nzchar(ot), TRUE, FALSE),
           date = as.Date(date, format = "%a, %b %d, %Y")) %>% 
    select(!x)
  
  games_id <-
    games_page %>%
    html_elements("tr td[data-stat='box_score_text'] a") %>%
    html_attr("href") %>%
    str_extract(".*/([^.]+)\\.html$", 1)
  
  games_identifier <-
    tibble(id = games_id) %>%
    mutate(row_number = row_number())
  
  games_table <- 
    games_initial_table %>%
    left_join(games_identifier, by = join_by(row_number)) %>% 
    mutate(type = "GAME") %>% 
    select(!row_number)
  
  games_table
}

games_table <-
  month.name[10:12] %>%
  map2_dfr(2023, \(month, season) get_game_df(str_to_lower(month), season))



# AWARD

get_awards_df <- function() {
  playoffs_url <- "https://www.basketball-reference.com/playoffs/"

  playoffs_page <-
    read_html(playoffs_url) %>%
    html_element("table#champions_index")

  playoffs_initial_table <-
    playoffs_page %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    select(where(is_character)) %>%
    filter(nzchar(Year)) %>%
    mutate(row_number = row_number()) %>%
    clean_names()

  seasons_name <-
    playoffs_page %>%
    html_elements("tr th[data-stat='year_id'] a") %>%
    html_text2()

  seasons_id <-
    playoffs_page %>%
    html_elements("tr th[data-stat='year_id'] a") %>%
    html_attr("href") %>%
    str_extract(".*/([A-Z]+_\\d+).html", 1)

  seasons_identifier <-
    tibble(seasons_name, seasons_id) %>%
    mutate(row_number = row_number())

  awards_seasons_identifier_table <-
    playoffs_initial_table %>%
    left_join(seasons_identifier,
              by = c("year" = "seasons_name", "row_number"))

  champions_name <-
    playoffs_page %>%
    html_elements("tr td[data-stat='champion'] a") %>%
    html_text2()

  champions_id <-
    playoffs_page %>%
    html_elements("tr td[data-stat='champion'] a") %>%
    html_attr("href") %>%
    str_extract(".*/teams/([^/]+)/.*", 1)

  champions_identifier <-
    tibble(champions_name, champions_id) %>%
    mutate(row_number = row_number())

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

  mvp_url <- "https://www.basketball-reference.com/awards/mvp.html"

  mvp_page <-
    read_html(mvp_url) %>%
    html_element("table#mvp_NBA")

  mvp_initial_table <-
    mvp_page %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    mutate(row_number = row_number())

  mvps_name <-
    mvp_page %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_text2()

  mvps_id <-
    mvp_page %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_attr("href") %>%
    str_extract("[^/]+(?=\\.html$)")

  mvps_identifier <-
    tibble(mvps_name, mvps_id) %>%
    mutate(row_number = row_number())

  mvps_identifier_table <-
    mvp_initial_table %>%
    left_join(mvps_identifier, by = c("Player" = "mvps_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1)) %>%
    select(season, mvps_id)

  awards_mvp_identifier_table <-
    awards_champions_identifier_table %>%
    left_join(mvps_identifier_table, by = join_by(season)) %>%
    rename(all_of(c(mvp = "mvps_id")))

  roy_url <- "https://www.basketball-reference.com/awards/roy.html"

  roy_page <-
    read_html(roy_url) %>%
    html_element("table#roy_NBA")

  roy_initial_table <-
    roy_page %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    mutate(row_number = row_number())

  roys_name <-
    roy_page %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_text2()

  roys_id <-
    roy_page %>%
    html_elements("tr td[data-stat='player'] a") %>%
    html_attr("href") %>%
    str_extract("[^/]+(?=\\.html$)")

  roys_identifier <-
    tibble(roys_name, roys_id) %>%
    mutate(row_number = row_number())

  roys_identifier_table <-
    roy_initial_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\(Tie\\)|\\*", ""))) %>%
    left_join(roys_identifier, by = c("Player" = "roys_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1)) %>%
    select(season, roys_id)

  awards_roy_identifier_table <-
    awards_mvp_identifier_table %>%
    left_join(roys_identifier_table, by = join_by(season), multiple = "all") %>%
    rename(all_of(c(roy = "roys_id")))

  awards_roy_identifier_table
  
  pts_url <- "https://www.basketball-reference.com/leaders/pts_yearly.html"
  
  pts_page <-
    read_html(pts_url) %>%
    html_element("table#leaders")
  
  pts_initial_table <-
    pts_page %>%
    html_table() %>%
    mutate(row_number = row_number())
  
  pts_name <-
    pts_page %>%
    html_elements("tr td a[href^='/players']") %>%
    html_text2()
  
  pts_id <-
    pts_page %>%
    html_elements("tr td a[href^='/players']") %>%
    html_attr("href") %>%
    str_extract("[^/]+(?=\\.html$)")
  
  pts_value <-
    pts_page %>%
    html_elements("tr td:nth-child(4)") %>%
    html_text2()

  pts_identifier <-
    tibble(pts_name, pts_id, pts_value) %>%
    mutate(row_number = row_number())

  pts_identifier_table <-
    pts_initial_table %>%
    mutate(Player = str_trim(str_replace_all(Player, "\\*", ""))) %>%
    left_join(pts_identifier, by = c("Player" = "pts_name", "row_number")) %>%
    mutate(season = paste0("NBA_", as.numeric(str_replace(Season, "-.*", "")) + 1),
           pts_id = paste0(pts_id, "/", pts_value)) %>%
    filter(season != "NBA_NA") %>% 
    select(season, pts_id)
  
  awards_pts_identifier_table <-
    awards_roy_identifier_table %>%
    left_join(pts_identifier_table,
              by = join_by(season),
              multiple = "all") %>%
    rename(all_of(c(pts_leader = "pts_id")))
  
  awards_pts_identifier_table
  
  # https://www.basketball-reference.com/leaders/ast_yearly.html
}
  
awards_table <- get_awards_df()

# Have some duplicates due to ties. need to account for both but not duplicate others in row.
# 1952 is an example.

df_long <-
  awards_table %>%
  pivot_longer(cols = -season,
               names_to = "award",
               values_to = "value") %>%
  filter(complete.cases(.)) %>% 
  mutate(type = "AWARD")
