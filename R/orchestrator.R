library(tidyverse)
library(rvest)
library(jsonlite)
library(janitor)
library(polite)
library(memoise)
library(uuid)

source("R/data_prep/season/seasons.R")
source("R/data_prep/team/teams.R")
source("R/data_prep/player/players.R")
source("R/data_prep/game/games.R")
source("R/data_prep/award/awards.R")
source("R/data_prep/relationship/relationships.R")



source("R/Page.R")
source("R/SeasonsPage.R")
source("R/Config.R")
source("R/utils.R")


seasons_page <- SeasonsPage(get_page_config("SEASON"))
seasons_nodes_new <- get_page_node(seasons_page)

seasons_stats_page <- SeasonsPage(get_page_config("SEASON_STATS"))
seasons_stats_new <- get_page_node_stats(seasons_stats_page)

seasons_teams_stats_page <- SeasonsPage(get_page_config("SEASON_TEAM_STATS"))
seasons_teams_stats_new <- get_page_multi_node_stats(seasons_teams_stats_page, seasons_nodes_new)


teams_nodes <- m_get_team_df()
teams_stats <- m_get_team_top_stats()

players_nodes <- m_get_player_df()
players_stats <- m_get_player_top_stats()
player_team_group <- m_get_player_team()
player_team_relationships <- generate_simple_relationships(
  player_team_group %>%
    rename(a = player, b = team), 
  "PLAYED_FOR"
)

games_nodes <- m_get_game_df()
games_stats <- m_get_games_top_stats()

games_players_stats <- m_get_game_player_stats()
game_player_relationships <- generate_relationships(games_players_stats, "PARTICIPATED_IN")

games_teams_stats <- m_get_game_team_stats()
game_team_relationships <- generate_relationships(games_teams_stats, "PARTICIPATED_IN")

game_season_relationships <- generate_simple_relationships(
  games_nodes %>%
    select(id, season) %>%
    rename(a = season, b = id), 
  "HAS_EVENT"
)

colleges_nodes <- m_get_college_df()
players_colleges <- players_nodes %>%
  filter(colleges != "" & !is.na(colleges)) %>%
  separate_rows(colleges, sep = ",\\s*") %>%
  separate(colleges, into = c("college_name", "college_id"), sep = "/") %>%
  select(player = id, college = college_id)

player_college_relationship <- generate_simple_relationships(
  players_colleges %>%
    rename(a = player, b = college), 
  "ATTENDED"
)

awards_nodes <- m_get_awards_df()
