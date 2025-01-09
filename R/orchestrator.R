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
source("R/utils.R")

seasons_nodes <- m_get_season_df()
seasons_stats <- m_get_season_top_stats()
seasons_teams_stats <- m_get_season_team_stats()

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
awards_nodes <- m_get_awards_df()
