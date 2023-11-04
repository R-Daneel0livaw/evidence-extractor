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
source("R/utils.R")

seasons_nodes <- m_get_season_df()
seasons_stats <- m_get_season_top_stats()
seasons_teams_stats <- m_get_season_team_stats()

teams_nodes <- m_get_team_df()
teams_stats <- m_get_team_top_stats()

players_nodes <- m_get_player_df()
players_stats <- m_get_player_top_stats()

colleges_table <- m_get_college_df()
games_table <- m_get_game_df()
awards_table <- m_get_awards_df()

# process_row <- function(row) {
#   view <- row$view
#   stat_suffix <- row$stat_suffix
#   stats_start <- row$stats_start
#   stats_end <- row$stats_end
#   rename_start <- row$rename_start
#   
#   df <- data.frame(
#     view = view,
#     stat_suffix = stat_suffix,
#     stats_start = stats_start,
#     stats_end = stats_end,
#     rename_start = rename_start
#   )
#   
#   df
# }
# 
# result <- get_season_team_data() %>%
#   transpose() %>%
#   map_dfr(process_row)