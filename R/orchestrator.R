library(tidyverse)
library(rvest)
library(jsonlite)
library(janitor)
library(polite)
library(memoise)

source("R/data_prep/season/seasons.R")
source("R/data_prep/team/teams.R")
source("R/data_prep/player/players.R")
source("R/data_prep/game/games.R")
source("R/data_prep/award/awards.R")
source("R/utils.R")

seasons_table <- m_get_season_df()
teams_table <- m_get_team_df()
players_table <- m_get_player_df()
colleges_table <- m_get_college_df()
games_table <- m_get_game_df()
awards_table <- m_get_awards_df()


filtered_df <- 
  teams_table %>%
  select((which(names(.) == "to")):last_col())


teams_stats <-
  map2(
    names(filtered_df),
    filtered_df,
    \(name, value, id) data.frame(name, value = as.character(value), id, type = "STAT"),
    teams_table$id
  ) %>%
  bind_rows() 


