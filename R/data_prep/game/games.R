get_game_df <- function() {
  season <- seasons_nodes %>% filter(id == "NBA_2024") %>% pull(id)
  games_table <-
    month.name[10:12] %>%
    map2_dfr(season, \(month, season) get_games_group(str_to_lower(month), season))
  
  games_table
}

m_get_game_df <- memoise(get_game_df)

get_games_top_stats <- function() {
  games_stats <- convert_to_stats(m_get_game_df(), "visitor_pts") %>% filter(name %in% c("visitor_pts", "home_pts"))
}

m_get_games_top_stats <- memoise(get_games_top_stats)

get_games_group <- function(month, season) {
  games_page <- discover_page(paste0("https://www.basketball-reference.com/leagues/", season,
    "_games-", month, ".html"))
  games_view <- games_page("table#schedule")

  identifier <-
    extract_identifier(view = games_view,
                       identifier = "tr td[data-stat='box_score_text'] a",
                       names = c("id"),
                       add_text = FALSE,
                       id = str_extract(id, ".*/([^.]+)\\.html$", 1)) %>%
    mutate(row_number = row_number())

  games_table <-
    join_games_identifier(get_clean_games_table(games_view), identifier) %>%
    join_games_identifier(get_visitor(games_view)) %>% 
    join_games_identifier(get_home(games_view)) %>% 
    mutate(type = "GAME",
           season = season) %>%
    relocate(type, id, date, start_et, ot, arena, attend) %>%
    select(!c(row_number, notes, visitor, home))
  
  games_table
}

get_game_player_stats <- function() {
  games_players_stats_table <-  
    join_config_stat(get_game_player_config(), get_game_df()$id[1]) %>% 
    left_join(get_game_df(), by = c("stat" = "id")) %>% 
    rowwise() %>%
    mutate(view = str_replace(view, "\\{\\{DYNAMIC\\}\\}", get(dynamic_field))) %>%
    ungroup() %>%
    transpose() %>% 
    reduce(function(accumulator, config_row) {
      previous_stats <- if ("previous_stats" %in% names(accumulator)) {
        accumulator$previous_stats  
      } else {
        character(0) 
      }
      config_row$previous_stats <- previous_stats
      new_data <- get_game_player_stats_group(config_row)
      accumulator$previous_stats <- unique(new_data$name)
      accumulator$data <- bind_rows(accumulator$data, new_data)
      accumulator
    }, .init = list(data = data.frame(), previous_stats = character(0))) %>% .$data
  games_players_stats_table
}

m_get_game_player_stats <- memoise(get_game_player_stats)

get_game_team_stats <- function() {
  games_teams_stats_box_table <-
    join_config_stat(get_game_team_config(), get_game_df()$id[1]) %>%
    left_join(get_game_df(), by = c("stat" = "id")) %>%
    rowwise() %>%
    mutate(view = str_replace(view, "\\{\\{DYNAMIC\\}\\}", get(dynamic_field)),
           identifier = str_replace(identifier, "\\{\\{DYNAMIC\\}\\}", get(dynamic_field)),
           dynamic_field = get(dynamic_field)) %>%
    ungroup() %>%
    transpose() %>%
    map_dfr(\(config_row) get_game_team_stats_group(config_row))
  
  
  games_teams_stats_shoot_table <-
    join_config_stat(get_game_team_config_2(), get_game_df()$id[1]) %>%
    left_join(get_game_df(), by = c("stat" = "id")) %>%
    rowwise() %>%
    mutate(view = str_replace(view, "\\{\\{DYNAMIC\\}\\}", get(dynamic_field)),
           dynamic_field = get(dynamic_field)) %>%
    ungroup() %>%
    transpose() %>%
    map_dfr(\(config_row) get_game_team_stats_group_2(config_row))

  games_teams_stats_table <- rbind(games_teams_stats_box_table, games_teams_stats_shoot_table)

  games_teams_stats_table
}

m_get_game_team_stats <- memoise(get_game_team_stats)


get_player_team <- function() {
  players_teams_table <-  
    join_config_stat(get_player_team_config(), get_game_df()$id[1]) %>% 
    left_join(get_game_df(), by = c("stat" = "id")) %>% 
    rowwise() %>%
    mutate(view = str_replace(view, "\\{\\{DYNAMIC\\}\\}", get(dynamic_field)),
           dynamic_field = get(dynamic_field)) %>%
    ungroup() %>%
    transpose() %>% 
    map_dfr(\(config_row) get_player_team_group(config_row))
    
  players_teams_table
}

m_get_player_team <- memoise(get_player_team)

get_game_player_stats_group <- function(config_row) {
  game_player_stats_page <- discover_page(paste0("https://www.basketball-reference.com/boxscores/", config_row$stat, ".html"))
  get_individual_game_player_stats_group(config_row, game_player_stats_page(config_row$view))
}

get_individual_game_player_stats_group <- function(config_row, view) {
  identifier <-
    extract_identifier(view = view,
                       identifier = "tr th[data-stat='player'] a",
                       name = c("id", "player"),
                       id = str_extract(id, "(?<=/players/[a-z]/)[a-z0-9]+"))
  
  game_identifier_table <-
    join_identifier(initial_table = get_clean_game_player_stats_table(view, config_row$multi_row_header,
                                                                        config_row$dummy_header),
                    identifier = identifier,
                    player) %>%
    filter(!is.na(id))
  
  
  player_stats_table <-
    game_identifier_table %>%
    mutate(type = "PLAYER") %>%
    relocate(type, id) %>%
    rename_stats(config_row$stat_suffix, config_row$rename_start, config_row$stats_end) %>% 
    mutate(starter = row_number() <= 5)

  player_stats <- convert_to_stats(player_stats_table, config_row$stats_start) %>%
    filter(!(name %in% config_row$previous_stats)) %>%
    mutate(id = get_uuid(nrow(.))) %>%
    relocate(type, id)

  game_player_stats <- duplicate_stats(player_stats, "GAME", config_row$stat)

  game_player_stats
}

get_game_team_stats_group <- function(config_row) {
  game_team_stats_page <- discover_page(paste0("https://www.basketball-reference.com/boxscores/", config_row$stat, ".html"))
  get_individual_game_team_stats_group(config_row, game_team_stats_page(config_row$view))
}

get_game_team_stats_group_2 <- function(config_row) {
  game_team_stats_page <- discover_page(paste0("https://www.basketball-reference.com/boxscores/shot-chart/", config_row$stat, ".html"))
  get_individual_game_team_stats_group_2(config_row, game_team_stats_page(config_row$view))
}

get_individual_game_team_stats_group <- function(config_row, view) {
  identifier <-
    extract_value(view = view,
                  identifier = config_row$identifier,
                  name = "pts",
                  id = config_row$dynamic_field)
  
  team_stats_table <-
    identifier %>%
    mutate(type = "TEAM") %>%
    relocate(type, id) %>%
    rename_stats(config_row$stat_suffix, config_row$rename_start, config_row$stats_end)
  
  team_stats <- convert_to_stats(team_stats_table, paste0(config_row$stats_start, "_", config_row$stat_suffix)) %>%
    mutate(id = get_uuid(nrow(.))) %>%
    relocate(type, id)

  game_team_stats <- duplicate_stats(team_stats, "GAME", config_row$stat)

  game_team_stats
}

get_individual_game_team_stats_group_2 <- function(config_row, view) {
  team_stats_table <- get_clean_game_team_stats_table(view, config_row$multi_row_header, 
                                                       config_row$dummy_header) %>% 
    mutate(type = "TEAM", 
           id = config_row$dynamic_field) %>% 
    pivot_wider(names_from = quarter, values_from = config_row$stats_start:config_row$stats_end, names_prefix = "q_") %>% 
    relocate(type, id) %>% 
    clean_names()
  
  team_stats <- convert_to_stats(team_stats_table, "fg_q_1st") %>%
    mutate(id = get_uuid(nrow(.))) %>%
    relocate(type, id)

  game_team_stats <- duplicate_stats(team_stats, "GAME", config_row$stat)

  game_team_stats
}

get_player_team_group <- function(config_row) {
  player_team_page <- discover_page(paste0("https://www.basketball-reference.com/boxscores/", config_row$stat, ".html"))
  get_individual_player_team_group(config_row, player_team_page(config_row$view))
}

get_individual_player_team_group <- function(config_row, view) {
  identifier <-
    extract_identifier(view = view,
                       identifier = "tr th[data-stat='player'] a",
                       name = c("id", "player"),
                       id = str_extract(id, "(?<=/players/[a-z]/)[a-z0-9]+"))

  player_team <- 
    identifier %>% 
    mutate(player = id,
           team = config_row$dynamic_field) %>% 
    select(player, team)

  player_team
}

get_clean_games_table <- function(view) {
  games_initial_table <-
    view %>%
    get_clean_table() %>% 
    rename(
      visitor = visitor_neutral,
      home = home_neutral,
      home_pts = pts_2,
      visitor_pts = pts,
      ot = x_2
    ) %>%
    mutate(
      row_number = row_number(),
      ot = ifelse(nzchar(ot), TRUE, FALSE),
      date = as.Date(date, format = "%a, %b %d, %Y")
    ) %>%
    select(!x)
  
  games_initial_table
}

get_clean_game_player_stats_table <- function(view, multi_row_header = FALSE, 
                                                dummy_header = FALSE) {
  game_initial_table <- 
    get_clean_table(view, multi_row_header)
  
  colnames(game_initial_table) <- get_column_names(view, "tfoot tr:nth-child(1) > *")
  
  if(dummy_header) {
    game_initial_table <-
      game_initial_table %>% 
      select(-c(DUMMY))
  } 
  
  game_initial_table
}

get_clean_game_team_stats_table <- function(view, multi_row_header = FALSE, 
                                                dummy_header = FALSE) {
  game_initial_table <- 
    get_clean_table(view, multi_row_header)
  
  colnames(game_initial_table) <- get_column_names(view, "tfoot tr:nth-child(1) > *")
  
  if(dummy_header) {
    game_initial_table <-
      game_initial_table %>% 
      select(-c(DUMMY))
  } 
  
  game_initial_table
}

get_visitor <- function(view) {
  visitor_identifier <-
    extract_identifier(view = view,
                       identifier = "tr td[data-stat='visitor_team_name'] a",
                       names = c("visitor_id"),
                       add_text = FALSE,
                       visitor_id = str_extract(visitor_id,  ".*/teams/([^/]+)/.*", 1)) %>%
    mutate(row_number = row_number())
  
  visitor_identifier
}

get_home <- function(view) {
  home_identifier <-
    extract_identifier(view = view,
                       identifier = "tr td[data-stat='home_team_name'] a",
                       names = c("home_id"),
                       add_text = FALSE,
                       home_id = str_extract(home_id,  ".*/teams/([^/]+)/.*", 1)) %>%
    mutate(row_number = row_number())
  
  home_identifier
}

get_game_player_config <- function() {
  data <- tribble(
    ~view, ~stat_suffix,  ~stats_start, ~stats_end, ~rename_start, ~multi_row_header, ~dummy_header, ~dynamic_field,
    "table#box-{{DYNAMIC}}-game-basic", "",  "mp", "plus_minus", "", TRUE, FALSE, "visitor_id",
    "table#box-{{DYNAMIC}}-game-advanced", "",  "ts_pct", "bpm", "", TRUE, FALSE, "visitor_id",
    "table#box-{{DYNAMIC}}-game-basic", "",  "mp", "plus_minus", "", TRUE, FALSE, "home_id",
    "table#box-{{DYNAMIC}}-game-advanced", "",  "ts_pct", "bpm", "", TRUE, FALSE, "home_id"
  )
  
  data
}

get_game_team_config <- function() {
  data <- tribble(
    ~view, ~stat_suffix,  ~stats_start, ~stats_end, ~rename_start, ~multi_row_header, ~dummy_header, ~dynamic_field, ~identifier,
    "table#box-{{DYNAMIC}}-q1-basic", "q1",  "pts", "pts", "pts", TRUE, FALSE, "visitor_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q1-basic", "q1",  "pts", "pts", "pts", TRUE, FALSE, "home_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q2-basic", "q2",  "pts", "pts", "pts", TRUE, FALSE, "visitor_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q2-basic", "q2",  "pts", "pts", "pts", TRUE, FALSE, "home_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q3-basic", "q3",  "pts", "pts", "pts", TRUE, FALSE, "visitor_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q3-basic", "q3",  "pts", "pts", "pts", TRUE, FALSE, "home_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q4-basic", "q4",  "pts", "pts", "pts", TRUE, FALSE, "visitor_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-q4-basic", "q4",  "pts", "pts", "pts", TRUE, FALSE, "home_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-game-basic", "total",  "pts", "pts", "pts", TRUE, FALSE, "visitor_id", "tfoot tr td[data-stat='pts']",
    "table#box-{{DYNAMIC}}-game-basic", "total",  "pts", "pts", "pts", TRUE, FALSE, "home_id", "tfoot tr td[data-stat='pts']"
  )
  
  data
}


get_game_team_config_2 <- function() {
  data <- tribble(
    ~view, ~stat_suffix,  ~stats_start, ~stats_end, ~rename_start, ~multi_row_header, ~dummy_header, ~dynamic_field,
    "table#shooting-{{DYNAMIC}}", "",  "quarter", "fg_ast_pct", "", FALSE, FALSE, "visitor_id",
    "table#shooting-{{DYNAMIC}}", "",  "quarter", "fg_ast_pct", "", FALSE, FALSE, "home_id",
  )
  
  data
}


get_player_team_config <- function() {
  data <- tribble(
    ~view, ~dynamic_field,
    "table#box-{{DYNAMIC}}-game-basic", "visitor_id",
    "table#box-{{DYNAMIC}}-game-basic", "home_id",
  )
  
  data
}

join_games_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, row_number)
  
  joined_table
}
