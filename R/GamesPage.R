GamesPage <- function(config) {
  page <- Page(config)
  structure(page, class = c("GamesPage", class(page)))
}

get_page_node.GamesPage <- function(page, dependent_nodes) {
  print(dependent_nodes)
  params_grid <- join_config_stat(page$config, dependent_nodes$id[2], str_to_lower(month.name[10:12])) %>% transpose()
  params_grid %>%
    map_dfr(\(config_row) {
      base_get_page_node(
        page = page,
        config = config_row,
        clean_fn = get_clean_games_table,
        join_fn = function(view, identifier) {
          join_games_identifier(view, identifier) %>%
          join_games_identifier(get_visitor(games_view)) %>%
          join_games_identifier(get_home(games_view))
        },
        mutate_fn = function(data, view) {
          data %>%
            mutate(type = page$config$type,
                   season = season) %>%
            relocate(type, id, date, start_et, ot, arena, attend)
        },
        filter_fn = function(data) {
          data %>% mutate(row_number = row_number())
        },
        select_cols =  c("-row_number", "-notes", "visitor", "home")
      )
  })
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

join_games_identifier <- function(initial_table, identifier) {
  joined_table <-
    join_identifier(initial_table = initial_table, identifier = identifier, row_number)
  
  joined_table
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