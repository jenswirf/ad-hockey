library(rvest)
library(stringr)
library(magrittr)
library(lubridate)
library(tidyverse)


update_all <- function(dir = "data") {

  schedule <<- read_schedule(this_season())

  games <<- update_games(dir)

  player_games <- update_player_games(dir)
  skater_games <<- player_games$skaters
  goalie_games <<- player_games$goalies

  players <<- update_players(dir, this_season())

  injuries <<- update_injuries(dir, this_season())

  fantasy_roster <<- update_fantasy_roster(dir)

  # teams

  # lines

  .updated <- now()
  write_file(sprintf("Data as of _%s_", today()), "readme.md")

  invisible()
}


update_fantasy_roster <- function(dir) {

  fantasy_roster <- read_fantasy_roster()
  write_csv(fantasy_roster, file.path(dir, "fantasy_roster.csv"))

  fantasy_roster
}

update_injuries <- function(dir, ...) {

  injuries <- read_injuries(...)
  write_csv(injuries, file.path(dir, "injuries.csv"))

  injuries
}

update_players <- function(dir, ...) {

  players <- read_players(...)
  write_csv(players, file.path(dir, "players.csv"))

  players
}

update_player_games <- function(dir) {

  if (!all(file.exists(file.path(dir, "skater_games.csv"), file.path(dir, "goalie_games.csv"))))
    stop("Data files are missing.")

  # old
  skater_games_old <- read_csv(file.path(dir, "skater_games.csv"))
  goalie_games_old <- read_csv(file.path(dir, "goalie_games.csv"))
  max_dt <- max(skater_games_old$date)

  # new
  player_games <- read_player_games(update = max_dt)
  skater_games_new <- player_games$skaters
  goalie_games_new <- player_games$goalies

  # bind
  skater_games <- skater_games_old %>%
    filter(date < max_dt) %>%
    bind_rows(skater_games_new)

  goalie_games <- goalie_games_old %>%
    filter(date < max_dt) %>%
    bind_rows(goalie_games_new)

  # rewrite
  skater_games %>% write_csv(file.path(dir, "skater_games.csv"))
  goalie_games %>% write_csv(file.path(dir, "goalie_games.csv"))

  list(skaters = skater_games,
       goalies = goalie_games)
}


update_games <- function(dir) {

  # old
  games_old <- read_csv(file.path(dir, "games.csv"))
  max_season <- max(games_old$season)

  # new
  games_new <- read_games(season = max_season)

  # bind
  games <- games_old %>%
    filter(season < max_season) %>%
    bind_rows(games_new)

  # rewrite
  games %>% write_csv(file.path(dir, "games.csv"))

  games
}
