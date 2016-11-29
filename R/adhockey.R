library(rvest)
library(stringr)
library(magrittr)
library(lubridate)
library(tidyverse)

refresh_all <- function(dir = "../../data") {

  # schedule
  schedule <<- read_schedule(this_season())

  # games
  games <<- update_games(dir)

  # player games
  player_games <- update_player_games(dir)
  skater_games <<- player_games$skaters
  goalie_games <<- player_games$goalies


  # players + (injuries) + (lines)
  injuries <- read_injuries(this_season())
  # teams


  # fantasy_roster


  .updated <- now()
  write_file(sprintf("Data as of _%s_", today()), "readme.md")

  invisible()
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
