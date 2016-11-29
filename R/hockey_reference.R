library(rvest)
library(stringr)
library(magrittr)
library(tidyverse)

read_games <- function(season) {

  message("Reading games.")

  teams <- read_teams()
  games <- NULL

  for (s in season) {
    for (team in teams$short) {

      cat(".")

      url <- sprintf("http://www.hockey-reference.com/teams/%s/%s_games.html", team, s)

      games_page <- read_html(url)

      games_table <- games_page %>%
        html_table(header = F) %>%
        .[[1]] %>%
        .[-c(1,2),] %>%
        as_tibble()

      names(games_table) <- c("game_index", "date", "time", "home_away", "versus", "goals",
                              "goals_against", "win_loss", "overtime","cum_wins","cum_losses", "cum_overtime_losses",
                              "streak","NULL2","shots",
                              "penalty_in_minutes_against", "power_play_goals", "power_play_opportunities",
                              "short_handed_goals","NULL3", "shots_against", "penalty_in_minutes_in_favor",
                              "power_play_goals_against", "power_play_opportunities_against",
                              "short_handed_goals_against", "attendance", "game_length", "notes")

      team_games <- games_table %>%
        select(-matches("NULL"), -notes, -time) %>%
        filter(game_index != "GP",
               goals != "") %>%
        mutate(season = s,
               team = team,
               game_index = as.integer(game_index)) %>%
        left_join(teams, by = c("versus" = "long")) %>%
        mutate(versus = short) %>%
        select(-short) %>%
        mutate(date = as.Date(date),
               home_away = ifelse(home_away == "", "H", "A")) %>%
        mutate_at(vars(matches("goals"),
                       matches("shots"),
                       matches("cum"),
                       matches("penalty"),
                       matches("oppor")), funs(as.integer)) %>%
        mutate(game_length = as.integer(str_extract(game_length, "\\d+")) * 60 +
                 as.integer(str_extract(game_length, "\\d+$"))) %>%
        mutate(attendance = as.numeric(str_replace_all(attendance, ",", ""))) %>%
        .[,c(26, 25, 1:24)]

      games <- bind_rows(games, team_games)

    }
  }

  games

}


read_schedule <- function(season) {

  message("Reading schedule.")

  schedules <- NULL

  for (s in season) {

    cat(".")

    url <- sprintf("http://www.hockey-reference.com/leagues/NHL_%s_games.html", s)
    schedule_page <- read_html(url)

    schedule <- schedule_page %>%
      html_table(header = F) %>%
      .[[1]] %>%
      .[-1,] %>%
      .[,c(1,2,4)] %>%
      tbl_df()

    names(schedule) <- c("date", "visitor", "home")

    links <- schedule_page %>%
      html_nodes("#games a") %>%
      html_attr("href") %>%
      subset(str_detect(., "boxscores"))

    schedule <- schedule %>%
      mutate(season = s,
             link = c(str_c("http://www.hockey-reference.com", links), rep(NA, nrow(schedule) - length(links))))


    schedules <- bind_rows(schedules, schedule)
  }

  schedules
}



read_player_games <- function(season, update) {

  if (!missing(update))
    season <- 2017 # this_season()

  games <- read_schedule(season) %>%
    filter(!is.na(link))

  if (!missing(update)) {
    games <- games %>%
      filter(date >= update)
  }

  message("Reading player games.")

  skaters <- NULL
  goalies <- NULL

  for (l in 1:nrow(games)) {

    cat(".")

    url <- games$link[l]
    dt <- games$date[l]

    game_page <- read_html(url)

    table_ids <- game_page %>%
      html_nodes("table") %>%
      html_attr("id")

    tables <- game_page %>%
      html_nodes("table") %>%
      .[3:6] %>%
      html_table(fill = T)

    names(tables) <- table_ids[3:6]

    teams <- subset(table_ids, str_detect(table_ids, "_skaters")) %>% str_replace("_skaters", "")
    visitor <- teams[1]
    home <- teams[2]

    # skater stats
    visitor_skater_stats <- tables[1][[1]][-c(1:2),]
    home_skater_stats <- tables[3][[1]][-c(1:2),]

    names(visitor_skater_stats) <- names(home_skater_stats) <-
      c("rank", "player", "goals", "assists", "points", "plus_minus", "penalty_minutes", "even_strength_goals",
        "power_play_goals", "short_handed_goals", "game_winning_goals", "even_strengt_assists",
        "power_play_assists", "short_handed_assists", "shots", "shot_percentage", "shifts", "minutes_on_ice")

    visitor_skater_stats %<>% mutate(team = visitor, versus = home)
    home_skater_stats %<>% mutate(team = home, versus = visitor)

    skater_stats <- visitor_skater_stats %>%
      bind_rows(home_skater_stats) %>%
      filter(player != "TOTAL") %>%
      mutate(minutes_on_ice = round(as.integer(str_extract(minutes_on_ice, "\\d+")) +
                                      (as.integer(str_extract(minutes_on_ice, "\\d+$")) / 60), 1),
             shot_percentage = as.numeric(shot_percentage)) %>%
      mutate_at(vars(-player, -team, -shot_percentage, -minutes_on_ice), as.integer) %>%
      replace_na(list(shot_percentage = 0)) %>%
      mutate(shot_percentage = shot_percentage / 100) %>%
      mutate(date = as.Date(dt)) %>%
      select(-rank)

    # goalie stats
    visitor_goalie_stats <- tables[2][[1]][-c(1),]
    home_goalie_stats <- tables[4][[1]][-c(1),]

    names(visitor_goalie_stats) <- names(home_goalie_stats) <-
      c("rank", "player", "decision", "goals_allowed", "shots_against", "saves", "save_percentage",
        "shutout", "penalty_minutes", "minutes_on_ice")

    visitor_goalie_stats %<>% mutate(team = visitor, versus = home)
    home_goalie_stats %<>% mutate(team = home, versus = visitor)

    goalie_stats <- visitor_goalie_stats %>%
      bind_rows(home_goalie_stats) %>%
      mutate(minutes_on_ice = round(as.integer(str_extract(minutes_on_ice, "\\d+")) +
                                      (as.integer(str_extract(minutes_on_ice, "\\d+$")) / 60), 1),
             save_percentage = as.numeric(save_percentage)) %>%
      mutate_at(vars(goals_allowed, shots_against, saves, shutout, penalty_minutes), funs(as.integer)) %>%
      mutate(date = as.Date(dt)) %>%
      select(-rank) %>%
      group_by(team) %>%
      mutate(decision = max(decision))


    skaters <- bind_rows(skaters, skater_stats)
    goalies <- bind_rows(goalies, goalie_stats)
  }


  out <- list(skaters = as_tibble(skaters),
              goalies = as_tibble(goalies))
  out
}


# read basic info on all players from hockey reference
read_players <- function(season) {

  message("Reading players:")

  roster <- NULL
  teams <- read_teams()

  for (team in teams$short) {

    cat(".")

    url <- sprintf("http://www.hockey-reference.com/teams/%s/%s.html", team, season)
    team_page <- read_html(url)

    # parse tables
    team_roster <- team_page %>%
      html_nodes("table") %>%
      .[2] %>%
      html_table(fill = T) %>%
      .[[1]] %>%
      .[,c(1:10, 12)] %>%
      tbl_df() %>%
      transmute(no = No.,
                player = str_replace_all(Player,"\\s\\(.\\)", ""),
                letter = str_replace_all(str_extract(Player, "\\(.+\\)"), "\\(|\\)", ""),
                origin = str_to_upper(Flag),
                position = Pos,
                age = Age,
                height = Ht,
                weight = Wt,
                shoots = str_extract(`S/C`, "[[:alpha:]]"),
                exp = as.character(Exp),
                salary = Salary,
                team = team)

    if (is.null(roster)) {
      roster <- team_roster
    } else {
      roster <- bind_rows(roster, team_roster)
    }
  }

  roster
}
