library(tidyverse)
library(rvest)
library(stringr)

read_injuries <- function(season) {

  message("Reading injuries:")

  teams <- read_teams()
  injuries <- NULL

  for (team in teams$short) {

    cat(".")

    url <- str_c("http://widgets.sports-reference.com/wg.fcgi?css=1&site=hr&url=%2Fteams%2F", team, "%2F", season,".html&div=div_injury")
    team_injuries <- NULL

    try({
      team_injuries <- read_html(url) %>%
        html_table() %>%
        .[[1]] %>%
        tbl_df() %>%
        transmute(player = Player,
                  team = team,
                  injury_date = Date_injury,
                  injury_type = Injury_type,
                  injury_note = Injury_note)
    }, silent = T)

    injuries <- bind_rows(injuries, team_injuries)

  }

  injuries
}
