library(tidyverse)
library(rvest)
library(stringr)


read_fantasy_roster <- function(league_id = Sys.getenv("YAHOO_LEAGUE"),
                                user = Sys.getenv("YAHOO_USR"),
                                passwd = Sys.getenv("YAHOO_PWD")) {

  message("Reading yahoo league roster:")

  cat(".")

  get_fantasy_page <- function(url) {
    s <- html_session(url)
    form <- html_form(s)[[1]]
    form <- set_values(form, username = user)
    s <- submit_form(s, form)
    form <- html_form(s)[[1]]
    form <- set_values(form, passwd = passwd)
    s <- submit_form(s,form)
    s <- jump_to(s, url)
    page <- read_html(s)
    page
  }

  clean_roster <- function(df) {

    tbl_df(df) %>%
      rename(roster_spot = Pos) %>%
      separate(Player, sep = "\n", into = c("x1", "player", "x2", "x3", "status", "next_game")) %>%
      select(-starts_with("x")) %>%
      separate(player, sep = " - ", into = c("player", "position")) %>%
      mutate(player = str_trim(player),
             team = str_to_upper(str_extract(player, "\\w{2,3}$")),
             player = str_replace(player, "\\w{2,3}$", ""),
             home_away = ifelse(str_detect(next_game, "\\@"), "away", "home"),
             next_versus = str_to_upper(str_extract(next_game, "\\w{2,3}$")),
             next_game = str_extract(next_game, "\\d:\\d\\d am"),
             has_game_tonight = ifelse(!is.na(next_game), 1, 0),
             status = ifelse(str_trim(status) == "", NA, status)) %>%
      mutate_all(str_trim) %>%
      mutate(team = ifelse(team == "SJ", "SJS", team),
             team = ifelse(team == "TB", "TBL", team),
             team = ifelse(team == "LA", "LAK", team),
             team = ifelse(team == "NJ", "NJD", team),
             team = ifelse(team == "ANH", "ANA", team)) -> out

    out
  }

  roster <- NULL

  try({

    url <- sprintf("https://hockey.fantasysports.yahoo.com/hockey/65267/startingrosters", league_id)

    roster_page <- suppressMessages(get_fantasy_page(url))

    tables <- roster_page %>% html_nodes(".Table") %>% html_table()

    owners <- roster_page %>% html_nodes(".W-100") %>% html_nodes("a") %>%
      subset(str_detect(., "/hockey/\\d+/\\d")) %>%
      html_text()

    names(tables) <- owners

    suppressWarnings(
      roster <- map_df(tables, clean_roster) %>%
        mutate(owner = rep(owners, times = map_int(tables, nrow)))
    )

  }, silent = T)


  if (is.null(roster)) {
    return("Error: Failed")
  }

  roster
}
