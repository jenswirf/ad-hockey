library(rvest)
library(stringr)
library(magrittr)
library(lubridate)
library(tidyverse)

add_skater_fantasy <- function(df) {

  df <- df %>%
    mutate(f_points = (goals * 3) +
             (assists * 2) +
             (plus_minus * 1) +
             (penalty_minutes * .5) +
             (shots * .4) +
             (power_play_goals + power_play_assists))
  df

}

add_goalie_fantasy <- function(df) {


}

read_teams <- function() {

  teams <- data_frame(short = c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL",
                                "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NJD", "NSH",
                                "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR",
                                "VAN", "WPG", "WSH"),
                      long = c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres",
                               "Carolina Hurricanes", "Columbus Blue Jackets", "Calgary Flames",
                               "Chicago Blackhawks", "Colorado Avalanche", "Dallas Stars",
                               "Detroit Red Wings", "Edmonton Oilers", "Florida Panthers", "Los Angeles Kings",
                               "Minnesota Wild", "Montreal Canadiens", "New Jersey Devils",
                               "Nashville Predators", "New York Islanders", "New York Rangers",
                               "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins",
                               "San Jose Sharks", "St. Louis Blues", "Tampa Bay Lightning",
                               "Toronto Maple Leafs", "Vancouver Canucks", "Winnipeg Jets", "Washington Capitals"
                      ))
  teams
}

this_season <- function() {
  m <- month(now())
  y <- year(now())
  ifelse(m > 6, y + 1, y)
}
