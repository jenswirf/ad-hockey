library(tidyverse)
library(rvest)
library(stringr)


read_lines <- function() {


  message("Reading lineups.")
  teams <- read_teams()

  lines <- NULL

  for (i in 1:nrow(teams)) {

    cat(".")

    team <- str_replace_all(str_replace_all(str_to_lower(teams[i, 2]$long), "\\s", "-"), "\\.", "")

    url <- sprintf("http://www2.dailyfaceoff.com/teams/roster/16/%s/", team)

    line_page <- read_html(url)

    lines_table <- line_page %>%
      html_table() %>%
      .[[1]] %>%
      tbl_df()

    names(lines_table) <- c("player", "line")

    team_lines <- lines_table %>%
      separate(player, into = c("last", "first"), sep = ",") %>%
      transmute(player = str_trim(str_c(first, last, sep = " ")),
                line,
                team = teams[i, ]$short)

    lines <- bind_rows(lines, team_lines)
  }

  lines
}



