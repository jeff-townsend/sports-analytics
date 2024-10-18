library(tidyverse)
library(hockeyR)

pbp <- load_pbp(2024)
games <- get_game_ids(season = 2024)

rosters <- data.frame(game_id = integer(),
                      team_id = integer(),
                      player_id = integer(),
                      player_name = character(),
                      position = character(),
                      position_type = character())

g <- 1
for(g in 1:(nrow(games))){

  game.roster <-
    cbind(g, get_game_rosters(games$game_id[g])) %>%
    rename(game_id = g)
  
  rosters <- rbind(rosters, game.roster)
  
  g <- g + 1
  
}

events <-
  pbp %>%
  filter(event %in% c("goal", "shot-on-goal"))
