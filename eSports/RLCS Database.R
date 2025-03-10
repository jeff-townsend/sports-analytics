library(tidyverse)
library(ggthemes)
library(readr)

player.games.import <- read_delim("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/eSports/rlcs_2025_s1_eu.csv",
                                  delim = ";")
colnames(player.games.import) <- gsub(" ", "_", colnames(player.games.import))

player.games <- player.games.import

team.games <-
  player.games %>%
  filter(result == "win") %>%
  group_by(replay_id, date, team_name, opposing_team_name) %>%
  summarize(winning_score = sum(goals),
            losing_score = mean(goals_conceded)) %>%
  ungroup() %>%
  rename(replay_date = date,
         winning_team = team_name,
         losing_team = opposing_team_name) %>%
  arrange(replay_date, replay_id)
game.ids <- data.frame(game_id = c(1:nrow(team.games)))
games <- cbind(game.ids, team.games)
