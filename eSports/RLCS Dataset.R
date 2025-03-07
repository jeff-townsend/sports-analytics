library(tidyverse)
library(ggthemes)
library(readr)

player.games.import <- read_delim("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/eSports/rlcs_2025_s1_eu.csv",
                                  delim = ";")
colnames(player.games.import) <- gsub(" ", "_", colnames(player.games.import))

player.games <- player.games.import

team.games <-
  player.games %>%
  group_by(replay_id, team_name) %>%
  summarize(gf = sum(goals),
            ga = mean(goals_conceded))
