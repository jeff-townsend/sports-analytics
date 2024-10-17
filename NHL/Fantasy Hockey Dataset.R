library(tidyverse)
library(readr)

skaters.import <- read_csv("NHL/2023-24 NHL Skater Data.csv")

skaters <-
  skaters.import %>%
  filter(situation != "all") %>%
  mutate(assists = I_F_primaryAssists + I_F_secondaryAssists) %>%
  select(playerId, name, season, team, situation, position, games_played, icetime,
         I_F_goals, assists, I_F_shotsOnGoal, OnIce_F_goals, OnIce_A_goals) %>%
  rename(player_id = playerId,
         player = name,
         toi = icetime,
         goals = I_F_goals,
         sog = I_F_shotsOnGoal,
         gf = OnIce_F_goals,
         ga = OnIce_A_goals) %>%
  mutate(toi = toi / 60,
         pp_toi = ifelse(situation == "5on4", toi, 0),
         plus_minus = ifelse(situation == "5on5", gf - ga,
                             ifelse(situation == "5on4", -ga,
                                    ifelse(situation == "4on5", gf, 0))),
         pp_points = ifelse(situation == "5on4", goals + assists, 0)) %>%
  mutate(fantasy_points = 6*goals + 4*assists + 0.9*sog + 2*plus_minus + 2*pp_points) %>%
  group_by(player_id, player, season, team, position, games_played) %>%
  summarize(toi = sum(toi),
            pp_toi = sum(pp_toi),
            goals = sum(goals),
            assists = sum(assists),
            sog = sum(sog),
            plus_minus = sum(plus_minus),
            pp_points = sum(pp_points),
            fantasy_points = sum(fantasy_points))
