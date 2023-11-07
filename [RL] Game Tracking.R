library(readr)
library(tidyverse)
library(janitor)
library(ggthemes)

solo_games_load <- read_csv("Data Analysis/Rocket League/s12 matches (player level).csv")
player_games_load <-
  rbind(read_csv("Data Analysis/Rocket League/Christian Gaming Alliance/October Games (player level).csv"),
        read_csv("Data Analysis/Rocket League/Christian Gaming Alliance/November Games (player level).csv"))
team_games_load <-
  rbind(read_csv("Data Analysis/Rocket League/Christian Gaming Alliance/October Games (team level).csv"),
        read_csv("Data Analysis/Rocket League/Christian Gaming Alliance/November Games (team level).csv"))

solo_games <-
  solo_games_load %>%
  clean_names() %>%
  mutate(game_duration_full = total_distance / avg_speed)

player_games <-
  player_games_load %>%
  clean_names() %>%
  mutate(game_duration_full = total_distance / avg_speed)

team_games <-
  team_games_load %>%
  clean_names()

### Milk Men Stats
team_offense <-
  player_games %>%
  filter(team_name == "MysterioustheDave & qazabaz & JT") %>%
  group_by(replay_id, date, result) %>%
  summarize(duration = min(game_duration_full),
            gf = sum(goals),
            sf = sum(shots))

team_defense <-
  player_games %>%
  filter(opposing_team_name == "MysterioustheDave & qazabaz & JT") %>%
  group_by(replay_id, date) %>%
  summarize(ga = sum(goals),
            sa = sum(shots))

team_stats <-
  team_offense %>%
  inner_join(team_defense, by = c("replay_id", "date"))

team_stats %>%
  group_by(result) %>%
  summarize(gf = mean(gf),
            ga = mean(ga),
            sf = mean(sf),
            sa = mean(sa))

team_games %>%
  filter(team_name == "MysterioustheDave & qazabaz & JT") %>%
  mutate(shot_advantage = ifelse(shots > shots_conceded, 1, ifelse(shots == shots_conceded, 0.5, 0))) %>%
  group_by(result, shot_advantage) %>%
  summarize(games = n())

player_games %>%
  filter(team_name == "qazabaz & JT & MysterioustheDave") %>%
  group_by(result, player_name) %>%
  summarize(score = mean(score),
            goals = mean(goals),
            shots = mean(shots)) %>%
  arrange(player_name, result)

player_speed <-
  player_games %>%
  filter(team_name == "qazabaz & JT & MysterioustheDave") %>%
  select(replay_id, player_name, result, avg_speed)

ggplot(player_speed, aes(x = avg_speed, fill = player_name)) +
  geom_density(alpha = 0.75) +
  theme_fivethirtyeight() +
  theme(title = element_text(),
        legend.title = element_blank()) +
  ggtitle("Average Speed")

player_speed %>%
  group_by(player_name) %>%
  summarize(sd = sd(avg_speed),
            cv = sd(avg_speed) / mean(avg_speed))

player_speed %>%
  group_by(player_name, result) %>%
  summarize(speed = mean(avg_speed))
    
dave_speed <-
  player_games %>%
  filter(player_name == "MysterioustheDave") %>%
  select(result, avg_speed, demos_inflicted)

cor(dave_speed$avg_speed, dave_speed$demos_inflicted)

dave_speed %>%
  group_by(demos_inflicted) %>%
  summarize(games = n(),
            avg_speed = mean(avg_speed),
            win_rate = mean(ifelse(result == "win", 1, 0)))

### Solo Queue Stats
my_teams <-
  solo_games %>%
  filter(player_name == "JT") %>%
  distinct(team_name)

solo_team_offense <-
  solo_games %>%
  inner_join(my_teams, by = "team_name") %>%
  group_by(replay_id, replay_title, map, date, result) %>%
  summarize(duration = mean(game_duration_full),
            team_goals = sum(goals),
            team_shots = sum(shots),
            team_saves = sum(saves),
            team_score = sum(score),
            team_boost = mean(avg_boost_amount),
            speed = mean(ifelse(player_name == "JT", avg_speed, NA), na.rm = TRUE),
            goals = mean(ifelse(player_name == "JT", goals, NA), na.rm = TRUE),
            assists = mean(ifelse(player_name == "JT", assists, NA), na.rm = TRUE),
            saves = mean(ifelse(player_name == "JT", saves, NA), na.rm = TRUE),
            shots = mean(ifelse(player_name == "JT", shots, NA), na.rm = TRUE),
            score = mean(ifelse(player_name == "JT", score, NA), na.rm = TRUE)) %>%
  ungroup()

solo_team_defense <-
  solo_games %>%
  inner_join(my_teams, by = c("opposing_team_name" = "team_name")) %>%
  group_by(replay_id, replay_title, map, date) %>%
  summarize(opp_goals = sum(goals),
            opp_shots = sum(shots),
            opp_saves = sum(saves),
            opp_score = sum(score),
            opp_boost = mean(avg_boost_amount)) %>%
  ungroup()

solo_team_stats <-
  solo_team_offense %>%
  inner_join(solo_team_defense, by = c("replay_id", "replay_title", "map", "date")) %>%
  mutate(speed_game = ifelse(date >= ymd_hms("2023-11-05 17:12:54"), 1, 0))

solo_team_stats %>%
  group_by(speed_game) %>%
  summarize(speed = mean(speed),
            games = n(),
            wins = sum(ifelse(result == "win", 1, 0)),
            win_rate = mean(ifelse(result == "win", 1, 0)),
            gf = mean(team_goals) / mean(duration) * 300,
            ga = mean(opp_goals) / mean(duration) * 300,
            sf = mean(team_shots) / mean(duration) * 300,
            sa = mean(opp_shots) / mean(duration) * 300,
            tm_saves = mean(team_saves) / mean(duration) * 300,
            opp_saves = mean(opp_saves) / mean(duration) * 300,
            tm_score = mean(team_score) / mean(duration) * 300,
            opp_score = mean(opp_score) / mean(duration) * 300,
            goals = mean(goals) / mean(duration) * 300,
            shots = mean(shots) / mean(duration) * 300,
            assists = mean(assists) / mean(duration) * 300,
            saves = mean(saves) / mean(duration) * 300,
            score = mean(score) / mean(duration) * 300)
