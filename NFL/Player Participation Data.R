library(tidyverse)
library(nflreadr)

# fantasy-relevant plays

pbp <-
  load_pbp(2023) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass"),
         two_point_attempt == 0) %>%
  select(game_id, season, week, play_id, desc, play_type,
         down, ydstogo, yardline_100, goal_to_go, qtr, score_differential, xpass,
         yards_gained, epa, passer_id, rusher_id, receiver_id)
pbp$id <- c(1:(nrow(pbp)))

# fantasy-relevant players

rosters <-
  load_rosters(2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id)) %>%
  rename(player_id = gsis_id,
         player = full_name) %>%
  select(player_id, season, player, position)

# game participation data

participation <-
  load_participation(2023) %>%
  rename(game_id = nflverse_game_id,
         team = possession_team) %>%
  select(game_id, play_id, team, offense_players) %>%
  inner_join(pbp, by = c("game_id", "play_id"))

players <-
  participation %>%
  separate_rows(offense_players, sep = ";") %>%
  rename(player_id = offense_players) %>%
  select(id, season, week, game_id, play_id, player_id, team, passer_id, rusher_id, receiver_id) %>%
  inner_join(rosters %>% select(gsis_id, player, position), by = "player_id") %>%
  mutate(used = ifelse(player_id == passer_id | player_id == rusher_id | player_id == receiver_id, 1, 0)) %>%
  mutate(used = ifelse(is.na(used), 0, used))

team.games <-
  participation %>%
  group_by(game_id, season, team) %>%
  summarize(team_snaps = n())

player.games <-
  players %>%
  group_by(player_id, game_id, season, week, player, position, team) %>%
  summarize(snaps = n(),
            opportunities = sum(used)) %>%
  ungroup() %>%
  inner_join(team.games, by = c("season", "game_id", "team")) %>%
  mutate(snap_rate = snaps / team_snaps,
         usage_rate = opportunities / snaps)

player.seasons <-
  player.games %>%
  group_by(player_id, season, player, position, team) %>%
  summarize(gp = n(),
            snaps = sum(snaps) / n(),
            opportunities = sum(opportunities) / n(),
            team_snaps = sum(team_snaps) / n()) %>%
  ungroup() %>%
  mutate(snap_rate = snaps / team_snaps,
         usage_rate = opportunities / snaps)

View(player.seasons %>% filter(position == "RB"))
