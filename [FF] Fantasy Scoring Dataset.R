library(tidyverse)
library(nflreadr)

# fantasy-relevant plays
start.season <- 2016

pbp <-
  load_pbp(start.season:2023) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass", "qb_kneel")) %>%
  mutate(off_touchdown = ifelse(touchdown == 1, ifelse(posteam == td_team, 1, 0), 0))

# fantasy-relevant players
rosters <-
  load_rosters(start.season:2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id)) %>%
  rename(player = full_name)

# game participation data
participation <- load_participation(2016:2023)

off.usage <-
  participation %>%
  separate_rows(offense_players, sep = ";") %>%
  distinct(nflverse_game_id, offense_players) %>%
  rename(game_id = nflverse_game_id,
         player_id = offense_players) %>%
  inner_join(rosters %>% select(player, position, gsis_id), by = c("player_id" = "gsis_id"))

# define replacement level

replacement.ranks <- data.frame(position = c("QB", "RB", "WR", "TE"),
                                prk = c(13, 31, 31, 13))

# passing scoring
pbp.pass <-
  pbp %>%
  filter(play_type == "pass",
         two_point_attempt == 0) %>%
  mutate(fumble = ifelse(fumble == 0, 0, ifelse(passer_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, yards_gained, off_touchdown, interception, fumble, sack, passer_id) %>%
  rename(touchdown = off_touchdown) %>%
  inner_join(rosters %>% select(season, player, position, gsis_id), by = c("passer_id" = "gsis_id", "season"))

pass.points <-
  pbp.pass %>%
  group_by(season, passer_id, player, position) %>%
  rename(player_id = passer_id) %>%
  summarize(pass_yards = sum(yards_gained * (1 - sack)),
            pass_tds = sum(touchdown),
            int = sum(interception),
            pass_fum = sum(fumble)) %>%
  ungroup() %>%
  mutate(pass_points = 0.04*pass_yards + 4*pass_tds - 2*int - pass_fum)

# rushing scoring
pbp.rush <-
  pbp %>%
  filter(play_type %in% c("run", "qb_kneel"),
         two_point_attempt == 0) %>%
  mutate(rusher_id = ifelse(qb_scramble == 1, passer_id, rusher_id),
         fumble = ifelse(fumble == 0, 0, ifelse(rusher_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, yards_gained, off_touchdown, fumble, rusher_id) %>%
  rename(touchdown = off_touchdown) %>%
  inner_join(rosters %>% select(season, player, position, gsis_id), by = c("rusher_id" = "gsis_id", "season"))

rush.points <-
  pbp.rush %>%
  group_by(season, rusher_id, player, position) %>%
  rename(player_id = rusher_id) %>%
  summarize(rush_yards = sum(yards_gained),
            rush_tds = sum(touchdown),
            rush_fum = sum(fumble)) %>%
  ungroup() %>%
  mutate(rush_points = 0.1*rush_yards + 6*rush_tds - rush_fum)

# receiving scoring
pbp.rec <-
  pbp %>%
  filter(play_type == "pass",
         two_point_attempt == 0) %>%
  mutate(fumble = ifelse(fumble == 0, 0, ifelse(receiver_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, complete_pass, yards_gained, off_touchdown, fumble, receiver_id) %>%
  rename(touchdown = off_touchdown) %>%
  inner_join(rosters %>% select(season, player, position, gsis_id), by = c("receiver_id" = "gsis_id", "season"))

rec.points <-
  pbp.rec %>%
  group_by(season, receiver_id, player, position) %>%
  rename(player_id = receiver_id) %>%
  summarize(rec = sum(complete_pass),
            rec_yards = sum(yards_gained * complete_pass), ## has a few small discrepancies
            rec_tds = sum(touchdown),
            rec_fum = sum(fumble)) %>%
  ungroup() %>%
  mutate(rec_points = 0.5*rec + 0.1*rec_yards + 6*rec_tds - rec_fum)

total.points <-
  pass.points %>%
  full_join(rush.points, by = c("season", "player_id", "player", "position")) %>%
  full_join(rec.points, by = c("season", "player_id", "player", "position"))
total.points[is.na(total.points)] <- 0
total.points$fantasy_points <- with(total.points, pass_points + rush_points + rec_points)

position.ranks <-
  total.points %>%
  select(season, player_id, player, position, fantasy_points) %>%
  group_by(season, position) %>%
  mutate(prk = rank(desc(fantasy_points), ties.method = "first"))

replacement.points <-
  position.ranks %>%
  inner_join(replacement.ranks, by = c("position", "prk")) %>%
  select(season, position, fantasy_points) %>%
  rename(replacement_points = fantasy_points)

player.value <-
  position.ranks %>%
  inner_join(replacement.points, by = c("season", "position")) %>%
  mutate(fpar = fantasy_points - replacement_points) %>%
  select(-replacement_points) %>%
  group_by(season) %>%
  mutate(ork = rank(desc(fpar), ties.method = "first"))

View(player.value %>% filter(player == "Ja'Marr Chase") %>% arrange(desc(season)))
