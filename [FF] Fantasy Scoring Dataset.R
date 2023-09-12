library(tidyverse)
library(nflreadr)

# fantasy-relevant plays
nfl.pbp <-
  load_pbp(2023) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass", "qb_kneel"))
# fantasy-relevant players
nfl.roster <-
  load_rosters(2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id))

# passing scoring
nfl.pbp.pass <-
  nfl.pbp %>%
  filter(play_type == "pass",
         two_point_attempt == 0) %>%
  mutate(fumble = ifelse(fumble == 0, 0, ifelse(passer_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, yards_gained, touchdown, interception, fumble, sack, passer_id) %>%
  inner_join(nfl.roster %>% select(season, full_name, position, gsis_id), by = c("passer_id" = "gsis_id", "season"))

# rushing scoring
nfl.pbp.rush <-
  nfl.pbp %>%
  filter(play_type %in% c("run", "qb_kneel"),
         two_point_attempt == 0) %>%
  mutate(rusher_id = ifelse(qb_scramble == 1, passer_id, rusher_id),
         fumble = ifelse(fumble == 0, 0, ifelse(rusher_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, yards_gained, touchdown, fumble, rusher_id) %>%
  inner_join(nfl.roster %>% select(season, full_name, position, gsis_id), by = c("rusher_id" = "gsis_id", "season"))

# receiving scoring
nfl.pbp.rec <-
  nfl.pbp %>%
  filter(play_type == "pass",
         two_point_attempt == 0) %>%
  mutate(fumble = ifelse(fumble == 0, 0, ifelse(receiver_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, complete_pass, yards_gained, touchdown, fumble, receiver_id) %>%
  inner_join(nfl.roster %>% select(season, full_name, position, gsis_id), by = c("receiver_id" = "gsis_id", "season"))
