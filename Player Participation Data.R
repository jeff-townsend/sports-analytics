library(tidyverse)
library(nflreadr)

# fantasy-relevant plays

pbp <-
  load_pbp(2023) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass", "qb_kneel"))
pbp$pbp_id <- c(1:(nrow(pbp)))

# fantasy-relevant players

rosters <-
  load_rosters(2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id)) %>%
  rename(player = full_name)

# game participation data

participation <-
  load_participation(2023) %>%
  rename(game_id = nflverse_game_id,
         team = possession_team) %>%
  inner_join(pbp %>% select(season, game_id, play_id), by = c("game_id", "play_id")) %>%
  mutate(rbs = substr(offense_personnel,
                      unlist(gregexpr("RB", offense_personnel)) - 2,
                      unlist(gregexpr("RB", offense_personnel)) - 2),
         tes = substr(offense_personnel,
                      unlist(gregexpr("TE", offense_personnel)) - 2,
                      unlist(gregexpr("TE", offense_personnel)) - 2),
         wrs = substr(offense_personnel,
                      unlist(gregexpr("WR", offense_personnel)) - 2,
                      unlist(gregexpr("WR", offense_personnel)) - 2))
participation$participation_id <- c(1:(nrow(participation)))

players <-
  participation %>%
  separate_rows(offense_players, sep = ";") %>%
  rename(player_id = offense_players) %>%
  select(participation_id, season, game_id, player_id, team, offense_personnel, rbs, tes, wrs) %>%
  inner_join(rosters %>% select(season, gsis_id, player, position), by = c("season", "player_id" = "gsis_id"))

team.games <-
  participation %>%
  group_by(game_id, team) %>%
  summarize(team_snaps = n(),
            team_snaps_11 = sum(ifelse(offense_personnel == "1 RB, 1 TE, 3 WR", 1, 0)),
            team_snaps_12 = sum(ifelse(offense_personnel == "1 RB, 2 TE, 2 WR", 1, 0)),
            team_snaps_13 = sum(ifelse(offense_personnel == "1 RB, 3 TE, 1 WR", 1, 0)),
            team_snaps_21 = sum(ifelse(offense_personnel == "2 RB, 1 TE, 2 WR", 1, 0)),
            team_snaps_22 = sum(ifelse(offense_personnel == "2 RB, 2 TE, 1 WR", 1, 0))) %>%
  ungroup() %>%
  mutate(team_snaps_other = team_snaps - team_snaps_11 - team_snaps_12 - team_snaps_13 - team_snaps_21 - team_snaps_22)

player.games <-
  players %>%
  group_by(player_id, player, position, game_id, team) %>%
  summarize(snaps = n(),
            snaps_11 = sum(ifelse(offense_personnel == "1 RB, 1 TE, 3 WR", 1, 0)),
            snaps_12 = sum(ifelse(offense_personnel == "1 RB, 2 TE, 2 WR", 1, 0)),
            snaps_13 = sum(ifelse(offense_personnel == "1 RB, 3 TE, 1 WR", 1, 0)),
            snaps_21 = sum(ifelse(offense_personnel == "2 RB, 1 TE, 2 WR", 1, 0)),
            snaps_22 = sum(ifelse(offense_personnel == "2 RB, 2 TE, 1 WR", 1, 0))) %>%
  ungroup() %>%
  mutate(snaps_other = snaps - (snaps_11 + snaps_12 + snaps_13 + snaps_21 + snaps_22))
  
player.snaps <-
  player.games %>%
  inner_join(team.games, by = c("game_id", "team")) %>%
  group_by(player_id, player, position) %>%
  summarize(snaps = sum(snaps),
            snaps_11 = sum(snaps_11),
            snaps_12 = sum(snaps_12),
            snaps_13 = sum(snaps_13),
            snaps_21 = sum(snaps_21),
            snaps_22 = sum(snaps_22),
            snaps_other = sum(snaps_other),
            team_snaps = sum(team_snaps),
            team_snaps_11 = sum(team_snaps_11),
            team_snaps_12 = sum(team_snaps_12),
            team_snaps_13 = sum(team_snaps_13),
            team_snaps_21 = sum(team_snaps_21),
            team_snaps_22 = sum(team_snaps_22),
            team_snaps_other = sum(team_snaps_other)) %>%
  ungroup() %>%
  mutate(snap_share = snaps / team_snaps,
         snap_share_11 = snaps_11 / team_snaps_11,
         snap_share_12 = snaps_12 / team_snaps_12,
         snap_share_13 = snaps_13 / team_snaps_13,
         snap_share_21 = snaps_21 / team_snaps_21,
         snap_share_22 = snaps_22 / team_snaps_22,
         snap_share_other = snaps_other / team_snaps_other)
  #select(-c(team_snaps, team_snaps_11, team_snaps_12, team_snaps_13, team_snaps_21, team_snaps_22, team_snaps_other))
  

## WR snaps by personnel

wr.snaps <-
  player.snaps %>%
  filter(position == "WR") %>%
  mutate(snaps_2wr = snaps_12 + snaps_21,
         snaps_3wr = snaps_11,
         team_snaps_2wr = team_snaps_12 + team_snaps_21,
         team_snaps_3wr = team_snaps_11,
         snap_share_2wr = snaps_2wr / team_snaps_2wr,
         snap_share_3wr = snaps_3wr / team_snaps_3wr,
         personnel_dependency = snap_share_3wr - snap_share_2wr) %>%
  select(player, snaps, snaps_2wr, snaps_3wr, snap_share_2wr, snap_share_3wr, personnel_dependency)
