library(tidyverse)
library(nflreadr)

#### PBP Import Code #####

pbp.import <-
  load_pbp() %>%
  filter(!is.na(play_type))

pbp <-
  pbp.import %>%
  rename(play_type_pbp = play_type) %>%
  arrange(start_time, game_id, play_id) %>%
  mutate(play_id = c(1:(nrow(pbp.import))))

##### Play Classification Code #####

plays <-
  pbp %>%
  select(play_id, game_id, week, posteam, defteam, down, ydstogo, yards_gained, play_type_pbp, play_type_nfl,
         qb_dropback, pass_attempt, sack, qb_scramble, rush_attempt, qb_spike, qb_kneel,
         penalty, penalty_team, penalty_yards, epa, success) %>%
  ## overwrite NA values
  mutate(yards_gained = ifelse(!is.na(yards_gained), yards_gained, 0),
         pass_attempt = ifelse(!is.na(pass_attempt), pass_attempt, 0),
         sack = ifelse(!is.na(sack), sack, 0),
         rush_attempt = ifelse(!is.na(rush_attempt), rush_attempt, 0),
         penalty = ifelse(!is.na(penalty), penalty, 1), # offsetting penalties
         penalty_yards = ifelse(!is.na(penalty_yards), penalty_yards, 0)) %>%
  mutate(play = ifelse(play_type_pbp == "no_play", 0, 1),
         unit = ifelse(play_type_nfl %in% c("KICK_OFF", "PUNT", "FIELD_GOAL", "XP_KICK"), "Special Teams", "Offense"),
         sub_unit = ifelse(unit == "Special Teams",
                           ifelse(play_type_nfl == "KICK_OFF",
                                  "Kickoff",
                                  ifelse(play_type_nfl == "PUNT",
                                         "Punt",
                                         ifelse(play_type_nfl == "FIELD_GOAL",
                                                "Field Goal",
                                                "Extra Point"))),
                           ifelse(play_type_nfl == "PAT2",
                                  "Conversion",
                                  "Offense")),
         penalty_type = ifelse(penalty == 1,
                               ifelse(is.na(penalty_team),
                                      "Offsetting Penalties",
                                      ifelse(penalty_team == posteam,
                                             "Offensive Penalty",
                                             "Defensive Penalty")),
                               "No Penalty")) %>%
  mutate(play_type = ifelse(qb_dropback == 1 & pass_attempt == 1 & sack == 0, "Pass Attempt", "Other"),
         play_type = ifelse(qb_dropback == 1 & sack == 1, "Sack", play_type),
         play_type = ifelse(qb_dropback == 1 & qb_scramble == 1, "Scramble", play_type),
         play_type = ifelse(qb_dropback == 0 & rush_attempt == 1 & qb_kneel == 0, "Designed Run", play_type),
         play_type = ifelse(qb_spike == 1, "Spike", play_type),
         play_type = ifelse(qb_kneel == 1, "Kneel", play_type),
         play_type = ifelse(penalty == 1 & play == 0, "Penalty Play", play_type),
         play_type = ifelse(play_type_nfl == "KICK_OFF" & play == 1, "Kickoff", play_type),
         play_type = ifelse(play_type_nfl == "PUNT" & play == 1, "Punt", play_type),
         play_type = ifelse(play_type_nfl == "FIELD_GOAL" & play == 1, "Field Goal", play_type),
         play_type = ifelse(play_type_nfl == "XP_KICK" & play == 1, "Extra Point", play_type),
         play_type = ifelse(play_type_nfl == "TIMEOUT", "Timeout", play_type)) %>%
  mutate(offensive_play = ifelse(play_type %in% c("Pass Attempt", "Designed Run", "Scramble", "Sack") & sub_unit == "Offense", 1, 0)) %>%
  mutate(pass_attempt = ifelse(qb_dropback == 1 & pass_attempt == 1 & sack == 0, 1, 0),
         sack = ifelse(qb_dropback == 1 & sack == 1, 1, 0),
         scramble = ifelse(qb_dropback == 1 & qb_scramble == 1, 1, 0),
         dropback = ifelse(pass_attempt + sack + scramble >= 1, 1, 0),
         designed_run = ifelse(qb_dropback == 0 & rush_attempt == 1 & qb_kneel == 0, 1, 0),
         spike = ifelse(qb_spike == 1 & play == 1, 1, 0),
         kneel = ifelse(qb_kneel == 1 & play == 1, 1, 0),
         penalty_play = ifelse(penalty == 1 & play == 0, 1, 0),
         kickoff = ifelse(play_type_nfl == "KICK_OFF" & play == 1, 1, 0),
         punt = ifelse(play_type_nfl == "PUNT" & play == 1, 1, 0),
         field_goal = ifelse(play_type_nfl == "FIELD_GOAL" & play == 1, 1, 0),
         extra_point = ifelse(play_type_nfl == "XP_KICK" & play == 1, 1, 0),
         timeout = ifelse(play_type_nfl == "TIMEOUT", 1, 0)) %>%
  mutate(play_types = pass_attempt + sack + scramble + designed_run + spike + kneel + penalty_play +
           kickoff + punt + field_goal + extra_point + timeout) %>%
  select(play_id, game_id, week, posteam, defteam, down, ydstogo, yards_gained, epa, success,
         play_type, unit, sub_unit, penalty_type, offensive_play,
         pass_attempt, sack, scramble, dropback, designed_run, spike, kneel, penalty_play,
         kickoff, punt, field_goal, extra_point, timeout, play_types)
