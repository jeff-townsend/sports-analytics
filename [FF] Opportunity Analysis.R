library(tidyverse)
library(nflreadr)

nfl.pbp <- load_pbp(2021) %>% filter(season_type == "REG",
                                     play_type %in% c("run", "pass")) # data goes back to 1999
nfl.roster <- load_rosters(2021)
# nfl.roster <-
#   nfl.roster %>%
#   mutate(full_name = ifelse(season >= 2016, full_name, paste(first_name, last_name)))

dropbacks <-
  nfl.pbp %>%
  filter(qb_dropback == 1) %>%
  mutate(player_id = ifelse(is.na(passer_player_id), rusher_player_id, passer_player_id)) %>%
  select(play_id, game_id, player_id,
         pass_attempt, sack, passing_yards, pass_touchdown, interception,
         qb_scramble, rushing_yards, rush_touchdown, fumble,
         two_point_conv_result) %>%
  rename(two_point_conversion = two_point_conv_result) %>%
  mutate(two_point_conversion = ifelse(two_point_conversion == "success", 1, 0)) %>%
  mutate(passing_yards = ifelse(is.na(passing_yards), 0, passing_yards),
         rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards),
         two_point_conversion = ifelse(is.na(two_point_conversion), 0, two_point_conversion)) %>%
  mutate(fantasy_points = 0.04*passing_yards + 4*pass_touchdown - 2*interception +
           0.1*rushing_yards + 6*rush_touchdown - fumble + 2*two_point_conversion)

dropback.fp <-
  dropbacks %>%
  inner_join(nfl.roster, by = c("player_id" = "gsis_id")) %>%
  group_by(player_id, full_name) %>%
  summarize(dropbacks = n(),
            fantasy_points = sum(fantasy_points))

carries <-
  nfl.pbp %>%
  filter(qb_dropback == 0,
         rush_attempt == 1) %>%
  rename(player_id = rusher_player_id) %>%
  select(play_id, game_id, player_id,
         rushing_yards, rush_touchdown, fumble,
         two_point_conv_result) %>%
  rename(two_point_conversion = two_point_conv_result) %>%
  mutate(two_point_conversion = ifelse(two_point_conversion == "success", 1, 0)) %>%
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards),
         two_point_conversion = ifelse(is.na(two_point_conversion), 0, two_point_conversion)) %>%
  mutate(fantasy_points = 0.1*rushing_yards + 6*rush_touchdown - fumble + 2*two_point_conversion)

carry.fp <-
  carries %>%
  inner_join(nfl.roster, by = c("player_id" = "gsis_id")) %>%
  group_by(player_id, full_name) %>%
  summarize(carries = n(),
            fantasy_points = sum(fantasy_points))

targets <-
  nfl.pbp %>%
  filter(pass_attempt == 1,
         sack == 0) %>%
  rename(player_id = receiver_player_id,
         reception = complete_pass,
         receiving_touchdown = pass_touchdown) %>%
  select(play_id, game_id, player_id,
         reception, receiving_yards, receiving_touchdown, fumble,
         two_point_conv_result) %>%
  rename(two_point_conversion = two_point_conv_result) %>%
  mutate(two_point_conversion = ifelse(two_point_conversion == "success", 1, 0)) %>%
  mutate(receiving_yards = ifelse(is.na(receiving_yards), 0, receiving_yards),
         two_point_conversion = ifelse(is.na(two_point_conversion), 0, two_point_conversion)) %>%
  mutate(fantasy_points = 0.5*reception + 0.1*receiving_yards + 6*receiving_touchdown - fumble + 2*two_point_conversion)

target.fp <-
  targets %>%
  inner_join(nfl.roster, by = c("player_id" = "gsis_id")) %>%
  group_by(player_id, full_name) %>%
  summarize(targets = n(),
            fantasy_points = sum(fantasy_points))

dropback.opps <- dropbacks %>% select(player_id, fantasy_points) %>% mutate(opp_type = "dropback")
carry.opps <- carries %>% select(player_id, fantasy_points) %>% mutate(opp_type = "carry")
target.opps <- targets %>% select(player_id, fantasy_points) %>% mutate(opp_type = "target")

opportunities <-
  rbind(dropback.opps, carry.opps, target.opps) %>%
  inner_join(nfl.roster, by = c("player_id" = "gsis_id")) %>%
  select(player_id, full_name, position, fantasy_points) %>%
  arrange(player_id, desc(fantasy_points)) %>%
  group_by(player_id, full_name, position) %>%
  mutate(fp_rank = rank(-fantasy_points, ties.method = "first"),
         fp_rank_desc = rank(fantasy_points, ties.method = "last"),
         total_opps = fp_rank + fp_rank_desc - 1,
         pct_95 = total_opps * 0.05,
         top_opp = ifelse(fp_rank <= pct_95, 1, 0))

top.opps <-
  opportunities %>%
  group_by(player_id, full_name, position) %>%
  summarize(opps = n(),
            total_fps = sum(fantasy_points),
            top_5_fps = sum(top_opp * fantasy_points)) %>%
  ungroup() %>%
  mutate(top_5_weight = top_5_fps / total_fps)

View(top.opps %>% filter(position == "WR", opps >= 50))
