library(tidyverse)
library(hockeyR)

pbp <- load_pbp(2024)
games <-
  get_game_ids(season = 2024) %>%
  filter(game_type == "REG")

rosters <- data.frame(game_id = integer(),
                      team_id = integer(),
                      player_id = integer(),
                      player_name = character(),
                      position = character(),
                      position_type = character())

g <- 1
for(g in 1:(nrow(games))){

  game.id <- as.numeric(games$game_id[g])
  game.roster <-
    cbind(game.id, get_game_rosters(game.id)) %>%
    rename(game_id = game.id)
  
  rosters <- rbind(rosters, game.roster)
  
  g <- g + 1
  
}

events <-
  pbp %>%
  arrange(game_id, event_idx) %>%
  mutate(event_id = 1:n()) %>%
  filter(event_type %in% c("GOAL", "SHOT", "BLOCKED_SHOT"),
         period < 5) %>%
  mutate(player1_id = ifelse(event_team_type == "home", home_on_1_id, away_on_1_id),
         player2_id = ifelse(event_team_type == "home", home_on_2_id, away_on_2_id),
         player3_id = ifelse(event_team_type == "home", home_on_3_id, away_on_3_id),
         player4_id = ifelse(event_team_type == "home", home_on_4_id, away_on_4_id),
         player5_id = ifelse(event_team_type == "home", home_on_5_id, away_on_5_id),
         player6_id = ifelse(event_team_type == "home", home_on_6_id, away_on_6_id),
         opponent1_id = ifelse(event_team_type == "away", home_on_1_id, away_on_1_id),
         opponent2_id = ifelse(event_team_type == "away", home_on_2_id, away_on_2_id),
         opponent3_id = ifelse(event_team_type == "away", home_on_3_id, away_on_3_id),
         opponent4_id = ifelse(event_team_type == "away", home_on_4_id, away_on_4_id),
         opponent5_id = ifelse(event_team_type == "away", home_on_5_id, away_on_5_id),
         opponent6_id = ifelse(event_team_type == "away", home_on_6_id, away_on_6_id)) %>%
  select(event_id, game_id, event_type, strength_state,
         shootingPlayerId, scoringPlayerId, assist1PlayerId, assist2PlayerId, blockingPlayerId,
         player1_id, player2_id, player3_id, player4_id, player5_id, player6_id,
         opponent1_id, opponent2_id, opponent3_id, opponent4_id, opponent5_id, opponent6_id) %>%
  rename(shooter_id = shootingPlayerId,
         scorer_id = scoringPlayerId,
         primary_assist_id = assist1PlayerId,
         secondary_assist_id = assist2PlayerId,
         blocker_id = blockingPlayerId) %>%
  mutate(shooter_id = ifelse(!is.na(scorer_id), scorer_id, shooter_id)) %>%
  mutate(is_goal = ifelse(event_type == "GOAL", 1, 0),
         is_sog = ifelse(event_type %in% c("GOAL", "SHOT"), 1, 0),
         is_plusminus = ifelse(strength_state %in% c("5v5", "4v5", "6v5", "4v4", "3v3",
                                                     "5v6", "4v6", "3v4", "3v5", "6v3") & is_goal == 1, 1, 0),
         is_ppp = ifelse(strength_state %in% c("5v4", "5v3", "4v3", "6v4", "6v3") & is_goal == 1, 1, 0))

goals <-
  rosters %>%
  inner_join(events, by = c("game_id", "player_id" = "shooter_id")) %>%
  group_by(player_id) %>%
  summarize(goals = sum(is_goal),
            sog = sum(is_sog),
            ppg = sum(is_ppp))

assists <-
  rbind(
    rosters %>%
      inner_join(events, by = c("game_id", "player_id" = "primary_assist_id")) %>%
      group_by(player_id) %>%
      summarize(assists = n(),
                ppa = sum(is_ppp)) %>%
      ungroup(),
    rosters %>%
      inner_join(events, by = c("game_id", "player_id" = "secondary_assist_id")) %>%
      group_by(player_id) %>%
      summarize(assists = n(),
                ppa = sum(is_ppp)) %>%
      ungroup()) %>%
  group_by(player_id) %>%
  summarize(assists = sum(assists),
            ppa = sum(ppa))

plus.minus <-
  rbind(
    rbind(events %>% filter(is_plusminus == 1) %>% select(game_id, player1_id) %>% rename(player_id = player1_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, player2_id) %>% rename(player_id = player2_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, player3_id) %>% rename(player_id = player3_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, player4_id) %>% rename(player_id = player4_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, player5_id) %>% rename(player_id = player5_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, player6_id) %>% rename(player_id = player6_id)) %>%
      mutate(plus_minus = 1),
    rbind(events %>% filter(is_plusminus == 1) %>% select(game_id, opponent1_id) %>% rename(player_id = opponent1_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, opponent2_id) %>% rename(player_id = opponent2_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, opponent3_id) %>% rename(player_id = opponent3_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, opponent4_id) %>% rename(player_id = opponent4_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, opponent5_id) %>% rename(player_id = opponent5_id),
          events %>% filter(is_plusminus == 1) %>% select(game_id, opponent6_id) %>% rename(player_id = opponent6_id)) %>%
      mutate(plus_minus = -1)) %>%
  inner_join(rosters, by = c("game_id", "player_id")) %>%
  group_by(player_id) %>%
  summarize(plus_minus = sum(plus_minus))

blocks <-
  rosters %>%
  inner_join(events, by = c("game_id", "player_id" = "blocker_id")) %>%
  group_by(player_id) %>%
  summarize(blocks = n())

players <-
  rosters %>%
  distinct(player_id, player_name, position) %>%
  left_join(goals, by = "player_id") %>%
  left_join(assists, by = "player_id") %>%
  left_join(plus.minus, by = "player_id") %>%
  left_join(blocks, by = "player_id")
players[is.na(players)] <- 0

fantasy <-
  players %>%
  mutate(ppp = ppg + ppa,
         fantasy_points = 6*goals + 4*assists + 2*plus_minus + 2*ppp + 0.9*sog + blocks) %>%
  select(player_id, player_name, fantasy_points, goals, assists, plus_minus, ppp, sog, blocks)
