library(readr)
library(tidyverse)

##### Import Data

games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/games.csv")
players <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/players.csv")
birds <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/birds.csv")
player.games <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/player_games.csv")
bird.games <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/bird_games.csv")

##### Add Game Variables

games <-
  games.import %>%
  mutate(is_nectar = ifelse(deck %in% c("Oceania Expansion", "Asia Expansion"), 1, 0)) %>%
  inner_join(player.games %>%
               select(player_game_id, game_id, player_id, player_score,
                      bird_points, bonus_points, round_points, egg_points, food_points, tuck_points, nectar_points),
             by = "game_id") %>%
  inner_join(players %>% select(player_id, player_archetype),
             by = "player_id") %>%
  mutate(is_bot = ifelse(player_archetype == "Bot", 1, 0)) %>%
  group_by(game_id, date, time, deck, players, is_nectar) %>%
  summarize(is_bot_game = max(is_bot),
            is_solo_game = ifelse(max(player_id) == 1, 1,
                                  ifelse(min(player_id) == 9, 1, 0)),
            score = mean(ifelse(is_bot == 1, NA, player_score), na.rm = TRUE),
            bird_points = mean(ifelse(is_bot == 1, NA, bird_points), na.rm = TRUE),
            bonus_points = mean(ifelse(is_bot == 1, NA, bonus_points), na.rm = TRUE),
            round_points = mean(ifelse(is_bot == 1, NA, round_points), na.rm = TRUE),
            egg_points = mean(ifelse(is_bot == 1, NA, egg_points), na.rm = TRUE),
            food_points = mean(ifelse(is_bot == 1, NA, food_points), na.rm = TRUE),
            tuck_points = mean(ifelse(is_bot == 1, NA, tuck_points), na.rm = TRUE),
            nectar_points = mean(ifelse(is_bot == 1, NA, nectar_points), na.rm = TRUE)) %>%
  ungroup()

##### Game Types

game.types <-
  games %>%
  group_by(players, is_bot_game, is_nectar) %>%
  summarize(games = n(),
            gametype_average = mean(average_score))

##### Player Ratings

player.ratings <-
  players %>%
  filter(player_archetype %in% c("Andrew", "Brent", "David", "Jeff")) %>%
  inner_join(player.games %>%
               select(game_id, player_id, player_score),
             by = "player_id") %>%
  inner_join(games %>%
               filter(is_bot_game == 0,
                      is_solo_game == 0) %>%
               select(game_id, players, is_nectar),
             by = "game_id") %>%
  inner_join(game.types %>%
               filter(is_bot_game == 0),
             by = c("players", "is_nectar")) %>%
  mutate(baseline_score = (players * games * gametype_average - player_score) / (players * games - 1),
         relative_score = player_score - baseline_score) %>%
  group_by(player_name) %>%
  summarize(games = n(),
            player_average = mean(player_score),
            baseline_average = mean(baseline_score),
            player_rating = mean(relative_score)) %>%
  ungroup() %>%
  arrange(desc(player_rating))
