library(readr)
library(tidyverse)

##### Import Data

games.import <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/games.csv") %>%
  rename(game_id = id)
players <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/players.csv") %>%
  rename(player_id = id)
birds <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/birds.csv") %>%
  rename(bird_id = id)

player.games <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/player_games.csv") %>%
  rename(player_game_id = id)
bird.games <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/bird_games.csv") %>%
  rename(bird_game_id = id)

##### Add Game Variables

games <-
  games %>%
  inner_join(player.games %>% select(player_game_id, game_id, player_id, player_score),
             by = "game_id") %>%
  inner_join(players %>% select(player_id, player_archetype),
             by = "player_id") %>%
  mutate(is_bot = ifelse(player_archetype == "Bot", 1, 0)) %>%
  group_by(game_id, date, time, deck, players) %>%
  summarize(is_bot_game = max(is_bot)) %>%
  ungroup()
  

##### Game Types

game.types <-
  games %>%
  select(game_id, players, is_bot_game) %>%
  inner_join(player.games %>% select(game_id, player_id, player_score),
             by = "game_id") %>%
  inner_join(players %>% select(player_id, player_archetype),
             by = "player_id") %>%
  filter(player_archetype != "Bot") %>%
  group_by(game_id, players, is_bot_game) %>%
  summarize(player_games = n(),
            total_score = sum(player_score),
            game_average = mean(player_score)) %>%
  ungroup() %>%
  group_by(players, is_bot_game) %>%
  summarize(games = n(),
            gametype_average = sum(total_score) / sum(player_games))

##### Player Ratings

global.average <- with(game.types %>% filter(is_bot_game == 0), mean(gametype_average))
player.ratings <-
  players %>%
  filter(player_archetype %in% c("Andrew", "Brent", "David", "Jeff")) %>%
  inner_join(player.games %>% select(game_id, player_id, player_score),
             by = "player_id") %>%
  inner_join(games %>%
               filter(is_bot_game == 0) %>%
               select(game_id, players),
             by = "game_id") %>%
  inner_join(game.types %>% filter(is_bot_game == 0),
             by = "players") %>%
  mutate(baseline_score = (players * games * gametype_average - player_score) / (players * games - 1),
         relative_score = player_score - baseline_score) %>%
  group_by(player_archetype) %>%
  summarize(games = n(),
            player_average = mean(player_score),
            baseline_average = mean(baseline_score),
            relative_average = mean(relative_score),
            player_rating = global.average + relative_average) %>%
  arrange(desc(relative_average))

##### Bird Plus/Minus

bird.game.pm <-
  bird.games %>%
  inner_join(player.games %>%
               mutate(birds = forest_birds + grasslands_birds + wetlands_birds) %>%
               select(player_game_id, player_score, birds),
             by = "player_game_id") %>%
  inner_join(games %>% select(game_id, players, is_bot_game),
           by = "game_id") %>%
  inner_join(game.types,
             by = c("players", "is_bot_game")) %>%
  mutate(baseline_score = (players * games * gametype_average - player_score) / (players * games - 1),
         game_pm = player_score - baseline_score) %>%
         #game_pm = relative_score / birds) %>%
  select(bird_game_id, game_id, player_game_id, bird_name, habitat, birds, player_score, baseline_score, game_pm)

bird.pm <-
  bird.game.pm %>%
  group_by(bird_name) %>%
  summarize(games = n(),
            total_bird_pm = sum(game_pm),
            bird_pm = mean(game_pm))

bird.game.apm <-
  bird.game.pm %>%
  inner_join(bird.pm %>% select(bird_name, games, total_bird_pm),
             by = "bird_name") %>%
  mutate(oos_bird_pm = ifelse(games == 1, 0, (total_bird_pm - game_pm) / (games - 1))) %>%
  group_by(player_game_id) %>%
  mutate(expected_score = (sum(oos_bird_pm) - oos_bird_pm) / (birds - 1) + baseline_score,
         game_apm = player_score - expected_score) %>%
  select(-c(games, total_bird_pm, oos_bird_pm))

bird.apm <-
  bird.game.apm %>%
  group_by(bird_name) %>%
  summarize(games = n(),
            total_bird_pm = sum(game_pm),
            bird_pm = mean(game_pm),
            total_bird_apm = sum(game_apm),
            bird_apm = mean(game_apm))

##### Attribute Effects

habitat.pm <-
  bird.apm %>%
  inner_join(birds %>% select(bird_id, bird_name, forest, grasslands, wetlands),
             by = "bird_name") %>%
  mutate(habitats = forest + grasslands + wetlands) %>%
  group_by(habitats, forest, grasslands, wetlands) %>%
  summarize(played = sum(games),
            total_apm = sum(total_bird_apm)) %>%
  ungroup() %>%
  mutate(habitat_apm = total_apm / played)

food.pm <-
  bird.apm %>%
  inner_join(birds %>% select(bird_id, bird_name, food_cost),
             by = "bird_name") %>%
  group_by(food_cost) %>%
  summarize(played = sum(games),
            total_apm = sum(total_bird_apm)) %>%
  ungroup() %>%
  mutate(food_apm = total_apm / played)

bird.apm %>%
  group_by(games) %>%
  summarize(avg_apm = mean(bird_apm)) %>%
  arrange(desc(games))
