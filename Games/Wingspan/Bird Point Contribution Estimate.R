library(readr)
library(tidyverse)
library(ggthemes)

##### Import Data

games <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/games.csv")
players <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/players.csv")
birds <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/birds.csv")
player.games <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/player_games.csv")
bird.games <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/bird_games.csv")
turns.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/turns.csv")
actions.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Wingspan/actions.csv")

### Keep Score
# currently doesn't include bonus and end of round points
actions <-
  actions.import %>%
  ungroup() %>%
  left_join(birds %>%
              select(bird_name, bird_points),
            by = c("action_subtype" = "bird_name")) %>%
  mutate(bird_points = ifelse(action_type == "Play Bird", bird_points, 0),
         egg_points = ifelse(action_type == "Lay Egg", 1,
                             ifelse(action_type == "Discard Egg", -1, 0)),
         food_points = ifelse(action_type == "Cache Food", 1, 0),
         tuck_points = ifelse(action_type == "Tuck Card", 1, 0),
         points = bird_points + egg_points + food_points + tuck_points)

turns <-
  turns.import %>%
  inner_join(players %>%
               select(player_id, player_name),
             by = "player_id") %>%
  mutate(player_name = ifelse(player_name == "Blue Jeff", "Blue",
                              ifelse(player_name == "Red Jeff", "Red", player_name))) %>%
  inner_join(actions %>%
               select(turn_id, points),
             by = "turn_id") %>%
  group_by(turn_id, game_id, player_name, turn, turn_type) %>%
  summarize(points = sum(points)) %>%
  ungroup() %>%
  group_by(player_name) %>%
  mutate(score = cumsum(points)) %>%
  ungroup()

# plot cumulative score by turn
ggplot(turns, aes(x = turn, y = score, colour = player_name)) +
  geom_line() +
  scale_color_manual(values = c("Blue", "Red")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(8, 15, 21, 26))

### Bird Point Contribution Estimate (PCE)

## Bird Points:
  # Attribute all bird points to played bird
## Food Points:
  # Attribute all food points to activated bird
## Tuck Points:
  # Attribute all tuck points to activated bird
actions %>%
  filter(action_type %in% c("Play Bird", "Cache Food", "Tuck Card")) %>%
  rename(bird_name = action_subtype) %>%
  select(action_id, turn_id, player_id, bird_name, bird_points, food_points, tuck_points) %>%
  group_by(player_id, bird_name) %>%
  summarize(bird_points = sum(bird_points),
            food_points = sum(food_points),
            tuck_points = sum(tuck_points)) %>%
  ungroup() %>%
  mutate(pce = bird_points + food_points + tuck_points) %>%
  arrange(player_id, desc(pce))

## Egg Points:
  # Apply egg cost to played birds
  # Apply egg points to activated bird
  # Measure "baseline income" -- two eggs per Grasslands activation
  # Measure "added income" -- Grasslands egg income beyond the baseline of two eggs

egg.points <-
  rbind(actions %>%
        filter(action_type %in% c("Lay Egg", "Discard Egg")) %>%
        mutate(net_eggs = ifelse(action_type == "Lay Egg", 1, -1)) %>%
        select(action_id, turn_id, player_id, net_eggs, action_subtype) %>%
        rename(egg_action_id = action_id,
               bird_name = action_subtype) %>%
        inner_join(actions %>%
                     filter(action_type %in% c("Activate Habitat", "Activate Power", "Play Bird")) %>%
                     select(action_id, turn_id, action_type, action_subtype),
                   by = "turn_id",
                   relationship = "many-to-many") %>%
        filter(action_id < egg_action_id) %>%
        group_by(egg_action_id) %>%
        mutate(activated_action_id = max(action_id)) %>%
        ungroup() %>%
        filter(action_id == activated_action_id) %>%
        group_by(player_id, action_subtype) %>%
        summarize(egg_points = sum(net_eggs)) %>%
        ungroup() %>%
        inner_join(actions %>%
                     filter(action_type == "Activate Habitat",
                            action_subtype == "Grasslands") %>%
                     group_by(player_id) %>%
                     summarize(baseline_eggs = 2*n()) %>%
                     ungroup(),
                   by = "player_id") %>%
          mutate(egg_points = ifelse(action_subtype == "Grasslands", egg_points - baseline_eggs, egg_points),
                 action_subtype = ifelse(action_subtype == "Grasslands", "Grasslands Income", action_subtype)) %>%
          select(-baseline_eggs),
        actions %>%
          filter(action_type == "Activate Habitat",
                 action_subtype == "Grasslands") %>%
          mutate(action_subtype = "Grasslands Baseline") %>%
          group_by(player_id, action_subtype) %>%
          summarize(egg_points = 2*n()) %>%
          ungroup())
