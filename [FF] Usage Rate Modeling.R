library(tidyverse)
library(nflreadr)

#library(caret)
library(rpart)
library(rpart.plot)
#library(rattle)
#library(randomForest)

# fantasy-relevant plays

pbp <-
  load_pbp(2016:2023) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass"),
         two_point_attempt == 0) %>%
  select(game_id, season, week, play_id, posteam, defteam, desc, play_type,
         down, ydstogo, yardline_100, goal_to_go, qtr, score_differential,
         passer_id, rusher_id, receiver_id) %>%
  mutate(down = as.factor(down)) %>%
  rename(distance = ydstogo)
pbp$id <- c(1:(nrow(pbp)))

# fantasy-relevant players

rosters <-
  load_rosters(2016:2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id)) %>%
  rename(player_id = gsis_id,
         player = full_name) %>%
  select(player_id, season, player, position)

# usage indicators

plays <-
  pbp %>%
  left_join(rosters %>% select(player_id, season, position), by = c("season", "passer_id" = "player_id")) %>%
  rename(passer_position = position) %>%
  left_join(rosters %>% select(player_id, season, position), by = c("season", "rusher_id" = "player_id")) %>%
  rename(rusher_position = position) %>%
  left_join(rosters %>% select(player_id, season, position), by = c("season", "receiver_id" = "player_id")) %>%
  rename(receiver_position = position) %>%
  mutate(qb_opportunity = ifelse(passer_position == "QB" | rusher_position == "QB" | receiver_position == "QB", 1, 0),
         rb_opportunity = ifelse(passer_position == "RB" | rusher_position == "RB" | receiver_position == "RB", 1, 0),
         wr_opportunity = ifelse(passer_position == "WR" | rusher_position == "WR" | receiver_position == "WR", 1, 0),
         te_opportunity = ifelse(passer_position == "TE" | rusher_position == "TE" | receiver_position == "TE", 1, 0)) %>%
  mutate(qb_opportunity = ifelse(!is.na(qb_opportunity), qb_opportunity, 0),
         rb_opportunity = ifelse(!is.na(rb_opportunity), rb_opportunity, 0),
         wr_opportunity = ifelse(!is.na(wr_opportunity), wr_opportunity, 0),
         te_opportunity = ifelse(!is.na(te_opportunity), te_opportunity, 0)) %>%
  mutate(rng = runif(nrow(pbp))) %>%
  select(-passer_position, -rusher_position, -receiver_position)

train.plays <-
  plays %>%
  filter(rng <= .9) %>%
  select(-rng)

test.plays <-
  plays %>%
  filter(rng > .9) %>%
  select(-rng)

### build decision trees

## Down & Distance
tree.dd.data <-
  train.plays %>%
  select(rb_opportunity, down, distance)

tree.dd.cp1 <- rpart(rb_opportunity ~ .,
                     data = tree.dd.data,
                     method = "class",
                     cp = .01) # default cp
rpart.plot(tree.dd.cp1)

tree.dd.cp2 <- rpart(rb_opportunity ~ .,
                     data = tree.dd.data,
                     method = "class",
                     cp = .007)
rpart.plot(tree.dd.cp2)

tree.dd.cp3 <- rpart(rb_opportunity ~ .,
                     data = tree.dd.data,
                     method = "class",
                     cp = .001)
rpart.plot(tree.dd.cp3)

tree.dd.cp4 <- rpart(rb_opportunity ~ .,
                     data = tree.dd.data,
                     method = "class",
                     cp = -1)
rpart.plot(tree.dd.cp4)

## Down & Distance, Score Differential
tree.sd.data <-
  train.plays %>%
  select(rb_opportunity, down, distance, score_differential)

tree.sd.cp1 <- rpart(rb_opportunity ~ .,
                     data = tree.sd.data,
                     method = "class",
                     cp = .01) # default cp
rpart.plot(tree.sd.cp1)

tree.sd.cp2 <- rpart(rb_opportunity ~ .,
                     data = tree.sd.data,
                     method = "class",
                     cp = -1)
rpart.plot(tree.sd.cp2)

# evaluate accuracy

tree.mod <- tree.sd.cp2
rpart.plot(tree.mod)

test.predictions <-
  test.plays %>%
  mutate(rb_prediction = as.numeric(predict(tree.mod, newdata = test.plays, type = "class")) - 1,
         rb_probability = predict(tree.mod, newdata = test.plays, type = "prob")[,2], # [,2] = probability of "1"
         rb_probability = ifelse(rb_probability == 1,
                                 .99,
                                 ifelse(rb_probability == 0,
                                        .01,
                                        rb_probability)),
         rb_logloss = rb_opportunity * log(rb_probability) + (1 - rb_opportunity) * log(1 - rb_probability))

test.predictions %>%
  summarize(rb_usage_rate = mean(rb_opportunity),
            predicted_rb_usage_rate = mean(rb_prediction),
            rb_usage_probability = mean(rb_probability),
            rb_logloss = -mean(rb_logloss))

## Down & Distance, CP Default -- 0.659
## Down & Distance, CP Level 2 (.007) -- 0.651
## Down & Distance, CP Level 3 (.001) -- 0.651; no improvement
## Down & Distance, CP Unrestricted -- 0.646

## Down & Distance + Score Differential, CP Default -- 0.661
## Down & Distance + Score Differential, CP Unrestricted -- 0.644
