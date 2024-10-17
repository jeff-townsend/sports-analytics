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
  mutate(down = as.factor(down),
         qtr = as.factor(qtr)) %>%
  rename(distance = ydstogo)
pbp$id <- c(1:(nrow(pbp)))

# fantasy-relevant players

rosters <-
  load_rosters(2016:2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         depth_chart_position != "FB",
         !is.na(gsis_id)) %>%
  rename(player_id = gsis_id,
         player = full_name) %>%
  select(player_id, season, player, position)

View(rosters %>% filter(position == "RB"))

# usage indicators

set.seed(921)
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
                     method = "class")
rpart.plot(tree.dd.cp1)

tree.dd.cp2 <- rpart(rb_opportunity ~ .,
                     data = tree.dd.data,
                     method = "class",
                     cp = .001)
rpart.plot(tree.dd.cp2)

tree.dd.cp3 <- rpart(rb_opportunity ~ .,
                     data = tree.dd.data,
                     method = "class",
                     cp = -1,
                     minbucket = 500)
rpart.plot(tree.dd.cp3)

## Down & Distance, Score Differential
tree.sd.data <-
  train.plays %>%
  select(rb_opportunity, down, distance, score_differential)

tree.sd.cp1 <- rpart(rb_opportunity ~ .,
                     data = tree.sd.data,
                     method = "class")
rpart.plot(tree.sd.cp1)

tree.sd.cp2 <- rpart(rb_opportunity ~ .,
                     data = tree.sd.data,
                     method = "class",
                     cp = .001) # default cp
rpart.plot(tree.sd.cp2)

tree.sd.cp3 <- rpart(rb_opportunity ~ .,
                     data = tree.sd.data,
                     method = "class",
                     cp = -1,
                     minbucket = 500)
rpart.plot(tree.sd.cp3)

## Down & Distance, Score Differential, Quarter
tree.qtr.data <-
  train.plays %>%
  select(rb_opportunity, down, distance, score_differential, qtr)

tree.qtr.cp1 <- rpart(rb_opportunity ~ .,
                      data = tree.qtr.data,
                      method = "class")
rpart.plot(tree.qtr.cp1)

tree.qtr.cp2 <- rpart(rb_opportunity ~ .,
                      data = tree.qtr.data,
                      method = "class",
                      cp = .001)
rpart.plot(tree.qtr.cp2)

tree.qtr.cp3 <- rpart(rb_opportunity ~ .,
                      data = tree.qtr.data,
                      method = "class",
                      cp = -1,
                      minbucket = 500)
rpart.plot(tree.qtr.cp3)

## Down & Distance, Score Differential, Quarter, Yardline
tree.yd.data <-
  train.plays %>%
  select(rb_opportunity, down, distance, score_differential, qtr, yardline_100)

tree.yd.cp1 <- rpart(rb_opportunity ~ .,
                     data = tree.yd.data,
                     method = "class")
rpart.plot(tree.yd.cp1)

tree.yd.cp2 <- rpart(rb_opportunity ~ .,
                     data = tree.yd.data,
                     method = "class",
                     cp = .001) # default cp
rpart.plot(tree.yd.cp2)

tree.yd.cp3 <- rpart(rb_opportunity ~ .,
                     data = tree.yd.data,
                     method = "class",
                     cp = .00001) # default cp
rpart.plot(tree.yd.cp3)

tree.yd.cp4 <- rpart(rb_opportunity ~ .,
                     data = tree.yd.data,
                     method = "class",
                     cp = -1,
                     minbucket = 500)
rpart.plot(tree.yd.cp4)

# evaluate accuracy

tree.mod <- tree.yd.cp4
#rpart.plot(tree.mod)

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

## Down & Distance, CP Default -- 0.657
## Down & Distance, CP .001 -- 0.648
## Down & Distnace, CP .00001 -- 0.645
## Down & Distance, MinBucket 500 -- 0.645

## Down & Distance + Score Differential, CP Default -- 0.659
## Down & Distance + Score Differential, CP .001 -- 0.642
## Down & Distance + Score Differential, CP .00001 -- 0.638
## Down & Distance + Score Differential, MinBucket 500 -- 0.636

## Down & Distance + Score Differential + Quarter, CP Default -- 0.659
## Down & Distance + Score Differential + Quarter, CP .001 -- 0.637
## Down & Distance + Score Differential + Quarter, CP .00001 -- 0.633
## Down & Distance + Score Differential + Quarter, MinBucket 500 -- 0.629

## Down & Distance + Score Differential + Quarter + Yardline, CP Default -- 0.659
## Down & Distance + Score Differential + Quarter + Yardline, CP .001 -- 0.637
## Down & Distance + Score Differential + Quarter + Yardline, CP .00001 -- 0.653
## Down & Distance + Score Differential + Quarter + Yardline, MinBucket 500 -- 0.629


##### Apply model results

participation <-
  load_participation(2016:2023) %>%
  rename(game_id = nflverse_game_id,
         team = possession_team) %>%
  select(game_id, play_id, team, offense_players) %>%
  inner_join(pbp, by = c("game_id", "play_id"))

players <-
  participation %>%
  separate_rows(offense_players, sep = ";") %>%
  rename(player_id = offense_players) %>%
  select(id, season, week, game_id, play_id, player_id, team,
         down, distance, score_differential, qtr, yardline_100,
         passer_id, rusher_id, receiver_id) %>%
  inner_join(rosters %>% select(player_id, season, player, position), by = c("season", "player_id")) %>%
  mutate(opportunity = ifelse(player_id == passer_id | player_id == rusher_id | player_id == receiver_id, 1, 0)) %>%
  mutate(opportunity = ifelse(is.na(opportunity), 0, opportunity))
players$xrb_opportunity = predict(tree.mod, newdata = players, type = "prob")[,2]

team.games <-
  participation %>%
  group_by(game_id, season, team) %>%
  summarize(team_snaps = n())

player.games <-
  players %>%
  filter(position == "RB") %>%
  group_by(player_id, game_id, season, week, player, position, team) %>%
  summarize(snaps = n(),
            opportunities = sum(opportunity),
            xopportunities = sum(xrb_opportunity)) %>%
  ungroup() %>%
  inner_join(team.games, by = c("season", "game_id", "team")) %>%
  mutate(snap_rate = snaps / team_snaps,
         usage_rate = opportunities / snaps,
         xusage_rate = xopportunities / snaps,
         usage_over_expected = usage_rate - xusage_rate,
         opportunities_over_expected = opportunities - xopportunities)

player.seasons <-
  player.games %>%
  group_by(player_id, season, player, position, team) %>%
  summarize(gp = n(),
            snaps = sum(snaps) / n(),
            opportunities = sum(opportunities) / n(),
            xopportunities = sum(xopportunities) / n(),
            team_snaps = sum(team_snaps) / n()) %>%
  ungroup() %>%
  mutate(snap_rate = snaps / team_snaps,
         usage_rate = opportunities / snaps,
         xusage_rate = xopportunities / snaps,
         usage_over_expected = usage_rate - xusage_rate,
         opportunities_over_expected = opportunities - xopportunities)

View(player.seasons %>% filter(season == 2023, gp >= 10))
