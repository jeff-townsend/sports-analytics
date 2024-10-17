#### objective: build a model to predict whether a given play will be a run or a pass

## required libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

### assemble data

## pbp data
seasons <- 2011:2020 ## data pre-2011 has different player IDs, which makes it difficult to use
nfl.pbp.load <- purrr::map_dfr(seasons, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})

set.seed(921)
nfl.pbp <-
  nfl.pbp.load %>%
  mutate(quarter_minutes_remaining = round(quarter_seconds_remaining / 60, 0),
         rng = runif(dim(nfl.pbp.load)[1])) %>%
  filter(season_type == "REG",
         play_type %in% c("pass", "run"),
         !is.na(down)) %>%
  mutate(down = as.factor(down),
         qtr = as.factor(qtr))

train.pbp <-
  nfl.pbp %>%
  filter(rng <= .9)

test.pbp <-
  nfl.pbp %>%
  filter(rng > .9)

## dumb model: predict pass every play
pass.rate <- mean(train.pbp$pass)
test.pbp <-
  test.pbp %>%
  mutate(dumb_pred_pass = pass.rate) %>%
  mutate(dumb_acc_pred = pass - dumb_pred_pass)

mean(abs(test.pbp$dumb_acc_pred)) # 0.475

## let's start exploring some correlations

# based on down
ggplot(nfl.pbp %>% group_by(down) %>% summarize(pass_rate = mean(pass)),
       aes(x = down, y = pass_rate)) +
  geom_col() +
  scale_y_continuous(limits = c(0:1))

ggplot(nfl.pbp %>% group_by(ydstogo) %>% summarize(pass_rate = mean(pass)),
       aes(x = ydstogo, y = pass_rate)) +
  geom_point() +
  geom_hline(yintercept = pass.rate,
             linetype = "dashed")

ggplot(nfl.pbp %>% group_by(yardline_100) %>% summarize(pass_rate = mean(pass)),
       aes(x = yardline_100, y = pass_rate)) +
  geom_point() +
  scale_y_continuous(limits = c(0:1)) +
  scale_x_reverse() +
  geom_hline(yintercept = pass.rate,
             linetype = "dashed")

View(
  nfl.pbp %>%
    filter(abs(score_differential) == 0, qtr == 2) %>%
    mutate(time_remaining_bucket = if_else(quarter_minutes_remaining >= 5, 1,
                                           if_else(quarter_minutes_remaining >= 2, 2, 3))) %>%
    group_by(score_differential, time_remaining_bucket) %>%
    summarize(pass_rate = mean(pass),
              freq = n())
)

## now let's make a slightly less dumb model just based on down
mod.down <- glm(pass ~ down, data = train.pbp, family = binomial)
summary(mod.down)

pred.down <- data.frame(pass = test.pbp$pass,
                        down = test.pbp$down)

test.pbp$down_pred_pass <- predict.glm(mod.down, pred.down, type = "response")
test.pbp <-
  test.pbp %>%
  mutate(down_pred_pass = if_else(is.na(down_pred_pass), pass.rate, down_pred_pass),
         down_acc_pred = pass - down_pred_pass)

mean(abs(test.pbp$down_acc_pred)) # 0.446

# 3rd down has an 83% pass rate, but this is heavily influenced by distance to the first down
# pull 3rd down pass rate by distance
dd.data <-
  nfl.pbp %>%
  filter(ydstogo <= 25) %>%
  group_by(down, ydstogo) %>%
  summarize(pass_rate = mean(pass))

ggplot(dd.data, aes(x = ydstogo, y = pass_rate, col = down)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0:1)) +
  geom_hline(yintercept = pass.rate,
             linetype = "dashed")

## let's improve the model by adding down+distance
mod.dd <- glm(pass ~ down + ydstogo, data = train.pbp, family = binomial)
summary(mod.dd)

pred.dd <- data.frame(pass = test.pbp$pass,
                      down = test.pbp$down,
                      ydstogo = test.pbp$ydstogo)

test.pbp$dd_pred_pass <- predict.glm(mod.dd, pred.dd, type = "response")
test.pbp <-
  test.pbp %>%
  mutate(dd_pred_pass = if_else(is.na(dd_pred_pass), pass.rate, dd_pred_pass),
         dd_acc_pred = pass - dd_pred_pass)

mean(abs(test.pbp$dd_acc_pred)) # 0.429

View(
  test.pbp %>%
    select(play_id, down, ydstogo, pass, down_pred_pass, dd_pred_pass)
)
# this misses on some down+distance combinations because the model assumes
# distance has a linear relationship with pass rate, which it does not.

## make the down+distance better by adding an interaction effect(?)
mod.dd2 <- glm(pass ~ down * ydstogo, data = train.pbp, family = binomial)
summary(mod.dd2)

pred.dd2 <- data.frame(pass = test.pbp$pass,
                       down = test.pbp$down,
                       ydstogo = test.pbp$ydstogo)

test.pbp$dd2_pred_pass <- predict.glm(mod.dd2, pred.dd2, type = "response")
test.pbp <-
  test.pbp %>%
  mutate(dd2_pred_pass = if_else(is.na(dd2_pred_pass), pass.rate, dd2_pred_pass),
         dd2_acc_pred = pass - dd2_pred_pass)

mean(abs(test.pbp$dd2_acc_pred)) # 0.427

# this just assumes a unique linear relationship between distance and pass rate
# by down; it doesn't fix the core problem

## let's try decision trees
# base tree
train.pbp.dd <-
  train.pbp %>%
  select(pass, down, ydstogo)
tree.dd <- rpart(pass ~ ., data = train.pbp.dd, method = "class")
summary(tree.dd)

fancyRpartPlot(tree.dd, caption = NULL)

test.pbp$dd_tree_pred_pass <- predict(tree.dd, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(dd_tree_acc_pred = pass - dd_tree_pred_pass)

mean(abs(test.pbp$dd_tree_acc_pred)) # 0.425... not really any better (it's not smart enough)

# add splits
tree.dd2 <- rpart(pass ~ ., data = train.pbp.dd, method = "class", cp = -1)
tree.dd2
fancyRpartPlot(tree.dd2, caption = NULL)

test.pbp$dd2_tree_pred_pass <- predict(tree.dd2, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(dd2_tree_acc_pred = pass - dd2_tree_pred_pass)

mean(abs(test.pbp$dd2_tree_acc_pred)) # 0.414... we're improving

## let's add more variables
# add score differential; low complexity
train.pbp.dds <-
  train.pbp %>%
  select(pass, down, ydstogo, score_differential)
tree.dds <- rpart(pass ~ .,
                  data = train.pbp.dds,
                  method = "class")
tree.dds
fancyRpartPlot(tree.dd, caption = NULL)
fancyRpartPlot(tree.dds, caption = NULL)

test.pbp$dds_tree_pred_pass <- predict(tree.dds, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(dds_tree_acc_pred = pass - dds_tree_pred_pass)

mean(abs(test.pbp$dds_tree_acc_pred)) # 0.414

# score differential; remove cp
tree.dds2 <- rpart(pass ~ .,
                  data = train.pbp.dds,
                  method = "class",
                  cp = -1)
#fancyRpartPlot(tree.dds2, caption = NULL)

test.pbp$dds2_tree_pred_pass <- predict(tree.dds2, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(dds2_tree_acc_pred = pass - dds2_tree_pred_pass)

mean(abs(test.pbp$dds2_tree_acc_pred)) # 0.395

# add quarter; low complexity
train.pbp.ddsq <-
  train.pbp %>%
  select(pass, down, ydstogo, score_differential, qtr)
tree.ddsq <- rpart(pass ~ .,
                  data = train.pbp.ddsq,
                  method = "class")
fancyRpartPlot(tree.dds, caption = NULL)
fancyRpartPlot(tree.ddsq, caption = NULL)

test.pbp$ddsq_tree_pred_pass <- predict(tree.ddsq, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(ddsq_tree_acc_pred = pass - ddsq_tree_pred_pass)

mean(abs(test.pbp$ddsq_tree_acc_pred)) # 0.412

# quarter; remove cp
tree.ddsq2 <- rpart(pass ~ .,
                   data = train.pbp.ddsq,
                   method = "class",
                   cp = -1)

test.pbp$ddsq2_tree_pred_pass <- predict(tree.ddsq2, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(ddsq2_tree_acc_pred = pass - ddsq2_tree_pred_pass)

mean(abs(test.pbp$ddsq2_tree_acc_pred)) # 0.381

# quarter with time remaining; remove cp
train.pbp.ddsq3 <-
  train.pbp %>%
  select(pass, down, ydstogo, score_differential, qtr, quarter_minutes_remaining)
tree.ddsq3 <- rpart(pass ~ .,
                    data = train.pbp.ddsq3,
                    method = "class",
                    cp = -1)

test.pbp$ddsq3_tree_pred_pass <- predict(tree.ddsq3, newdata = test.pbp, type = "prob")[,2]
test.pbp <-
  test.pbp %>%
  mutate(ddsq3_tree_acc_pred = pass - ddsq3_tree_pred_pass)

mean(abs(test.pbp$ddsq3_tree_acc_pred)) # 0.363... the best accuracy but it's predicting 0% and 100% a lot

View(test.pbp %>% filter(ddsq2_tree_pred_pass == 1) %>% summarize(pass.rate = mean(pass), freq = n()))
# this is a symptom of overfitting

# let's move forward with this model for now

nfl.pbp$pred_pass = predict(tree.ddsq3, newdata = nfl.pbp, type = "prob")[,2]

pass.att.prob <-
  nfl.pbp %>%
  summarize(pass.att.prob = (sum(pass_attempt) - sum(sack)) / sum(pass))

nfl.pbp.2020 <-
  nfl.pbp %>%
  filter(substring(game_id, 1, 4) == "2020")

View(
  nfl.pbp.2020 %>%
    filter(pass_attempt == 1)
)

View(
nfl.pbp.2020 %>%
  group_by(posteam) %>%
  summarize(pass.plays = sum(pass),
            pass.attempts = sum(pass_attempt) - sum(sack),
            expass = sum(pred_pass),
            pass.rate = (sum(pass_attempt) - sum(sack)) / n(),
            plays = n()) %>%
  ungroup() %>%
  mutate(expat = expass * .9,
         expat.rate = expat / plays,
         diff.rate = pass.rate - expat.rate,
         att.rate = pass.attempts / pass.plays)
)

# apply to 2021 season
current <- 2021
nfl.pbp.current.load <- purrr::map_dfr(current, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})

nfl.pbp.current <-
  nfl.pbp.current.load %>%
  mutate(quarter_minutes_remaining = round(quarter_seconds_remaining / 60, 0)) %>%
  filter(season_type == "REG",
         play_type %in% c("pass", "run")) %>%
  mutate(down = as.factor(down),
         qtr = as.factor(qtr))

nfl.pbp.current$pred_pass = predict(tree.ddsq3, newdata = nfl.pbp.current, type = "prob")[,2]

nfl.pbp.current.expass <-
  nfl.pbp.current %>%
    group_by(posteam) %>%
    summarize(pass.plays = sum(pass),
              pass.attempts = sum(pass_attempt) - sum(sack),
              expass = sum(pred_pass),
              pass.rate = mean(pass),
              expass.rate = mean(pred_pass),
              plays = n()) %>%
    ungroup() %>%
    mutate(expat = expass * .9,
           expat.rate = expat / plays,
           diff.rate = pass.rate - expass.rate,
           att.rate = pass.attempts / pass.plays,
           tendency = if_else(diff.rate > 0, "pass heavy", "run heavy")) %>%
    arrange(diff.rate)

nfl.pbp.current.expass$posteam <- factor(nfl.pbp.current.expass$posteam,
                                         levels = nfl.pbp.current.expass$posteam)

ggplot(data = nfl.pbp.current.expass, aes(x = expass, y = pass.plays, label = posteam)) +
  geom_text() +
  xlab("Situation-Expected Pass Plays") +
  ylab("Pass Plays") +
  geom_abline(linetype = "dotted") +
  scale_y_continuous(limits = c(300,500)) +
  scale_x_continuous(limits = c(300,500))

## why are so many more teams above the dotted line than below
mean(nfl.pbp.current$pass)
mean(nfl.pbp.current$pred_pass)
nfl.pbp.current %>% summarize(plays = n() / 32)

nfl.pbp.current.expass %>%
  mutate(delta = pass.plays - expass) %>%
  summarize(avg.delta = mean(delta))
# there is an inherent bias in the model because of changing circumstances over the years
# it's nothing crazy but clearly we can see it in aggregate


ggplot(data = nfl.pbp.current.expass, aes(x=posteam, y=diff.rate, label=diff.rate)) + 
  geom_bar(stat='identity', aes(fill = tendency), width=.5)  +
  scale_fill_manual(name="Tendency", 
                    labels = c("Pass Heavy", "Run Heavy"), 
                    values = c("pass heavy"="#00ba38", "run heavy"="#f8766d")) +
  xlab("Team") +
  ylab("Pass Rate vs. Expected") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()

