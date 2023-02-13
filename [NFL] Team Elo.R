library(tidyverse)
library(nflreadr)

# set up the data
nfl.pbp <-
  load_pbp() %>%
  filter(game_seconds_remaining == 3600)

pyth.exp <- 2.37
nfl.games <-
  nfl.pbp %>%
  distinct(game_id, week, home_team, away_team, home_score, away_score, spread_line, result, location) %>%
  mutate(home_win = ifelse(result > 0, 1, ifelse(result < 0, 0, 0.5)),
         away_win = 1 - home_win,
         home_pyth = home_score^pyth.exp / (home_score^pyth.exp + away_score^pyth.exp),
         is_neutral = ifelse(location == "Neutral", 1, 0))

# initiate variables/tables for loop
nfl.elo <-
  rbind(nfl.games %>% filter(week == 1) %>% select(home_team, week) %>% rename(team = home_team),
        nfl.games %>% filter(week == 1) %>% select(away_team, week) %>% rename(team = away_team)) %>%
  mutate(pre_elo = 1500,
         elo_delta = 0)
nfl.results <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(nfl.results) <- c("game_id", "week", "home_win",
                           "home_pre_elo", "away_pre_elo", "home_expectation",
                           "home_elo_delta", "away_elo_delta", "log_loss")
k.factor <- 64
hfa <- 48

# run loop
w <- 1
for(w in 1:22){

  nfl.pre.elo <-
    nfl.elo %>%
    filter(week == w)
  
  nfl.weekly.games <-
    nfl.games %>%
    inner_join(nfl.pre.elo, by = c("home_team" = "team", "week")) %>%
    rename(home_pre_elo = pre_elo) %>%
    inner_join(nfl.pre.elo, by = c("away_team" = "team", "week")) %>%
    rename(away_pre_elo = pre_elo) %>%
    mutate(home_expectation = 1 / (1 + 10 ^ ((away_pre_elo - (home_pre_elo + (1-is_neutral)*hfa))/400)),
           away_expectation = 1 / (1 + 10 ^ (((home_pre_elo + (1-is_neutral)*hfa) - away_pre_elo)/400))) %>%
    mutate(log_loss = -(home_win*log(home_expectation) + away_win*log(away_expectation))) %>%
    mutate(home_elo_delta = k.factor * (home_win - home_expectation),
           away_elo_delta = k.factor * (away_win - away_expectation),
           home_post_elo = home_pre_elo + home_elo_delta,
           away_post_elo = away_pre_elo + away_elo_delta)
  
  nfl.post.elo <-
    nfl.pre.elo %>%
    left_join(nfl.weekly.games %>%
                select(home_team, week, home_post_elo),
              by = c("team" = "home_team", "week")) %>%
    left_join(nfl.weekly.games %>%
                select(away_team, week, away_post_elo),
              by = c("team" = "away_team", "week")) %>%
    mutate(post_elo = ifelse(!is.na(home_post_elo), home_post_elo,
                             ifelse(!is.na(away_post_elo), away_post_elo,
                                    pre_elo)),
           elo_delta = post_elo - pre_elo) %>%
    select(team, week, pre_elo, post_elo, elo_delta)
  
  nfl.elo <- rbind(nfl.elo,
                   nfl.post.elo %>%
                     select(team, week, post_elo, elo_delta) %>%
                     mutate(week = week + 1) %>%
                     rename(pre_elo = post_elo))
  nfl.results <- rbind(nfl.results,
                       nfl.weekly.games %>%
                         select(game_id, week, home_win, home_pre_elo, away_pre_elo,
                                home_expectation, home_elo_delta, away_elo_delta, log_loss))
  
  w <- w + 1
}

nfl.final.elo <-
  nfl.elo %>%
  filter(week == 23) %>%
  select(team, pre_elo) %>%
  rename(final_elo = pre_elo) %>%
  arrange(desc(final_elo))

mean(nfl.results$log_loss)
mean(nfl.results$home_elo_delta)
nfl.elo %>%
  filter(elo_delta != 0) %>%
  summarize(avg_delta = mean(abs(elo_delta)))
