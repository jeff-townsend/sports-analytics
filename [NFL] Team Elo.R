library(tidyverse)
library(nflreadr)

# set up the data
nfl.pbp <-
  load_pbp(seasons = c(2020:2022)) %>%
  filter(game_seconds_remaining == 3600)

pyth.exp <- 2.37
k.factor <- 64
hfa <- 20

# run season loop
s <- 2020
for(s in 2020:2022)
  {
    nfl.games <-
      nfl.pbp %>%
      distinct(game_id, week, season, home_team, away_team, home_score, away_score, spread_line, result, location) %>%
      mutate(home_win = ifelse(result > 0, 1, ifelse(result < 0, 0, 0.5)),
             away_win = 1 - home_win,
             home_pyth = home_score^pyth.exp / (home_score^pyth.exp + away_score^pyth.exp),
             is_neutral = ifelse(location == "Neutral", 1, 0))
    
    # initiate variables/tables for loop
    nfl.elo <-
      rbind(nfl.games %>% filter(week == 1) %>% select(home_team, week, season) %>% rename(team = home_team),
            nfl.games %>% filter(week == 1) %>% select(away_team, week, season) %>% rename(team = away_team)) %>%
      mutate(pre_elo = 1500,
             elo_delta = 0)
    nfl.results <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(nfl.results) <- c("game_id", "week", "season", "home_win",
                               "home_pre_elo", "away_pre_elo", "home_expectation",
                               "home_elo_delta", "away_elo_delta", "log_loss")
    
    
    # run weekly loop
    w <- 1
    for(w in 1:22)
      {
        nfl.pre.elo <-
          nfl.elo %>%
          filter(week == w)
        
        nfl.weekly.games <-
          nfl.games %>%
          inner_join(nfl.pre.elo, by = c("home_team" = "team", "week", "season")) %>%
          rename(home_pre_elo = pre_elo) %>%
          inner_join(nfl.pre.elo, by = c("away_team" = "team", "week", "season")) %>%
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
                      select(home_team, week, season, home_post_elo),
                    by = c("team" = "home_team", "week", "season")) %>%
          left_join(nfl.weekly.games %>%
                      select(away_team, week, season, away_post_elo),
                    by = c("team" = "away_team", "week", "season")) %>%
          mutate(post_elo = ifelse(!is.na(home_post_elo), home_post_elo,
                                   ifelse(!is.na(away_post_elo), away_post_elo,
                                          pre_elo)),
                 elo_delta = post_elo - pre_elo) %>%
          select(team, week, season, pre_elo, post_elo, elo_delta)
        
        nfl.elo <- rbind(nfl.elo,
                         nfl.post.elo %>%
                           select(team, week, season, post_elo, elo_delta) %>%
                           mutate(week = week + 1) %>%
                           rename(pre_elo = post_elo))
        nfl.results <- rbind(nfl.results,
                             nfl.weekly.games %>%
                               select(game_id, week, season, home_win, home_pre_elo, away_pre_elo,
                                      home_expectation, home_elo_delta, away_elo_delta, log_loss))
        
        w <- w + 1
    }
    s <- s + 1
}

nfl.final.elo <-
  nfl.elo %>%
  filter(week == 23) %>%
  select(team, season, pre_elo) %>%
  rename(final_elo = pre_elo) %>%
  inner_join(nfl.elo %>%
               filter(elo_delta != 0),
             by = c("team", "season")) %>%
  group_by(team, season, final_elo) %>%
  summarize(avg_elo_delta = mean(elo_delta)) %>%
  arrange(desc(final_elo))

# check for prediction accuracy
mean(nfl.results$log_loss)
# check for home-field advantage (should be 0 once controlled for)
mean(nfl.results$home_elo_delta)
# check for prior-year effects
yoy.elo <-
  nfl.final.elo %>%
  mutate(next_season = season + 1) %>%
  inner_join(nfl.final.elo,
             by = c("team", "next_season" = "season")) %>%
  rename(prior_season = season,
         prior_final_elo = final_elo.x,
         current_season = next_season,
         current_elo_delta = avg_elo_delta.y) %>%
  select(team, prior_season, prior_final_elo, current_season, current_elo_delta)

ggplot(yoy.elo, aes(x = prior_final_elo, y = current_elo_delta)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
cor(yoy.elo$prior_final_elo, yoy.elo$current_elo_delta)
