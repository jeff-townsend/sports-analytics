library(tidyverse)
library(nflreadr)

# set up the data
starting.season <- 2012
nfl.pbp <-
  load_pbp(seasons = c(starting.season:2022)) %>%
  filter(game_seconds_remaining == 3600)

# initiate tables/variables for loop
nfl.elo <- data.frame(team = character(),
                      season = integer(),
                      week = integer(),
                      games = integer(),
                      pre_elo = double(),
                      post_elo = double(),
                      elo_delta = double(),
                      pre_pd = double(),
                      game_pd = double(),
                      post_pd = double())
nfl.results <- data.frame(game_id = character(),
                          week = integer(),
                          season = integer(),
                          home_win = double(),
                          home_pre_elo = double(),
                          away_pre_elo = double(),
                          home_expectation = double(),
                          home_elo_delta = double(),
                          away_elo_delta = double(),
                          spread_log_loss = double(),
                          elo_log_loss = double())

pyth.exp <- 3.25
k.factor <- 48
hfa <- 42
yoy.reg <- 0.55

# run season-level loop
s <- starting.season
for(s in starting.season:2022)
{
  nfl.games <-
    nfl.pbp %>%
    filter(season == s) %>%
    distinct(game_id, week, home_team, away_team, home_score, away_score, spread_line, total_line, result, location) %>%
    mutate(home_win = ifelse(result > 0, 1, ifelse(result < 0, 0, 0.5)),
           away_win = 1 - home_win,
           home_implied_score = total_line/2 + spread_line/2,
           away_implied_score = total_line/2 - spread_line/2,
           home_implied_pct = home_implied_score^pyth.exp / (home_implied_score^pyth.exp + away_implied_score^pyth.exp),
           away_implied_pct = 1 - home_implied_pct,
           home_pyth = home_score^pyth.exp / (home_score^pyth.exp + away_score^pyth.exp),
           is_neutral = ifelse(location == "Neutral", 1, 0))
  
  # initiate variables/tables for loop
  nfl.starting.elo <-
    rbind(nfl.games %>% filter(week == 1) %>% select(home_team, week) %>% rename(team = home_team),
          nfl.games %>% filter(week == 1) %>% select(away_team, week) %>% rename(team = away_team)) %>%
    left_join(nfl.elo %>%
                filter(week == 23,
                       season == s - 1) %>%
                select(team, season, games, pre_elo),
              by = "team") %>%
    mutate(season = s,
           games = 0,
           pre_elo = ifelse(is.na(pre_elo), 1500, (pre_elo-1500)*yoy.reg + 1500),
           post_elo = pre_elo,
           elo_delta = 0,
           pre_pd = 0,
           game_pd = 0,
           post_pd = 0)
  nfl.elo <- rbind(nfl.elo, nfl.starting.elo)
  
  
  # run week-level loop
  w <- 1
  for(w in 1:22)
  {
    nfl.pre.elo <-
      nfl.elo %>%
      filter(week == w,
             season == s)
    
    nfl.weekly.games <-
      nfl.games %>%
      inner_join(nfl.pre.elo, by = c("home_team" = "team", "week")) %>%
      rename(home_pre_elo = pre_elo) %>%
      inner_join(nfl.pre.elo, by = c("away_team" = "team", "week")) %>%
      rename(away_pre_elo = pre_elo) %>%
      mutate(home_expectation = 1 / (1 + 10 ^ ((away_pre_elo - (home_pre_elo + (1-is_neutral)*hfa))/400)),
             away_expectation = 1 / (1 + 10 ^ (((home_pre_elo + (1-is_neutral)*hfa) - away_pre_elo)/400))) %>%
      mutate(spread_log_loss = -(home_win*log(home_implied_pct) + away_win*log(away_implied_pct)),
             elo_log_loss = -(home_win*log(home_expectation) + away_win*log(away_expectation))) %>%
      mutate(home_elo_delta = k.factor * (home_win - home_expectation),
             away_elo_delta = k.factor * (away_win - away_expectation),
             home_post_elo = home_pre_elo + home_elo_delta,
             away_post_elo = away_pre_elo + away_elo_delta)
    
    nfl.post.elo <-
      nfl.pre.elo %>%
      left_join(nfl.weekly.games %>%
                  select(home_team, week, home_post_elo, result),
                by = c("team" = "home_team", "week")) %>%
      rename(home_result = result) %>%
      left_join(nfl.weekly.games %>%
                  select(away_team, week, away_post_elo, result),
                by = c("team" = "away_team", "week")) %>%
      rename(away_result = result) %>%
      mutate(season = s,
             games = ifelse(!is.na(home_result), games+1,
                            ifelse(!is.na(away_result), games+1,
                                   games)),
             post_elo = ifelse(!is.na(home_post_elo), home_post_elo,
                               ifelse(!is.na(away_post_elo), away_post_elo,
                                      pre_elo)),
             elo_delta = post_elo - pre_elo,
             game_pd = ifelse(!is.na(home_result), home_result,
                              ifelse(!is.na(away_result), -away_result,
                                     0)),
             post_pd = pre_pd + game_pd) %>%
      select(team, week, season, games, pre_elo, post_elo, elo_delta, pre_pd, game_pd, post_pd)
    
    # update post_elo and elo_delta from current week
    nfl.elo <-
      nfl.elo %>%
      left_join(nfl.post.elo, by = c("team", "week", "season")) %>%
      rename(games = games.x,
             pre_elo = pre_elo.x,
             post_elo = post_elo.x,
             elo_delta = elo_delta.x,
             pre_pd = pre_pd.x,
             game_pd = game_pd.x,
             post_pd = post_pd.x) %>%
      mutate(post_elo = ifelse(is.na(post_elo.y), post_elo, post_elo.y),
             elo_delta = post_elo - pre_elo,
             game_pd = ifelse(is.na(game_pd.y), game_pd, game_pd.y),
             post_pd = pre_pd + game_pd) %>%
      select(team, week, season, games, pre_elo, post_elo, elo_delta, pre_pd, game_pd, post_pd)
    
    # insert pre_elo for the following week
    nfl.elo <- rbind(nfl.elo,
                     nfl.post.elo %>%
                       mutate(week = week + 1,
                              pre_elo = post_elo,
                              pre_pd = post_pd) %>%
                       mutate(post_elo = pre_elo, # initiate post_elo as pre_elo, which we'll update the subsequent week
                              post_pd = pre_pd,
                              elo_delta = 0, # initiate elo_delta as 0, which we'll update the subsequent week
                              game_pd = 0))
    
    nfl.results <- rbind(nfl.results,
                         nfl.weekly.games %>%
                           mutate(season = s) %>%
                           select(game_id, week, season, home_win, home_pre_elo, away_pre_elo,
                                  home_expectation, home_elo_delta, away_elo_delta, spread_log_loss, elo_log_loss))
    
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
mean(nfl.results$spread_log_loss)
nfl.results %>%
  filter(season != starting.season) %>%
  summarize(log_loss = mean(elo_log_loss))
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
