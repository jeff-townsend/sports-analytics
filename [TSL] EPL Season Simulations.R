library(tidyverse)
library(ggthemes)
library(readr)

games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_schedule_2425.csv",
                         col_types = cols(date = col_date(format = "%m/%d/%y")))

teams.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_elo_2425.csv")

teams <-
  games.import %>%
  distinct(home) %>%
  rename(fbref_team = home) %>%
  arrange(fbref_team)

teams$tsl_team <- teams.import$team
teams$elo <- teams.import$elo

games <-
  games.import %>%
  inner_join(teams %>%
               select(fbref_team, elo) %>%
               rename(team = fbref_team),
             by = c("home" = "team")) %>%
  rename(home_elo = elo) %>%
  inner_join(teams %>%
               select(fbref_team, elo) %>%
               rename(team = fbref_team),
             by = c("away" = "team")) %>%
  rename(away_elo = elo) %>%
  mutate(elo_diff = home_elo - away_elo) %>%
  mutate(home_win_prob = 1 / (10^(-elo_diff/400) + 1) * .78,
         draw_prob = .22,
         away_win_prob = .78 - home_win_prob)

set.seed(816)
simulations <- 10000
seasons <- data.frame(season_id = c(1:simulations))

season.games <- data.frame(season_game_id = c(1:(nrow(games)*simulations)),
                           season_id = rep(seasons$season_id, each = nrow(games)),
                           game_id = games$game_id,
                           week = games$week,
                           home = games$home,
                           away = games$away,
                           home_win_prob = games$home_win_prob,
                           draw_prob = games$draw_prob,
                           away_win_prob = games$away_win_prob)
  
game.simulations <-
  season.games %>%
  mutate(rng = runif(nrow(season.games)),
         home_win = ifelse(rng <= home_win_prob, 1, 0),
         away_win = ifelse(rng >= (1 - away_win_prob), 1, 0),
         draw = 1 - home_win - away_win,
         home_points = 3*home_win + draw,
         away_points = 3*away_win + draw)

home.performance <-
  game.simulations %>%
  group_by(home, season_id) %>%
  summarize(matches = n(),
            wins = sum(home_win),
            draws = sum(draw),
            losses = sum(away_win),
            points = sum(home_points)) %>%
  ungroup() %>%
  rename(team = home)

away.performance <-
  game.simulations %>%
  group_by(away, season_id) %>%
  summarize(matches = n(),
            wins = sum(away_win),
            draws = sum(draw),
            losses = sum(home_win),
            points = sum(away_points)) %>%
  ungroup() %>%
  rename(team = away)

tables <-
  rbind(home.performance, away.performance) %>%
  group_by(team, season_id) %>%
  summarize(matches = sum(matches),
            wins = sum(wins),
            draws = sum(draws),
            losses = sum(losses),
            points = sum(points)) %>%
  ungroup() %>%
  group_by(season_id) %>%
  mutate(placement = rank(desc(points), ties.method = "random"),
         top4 = ifelse(placement <= 4, 1, 0))

summary <-
  tables %>%
  group_by(team) %>%
  summarize(wins = mean(wins),
            draws = mean(draws),
            losses = mean(losses),
            points = mean(points),
            top4 = mean(top4))

sum(teams$elo)
