library(tidyverse)
library(ggthemes)
library(readr)

games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_schedule_2425.csv",
                         col_types = cols(date = col_date(format = "%m/%d/%y")))

teams.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_team_strength_2425.csv")

teams <-
  games.import %>%
  distinct(home) %>%
  rename(fbref_team = home) %>%
  arrange(fbref_team)

teams$tsl_team <- teams.import$team
teams$rgf <- teams.import$gd / 2
teams$rga <- teams.import$gd / 2
teams$rating <- teams.import$gd

games <-
  games.import %>%
  inner_join(teams %>%
               select(fbref_team, rgf, rga) %>%
               rename(team = fbref_team),
             by = c("home" = "team")) %>%
  rename(home_rgf = rgf,
         home_rga = rga) %>%
  inner_join(teams %>%
               select(fbref_team, rgf, rga) %>%
               rename(team = fbref_team),
             by = c("away" = "team")) %>%
  rename(away_rgf = rgf,
         away_rga = rga) %>%
  mutate(home_lambda = home_rgf - away_rga + 1.5,
         away_lambda = away_rgf - home_rga + 1.5)

set.seed(816)
simulations <- 10000
seasons <- data.frame(season_id = c(1:simulations))

season.games <- data.frame(season_game_id = c(1:(nrow(games)*simulations)),
                           season_id = rep(seasons$season_id, each = nrow(games)),
                           game_id = games$game_id,
                           week = games$week,
                           home = games$home,
                           away = games$away,
                           home_lambda = games$home_lambda,
                           away_lambda = games$away_lambda)

game.simulations <-
  season.games %>%
  mutate(home_goals = rpois(nrow(season.games), lambda = home_lambda),
         away_goals = rpois(nrow(season.games), lambda = away_lambda),
         home_win = ifelse(home_goals > away_goals, 1, 0),
         away_win = ifelse(away_goals > home_goals, 1, 0),
         draw = ifelse(home_goals == away_goals, 1, 0),
         home_points = 3*home_win + draw,
         away_points = 3*away_win + draw)

home.performance <-
  game.simulations %>%
  group_by(home, season_id) %>%
  summarize(matches = n(),
            wins = sum(home_win),
            gf = sum(home_goals),
            ga = sum(away_goals),
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
            gf = sum(away_goals),
            ga = sum(home_goals),
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
            points = sum(points),
            gf = sum(gf),
            ga = sum(ga),) %>%
  ungroup() %>%
  group_by(season_id) %>%
  mutate(placement = rank(desc(points), ties.method = "random"),
         title = ifelse(placement == 1, 1, 0),
         top2 = ifelse(placement <= 2, 1, 0),
         top4 = ifelse(placement <= 4, 1, 0),
         top6 = ifelse(placement <= 6, 1, 0),
         top10 = ifelse(placement <= 10, 1, 0))

summary <-
  tables %>%
  group_by(team) %>%
  summarize(points = mean(points),
            wins = mean(wins),
            draws = mean(draws),
            losses = mean(losses),
            gd = mean(gf - ga) / 38,
            title = mean(title),
            top2 = mean(top2),
            top4 = mean(top4),
            top6 = mean(top6),
            top10 = mean(top10))
