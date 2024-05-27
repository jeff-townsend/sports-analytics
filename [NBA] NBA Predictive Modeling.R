library(readr)
library(tidyverse)
library(lubridate)

setwd("/Users/jtownsend/Downloads")

bref.df <- data.frame(matrix(ncol = 13, nrow = 0))
col.names <- c("game_id", "season", "game_date", "start_time", "away_team", "away_points",
               "home_team", "home_points", "box_score", "overtimes", "attendance", "arena", "notes")
colnames(bref.df) <- col.names

starting.season <- 2022
ending.season <- 2024

s <- starting.season

for(s in starting.season:ending.season)
  {
  bref.import <-
    read_csv(paste0("basketball_reference_games_", s, ".csv"),
             col_types = cols(game_date = col_date(format = "%Y-%m-%d"),
                              notes = col_character())) %>%
    mutate(overtimes = ifelse(!is.na(overtimes), overtimes, ""),
           notes = ifelse(!is.na(notes), notes, "")) %>%
    filter(!is.na(box_score))
  
  bref.df <- rbind(bref.df, bref.import)
  s <- s + 1
}

# use game dates to determine whether it's a playoff game
p <- c("2015-04-18", "2016-04-16", "2017-04-15", "2018-04-14", "2019-04-13",
       "2020-08-17", "2021-05-22", "2022-04-16", "2023-04-15", "2024-04-20")
playoff.dates <- data.frame(season = c(2015:2024),
                            playoff_start_date = p)

## create new data frame where each observation is a team game

away.df <-
  bref.df %>%
  select(game_id, season, game_date, away_team, away_points, home_team, home_points, notes) %>%
  rename(team = away_team,
         points = away_points,
         opponent = home_team,
         points_allowed = home_points) %>%
  mutate(is_home = 0)

home.df <-
  bref.df %>%
  select(game_id, season, game_date, home_team, home_points, away_team, away_points, notes) %>%
  rename(team = home_team,
         points = home_points,
         opponent = away_team,
         points_allowed = away_points) %>%
  mutate(is_home = 1)

# merge two tables and add new columns

merge.df <-
  rbind(away.df, home.df) %>%
  group_by(season, team) %>%
  mutate(game_number = rank(game_date))

games.tmp <-
  merge.df %>%
  arrange(game_id) %>%
  mutate(pd = points - points_allowed,
         is_win = ifelse(pd > 0, 1, 0)) %>%
  inner_join(playoff.dates, by = "season") %>%
  mutate(game_type = ifelse(game_date >= playoff_start_date | 
                              notes == "Play-In Game" | 
                              game_id == "202312090LAL",
                            "Playoffs", "Regular Season"),
         game_subtype = ifelse(notes == "Play-In Game", "Play-In",
                               ifelse(notes == "In-Season Tournament", "In-Season Tournament", game_type))) %>%
  select(-playoff_start_date) %>%
  left_join(merge.df %>%
              mutate(game_number_match = game_number + 1) %>%
              select(season, team, game_date, game_number_match),
            by = c("season", "team", c("game_number" = "game_number_match")),
            suffix = c("", "_prior")) %>%
  mutate(days_rest = as.numeric(game_date - game_date_prior) - 1) %>%
  select(-game_date_prior)

games.df <-
  games.tmp %>%
  inner_join(games.tmp %>%
               select(game_id, season, team, days_rest),
             by = c("game_id", "season", c("opponent" = "team")),
             suffix = c("","_opp")) %>%
  mutate(rest_diff = days_rest - days_rest_opp,
         days_rest_adj = ifelse(is.na(days_rest), 7, days_rest),
         days_rest_opp_adj = ifelse(is.na(days_rest_opp), 7, days_rest_opp),
         rest_diff_adj = days_rest_adj - days_rest_opp_adj,
         rest_diff_bucket = ifelse(rest_diff_adj > 2, 2,
                                   ifelse(rest_diff_adj < -2, -2, rest_diff_adj))) %>%
  select(-days_rest_adj, -days_rest_opp_adj, -rest_diff_adj)

game.pred.mod <- lm(pd ~ is_home*game_type + rest_diff_bucket, data = games.df)
summary(game.pred.mod)

games.df$pred_pd = predict(game.pred.mod, newdata = games.df)
games.df$adj_pd = with(games.df, pd - pred_pd)

games.df %>%
  group_by(game_type) %>%
  summarize(error = mean(abs(adj_pd)))

## summarize regular season data

season.stats <-
  games.df %>%
  filter(game_type == "Regular Season") %>%
  group_by(season, team) %>%
  summarize(games = n(),
            wins = sum(is_win),
            losses = n() - sum(is_win),
            win_rate = mean(is_win),
            pf = mean(points),
            pa = mean(points_allowed),
            pd = mean(pd),
            exp_pd = mean(pred_pd),
            adj_pd = mean(adj_pd))

xwinrate.mod <- lm(win_rate ~ pd, data = season.stats)
#summary(xwinrate.mod)

## split-half reliability

rs.games.df <-
  games.df %>%
  filter(game_type == "Regular Season") %>%
  select(-game_number) %>%
  group_by(season, team) %>%
  mutate(game_number = rank(game_date)) %>%
  ungroup()

odd.even.stats <-
  rs.games.df %>%
  mutate(is_odd = ifelse(game_number%%2 == 1, 1, 0)) %>%
  group_by(season, team, is_odd) %>%
  summarize(games = n(),
            wins = sum(is_win),
            losses = n() - sum(is_win),
            win_rate = mean(is_win),
            pf = mean(points),
            pa = mean(points_allowed)) %>%
  ungroup() %>%
  mutate(pd = pf - pa,
         xwin_rate = xwinrate.mod$coefficients[1] + xwinrate.mod$coefficients[2]*pd,
         win_rate_delta = win_rate - xwin_rate)

shr.data <-
  odd.even.stats %>%
  filter(is_odd == 1) %>%
  select(-is_odd) %>%
  inner_join(odd.even.stats %>%
               filter(is_odd == 0) %>%
               select(-is_odd),
             by = c("season", "team"), suffix = c("_odd", "_even"))

# ggplot(shr.data, aes(x = pd_even, y = win_rate_odd)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x)

# shr.data %>%
#   summarize(r = cor(win_rate_odd, win_rate_even)) %>%
#   mutate(r2 = r^2)
# 
# shr.data %>%
#   summarize(r = cor(pd_odd, pd_even)) %>%
#   mutate(r2 = r^2)

shr.mod <- lm(win_rate_even ~ pd_odd, data = shr.data)
#summary(wr.mod)
