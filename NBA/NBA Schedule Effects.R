library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(rpart)
library(rpart.plot)

#setwd("/Users/jtownsend/Downloads")
#setwd("/Users/Jeff/Documents/Data Analysis/NBA")

starting.season <- 2009
ending.season <- 2024

bref.df <- data.frame(matrix(ncol = 13, nrow = 0))
col.names <- c("game_id", "season", "game_date", "start_time", "away_team", "away_points",
               "home_team", "home_points", "box_score", "overtimes", "attendance", "arena", "notes")
colnames(bref.df) <- col.names

s <- starting.season

for(s in starting.season:ending.season)
{
  myfile <- paste0("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NBA/basketball_reference_games_", s, ".csv")
  bref.import <-
    read_csv(myfile,
             col_types = cols(game_date = col_date(format = "%Y-%m-%d"),
                              notes = col_character())) %>%
    mutate(overtimes = ifelse(!is.na(overtimes), overtimes, ""),
           notes = ifelse(!is.na(notes), notes, "")) %>%
    filter(!is.na(box_score))
  
  bref.df <- rbind(bref.df, bref.import)
  s <- s + 1
}

# use game dates to determine whether it's a playoff game
p <- c("2009-04-18",
       "2010-04-17", "2011-04-16", "2012-04-28", "2013-04-20", "2014-04-19",
       "2015-04-18", "2016-04-16", "2017-04-15", "2018-04-14", "2019-04-13",
       "2020-08-17", "2021-05-22", "2022-04-16", "2023-04-15", "2024-04-20")
playoff.dates <- data.frame(season = c(starting.season:ending.season),
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
  mutate(game_number = rank(game_date)) %>%
  ungroup()

games.df <-
  merge.df %>%
  arrange(game_id) %>%
  # add game results
  mutate(pd = points - points_allowed,
         is_win = ifelse(pd > 0, 1, 0)) %>%
  # add game type
  inner_join(playoff.dates, by = "season") %>%
  mutate(game_type = ifelse(game_date >= playoff_start_date | 
                              notes == "Play-In Game" | 
                              game_id == "202312090LAL",
                            "Playoffs", "Regular Season"),
         game_subtype = ifelse(notes == "Play-In Game", "Play-In",
                               ifelse(notes == "In-Season Tournament", "In-Season Tournament", game_type))) %>%
  select(-playoff_start_date) %>%
  # add regular season game number
  group_by(season, team, game_type) %>%
  mutate(season_game_number = rank(game_number)) %>%
  ungroup() %>%
  # add days rest
  left_join(merge.df %>%
              mutate(game_number_match = game_number + 1) %>%
              select(season, team, game_date, game_number_match),
            by = c("season", "team", c("game_number" = "game_number_match")),
            suffix = c("", "_prior")) %>%
  mutate(days_rest = as.numeric(game_date - game_date_prior) - 1) %>%
  select(-game_date_prior)

# add season-to-date point differential
stats.df <-
  games.df %>%
  group_by(season, team) %>%
  mutate(games_remaining = rank(desc(game_number)) - 1,
         season_pd_prior = cumsum(pd) - pd,
         season_pd_post = cumsum(pd)) %>%
  ungroup() %>%
  mutate(avg_pd_prior = ifelse(game_number == 1, 0, season_pd_prior / (game_number - 1)),
         avg_pd_post = season_pd_post / game_number) %>%
  select(season, team, game_number, season_pd_prior, avg_pd_prior, season_pd_post, avg_pd_post, games_remaining)

yoy.df <-
  stats.df %>%
  filter(games_remaining == 0) %>%
  select(season, team, avg_pd_post) %>%
  rename(avg_pd = avg_pd_post) %>%
  mutate(prior_season = season - 1) %>%
  inner_join(stats.df %>%
               filter(games_remaining == 0) %>%
               select(season, team, avg_pd_post) %>%
               rename(avg_pd = avg_pd_post),
             by = c("prior_season" = "season", "team"),
             suffix = (c("","_prior")))

yoy.pd.mod <- lm(avg_pd ~ 0 + avg_pd_prior, data = yoy.df)
summary(yoy.pd.mod)

yoy.df$preseason_pd <- predict(yoy.pd.mod, newdata = yoy.df)

pred.games.df <-
  games.df %>%
  # add opponent info
  inner_join(games.df %>% select(game_id, season, team, game_number, season_game_number, days_rest),
             by = c("game_id", "season", c("opponent" = "team")),
             suffix = c("","_opp")) %>%
  # calculate matchup metrics
  mutate(combined_game_number = (season_game_number + season_game_number_opp) / 2,
         rest_diff = days_rest - days_rest_opp,
         days_rest_adj = ifelse(is.na(days_rest), 7, days_rest),
         days_rest_opp_adj = ifelse(is.na(days_rest_opp), 7, days_rest_opp),
         rest_diff_adj = days_rest_adj - days_rest_opp_adj,
         rest_diff_bucket = ifelse(rest_diff_adj > 2, 2,
                                   ifelse(rest_diff_adj < -2, -2, rest_diff_adj))) %>%
  select(-days_rest_adj, -days_rest_opp_adj, -rest_diff_adj)

tree.df <-
  pred.games.df %>%
  select(pd, is_home, game_type, days_rest, days_rest_opp)

tree.mod <-
  rpart(pd ~ .,
        data = tree.df,
        method = "anova",
        cp = -1,
        minbucket = 500)
rpart.plot(tree.mod)

schedule.combinations <-
  pred.games.df %>%
  distinct(is_home, game_type, days_rest, days_rest_opp)
schedule.combinations$schedule_pd <- rpart.predict(tree.mod, newdata = schedule.combinations)


