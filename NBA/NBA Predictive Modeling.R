library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)

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

pd.padding <- 15
pred.games.df <-
  games.df %>%
  filter(season >= 2010) %>%
  # add opponent info
  inner_join(games.df %>% select(game_id, season, team, game_number, season_game_number, days_rest),
             by = c("game_id", "season", c("opponent" = "team")),
             suffix = c("","_opp")) %>%
  # add prior year stats
  inner_join(yoy.df %>% select(season, team, preseason_pd),
             by = c("season", "team")) %>%
  # add opponent's prior year stats
  inner_join(yoy.df %>% select(season, team, preseason_pd) %>% rename(preseason_pd_opp = preseason_pd),
             by = c("season", "opponent" = "team")) %>%
  # add season-to-date stats
  inner_join(stats.df %>%
               select(season, team, game_number, avg_pd_prior) %>%
               rename(avg_pd = avg_pd_prior),
             by = c("season", "team", "game_number")) %>%
  # add opponent's season-to-date stats
  inner_join(stats.df %>%
               select(season, team, game_number, avg_pd_prior) %>%
               rename(avg_pd_opp = avg_pd_prior),
             by = c("season", "opponent" = "team", "game_number_opp" = "game_number")) %>%
  # # add last 25 game stats
  # mutate(game_number25 = ifelse(game_number <= 25, 1, game_number - 25)) %>%
  # inner_join(pd.df, by = c("season", "team", c("game_number25" = "game_number")), suffix = c("", "25")) %>%
  # mutate(avg_pd25 = ifelse(game_number == 1, 0, (season_pd - season_pd25) / (game_number - game_number25))) %>%
  # select(-game_number25, -season_pd25) %>%
  # # add opponent's last 25 game stats
  # mutate(game_number_opp25 = ifelse(game_number_opp <= 25, 1, game_number_opp - 25)) %>%
  # inner_join(pd.df %>% rename(season_pd_opp = season_pd),
  #            by = c("season", "opponent" = "team", "game_number_opp25" = "game_number"), suffix = c("", "25")) %>%
  # mutate(avg_pd_opp25 = ifelse(game_number_opp == 1, 0, (season_pd_opp - season_pd_opp25) / (game_number_opp - game_number_opp25))) %>%
  # select(-game_number_opp25, -season_pd_opp25) %>%
  
  # add padded stats
  mutate(padded_pd = (avg_pd * (game_number - 1) + pd.padding*preseason_pd) / (game_number + pd.padding - 1),
         padded_pd_opp = (avg_pd_opp * (game_number_opp - 1) + pd.padding*preseason_pd_opp) / (game_number_opp + pd.padding - 1)) %>%
  # calculate matchup metrics
  mutate(combined_game_number = (season_game_number + season_game_number_opp) / 2,
         rest_diff = days_rest - days_rest_opp,
         days_rest_adj = ifelse(is.na(days_rest), 7, days_rest),
         days_rest_opp_adj = ifelse(is.na(days_rest_opp), 7, days_rest_opp),
         rest_diff_adj = days_rest_adj - days_rest_opp_adj,
         rest_diff_bucket = ifelse(rest_diff_adj > 2, 2,
                                   ifelse(rest_diff_adj < -2, -2, rest_diff_adj)),
         preseason_pd_delta = preseason_pd - preseason_pd_opp,
         matchup_pd = avg_pd - avg_pd_opp,
         padded_pd_delta = padded_pd - padded_pd_opp) %>%
  select(-days_rest_adj, -days_rest_opp_adj, -rest_diff_adj)

schedule.mod <- lm(pd ~ is_home*game_type + rest_diff_bucket, data = pred.games.df)
#summary(schedule.mod)

pred.games.df$schedule_pd = predict(schedule.mod, newdata = pred.games.df)
pred.games.df$adj_pd = with(pred.games.df, pd - schedule_pd)

matchup.df <-
  pred.games.df %>%
  filter(game_type == "Regular Season") %>%
  select(game_id, season, team, opponent, combined_game_number, pd,
         avg_pd, avg_pd_opp, matchup_pd, preseason_pd_delta, padded_pd_delta, adj_pd) %>%
  mutate(matchup_pd_error = abs(matchup_pd - adj_pd),
         preseason_pd_error = abs(preseason_pd_delta - adj_pd),
         padded_pd_error = abs(padded_pd_delta - adj_pd))

with(matchup.df, cor(matchup_pd, adj_pd)^2)
# ggplot(matchup.df, aes(x = matchup_pd, y = adj_pd)) +
#   geom_point() +
#   theme_fivethirtyeight()
with(matchup.df, cor(padded_pd_delta, adj_pd)^2)

# overall prediction error
matchup.df %>%
  mutate(season_segment = ifelse(combined_game_number <= 20, "1-20",
                                 ifelse(combined_game_number <= 40, "21-40",
                                        ifelse(combined_game_number <= 60, "41-60", "61-82")))) %>%
  group_by(season_segment) %>%
  summarize(mov = mean(abs(pd)),
            error = mean(matchup_pd_error),
            preseason_error = mean(preseason_pd_error),
            padded_error = mean(padded_pd_error)) %>%
  ungroup() %>%
  mutate(rel_error = error - mov,
         rel_preseason_error = preseason_error - mov,
         rel_padded_error = padded_error - mov)

# prediction error trend
matchup.agg <-
  matchup.df %>%
  group_by(combined_game_number) %>%
  summarize(n = n()/2,
            mov = mean(abs(pd)),
            error = mean(matchup_pd_error),
            preseason_error = mean(preseason_pd_error),
            padded_error = mean(padded_pd_error)) %>%
  ungroup() %>%
  mutate(rel_error = error - mov,
         rel_preseason_error = preseason_error - mov,
         rel_padded_error = padded_error - mov) %>%
  mutate(explained_error = -(error - mov) / mov)

ggplot(matchup.agg, aes(x = combined_game_number, y = rel_padded_error)) +
  geom_line() +
  geom_smooth() +
  theme_fivethirtyeight() +
  labs(title = "Relative prediction error during an 82-game NBA season",
       subtitle = "Using padded season-to-date point differential as a predictor") +
  theme(plot.subtitle = element_text(face = "italic"))

# example of padded PD using 2024 Cavs
cavs24 <-
  rbind(
    pred.games.df %>%
    filter(season == 2024,
           team == "Cleveland Cavaliers",
           game_type == "Regular Season") %>%
    select(season, team, game_number, avg_pd) %>%
    rename(value = avg_pd) %>%
    mutate(metric = "Actual PD"),
    pred.games.df %>%
      filter(season == 2024,
             team == "Cleveland Cavaliers",
             game_type == "Regular Season") %>%
      select(season, team, game_number, preseason_pd) %>%
      rename(value = preseason_pd) %>%
      mutate(metric = "Projected PD"),
    pred.games.df %>%
      filter(season == 2024,
             team == "Cleveland Cavaliers",
             game_type == "Regular Season") %>%
      select(season, team, game_number, padded_pd) %>%
      rename(value = padded_pd) %>%
      mutate(metric = "Padded PD")
  )

ggplot(cavs24, aes(x = game_number, y = value, col = metric)) +
  geom_line() +
  theme_fivethirtyeight() +
  labs(title = "Cleveland Cavaliers' metrics during the 2024 NBA season") +
  xlab("Game Number") +
  ylab("Point Differential") +
  theme(legend.title = element_blank(),
        axis.title = element_text())
