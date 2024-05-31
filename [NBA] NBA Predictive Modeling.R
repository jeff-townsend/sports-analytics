library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)

setwd("/Users/jtownsend/Downloads")
#setwd("/Users/Jeff/Documents/Data Analysis/NBA")

starting.season <- 2010
ending.season <- 2024

bref.df <- data.frame(matrix(ncol = 13, nrow = 0))
col.names <- c("game_id", "season", "game_date", "start_time", "away_team", "away_points",
               "home_team", "home_points", "box_score", "overtimes", "attendance", "arena", "notes")
colnames(bref.df) <- col.names

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
p <- c("2010-04-17", "2011-04-16", "2012-04-28", "2013-04-20", "2014-04-19",
       "2015-04-18", "2016-04-16", "2017-04-15", "2018-04-14", "2019-04-13",
       "2020-08-17", "2021-05-22", "2022-04-16", "2023-04-15", "2024-04-20")
playoff.dates <- data.frame(season = c(2010:2024),
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
pd.df <-
  games.df %>%
  group_by(season, team) %>%
  mutate(season_pd = ifelse(game_number == 1, 0, cumsum(pd) - pd)) %>%
  ungroup() %>%
  select(season, team, game_number, season_pd)

pred.games.df <-
  games.df %>%
  # add opponent info
  inner_join(games.df %>% select(game_id, season, team, game_number, season_game_number, days_rest),
             by = c("game_id", "season", c("opponent" = "team")),
             suffix = c("","_opp")) %>%
  # add season-to-date stats
  inner_join(pd.df, by = c("season", "team", "game_number")) %>%
  mutate(avg_pd = ifelse(game_number == 1, 0, season_pd / (game_number - 1))) %>% # convert pd to an average
  # add opponent's season-to-date stats
  inner_join(pd.df %>% rename(season_pd_opp = season_pd),
             by = c("season", "opponent" = "team", "game_number_opp" = "game_number")) %>%
  mutate(avg_pd_opp = ifelse(game_number_opp == 1, 0, season_pd_opp / (game_number_opp - 1))) %>% # convert pd to an average
  # add last 25 game stats
  mutate(game_number25 = ifelse(game_number <= 25, 1, game_number - 25)) %>%
  inner_join(pd.df, by = c("season", "team", c("game_number25" = "game_number")), suffix = c("", "25")) %>%
  mutate(avg_pd25 = ifelse(game_number == 1, 0, (season_pd - season_pd25) / (game_number - game_number25))) %>%
  select(-game_number25, -season_pd25) %>%
  # add opponent's last 25 game stats
  mutate(game_number_opp25 = ifelse(game_number_opp <= 25, 1, game_number_opp - 25)) %>%
  inner_join(pd.df %>% rename(season_pd_opp = season_pd),
             by = c("season", "opponent" = "team", "game_number_opp25" = "game_number"), suffix = c("", "25")) %>%
  mutate(avg_pd_opp25 = ifelse(game_number_opp == 1, 0, (season_pd_opp - season_pd_opp25) / (game_number_opp - game_number_opp25))) %>%
  select(-game_number_opp25, -season_pd_opp25) %>%
  # calculate matchup metrics
  mutate(combined_game_number = (season_game_number + season_game_number_opp) / 2,
         rest_diff = days_rest - days_rest_opp,
         days_rest_adj = ifelse(is.na(days_rest), 7, days_rest),
         days_rest_opp_adj = ifelse(is.na(days_rest_opp), 7, days_rest_opp),
         rest_diff_adj = days_rest_adj - days_rest_opp_adj,
         rest_diff_bucket = ifelse(rest_diff_adj > 2, 2,
                                   ifelse(rest_diff_adj < -2, -2, rest_diff_adj)),
         matchup_pd = avg_pd - avg_pd_opp,
         matchup_pd25 = avg_pd25 - avg_pd_opp25) %>%
  select(-days_rest_adj, -days_rest_opp_adj, -rest_diff_adj)

schedule.mod <- lm(pd ~ is_home*game_type + rest_diff_bucket, data = pred.games.df)
#summary(schedule.mod)

pred.games.df$schedule_pd = predict(schedule.mod, newdata = pred.games.df)
pred.games.df$adj_pd = with(pred.games.df, pd - schedule_pd)

matchup.df <-
  pred.games.df %>%
  filter(game_type == "Regular Season") %>%
  select(game_id, season, team, opponent, combined_game_number, pd,
         avg_pd, avg_pd_opp, matchup_pd, avg_pd25, avg_pd_opp25, matchup_pd25, adj_pd) %>%
  mutate(matchup_pd_error = abs(matchup_pd - adj_pd),
         matchup_pd25_error = abs(matchup_pd25 - adj_pd))

with(matchup.df, cor(matchup_pd, adj_pd)^2)
# ggplot(matchup.df, aes(x = matchup_pd, y = adj_pd)) +
#   geom_point() +
#   theme_fivethirtyeight()
with(matchup.df, cor(matchup_pd25, adj_pd)^2)

# overall prediction error
matchup.df %>%
  #filter(combined_game_number >= 25) %>%
  summarize(mov = mean(abs(pd)),
            error = mean(matchup_pd_error),
            error25 = mean(matchup_pd25_error)) %>%
  mutate(rel_error = error - mov,
         rel_error25 = error25 - mov)

# prediction error trend
matchup.agg <-
  matchup.df %>%
  group_by(combined_game_number) %>%
  summarize(n = n()/2,
            mov = mean(abs(pd)),
            error = mean(matchup_pd_error),
            error25 = mean(matchup_pd25_error)) %>%
  ungroup() %>%
  mutate(rel_error = error - mov,
         rel_error25 = error25 - mov)

ggplot(matchup.agg, aes(x = combined_game_number, y = rel_error25)) +
  geom_line() +
  geom_smooth() +
  theme_fivethirtyeight() +
  labs(title = "Relative prediction error during an 82-game NBA season",
       subtitle = "Using point differential from the previous 25 games") +
  theme(plot.subtitle = element_text(face = "italic"))
