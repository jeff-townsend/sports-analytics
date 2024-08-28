library(tidyverse)
library(readr)

fixtures.import <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_fbref_fixtures.csv",
           col_types = cols(date = col_date(format = "%m/%d/%y"))) %>%
  filter(season == 2023)

teams <-
  fixtures.import %>%
  distinct(home) %>%
  rename(team = home)

# initiate tables for loop
team.elo <- data.frame(team = character(),
                       week = integer(),
                       pre_elo = double(),
                       post_elo = double(),
                       elo_delta = double())
elo.results <- data.frame(fixture_id = integer(),
                          week = integer(),
                          home_team = character(),
                          home_result = double(),
                          home_pre_elo = double(),
                          away_team = character(),
                          away_pre_elo = double(),
                          home_expectation = double(),
                          home_elo_delta = double(),
                          away_elo_delta = double(),
                          rmse = double())

fixtures <-
  fixtures.import %>%
  mutate(fixture_id = c(1:(nrow(fixtures.import)))) %>%
  select(fixture_id, week, home, away, home_goals, away_goals) %>%
  rename(home_team = home,
         away_team = away) %>%
  mutate(home_result = ifelse(home_goals > away_goals, 1, ifelse(home_goals == away_goals, 0.5, 0)),
         away_result = 1 - home_result)
  

starting.elo <-
  teams %>%
  mutate(week = 1,
         pre_elo = 1500,
         post_elo = pre_elo,
         elo_delta = 0)
team.elo <- rbind(team.elo, starting.elo)

# initiate variables for loop
k <- 110
hfa <- 50
w <- 1

for(w in 1:38){
  
  week.fixtures <-
    fixtures %>%
    inner_join(team.elo %>%
                 filter(week == w) %>%
                 select(team, week, pre_elo),
               by = c("home_team" = "team", "week")) %>%
    rename(home_pre_elo = pre_elo) %>%
    inner_join(team.elo %>%
                 filter(week == w) %>%
                 select(team, week, pre_elo),
               by = c("away_team" = "team", "week")) %>%
    rename(away_pre_elo = pre_elo) %>%
    mutate(home_expectation = 1 / (1 + 10 ^ ((away_pre_elo - (home_pre_elo + hfa)) / 400)),
           away_expectation = 1 / (1 + 10 ^ (((home_pre_elo + hfa) - away_pre_elo) / 400)),
           home_elo_delta = k * (home_result - home_expectation),
           away_elo_delta = k * (away_result - away_expectation),
           home_post_elo = home_pre_elo + home_elo_delta,
           away_post_elo = away_pre_elo + away_elo_delta)
  
  # update post_elo and elo_delta from current week
  team.elo <-
    rbind(
      team.elo %>%
        filter(week < w),
      team.elo %>%
        select(team, week, pre_elo) %>%
        inner_join(week.fixtures %>%
                     select(home_team, week, home_post_elo, home_elo_delta),
                   by = c("team" = "home_team", "week")) %>%
        rename(post_elo = home_post_elo,
               elo_delta = home_elo_delta),
      team.elo %>%
        select(team, week, pre_elo) %>%
        inner_join(week.fixtures %>%
                     select(away_team, week, away_post_elo, away_elo_delta),
                   by = c("team" = "away_team", "week")) %>%
        rename(post_elo = away_post_elo,
               elo_delta = away_elo_delta)
    )
  
  # insert pre_elo for the following week
  team.elo <-
    rbind(
      team.elo,
      team.elo %>%
        filter(week == w) %>%
        mutate(week = w + 1,
               pre_elo = post_elo,
               elo_delta = 0)
    )
  
  elo.results <-
    rbind(
      elo.results,
      week.fixtures %>%
        select(fixture_id, week, home_team, home_result, home_pre_elo, away_team,
               away_pre_elo, home_expectation, home_elo_delta, away_elo_delta) %>%
        mutate(rmse = sqrt((home_result - home_expectation)^2))
    )
  
  w <- w + 1
  #gc()
}

final.elo <-
  team.elo %>%
  filter(week == 39) %>%
  select(team, post_elo) %>%
  rename(final_elo = post_elo)

elo.rmse <-
  elo.results %>%
  group_by(week) %>%
  summarize(rmse = mean(rmse))
