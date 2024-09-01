library(tidyverse)
library(ggthemes)
library(readr)

fixtures.import <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_fbref_fixtures.csv",
           col_types = cols(date = col_date(format = "%m/%d/%y")))

teams <-
  fixtures.import %>%
  distinct(home, season) %>%
  rename(team = home)

fixtures <-
  fixtures.import %>%
  mutate(fixture_id = c(1:(nrow(fixtures.import)))) %>%
  select(fixture_id, season, week, home, away, home_goals, away_goals, home_xg, away_xg) %>%
  rename(home_team = home,
         away_team = away) %>%
  mutate(home_result = ifelse(home_goals > away_goals, 1, ifelse(home_goals == away_goals, 0.5, 0)),
         away_result = 1 - home_result)

fixtures.long <-
  rbind(
    fixtures %>%
      mutate(is_home = 1) %>%
      select(fixture_id, season, week, home_team, away_team, is_home, home_goals, away_goals, home_xg, away_xg) %>%
      rename(team = home_team,
             opponent = away_team,
             gf = home_goals,
             ga = away_goals,
             xgf = home_xg,
             xga = away_xg),
    fixtures %>%
      mutate(is_home = 0) %>%
      select(fixture_id, season, week, away_team, home_team, is_home, away_goals, home_goals, away_xg, home_xg) %>%
      rename(team = away_team,
             opponent = home_team,
             gf = away_goals,
             ga = home_goals,
             xgf = away_xg,
             xga = home_xg)) %>%
  arrange(fixture_id) %>%
  mutate(result = ifelse(gf > ga, 1, ifelse(gf == ga, 0.5, 0)),
         gd = gf - ga)

# initiate tables for loop

team.elo <- data.frame(team = character(),
                       season = integer(),
                       week = integer(),
                       pre_elo = double(),
                       post_elo = double(),
                       elo_delta = double(),
                       season_gd = double())
elo.results <- data.frame(fixture_id = integer(),
                          season = integer(),
                          week = integer(),
                          team = character(),
                          pre_elo = double(),
                          opponent = character(),
                          opp_pre_elo = double(),
                          is_home = integer(),
                          expectation = double(),
                          result = double(),
                          elo_delta = double(),
                          rmse = double())

# initiate variables for loop

k <- 50
hfa <- 48
s <- 2019
for(s in 2019:2023){
  
  relegation.elo <-
    teams %>%
    filter(season == s) %>%
    right_join(team.elo %>%
                 filter(season == s - 1,
                        week == 39) %>%
                 select(team, post_elo, season_gd),
               by = "team") %>%
    rename(final_elo = post_elo) %>%
    filter(is.na(season)) %>%
    summarize(final_elo = mean(final_elo),
              season_gd = mean(season_gd))
  
  promotion.elo <- with(relegation.elo, ifelse(is.nan(final_elo), 1500, final_elo))
  promotion.gd <- with(relegation.elo, ifelse(is.nan(season_gd), 0, season_gd))
  
  starting.elo <-
    teams %>%
    filter(season == s) %>%
    mutate(week = 1,
           prior_season = season - 1) %>%
    left_join(team.elo %>%
                filter(week == 39) %>%
                select(team, season, post_elo, season_gd),
              by = c("team", "prior_season" = "season")) %>%
    mutate(final_elo_prior = ifelse(!is.na(post_elo), post_elo, promotion.elo),
           season_gd_prior = ifelse(!is.na(season_gd), season_gd, promotion.gd)) %>%
    select(team, season, week, final_elo_prior, season_gd_prior)
  starting.elo$pre_elo <- with(starting.elo, 915.599 + 0.391*final_elo_prior + 1.8891*season_gd_prior)
  #starting.elo$pre_elo <- 1500
  starting.elo$post_elo <- starting.elo$pre_elo
  starting.elo$elo_delta <- 0
  starting.elo$season_gd <- 0
  
  team.elo <- rbind(team.elo,
                    starting.elo %>%
                      select(team, season, week, pre_elo, post_elo, elo_delta, season_gd))
  
  w <- 1
  for(w in 1:38){
    
    week.fixtures <-
      fixtures.long %>%
      select(-xgf, -xga) %>%
      inner_join(team.elo %>%
                   filter(season == s,
                          week == w) %>%
                   select(team, season, week, pre_elo, season_gd),
                 by = c("team", "season", "week")) %>%
      inner_join(team.elo %>%
                   filter(season == s,
                          week == w) %>%
                   select(team, season, week, pre_elo) %>%
                   rename(opp_pre_elo = pre_elo),
                 by = c("opponent" = "team", "season", "week")) %>%
      mutate(expectation = 1 / (1 + 10 ^ (((opp_pre_elo + hfa*(1-is_home)) - (pre_elo + hfa*is_home)) / 400)),
             margin = abs(gd),
             adj_k = ifelse(margin <= 1, k, (1 + (2 ^ (margin - 1) - 1) / (2 ^ (margin - 1))) * k),
             elo_delta = adj_k * (result - expectation),
             post_elo = pre_elo + elo_delta,
             season_gd = season_gd + gd)
    
    # update post_elo and elo_delta from current week
    team.elo <-
      rbind(
        team.elo %>%
          filter(season < s),
        team.elo %>%
          filter(season == s,
                 week < w),
        team.elo %>%
          select(team, season, week, pre_elo) %>%
          inner_join(week.fixtures %>%
                       select(team, season, week, post_elo, elo_delta, season_gd),
                     by = c("team", "season", "week")))
    
    # insert pre_elo for the following week
    team.elo <-
      rbind(
        team.elo,
        team.elo %>%
          filter(season == s,
                 week == w) %>%
          mutate(week = w + 1,
                 pre_elo = post_elo,
                 elo_delta = 0))
    
    elo.results <-
      rbind(
        elo.results,
        week.fixtures %>%
          select(fixture_id, season, week, team, pre_elo, opponent, opp_pre_elo,
                 is_home, expectation, result, elo_delta) %>%
          mutate(rmse = sqrt((result - expectation)^2)))
    
    w <- w + 1
    #gc()
  }
  
  s <- s + 1
  
}

final.elo <-
  team.elo %>%
  filter(week == 39) %>%
  select(team, season, post_elo, season_gd) %>%
  rename(final_elo = post_elo)

# calculte home-field advantage
hfa.delta <- with(elo.results %>% filter(is_home == 1), mean(result - expectation))
hfa.elo <- -400*log10(1 / (hfa.delta + 0.5) - 1)

elo.rmse <-
  elo.results %>%
  group_by(season) %>%
  summarize(rmse = mean(rmse))
mean(elo.rmse$rmse)

yoy.elo <-
  final.elo %>%
  mutate(prior_season = season - 1) %>%
  inner_join(final.elo, by = c("team", "prior_season" = "season"), suffix = c("", "_prior")) %>%
  select(-prior_season)

# ggplot(yoy.elo, aes(x = final_elo_prior, y = final_elo)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_fivethirtyeight()

#elo.pred <- lm(final_elo ~ final_elo_prior + season_gd_prior, data = yoy.elo)
summary(elo.pred)

elo.predictions <-
  final.elo %>%
  filter(season == 2023) %>%
  select(team, final_elo, season_gd) %>%
  rename(final_elo_prior = final_elo,
         season_gd_prior = season_gd)
elo.predictions$starting_elo = predict(elo.pred, newdata = elo.predictions)
