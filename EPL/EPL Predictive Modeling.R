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

# initiate tables for loop
team.elo <- data.frame(team = character(),
                       season = integer(),
                       week = integer(),
                       pre_elo = double(),
                       post_elo = double(),
                       elo_delta = double())
elo.results <- data.frame(fixture_id = integer(),
                          season = integer(),
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
             xga = home_xg)
  ) %>%
  arrange(fixture_id) %>%
  mutate(win = ifelse(gf > ga, 1, 0),
         draw = ifelse(gf == ga, 1, 0),
         loss = 1 - win - draw,
         points = 3 * win + draw)

# initiate variables for loop
k <- 70
hfa <- 47
yoy.reg <- .35
s <- 2019
for(s in 2019:2023){
  
  relegation.elo <-
    teams %>%
    filter(season == s) %>%
    right_join(team.elo %>%
                 filter(season == s - 1,
                        week == 39) %>%
                 select(team, post_elo),
               by = "team") %>%
    rename(final_elo = post_elo) %>%
    filter(is.na(season)) %>%
    select(team, final_elo)
  
  promotion.elo <- ifelse(is.nan(mean(relegation.elo$final_elo)), 1500, mean(relegation.elo$final_elo))
  
  starting.elo <-
    teams %>%
    filter(season == s) %>%
    mutate(week = 1,
           prior_season = season - 1) %>%
    left_join(team.elo %>%
                filter(week == 39) %>%
                select(team, season, post_elo),
              by = c("team", "prior_season" = "season")) %>%
    mutate(pre_elo = ifelse(is.na(post_elo), promotion.elo, post_elo),
           pre_elo = pre_elo - yoy.reg * (pre_elo - 1500),
           post_elo = pre_elo,
           elo_delta = 0) %>%
    select(team, season, week, pre_elo, post_elo, elo_delta)
  team.elo <- rbind(team.elo, starting.elo)
  
  w <- 1
  for(w in 1:38){
    
    week.fixtures <-
      fixtures %>%
      select(-home_xg, -away_xg) %>%
      inner_join(team.elo %>%
                   filter(season == s,
                          week == w) %>%
                   select(team, season, week, pre_elo),
                 by = c("home_team" = "team", "season", "week")) %>%
      rename(home_pre_elo = pre_elo) %>%
      inner_join(team.elo %>%
                   filter(season == s,
                          week == w) %>%
                   select(team, season, week, pre_elo),
                 by = c("away_team" = "team", "season", "week")) %>%
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
          filter(season < s),
        team.elo %>%
          filter(season == s,
                 week < w),
        team.elo %>%
          select(team, season, week, pre_elo) %>%
          inner_join(week.fixtures %>%
                       select(home_team, season, week, home_post_elo, home_elo_delta),
                     by = c("team" = "home_team", "season", "week")) %>%
          rename(post_elo = home_post_elo,
                 elo_delta = home_elo_delta),
        team.elo %>%
          select(team, season, week, pre_elo) %>%
          inner_join(week.fixtures %>%
                       select(away_team, season, week, away_post_elo, away_elo_delta),
                     by = c("team" = "away_team", "season", "week")) %>%
          rename(post_elo = away_post_elo,
                 elo_delta = away_elo_delta))
    
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
          select(fixture_id, season, week, home_team, home_result, home_pre_elo,
                 away_team, away_pre_elo, home_expectation, home_elo_delta, away_elo_delta) %>%
          mutate(rmse = sqrt((home_result - home_expectation)^2)))
    
    w <- w + 1
    #gc()
  }
  
  s <- s + 1
  
}

final.elo <-
  team.elo %>%
  filter(week == 39) %>%
  select(team, season, post_elo) %>%
  rename(final_elo = post_elo)

final.stats <-
  fixtures.long %>%
  group_by(team, season) %>%
  summarize(gd = mean(gf - ga),
            xgd = mean(xgf - xga)) %>%
  ungroup() %>%
  inner_join(final.elo, by = c("team", "season"))

# calculte home-field advantage
hfa.delta <- with(elo.results, mean(home_result - home_expectation))
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

yoy.stats <-
  final.stats %>%
  mutate(prior_season = season - 1) %>%
  inner_join(final.stats, by = c("team", "prior_season" = "season"), suffix = c("", "_prior")) %>%
  select(-prior_season)

ggplot(yoy.elo, aes(x = final_elo_prior, y = final_elo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight()

elo.pred <- lm(final_elo ~ final_elo_prior + gd_prior, data = yoy.stats)
summary(elo.pred)

elo.predictions <-
  final.stats %>%
  filter(season == 2023) %>%
  select(team, final_elo, gd) %>%
  rename(final_elo_prior = final_elo,
         gd_prior = gd)
elo.predictions$starting_elo = predict(elo.pred, newdata = elo.predictions)
