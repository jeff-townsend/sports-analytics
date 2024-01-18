library(tidyverse)
library(nflreadr)

nfl.home.games <-
  load_pbp(2018) %>%
  filter(game_seconds_remaining == 3600,
         season_type == "REG") %>%
  distinct(game_id, season, week, home_team, away_team, home_score, away_score, spread_line, total_line, result, location) %>%
  rename(team = home_team,
         opp = away_team,
         pf = home_score,
         pa = away_score) %>%
  mutate(exp_pf = total_line/2 + spread_line/2,
         exp_pa = total_line/2 - spread_line/2)

nfl.away.games <- data.frame(game_id = nfl.home.games$game_id,
                             season = nfl.home.games$season,
                             week = nfl.home.games$week,
                             team = nfl.home.games$opp,
                             opp = nfl.home.games$team,
                             pf = nfl.home.games$pa,
                             pa = nfl.home.games$pf,
                             spread_line = -nfl.home.games$spread_line,
                             total_line = nfl.home.games$total_line,
                             result = -nfl.home.games$result,
                             location = nfl.home.games$location,
                             exp_pf = nfl.home.games$exp_pa,
                             exp_pa = nfl.home.games$exp_pf)


nfl.games <- rbind(nfl.home.games, nfl.away.games %>%
                                   mutate(location = ifelse(location == "Home", "Away", location)))

team.performance <-
  nfl.games %>%
  group_by(team) %>%
  summarize(gp = n(),
            pd = mean(pf) - mean(pa),
            exp_pd = mean(exp_pf) - mean(exp_pa)) %>%
  ungroup() %>%
  mutate(poe = pd - exp_pd)

nfl.games.sos <-
  nfl.games %>%
  inner_join(team.performance %>% select(team, pd, exp_pd), by = c("opp" = "team")) %>%
  rename(opp_pd = pd,
         opp_exp_pd = exp_pd) %>%
  mutate(adj_pd = result + opp_pd - 1.5*ifelse(location == "Home", 1,
                                               ifelse(location == "Away", -1, 0)),
         adj_exp_pd = spread_line + opp_exp_pd - 1.5*ifelse(location == "Home", 1,
                                                            ifelse(location == "Away", -1, 0)))

team.performance.adj <-
  nfl.games.sos %>%
  group_by(team) %>%
  summarize(gp = n(),
            pd = mean(pf) - mean(pa),
            adj_pd = mean(adj_pd),
            exp_pd = mean(exp_pf) - mean(exp_pa),
            adj_exp_pd = mean(adj_exp_pd)) %>%
  ungroup() %>%
  mutate(poe = adj_pd - adj_exp_pd)

recent.performance.adj <-
  nfl.games.sos %>%
  filter(week >= 12,
         week <= 17) %>%
  group_by(team) %>%
  summarize(gp = n(),
            pd = mean(pf) - mean(pa),
            adj_pd = mean(adj_pd),
            exp_pd = mean(exp_pf) - mean(exp_pa),
            adj_exp_pd = mean(adj_exp_pd)) %>%
  ungroup() %>%
  mutate(poe = adj_pd - adj_exp_pd)

View(nfl.games.sos %>% filter(team == "PHI") %>% arrange(game_id))
