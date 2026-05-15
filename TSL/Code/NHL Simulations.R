library(tidyverse)
library(ggthemes)

library(readxl)
nhl.schedule.import <- read_excel("Data Analysis/Team Super League/2023-24 NHL Schedule.xlsx")
nhl.teams <-
  read_excel("Data Analysis/Team Super League/2023-24 Teams.xlsx") %>%
  filter(league == "NHL")
nhl.team.strength <- read_excel("Data Analysis/Team Super League/2023-24 NHL Team Strength.xlsx")

tsl.scoring <- data.frame(placement = c(1:16),
                          tsl_points = c(32, 27, 23, 23, 19, 19, 16, 14,
                                         7, 6, 5, 4, 3, 2, 1, 0))

## import playoff matchup data
rounds.import <- read_excel("Data Analysis/Team Super League/Playoff Rounds.xlsx", sheet = "NHL")
series.import <- read_excel("Data Analysis/Team Super League/Playoff Games.xlsx", sheet = "NHL")
matchups.import <- read_excel("Data Analysis/Team Super League/Playoff Matchups.xlsx", sheet = "NHL")

hfa <- 0.00 ## use 50% win rate for home teams

nhl.schedule <-
  nhl.schedule.import %>%
  mutate(completed = 0) %>%
  inner_join(nhl.team.strength %>% select(team, win_rate), by = c("away_team" = "team")) %>%
  rename(away_win_rate = win_rate) %>%
  inner_join(nhl.team.strength %>% select(team, win_rate), by = c("home_team" = "team")) %>%
  rename(home_win_rate = win_rate) %>%
  mutate(away_win_prob = (away_win_rate - away_win_rate * home_win_rate) /
           (away_win_rate + home_win_rate - 2 * away_win_rate * home_win_rate),
         home_win_prob = 1 - away_win_prob)

set.seed(1010)
simulations <- 1000

nhl.seasons <- data.frame(id = c(1:(nrow(nhl.schedule)*simulations)),
                          season_id = rep(c(1:simulations), each = nrow(nhl.schedule)),
                          date = nhl.schedule$date,
                          away_team = nhl.schedule$away_team,
                          home_team = nhl.schedule$home_team,
                          away_win_prob = nhl.schedule$away_win_prob,
                          home_win_prob = nhl.schedule$home_win_prob)

nhl.simulations <-
  nhl.seasons %>%
  mutate(rng = runif(nrow(nhl.seasons))) %>%
  mutate(away_win = ifelse(rng < away_win_prob, 1, 0),
         home_win = 1 - away_win,
         away_points = 2 * away_win,
         home_points = 2 * home_win)

home.performance <-
  nhl.simulations %>%
  group_by(season_id, home_team) %>%
  summarize(points = sum(home_points)) %>%
  ungroup() %>%
  rename(team = home_team)

away.performance <-
  nhl.simulations %>%
  group_by(season_id, away_team) %>%
  summarize(points = sum(away_points)) %>%
  ungroup() %>%
  rename(team = away_team)

standings <-
  home.performance %>%
  inner_join(nhl.teams %>% select(team, conference, division), by = "team") %>%
  inner_join(away.performance, by = c("season_id", "team")) %>%
  mutate(points = points.x + points.y) %>%
  select(season_id, team, conference, division, points) %>%
  group_by(season_id, conference, division) %>%
  mutate(division_placement = rank(desc(points), ties.method = "random")) %>%
  ungroup() %>%
  group_by(season_id, conference) %>%
  mutate(conference_placement = rank(desc(points-division_placement/100), ties.method = "random"))

division.winners <-
  standings %>%
  filter(division_placement == 1) %>%
  mutate(seed = ifelse(conference_placement == 1, 1, 2)) %>%
  select(season_id, team, conference, division, seed)

wildcard.teams <-
  standings %>%
  filter(division_placement > 3) %>%
  mutate(wildcard_placement = rank(conference_placement)) %>%
  filter(wildcard_placement <= 2) %>%
  select(season_id, team, conference, wildcard_placement) %>%
  inner_join(division.winners %>% select(-team) %>% mutate(wc_match = 3 - seed),
             by = c("season_id", "conference", "wildcard_placement" = "wc_match")) %>%
  select(season_id, team, conference, division) %>%
  mutate(seed = 4)

wildcard.seeding <- rbind(standings %>%
                            filter(division_placement <= 3) %>%
                            select(season_id, team, conference, division, division_placement) %>%
                            rename(seed = division_placement),
                          wildcard.teams)

wildcard.round <-
  matchups.import %>%
  inner_join(rounds.import %>% select(id, points_per_win), by = c("round_id" = "id")) %>%
  filter(round_id == 1) %>%
  inner_join(wildcard.seeding, by = c("home_seed" = "seed")) %>%
  rename(home_team = team) %>%
  inner_join(wildcard.seeding, by = c("season_id", "away_seed" = "seed", "conference", "division")) %>%
  rename(away_team = team) %>%
  select(season_id, id, series_id, round_id, points_per_win,
         home_seed, home_team, away_seed, away_team, conference, division) %>%
  rename(matchup_id = id) %>%
  mutate(home_win_rate = 0.5,
         away_win_rate = 1 - home_win_rate,
         home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob,
         rng = runif(4*simulations*(nrow(matchups.import %>% filter(round_id == 1)))),
         winner = ifelse(rng < home_win_prob, home_team, away_team),
         winner_reseed = home_seed) %>%
  ungroup()

divisional.seeding <-
  wildcard.round %>%
  select(season_id, winner, conference, division, winner_reseed) %>%
  rename(team = winner,
         seed = winner_reseed)

divisional.round <-
  matchups.import %>%
  inner_join(rounds.import %>% select(id, points_per_win), by = c("round_id" = "id")) %>%
  filter(round_id == 2) %>%
  inner_join(divisional.seeding, by = c("home_seed" = "seed")) %>%
  rename(home_team = team) %>%
  inner_join(divisional.seeding, by = c("season_id", "away_seed" = "seed", "conference", "division")) %>%
  rename(away_team = team) %>%
  select(season_id, id, series_id, round_id, points_per_win,
         home_seed, home_team, away_seed, away_team, conference, division) %>%
  rename(matchup_id = id) %>%
  mutate(home_win_rate = 0.5,
         away_win_rate = 1 - home_win_rate,
         home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob,
         rng = runif(4*simulations*(nrow(matchups.import %>% filter(round_id == 2)))),
         winner = ifelse(rng < home_win_prob, home_team, away_team),
         winner_reseed = home_seed) %>%
  ungroup()

conference.seeding <-
  divisional.round %>%
  select(season_id, winner, conference, winner_reseed) %>%
  rename(team = winner,
         seed = winner_reseed)

# fix fact that both seeds = 1
conference.finals <-
  matchups.import %>%
  inner_join(rounds.import %>% select(id, points_per_win), by = c("round_id" = "id")) %>%
  filter(round_id == 3) %>%
  inner_join(divisional.seeding, by = c("home_seed" = "seed")) %>%
  rename(home_team = team) %>%
  inner_join(divisional.seeding, by = c("season_id", "away_seed" = "seed", "conference")) %>%
  rename(away_team = team) %>%
  select(season_id, id, series_id, round_id, points_per_win,
         home_seed, home_team, away_seed, away_team, conference, division) %>%
  rename(matchup_id = id) %>%
  mutate(home_win_rate = 0.5,
         away_win_rate = 1 - home_win_rate,
         home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob,
         rng = runif(4*simulations*(nrow(matchups.import %>% filter(round_id == 2)))),
         winner = ifelse(rng < home_win_prob, home_team, away_team),
         winner_reseed = home_seed) %>%
  ungroup()
