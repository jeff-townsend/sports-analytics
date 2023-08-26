library(tidyverse)
library(readxl)
epl.schedule.load <- read_excel("Data Analysis/Team Super League/2023-24 EPL Schedule.xlsx", 
                                col_types = c("numeric", "date", "text", "text"))
tsl.scoring <- data.frame(placement = c(1:20),
                          tsl_points = c(80, 60, 40, 40, 25, 20, 16, 14, 12, 10,
                                         8, 7, 6, 5, 4, 3, 2, 0, 0, 0))

# start with 39/39/22 probabilities

epl.schedule <-
  epl.schedule.load %>%
  mutate(home_win_prob = .43,
         draw_prob = .23,
         away_win_prob = .34)

simulations <- 1000
epl.seasons <- data.frame(id = c(1:(nrow(epl.schedule)*simulations)),
                          season_id = rep(c(1:simulations), each = nrow(epl.schedule)),
                          week = epl.schedule$week,
                          home = epl.schedule$home,
                          away = epl.schedule$away,
                          home_win_prob = epl.schedule$home_win_prob,
                          draw_prob = epl.schedule$draw_prob,
                          away_win_prob = epl.schedule$away_win_prob)


epl.simulations <-
  epl.seasons %>%
  mutate(rng = runif(nrow(epl.seasons))) %>%
  mutate(home_win = ifelse(rng < home_win_prob, 1, 0),
         draw = ifelse(rng > (1 - draw_prob), 1, 0),
         away_win = 1 - home_win - draw,
         home_points = 3*home_win + draw,
         away_points = 3*away_win + draw)

home.performance <-
  epl.simulations %>%
  group_by(season_id, home) %>%
  summarize(points = sum(home_points)) %>%
  ungroup() %>%
  rename(team = home)

away.performance <-
  epl.simulations %>%
  group_by(season_id, away) %>%
  summarize(points = sum(away_points)) %>%
  ungroup() %>%
  rename(team = away)

team.performance <-
  home.performance %>%
  inner_join(away.performance, by = c("season_id", "team")) %>%
  mutate(points = points.x + points.y) %>%
  select(season_id, team, points) %>%
  group_by(season_id) %>%
  mutate(placement = rank(desc(points), ties.method = "random")) %>%
  inner_join(tsl.scoring, by = "placement")

performance.summary <-
  team.performance %>%
  group_by(team) %>%
  summarize(avg_points = mean(points),
            avg_tsl_points = mean(tsl_points))
