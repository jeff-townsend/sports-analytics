library(tidyverse)
library(ggthemes)

library(readxl)
epl.schedule.load <- read_excel("Data Analysis/Team Super League/2023-24 EPL Schedule.xlsx", 
                                col_types = c("numeric", "date", "text", "text", "numeric",
                                              "numeric", "numeric", "numeric"))
epl.team.strength <- read_excel("Data Analysis/Team Super League/2023-24 EPL Team Strength.xlsx")
tsl.teams <- read_excel("Data Analysis/Team Super League/2023-24 Teams.xlsx")
tsl.scoring <- data.frame(placement = c(1:20),
                          tsl_points = c(80, 60, 40, 40, 25, 20, 16, 14, 12, 10,
                                         8, 7, 6, 5, 4, 3, 2, 0, 0, 0))

epl.schedule <-
  epl.schedule.load %>%
  mutate(completed = ifelse(week <= 3, completed, 0)) %>%
  inner_join(epl.team.strength %>% select(team, win_rate), by = c("home" = "team")) %>%
  rename(home_win_rate = win_rate) %>%
  inner_join(epl.team.strength %>% select(team, win_rate), by = c("away" = "team")) %>%
  rename(away_win_rate = win_rate) %>%
  mutate(home_win_prob = ifelse(completed == 1, home_win,
                                (home_win_rate - home_win_rate * away_win_rate) /
                                  (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate) * .78),
         draw_prob = ifelse(completed == 1, draw, .22),
         away_win_prob = ifelse(completed == 1, away_win, .78 - home_win_prob))

set.seed(811)
simulations <- 25000
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

team.summary <-
  team.performance %>%
  inner_join(tsl.teams, by = "team") %>%
  group_by(team, owner, cost) %>%
  summarize(avg_points = mean(points),
            avg_tsl_points = mean(tsl_points),
            league_title = mean(ifelse(placement == 1, 1, 0)),
            top_4 = mean(ifelse(placement <= 4, 1, 0)),
            top_10 = mean(ifelse(placement <= 10, 1, 0)),
            relegation = mean(ifelse(placement >= 18, 1, 0)),
            avg_tsl_net = mean(tsl_points - cost))
View(team.summary %>% arrange(desc(avg_tsl_points)))

tsl.summary <-
  team.performance %>%
  inner_join(tsl.teams, by = "team") %>%
  group_by(season_id, owner) %>%
  summarize(tsl_points = sum(tsl_points),
            tsl_cost = sum(cost)) %>%
  ungroup() %>%
  mutate(tsl_net = tsl_points - tsl_cost)

tsl.averages <-
  tsl.summary %>%
  group_by(owner) %>%
  summarize(average = mean(tsl_net))

ggplot(tsl.summary, aes(x = tsl_net, fill = owner)) +
  geom_density(alpha = .75) +
  geom_vline(xintercept = tsl.averages$average[1], color = "#FF2700") +
  geom_vline(xintercept = tsl.averages$average[2], color = "#77AB43") +
  geom_vline(xintercept = tsl.averages$average[3], color = "#008FD5") +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  ggtitle("EPL TSL Performance - Matchweek 3")
tsl.averages
