library(tidyverse)
library(ggthemes)
library(readr)

tsl.scoring <- data.frame(placement = c(1:20),
                          tsl_points = c(80, 60, 40, 40, 25, 20, 16, 14, 12, 10, 8, 7, 6, 5, 4, 3, 2, 0, 0, 0))

games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_schedule_2425.csv",
                         col_types = cols(date = col_date(format = "%m/%d/%y")))

teams.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_team_strength_2425.csv")

teams <-
  games.import %>%
  distinct(home) %>%
  rename(fbref_team = home) %>%
  arrange(fbref_team)

teams$tsl_team <- teams.import$team
teams$gd <- teams.import$gd

set.seed(816)
s <- 10000

team.seasons <- data.frame(id = c(1:(nrow(teams)*s)),
                           season_id = rep(c(1:s), each = nrow(teams)),
                           team = teams$fbref_team,
                           team_gd = teams$gd,
                           season_gd = rnorm(nrow(teams)*s,
                                             mean = teams$gd,
                                             sd = 0.28))
team.seasons$season_rgf = with(team.seasons, 0.5*season_gd)
team.seasons$season_rga = with(team.seasons, 0.5*season_gd)

hfa <- 0.3
games <- data.frame(id = c(1:(nrow(games.import)*s)),
                    season_id = rep(c(1:s), each = nrow(games.import)),
                    game_id = c(1:(nrow(games.import))),
                    home_team = games.import$home,
                    away_team = games.import$away)

simulations <-
  games %>%
  inner_join(team.seasons %>% select(season_id, team, season_rgf, season_rga),
             by = c("season_id", "home_team" = "team")) %>%
  rename(home_rgf = season_rgf,
         home_rga = season_rga) %>%
  inner_join(team.seasons %>% select(season_id, team, season_rgf, season_rga),
             by = c("season_id", "away_team" = "team")) %>%
  rename(away_rgf = season_rgf,
         away_rga = season_rga) %>%
  mutate(home_lambda = home_rgf - away_rga + 1.5 + 0.5*hfa,
         away_lambda = away_rgf - home_rga + 1.5 - 0.5*hfa) %>%
  mutate(home_goals = rpois(nrow(games), lambda = ifelse(home_lambda < 0, 0, home_lambda)),
         away_goals = rpois(nrow(games), lambda = ifelse(away_lambda < 0, 0, away_lambda)),
         home_win = ifelse(home_goals > away_goals, 1, 0),
         away_win = ifelse(away_goals > home_goals, 1, 0),
         draw = ifelse(home_goals == away_goals, 1, 0),
         home_points = 3*home_win + draw,
         away_points = 3*away_win + draw)

performance <-
  rbind(simulations %>%
        group_by(home_team, season_id) %>%
        summarize(matches = n(),
                  wins = sum(home_win),
                  gf = sum(home_goals),
                  ga = sum(away_goals),
                  draws = sum(draw),
                  losses = sum(away_win),
                  points = sum(home_points)) %>%
        ungroup() %>%
        rename(team = home_team),
        simulations %>%
        group_by(away_team, season_id) %>%
        summarize(matches = n(),
                  wins = sum(away_win),
                  gf = sum(away_goals),
                  ga = sum(home_goals),
                  draws = sum(draw),
                  losses = sum(home_win),
                  points = sum(away_points)) %>%
        ungroup() %>%
        rename(team = away_team))

tables <-
  performance %>%
  group_by(team, season_id) %>%
  summarize(matches = sum(matches),
            wins = sum(wins),
            draws = sum(draws),
            losses = sum(losses),
            points = sum(points),
            gf = sum(gf),
            ga = sum(ga),) %>%
  ungroup() %>%
  group_by(season_id) %>%
  mutate(placement = rank(desc(points), ties.method = "random"),
         title = ifelse(placement == 1, 1, 0),
         top4 = ifelse(placement <= 4, 1, 0),
         top10 = ifelse(placement <= 10, 1, 0),
         relegation = ifelse(placement >= 18, 1, 0))

summary <-
  tables %>%
  inner_join(tsl.scoring, by = "placement") %>%
  group_by(team) %>%
  summarize(tsl_points = mean(tsl_points),
            points = mean(points),
            wins = mean(wins),
            draws = mean(draws),
            losses = mean(losses),
            gd = mean(gf - ga) / 38,
            title = mean(title),
            top4 = mean(top4),
            top10 = mean(top10),
            relegation = mean(relegation))

placements <-
  tables %>%
  group_by(team, placement) %>%
  summarize(frequency = n()) %>%
  ungroup() %>%
  mutate(proportion = frequency / s)

# win.probs <-
#   game.simulations %>%
#   group_by(game_id) %>%
#   summarize(home_lambda = mean(home_lambda),
#             away_lambda = mean(away_lambda),
#             lambda_diff = mean(home_lambda - away_lambda),
#             win_prob = mean(home_win),
#             draw_prob = mean(draw),
#             loss_prob = mean(away_win)) %>%
#   ungroup() %>%
#   group_by(lambda_diff) %>%
#   summarize(win_prob = mean(win_prob),
#             draw_prob = mean(draw_prob),
#             loss_prob = mean(loss_prob)) %>%
#   ungroup() %>%
#   mutate(exp_score = win_prob + 0.5*draw_prob)
# 
# ggplot(win.probs, aes(x = lambda_diff, y = exp_score)) +
#   geom_line()
# summary(lm(lambda_diff ~ exp_score, data = win.probs))
