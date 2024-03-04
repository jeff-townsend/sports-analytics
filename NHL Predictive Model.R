library(readr)
library(tidyverse)
library(ggpmisc)

teams.import <- read_csv("Data Analysis/NHL/teams_2008-2023.csv")

teams <-
  teams.import %>%
  filter(situation == "all",
         playoffGame == 0,
         season < 2023,
         season != 2012,
         season != 2020) %>%
  mutate(gd = goalsFor - goalsAgainst,
         result = ifelse(gd > 0, 1, ifelse(gd < 0, 0, 0.5)),
         sd = shotAttemptsFor - shotAttemptsAgainst,
         adj_sd = scoreAdjustedShotsAttemptsFor - scoreAdjustedShotsAttemptsAgainst,
         pdo = goalsFor / shotAttemptsFor + (1 - goalsAgainst / shotAttemptsAgainst)) %>%
  group_by(team, season) %>%
  mutate(game_number = rank(gameId)) %>%
  ungroup() %>%
  mutate(is_odd = ifelse(game_number%%2 == 1, 1, 0))

stats.total <-
  teams %>%
  group_by(season, team) %>%
  summarize(win_rate = mean(result),
            gd = mean(gd),
            sd = mean(sd),
            adj_sd = mean(adj_sd),
            pdo = mean(goalsFor) / mean(shotAttemptsFor) + (1 - mean(goalsAgainst) / mean(shotAttemptsAgainst)))

ggplot(teams, aes(x = adj_sd, y = gd)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

gd.wr.lm <- lm(win_rate ~ gd, data = stats.total)
summary(gd.wr.lm)

odd.stats <-
  teams %>%
  filter(is_odd == 1) %>%
  group_by(season, team) %>%
  summarize(odd_win_rate = mean(result),
            odd_gd = mean(gd),
            odd_gf = mean(goalsFor),
            odd_ga = mean(goalsAgainst),
            odd_sd = mean(sd),
            odd_sf = mean(shotAttemptsFor),
            odd_sa = mean(shotAttemptsAgainst),
            odd_adj_sd = mean(adj_sd)) %>%
  ungroup() %>%
  mutate(odd_shpct = odd_gf / odd_sf,
         odd_opp_shpct = odd_ga / odd_sa,
         odd_pdo = odd_shpct + (1 - odd_opp_shpct))

even.stats <-
  teams %>%
  filter(is_odd == 0) %>%
  group_by(season, team) %>%
  summarize(even_win_rate = mean(result),
            even_gd = mean(gd),
            even_gf = mean(goalsFor),
            even_ga = mean(goalsAgainst),
            even_sd = mean(sd),
            even_sf = mean(shotAttemptsFor),
            even_sa = mean(shotAttemptsAgainst),
            even_adj_sd = mean(adj_sd)) %>%
  ungroup() %>%
  mutate(even_shpct = even_gf / even_sf,
         even_opp_shpct = even_ga / even_sa,
         even_pdo = even_shpct + (1 - even_opp_shpct))

stats.shr <-
  odd.stats %>%
  inner_join(even.stats, by = c("season", "team")) %>%
  mutate(odd_xwin_rate = gd.wr.lm$coefficients[1] + gd.wr.lm$coefficients[2]*odd_gd,
         even_xwin_rate = gd.wr.lm$coefficients[1] + gd.wr.lm$coefficients[2]*even_gd,
         odd_win_rate_delta = odd_win_rate - odd_xwin_rate,
         even_win_rate_delta = even_win_rate - even_xwin_rate)

ggplot(stats.shr, aes(x = even_adj_sd, y = odd_adj_sd)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

stats.wr <-
  rbind(stats.shr %>%
          select(season, team, odd_win_rate, even_gd, even_sd, even_adj_sd, even_pdo) %>%
          mutate(split_pred = "Odd") %>%
          rename(win_rate = odd_win_rate,
                 gd = even_gd,
                 sd = even_sd,
                 adj_sd = even_adj_sd,
                 pdo = even_pdo),
        stats.shr %>%
          select(season, team, even_win_rate, odd_gd, odd_sd, odd_adj_sd, odd_pdo) %>%
          mutate(split_pred = "Even") %>%
          rename(win_rate = even_win_rate,
                 gd = odd_gd,
                 sd = odd_sd,
                 adj_sd = odd_adj_sd,
                 pdo = odd_pdo))

ggplot(stats.wr, aes(x = pdo, y = win_rate)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

summary(lm(win_rate ~ sd + pdo, data = stats.wr))
