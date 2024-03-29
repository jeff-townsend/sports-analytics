library(readr)
library(tidyverse)
library(ggpmisc)
library(ggthemes)

file.location <- "Data Analysis/NHL/"

team.games.import <- read_csv(paste0(file.location,"teams_2008-2023.csv"))

team.games <-
  team.games.import %>%
  filter(playoffGame == 0) %>%
  select(team, season, gameId, situation, goalsFor, goalsAgainst,
         shotAttemptsFor, shotAttemptsAgainst, scoreAdjustedShotsAttemptsFor, scoreAdjustedShotsAttemptsAgainst,
         xGoalsFor, xGoalsAgainst) %>%
  rename(game_id = gameId,
         gf = goalsFor,
         ga = goalsAgainst,
         sf = shotAttemptsFor,
         sa = shotAttemptsAgainst,
         adj_sf = scoreAdjustedShotsAttemptsFor,
         adj_sa = scoreAdjustedShotsAttemptsAgainst,
         xgf = xGoalsFor,
         xga = xGoalsAgainst) %>%
  mutate(gd = gf - ga,
         sd = sf - sa,
         adj_sd = adj_sf - adj_sa,
         xgd = xgf - xga,
         pdo = gf / sf + (1 - ga / sa))

team.games.wide <-
  team.games %>%
  filter(situation == "all") %>%
  select(-situation) %>%
  rename_with(.fn = ~ paste(.x, "all", sep = "_")) %>%
  rename(team = team_all,
         season = season_all,
         game_id = game_id_all) %>%
  inner_join(team.games %>%
               filter(situation == "5on5") %>%
               select(-situation) %>%
               rename_with(.fn = ~ paste(.x, "5on5", sep = "_")) %>%
               rename(team = team_5on5,
                      season = season_5on5,
                      game_id = game_id_5on5),
             by = c("team", "season", "game_id")) %>%
  mutate(result = ifelse(gd_all > 0, 1, ifelse(gd_all < 0, 0, 0.5))) %>%
  group_by(team, season) %>%
  mutate(game_number = rank(game_id)) %>%
  ungroup() %>%
  mutate(is_odd = ifelse(game_number%%2 == 1, 1, 0))

stats.total <-
  team.games.wide %>%
  filter(season < 2023,
         season != 2012,
         season != 2019,
         season != 2020) %>%
  group_by(season, team) %>%
  summarize(win_rate = mean(result),
            gd_all = mean(gd_all),
            sd_all = mean(sd_all),
            adj_sd_all = mean(adj_sd_all),
            xgd_all = mean(xgd_all),
            pdo_all = mean(gf_all) / mean(sf_all) + (1 - mean(ga_all) / mean(sa_all)),
            gd_5on5 = mean(gd_5on5),
            sd_5on5 = mean(sd_5on5),
            adj_sd_5on5 = mean(adj_sd_5on5),
            xgd_5on5 = mean(xgd_5on5),
            pdo_5on5 = mean(gf_5on5) / mean(sf_5on5) + (1 - mean(ga_5on5) / mean(sa_5on5)))

# ggplot(stats.total, aes(x = gd_all, y = win_rate)) +
#   stat_poly_line() +
#   stat_poly_eq(use_label(c("eq", "R2"))) +
#   geom_point()

gd.wr.lm <- lm(win_rate ~ gd_all, data = stats.total)
summary(gd.wr.lm)

odd.stats <-
  team.games.wide %>%
  filter(season < 2023,
         season != 2012,
         season != 2019,
         season != 2020,
         is_odd == 1) %>%
  group_by(season, team) %>%
  summarize(odd_win_rate = mean(result),
            odd_gd_all = mean(gd_all),
            odd_gf_all = mean(gf_all),
            odd_ga_all = mean(ga_all),
            odd_sd_all = mean(sd_all),
            odd_sf_all = mean(sf_all),
            odd_sa_all = mean(sa_all),
            odd_adj_sd_all = mean(adj_sd_all),
            odd_xgf_all = mean(xgf_all),
            odd_xga_all = mean(xga_all),
            odd_xgd_all = mean(xgd_all),
            odd_shpct_all = mean(gf_all) / mean(sf_all),
            odd_opp_shpct_all = mean(ga_all) / mean(sa_all),
            odd_pdo_all = mean(gf_all) / mean(sf_all) + (1 - mean(ga_all) / mean(sa_all)),
            odd_xshpct_all = mean(xgf_all) / mean(sf_all),
            odd_opp_xshpct_all = mean(xga_all) / mean(sa_all),
            odd_xpdo_all = mean(xgf_all) / mean(sf_all) + (1 - mean(xga_all) / mean(sa_all)),
            odd_gfoe_all = (mean(gf_all) - mean(xgf_all)) / mean(sf_all),
            odd_gaoe_all = (mean(ga_all) - mean(xga_all)) / mean(sa_all),
            odd_gdoe_all = (mean(gf_all) - mean(xgf_all)) / mean(sf_all) +
                            (mean(ga_all) - mean(xga_all)) / mean(sa_all),
            odd_gd_5on5 = mean(gd_5on5),
            odd_gf_5on5 = mean(gf_5on5),
            odd_ga_5on5 = mean(ga_5on5),
            odd_sd_5on5 = mean(sd_5on5),
            odd_sf_5on5 = mean(sf_5on5),
            odd_sa_5on5 = mean(sa_5on5),
            odd_adj_sd_5on5 = mean(adj_sd_5on5),
            odd_xgf_5on5 = mean(xgf_5on5),
            odd_xga_5on5 = mean(xga_5on5),
            odd_xgd_5on5 = mean(xgd_5on5),
            odd_shpct_5on5 = mean(gf_5on5) / mean(sf_5on5),
            odd_opp_shpct_5on5 = mean(ga_5on5) / mean(sa_5on5),
            odd_pdo_5on5 = mean(gf_5on5) / mean(sf_5on5) + (1 - mean(ga_5on5) / mean(sa_5on5)),
            odd_xshpct_5on5 = mean(xgf_5on5) / mean(sf_5on5),
            odd_opp_xshpct_5on5 = mean(xga_5on5) / mean(sa_5on5),
            odd_xpdo_5on5 = mean(xgf_5on5) / mean(sf_5on5) + (1 - mean(xga_5on5) / mean(sa_5on5)),
            odd_gfoe_5on5 = (mean(gf_5on5) - mean(xgf_5on5)) / mean(sf_5on5),
            odd_gaoe_5on5 = (mean(ga_5on5) - mean(xga_5on5)) / mean(sa_5on5),
            odd_gdoe_5on5 = (mean(gf_5on5) - mean(xgf_5on5)) / mean(sf_5on5) +
              (mean(ga_5on5) - mean(xga_5on5)) / mean(sa_5on5))

even.stats <-
  team.games.wide %>%
  filter(season < 2023,
         season != 2012,
         season != 2019,
         season != 2020,
         is_odd == 0) %>%
  group_by(season, team) %>%
  summarize(even_win_rate = mean(result),
            even_gd_all = mean(gd_all),
            even_gf_all = mean(gf_all),
            even_ga_all = mean(ga_all),
            even_sd_all = mean(sd_all),
            even_sf_all = mean(sf_all),
            even_sa_all = mean(sa_all),
            even_adj_sd_all = mean(adj_sd_all),
            even_xgf_all = mean(xgf_all),
            even_xga_all = mean(xga_all),
            even_xgd_all = mean(xgd_all),
            even_shpct_all = mean(gf_all) / mean(sf_all),
            even_opp_shpct_all = mean(ga_all) / mean(sa_all),
            even_pdo_all = mean(gf_all) / mean(sf_all) + (1 - mean(ga_all) / mean(sa_all)),
            even_xshpct_all = mean(xgf_all) / mean(sf_all),
            even_opp_xshpct_all = mean(xga_all) / mean(sa_all),
            even_xpdo_all = mean(xgf_all) / mean(sf_all) + (1 - mean(xga_all) / mean(sa_all)),
            even_gfoe_all = (mean(gf_all) - mean(xgf_all)) / mean(sf_all),
            even_gaoe_all = (mean(ga_all) - mean(xga_all)) / mean(sa_all),
            even_gdoe_all = (mean(gf_all) - mean(xgf_all)) / mean(sf_all) +
              (mean(ga_all) - mean(xga_all)) / mean(sa_all),
            even_gd_5on5 = mean(gd_5on5),
            even_gf_5on5 = mean(gf_5on5),
            even_ga_5on5 = mean(ga_5on5),
            even_sd_5on5 = mean(sd_5on5),
            even_sf_5on5 = mean(sf_5on5),
            even_sa_5on5 = mean(sa_5on5),
            even_adj_sd_5on5 = mean(adj_sd_5on5),
            even_xgf_5on5 = mean(xgf_5on5),
            even_xga_5on5 = mean(xga_5on5),
            even_xgd_5on5 = mean(xgd_5on5),
            even_shpct_5on5 = mean(gf_5on5) / mean(sf_5on5),
            even_opp_shpct_5on5 = mean(ga_5on5) / mean(sa_5on5),
            even_pdo_5on5 = mean(gf_5on5) / mean(sf_5on5) + (1 - mean(ga_5on5) / mean(sa_5on5)),
            even_xshpct_5on5 = mean(xgf_5on5) / mean(sf_5on5),
            even_opp_xshpct_5on5 = mean(xga_5on5) / mean(sa_5on5),
            even_xpdo_5on5 = mean(xgf_5on5) / mean(sf_5on5) + (1 - mean(xga_5on5) / mean(sa_5on5)),
            even_gfoe_5on5 = (mean(gf_5on5) - mean(xgf_5on5)) / mean(sf_5on5),
            even_gaoe_5on5 = (mean(ga_5on5) - mean(xga_5on5)) / mean(sa_5on5),
            even_gdoe_5on5 = (mean(gf_5on5) - mean(xgf_5on5)) / mean(sf_5on5) +
              (mean(ga_5on5) - mean(xga_5on5)) / mean(sa_5on5))

stats.shr <-
  odd.stats %>%
  inner_join(even.stats, by = c("season", "team")) %>%
  mutate(odd_xwin_rate = gd.wr.lm$coefficients[1] + gd.wr.lm$coefficients[2]*odd_gd_all,
         even_xwin_rate = gd.wr.lm$coefficients[1] + gd.wr.lm$coefficients[2]*even_gd_all,
         odd_win_rate_delta = odd_win_rate - odd_xwin_rate,
         even_win_rate_delta = even_win_rate - even_xwin_rate)

ggplot(stats.shr, aes(x = odd_xpdo_5on5, y = even_xpdo_5on5)) +
  stat_poly_line() +
  stat_poly_eq(use_label("R2")) +
  geom_point()

stats.wr <-
  rbind(stats.shr %>%
          mutate(split_pred = "Odd") %>%
          select(season, team, split_pred, odd_win_rate,
                 even_gd_all, even_sd_all, even_adj_sd_all, even_pdo_all, even_xgd_all,
                 even_gd_5on5, even_sd_5on5, even_adj_sd_5on5, even_pdo_5on5, even_xgd_5on5) %>%
          rename(win_rate = odd_win_rate,
                 gd_all = even_gd_all,
                 sd_all = even_sd_all,
                 adj_sd_all = even_adj_sd_all,
                 pdo_all = even_pdo_all,
                 xgd_all = even_xgd_all,
                 gd_5on5 = even_gd_5on5,
                 sd_5on5 = even_sd_5on5,
                 adj_sd_5on5 = even_adj_sd_5on5,
                 pdo_5on5 = even_pdo_5on5,
                 xgd_5on5 = even_xgd_5on5),
        stats.shr %>%
          mutate(split_pred = "Even") %>%
          select(season, team, split_pred, even_win_rate,
                 odd_gd_all, odd_sd_all, odd_adj_sd_all, odd_pdo_all, odd_xgd_all,
                 odd_gd_5on5, odd_sd_5on5, odd_adj_sd_5on5, odd_pdo_5on5, odd_xgd_5on5) %>%
          rename(win_rate = even_win_rate,
                 gd_all = odd_gd_all,
                 sd_all = odd_sd_all,
                 adj_sd_all = odd_adj_sd_all,
                 pdo_all = odd_pdo_all,
                 xgd_all = odd_xgd_all,
                 gd_5on5 = odd_gd_5on5,
                 sd_5on5 = odd_sd_5on5,
                 adj_sd_5on5 = odd_adj_sd_5on5,
                 pdo_5on5 = odd_pdo_5on5,
                 xgd_5on5 = odd_xgd_5on5)
  ) %>%
  mutate(gd_other = gd_all - gd_5on5,
         sd_other = sd_all - sd_5on5,
         adj_sd_other = adj_sd_all - adj_sd_5on5,
         xgd_other = xgd_all - xgd_5on5)

ggplot(stats.wr, aes(x = adj_sd_all, y = win_rate)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

wr.mod.all <- lm(win_rate ~ adj_sd_all + pdo_all, data = stats.wr)
wr.mod.5on5 <- lm(win_rate ~ adj_sd_5on5 + pdo_5on5 + gd_other, data = stats.wr)

stats.total.2023 <-
  team.games.wide %>%
  filter(season == 2023) %>%
  group_by(season, team) %>%
  summarize(win_rate = mean(result),
            gd_all = mean(gd_all),
            sd_all = mean(sd_all),
            adj_sd_all = mean(adj_sd_all),
            pdo_all = mean(gf_all) / mean(sf_all) + (1 - mean(ga_all) / mean(sa_all)),
            gd_5on5 = mean(gd_5on5),
            sd_5on5 = mean(sd_5on5),
            adj_sd_5on5 = mean(adj_sd_5on5),
            pdo_5on5 = mean(gf_5on5) / mean(sf_5on5) + (1 - mean(ga_5on5) / mean(sa_5on5)))

team.ratings <-
  team.games.wide %>%
  filter(season == 2023) %>%
  group_by(team) %>%
  mutate(game_number_rev = rank(desc(game_id))) %>%
  filter(game_number_rev <= 41) %>%
  summarize(win_rate = mean(result),
            gd_all = mean(gd_all),
            adj_sd_all = mean(adj_sd_all),
            pdo_all = mean(gf_all) / mean(sf_all) + (1 - mean(ga_all) / mean(sa_all)),
            gd_5on5 = mean(gd_5on5),
            adj_sd_5on5 = mean(adj_sd_5on5),
            pdo_5on5 = mean(gf_5on5) / mean(sf_5on5) + (1 - mean(ga_5on5) / mean(sa_5on5)),
            gd_other = mean(gd_all) - mean(gd_5on5)) %>%
  ungroup() %>%
  mutate(rating_all = wr.mod.all$coefficients[1] + wr.mod.all$coefficients[2]*adj_sd_all +
           wr.mod.all$coefficients[3]*pdo_all,
         rating_5on5 = wr.mod.5on5$coefficients[1] + wr.mod.5on5$coefficients[2]*adj_sd_5on5 +
           wr.mod.5on5$coefficients[3]*pdo_5on5 + wr.mod.5on5$coefficients[4]*gd_other)

## create google sheet output
write_csv(team.ratings %>% select(team, adj_sd_5on5, pdo_5on5, gd_other, rating_5on5),
          paste0(file.location,"nhl_ratings.csv"))

ggplot(stats.shr, aes(x = even_adj_sd_5on5, y = odd_adj_sd_5on5)) +
  stat_poly_line() +
  stat_poly_eq(use_label("R2")) +
  geom_point() +
  theme_fivethirtyeight() +
  ggtitle("Split-Half Reliability: 5-on-5 Score-Adjusted Shot Differential",
          subtitle = "Comparison between even- and odd-numbered games; data since 08-09, only including team seasons with 82 games") +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 10)) +
  annotate("text", x = 5, y = 19, label = "Chicago's 09-10 season", size = 3) +
  geom_segment(aes(x = 8, y = 18, xend = 9.5, yend = 16),
               linewidth = 0.5,
               color = "red",
               arrow = arrow(length = unit(0.15, "cm")))
               
