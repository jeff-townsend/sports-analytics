library(tidyverse)
library(nflreadr)
library(ggpmisc)
library(ggthemes)

# fantasy-relevant plays
nfl.pbp <-
  load_pbp(2014:2023) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass", "qb_kneel"))
# fantasy-relevant players
nfl.roster <-
  load_rosters(2014:2023) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id)) %>%
  mutate(full_name = ifelse(season >= 2016, full_name, paste(first_name, last_name))) %>%
  distinct(season, gsis_id, full_name, position)

# passing scoring
nfl.pbp.pass <-
  nfl.pbp %>%
  filter(play_type == "pass",
         two_point_attempt == 0) %>%
  mutate(fumble = ifelse(fumble == 0, 0, ifelse(passer_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, yards_gained, touchdown, interception, fumble, sack, passer_id) %>%
  inner_join(nfl.roster, by = c("passer_id" = "gsis_id", "season")) %>%
  rename(player_id = passer_id,
         player = full_name)

# rushing scoring
nfl.pbp.rush <-
  nfl.pbp %>%
  filter(play_type %in% c("run", "qb_kneel"),
         two_point_attempt == 0) %>%
  mutate(rusher_id = ifelse(qb_scramble == 1, passer_id, rusher_id),
         fumble = ifelse(fumble == 0, 0, ifelse(rusher_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, yards_gained, touchdown, fumble, rusher_id) %>%
  inner_join(nfl.roster, by = c("rusher_id" = "gsis_id", "season")) %>%
  rename(player_id = rusher_id,
         player = full_name)

# receiving scoring
nfl.pbp.rec <-
  nfl.pbp %>%
  filter(play_type == "pass",
         two_point_attempt == 0) %>%
  mutate(fumble = ifelse(fumble == 0, 0, ifelse(receiver_id == fumbled_1_player_id, 1, 0))) %>%
  select(season, play_id, game_id, week, posteam, defteam, complete_pass, yards_gained, touchdown, fumble, receiver_id) %>%
  inner_join(nfl.roster, by = c("receiver_id" = "gsis_id", "season")) %>%
  rename(player_id = receiver_id,
         player = full_name)


## WR split-half reliability

wr.odd.stats <-
  nfl.pbp.rec %>%
  filter(position == "WR",
         week%%2 == 1) %>%
  group_by(player_id, player, season) %>%
  summarize(gp = n_distinct(game_id),
            targets = n(),
            receptions = sum(complete_pass),
            yards = sum(yards_gained)) %>%
  ungroup() %>%
  mutate(ypg = yards / gp,
         tpg = targets / gp,
         rpg = receptions / gp,
         catch_rate = receptions / targets,
         ypt = yards / targets,
         ypr = yards / receptions) %>%
  filter(targets >= 32)

wr.even.stats <-
  nfl.pbp.rec %>%
  filter(position == "WR",
         week%%2 == 0) %>%
  group_by(player_id, player, season) %>%
  summarize(gp = n_distinct(game_id),
            targets = n(),
            receptions = sum(complete_pass),
            yards = sum(yards_gained)) %>%
  ungroup() %>%
  mutate(ypg = yards / gp,
         tpg = targets / gp,
         rpg = receptions / gp,
         catch_rate = receptions / targets,
         ypt = yards / targets,
         ypr = yards / receptions) %>%
  filter(targets >= 32)

wr.shr <-
  wr.odd.stats %>%
  inner_join(wr.even.stats, by = c("player_id", "player", "season")) %>%
  rename_with(.fn = ~ gsub(".x", "_odd", .x), fixed = TRUE) %>%
  rename_with(.fn = ~ gsub(".y", "_even", .x, fixed = TRUE))

cor(wr.shr$ypg_odd, wr.shr$ypg_even)^2
cor(wr.shr$tpg_odd, wr.shr$tpg_even)^2
cor(wr.shr$rpg_odd, wr.shr$rpg_even)^2
cor(wr.shr$catch_rate_odd, wr.shr$catch_rate_even)^2
cor(wr.shr$ypt_odd, wr.shr$ypt_even)^2
cor(wr.shr$ypr_odd, wr.shr$ypr_even)^2

ggplot(wr.shr, aes(x = ypt_odd, y = ypt_even)) +
  stat_poly_line() +
  stat_poly_eq(use_label("R2")) +
  geom_point() +
  theme_fivethirtyeight() +
  ggtitle("Split-Half Reliability: Yards per Target",
          subtitle = "Comparison between even- and odd-numbered weeks; data since 2014") +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 10)) +
  annotate("text", x = 13, y = 14, label = "Aiyuk's 2023 season", size = 3) +
  geom_segment(aes(x = 13, y = 13.75, xend = 12.75, yend = 13),
               linewidth = 0.5,
               color = "red",
               arrow = arrow(length = unit(0.15, "cm")))

ggplot(wr.shr, aes(x = tpg_odd, y = tpg_even)) +
  stat_poly_line() +
  stat_poly_eq(use_label("R2")) +
  geom_point() +
  theme_fivethirtyeight() +
  ggtitle("Split-Half Reliability: Targets per Game",
          subtitle = "Comparison between even- and odd-numbered weeks; data since 2014") +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 10))

wr.ypg <-
  rbind(wr.shr %>%
          mutate(split_pred = "Odd") %>%
          select(season, player, split_pred, ypg_odd,
                 tpg_even, rpg_even, catch_rate_even, ypt_even, ypr_even) %>%
          rename(ypg = ypg_odd,
                 tpg = tpg_even,
                 rpg = rpg_even,
                 catch_rate = catch_rate_even,
                 ypt = ypt_even,
                 ypr = ypr_even),
        wr.shr %>%
          mutate(split_pred = "Even") %>%
          select(season, player, split_pred, ypg_even,
                 tpg_odd, rpg_odd, catch_rate_odd, ypt_odd, ypr_odd) %>%
          rename(ypg = ypg_even,
                 tpg = tpg_odd,
                 rpg = rpg_odd,
                 catch_rate = catch_rate_odd,
                 ypt = ypt_odd,
                 ypr = ypr_odd))

ggplot(wr.ypg, aes(x = tpg, y = ypg)) +
  stat_poly_line() +
  stat_poly_eq(use_label("R2")) +
  geom_point() +
  theme_fivethirtyeight() +
  ggtitle("Predicting Yards per Game using Yards per Target",
          subtitle = "Comparison between even- and odd-numbered weeks; data since 2014") +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 10),
        axis.title.x = element_text(),
        axis.title.y = element_text()) +
  scale_x_continuous("Yards per Target") +
  scale_y_continuous("Out-of-Sample Yards per Game")

wr.ypg.mod <- lm(ypg ~ tpg + catch_rate + ypr, data = wr.ypg)
summary(wr.ypg.mod)

wr.stats.2023 <-
  nfl.pbp.rec %>%
  filter(position == "WR",
         season == 2023) %>%
  group_by(player_id, player) %>%
  summarize(gp = n_distinct(game_id),
            targets = n(),
            receptions = sum(complete_pass),
            yards = sum(yards_gained)) %>%
  ungroup() %>%
  mutate(ypg = yards / gp,
         tpg = targets / gp,
         rpg = receptions / gp,
         catch_rate = receptions / targets,
         ypt = yards / targets,
         ypr = yards / receptions) %>%
  filter(targets >= 64) %>%
  mutate(mod_ypg = -10.58 + 6.413*tpg + 14.75*catch_rate + 1.088*ypr)
