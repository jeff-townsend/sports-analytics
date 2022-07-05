library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)


seasons <- 2011:2021 ## data pre-2011 has different player IDs, which makes it difficult to use
nfl.pbp.load <- purrr::map_dfr(seasons, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})

nfl.rosters.load <- fast_scraper_roster(seasons)

set.seed(926)
nfl.pbp <-
  nfl.pbp.load %>%
  mutate(quarter_minutes_remaining = round(quarter_seconds_remaining / 60, 0),
         rng = runif(dim(nfl.pbp.load)[1])) %>%
  filter(season_type == "REG",
         play_type %in% c("pass", "run")) %>%
  mutate(down = as.factor(down),
         qtr = as.factor(qtr))

# train.pbp <-
#   nfl.pbp %>%
#   filter(rng <= .9)
# 
# test.pbp <-
#   nfl.pbp %>%
#   filter(rng > .9)

## wr aggregates by season
wr.agg <-
  nfl.pbp %>%
  filter(!is.na(receiver_player_id)) %>%
  mutate(completed_air_yards = air_yards * complete_pass) %>%
  inner_join(nfl.rosters.load, by = c("receiver_player_id" = "gsis_id", "season")) %>%
  filter(position == "WR") %>%
  group_by(season, receiver_player_id, full_name) %>%
  summarize(gp = n_distinct(game_id),
            targets = n(),
            catches = sum(complete_pass),
            yards = sum(yards_gained),
            air_yards = sum(air_yards, na.rm = TRUE),
            completed_air_yards = sum(completed_air_yards, na.rm = TRUE),
            yards_after_catch = sum(yards_after_catch, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avg_yards = yards / gp,
         pct_yac = yards_after_catch / (completed_air_yards + yards_after_catch)) %>%
  rename(player_name = full_name)

          # season.agg <-
          #   wr.agg %>%
          #   group_by(season) %>%
          #   summarize(yards = sum(yards),
          #             completed_air_yards = sum(completed_air_yards),
          #             yards_after_catch = sum(yards_after_catch)) %>%
          #   mutate(pct_air = completed_air_yards / (completed_air_yards + yards_after_catch),
          #          pct_yac = yards_after_catch / (completed_air_yards + yards_after_catch))

norm.variable <- wr.agg %>% filter(targets >= 48, gp >= 8) %>% summarize(mean_yards = mean(avg_yards),
                                                                         sd_yards = sd(avg_yards),
                                                                         mean_pct_yac = mean(pct_yac),
                                                                         sd_pct_yac = sd(pct_yac))

wr.agg2 <-
  wr.agg %>%
  filter(targets >= 48,
         gp >= 8) %>%
  mutate(yards_norm = (avg_yards - norm.variable$mean_yards) / norm.variable$sd_yards,
         pct_yac_norm = (pct_yac - norm.variable$mean_pct_yac) / norm.variable$sd_pct_yac)

chase.2021 <- wr.agg2 %>% filter(player_name == "Ja'Marr Chase")
chase.comps <-
  wr.agg2 %>%
  filter(player_name != "Ja'Marr Chase") %>%
  mutate(chase_yards_norm = chase.2021$yards_norm,
         chase_pct_yac_norm = chase.2021$pct_yac_norm,
         chase_similarity = sqrt((yards_norm - chase_yards_norm)^2 + (pct_yac_norm - chase_pct_yac_norm)^2)) %>%
  filter(chase_similarity <= .5)
chase.comps.next <-
  chase.comps %>%
  filter(season != 2021) %>%
  mutate(next_season = season + 1) %>%
  inner_join(wr.agg2, by = c("receiver_player_id", "player_name", "next_season" = "season")) %>%
  select(next_season, receiver_player_id, player_name, avg_yards.y, pct_yac.y) %>%
  rename(season = next_season,
         avg_yards = avg_yards.y,
         pct_yac = pct_yac.y)
          
          ggplot(data = wr.agg2, aes(x = avg_yards, y = pct_yac)) +
            geom_point() +
            geom_point(data = chase.2021,
                       aes(x = avg_yards, y = pct_yac),
                       color = "green",
                       size = 3) +
            geom_point(data = chase.comps,
                       aes(x = avg_yards, y = pct_yac),
                       color = "blue",
                       size = 3) +
            geom_point(data = chase.comps.next,
                       aes(x = avg_yards, y = pct_yac),
                       color = "red",
                       size = 3) +
            geom_smooth(method = "lm") +
            theme_fivethirtyeight() +
            ggtitle("What % of a WR's yards are after the catch?") +
            theme(axis.title = element_text()) +
            xlab("Receiving Yards per Game") +
            ylab("YAC Percentage")
          