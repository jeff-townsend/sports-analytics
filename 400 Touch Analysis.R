library(tidyverse)
library(nflreadr)

nfl.pbp <- load_pbp(1999:2021) %>% filter(season_type == "REG") # data goes back to 1999
nfl.roster <- load_rosters(1999:2021)
nfl.roster <-
  nfl.roster %>%
  mutate(full_name = ifelse(season >= 2016, full_name, paste(first_name, last_name)))

rush.agg <-
  nfl.pbp %>%
  filter(!is.na(rusher_player_id)) %>%
  group_by(rusher_player_id, season) %>%
  summarize(games = n_distinct(game_id),
            carries = sum(rush_attempt),
            rush_yards = sum(rushing_yards, na.rm = TRUE),
            rush_tds = sum(rush_touchdown)) %>%
  rename(player_id = rusher_player_id) #%>%
  #inner_join(nfl.roster, by = c("player_id" = "gsis_id", "season")) %>%
  #select(player_id, full_name, season, years_exp, games, carries, rush_yards, rush_tds)

rush.yoy <-
  rush.agg %>%
  inner_join(rush.agg %>% mutate(last_season = season - 1), by = c("player_id","season" = "last_season")) %>%
  select(player_id, season,
         games.x, carries.x, rush_yards.x, rush_tds.x,
         season.y, games.y, carries.y, rush_yards.y, rush_tds.y) %>%
  rename(#full_name = full_name.x,
         #years_exp = years_exp.x,
         season_y1 = season,
         games_y1 = games.x,
         carries_y1 = carries.x,
         rush_yards_y1 = rush_yards.x,
         rush_tds_y1 = rush_tds.x,
         season_y2 = season.y,
         games_y2 = games.y,
         carries_y2 = carries.y,
         rush_yards_y2 = rush_yards.y,
         rush_tds_y2 = rush_tds.y) %>%
  filter(carries_y1 >= 175,
         season_y2 <= 2021) %>%
  mutate(carries_bucket = ifelse(carries_y1 >= 350, "350+ Carries",
                                 ifelse(carries_y1 >= 325, "325-349 Carries",
                                        ifelse(carries_y1 >= 300, "300-324 Carries",
                                               ifelse(carries_y1 >= 275, "275-299 Carries",
                                                      ifelse(carries_y1 >= 250, "250-274 Carries",
                                                             ifelse(carries_y1 >= 225, "225-249 Carries", "175-224 Carries")))))))

### workload analysis
carry.buckets <-
  rush.yoy %>%
  group_by(carries_bucket) %>%
  summarize(players = n(),
            games_y1 = mean(games_y1),
            carries_y1 = mean(carries_y1),
            rush_yards_y1 = mean(rush_yards_y1),
            games_y2 = mean(games_y2),
            carries_y2 = mean(carries_y2),
            rush_yards_y2 = mean(rush_yards_y2)) %>%
  ungroup() %>%
  mutate(avg_carries_y1 = carries_y1 / games_y1,
         avg_carries_y2 = carries_y2 / games_y2,
         ypc_y1 = rush_yards_y1 / carries_y1,
         ypc_y2 = rush_yards_y2 / carries_y2)

### ypc progression
ypc.yoy <-
  rush.yoy %>%
  filter(carries_y1 >= 200) %>%
  mutate(ypc_y1 = rush_yards_y1 / carries_y1,
         ypc_bucket = ifelse(ypc_y1 >= 5.25, "5.25+ YPC",
                             ifelse(ypc_y1 >= 5, "5.0-5.24 YPC",
                                    ifelse(ypc_y1 >= 4.75, "4.75-4.99 YPC", "4.5-4.74 YPC")))) %>%
  filter(ypc_y1 >= 4.5) %>%
  group_by(ypc_bucket) %>%
  summarize(players = n(),
            games_y1 = mean(games_y1),
            carries_y1 = mean(carries_y1),
            rush_yards_y1 = mean(rush_yards_y1),
            games_y2 = mean(games_y2),
            carries_y2 = mean(carries_y2),
            rush_yards_y2 = mean(rush_yards_y2)) %>%
  ungroup() %>%
  mutate(avg_carries_y1 = carries_y1 / games_y1,
         avg_carries_y2 = carries_y2 / games_y2,
         ypc_y1 = rush_yards_y1 / carries_y1,
         ypc_y2 = rush_yards_y2 / carries_y2)

### td rate progression
td.yoy <-
  rush.yoy %>%
  filter(carries_y1 >= 200) %>%
  mutate(carries_per_td = carries_y1 / rush_tds_y1,
         tds_bucket = ifelse(carries_per_td < 20, "20- Carries/TD",
                             ifelse(carries_per_td <= 25, "20-25 Carries/TD",
                                    ifelse(carries_per_td <= 30, "25-30 Carries/TD",
                                           ifelse(carries_per_td <= 35, "30-35 Carries/TD", "35+ Carries/TD"))))) %>%
  group_by(tds_bucket) %>%
  summarize(players = n(),
            games_y1 = mean(games_y1),
            carries_y1 = mean(carries_y1),
            rush_tds_y1 = mean(rush_tds_y1),
            games_y2 = mean(games_y2),
            carries_y2 = mean(carries_y2),
            rush_tds_y2 = mean(rush_tds_y2)) %>%
  ungroup() %>%
  mutate(avg_carries_y1 = carries_y1 / games_y1,
         avg_carries_y2 = carries_y2 / games_y2,
         carries_per_td_y1 = carries_y1 / rush_tds_y1,
         carries_per_td_y2 = carries_y2 / rush_tds_y2)

View(  rush.yoy %>%
         filter(carries_y1 >= 200) %>%
         group_by(season_y1) %>%
         summarize(carries_per_td = mean(carries_y1) / mean(rush_tds_y1)))
