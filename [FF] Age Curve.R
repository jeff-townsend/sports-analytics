library(nflreadr)
library(tidyverse)

start.season <- 2013

nfl.pbp.load <- load_pbp(start.season:2022)
nfl.roster.load <- load_rosters(start.season:2022)

start.draft <- as.numeric(min(nfl.roster.load$entry_year, na.rm = TRUE))
nfl.draft.load <- load_draft_picks(start.draft:2022)

undrafted.pick <- max(nfl.draft.load$pick) + 1

# add approximate age to the roster table
# restrict to fantasy-relevant positions
nfl.roster <-
  nfl.roster.load %>%
  filter(position %in% c("QB", "RB", "WR", "TE"),
         !is.na(gsis_id)) %>%
  left_join(nfl.draft.load %>% select(gsis_id, pick), by = "gsis_id") %>%
  rename(draft_pick = pick) %>%
  mutate(age = season - as.numeric(substr(birth_date, 0, 4)),
         draft_pick = ifelse(is.na(draft_pick), undrafted.pick, draft_pick))

# isolate fantasy-relevant plays
nfl.pbp.rush <-
  nfl.pbp.load %>%
  filter(play_type == "run",
         season_type == "REG") %>%
  mutate(rusher_id = ifelse(qb_scramble == 1, passer_id, rusher_id)) %>%
  select(season, play_id, game_id, week, posteam, desc, play_type, yards_gained, rusher_id) %>%
  inner_join(nfl.roster %>% select(season, full_name, position, age, team, draft_pick, gsis_id),
             by = c("rusher_id" = "gsis_id", "season"))

nfl.pbp.rec <-
  nfl.pbp.load %>%
  filter(play_type == "pass",
         season_type == "REG") %>%
  select(season, play_id, game_id, week, posteam, desc, play_type, yards_gained, receiver_id) %>%
  inner_join(nfl.roster %>% select(season, full_name, position, age, team, draft_pick, gsis_id),
             by = c("receiver_id" = "gsis_id", "season"))

### compile positional stats
# RBs
rb.rushing <-
  nfl.pbp.rush %>%
  filter(position == "RB") %>%
  rename(player_id = rusher_id) %>%
  group_by(season, player_id, full_name, age, team, draft_pick) %>%
  summarize(gp = n_distinct(game_id),
            carries = n(),
            yards = sum(yards_gained)) %>%
  ungroup() %>%
  mutate(ypg = yards / gp,
         cpg = carries / gp,
         ypc = yards / carries) %>%
  filter(ypg > 0)

rb.receiving <-
  nfl.pbp.rec %>%
  filter(position == "RB") %>%
  rename(player_id = receiver_id) %>%
  group_by(season, player_id, full_name, age, team, draft_pick) %>%
  summarize(gp = n_distinct(game_id),
            targets = n(),
            yards = sum(yards_gained)) %>%
  ungroup() %>%
  mutate(ypg = yards / gp,
         tpg = targets / gp,
         ypt = yards / targets) %>%
  filter(ypg > 0)

# WRs
wr.receiving <-
  nfl.pbp.rec %>%
  filter(position == "WR") %>%
  rename(player_id = receiver_id) %>%
  group_by(season, player_id, full_name, age, team, draft_pick) %>%
  summarize(gp = n_distinct(game_id),
            targets = n(),
            yards = sum(yards_gained)) %>%
  ungroup() %>%
  mutate(ypg = yards / gp,
         tpg = targets / gp,
         ypt = yards / targets) %>%
  filter(ypg > 0)

### rb rushing progression
rb.y1 <-
  rb.rushing %>%
  filter(season < 2022)

rb.y2 <-
  rb.rushing %>%
  filter(season > 2013)

# yoy comparisons
rb.progression <-
  rb.y1 %>%
  select(season, player_id, full_name, draft_pick, age, ypg, cpg, ypc) %>%
  mutate(next_season = season + 1) %>%
  inner_join(rb.y2 %>% select(season, player_id, full_name, draft_pick, age, ypg, cpg, ypc),
             by = c("player_id", "full_name", "next_season" = "season", "draft_pick")) %>%
  select(-season, -next_season) %>%
  rename(age_y1 = age.x,
         ypg_y1 = ypg.x,
         cpg_y1 = cpg.x,
         ypc_y1 = ypc.x,
         age_y2 = age.y,
         ypg_y2 = ypg.y,
         cpg_y2 = cpg.y,
         ypc_y2 = ypc.y) %>%
  mutate(ypg_diff = ypg_y2 - ypg_y1,
         ypg_pct_change = (ypg_y2 - ypg_y1) / ypg_y1)

# age summaries
rb.prog.summary <-
  rb.progression %>%
  filter(cpg_y1 >= 8) %>%
  group_by(age_y2) %>%
  summarize(avg_ypg_y1 = mean(ypg_y1),
            avg_ypg_y2 = mean(ypg_y2),
            avg_cpg_y1 = mean(cpg_y1),
            avg_cpg_y2 = mean(cpg_y2),
            avg_ypc_y1 = mean(ypg_y1) / mean(cpg_y1),
            avg_ypc_y2 = mean(ypg_y2) / mean(cpg_y2),
            avg_diff = mean(ypg_diff),
            avg_pct_change = (mean(ypg_y2) - mean(ypg_y1)) / mean(ypg_y1),
            sample_size = n(),
            avg_draft_pick = mean(draft_pick))

# summary data
rb.22.prog <-
  rb.progression %>%
  filter(age_y2 == 22,
         cpg_y1 >= 8)
mean(rb.22.prog$ypg_y1)
mean(rb.22.prog$ypg_y2)
mean(rb.22.prog$cpg_y1)
mean(rb.22.prog$cpg_y2)
mean(rb.22.prog$ypg_y1)/mean(rb.22.prog$cpg_y1)
mean(rb.22.prog$ypg_y2)/mean(rb.22.prog$cpg_y2)


### wr receiver progression
wr.y1 <-
  wr.receiving %>%
  filter(season < 2022)

wr.y2 <-
  wr.receiving %>%
  filter(season > 2013)

# yoy comparisons
wr.progression <-
  wr.y1 %>%
  select(season, player_id, full_name, draft_pick, age, ypg, tpg, ypt) %>%
  mutate(next_season = season + 1) %>%
  inner_join(wr.y2 %>% select(season, player_id, full_name, draft_pick, age, ypg, tpg, ypt),
             by = c("player_id", "full_name", "next_season" = "season", "draft_pick")) %>%
  select(-season, -next_season) %>%
  rename(age_y1 = age.x,
         ypg_y1 = ypg.x,
         tpg_y1 = tpg.x,
         ypt_y1 = ypt.x,
         age_y2 = age.y,
         ypg_y2 = ypg.y,
         tpg_y2 = tpg.y,
         ypt_y2 = ypt.y) %>%
  mutate(ypg_diff = ypg_y2 - ypg_y1,
         ypg_pct_change = (ypg_y2 - ypg_y1) / ypg_y1)

# age summaries
wr.prog.summary <-
  wr.progression %>%
  filter(tpg_y1 >= 5) %>%
  group_by(age_y2) %>%
  summarize(avg_ypg_y1 = mean(ypg_y1),
            avg_ypg_y2 = mean(ypg_y2),
            avg_tpg_y1 = mean(tpg_y1),
            avg_tpg_y2 = mean(tpg_y2),
            avg_ypt_y1 = mean(ypg_y1) / mean(tpg_y1),
            avg_ypt_y2 = mean(ypg_y2) / mean(tpg_y2),
            avg_diff = mean(ypg_diff),
            avg_pct_change = (mean(ypg_y2) - mean(ypg_y1)) / mean(ypg_y1),
            sample_size = n(),
            avg_draft_pick = mean(draft_pick))
