library(readr)
library(tidyverse)

seasons <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/seasons.csv")
teams <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/teams.csv")
draft.costs <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/draft_costs.csv")
draft.picks <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/draft_picks.csv")
keepers <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/keepers.csv")
trades <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/trades.csv")
team.results <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/team_results.csv")

drafted.teams <-
  draft.picks %>%
  inner_join(teams %>% select(season_id, team, franchise),
             by = c("season_id", "team")) %>%
  inner_join(seasons %>% select(season_id, season),
             by = "season_id") %>%
  mutate(draft_season = season) %>%
  inner_join(draft.costs,
             by = "round") %>%
  rename(team_cost = draft_cost) %>%
  select(season_id, season, owner, team, franchise, draft_season, team_cost)

kept.teams <-
  keepers %>%
  inner_join(teams %>% select(season_id, team, franchise),
             by = c("season_id", "team")) %>%
  inner_join(seasons %>% select(season_id, season),
             by = "season_id") %>%
  inner_join(drafted.teams %>% select(franchise, draft_season, team_cost),
             by = "franchise") %>%
  rename(draft_cost = team_cost) %>%
  filter(draft_season < season) %>%
  group_by(owner, franchise, season) %>%
  mutate(max_draft_season = max(draft_season)) %>%
  filter(draft_season == max_draft_season) %>%
  mutate(team_cost = draft_cost + 5 * (season - draft_season - 1)) %>%
  select(season_id, season, owner, team, franchise, draft_season, team_cost)

rosters <-
  rbind(drafted.teams, kept.teams) %>%
  left_join(trades %>% rename(owner = from_owner),
            by = c("season_id", "owner", "team")) %>%
  mutate(owner = ifelse(is.na(trade_id), owner, to_owner)) %>%
  select(season_id, season, owner, team, draft_season, team_cost)

standings <-
  rosters %>%
  inner_join(team.results %>% select(season_id, team, points),
             by = c("season_id", "team")) %>%
  group_by(season, owner) %>%
  summarize(total_points = sum(points),
            total_cost = sum(team_cost)) %>%
  ungroup() %>%
  mutate(net_points = total_points - total_cost) %>%
  arrange(season, desc(net_points))
