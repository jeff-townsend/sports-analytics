library(readr)
library(tidyverse)
library(ggthemes)

seasons <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/seasons.csv")
teams <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/teams.csv")
draft.costs <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/draft_costs.csv")
draft.picks <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/draft_picks.csv")
keepers <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/keepers.csv")
trades <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/trades.csv")
team.results <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/TSL/Data/team_results.csv")

drafted.teams <-
  draft.picks %>%
  inner_join(teams %>% select(season_id, team, franchise, league),
             by = c("season_id", "team")) %>%
  inner_join(seasons %>% select(season_id, season),
             by = "season_id") %>%
  mutate(draft_season = season) %>%
  inner_join(draft.costs,
             by = "round") %>%
  rename(team_cost = draft_cost) %>%
  select(season_id, season, owner, team, franchise, league, draft_season, team_cost)

kept.teams <-
  keepers %>%
  inner_join(teams %>% select(season_id, team, franchise, league),
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
  select(season_id, season, owner, team, franchise, league, draft_season, team_cost)

rosters <-
  rbind(drafted.teams, kept.teams) %>%
  left_join(trades %>% rename(owner = from_owner),
            by = c("season_id", "owner", "team")) %>%
  mutate(owner = ifelse(is.na(trade_id), owner, to_owner)) %>%
  select(season_id, season, owner, team, league, draft_season, team_cost)

## Performance

team.performance <-
  rosters %>%
  inner_join(team.results %>% select(season_id, team, points),
             by = c("season_id", "team")) %>%
  mutate(net_points = points - team_cost)

## Historical Standings

standings <-
  team.performance %>%
  group_by(season, owner) %>%
  summarize(total_points = sum(points),
            total_cost = sum(team_cost)) %>%
  ungroup() %>%
  mutate(net_points = total_points - total_cost) %>%
  arrange(season, desc(net_points))

## Performance by League

league.performance <-
  team.performance %>%
  group_by(season, owner, league) %>%
  summarize(teams = n(),
            total_points = sum(points),
            total_cost = sum(team_cost)) %>%
  ungroup() %>%
  mutate(net_points = total_points - total_cost,
         net_points_avg = net_points / teams) %>%
  arrange(season, desc(net_points))


## Charts

ggplot(team.performance %>%
         group_by(owner, league) %>%
         summarize(team_cost = mean(team_cost)) %>%
         arrange(league, owner),
       aes(x = owner, fill = league, y = team_cost)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c("#219ebc", "#023047", "#ffb703", "#fb8500")) +
  theme(legend.title = element_blank()) +
  ggtitle("Average Team Cost by League")

team.performance %>%
  group_by(league) %>%
  summarize(net_points = mean(net_points)) %>%
  arrange(league)

team.performance %>%
  group_by(owner) %>%
  summarize(team_cost = mean(team_cost)) %>%
  arrange(owner)
