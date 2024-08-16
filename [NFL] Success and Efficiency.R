library(tidyverse)
library(nflreadr)
library(ggthemes)

# set up the data
starting.season <- 2023
ending.season <- 2023
pbp.load <-
  load_pbp(seasons = c(starting.season:ending.season)) %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass"))

pbp <-
  pbp.load %>%
  filter(two_point_attempt == 0) %>%
  select(season, game_id, play_id, posteam, defteam, down, ydstogo, play_type, qb_dropback, yards_gained, epa, success) %>%
  rename(epa_success = success) %>%
  mutate(required_yards = ceiling(ifelse(down == 1, 0.4*ydstogo, ifelse(down == 2, 0.6*ydstogo, ydstogo))),
         success = ifelse(yards_gained >= required_yards, 1, 0),
         relative_yards = yards_gained - required_yards)

team.offense <-
  pbp %>%
  filter(season == 2023) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            successes = sum(success),
            success_rate = mean(success),
            ypp = mean(yards_gained),
            required_ypp = mean(required_yards),
            surplus_ypp = mean(ifelse(success == 1, relative_yards, NA), na.rm = TRUE),
            deficit_ypp = mean(ifelse(success == 0, relative_yards, NA), na.rm = TRUE),
            volatility = sd(relative_yards)) %>%
  ungroup() %>%
  rename(team = posteam)

ggplot

team.defense <-
  nfl.pbp %>%
  filter(season == 2023) %>%
  group_by(defteam) %>%
  summarize(plays = n(),
            successes = sum(success),
            success_rate = mean(success),
            successful_epa = mean(ifelse(success == 1, epa, NA), na.rm = TRUE),
            failed_epa = mean(ifelse(success == 0, epa, NA), na.rm = TRUE),
            epa_per_play = mean(epa),
            epa_volatility = sd(epa)) %>%
  ungroup() %>%
  rename(team = defteam)

team.margins <-
  team.offense %>%
  inner_join(team.defense, by = "team") %>%
  select(team, success_rate.x, success_rate.y, epa_per_play.x, epa_per_play.y) %>%
  rename(off_success_rate = success_rate.x,
         def_success_rate = success_rate.y,
         off_epa_per_play = epa_per_play.x,
         def_epa_per_play = epa_per_play.y) %>%
  mutate(success_rate_diff = off_success_rate - def_success_rate,
         epa_per_play_diff = off_epa_per_play - def_epa_per_play)

avg.offense <- mean(team.margins$off_success_rate)
avg.defense <- mean(team.margins$def_success_rate)
ggplot(team.margins, aes(x = def_success_rate, y = off_success_rate, label = team)) +
  geom_text() +
  scale_x_reverse() +
  geom_hline(yintercept = avg.defense, linetype = "dotted") +
  geom_vline(xintercept = avg.offense, linetype = "dotted") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("Team Success Rates") +
  xlab("Defensive Success Rate") +
  ylab("Offensive Success Rate")

avg.offense.epa <- mean(team.margins$off_epa_per_play)
avg.defense.epa <- mean(team.margins$def_epa_per_play)
ggplot(team.margins, aes(x = def_epa_per_play, y = off_epa_per_play, label = team)) +
  geom_text() +
  scale_x_reverse() +
  geom_hline(yintercept = avg.defense.epa, linetype = "dotted") +
  geom_vline(xintercept = avg.offense.epa, linetype = "dotted") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("Team EPA per Play") +
  xlab("Defensive EPA per Play") +
  ylab("Offensive EPA per Play")

ggplot(team.margins, aes(x = off_success_rate, y = off_epa_per_play, label = team)) +
  geom_text() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("Team Offensive Stats (2022)") +
  xlab("Offensive Success Rate") +
  ylab("Offensive EPA per Play")
