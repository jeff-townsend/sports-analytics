library(tidyverse)
library(nflreadr)

nfl.pbp.load <- load_pbp(seasons = 2022)

nfl.pbp.neutral.downs <-
  nfl.pbp.load %>%
  select(play_id, posteam, xpass, air_yards, down, ydstogo, yardline_100) %>%
  filter(!is.na(air_yards),
         down == 1,
         ydstogo == 10,
         yardline_100 >= 40, yardline_100 <= 80,
         xpass >= 0.25, xpass <= 0.75) %>%
  mutate(is_short = ifelse(air_yards <= ydstogo, 1, 0),
         is_deep = ifelse(air_yards >= ydstogo + 15, 1, 0),
         is_intermediate = 1 - is_short - is_deep)

team.neutral.downs <-
  nfl.pbp.neutral.downs %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            short_passes = sum(is_short),
            intermediate_passes = sum(is_intermediate),
            deep_passes = sum(is_deep)) %>%
  ungroup() %>%
  mutate(short_rate = short_passes / plays,
         intermediate_rate = intermediate_passes / plays)

nfl.pbp.third.downs <-
  nfl.pbp.load %>%
  select(play_id, posteam, play_type, xpass, qb_dropback, down, ydstogo, air_yards, first_down) %>%
  filter(play_type == 'pass',
         down == 3,
         ydstogo >= 7, ydstogo <= 10) %>%
  mutate(throw_for_first = ifelse(air_yards >= ydstogo, 1, 0))

team.third.passing <-
  nfl.pbp.third.downs %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            first_downs = sum(first_down),
            throw_for_first = sum(throw_for_first, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(first_rate = first_downs / plays,
         throw_for_first_rate = throw_for_first / plays)

ggplot(team.third.passing, aes(x = throw_for_first_rate, y = first_rate)) +
  geom_point() +
  geom_label(aes(label = posteam)) +
  scale_x_continuous(limits = c(0.2, 0.6), labels = scales::percent) +
  scale_y_continuous(limits = c(0.2, 0.6), labels = scales::percent) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("3rd & 7-10 Behavior") +
  xlab("Air Yards Attempt Rate") +
  ylab("First Down Success Rate")
