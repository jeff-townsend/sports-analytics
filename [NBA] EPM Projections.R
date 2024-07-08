library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

epm.load <- read_csv("Downloads/EPM Dataset.csv")

epm.data <-
  epm.load %>%
  rename(player_id = nba_id,
         player_name = name)

epm.reg <-
  epm.data %>%
  filter(is_playoff == 0)

player.epm <-
  epm.reg %>%
  select(player_id, player_name, season, epm, gp, min, mpg, usg, oepm, depm)

with(player.epm, cor(epm, min)^2)
ggplot(data = player.epm, aes(x = min, y = epm)) +
  geom_point() +
  theme_fivethirtyeight()

player.yoy <-
  player.epm %>%
  inner_join(player.epm %>% mutate(season = season - 1),
             by = c("player_id", "player_name", "season"),
             suffix = c("","_next")) %>%
  inner_join(player.epm %>% mutate(season = season + 1),
             by = c("player_id", "player_name", "season"),
             suffix = c("", "_prior")) %>%
  filter(min >= 500,
         mpg >= 10)

with(player.yoy, cor(depm, depm_next)^2)
ggplot(data = player.yoy, aes(x = depm, y = depm_next)) +
  geom_point() +
  theme_fivethirtyeight()

epm.pred <- lm(epm_next ~ epm + epm_prior, data = player.yoy)
summary(epm.pred)

oepm.pred <- lm(oepm_next ~ oepm + oepm_prior + mpg + usg, data = player.yoy)
summary(oepm.pred)

depm.pred <- lm(depm_next ~ depm + depm_prior + mpg, data = player.yoy)
summary(depm.pred)

epm.projections <-
  player.epm %>%
  left_join(player.epm %>%
              select(player_id, player_name, season, epm) %>%
              mutate(season = season - 1),
            by = c("player_id", "player_name", "season"),
            suffix = c("", "_actual")) %>%
  inner_join(player.epm %>%
               select(player_id, player_name, season, epm, oepm, depm) %>%
               mutate(season = season + 1),
            by = c("player_id", "player_name", "season"),
            suffix = c("", "_prior")) %>%
  filter(min >= 500,
         mpg >= 10)

epm.projections$oepm_projected <- predict(oepm.pred, newdata = epm.projections)
epm.projections$depm_projected <- predict(depm.pred, newdata = epm.projections)
epm.projections$epm_projected <- with(epm.projections, oepm_projected + depm_projected)

epm.projections.24 <-
  epm.projections %>%
  filter(season == 2024)

write_csv(epm.projections.24, "Downloads/EPM Projections.csv")
