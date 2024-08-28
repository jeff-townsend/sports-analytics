library(tidyverse)
library(ggthemes)
library(readr)

fixtures.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/EPL/epl_fbref_fixtures.csv",
                            col_types = cols(date = col_date(format = "%m/%d/%y")))

fixtures <-
  fixtures.import %>%
  select(season, week, home, away, home_goals, away_goals, home_xg, away_xg) %>%
  mutate(fixture_id = c(1:(nrow(fixtures.import))))

fixtures.long <-
  rbind(
    fixtures %>%
      mutate(is_home = 1) %>%
      select(fixture_id, season, week, home, away, is_home, home_goals, away_goals, home_xg, away_xg) %>%
      rename(team = home,
             opponent = away,
             gf = home_goals,
             ga = away_goals,
             xgf = home_xg,
             xga = away_xg),
    fixtures %>%
      mutate(is_home = 0) %>%
      select(fixture_id, season, week, away, home, is_home, away_goals, home_goals, away_xg, home_xg) %>%
      rename(team = away,
             opponent = home,
             gf = away_goals,
             ga = home_goals,
             xgf = away_xg,
             xga = home_xg)
  ) %>%
  arrange(fixture_id) %>%
  mutate(win = ifelse(gf > ga, 1, 0),
         draw = ifelse(gf == ga, 1, 0),
         loss = 1 - win - draw,
         points = 3*win + draw)

table <-
  fixtures.long %>%
  group_by(season, team) %>%
  summarize(points = sum(points),
            #win_rate = mean(win + 0.5*draw), ## doesn't add value
            gd = mean(gf - ga),
            xgd = mean(xgf - xga)) %>%
  ungroup() %>%
  mutate(gdoe = gd - xgd)

xpts.mod <- lm(points ~ gd, data = table)
#table$xpts = predict(xpts.mod, table) ## doesn't add value
#table$poe = with(table, points - xpts) ## doesn't add value

yoy.table <-
  table %>%
  filter(season >= 2020) %>%
  mutate(prior_season = season - 1) %>%
  inner_join(table, by = c("prior_season" = "season", "team"),
             suffix = c("","_prior")) %>%
  select(-prior_season)

# ggplot(yoy.table, aes(x = prior_points, y = points)) +
#   geom_point() +
#   theme_fivethirtyeight()

with(yoy.table, cor(points_prior, points)^2)
with(yoy.table, cor(gd_prior, gd)^2)
with(yoy.table, cor(gd_prior, points)^2)
#with(yoy.table, cor(poe_prior, poe)^2)
with(yoy.table, cor(xgd_prior, xgd)^2)
with(yoy.table, cor(xgd_prior, points)^2)
with(yoy.table, cor(gdoe_prior, gdoe)^2)

yoy.mod <- lm(points ~ xgd_prior + gdoe_prior, data = yoy.table)
summary(yoy.mod)

pred.table <-
  table %>%
  filter(season == 2023) %>%
  select(team, points, xgd, gdoe) %>%
  rename(points_prior = points,
         xgd_prior = xgd,
         gdoe_prior = gdoe)
pred.table$projected_points = predict(yoy.mod, pred.table)
