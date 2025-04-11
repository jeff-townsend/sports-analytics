library(tidyverse)
library(readr)

##### Step 1: Model input variables: CF60, CA60, Shooting %, Opp Shooting %
## Import data from Natural Stat Trick
team.games.import <- read_csv("Downloads/NHL/2425_team_games.csv")

## Add variable calculations
team.games <-
  team.games.import %>%
  mutate(cf60 = cf / toi * 60,
         ca60 = ca / toi * 60,
         shpct = gf / cf,
         opp_shpct = ga / ca) %>%
  group_by(team) %>%
  mutate(team_game_number = rank(team_game_id))

## Aggregate season totals
teams <-
  team.games %>%
  group_by(team) %>%
  summarize(total_toi = sum(toi),
            total_cf = sum(cf),
            total_ca = sum(ca),
            total_gf = sum(gf),
            total_ga = sum(ga)) %>%
  ungroup() %>%
  mutate(cf60 = total_cf / total_toi * 60,
         ca60 = total_ca / total_toi * 60,
         shpct = total_gf / total_cf,
         opp_shpct = total_ga / total_ca) 

## Model out-of-sample correlations (i.e. how do the other 81 games predict this game?)
team.games.oos <-
  team.games %>%
  inner_join(teams, by = "team") %>%
  mutate(oos_toi = total_toi - toi,
         oos_cf60 = (total_cf - cf) / oos_toi * 60,
         oos_ca60 = (total_ca - ca) / oos_toi * 60,
         oos_shpct = (total_gf - gf) / (total_cf - cf),
         oos_opp_shpct = (total_ga - ga) / (total_ca - ca))

## Build model for each input variable
cf60.mod <- lm(cf60 ~ oos_cf60, data = team.games.oos)
ca60.mod <- lm(ca60 ~ oos_ca60, data = team.games.oos)
shpct.mod <- lm(shpct ~ oos_shpct, data = team.games.oos)
opp.shpct.mod <- lm(opp_shpct ~ oos_opp_shpct, data = team.games.oos)

## Apply model to season totals

teams.pred <-
  teams %>%
  mutate(pred_cf60 = predict(cf60.mod, newdata = teams %>% rename(oos_cf60 = cf60))) %>%
  mutate(pred_ca60 = predict(ca60.mod, newdata = teams %>% rename(oos_ca60 = ca60))) %>%
  mutate(pred_shpct = predict(shpct.mod, newdata = teams %>% rename(oos_shpct = shpct))) %>%
  mutate(pred_opp_shpct = predict(opp.shpct.mod, newdata = teams %>% rename(oos_opp_shpct = opp_shpct)))

## Store value adjustments to correct for bias
cf60.adj <- with(teams.pred, mean(cf60 - pred_cf60))
ca60.adj <- with(teams.pred, mean(ca60 - pred_ca60))
shpct.adj <- with(teams.pred, mean(shpct - pred_shpct))
opp.shpct.adj <- with(teams.pred, mean(opp_shpct - pred_opp_shpct))

## Compile team ratings, using adjusted values
team.ratings <-
  teams.pred %>%
  mutate(gf60 = total_gf / total_toi * 60,
         ga60 = total_ga / total_toi * 60,
         gd60 = gf60 - ga60,
         pred_cf60 = as.numeric(pred_cf60 + cf60.adj),
         pred_ca60 = as.numeric(pred_ca60 + ca60.adj),
         pred_shpct = as.numeric(pred_shpct + shpct.adj),
         pred_opp_shpct = as.numeric(pred_opp_shpct + opp.shpct.adj),
         pred_gf60 = pred_shpct * pred_cf60,
         pred_ga60 = pred_opp_shpct * pred_ca60,
         rating = pred_gf60 - pred_ga60) %>%
  select(team, rating, gf60, ga60, gd60, cf60, ca60, shpct, opp_shpct,
         pred_gf60, pred_ga60, pred_cf60, pred_ca60, pred_shpct, pred_opp_shpct) %>%
  arrange(desc(rating))

##### Perform Game Simulations
simulations <- 10000
home.adj <- with(team.games %>% filter(is_home == 1), mean(ifelse(gf > ga, 1, 0))) - 0.5

away.team <- "Minnesota Wild"
home.team <- "Calgary Flames"

shots.lg <- with(team.ratings, mean(cf60)) ## league average shots per game
sf.home <- with(team.ratings %>% filter(team == home.team), pred_cf60) ## TB's average
sa.home <- with(team.ratings %>% filter(team == home.team), pred_ca60) ## TB's average allowed
sf.away <- with(team.ratings %>% filter(team == away.team), pred_cf60) ## FLA's average
sa.away <- with(team.ratings %>% filter(team == away.team), pred_ca60) ## FLA's average allowed

shpct.lg <- with(team.ratings, mean(shpct)) ## league average shooting percentage
sp.home <- with(team.ratings %>% filter(team == home.team), pred_shpct) ## TB's shooting percentage
spa.home <- with(team.ratings %>% filter(team == home.team), pred_opp_shpct) ## TB's shooting percentage allowed
sp.away <- with(team.ratings %>% filter(team == away.team), pred_shpct) ## FLA's shooting percentage
spa.away <- with(team.ratings %>% filter(team == away.team), pred_opp_shpct) ## FLA's shooting percentage allowed

home.shots <- round(shots.lg + ((sf.home - shots.lg) + (sa.away - shots.lg))) ## TB shot prediction
home.shpct <- shpct.lg+ ((sp.home - shpct.lg) + (spa.away - shpct.lg)) ## TB shooting percentage prediction
away.shots <- round(shots.lg + ((sf.away - shots.lg) + (sa.home - shots.lg))) ## FLA shot prediction
away.shpct <- shpct.lg + ((sp.away - shpct.lg) + (spa.home - shpct.lg)) ## FLA shooting percentage prediction

shots.sim <- data.frame(game_id = c(1:simulations),
                        home_shots = rpois(n = simulations, lambda = home.shots),
                        away_shots = rpois(n = simulations, lambda = away.shots))

goals.sim <-
  shots.sim %>%
  mutate(home_goals = 0,
         away_goals = 0)
s <- 1
for(s in 1:simulations)
  {
  goals.sim$home_goals[s] <- rbinom(n = 1, size = goals.sim$home_shots[s], prob = home.shpct)
  goals.sim$away_goals[s] <- rbinom(n = 1, size = goals.sim$away_shots[s], prob = away.shpct)
  }

games.sim <-
  goals.sim %>%
  mutate(home_win = ifelse(home_goals > away_goals, 1, ifelse(home_goals == away_goals, 0.5, 0)))

with(games.sim, mean(home_win) + home.adj)
