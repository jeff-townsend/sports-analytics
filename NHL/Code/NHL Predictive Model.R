library(tidyverse)
library(readr)

##### Step 1: Model input variables: CF60, CA60, Shooting %, Opp Shooting %
## Import data from Natural Stat Trick

team.games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NHL/Data/team_games.csv")

## Add variable calculations
team.games <-
  team.games.import %>%
  mutate(cf60 = cf / toi * 60,
         ca60 = ca / toi * 60,
         shpct = gf / cf,
         opp_shpct = ga / ca,
         xshpct = xgf / cf,
         opp_xshpct = xga / ca,
         spoe = shpct - xshpct,
         opp_spoe = opp_shpct - opp_xshpct) %>%
  group_by(team, season) %>%
  mutate(team_game_number = rank(team_game_id))

## Aggregate season totals
team.seasons <-
  team.games %>%
  group_by(team, season) %>%
  summarize(total_toi = sum(toi),
            total_cf = sum(cf),
            total_ca = sum(ca),
            total_gf = sum(gf),
            total_ga = sum(ga),
            total_xgf = sum(xgf),
            total_xga = sum(xga)) %>%
  ungroup() %>%
  mutate(cf60 = total_cf / total_toi * 60,
         ca60 = total_ca / total_toi * 60,
         shpct = total_gf / total_cf,
         opp_shpct = total_ga / total_ca,
         xshpct = total_xgf / total_cf,
         opp_xshpct = total_xga / total_ca,
         spoe = shpct - xshpct,
         opp_spoe = opp_shpct - opp_xshpct) 

## Model out-of-sample correlations (i.e. how do the other 81 games predict this game?)
team.games.oos <-
  team.games %>%
  inner_join(team.seasons %>%
               select(team, season, total_toi, total_gf, total_ga,
                      total_cf, total_ca, total_xgf, total_xga),
             by = c("team", "season")) %>%
  mutate(oos_toi = total_toi - toi,
         oos_cf60 = (total_cf - cf) / oos_toi * 60,
         oos_ca60 = (total_ca - ca) / oos_toi * 60,
         oos_shpct = (total_gf - gf) / (total_cf - cf),
         oos_opp_shpct = (total_ga - ga) / (total_ca - ca),
         oos_xshpct = (total_xgf - xgf) / (total_cf - cf),
         oos_opp_xshpct = (total_xga - xga) / (total_ca - ca),
         oos_spoe = oos_shpct - oos_xshpct,
         oos_opp_spoe = oos_opp_shpct - oos_opp_xshpct) %>%
  select(-c(total_toi, total_gf, total_ga, total_cf, total_ca, total_xgf, total_xga))

## Filter to the minimum number of games a team has played this season to compile model dataset
min.gp <- with(team.games.oos %>%
                 filter(season == "2425") %>%
                 group_by(team) %>%
                 summarize(gp = max(team_game_number)) %>%
                 ungroup(),
               min(gp))

team.games.mod <-
  team.games.oos %>%
  filter(team_game_number <= min.gp)

## Build models for each input variable
cf60.mod <- lm(cf60 ~ oos_cf60, data = team.games.mod)
ca60.mod <- lm(ca60 ~ oos_ca60, data = team.games.mod)
# shpct.mod <- lm(shpct ~ oos_shpct, data = team.games.mod)
# opp.shpct.mod <- lm(opp_shpct ~ oos_opp_shpct, data = team.games.mod)
xshpct.mod <- lm(xshpct ~ oos_xshpct, data = team.games.mod)
opp.xshpct.mod <- lm(opp_xshpct ~ oos_opp_xshpct, data = team.games.mod)
spoe.mod <- lm(spoe ~ oos_spoe, data = team.games.mod)
opp.spoe.mod <- lm(opp_spoe ~ oos_opp_spoe, data = team.games.mod)

## Apply model to season totals

teams <- team.seasons %>% filter(season == "2425")
teams.pred <-
  teams %>%
  mutate(pred_cf60 = predict(cf60.mod, newdata = teams %>% rename(oos_cf60 = cf60))) %>%
  mutate(pred_ca60 = predict(ca60.mod, newdata = teams %>% rename(oos_ca60 = ca60))) %>%
  # mutate(pred_shpct = predict(shpct.mod, newdata = teams %>% rename(oos_shpct = shpct))) %>%
  # mutate(pred_opp_shpct = predict(opp.shpct.mod, newdata = teams %>% rename(oos_opp_shpct = opp_shpct))) %>%
  mutate(pred_xshpct = predict(xshpct.mod, newdata = teams %>% rename(oos_xshpct = xshpct))) %>%
  mutate(pred_opp_xshpct = predict(opp.xshpct.mod, newdata = teams %>% rename(oos_opp_xshpct = opp_xshpct))) %>%
  mutate(pred_spoe = predict(spoe.mod, newdata = teams %>% rename(oos_spoe = spoe))) %>%
  mutate(pred_opp_spoe = predict(opp.spoe.mod, newdata = teams %>% rename(oos_opp_spoe = opp_spoe)))

## Store value adjustments to correct for bias
cf60.adj <- with(teams.pred, mean(cf60 - pred_cf60))
ca60.adj <- with(teams.pred, mean(ca60 - pred_ca60))
# shpct.adj <- with(teams.pred, mean(shpct - pred_shpct))
# opp.shpct.adj <- with(teams.pred, mean(opp_shpct - pred_opp_shpct))
xshpct.adj <- with(teams.pred, mean(xshpct - pred_xshpct))
opp.xshpct.adj <- with(teams.pred, mean(opp_xshpct - pred_opp_xshpct))
spoe.adj <- with(teams.pred, mean(spoe - pred_spoe))
opp.spoe.adj <- with(teams.pred, mean(opp_spoe - pred_opp_spoe))

## Compile team ratings, using adjusted values
team.ratings <-
  teams.pred %>%
  mutate(gf60 = total_gf / total_toi * 60,
         ga60 = total_ga / total_toi * 60,
         gd60 = gf60 - ga60,
         pred_cf60 = as.numeric(pred_cf60 + cf60.adj),
         pred_ca60 = as.numeric(pred_ca60 + ca60.adj),
         pred_xshpct = as.numeric(pred_xshpct + xshpct.adj),
         pred_opp_xshpct = as.numeric(pred_opp_xshpct + opp.xshpct.adj),
         pred_spoe = as.numeric(pred_spoe + spoe.adj),
         pred_opp_spoe = as.numeric(pred_opp_spoe + opp.spoe.adj),
         pred_shpct = pred_xshpct + pred_spoe,
         pred_opp_shpct = pred_opp_xshpct + pred_opp_spoe,
         pred_gf60 = pred_shpct * pred_cf60,
         pred_ga60 = pred_opp_shpct * pred_ca60,
         rating = pred_gf60 - pred_ga60) %>%
  select(team, rating, gf60, ga60, gd60, cf60, ca60, shpct, opp_shpct, xshpct, opp_xshpct,
         pred_gf60, pred_ga60, pred_cf60, pred_ca60, pred_shpct, pred_opp_shpct, pred_xshpct, pred_opp_xshpct) %>%
  arrange(desc(rating))

## Export Team Ratings

setwd("~/Downloads")
write.csv(team.ratings, "NHL/team_ratings.csv")

##### Perform Game Simulations

a <- with(team.ratings, mean(cf60)) ## league average shots per game
b <- with(team.ratings, mean(shpct)) ## league average shooting percentage

calc_win_prob <- function(simulations, home, away,
                          home_sf, home_sa, home_sp, home_spa,
                          away_sf, away_sa, away_sp, away_spa)
{
  shots.lg <- 58.93
  shpct.lg <- 0.05057
  
  home.shots <- round(shots.lg + ((home_sf - home_sa) + (away_sa - shots.lg))) ## TB shot prediction
  home.shpct <- shpct.lg+ ((home_sp - shpct.lg) + (away_spa - shpct.lg)) ## TB shooting percentage prediction
  away.shots <- round(shots.lg + ((away_sf - shots.lg) + (home_sa - shots.lg))) ## FLA shot prediction
  away.shpct <- shpct.lg + ((away_sp - shpct.lg) + (home_spa - shpct.lg)) ## FLA shooting percentage prediction
  
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
    s <- s + 1
  }
  
  games.sim <-
    goals.sim %>%
    mutate(home_win = ifelse(home_goals > away_goals, 1, ifelse(home_goals == away_goals, 0.5, 0)))
  
  home.adj <- with(team.games %>% filter(is_home == 1), mean(ifelse(gf > ga, 1, 0))) - 0.5
  return(with(games.sim, mean(home_win) + home.adj))
}

simulations <- 10000
away <- "Minnesota Wild"
home <- "Calgary Flames"

calc_win_prob(simulations = 25000, home = "Utah Hockey Club", away = "Nashville Predators",
              home_sf = sf.home, home_sa = sa.home, home_sp = sp.home, home_spa = spa.home,
              away_sf = sf.away, away_sa = sa.away, away_sp = sp.away, away_spa = spa.away)
