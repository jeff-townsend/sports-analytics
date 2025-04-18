library(tidyverse)
library(readr)

##### Step 1: Model input variables: CF60, CA60, Shooting %, Opp Shooting %
## Import data from Natural Stat Trick

team.games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NHL/Data/team_games.csv")

## Add variable calculations
team.games <-
  team.games.import %>%
  mutate(gf60 = gf / toi * 60,
         ga60 = ga / toi * 60,
         cf60 = cf / toi * 60,
         ca60 = ca / toi * 60) %>%
  group_by(team, season) %>%
  mutate(team_game_number = rank(team_game_id))

## Aggregate season totals
team.seasons <-
  team.games %>%
  group_by(team, season) %>%
  summarize(total_toi = sum(toi),
            total_gf = sum(gf),
            total_ga = sum(ga),
            total_cf = sum(cf),
            total_sf = sum(sf),
            total_bf = sum(ff) - sum(sf),
            total_mf = sum(cf) - sum(ff),
            total_ca = sum(ca),
            total_sa = sum(sa),
            total_ba = sum(fa) - sum(sa),
            total_ma = sum(ca) - sum(fa),
            total_xgf = sum(xgf),
            total_xga = sum(xga)) %>%
  ungroup() %>%
  mutate(gf60 = total_gf / total_toi * 60,
         ga60 = total_ga / total_toi * 60,
         cf60 = total_cf / total_toi * 60,
         sf60 = total_sf / total_toi * 60,
         bf60 = total_bf / total_toi * 60,
         mf60 = total_mf / total_toi * 60,
         ca60 = total_ca / total_toi * 60,
         sa60 = total_sa / total_toi * 60,
         ba60 = total_ba / total_toi * 60,
         ma60 = total_ma / total_toi * 60,
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
                      total_cf, total_sf, total_bf, total_mf,
                      total_ca, total_sa, total_ba, total_ma,
                      total_xgf, total_xga),
             by = c("team", "season")) %>%
  mutate(oos_toi = total_toi - toi,
         oos_cf60 = (total_cf - cf) / oos_toi * 60,
         oos_sf60 = (total_sf - sf) / oos_toi * 60,
         oos_bf60 = (total_bf - (ff - sf)) / oos_toi * 60,
         oos_mf60 = (total_mf - (cf - ff)) / oos_toi * 60,
         oos_ca60 = (total_ca - ca) / oos_toi * 60,
         oos_sa60 = (total_sa - sa) / oos_toi * 60,
         oos_ba60 = (total_ba - (fa - sa)) / oos_toi * 60,
         oos_ma60 = (total_ma - (ca - fa)) / oos_toi * 60,
         oos_shpct = (total_gf - gf) / (total_cf - cf),
         oos_opp_shpct = (total_ga - ga) / (total_ca - ca),
         oos_xshpct = (total_xgf - xgf) / (total_cf - cf),
         oos_opp_xshpct = (total_xga - xga) / (total_ca - ca),
         oos_spoe = oos_shpct - oos_xshpct,
         oos_opp_spoe = oos_opp_shpct - oos_opp_xshpct) %>%
  select(-c(total_toi, total_gf, total_ga,
            total_cf, total_sf, total_bf, total_mf,
            total_ca, total_sa, total_ba, total_ma,
            total_xgf, total_xga))

## Filter to the minimum number of games a team has played this season to compile model dataset
min.gp <- with(team.games.oos %>%
                 filter(season == "2425") %>%
                 group_by(team) %>%
                 summarize(gp = max(team_game_number)) %>%
                 ungroup(),
               min(gp))

team.games.ec <-
  team.games.oos %>%
  filter(team_game_number <= min.gp)

## Effective Shot Attempts -- weighing shot attempts by shot outcome value (on goal, blocked, missed)
gf60.cf.mod <- lm(gf60 ~ oos_cf60, data = team.games.ec)
gf60.ecf.mod <- lm(gf60 ~ oos_sf60 + oos_bf60 + oos_mf60, data = team.games.ec)
ga60.ca.mod <- lm(ga60 ~ oos_ca60, data = team.games.ec)
ga60.eca.mod <- lm(ga60 ~ oos_sa60 + oos_ba60 + oos_ma60, data = team.games.ec)

gf60.cf.int <- as.numeric(gf60.cf.mod$coefficients[1])
gf60.cf.coef <- as.numeric(gf60.cf.mod$coefficients[2])
oos.ecf60 <- data.frame(oos_ecf60 = (as.numeric(predict(gf60.ecf.mod, newdata = team.games.ec)) - gf60.cf.int) / gf60.cf.coef)
ga60.ca.int <- as.numeric(ga60.ca.mod$coefficients[1])
ga60.ca.coef <- as.numeric(ga60.ca.mod$coefficients[2])
oos.eca60 <- data.frame(oos_eca60 = (as.numeric(predict(ga60.eca.mod, newdata = team.games.ec)) - ga60.ca.int) / ga60.ca.coef)
team.games.mod <- cbind(team.games.ec, oos.ecf60, oos.eca60)

## Build models for GF/60 and GA/60
gf60.mod <- lm(gf60 ~ oos_ecf60 + oos_xshpct + oos_spoe, data = team.games.mod)
ga60.mod <- lm(ga60 ~ oos_eca60 + oos_opp_xshpct + oos_opp_spoe, data = team.games.mod)

summary(gf60.mod)
summary(ga60.mod)

# cf60.mod <- lm(cf60 ~ oos_sf60 + oos_bf60 + oos_mf60, data = team.games.mod)
# ca60.mod <- lm(ca60 ~ oos_sa60 + oos_ba60 + oos_ma60, data = team.games.mod)
# 
# summary(cf60.mod)
# summary(ca60.mod)

## Apply model to season totals

ecf60 <- data.frame(ecf60 = (as.numeric(predict(gf60.ecf.mod, newdata = team.games.ec)) - gf60.cf.int) / gf60.cf.coef)
eca60 <- data.frame(eca60 = (as.numeric(predict(ga60.eca.mod, newdata = team.games.ec)) - ga60.ca.int) / ga60.ca.coef)
teams <-
  team.seasons %>%
  mutate(ecf60 = (predict(gf60.ecf.mod, newdata = team.seasons %>% rename(oos_sf60 = sf60,
                                                                          oos_bf60 = bf60,
                                                                          oos_mf60 = mf60)) - gf60.cf.int) / gf60.cf.coef,
         eca60 = (predict(ga60.eca.mod, newdata = team.seasons %>% rename(oos_sa60 = sa60,
                                                                          oos_ba60 = ba60,
                                                                          oos_ma60 = ma60)) - ga60.ca.int) / ga60.ca.coef) %>%
  filter(season == "2425")

teams.pred <-
  teams %>%
  mutate(pred_gf60 = predict(gf60.mod,
                             newdata = teams %>% rename(oos_ecf60 = ecf60,
                                                        oos_xshpct = xshpct,
                                                        oos_spoe = spoe)),
         pred_ga60 = predict(ga60.mod,
                             newdata = teams %>% rename(oos_eca60 = eca60,
                                                        oos_opp_xshpct = opp_xshpct,
                                                        oos_opp_spoe = opp_spoe)))

## Store value adjustments to correct for bias
gf60.adj <- with(teams.pred, mean(gf60 - pred_gf60))
ga60.adj <- with(teams.pred, mean(gf60 - pred_ga60))

## Compile team ratings, using adjusted values
team.ratings <-
  teams.pred %>%
  mutate(gf60 = total_gf / total_toi * 60,
         ga60 = total_ga / total_toi * 60,
         gd60 = gf60 - ga60,
         pred_gf60 = as.numeric(pred_gf60 + gf60.adj),
         pred_ga60 = as.numeric(pred_ga60 + ga60.adj),
         rating = pred_gf60 - pred_ga60,
         sf.prop = total_sf / total_cf,
         bf.prop = total_bf / total_cf,
         mf.prop = total_mf / total_cf,
         sa.prop = total_sa / total_ca,
         ba.prop = total_ba / total_ca,
         ma.prop = total_ma / total_ca) %>%
  select(team, rating, pred_gf60, pred_ga60, gf60, ga60, gd60,
         cf60, sf.prop, bf.prop, mf.prop, ca60, sa.prop, ba.prop, ma.prop,
         shpct, opp_shpct, xshpct, opp_xshpct, spoe, opp_spoe) %>%
  arrange(desc(rating))

View(team.ratings %>%
       select(team, rating, sf.prop, bf.prop, mf.prop, sa.prop, ba.prop, ma.prop))

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
