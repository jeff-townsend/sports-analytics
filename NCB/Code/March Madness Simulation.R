library(readr)
library(tidyverse)
library(ggthemes)

## pre-tourney data

r1.d1 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/kenpom_2026_r1d1.csv")
#r1.d2 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_r64_d2_kenpom.csv")
#r2.d1 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_r32_d1_kenpom.csv")
#r2.d2 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_r32_d2_kenpom.csv")
#r3.d1 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/kenpom_2025_r3d1.csv")
#r3.d2 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/kenpom_2025_r3d2.csv")

# add pyth
ratings <-
  r1.d1 %>%
  mutate(raw.pyth = OE^11 / (OE^11 + DE^11), # using 11 because it seems to match what KP does
         adj.pyth = AdjOE^11 / (AdjOE^11 + AdjDE^11))

## load s-curve
bracket.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/tournament_brackets.csv")

# add kp rating to bracket
bracket <-
  bracket.import %>%
  filter(season == 2026) %>%
  inner_join(ratings, by = c("team" = "TeamName")) %>%
  select(s_curve, team, region, seed, adj.pyth) %>%
  rename(rating = adj.pyth)

## load matchup data
rounds.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/tournament_rounds.csv")
games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/tournament_games.csv")
matchups.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/tournament_matchups.csv")

## load bracket entries
entry.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/pool_entries.csv")

entrants <-
  entry.import %>%
  filter(season == 2025) %>%
  rename(entrant = entrant_name)

# add team info into matchups
matchups.tmp <-
  matchups.import %>%
  inner_join(rounds.import, by = "round_id") %>%
  inner_join(bracket, by = c("s_curve_a" = "s_curve")) %>%
  select(matchup_id, game_id, round_id, round_name, s_curve_a, team, rating, s_curve_b) %>%
  rename(team_a = team,
         rating_a = rating) %>%
  inner_join(bracket, by = c("s_curve_b" = "s_curve")) %>%
  select(matchup_id, game_id, round_id, round_name, s_curve_a, team_a, rating_a, s_curve_b, team, rating) %>%
  rename(team_b = team,
         rating_b = rating) %>%
  mutate(win_prob_a = (rating_a - rating_a*rating_b)/(rating_a + rating_b - 2*rating_a*rating_b),
         win_prob_b = 1 - win_prob_a)

## check game.id
matchups.tmp %>%
  filter(team_a == "Arkansas",
         team_b == "Texas Tech") %>%
  select(matchup_id, game_id)

matchups <-
  matchups.tmp %>%
  # mutate(win_prob_a = ifelse(matchup_id == 32, 0, win_prob_a), # creighton over louisville
  #        win_prob_b = ifelse(matchup_id == 32, 1, win_prob_b),
  )

## simulation tournament
set.seed(316)
#start.time <- Sys.time()
simulations <- 25000
simulation.ids <- data.frame(simulation_id = c(1:simulations))
games.simulation <- merge(games.import, simulation.ids)

# create round of 64
r64 <-
  games.simulation %>%
  filter(round_id == 1) %>%
  inner_join(matchups %>% select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  mutate(result = runif(n = 32 * simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

# now do round of 32, and so on
r32 <-
  games.simulation %>%
  filter(round_id == 2) %>%
  inner_join(matchups %>% select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(r64 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "simulation_id")) %>%
  inner_join(r64 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "simulation_id")) %>%
  mutate(result = runif(n = 16 * simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

s16 <-
  games.simulation %>%
  filter(round_id == 3) %>%
  inner_join(matchups %>% select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(r32 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "simulation_id")) %>%
  inner_join(r32 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "simulation_id")) %>%
  mutate(result = runif(n = 8 * simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

e8 <-
  games.simulation %>%
  filter(round_id == 4) %>%
  inner_join(matchups %>% select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(s16 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "simulation_id")) %>%
  inner_join(s16 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "simulation_id")) %>%
  mutate(result = runif(n = 4 * simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

f4 <-
  games.simulation %>%
  filter(round_id == 5) %>%
  inner_join(matchups %>% select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(e8 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "simulation_id")) %>%
  inner_join(e8 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "simulation_id")) %>%
  mutate(result = runif(n = 2 * simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

nc <-
  games.simulation %>%
  filter(round_id == 6) %>%
  inner_join(matchups %>% select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(f4 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "simulation_id")) %>%
  inner_join(f4 %>%
               select(simulation_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "simulation_id")) %>%
  mutate(result = runif(n = simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

## combine all of the rounds into one table
final.results <-
  rbind(r64, r32, s16, e8, f4, nc) %>%
  select(simulation_id, round_id, game_id, winner)

standings <-
  games.simulation %>%
  inner_join(rounds.import, by = "round_id") %>%
  select(simulation_id, game_id, round_id, points_per_win) %>%
  inner_join(final.results %>%
               select(simulation_id, game_id, winner),
             by = c("game_id", "simulation_id")) %>%
  inner_join(entrants %>%
               select(entrant, game_id, selection),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, points_per_win, winner, entrant, selection) %>%
  mutate(points_awarded = ifelse(selection == winner, points_per_win, 0)) %>%
  group_by(simulation_id, entrant) %>%
  summarize(total_points = sum(points_awarded)) %>%
  mutate(placement = rank(desc(total_points), ties.method = "random")) %>%
  inner_join(nc, by = "simulation_id") %>%
  select(simulation_id, placement, entrant, total_points, winner) %>%
  rename(champion = winner) %>%
  mutate(won_gold = ifelse(placement == 1, 1, 0),
         won_silver = ifelse(placement == 2, 1, 0),
         won_bronze = ifelse(placement == 3, 1, 0)) %>%
  group_by(entrant) %>%
  summarize(wins = sum(won_gold),
            win_rate = mean(won_gold),
            #medal.rate = mean(won.gold + won.silver + won.bronze),
            exp_points = round(mean(total_points),0)) %>%
  ungroup() %>%
  mutate(stage_id = 58,
         stage = "S16 D2 Update") %>%
  arrange(desc(win_rate))

mean.win.rate <- mean(standings$win_rate)
ggplot(standings,
       aes(x = reorder(entrant, win_rate, sum), y = win_rate)) +
  geom_col() +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)),
            hjust = -0.5) +
  theme_fivethirtyeight() +
  ggtitle("Bracket Win Probability - S16 D2 Update") +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  geom_hline(yintercept = mean.win.rate,
             linetype = "dashed",
             color = "red")

#standings.history <- read.csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/NCB/Data/pool_standings_history.csv")

standings.history <- rbind(standings, standings.history)
#write.csv(historical.standings, "pool_standings_history.csv")

gc()
