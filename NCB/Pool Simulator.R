library(readr)
library(tidyverse)
library(ggthemes)

## pre-tourney data

ratings.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_r64_d1_kenpom_summary.csv")

# add pyth
ratings <-
  ratings.import %>%
  mutate(raw.pyth = OE^11 / (OE^11 + DE^11), # using 11 because it seems to match what KP does
         adj.pyth = AdjOE^11 / (AdjOE^11 + AdjDE^11))

## load s-curve
teams.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_bracket.csv")

# add kp rating to bracket
teams <-
  teams.import %>%
  inner_join(ratings, by = c("team" = "TeamName")) %>%
  select(s_curve, team, region, seed, adj.pyth) %>%
  rename(rating = adj.pyth)

## load matchup data
rounds.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/tourney_rounds.csv")
games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/tourney_games.csv")
matchups.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/tourney_matchups.csv")

# add team info into matchups
matchups <-
  matchups.import %>%
  inner_join(rounds.import, by = c("round_id" = "id")) %>%
  inner_join(teams, by = c("s_curve_a" = "s_curve")) %>%
  select(id, game_id, round_id, round_name, s_curve_a, team, rating, s_curve_b) %>%
  rename(team_a = team,
         rating_a = rating) %>%
  inner_join(teams, by = c("s_curve_b" = "s_curve")) %>%
  select(id, game_id, round_id, round_name, s_curve_a, team_a, rating_a, s_curve_b, team, rating) %>%
  rename(team_b = team,
         rating_b = rating) %>%
  mutate(win_prob_a = (rating_a - rating_a*rating_b)/(rating_a + rating_b - 2*rating_a*rating_b),
         win_prob_b = 1 - win_prob_a)

set.seed(316)
simulations <- 25000
simulation.ids <- data.frame(simulation_id = c(1:simulations))
games.simulation <-
  merge(games.import, simulation.ids) %>%
  rename(game_id = id)

# create round of 64
r64 <-
  games.simulation %>%
  filter(round_id == 1) %>%
  inner_join(matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  mutate(result = runif(n = 32 * simulations),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

# now do round of 32, and so on
r32 <-
  games.simulation %>%
  filter(round_id == 2) %>%
  inner_join(matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
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
  inner_join(matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
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
  inner_join(matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
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
  inner_join(matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
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
  inner_join(matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
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

gc()

## combine all of the rounds into one table
final.results <-
  rbind(r64, r32, s16, e8, f4, nc) %>%
  select(simulation_id, round_id, game_id, winner)

## analyze public brackets
bracket.ratings.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_bracket_ratings.csv")
bracket.ratings <-
  bracket.ratings.import %>%
  inner_join(teams.import %>%
               select(s_curve, team),
             by = "team")

bracket.matchups <-
  matchups.import %>%
  inner_join(rounds.import, by = c("round_id" = "id")) %>%
  inner_join(bracket.ratings, by = c("s_curve_a" = "s_curve")) %>%
  select(id, game_id, round_id, round_name, s_curve_a, team, rating, s_curve_b) %>%
  rename(team_a = team,
         rating_a = rating) %>%
  inner_join(bracket.ratings, by = c("s_curve_b" = "s_curve")) %>%
  select(id, game_id, round_id, round_name, s_curve_a, team_a, rating_a, s_curve_b, team, rating) %>%
  rename(team_b = team,
         rating_b = rating) %>%
  mutate(win_prob_a = (rating_a - rating_a*rating_b)/(rating_a + rating_b - 2*rating_a*rating_b),
         win_prob_b = 1 - win_prob_a)

set.seed(319)
brackets <- 25000
bracket.ids <- data.frame(bracket_id = c(1:brackets))
bracket.simulation <-
  merge(games.import, bracket.ids) %>%
  rename(game_id = id)

# create round of 64
r64.brackets <-
  bracket.simulation %>%
  filter(round_id == 1) %>%
  inner_join(bracket.matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(bracket_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  mutate(result = runif(n = 32 * brackets),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

# now do round of 32, and so on
r32.brackets <-
  bracket.simulation %>%
  filter(round_id == 2) %>%
  inner_join(bracket.matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(bracket_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(r64.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "bracket_id")) %>%
  inner_join(r64.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "bracket_id")) %>%
  mutate(result = runif(n = 16 * brackets),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

s16.brackets <-
  bracket.simulation %>%
  filter(round_id == 3) %>%
  inner_join(bracket.matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(bracket_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(r32.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "bracket_id")) %>%
  inner_join(r32.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "bracket_id")) %>%
  mutate(result = runif(n = 8 * brackets),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

e8.brackets <-
  bracket.simulation %>%
  filter(round_id == 4) %>%
  inner_join(bracket.matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(bracket_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(s16.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "bracket_id")) %>%
  inner_join(s16.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "bracket_id")) %>%
  mutate(result = runif(n = 4 * brackets),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

f4.brackets <-
  bracket.simulation %>%
  filter(round_id == 5) %>%
  inner_join(bracket.matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(bracket_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(e8.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "bracket_id")) %>%
  inner_join(e8.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "bracket_id")) %>%
  mutate(result = runif(n = 2 * brackets),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

nc.brackets <-
  bracket.simulation %>%
  filter(round_id == 6) %>%
  inner_join(bracket.matchups %>%
               rename(matchup_id = id) %>%
               select(matchup_id, game_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a),
             by = "game_id") %>%
  select(bracket_id, game_id, round_id, matchup_id, s_curve_a, team_a, s_curve_b, team_b, win_prob_a) %>%
  inner_join(f4.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_a" = "winner_s_curve", "bracket_id")) %>%
  inner_join(f4.brackets %>%
               select(bracket_id, winner_s_curve),
             by = c("s_curve_b" = "winner_s_curve", "bracket_id")) %>%
  mutate(result = runif(n = brackets),
         winner = ifelse(result <= win_prob_a, team_a, team_b),
         winner_s_curve = ifelse(result <= win_prob_a, s_curve_a, s_curve_b))

gc()

## combine all of the rounds into one table
final.brackets <-
  rbind(r64.brackets, r32.brackets, s16.brackets, e8.brackets, f4.brackets, nc.brackets) %>%
  select(bracket_id, round_id, game_id, winner)

## measure bracket probabilities
bracket.probs <-
  final.brackets %>%
  inner_join(rounds.import %>%
               select(id, round_abr) %>%
               rename(round = round_abr),
             by = c("round_id" = "id")) %>%
  rename(team = winner) %>%
  group_by(round_id, round, team) %>%
  summarize(prob = n() / simulations) %>%
  ungroup() %>%
  select(-round_id) %>%
  pivot_wider(names_from = round, values_from = prob, values_fill = 0)
colnames(bracket.probs) <- c("team", "R32", "S16", "E8", "F4", "NC", "Champ")

## bracket pool simulations

# best.brackets <- data.frame(pool_id = integer(),
#                             bracket_id = integer(),
#                             win_rate = double(),
#                             champion = character(),
#                             finalist1 = character(),
#                             finalist2 = character(),
#                             south = character(),
#                             east = character(),
#                             midwest = character(),
#                             west = character())
pool.size <- 27

entrant.ids <- sample(c(1:simulations), pool.size)

bracket.simulation <-
  merge(games.import, bracket.ids) %>%
  rename(game_id = id)

entrants <-
  final.brackets %>%
  filter(bracket_id %in% entrant.ids)

standings <-
  games.simulation %>%
  inner_join(rounds.import, by = c("round_id" = "id")) %>%
  select(simulation_id, game_id, round_id, points_per_win) %>%
  inner_join(final.results %>%
               select(simulation_id, game_id, winner),
             by = c("game_id", "simulation_id")) %>%
  inner_join(entrants %>%
               select(bracket_id, game_id, winner) %>%
               rename(selection = winner),
             by = "game_id") %>%
  select(simulation_id, game_id, round_id, points_per_win, winner, bracket_id, selection) %>%
  mutate(points_awarded = ifelse(selection == winner, points_per_win, 0)) %>%
  group_by(simulation_id, bracket_id) %>%
  summarize(total_points = sum(points_awarded)) %>%
  mutate(placement = rank(desc(total_points), ties.method = "min")) %>%
  inner_join(nc, by = "simulation_id") %>%
  select(simulation_id, placement, bracket_id, total_points, winner) %>%
  rename(champion = winner) %>%
  mutate(won_gold = ifelse(placement == 1, 1, 0),
         won_silver = ifelse(placement == 2, 1, 0),
         won_bronze = ifelse(placement == 3, 1, 0)) %>%
  group_by(bracket_id) %>%
  summarize(wins = sum(won_gold),
            win_rate = mean(won_gold),
            #medal.rate = mean(won.gold + won.silver + won.bronze),
            exp_points = round(mean(total_points),0)) %>%
  arrange(desc(win_rate))

summary <-
  standings %>%
  mutate(placement = rank(desc(wins), ties.method = "min")) %>%
  inner_join(entrants %>%
               filter(game_id == 63) %>%
               select(bracket_id, winner) %>%
               rename(champion = winner),
             by = "bracket_id") %>%
  inner_join(entrants %>%
               filter(game_id == 61) %>%
               select(bracket_id, winner) %>%
               rename(finalist1 = winner),
             by = "bracket_id") %>%
  inner_join(entrants %>%
               filter(game_id == 62) %>%
               select(bracket_id, winner) %>%
               rename(finalist2 = winner),
             by = "bracket_id") %>%
  inner_join(entrants %>%
               filter(game_id == 57) %>%
               select(bracket_id, winner) %>%
               rename(south = winner),
             by = "bracket_id") %>%
  inner_join(entrants %>%
               filter(game_id == 58) %>%
               select(bracket_id, winner) %>%
               rename(east = winner),
             by = "bracket_id") %>%
  inner_join(entrants %>%
               filter(game_id == 59) %>%
               select(bracket_id, winner) %>%
               rename(midwest = winner),
             by = "bracket_id") %>%
  inner_join(entrants %>%
               filter(game_id == 60) %>%
               select(bracket_id, winner) %>%
               rename(west = winner),
             by = "bracket_id")

best.bracket <-
  summary %>%
  filter(placement == 1) %>%
  mutate(pool_id = p) %>%
  select(pool_id, bracket_id, win_rate, champion, finalist1, finalist2, south, east, midwest, west)

best.brackets <- rbind(best.brackets, best.bracket)
p <- p + 1
gc()

best.brackets.expanded <-
  best.brackets %>%
  filter(champion == "Houston") %>%
         #finalist1 == "Florida") %>%
  inner_join(final.brackets %>%
              filter(game_id == 49) %>%
              select(bracket_id, winner) %>%
              rename(south_finalist1 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 50) %>%
              select(bracket_id, winner) %>%
              rename(east_finalist1 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 51) %>%
              select(bracket_id, winner) %>%
              rename(midwest_finalist1 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 52) %>%
              select(bracket_id, winner) %>%
              rename(west_finalist1 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 53) %>%
              select(bracket_id, winner) %>%
              rename(west_finalist2 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 54) %>%
              select(bracket_id, winner) %>%
              rename(midwest_finalist2 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 55) %>%
              select(bracket_id, winner) %>%
              rename(east_finalist2 = winner),
            by = "bracket_id") %>%
  inner_join(final.brackets %>%
              filter(game_id == 56) %>%
              select(bracket_id, winner) %>%
              rename(south_finalist2 = winner),
            by = "bracket_id")

best.brackets.expanded %>%
  group_by(champion, finalist1, finalist2, south, east, midwest, west) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(-freq_rank)


best.brackets.expanded %>%
  group_by(champion) %>%
  summarize(freq = n(),
            win_rate = mean(win_rate)) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(-freq_rank)

best.brackets.expanded %>%
  group_by(finalist1) %>%
  summarize(freq = n(),
            win_rate = mean(win_rate)) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(-freq_rank)

best.brackets.expanded %>%
  group_by(finalist2) %>%
  summarize(freq = n(),
            win_rate = mean(win_rate)) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(-freq_rank)

best.brackets.expanded %>%
  group_by(south) %>%
  summarize(freq = n(),
            win_rate = mean(win_rate)) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(-freq_rank)

best.brackets.expanded %>%
  group_by(east) %>%
  summarize(freq = n(),
            win_rate = mean(win_rate)) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(-freq_rank)

best.brackets.expanded %>%
  group_by(midwest) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(midwest, freq)

best.brackets.expanded %>%
  group_by(west) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(west, freq)

best.brackets.expanded %>%
  group_by(south_finalist1) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(south_finalist1, freq)

best.brackets.expanded %>%
  group_by(south_finalist2) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(south_finalist2, freq)

best.brackets.expanded %>%
  group_by(east_finalist1) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(east_finalist1, freq)

best.brackets.expanded %>%
  group_by(east_finalist2) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(east_finalist2, freq)

best.brackets.expanded %>%
  group_by(midwest_finalist1) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(midwest_finalist1, freq)

best.brackets.expanded %>%
  group_by(midwest_finalist2) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(midwest_finalist2, freq)

best.brackets.expanded %>%
  group_by(west_finalist1) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(west_finalist1, freq)

best.brackets.expanded %>%
  group_by(west_finalist2) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(freq_rank = rank(desc(freq), ties.method = "min")) %>%
  filter(freq_rank <= 2) %>%
  select(west_finalist2, freq)

