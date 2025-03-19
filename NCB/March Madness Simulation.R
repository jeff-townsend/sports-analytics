library(readr)
library(tidyverse)
library(ggthemes)

## pre-tourney data

r64.d1 <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_r64_d1_kenpom_summary.csv")
# kenpom.r64.d2.import <- read_csv("Data Analysis/NCB/Data/2024 R64 D2 KenPom Summary.csv")
# kenpom.r32.d1.import <- read_csv("Data Analysis/NCB/Data/2024 R32 D1 KenPom Summary.csv")
# kenpom.r32.d2.import <- read_csv(paste0(wd, "2024 R32 D2 KenPom Summary.csv"))
# kenpom.s16.d1.import <- read_csv(paste0(wd, "2024 S16 D1 KenPom Summary.csv"))
# kenpom.s16.d2.import <- read_csv("Data Analysis/NCB/Data/2022 S16 D2 KenPom Summary.csv")
# kenpom.e8.d1.import <- read_csv("Data Analysis/NCB/Data/2022 E8 D1 KenPom Summary.csv")

# add pyth
ratings <-
  r64.d1 %>%
  mutate(raw.pyth = OE^11 / (OE^11 + DE^11), # using 11 because it seems to match what KP does
         adj.pyth = AdjOE^11 / (AdjOE^11 + AdjDE^11))

## load s-curve
scurve.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/2025/2025_bracket.csv")

# add kp rating to bracket
scurve <-
  scurve.import %>%
  inner_join(ratings, by = c("team" = "TeamName")) %>%
  select(s_curve, team, region, seed, adj.pyth) %>%
  rename(rating = adj.pyth)

## load matchup data
rounds.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/tourney_rounds.csv")
games.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/tourney_games.csv")
matchups.import <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/NCB/tourney_matchups.csv")

## who picked whom
#pickrates.import <- read_csv("Data Analysis/NCB/Data/2022 Who Picked Whom.csv")

## load bracket entries
entry.import <- read_csv(paste0(wd, "2024 Entries.csv"))


# add team info into matchups
matchups.tmp <-
  matchups.import %>%
  inner_join(rounds.import, by = c("round_id" = "id")) %>%
  inner_join(scurve, by = c("s_curve_a" = "s_curve")) %>%
  select(id, game_id, round_id, round_name, s_curve_a, team, rating, s_curve_b) %>%
  rename(team_a = team,
         rating_a = rating) %>%
  inner_join(scurve, by = c("s_curve_b" = "s_curve")) %>%
  select(id, game_id, round_id, round_name, s_curve_a, team_a, rating_a, s_curve_b, team, rating) %>%
  rename(team_b = team,
         rating_b = rating) %>%
  mutate(win_prob_a = (rating_a - rating_a*rating_b)/(rating_a + rating_b - 2*rating_a*rating_b),
         win_prob_b = 1 - win_prob_a)

## check game.id
# matchups.tmp %>%
#   filter(team.a == "Yale",
#          team.b == "San Diego St.") %>%
#   select(id, game.id)

matchups <-
  matchups.tmp %>%
  # mutate(win.prob.a = ifelse(id == 29, 0, win.prob.a), # input game results
  #        win.prob.b = ifelse(id == 29, 1, win.prob.b))


set.seed(316)
#start.time <- Sys.time()
simulations <- 25000
simulation.ids <- data.frame(simulation.id = c(1:simulations))
games.simulation <- merge(games.import, simulation.ids)

# create round of 64
r64 <-
  games.simulation %>%
  filter(round.id == 1) %>%
  inner_join(matchups, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, s.curve.a, team.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  mutate(result = runif(n = 32 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

# now do round of 32, and so on
r32 <-
  games.simulation %>%
  filter(round.id == 2) %>%
  inner_join(matchups, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(r64, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(r64, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
  select(simulation.id, game.id.x, round.id.x, matchup.id.x, s.curve.a.x, team.a.x, s.curve.b.x, team.b.x, win.prob.a.x) %>%
  rename(game.id = game.id.x,
         round.id = round.id.x,
         matchup.id = matchup.id.x,
         s.curve.a = s.curve.a.x,
         team.a = team.a.x,
         s.curve.b = s.curve.b.x,
         team.b = team.b.x,
         win.prob.a = win.prob.a.x) %>%
  mutate(result = runif(n = 16 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

s16 <-
  games.simulation %>%
  filter(round.id == 3) %>%
  inner_join(matchups, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(r32, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(r32, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
  select(simulation.id, game.id.x, round.id.x, matchup.id.x, s.curve.a.x, team.a.x, s.curve.b.x, team.b.x, win.prob.a.x) %>%
  rename(game.id = game.id.x,
         round.id = round.id.x,
         matchup.id = matchup.id.x,
         s.curve.a = s.curve.a.x,
         team.a = team.a.x,
         s.curve.b = s.curve.b.x,
         team.b = team.b.x,
         win.prob.a = win.prob.a.x) %>%
  mutate(result = runif(n = 8 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

e8 <-
  games.simulation %>%
  filter(round.id == 4) %>%
  inner_join(matchups, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(s16, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(s16, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
  select(simulation.id, game.id.x, round.id.x, matchup.id.x, s.curve.a.x, team.a.x, s.curve.b.x, team.b.x, win.prob.a.x) %>%
  rename(game.id = game.id.x,
         round.id = round.id.x,
         matchup.id = matchup.id.x,
         s.curve.a = s.curve.a.x,
         team.a = team.a.x,
         s.curve.b = s.curve.b.x,
         team.b = team.b.x,
         win.prob.a = win.prob.a.x) %>%
  mutate(result = runif(n = 4 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

f4 <-
  games.simulation %>%
  filter(round.id == 5) %>%
  inner_join(matchups, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(e8, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(e8, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
  select(simulation.id, game.id.x, round.id.x, matchup.id.x, s.curve.a.x, team.a.x, s.curve.b.x, team.b.x, win.prob.a.x) %>%
  rename(game.id = game.id.x,
         round.id = round.id.x,
         matchup.id = matchup.id.x,
         s.curve.a = s.curve.a.x,
         team.a = team.a.x,
         s.curve.b = s.curve.b.x,
         team.b = team.b.x,
         win.prob.a = win.prob.a.x) %>%
  mutate(result = runif(n = 2 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

nc <-
  games.simulation %>%
  filter(round.id == 6) %>%
  inner_join(matchups, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(f4, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(f4, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
  select(simulation.id, game.id.x, round.id.x, matchup.id.x, s.curve.a.x, team.a.x, s.curve.b.x, team.b.x, win.prob.a.x) %>%
  rename(game.id = game.id.x,
         round.id = round.id.x,
         matchup.id = matchup.id.x,
         s.curve.a = s.curve.a.x,
         team.a = team.a.x,
         s.curve.b = s.curve.b.x,
         team.b = team.b.x,
         win.prob.a = win.prob.a.x) %>%
  mutate(result = runif(n = simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

gc()

## combine all of the rounds into one table
final.results <-
  rbind(r64, r32, s16, e8, f4, nc) %>%
  select(simulation.id, game.id, winner)

## assess brackets
standings <-
  games.simulation %>%
  inner_join(rounds.import, by = c("round.id" = "id")) %>%
  select(simulation.id, id, round.id, points.per.win) %>%
  inner_join(final.results, by = c("id" = "game.id", "simulation.id")) %>%
  inner_join(entry.import, by = c("id" = "game.id", "round.id")) %>%
  filter(entrant.name != "Chalk",
         entrant.name != "Most Likely") %>%
  select(simulation.id, id, round.id, points.per.win, winner, entrant.name, selection) %>%
  mutate(points.awarded = ifelse(selection == winner, points.per.win, 0)) %>%
  group_by(simulation.id, entrant.name) %>%
  summarize(total.points = sum(points.awarded)) %>%
  mutate(placement = rank(desc(total.points), ties.method = "min")) %>%
  inner_join(nc, by = "simulation.id") %>%
  select(simulation.id, placement, entrant.name, total.points, winner) %>%
  rename(champion = winner) %>%
  mutate(won.gold = ifelse(placement == 1, 1, 0),
         won.silver = ifelse(placement == 2, 1, 0),
         won.bronze = ifelse(placement == 3, 1, 0)) %>%
  group_by(entrant.name) %>%
  summarize(wins = sum(won.gold),
            win.rate = mean(won.gold),
            #medal.rate = mean(won.gold + won.silver + won.bronze),
            exp.points = round(mean(total.points),0)) %>%
  arrange(entrant.name)

gc()

write.csv(standings, file = paste0(wd, "2024 Live Standings.csv"))

mean.win.rate <- mean(standings$win.rate)
ggplot(standings,
       aes(x = reorder(entrant.name, win.rate, sum), y = win.rate)) +
  geom_col() +
  geom_text(aes(label = scales::percent(win.rate, accuracy = 0.1)),
            hjust = -0.5) +
  theme_fivethirtyeight() +
  ggtitle("Bracket Win Probability - Entering Sweet 16") +
  xlab("Entrant") +
  ylab("Win Probability") +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  geom_hline(yintercept = mean.win.rate,
             linetype = "dashed",
             color = "red")
View(standings %>% arrange(desc(win.rate)))
