library(readr)
library(tidyverse)
library(ggthemes)

## pre-tourney data
# kenpom.r64.d1.import <- read_csv("Data Analysis/NCB/Data/2024 R64 D1 KenPom Summary.csv")
# kenpom.r64.d2.import <- read_csv("Data Analysis/NCB/Data/2024 R64 D2 KenPom Summary.csv")
# kenpom.r32.d1.import <- read_csv("Data Analysis/NCB/Data/2024 R32 D1 KenPom Summary.csv")
kenpom.r32.d2.import <- read_csv("Data Analysis/NCB/Data/2024 R32 D2 KenPom Summary.csv")
# kenpom.s16.d1.import <- read_csv("Data Analysis/NCB/Data/2023 S16 D1 KenPom Summary.csv")
# kenpom.s16.d2.import <- read_csv("Data Analysis/NCB/Data/2022 S16 D2 KenPom Summary.csv")
# kenpom.e8.d1.import <- read_csv("Data Analysis/NCB/Data/2022 E8 D1 KenPom Summary.csv")

# add pyth
kenpom <-
  kenpom.r32.d2.import %>%
  mutate(raw.pyth = OE^11 / (OE^11 + DE^11), # using 11 because it seems to match what KP does
         adj.pyth = AdjOE^11 / (AdjOE^11 + AdjDE^11))

## load s-curve
scurve.import <- read_csv("Data Analysis/NCB/Data/2024 Bracket.csv")

# add kp rating to bracket
scurve <-
  scurve.import %>%
  inner_join(kenpom, by = c("team" = "TeamName")) %>%
  select(s.curve, team, region, seed, adj.pyth) %>%
  rename(rating = adj.pyth)

## load matchup data
rounds.import <- read_csv("Data Analysis/NCB/Data/Tourney Rounds.csv")
games.import <- read_csv("Data Analysis/NCB/Data/Tourney Games.csv")
matchups.import <- read_csv("Data Analysis/NCB/Data/Tourney Matchups.csv")

## who picked whom
#pickrates.import <- read_csv("Data Analysis/NCB/Data/2022 Who Picked Whom.csv")

## load bracket entries
entry.import <- read_csv("Data Analysis/NCB/Data/2024 Entries.csv")


# add team info into matchups
matchups.tmp <-
  matchups.import %>%
  inner_join(rounds.import, by = c("round.id" = "id")) %>%
  inner_join(scurve, by = c("s.curve.a" = "s.curve")) %>%
  select(id, game.id, round.id, round.name, s.curve.a, team, rating, s.curve.b) %>%
  rename(team.a = team,
         rating.a = rating) %>%
  inner_join(scurve, by = c("s.curve.b" = "s.curve")) %>%
  select(id, game.id, round.id, round.name, s.curve.a, team.a, rating.a, s.curve.b, team, rating) %>%
  rename(team.b = team,
         rating.b = rating) %>%
  mutate(win.prob.a = (rating.a - rating.a*rating.b)/(rating.a + rating.b - 2*rating.a*rating.b),
         win.prob.b = 1 - win.prob.a)

## check game.id
matchups.tmp %>%
  filter(team.a == "Purdue",
         team.b == "Utah St.") %>%
  select(id, game.id)

matchups <-
  matchups.tmp %>%
  mutate(win.prob.a = ifelse(id == 29, 0, win.prob.a), # michigan st. over mississippi st.
         win.prob.b = ifelse(id == 29, 1, win.prob.b),
         win.prob.a = ifelse(id == 24, 0, win.prob.a), # duquesne over byu
         win.prob.b = ifelse(id == 24, 1, win.prob.b),
         win.prob.a = ifelse(id == 11, 1, win.prob.a), # creighton over akron
         win.prob.b = ifelse(id == 11, 0, win.prob.b),
         win.prob.a = ifelse(id == 5, 1, win.prob.a), # arizona over long beach st.
         win.prob.b = ifelse(id == 5, 0, win.prob.b),
         win.prob.a = ifelse(id == 4, 1, win.prob.a), # north carolina over wagner
         win.prob.b = ifelse(id == 4, 0, win.prob.b),
         win.prob.a = ifelse(id == 9, 1, win.prob.a), # illinois over morehead st.
         win.prob.b = ifelse(id == 9, 0, win.prob.b),
         win.prob.a = ifelse(id == 22, 0, win.prob.a), # oregon over south carolina
         win.prob.b = ifelse(id == 22, 1, win.prob.b),
         win.prob.a = ifelse(id == 28, 1, win.prob.a), # dayton over nevada
         win.prob.b = ifelse(id == 28, 0, win.prob.b),
         win.prob.a = ifelse(id == 27, 1, win.prob.a), # texas over colorado st.
         win.prob.b = ifelse(id == 27, 0, win.prob.b),
         win.prob.a = ifelse(id == 10, 0, win.prob.a), # oakland over kentucky
         win.prob.b = ifelse(id == 10, 1, win.prob.b),
         win.prob.a = ifelse(id == 19, 1, win.prob.a), # gonzaga over mcneese st.
         win.prob.b = ifelse(id == 19, 0, win.prob.b),
         win.prob.a = ifelse(id == 8, 1, win.prob.a), # iowa st. over south dakota st.
         win.prob.b = ifelse(id == 8, 0, win.prob.b),
         win.prob.a = ifelse(id == 6, 1, win.prob.a), # tennessee over saint peter's
         win.prob.b = ifelse(id == 6, 0, win.prob.b),
         win.prob.a = ifelse(id == 23, 0, win.prob.a), # n.c. state over texas tech
         win.prob.b = ifelse(id == 23, 1, win.prob.b),
         win.prob.a = ifelse(id == 25, 1, win.prob.a), # washington st. over drake
         win.prob.b = ifelse(id == 25, 0, win.prob.b),
         win.prob.a = ifelse(id == 14, 1, win.prob.a), # kansas over samford
         win.prob.b = ifelse(id == 14, 0, win.prob.b),
         win.prob.a = ifelse(id == 32, 0, win.prob.a), # northwestern over florida atlantic
         win.prob.b = ifelse(id == 32, 1, win.prob.b),
         win.prob.a = ifelse(id == 12, 1, win.prob.a), # baylor over colgate
         win.prob.b = ifelse(id == 12, 0, win.prob.b),
         win.prob.a = ifelse(id == 17, 1, win.prob.a), # san diego st. over uab
         win.prob.b = ifelse(id == 17, 0, win.prob.b),
         win.prob.a = ifelse(id == 7, 1, win.prob.a), # marquette over western kentucky
         win.prob.b = ifelse(id == 7, 0, win.prob.b),
         win.prob.a = ifelse(id == 21, 1, win.prob.a), # clemson over new mexico
         win.prob.b = ifelse(id == 21, 0, win.prob.b),
         win.prob.a = ifelse(id == 1, 1, win.prob.a), # connecticut over stetson
         win.prob.b = ifelse(id == 1, 0, win.prob.b),
         win.prob.a = ifelse(id == 16, 0, win.prob.a), # yale over auburn
         win.prob.b = ifelse(id == 16, 1, win.prob.b),
         win.prob.a = ifelse(id == 26, 0, win.prob.a), # colorado over florida
         win.prob.b = ifelse(id == 26, 1, win.prob.b),
         win.prob.a = ifelse(id == 31, 0, win.prob.a), # texas a&m over nebraska
         win.prob.b = ifelse(id == 31, 1, win.prob.b),
         win.prob.a = ifelse(id == 15, 1, win.prob.a), # duke over vermont
         win.prob.b = ifelse(id == 15, 0, win.prob.b),
         win.prob.a = ifelse(id == 3, 1, win.prob.a), # purdue over grambling st.
         win.prob.b = ifelse(id == 3, 0, win.prob.b),
         win.prob.a = ifelse(id == 13, 1, win.prob.a), # alabama over charleston
         win.prob.b = ifelse(id == 13, 0, win.prob.b),
         win.prob.a = ifelse(id == 18, 0, win.prob.a), # james madison over wisconsin
         win.prob.b = ifelse(id == 18, 1, win.prob.b),
         win.prob.a = ifelse(id == 2, 1, win.prob.a), # houston over longwood
         win.prob.b = ifelse(id == 2, 0, win.prob.b),
         win.prob.a = ifelse(id == 30, 1, win.prob.a), # utah st. over tcu
         win.prob.b = ifelse(id == 30, 0, win.prob.b),
         win.prob.a = ifelse(id == 20, 0, win.prob.a), # grand canyon over saint mary's
         win.prob.b = ifelse(id == 20, 1, win.prob.b),
         ## round of 32
         win.prob.a = ifelse(id == 37, 1, win.prob.a), # arizona over dayton
         win.prob.b = ifelse(id == 37, 0, win.prob.b),
         win.prob.a = ifelse(id == 46, 0, win.prob.a), # gonzaga over kansas
         win.prob.b = ifelse(id == 46, 1, win.prob.b),
         win.prob.a = ifelse(id == 52, 1, win.prob.a), # north carolina over michigan st.
         win.prob.b = ifelse(id == 52, 0, win.prob.b),
         win.prob.a = ifelse(id == 40, 1, win.prob.a), # iowa st. over washington st.
         win.prob.b = ifelse(id == 40, 0, win.prob.b),
         win.prob.a = ifelse(id == 90, 0, win.prob.a), # n.c. state over oakland
         win.prob.b = ifelse(id == 90, 1, win.prob.b),
         win.prob.a = ifelse(id == 38, 1, win.prob.a), # tennessee over texas
         win.prob.b = ifelse(id == 38, 0, win.prob.b),
         win.prob.a = ifelse(id == 57, 1, win.prob.a), # illinois over duquesne
         win.prob.b = ifelse(id == 57, 0, win.prob.b),
         win.prob.a = ifelse(id == 59, 1, win.prob.a), # creighton over oregon
         win.prob.b = ifelse(id == 59, 0, win.prob.b),
         win.prob.a = ifelse(id == 55, 1, win.prob.a), # marquette over colorado
         win.prob.b = ifelse(id == 55, 0, win.prob.b),
         win.prob.a = ifelse(id == 35, 1, win.prob.a), # purdue over utah st.
         win.prob.b = ifelse(id == 35, 0, win.prob.b))


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

write.csv(standings, file = "Data Analysis/NCB/Data/2024 Live Standings.csv")

mean.win.rate <- mean(standings$win.rate)
ggplot(standings,
       aes(x = reorder(entrant.name, win.rate, sum), y = win.rate)) +
  geom_col() +
  geom_text(aes(label = scales::percent(win.rate, accuracy = 0.1)),
            hjust = -0.5) +
  theme_fivethirtyeight() +
  ggtitle("Bracket Win Probability - Start of Tournament") +
  xlab("Entrant") +
  ylab("Win Probability") +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  geom_hline(yintercept = mean.win.rate,
             linetype = "dashed",
             color = "red")
View(standings %>% arrange(desc(win.rate)))

win.rate.by.champ <-
  summary %>%
  mutate(won.gold = ifelse(placement == 1, 1, 0),
         won.silver = ifelse(placement == 2, 1, 0),
         won.bronze = ifelse(placement == 3, 1, 0)) %>%
  group_by(entrant.name, champion) %>%
  summarize(freq = n(),
            wins = sum(won.gold),
            win.rate = mean(won.gold),
            medal.rate = mean(won.gold + won.silver + won.bronze),
            exp.points = mean(total.points)) %>%
  arrange(desc(wins))

## extra stats
placement.distribution <-
  summary %>%
  group_by(entrant.name, placement) %>%
  summarize(freq = n(),
            prob = n() / 10000)

gg.pd.data <- placement.distribution %>% filter(entrant.name == "David McDevitt")
ggplot(gg.pd.data, aes(x = placement, y = prob)) +
  geom_col() +
  scale_x_discrete(limits = factor(1:6)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_fivethirtyeight() +
  ggtitle(paste(gg.pd.data$entrant.name, "Placement Distribution"))

bracket.overlap <-
  entry.import %>%
  inner_join(rounds.import, by = c("round.id" = "id")) %>%
  select(game.id, entrant.id, entrant.name, selection, points.per.win) %>%
  inner_join(entry.import, by = c("game.id", "selection")) %>%
  filter(entrant.id.x < entrant.id.y) %>%
  mutate(entrant.duo = paste(entrant.name.x, "and", entrant.name.y)) %>%
  select(game.id, entrant.duo, selection, points.per.win) %>%
  group_by(entrant.duo) %>%
  summarize(points.overlap = sum(points.per.win),
            pct.overlap = round(sum(points.per.win) / (320 * 6), 3)) %>%
  arrange(desc(points.overlap))

##########################################
## Swing Games
##########################################

swing.game.id <- 50

matchups.a <-
  matchups %>%
  mutate(win.prob.a = ifelse(game.id == swing.game.id, 1, win.prob.a),
         win.prob.b = ifelse(game.id == swing.game.id, 0, win.prob.b))

matchups.b <-
  matchups %>%
  mutate(win.prob.a = ifelse(game.id == swing.game.id, 0, win.prob.a),
         win.prob.b = ifelse(game.id == swing.game.id, 1, win.prob.b))

simulations <- 25000
simulation.ids <- data.frame(simulation.id = c(1:simulations))
games.simulation <- merge(games.import, simulation.ids)

set.seed(316)
# create round of 64
r64.a <-
  games.simulation %>%
  filter(round.id == 1) %>%
  inner_join(matchups.a, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, s.curve.a, team.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  mutate(result = runif(n = 32 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

# now do round of 32, and so on
r32.a <-
  games.simulation %>%
  filter(round.id == 2) %>%
  inner_join(matchups.a, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(r64.a, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(r64.a, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

s16.a <-
  games.simulation %>%
  filter(round.id == 3) %>%
  inner_join(matchups.a, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(r32.a, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(r32.a, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

e8.a <-
  games.simulation %>%
  filter(round.id == 4) %>%
  inner_join(matchups.a, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(s16.a, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(s16.a, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

f4.a <-
  games.simulation %>%
  filter(round.id == 5) %>%
  inner_join(matchups.a, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(e8.a, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(e8.a, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

nc.a <-
  games.simulation %>%
  filter(round.id == 6) %>%
  inner_join(matchups.a, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(f4.a, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(f4.a, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

set.seed(317)
r64.b <-
  games.simulation %>%
  filter(round.id == 1) %>%
  inner_join(matchups.b, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, s.curve.a, team.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  mutate(result = runif(n = 32 * simulations),
         winner = ifelse(result <= win.prob.a, team.a, team.b),
         winner.s.curve = ifelse(result <= win.prob.a, s.curve.a, s.curve.b))

r32.b <-
  games.simulation %>%
  filter(round.id == 2) %>%
  inner_join(matchups.b, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(r64.b, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(r64.b, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

s16.b <-
  games.simulation %>%
  filter(round.id == 3) %>%
  inner_join(matchups.b, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(r32.b, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(r32.b, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

e8.b <-
  games.simulation %>%
  filter(round.id == 4) %>%
  inner_join(matchups.b, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(s16.b, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(s16.b, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

f4.b <-
  games.simulation %>%
  filter(round.id == 5) %>%
  inner_join(matchups.b, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(e8.b, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(e8.b, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

nc.b <-
  games.simulation %>%
  filter(round.id == 6) %>%
  inner_join(matchups.b, by = c("id" = "game.id", "round.id")) %>%
  select(simulation.id, id, round.id, id.y, team.a, s.curve.a, s.curve.b, team.b, win.prob.a) %>%
  rename(game.id = id,
         matchup.id = id.y) %>%
  inner_join(f4.b, by = c("s.curve.a" = "winner.s.curve", "simulation.id")) %>%
  inner_join(f4.b, by = c("s.curve.b.x" = "winner.s.curve", "simulation.id")) %>%
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

## combine all of the rounds into one table
final.results.a <-
  rbind(r64.a, r32.a, s16.a, e8.a, f4.a, nc.a) %>%
  select(simulation.id, game.id, winner)
final.results.b <-
  rbind(r64.b, r32.b, s16.b, e8.b, f4.b, nc.b) %>%
  select(simulation.id, game.id, winner)

## assess brackets
performance.a <-
  games.simulation %>%
  inner_join(rounds.import, by = c("round.id" = "id")) %>%
  select(simulation.id, id, round.id, points.per.win) %>%
  inner_join(final.results.a, by = c("id" = "game.id", "simulation.id")) %>%
  inner_join(entry.import, by = c("id" = "game.id", "round.id")) %>%
  filter(entrant.name != "Chalk",
         entrant.name != "Most Likely") %>%
  select(simulation.id, id, round.id, points.per.win, winner, entrant.name, selection) %>%
  mutate(points.awarded = ifelse(selection == winner, points.per.win, 0))
performance.b <-
  games.simulation %>%
  inner_join(rounds.import, by = c("round.id" = "id")) %>%
  select(simulation.id, id, round.id, points.per.win) %>%
  inner_join(final.results.b, by = c("id" = "game.id", "simulation.id")) %>%
  inner_join(entry.import, by = c("id" = "game.id", "round.id")) %>%
  filter(entrant.name != "Chalk",
         entrant.name != "Most Likely") %>%
  select(simulation.id, id, round.id, points.per.win, winner, entrant.name, selection) %>%
  mutate(points.awarded = ifelse(selection == winner, points.per.win, 0))

summary.a <-
  performance.a %>%
  group_by(simulation.id, entrant.name) %>%
  summarize(total.points = sum(points.awarded)) %>%
  mutate(placement = rank(desc(total.points), ties.method = "min"),
         random.tiebreak = rank(desc(total.points), ties.method = "random")) %>%
  inner_join(nc.a, by = "simulation.id") %>%
  select(simulation.id, placement, entrant.name, total.points, winner, random.tiebreak) %>%
  rename(champion = winner)
summary.b <-
  performance.b %>%
  group_by(simulation.id, entrant.name) %>%
  summarize(total.points = sum(points.awarded)) %>%
  mutate(placement = rank(desc(total.points), ties.method = "min"),
         random.tiebreak = rank(desc(total.points), ties.method = "random")) %>%
  inner_join(nc.b, by = "simulation.id") %>%
  select(simulation.id, placement, entrant.name, total.points, winner, random.tiebreak) %>%
  rename(champion = winner)

standings.a <-
  summary.a %>%
  mutate(won.gold = ifelse(placement == 1, 1, 0),
         won.silver = ifelse(placement == 2, 1, 0),
         won.bronze = ifelse(placement == 3, 1, 0)) %>%
  group_by(entrant.name) %>%
  summarize(win.rate = mean(won.gold),
            medal.rate = mean(won.gold + won.silver + won.bronze),
            exp.points = mean(total.points)) %>%
  arrange(desc(win.rate))
standings.b <-
  summary.b %>%
  mutate(won.gold = ifelse(placement == 1, 1, 0),
         won.silver = ifelse(placement == 2, 1, 0),
         won.bronze = ifelse(placement == 3, 1, 0)) %>%
  group_by(entrant.name) %>%
  summarize(win.rate = mean(won.gold),
            medal.rate = mean(won.gold + won.silver + won.bronze),
            exp.points = mean(total.points)) %>%
  arrange(desc(win.rate))

standings.lift <-
  standings.a %>%
  inner_join(standings.b, by = "entrant.name") %>%
  select(entrant.name, win.rate.x, win.rate.y) %>%
  rename(win.rate.a = win.rate.x,
         win.rate.b = win.rate.y) %>%
  mutate(win.rate.delta = win.rate.a - win.rate.b) %>%
  arrange(desc(win.rate.delta))
View(standings.lift)
