library(tidyverse)
library(ggthemes)

library(readxl)
nfl.schedule.import <- read_excel("Data Analysis/Team Super League/2023-24 NFL Schedule.xlsx")
nfl.teams <-
  read_excel("Data Analysis/Team Super League/2023-24 Teams.xlsx") %>%
  filter(league == "NFL")
nfl.team.strength.import <- read_excel("Data Analysis/Team Super League/2023-24 NFL Team Strength.xlsx", sheet = "Team Strengths")
tsl.teams <- read_excel("Data Analysis/Team Super League/2023-24 Teams.xlsx")
tsl.scoring <- data.frame(placement = c(1:16),
                          tsl_points = c(32, 28, 25, 22, 19, 17, 15,
                                         8, 7, 6, 5, 4, 3, 2, 1, 0))

## import playoff matchup data
rounds.import <- read_excel("Data Analysis/Team Super League/Playoff Rounds.xlsx", sheet = "NFL")
games.import <- read_excel("Data Analysis/Team Super League/Playoff Games.xlsx", sheet = "NFL")
matchups.import <- read_excel("Data Analysis/Team Super League/Playoff Matchups.xlsx", sheet = "NFL")

hfa <- 0.03 ## use 53% win rate for home teams

nfl.schedule.base <-
  nfl.schedule.import %>%
  inner_join(nfl.team.strength.import, by = c("home_team" = "team")) %>%
  rename(home_win_rate = win_rate) %>%
  mutate(home_win_rate = home_win_rate + hfa / 2) %>%
  inner_join(nfl.team.strength.import, by = c("away_team" = "team")) %>%
  rename(away_win_rate = win_rate) %>%
  mutate(away_win_rate = away_win_rate - hfa / 2) %>%
  mutate(home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob)

# fix win rates to account for SOS
nfl.sos <-
  rbind(
    nfl.schedule.base %>%
      group_by(home_team) %>%
      summarize(cum_sos = sum(away_win_rate)) %>%
      rename(team = home_team) %>%
      ungroup(),
    nfl.schedule.base %>%
      group_by(away_team) %>%
      summarize(cum_sos = sum(home_win_rate)) %>%
      rename(team = away_team) %>%
      ungroup()
  ) %>%
  group_by(team) %>%
  summarize(sos = sum(cum_sos) / 17)

nfl.team.strength <-
  nfl.team.strength.import %>%
  left_join(nfl.sos, by = "team") %>%
  mutate(adj_win_rate = ifelse(!is.na(sos), sos, 0.5) / 0.5 * win_rate,
         exp_wins = win_rate * 17,
         adj_wins = adj_win_rate * 17)

set.seed(907)
simulations <- 10000
nfl.schedule <- data.frame(id = c(1:(nrow(nfl.schedule.import)*simulations)),
                           season_id = rep(c(1:simulations), each = nrow(nfl.schedule.import)),
                           week = nfl.schedule.import$week,
                           home_team = nfl.schedule.import$home_team,
                           away_team = nfl.schedule.import$away_team)

alpha = 5.25
beta = alpha / (8.5 / 17) - alpha
sqrt((alpha*beta / ((alpha + beta)^2 * (alpha + beta + 1)))) * 17

nfl.team.seasons <- data.frame(id = c(1:(nrow(nfl.team.strength)*simulations)),
                               season_id = rep(c(1:simulations), each = nrow(nfl.team.strength)),
                               team = nfl.team.strength$team,
                               team_win_rate = nfl.team.strength$adj_win_rate,
                               season_win_rate = rbeta(nrow(nfl.team.strength)*simulations,
                                                       shape1 = alpha,
                                                       shape2 = alpha / nfl.team.strength$adj_win_rate - alpha)) %>%
  mutate(season_wins = season_win_rate * 17)

nfl.seasons <-
  nfl.schedule %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "home_team" = "team")) %>%
  mutate(home_win_rate = season_win_rate + hfa / 2) %>%
  select(-season_win_rate) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "away_team" = "team")) %>%
  mutate(away_win_rate = season_win_rate - hfa / 2) %>%
  select(-season_win_rate) %>%
  mutate(home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob)

nfl.simulations <-
  nfl.seasons %>%
  mutate(rng = runif(nrow(nfl.seasons)),
         home_win = ifelse(rng < home_win_prob, 1, 0),
         away_win = 1 - home_win)

home.performance <-
  nfl.simulations %>%
  group_by(season_id, home_team) %>%
  summarize(wins = sum(home_win)) %>%
  ungroup() %>%
  rename(team = home_team)

away.performance <-
  nfl.simulations %>%
  group_by(season_id, away_team) %>%
  summarize(wins = sum(away_win)) %>%
  ungroup() %>%
  rename(team = away_team)

team.performance <-
  home.performance %>%
  inner_join(away.performance, by = c("season_id", "team")) %>%
  mutate(wins = wins.x + wins.y,
         losses = 17 - wins) %>%
  select(-wins.x, -wins.y) %>%
  inner_join(nfl.teams %>% select(team, conference, division), by = "team") %>%
  group_by(season_id, conference, division) %>%
  mutate(division_placement = rank(desc(wins), ties.method = "random")) %>%
  ungroup() %>%
  group_by(season_id, conference) %>%
  mutate(conference_placement = rank(desc(ifelse(division_placement == 1, wins*6, wins)), ties.method = "random")) %>%
  inner_join(tsl.scoring, by = c("conference_placement" = "placement"))

division.winners <-
  team.performance %>%
  filter(division_placement == 1) %>%
  rename(seed = conference_placement) %>%
  ungroup()

wildcard.teams <-
  team.performance %>%
  filter(division_placement > 1) %>%
  rename(seed = conference_placement) %>%
  filter(seed <= 8) %>%
  mutate(team = ifelse(seed == 8, "BYE", team))

wildcard.seeding <-
  rbind(division.winners, wildcard.teams) %>%
  select(season_id, team, conference, seed)

wildcard.round <-
  matchups.import %>%
  inner_join(rounds.import %>% select(id, points_per_win), by = c("round_id" = "id")) %>%
  filter(round_id == 1) %>%
  inner_join(wildcard.seeding, by = c("home_seed" = "seed")) %>%
  rename(home_team = team) %>%
  inner_join(wildcard.seeding, by = c("season_id", "away_seed" = "seed", "conference")) %>%
  rename(away_team = team) %>%
  select(season_id, id, game_id, round_id, points_per_win, home_seed, home_team, away_seed, away_team, conference) %>%
  rename(matchup_id = id) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "home_team" = "team")) %>%
  mutate(home_win_rate = season_win_rate + hfa / 2) %>%
  select(-season_win_rate) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "away_team" = "team")) %>%
  mutate(away_win_rate = season_win_rate - hfa / 2) %>%
  select(-season_win_rate) %>%
  mutate(home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob,
         rng = runif(2*simulations*(nrow(matchups.import %>% filter(round_id == 1)))),
         winner = ifelse(rng < home_win_prob, home_team, away_team),
         winner_seed = ifelse(rng < home_win_prob, home_seed, away_seed)) %>%
  group_by(season_id, conference) %>%
  mutate(winner_reseed = rank(winner_seed)) %>%
  ungroup()

divisional.seeding <-
  wildcard.round %>%
  select(season_id, winner, conference, winner_reseed) %>%
  rename(team = winner,
         seed = winner_reseed)

divisional.round <-
  matchups.import %>%
  inner_join(rounds.import %>% select(id, points_per_win), by = c("round_id" = "id")) %>%
  filter(round_id == 2) %>%
  inner_join(divisional.seeding, by = c("home_seed" = "seed")) %>%
  rename(home_team = team) %>%
  inner_join(divisional.seeding, by = c("season_id", "away_seed" = "seed", "conference")) %>%
  rename(away_team = team) %>%
  select(season_id, id, game_id, round_id, points_per_win, home_seed, home_team, away_seed, away_team, conference) %>%
  rename(matchup_id = id) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "home_team" = "team")) %>%
  mutate(home_win_rate = season_win_rate + hfa / 2) %>%
  select(-season_win_rate) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "away_team" = "team")) %>%
  mutate(away_win_rate = season_win_rate - hfa / 2) %>%
  select(-season_win_rate) %>%
  mutate(home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob,
         rng = runif(2*simulations*(nrow(matchups.import %>% filter(round_id == 2)))),
         winner = ifelse(rng < home_win_prob, home_team, away_team),
         winner_seed = ifelse(rng < home_win_prob, home_seed, away_seed)) %>%
  group_by(season_id, conference) %>%
  mutate(winner_reseed = rank(winner_seed)) %>%
  ungroup()

conference.seeding <-
  divisional.round %>%
  select(season_id, winner, conference, winner_reseed) %>%
  rename(team = winner,
         seed = winner_reseed)

conference.championship <-
  matchups.import %>%
  inner_join(rounds.import %>% select(id, points_per_win), by = c("round_id" = "id")) %>%
  filter(round_id == 3) %>%
  inner_join(conference.seeding, by = c("home_seed" = "seed")) %>%
  rename(home_team = team) %>%
  inner_join(conference.seeding, by = c("season_id", "away_seed" = "seed", "conference")) %>%
  rename(away_team = team) %>%
  select(season_id, id, game_id, round_id, points_per_win, home_seed, home_team, away_seed, away_team, conference) %>%
  rename(matchup_id = id) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "home_team" = "team")) %>%
  mutate(home_win_rate = season_win_rate + hfa / 2) %>%
  select(-season_win_rate) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "away_team" = "team")) %>%
  mutate(away_win_rate = season_win_rate - hfa / 2) %>%
  select(-season_win_rate) %>%
  mutate(home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob,
         rng = runif(2*simulations*(nrow(matchups.import %>% filter(round_id == 3)))),
         winner = ifelse(rng < home_win_prob, home_team, away_team),
         winner_seed = ifelse(rng < home_win_prob, home_seed, away_seed)) %>%
  group_by(season_id, conference) %>%
  mutate(winner_reseed = rank(winner_seed)) %>%
  ungroup()

afc.winners <-
  conference.championship %>%
  filter(conference == "AFC") %>%
  select(season_id, winner) %>%
  rename(afc_team = winner)

nfc.winners <-
  conference.championship %>%
  filter(conference == "NFC") %>%
  select(season_id, winner) %>%
  rename(nfc_team = winner)

super.bowl <-
  afc.winners %>%
  inner_join(nfc.winners, by = "season_id") %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "afc_team" = "team")) %>%
  mutate(afc_win_rate = season_win_rate) %>%
  select(-season_win_rate) %>%
  inner_join(nfl.team.seasons %>% select(season_id, team, season_win_rate), by = c("season_id", "nfc_team" = "team")) %>%
  mutate(nfc_win_rate = season_win_rate) %>%
  select(-season_win_rate) %>%
  mutate(points_per_win = 20,
         afc_win_prob = (afc_win_rate - afc_win_rate * nfc_win_rate) / 
           (afc_win_rate + nfc_win_rate - 2 * afc_win_rate * nfc_win_rate),
         nfc_win_prob = 1 - afc_win_prob,
         rng = runif(nrow(afc.winners)),
         winner = ifelse(rng < afc_win_prob, afc_team, nfc_team))

regular.season.scoring <-
  team.performance %>%
  select(season_id, team, wins, tsl_points)

playoff.scoring <-
  rbind(
    wildcard.round %>% select(season_id, winner, points_per_win),
    divisional.round %>% select(season_id, winner, points_per_win),
    conference.championship %>% select(season_id, winner, points_per_win),
    super.bowl %>% select(season_id, winner, points_per_win)
  ) %>%
  group_by(season_id, winner) %>%
  summarize(tsl_points = sum(points_per_win)) %>%
  ungroup() %>%
  rename(team = winner)

team.summary <-
  regular.season.scoring %>%
  left_join(playoff.scoring, by = c("season_id", "team")) %>%
  rename(tsl_season_points = tsl_points.x,
         tsl_playoff_points = tsl_points.y) %>%
  mutate(tsl_playoff_points = ifelse(!is.na(tsl_playoff_points), tsl_playoff_points, 0),
         total_tsl_points = tsl_season_points + tsl_playoff_points,
         sb_winner = ifelse(tsl_playoff_points == 50, 1, 0),
         conf_champion = ifelse(tsl_playoff_points >= 30, 1, 0),
         won_division = ifelse(tsl_season_points >= 22, 1, 0),
         made_playoffs = ifelse(tsl_season_points >= 15, 1, 0)) %>%
  group_by(team) %>%
  summarize(avg_wins = mean(wins),
            avg_season_points = mean(tsl_season_points),
            avg_playoff_points = mean(tsl_playoff_points),
            avg_total_points = mean(total_tsl_points),
            sb_rate = mean(sb_winner),
            conf_champ_rate = mean(conf_champion),
            div_win_rate = mean(won_division),
            playoff_rate = mean(made_playoffs)) %>%
  ungroup()

# team.actuals <- data.frame(team = c("Baltimore Ravens", "Detroit Lions",
#                                     "Kansas City Chiefs", "San Francisco 49ers"),
#                            wins = c(13, 10, 11, 13))
# 
# simulation.matches <-
#   team.performance %>%
#   inner_join(team.actuals, by = c("team", "wins")) %>%
#   inner_join(playoff.scoring %>% filter(tsl_points >= 15), by = c("season_id", "team")) %>%
#   inner_join(nfl.team.seasons, by = c("season_id", "team")) %>%
#   group_by(team) %>%
#   summarize(estimated_win_rate = mean(season_win_rate))

# rs.performance <-
#   team.performance %>%
#   group_by(team, wins) %>%
#   summarize(freq = n()) %>%
#   ungroup() %>%
#   mutate(prop = freq / simulations)

# rs.performance %>%
#   filter(team == "Detroit Lions")
# sd(rs.performance$prop)

# View(
#   team.summary %>%
#     inner_join(nfl.teams, by = "team") %>%
#     filter(conference == "NFC",
#            division == "South") %>%
#     arrange(desc(div_win_rate))
# )

tsl.rs.summary <-
  regular.season.scoring %>%
  inner_join(tsl.teams, by = "team") %>%
  group_by(season_id, owner) %>%
  summarize(tsl_cost = sum(cost),
            tsl_rs_points = sum(tsl_points)) %>%
  ungroup()

tsl.po.summary <-
  playoff.scoring %>%
  inner_join(tsl.teams, by = "team") %>%
  group_by(season_id, owner) %>%
  summarize(tsl_po_points = sum(tsl_points)) %>%
  ungroup()

tsl.summary <-
  tsl.rs.summary %>%
  left_join(tsl.po.summary, by = c("season_id", "owner")) %>%
  mutate(tsl_po_points = ifelse(!is.na(tsl_po_points), tsl_po_points, 0),
         tsl_points = tsl_rs_points + tsl_po_points,
         tsl_net = tsl_points - tsl_cost)

tsl.averages <-
  tsl.summary %>%
  group_by(owner) %>%
  summarize(average = mean(tsl_net))

ggplot(tsl.summary, aes(x = tsl_net, fill = owner)) +
  geom_density(alpha = .75) +
  geom_vline(xintercept = tsl.averages$average[1], color = "#FF2700") +
  geom_vline(xintercept = tsl.averages$average[2], color = "#77AB43") +
  geom_vline(xintercept = tsl.averages$average[3], color = "#008FD5") +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  ggtitle("NFL TSL Preseason Projections")
tsl.averages
