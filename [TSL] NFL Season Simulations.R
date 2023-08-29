library(tidyverse)

library(readxl)
nfl.schedule.import <- read_excel("Data Analysis/Team Super League/2023-24 NFL Schedule.xlsx")
nfl.teams <-
  read_excel("Data Analysis/Team Super League/2023-24 Teams.xlsx") %>%
  filter(league == "NFL")
nfl.team.strength.import <- read_excel("Data Analysis/Team Super League/2023-24 NFL Team Strength.xlsx", sheet = "Team Strengths")
tsl.scoring <- data.frame(placement = c(1:16),
                          tsl_points = c(32, 28, 25, 22, 19, 17, 15,
                                         8, 7, 6, 5, 4, 3, 2, 1, 0))

## import playoff matchup data
rounds.import <- read_excel("Data Analysis/Team Super League/NFL Playoff Rounds.xlsx")
games.import <- read_excel("Data Analysis/Team Super League/NFL Playoff Games.xlsx")
matchups.import <- read_excel("Data Analysis/Team Super League/NFL Playoff Matchups.xlsx")


nfl.schedule.base <-
  nfl.schedule.import %>%
  inner_join(nfl.team.strength.import, by = c("home_team" = "team")) %>%
  rename(home_win_rate = win_rate) %>%
  inner_join(nfl.team.strength.import, by = c("away_team" = "team")) %>%
  rename(away_win_rate = win_rate) %>%
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
  inner_join(nfl.sos, by = "team") %>%
  mutate(adj_win_rate = sos / 0.5 * win_rate)

nfl.schedule <-
  nfl.schedule.import %>%
  inner_join(nfl.team.strength %>% select(team, adj_win_rate), by = c("home_team" = "team")) %>%
  rename(home_win_rate = adj_win_rate) %>%
  inner_join(nfl.team.strength %>% select(team, adj_win_rate), by = c("away_team" = "team")) %>%
  rename(away_win_rate = adj_win_rate) %>%
  mutate(home_win_prob = (home_win_rate - home_win_rate * away_win_rate) / 
           (home_win_rate + away_win_rate - 2 * home_win_rate * away_win_rate),
         away_win_prob = 1 - home_win_prob)

set.seed(907)
simulations <- 10000
nfl.seasons <- data.frame(id = c(1:(nrow(nfl.schedule)*simulations)),
                          season_id = rep(c(1:simulations), each = nrow(nfl.schedule)),
                          week = nfl.schedule$week,
                          home_team = nfl.schedule$home_team,
                          away_team = nfl.schedule$away_team,
                          home_win_prob = nfl.schedule$home_win_prob,
                          away_win_prob = nfl.schedule$away_win_prob)

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
  group_by(season_id, conference) %>%
  mutate(conference_placement = rank(desc(wins), ties.method = "random")) %>%
  ungroup() %>%
  group_by(season_id, conference, division) %>%
  mutate(division_placement = rank(conference_placement)) %>%
  ungroup() %>%
  inner_join(tsl.scoring, by = c("conference_placement" = "placement"))

division.winners <-
  team.performance %>%
  filter(division_placement == 1) %>%
  group_by(season_id, conference) %>%
  mutate(seed = rank(conference_placement)) %>%
  ungroup()

wildcard.teams <-
  team.performance %>%
  filter(division_placement > 1) %>%
  group_by(season_id, conference) %>%
  mutate(seed = rank(conference_placement) + 4) %>%
  ungroup() %>%
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
  mutate(home_win_prob = ifelse(away_team == "BYE", 1, .5),
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
  mutate(home_win_prob = .5,
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
  mutate(home_win_prob = .5,
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
  mutate(points_per_win = 20,
         afc_win_prob = .5,
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
