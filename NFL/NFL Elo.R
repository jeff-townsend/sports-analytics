
team.elo <- data.frame(team = character(),
                       elo = double())

elo.games <- data.frame(game_id = integer(),
                        season = integer(),
                        week = integer(),
                        team = character(),
                        elo = double(),
                        opponent = character(),
                        opp_elo = double(),
                        is_home = integer(),
                        expectation = double(),
                        result = integer(),
                        pf = integer(),
                        pa = integer(),
                        elo_delta = double(),
                        sq_err = double())

team.metrics <- data.frame(team = character(),
                           pd = double())

opp.metrics <- data.frame(team = character(),
                          opp_pd = double())

k <- 30
hfa <- 39
s <- start
for(s in start:2024){
  
  team.elo <-
    teams %>%
    filter(season == s) %>%
    left_join(team.elo, by = "team") %>%
    mutate(rel_elo = ifelse(is.na(elo), 0, elo - 1500)) %>% # set relative elo for model
    left_join(team.metrics, by = "team") %>%
    mutate(pd = ifelse(is.na(pd), 0, pd)) %>% # set pd to 0 for the first season
    left_join(opp.metrics, by = "team") %>%
    mutate(opp_pd = ifelse(is.na(opp_pd), 0, opp_pd)) %>% # set opp_pd to 0 for first season
    mutate(elo = 1500 + 0 * rel_elo + 10.8 * pd + 10.1 * opp_pd) %>%
    select(team, elo)
  
  g.max <- as.numeric(with(scores.long %>% filter(season == s), max(game_rank)))
  
  g <- 1
  for(g in 1:g.max){
    
    game <-
      scores.long %>%
      filter(season == s,
             game_rank == g) %>%
      inner_join(team.elo, by = "team") %>%
      inner_join(team.elo %>% rename(opp_elo = elo), by = c("opponent" = "team")) %>%
      mutate(game_elo = elo + is_home * hfa,
             opp_game_elo = opp_elo + (1 - is_home) * hfa,
             elo_diff = game_elo - opp_game_elo,
             expectation = 1 / (1 + 10 ^ (-elo_diff / 400)),
             elo_delta = k * (result - expectation),
             new_elo = elo + elo_delta)
    
    # update post_elo and elo_delta from current week
    team.elo <-
      team.elo %>%
      left_join(game %>% select(team, new_elo), by = "team") %>%
      mutate(elo = ifelse(is.na(new_elo), elo, new_elo)) %>%
      select(-new_elo)
    
    # store elo results to measure accuracy
    elo.games <-
      rbind(
        elo.games,
        game %>%
          select(game_id, season, week, team, elo, opponent, opp_elo,
                 is_home, expectation, result, pf, pa, elo_delta) %>%
          mutate(sq_error = (result - expectation) ^ 2))
    
    g <- g + 1
    
  }
  
  team.metrics <-
    elo.games %>%
    filter(season == s) %>%
    group_by(team) %>%
    summarize(pd = mean(pf - pa))
  
  opp.metrics <-
    elo.games %>%
    filter(season == s) %>%
    inner_join(team.metrics %>% rename(opp_pd = pd), by = c("opponent" = "team")) %>%
    group_by(team) %>%
    summarize(opp_pd = mean(opp_pd))
  
  s <- s + 1
    
}

# measure RMSE
elo.games %>%
  filter(season < 2024) %>%
  summarize(rmse = sqrt(mean(sq_error)))
# pb: 0.479 -- k-factor 50; hfa 39; 1500 elo
# pb: 0.479 -- k-factor 25; hfa 40; retain elo
# pb: 0.474 -- k-factor 30; hfa 39; use PD model

# calculate home-field advantage
elo.games %>%
  filter(season < 2024,
         is_home == 1) %>%
  #group_by(season) %>%
  summarize(hfa = mean(result - expectation),
            hfa_elo = -400*log10(1 / (hfa + 0.5) - 1))

# measure YOY regression
starting.week <-
  elo.games %>%
  group_by(team, season) %>%
  summarize(starting_week = min(week))

starting.elo <-
  elo.games %>%
  inner_join(starting.week, by = c("team", "season", "week" = "starting_week")) %>%
  rename(starting_elo = elo) %>%
  select(team, season, starting_elo)

final.week <-
  elo.games %>%
  group_by(team, season) %>%
  summarize(final_week = max(week))

final.elo <-
  elo.games %>%
  inner_join(final.week, by = c("team", "season", "week" = "final_week")) %>%
  mutate(final_elo = elo + elo_delta) %>%
  select(team, season, final_elo)

season.metrics <-
  elo.games %>%
  group_by(team, season) %>%
  summarize(pd = mean(pf - pa))

sos.metrics <-
  elo.games %>%
  inner_join(season.metrics %>% rename(opp_pd = pd), by = c("opponent" = "team", "season")) %>%
  group_by(team, season) %>%
  summarize(opp_pd = mean(opp_pd))

season.elo <-
  starting.elo %>%
  filter(season != 2024) %>%
  inner_join(final.elo, by = c("team", "season")) %>%
  mutate(elo_delta = final_elo - starting_elo) %>%
  inner_join(season.metrics, by = c("team", "season")) %>%
  inner_join(sos.metrics, by = c("team", "season")) %>%
  mutate(adj_pd = pd + opp_pd)

ggplot(season.elo %>% filter(season != 2014), aes(x = starting_elo, y = elo_delta)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("Starting Elo") +
  ylab("Elo Change")
with(season.elo, cor(starting_elo, elo_delta)^2)

# YOY data
yoy.elo <-
  season.elo %>%
  select(team, season, final_elo, pd, opp_pd) %>%
  rename(y1_elo = final_elo,
         y1_pd = pd,
         y1_opp_pd = opp_pd) %>%
  mutate(y1_rel_elo = y1_elo - 1500) %>%
  inner_join(season.elo %>%
               select(team, season, final_elo) %>%
               mutate(season = season - 1),
             by = c("team", "season")) %>%
  rename(y2_elo = final_elo) %>%
  mutate(y2_rel_elo = y2_elo - 1500)

summary(lm(y2_rel_elo ~ 0 + y1_pd + y1_opp_pd, data = yoy.elo))
with(yoy.elo, cor(y1_elo, y2_elo)^2)
