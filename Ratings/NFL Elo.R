library(tidyverse)
library(ggthemes)
library(nflreadr)

# set up the data
start <- 2014
end <- 2024
pbp <- load_pbp(seasons = c(start:end))

scores <-
  pbp %>%
  distinct(game_id, season, week, start_time, home_team, away_team, home_score, away_score) %>%
  mutate(start_time = gsub(",", "", start_time)) %>%
  mutate(start_time = as.POSIXct(start_time,
                                 format = "%m/%d/%y %H:%M:%S")) %>%
  group_by(season) %>%
  mutate(game_rank = rank(start_time,
                          ties.method = "first")) %>%
  ungroup() %>%
  select(-start_time)

scores.long <-
  rbind(
    scores %>%
      mutate(is_home = 1) %>%
      rename(team = home_team,
             opponent = away_team,
             pf = home_score,
             pa = away_score),
    scores %>%
      mutate(is_home = 0) %>%
      rename(team = away_team,
             opponent = home_team,
             pf = away_score,
             pa = home_score)) %>%
  arrange(season, game_rank) %>%
  mutate(pd = pf - pa,
         result = ifelse(pf > pa, 1, ifelse(pf < pa, 0, 0.5)))

teams <-
  scores.long %>%
  distinct(team, season)

# initiate tables for loop

elo.games <- data.frame(game_id = integer(),
                        season = integer(),
                        week = integer(),
                        team = character(),
                        elo = double(),
                        opponent = character(),
                        opp_elo = double(),
                        is_home = integer(),
                        expectation = double(),
                        result = double(),
                        elo_delta = double(),
                        rmse = double())

k <- 50
hfa <- 0
s <- start
for(s in start:2023){
  
  team.elo <-
    teams %>%
    filter(season == s) %>%
    mutate(elo = 1500) %>%
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
                 is_home, expectation, result, elo_delta) %>%
          mutate(rmse = sqrt((result - expectation) ^ 2)))
    
    g <- g + 1
    
  }
  
  s <- s + 1
    
}

# calculate home-field advantage
elo.games %>%
  filter(is_home == 1) %>%
  group_by(season) %>%
  summarize(hfa = mean(result - expectation),
            hfa_elo = -400*log10(1 / (hfa + 0.5) - 1))

# measure YOY regression
final.week <-
  elo.games %>%
  group_by(team, season) %>%
  summarize(final_week = max(week))

final.elo <-
  elo.games %>%
  inner_join(final.week, by = c("team", "season", "week" = "final_week")) %>%
  mutate(final_elo = elo + elo_delta) %>%
  select(team, season, final_elo)

yoy.elo <-
  final.elo %>%
  inner_join(final.elo %>% mutate(season = season - 1),
             by = c("team", "season")) %>%
  rename(y1_elo = final_elo.x,
         y2_elo = final_elo.y)

ggplot(yoy.elo, aes(x = y1_elo, y = y2_elo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("Elo (Season n-1)") +
  ylab("Elo (Season n)")
summary(lm(y2_elo ~ y1_elo, data = yoy.elo))

# measure RMSE
elo.games %>%
  summarize(rmse = mean(rmse)) # pb: 0.465
