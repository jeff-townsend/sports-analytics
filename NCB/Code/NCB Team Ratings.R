library(tidyverse)
library(hoopR)

compute_season_ratings <- function(season) {
# Load schedules (includes game results — great for modeling)
schedule <- suppressMessages(load_mbb_schedule(season))
d1_teams <- suppressMessages(espn_mbb_teams(season)) %>% pull(display_name)

regular_season <- schedule %>%
  filter(
    season_type == 2,
    home_display_name %in% d1_teams,
    away_display_name %in% d1_teams
  ) %>%
  select(game_id, game_date, home_team = home_display_name, away_team = away_display_name, 
         home_score, away_score, conf_game = conference_competition, 
         conference = groups_short_name) %>%
  mutate(
    home_win = as.integer(home_score > away_score),
    away_win = as.integer(away_score > home_score),
    conf_game = as.integer(conf_game)
  )

home <- regular_season %>%
  mutate(
    team          = home_team,
    opponent      = away_team,
    pf            = home_score,
    pa            = away_score,
    win           = home_win,
    home_away     = "home"
  )

away <- regular_season %>%
  mutate(
    team           = away_team,
    opponent       = home_team,
    pf             = away_score,
    pa             = home_score,
    win            = away_win,
    home_away      = "away"
  )

## Evaluate non-conference team ratings

regular_season_long_non_conf <- bind_rows(home, away) %>%
  select(game_id, game_date, team, opponent, home_away, pf, pa, win, conf_game) %>%
  filter(conf_game == 0)

team_summary_non_conf <- regular_season_long_non_conf %>%
  group_by(team) %>%
  summarise(
    wins   = sum(win, na.rm = TRUE),
    losses = sum(1 - win, na.rm = TRUE),
    pf     = round(mean(pf, na.rm = TRUE), 1),
    pa     = round(mean(pa, na.rm = TRUE), 1),
    pd     = round(mean(pf - pa, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(pd))

regular_season_long_sos_non_conf <- regular_season_long_non_conf %>%
  left_join(
    team_summary_non_conf %>% select(team, opp_pd = pd),
    by = c("opponent" = "team")
  )

team_summary_sos_non_conf <- team_summary_non_conf %>%
  left_join(
    regular_season_long_sos_non_conf %>%
      group_by(team) %>%
      summarise(sos = round(mean(opp_pd, na.rm = TRUE), 1)),
    by = "team"
  ) %>%
  mutate(rating = round(pd + sos, 1)) %>%
  arrange(desc(rating))

# Initialize
ratings_non_conf <- team_summary_sos_non_conf %>% select(team, pd, rating)

# # Store ratings at each iteration
# convergence_tracker <- ratings_non_conf %>% 
#   select(team, rating) %>% 
#   rename(iter_0 = rating)

for (i in 1:20) {
  ratings_non_conf <- regular_season_long_non_conf %>%
    left_join(ratings_non_conf %>% select(team, opp_rating = rating), by = c("opponent" = "team")) %>%
    group_by(team) %>%
    summarise(sos = round(mean(opp_rating, na.rm = TRUE), 1)) %>%
    left_join(team_summary_non_conf %>% select(team, pd), by = "team") %>%
    mutate(rating = round(pd + sos, 1))
  
  # convergence_tracker <- convergence_tracker %>%
  #   left_join(ratings_non_conf %>% select(team, rating), by = "team") %>%
  #   rename(!!paste0("iter_", i) := rating)
}

# # Assess correlation between consecutive iterations
# correlations <- sapply(1:20, function(i) {
#   cor(convergence_tracker[[paste0("iter_", i-1)]], 
#       convergence_tracker[[paste0("iter_", i)]], 
#       use = "complete.obs")
# })
# 
# cor_df <- data.frame(
#   iteration = 1:20,
#   cor_with_prior = round(correlations, 6)
# )
# 
# print(cor_df)

team_summary_final_non_conf <- team_summary_non_conf %>%
  left_join(ratings_non_conf %>% select(team, sos, rating), by = "team") %>%
  arrange(desc(rating))

## Evaluate conference team ratings

regular_season_long_conf <- bind_rows(home, away) %>%
  select(game_id, game_date, team, opponent, home_away, pf, pa, win, conf_game, conference) %>%
  filter(conf_game == 1)

team_summary_conf <- regular_season_long_conf %>%
  group_by(team, conference) %>%
  summarise(
    wins   = sum(win, na.rm = TRUE),
    losses = sum(1 - win, na.rm = TRUE),
    pf     = round(mean(pf, na.rm = TRUE), 1),
    pa     = round(mean(pa, na.rm = TRUE), 1),
    pd     = round(mean(pf - pa, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pd))

regular_season_long_sos_conf <- regular_season_long_conf %>%
  left_join(
    team_summary_conf %>% select(team, opp_pd = pd),
    by = c("opponent" = "team")
  )

team_summary_sos_conf <- team_summary_conf %>%
  left_join(
    regular_season_long_sos_conf %>%
      group_by(team) %>%
      summarise(sos = round(mean(opp_pd, na.rm = TRUE), 1)),
    by = "team"
  ) %>%
  mutate(rating = round(pd + sos, 1)) %>%
  arrange(desc(rating))

ratings_conf <- team_summary_sos_conf %>% select(team, pd, rating)

for (i in 1:20) {
  ratings_conf <- regular_season_long_conf %>%
    left_join(ratings_conf %>% select(team, opp_rating = rating), by = c("opponent" = "team")) %>%
    group_by(team) %>%
    summarise(sos = round(mean(opp_rating, na.rm = TRUE), 1)) %>%
    left_join(team_summary_conf %>% select(team, pd), by = "team") %>%
    mutate(rating = round(pd + sos, 1)) %>%
    arrange(desc(rating))
}

team_summary_final_conf <- team_summary_conf %>%
  left_join(ratings_conf %>% select(team, sos, rating), by = "team") %>%
  arrange(desc(rating))

## Adjust conference team ratings using conference strength based on non-conference performance

# Get conference for each team from team_summary_final_conf
team_conferences <- team_summary_final_conf %>%
  select(team, conference, rating) %>%
  rename(team_conf = conference, team_rating_conf = rating)

# Build non-conference games with conference and rating info for both teams
non_conf_games_with_conf <- regular_season_long_non_conf %>%
  # Join team's conference and rating
  left_join(team_conferences, by = "team") %>%
  # Join opponent's conference and rating
  left_join(
    team_conferences %>% rename(opp_conf = team_conf, opp_rating_conf = team_rating_conf),
    by = c("opponent" = "team")
  ) %>%
  # Calculate point differential and inferred conference strength difference
  mutate(
    pd_game        = pf - pa,
    conf_diff      = round(pd_game - (team_rating_conf - opp_rating_conf), 1)
  )

conference_strength <- non_conf_games_with_conf %>%
  group_by(team_conf, opp_conf) %>%
  summarise(
    games    = n(),
    conf_diff = round(mean(conf_diff, na.rm = TRUE), 1),
    .groups  = "drop"
  ) %>%
  group_by(team_conf) %>%
  summarise(
    games             = sum(games),
    avg_conf_strength = round(mean(conf_diff, na.rm = TRUE), 1),
    .groups           = "drop"
  ) %>%
  arrange(desc(avg_conf_strength))

# Initialize conference ratings with avg_conf_strength
conf_ratings <- conference_strength %>%
  select(conference = team_conf, avg_conf_strength) %>%
  rename(conf_rating = avg_conf_strength)

# # Store ratings at each iteration
# conf_convergence_tracker <- conf_ratings %>%
#   rename(iter_0 = conf_rating)

for (i in 1:12) {
  new_conf_ratings <- non_conf_games_with_conf %>%
    left_join(conf_ratings %>% rename(team_conf_rating = conf_rating),
              by = c("team_conf" = "conference")) %>%
    left_join(conf_ratings %>% rename(opp_conf_rating = conf_rating),
              by = c("opp_conf" = "conference")) %>%
    mutate(adj_conf_diff = pd_game - (team_rating_conf - opp_rating_conf) - opp_conf_rating) %>%
    group_by(team_conf) %>%
    summarise(conf_rating = round(mean(adj_conf_diff, na.rm = TRUE), 1), .groups = "drop") %>%
    rename(conference = team_conf)
  
  # Blend new rating with prior rating to dampen oscillation
  conf_ratings <- conf_ratings %>%
    left_join(new_conf_ratings %>% rename(new_conf_rating = conf_rating), by = "conference") %>%
    mutate(conf_rating = round(0.5 * new_conf_rating + 0.5 * conf_rating, 1)) %>%
    select(conference, conf_rating) %>%
    arrange(desc(conf_rating))
  
  # conf_convergence_tracker <- conf_convergence_tracker %>%
  #   left_join(conf_ratings, by = "conference") %>%
  #   rename(!!paste0("iter_", i) := conf_rating)
}

# # Assess correlation between consecutive iterations
# conf_correlations <- sapply(1:12, function(i) {
#   cor(conf_convergence_tracker[[paste0("iter_", i-1)]],
#       conf_convergence_tracker[[paste0("iter_", i)]],
#       use = "complete.obs")
# })
# 
# conf_cor_df <- data.frame(
#   iteration      = 1:12,
#   cor_with_prior = round(conf_correlations, 6)
# )
# 
# print(conf_cor_df)

conference_strength_final <- conference_strength %>%
  left_join(conf_ratings, by = c("team_conf" = "conference")) %>%
  select(team_conf, games, conf_rating) %>%
  arrange(desc(conf_rating))

team_summary_adj_conf <- team_summary_conf %>%
  left_join(ratings_conf %>% select(team, sos, rating), by = "team") %>%
  left_join(conf_ratings, by = "conference") %>%
  mutate(adj_rating = round(rating + conf_rating, 1)) %>%
  arrange(desc(adj_rating))

## Combine non-conference and conference ratings into one rating
team_ratings_combined <- team_summary_final_non_conf %>%
  select(team, rating_non_conf = rating, games_non_conf = wins) %>%
  mutate(games_non_conf = games_non_conf + team_summary_final_non_conf$losses) %>%
  full_join(
    team_summary_adj_conf %>% select(team, conference, rating_conf = rating, conf_rating, adj_rating_conf = adj_rating, games_conf = wins) %>%
      mutate(games_conf = games_conf + team_summary_adj_conf$losses),
    by = "team"
  ) %>%
  mutate(
    total_games = coalesce(games_non_conf, 0) + coalesce(games_conf, 0),
    rating      = round(
      (coalesce(rating_non_conf, 0) * coalesce(games_non_conf, 0) +
         coalesce(adj_rating_conf, 0) * coalesce(games_conf, 0)) / total_games, 1
    )
  ) %>%
  select(team, rating, rating_non_conf, rating_conf = adj_rating_conf, conference, conf_rating) %>%
  arrange(desc(rating))

team_ratings_combined %>%
  mutate(season = season)
}

# Run for all seasons
all_seasons <- 2009:most_recent_mbb_season()

progressr::with_progress({
  all_ratings <- purrr::map_dfr(all_seasons, compute_season_ratings)
})

# Assess year-over-year correlation
yoy_correlation <- all_ratings %>%
  arrange(team, season) %>%
  group_by(team) %>%
  mutate(prior_rating = lag(rating)) %>%
  ungroup() %>%
  filter(!is.na(prior_rating)) %>%
  group_by(season) %>%
  summarise(
    cor_with_prior = round(cor(rating, prior_rating, use = "complete.obs"), 3),
    n_teams        = n()
  ) %>%
  arrange(season)

print(yoy_correlation)
with(yoy_correlation, mean(cor_with_prior))
