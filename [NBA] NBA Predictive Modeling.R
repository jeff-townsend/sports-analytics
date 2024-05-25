library(rvest)
library(tidyverse)
library(lubridate)

setwd("/Users/jtownsend/Downloads")

# decide how far back to go
starting.season <- 2023
ending.season <- 2024

# use game dates to determine whether it's a playoff game
p <- c("2015-04-18", "2016-04-16", "2017-04-15", "2018-04-14", "2019-04-13",
       "2020-08-17", "2021-05-22", "2022-04-16", "2023-04-15", "2024-04-20")
playoff.dates <- data.frame(season = c(2015:2024),
                            playoff_start_date = p)

# initialize data frame

bref.df <- data.frame(matrix(ncol = 13, nrow = 0))
col.names <- c("game_id", "season", "game_date", "start_time", "away_team", "away_points",
               "home_team", "home_points", "box_score", "overtimes", "attendance", "arena", "notes")
colnames(bref.df) <- col.names

### loop through each season month

season <- starting.season
m <- 1

for(season in starting.season:ending.season)
{
  
  season.df <- data.frame(matrix(ncol = 13, nrow = 0))
  colnames(season.df) <- col.names
  
  for(m in 1:12)
  {
    
    month <- tolower(month.name[m])
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_games-", month, ".html")
    
    webpage <- NULL
    tryCatch(webpage <- read_html(url),
             error = function(e){'empty page'})
    
    if(is.null(webpage) == FALSE)
    {
      
      dates <-
        webpage %>% 
        html_nodes("table#schedule > tbody > tr > th") %>% 
        html_text()
      
      game.id <-
        webpage %>% 
        html_nodes("table#schedule > tbody > tr > th") %>%
        html_attr("csk")
      
      data <-
        webpage %>% 
        html_nodes("table#schedule > tbody > tr > td") %>% 
        html_text() %>%
        matrix(ncol = length(col.names) - 3, byrow = TRUE)
      
      month.df <- as.data.frame(cbind(game.id, season, dates, data), stringsAsFactors = FALSE)
      colnames(month.df) <- col.names
      
      season.df <- rbind(season.df, month.df)
      
    }
    
    m <- m + 1
    
  }
  
  write.csv(season.df, paste0("basketball_reference_games_", season, ".csv"))
  bref.df <- rbind(bref.df, season.df)
  
  season <- season + 1
  m <- 1
  
}

bref.df <-
  bref.df %>%
  filter(!is.na(away_points)) %>%
  arrange(game_id)

### end of loop

# change columns to the correct types
bref.df$season <- as.numeric(bref.df$season)
bref.df$away_points <- as.numeric(bref.df$away_points)
bref.df$home_points <- as.numeric(bref.df$home_points)
bref.df$attendance <- as.numeric(gsub(",", "", bref.df$attendance))
bref.df$game_date <- mdy(bref.df$game_date)


## create new data frame where each observation is a team game

away.df <-
  bref.df %>%
  select(game_id, season, game_date, away_team, away_points, home_team, home_points, notes) %>%
  rename(team = away_team,
         points = away_points,
         opponent = home_team,
         points_allowed = home_points) %>%
  mutate(is_home = 0)

home.df <-
  bref.df %>%
  select(game_id, season, game_date, home_team, home_points, away_team, away_points, notes) %>%
  rename(team = home_team,
         points = home_points,
         opponent = away_team,
         points_allowed = away_points) %>%
  mutate(is_home = 1)

# merge two tables and add new columns

merge.df <-
  rbind(away.df, home.df) %>%
  group_by(season, team) %>%
  mutate(game_number = rank(game_date))

games.tmp <-
  merge.df %>%
  arrange(game_id) %>%
  mutate(is_win = ifelse(points > points_allowed, 1, 0)) %>%
  inner_join(playoff.dates, by = "season") %>%
  mutate(game_type = ifelse(notes == "Play-In Game", "Play-In",
                            ifelse(game_id == "202312090LAL", "IST Final",
                                   ifelse(notes == "In-Season Tournament", "In-Season Tournament",
                                          ifelse(game_date >= playoff_start_date, "Playoffs", "Regular Season"))))) %>%
  select(-playoff_start_date) %>%
  left_join(merge.df %>%
              mutate(game_number_match = game_number + 1) %>%
              select(season, team, game_date, game_number_match),
            by = c("season", "team", c("game_number" = "game_number_match")),
            suffix = c("", "_prior")) %>%
  mutate(days_rest = as.numeric(game_date - game_date_prior) - 1) %>%
  select(-game_date_prior)

games.df <-
  games.tmp %>%
  inner_join(games.tmp %>%
               select(game_id, season, team, days_rest),
             by = c("game_id", "season", c("opponent" = "team")),
             suffix = c("","_opp")) %>%
  mutate(rest_diff = days_rest - days_rest_opp,
         rest_diff_bucket = ifelse(rest_diff > 2, 2,
                                   ifelse(rest_diff < -2, -2, rest_diff)))

rs.games.df <-
  games.df %>%
  filter(game_type %in% c("Regular Season", "In-Season Tournament")) %>%
  select(-game_number) %>%
  group_by(season, team) %>%
  mutate(game_number = rank(game_date)) %>%
  ungroup() %>%
  mutate(is_odd = ifelse(game_number%%2 == 1, 1, 0))

exp.pds <-
  games.df %>%
  group_by(is_home, rest_diff_bucket, game_type) %>%
  summarize(gp = n(),
            win_rate = mean(is_win),
            pd = mean(points) - mean(points_allowed))

## summarize regular season data

season.stats <-
  rs.games.df %>%
  group_by(season, team) %>%
  summarize(games = n(),
            wins = sum(is_win),
            losses = n() - sum(is_win),
            win_rate = mean(is_win),
            pf = mean(points),
            pa = mean(points_allowed)) %>%
  ungroup() %>%
  mutate(pd = pf - pa)

xwinrate.mod <- lm(win_rate ~ pd, data = season.stats)
#summary(xwinrate.mod)

## split-half reliability

odd.even.stats <-
  rs.games.df %>%
  group_by(season, team, is_odd) %>%
  summarize(games = n(),
            wins = sum(is_win),
            losses = n() - sum(is_win),
            win_rate = mean(is_win),
            pf = mean(points),
            pa = mean(points_allowed)) %>%
  ungroup() %>%
  mutate(pd = pf - pa,
         xwin_rate = xwinrate.mod$coefficients[1] + xwinrate.mod$coefficients[2]*pd,
         win_rate_delta = win_rate - xwin_rate)

shr.data <-
  odd.even.stats %>%
  filter(is_odd == 1) %>%
  select(-is_odd) %>%
  inner_join(odd.even.stats %>%
               filter(is_odd == 0) %>%
               select(-is_odd),
             by = c("season", "team"), suffix = c("_odd", "_even"))

# ggplot(shr.data, aes(x = pd_even, y = win_rate_odd)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x)

# shr.data %>%
#   summarize(r = cor(win_rate_odd, win_rate_even)) %>%
#   mutate(r2 = r^2)
# 
# shr.data %>%
#   summarize(r = cor(pd_odd, pd_even)) %>%
#   mutate(r2 = r^2)

shr.mod <- lm(win_rate_even ~ pd_odd, data = shr.data)
#summary(wr.mod)
