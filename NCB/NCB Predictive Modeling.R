library(tidyverse)
#devtools::install_github("jflancer/bigballR")
library(bigballR)

# initiate game table
games <- data.frame(game_id = integer(),
                    game_date = character(),
                    home_team = character(),
                    away_team = character(),
                    home_score = integer(),
                    away_score = integer(),
                    is_neutral = integer())

# create calendar table
dates <- c(as.Date("2024-11-04"):as.Date("2025-03-19"))
calendar <- data.frame(id = c(1:length(dates)),
                       dt = as.Date("1970-01-01") + dates)
calendar$dt_char <- with(calendar, as.character(format(dt, "%m/%d/%Y")))

# set variables for looping
d <- 1
d.max <- as.numeric(length(dates))

for(d in 1:d.max){
  
  game.date <- calendar[d,3]
  
  tryCatch(get.date.games <- get_date_games(date = game.date),
           error = function(e){'no games'})
  
  daily.games <-
    get.date.games %>%
    filter(!is.na(GameID)) %>%
    rename(game_id = GameID,
           game_date = Date,
           home_team = Home,
           away_team = Away,
           home_score = Home_Score,
           away_score = Away_Score,
           is_neutral = Neutral_Site) %>%
    select(game_id, game_date, home_team, away_team, home_score, away_score, is_neutral) %>%
    mutate(game_id = as.numeric(game_id),
           home_score = as.numeric(home_score),
           away_score = as.numeric(away_score),
           is_neutral = as.numeric(is_neutral))
  
  games <- rbind(games, daily.games)
  d <- d + 1
  
}

games <-
  games %>%
  mutate(game_id = as.numeric(game_id),
         game_date = as.Date(game_date, format = "%m/%d/%Y"))


team.games.all <-
  rbind(games %>%
          rename(team = home_team,
                 opp = away_team,
                 pf = home_score,
                 pa = away_score) %>%
          mutate(is_home = ifelse(is_neutral == 0, 1, 0),
                 pd = pf - pa),
        games %>%
          rename(team = away_team,
                 opp = home_team,
                 pf = away_score,
                 pa = home_score) %>%
          mutate(is_home = 0,
                 pd = pf - pa))

teams <-
  team.games %>%
  group_by(team) %>%
  summarize(gp = n()) %>%
  ungroup() %>%
  filter(gp >= 20) # filter non-D1 teams

# filter to D1-only, pre-tournament games
team.games <-
  team.games.all %>%
  inner_join(teams %>% select(team), by = "team") %>%
  inner_join(teams %>% select(team) %>% rename(opp = team), by = "opp")
  
team.pd <-
  team.games %>%
  group_by(team) %>%
  summarize(gp = n(),
            pf = mean(pf),
            pa = mean(pa),
            pd = mean(pd)) %>%
  ungroup()

sos.games <-
  team.games %>%
  inner_join(team.pd %>%
               select(team, pd) %>%
               rename(opp = team,
                      opp_pd = pd),
             by = "opp")

team.adj <-
  sos.games %>%
  group_by(team) %>%
  summarize(gp = n(),
            pf = mean(pf),
            pa = mean(pa),
            pd = mean(pd),
            sos = mean(opp_pd)) %>%
  ungroup() %>%
  mutate(adj_pd = pd + sos)

sos2.games <-
  team.games %>%
  inner_join(team.adj %>%
               select(team, adj_pd) %>%
               rename(opp = team,
                      opp_pd = adj_pd),
             by = "opp")

team.adj2 <-
  sos2.games %>%
  group_by(team) %>%
  summarize(gp = n(),
            pf = mean(pf),
            pa = mean(pa),
            pd = mean(pd),
            sos = mean(opp_pd)) %>%
  ungroup() %>%
  mutate(adj_pd = pd + sos)

sos3.games <-
  team.games %>%
  inner_join(team.adj2 %>%
               select(team, adj_pd) %>%
               rename(opp = team,
                      opp_pd = adj_pd),
             by = "opp")

team.ratings <-
  sos3.games %>%
  group_by(team) %>%
  summarize(gp = n(),
            pf = mean(pf),
            pa = mean(pa),
            pd = mean(pd),
            sos = mean(opp_pd)) %>%
  ungroup() %>%
  mutate(rating = pd + sos,
         rank = rank(desc(rating))) %>%
  select(rank, team, rating, pf, pa, pd, sos) %>%
  arrange(rank)
