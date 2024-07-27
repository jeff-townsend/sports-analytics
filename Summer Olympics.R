library(readr)
library(tidyverse)

sports <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/Olympics/olympic_sports.csv") %>%
  rename(sport_id = id)
regions <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/Olympics/olympic_regions.csv") %>%
  rename(region_id = id)
teams <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/Olympics/olympic_teams.csv") %>%
  rename(team_id = id)
tournaments <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/Olympics/olympic_tournaments.csv",
           col_types = cols(start_date = col_date(format = "%m/%d/%Y"),
                            end_date = col_date(format = "%m/%d/%Y"))) %>%
  rename(tournament_id = id)
matches <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/data/Olympics/olympic_matches.csv") %>%
  rename(match_id = id)

team.matches <-
  cbind(data.frame(team_match_id = c(1:(nrow(matches)*2))),
        rbind(matches %>%
              rename(team = team_a,
                     opp = team_b,
                     score = score_a,
                     opp_score = score_b,
                     subscore = subscore_a,
                     opp_subscore = subscore_b),
              matches %>%
                rename(team = team_b,
                       opp = team_a,
                       score = score_b,
                       opp_score = score_a,
                       subscore = subscore_b,
                       opp_subscore = subscore_a)) %>%
              arrange(match_id)) %>%
  mutate(result = ifelse(score > opp_score, "Win", ifelse(score == opp_score, "Draw", "Loss")))

## Men's Rugby Sevens

mrugby.matches <-
  team.matches %>%
  inner_join(sports, by = "sport_id") %>%
  filter(sport == "Rugby Sevens",
         event == "Men's") %>%
  select(-sport_id, -sport, -event) %>%
  group_by(opponent) %>%
  mutate(sos = )

mrugby.results <-
  mrugby.matches %>%
  group_by(team) %>%
  summarize(matches = n(),
            wins = sum(result == "Win"),
            draws = sum(result == "Draw"),
            losses = sum(result == "Loss"),
            win_rate = mean(result == "Win"),
            pf = mean(score),
            pa = mean(opp_score),
            pd = mean(score - opp_score))
