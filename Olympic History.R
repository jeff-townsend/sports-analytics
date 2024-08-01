library(readr)
library(tidyverse)

athlete.events <- read_csv("Downloads/athlete_events.csv")
noc.regions <- read_csv("Downloads/noc_regions.csv")

events <-
  athlete.events %>%
  group_by(sport, event, season) %>%
  summarize(first_year = min(year),
            last_year = max(year),
            total_years = n_distinct(year))

medalists <-
  athlete.events %>%
  select(id, name, sex, age, team, noc, year, season, sport, event, medal) %>%
  filter(!is.na(medal)) %>%
  mutate(is_gold = ifelse(medal == "Gold", 1, 0),
         is_silver = ifelse(medal == "Silver", 1, 0),
         is_bronze = ifelse(medal == "Bronze", 1, 0))

medalist.countries <-
  medalists %>%
  inner_join(noc.regions, by = "noc") %>%
  distinct(noc, region, year, season, sport, event, medal, is_gold, is_silver, is_bronze)

medal.table <-
  medalist.countries %>%
  group_by(noc, region, year, season) %>%
  summarize(gold_medals = sum(is_gold),
            silver_medals = sum(is_silver),
            bronze_medals = sum(is_bronze),
            total_medals = n(),
            effective_medals = sum(is_gold * 1.5 + is_silver + is_bronze * 0.5))

sport.medals <-
  medalist.countries %>%
  group_by(noc, region, year, season, sport) %>%
  summarize(gold_medals = sum(is_gold),
            silver_medals = sum(is_silver),
            bronze_medals = sum(is_bronze),
            total_medals = n(),
            effective_medals = sum(is_gold * 1.5 + is_silver + is_bronze * 0.5))
