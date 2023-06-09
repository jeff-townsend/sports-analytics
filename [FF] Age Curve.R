library(nflreadr)
library(tidyverse)

nfl.pbp.load <- load_pbp(2022)
nfl.roster.load <- load_rosters(2022)

# add approximate age to the roster table
nfl.roster <-
  nfl.roster.load %>%
  mutate(age = season - as.numeric(substr(birth_date, 0, 4)))

# isolate fantasy-relevant plays
nfl.pbp <-
  nfl.pbp.load %>%
  
