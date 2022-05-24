library(readr)
library(tidyverse)

##### question: does goal scoring follow a poisson distribution?

## currently downloading shot data from MoneyPuck and importing
shots.import <- read_csv("Documents/PD/shots_2021.csv")

## manipulate the imported file; because MP has done a lot of cleaning not much will need to be done here
shots <-
  shots.import %>%
  mutate(oppCode = ifelse(team == "HOME", awayTeamCode, homeTeamCode)) # will be helpful to pull defensive stats

# regular season only
shots.rs <- shots %>% filter(isPlayoffGame == 0)

game.level.goals <-
  shots.rs %>%
  group_by(game_id, teamCode) %>%
  summarize(goals = sum(goal)) %>%
  ungroup() %>%
  mutate(type = "actual") %>%
  select(type, goals)

set.seed(2622)
poisson.simulated.goals <- data.frame(type = "simulated",
                                      goals = rpois(n = nrow(game.level.goals), lambda = mean(game.level.goals$goals)))

goal.distribution.data <- rbind(game.level.goals, poisson.simulated.goals)

ggplot(goal.distribution.data, aes(x = goals, fill = type)) +
  geom_histogram(binwidth = 1,
                 alpha= 0.5,
                 position = "identity")

##### answer: yes, this is actually pretty close
    # there are likely more actuals at 0, 4, and 5 because of low- and high-scoring teams; data is skewed to be average
    # empty net goals and score effects would affect the distribution a bit, but not enough to be an issue
    # i.e. a team with 3 goals is more likely to score a 4th if they are up 3-2 late in a game as opposed to 3-0 (empty net)
    # i.e. a team with 2 goals may be more likely to score a 3rd if they are down 2-4 than up 2-0 (score effects)
