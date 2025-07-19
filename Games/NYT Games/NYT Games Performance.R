library(tidyverse)
library(readr)

crosswords.import <-
  read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/NYT%20Games/crosswords.csv",
           col_types = cols(date = col_date(format = "%m/%d/%y")))

crosswords <-
  crosswords.import %>%
  mutate(weekday_num = wday(date),
         weekday = weekdays(date),
         month_year = format(date, "%Y-%m"),
         month = months(date))

weekday.agg <-
  crosswords %>%
  group_by(weekday_num, weekday) %>%
  summarize(attempts = n(),
            solves = sum(solved),
            solve_rate = mean(solved)) %>%
  ungroup()

monthly.agg <-
  crosswords %>%
  inner_join(weekday.agg %>% select(weekday, solve_rate),
             by = "weekday") %>%
  group_by(month_year, month) %>%
  summarize(attempts = n(),
            solves = sum(solved),
            solve_rate = mean(solved),
            xsolve_rate = mean(solve_rate))

crosswords %>%
  inner_join(weekday.agg %>% select(weekday, solve_rate),
             by = "weekday") %>%
  filter(month == "April") %>%
  summarize(xsolve_rate = mean(solve_rate),
            solve_rate = mean(solved))
