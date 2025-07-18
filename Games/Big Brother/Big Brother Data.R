library(tidyverse)
library(ggthemes)
library(scales)
library(readr)

houseguests <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Big%20Brother/houseguests.csv")
events <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Big%20Brother/events.csv")
votes <- read_csv("https://raw.githubusercontent.com/jeff-townsend/sports-analytics/main/Games/Big%20Brother/votes.csv")
