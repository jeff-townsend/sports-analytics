library(rvest)
library(tidyverse)
library(lubridate)

year <- "2024"
month <- "october"
url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
              "_games-", month, ".html")
webpage <- read_html(url)

col_names <-
  webpage %>% 
  html_nodes("table#schedule > thead > tr > th") %>% 
  html_attr("data-stat")
col_names <- c("game_id", col_names)

dates <-
  webpage %>% 
  html_nodes("table#schedule > tbody > tr > th") %>% 
  html_text()

game_id <- webpage %>% 
  html_nodes("table#schedule > tbody > tr > th") %>%
  html_attr("csk")

data <- webpage %>% 
  html_nodes("table#schedule > tbody > tr > td") %>% 
  html_text() %>%
  matrix(ncol = length(col_names) - 2, byrow = TRUE)

month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
names(month_df) <- col_names

df <- month_df

# change columns to the correct types
df$visitor_pts <- as.numeric(df$visitor_pts)
df$home_pts    <- as.numeric(df$home_pts)
df$attendance  <- as.numeric(gsub(",", "", df$attendance))
df$date_game   <- mdy(df$date_game)
# add column to indicate if regular season or playoff
playoff_startDate <- ymd("2018-04-14")
df$game_type <- with(df, ifelse(date_game >= playoff_startDate, 
                                "Playoff", "Regular"))
# drop boxscore column
df$box_score_text <- NULL
