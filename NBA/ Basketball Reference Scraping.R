library(rvest)
library(tidyverse)
library(lubridate)

setwd("/Users/jtownsend/Downloads")

# initialize data frame

season <- 2022
season.df <- data.frame(matrix(ncol = 13, nrow = 0))
col.names <- c("game_id", "season", "game_date", "start_time", "away_team", "away_points",
               "home_team", "home_points", "box_score", "overtimes", "attendance", "arena", "notes")
colnames(season.df) <- col.names

### loop through each month

m <- 1

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
    dates <- dates[! dates == "Playoffs"]
    
    game.id <-
      webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk")
    game.id <- game.id[!is.na(game.id)]
    
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

season.df <- season.df %>% arrange(game_id)
season.df$game_date <- mdy(season.df$game_date)

#season.df <- season.df %>% filter(month(game_date) != 9)

write.csv(season.df, paste0("basketball_reference_games_", season, ".csv"), row.names = FALSE)

## boxscore scraping

url <- paste0("https://www.basketball-reference.com/boxscores/202310240DEN.html")
webpage <- read_html(url)

away.stats <- html_table(html_nodes(webpage, "table"))[[1]][-c(1,7),]
home.stats <- html_table(html_nodes(webpage, "table"))[[9]][-c(1,7),]

