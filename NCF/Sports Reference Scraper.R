library(tidyverse)
library(rvest)
library(httr)

# Function to scrape CFB schedule data
scrape_cfb_schedule <- function(url = "https://www.sports-reference.com/cfb/years/2024-schedule.html") {
  
  # Set headers including user agent to avoid blocking
  headers <- c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.5",
    "Accept-Encoding" = "gzip, deflate",
    "Connection" = "keep-alive"
  )
  
  # Make HTTP request with custom headers
  response <- GET(url, add_headers(.headers = headers))
  
  # Check if request was successful
  if(status_code(response) != 200) {
    stop(paste("HTTP request failed with status code:", status_code(response)))
  }
  
  # Read the webpage content
  page <- read_html(content(response, "text"))
  
  # Find the schedule table - typically has id "schedule" or class "schedule"
  # Sports Reference often uses tables with class "sortable stats_table"
  schedule_table <- page %>% 
    html_element("table.sortable.stats_table") %>%
    html_table()
  
  # Alternative selectors in case the above doesn't work
  if(is.null(schedule_table) || nrow(schedule_table) == 0) {
    # Try different table selectors
    schedule_table <- page %>% 
      html_element("#schedule") %>%
      html_table()
  }
  
  if(is.null(schedule_table) || nrow(schedule_table) == 0) {
    # Try finding any table on the page
    all_tables <- page %>% 
      html_elements("table") %>%
      html_table()
    
    # Usually the largest table is the schedule
    if(length(all_tables) > 0) {
      schedule_table <- all_tables[[which.max(sapply(all_tables, nrow))]]
    }
  }
  
  return(schedule_table)
}

# Function to clean and process the scraped data
clean_schedule_data <- function(raw_data) {
  
  if(is.null(raw_data) || nrow(raw_data) == 0) {
    stop("No data found. Check if the webpage structure has changed.")
  }
  
  # Print column names to help identify structure
  cat("Column names found:\n")
  print(names(raw_data))
  cat("\n")
  
  # Fix column names first - replace NA or empty names with generic names
  col_names <- c("id", "week", "date", "time", "day", "winner", "winner_pts", "venue", "loser", "loser_pts", "notes")
  
  # Apply the fixed names
  names(raw_data) <- col_names
  
  cat("Fixed column names:\n")
  print(names(raw_data))
  cat("\n")
  
  # Now safely clean the data using the first column
  first_col <- names(raw_data)[1]
  
  # Basic cleaning - remove empty rows using proper column reference
  cleaned_data <- raw_data %>%
    # Remove rows where first column is NA or empty
    filter(!is.na(.data[[first_col]]) & .data[[first_col]] != "" & .data[[first_col]] != " ") %>%
    # Remove header rows that might be repeated in the data
    filter(!grepl("Date|Week|Rk", .data[[first_col]], ignore.case = TRUE))
  
  # Try to standardize common column names
  names(cleaned_data) <- gsub("[^A-Za-z0-9]", "_", names(cleaned_data))
  names(cleaned_data) <- tolower(names(cleaned_data))
  
  return(cleaned_data)
}

# Function to extract specific game information
extract_game_info <- function(cleaned_data) {
  
  # This function will need to be customized based on actual column structure
  # Common columns in CFB schedule data:
  # Date, Time, Away Team, Home Team, Score, Notes, TV, etc.
  
  cat("First few rows of cleaned data:\n")
  print(head(cleaned_data, 3))
  cat("\n")
  
  # Return the cleaned data for now - user can customize based on actual structure
  return(cleaned_data)
}

# Main execution
tryCatch({
  
  cat("Scraping CFB 2024 schedule data...\n")
  
  # Scrape the data
  raw_schedule <- scrape_cfb_schedule()
  
  # Clean the data
  clean_schedule <- clean_schedule_data(raw_schedule)
  
  # Extract game information
  final_data <- extract_game_info(clean_schedule)
  
  cat("Scraping completed successfully!\n")
  cat("Number of records found:", nrow(final_data), "\n")
  
  # Save to CSV
  #write.csv(final_data, "cfb_2024_schedule.csv", row.names = FALSE)
  #cat("Data saved to 'cfb_2024_schedule.csv'\n")
  
  # Return the data
  final_data
  
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
  cat("This might be due to:\n")
  cat("1. Website blocking automated requests\n")
  cat("2. Changed webpage structure\n")
  cat("3. Network connectivity issues\n")
  cat("4. Missing required R packages\n")
  cat("\nTry installing missing packages with:\n")
  cat("install.packages(c('rvest', 'dplyr', 'lubridate', 'httr'))\n")
})

scrape_cfb_schedule(url = "https://www.sports-reference.com/cfb/years/2024-schedule.html")
