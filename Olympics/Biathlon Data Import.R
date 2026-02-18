library(tidyverse)
library(pdftools)

##### Event 1: Ostersund 20km Individual #####
### Dec 3, 2025

# Download and read the PDF
url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP01/SMIN/C73A_v1.pdf"
pdf_text_raw <- pdf_text(url)

# The text comes as a character vector (one element per page)
# Combine all pages
all_text <- paste(pdf_text_raw, collapse = "\n")

# Split into lines
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]  # Remove empty lines

# Parse the results table
# Pattern: Rank, Bib, LastName FirstName, Nat, P S P S T (shooting), Ski Time, Result, Behind, WC, NC
results <- lines %>%
  # Filter to lines that start with a rank number
  .[str_detect(., "^\\d+\\s")] %>%
  # Parse each line
  {
    do.call(rbind, lapply(., function(line) {
      parts <- str_split(line, "\\s+")[[1]]
      # Basic extraction — adjust indices based on actual layout
      data.frame(
        Rank     = parts[1],
        Bib      = parts[2],
        stringsAsFactors = FALSE
      )
    }))
  }

# --- Alternative: use a regex to capture all key fields ---
# Adjust the regex based on what head(lines) shows you

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)\\s+(\\S+)(?:\\s+(\\S+))?"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "prone1", "standing2", "prone3", "standing4", "total",
                  "ski_time", "result", "behind")

# Convert numeric columns
df <- df %>%
  mutate(across(c(rank, bib, prone1, standing2, prone3, standing4, total), as.integer))

# Store data
event1 <- df

##### Ostersund 10km Sprint #####
### Dec 6, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP01/SMSP/C73B_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\S+))?"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "prone1", "standing2", "total",
                  "result", "behind")

df <- df %>%
  mutate(across(c(rank, bib, prone1, standing2, total), as.integer))

event2 <- df

##### Ostersund 12.5km Pursuit #####
### Dec 7, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP01/SMPU/C73D_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\S+)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\d+))?\\s+(\\S+)"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "start_behind", "prone1", "prone2", "standing3", "standing4", "total",
                  "behind", "wc", "pursuit_only")

df <- df %>%
  mutate(across(c(rank, bib, prone1, prone2, standing3, standing4, total, wc), as.integer))

event3 <- df

##### Hochfilzen 10km Sprint #####
### Dec 12, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP02/SMSP/C73B_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\S+))?"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "prone1", "standing2", "total", "result", "behind")

df <- df %>%
  mutate(across(c(rank, bib, prone1, standing2, total), as.integer))

event4 <- df

##### Hochfilzen 12.5km Pursuit #####
### Dec 13, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP02/SMPU/C73D_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\S+)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\d+))?\\s+(\\S+)"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "start_behind", "prone1", "prone2", "standing3", "standing4", "total",
                  "behind", "wc", "pursuit_only")

df <- df %>%
  mutate(across(c(rank, bib, prone1, prone2, standing3, standing4, total), as.integer)) %>%
  mutate(wc = as.integer(wc))

event5 <- df

##### Annecy-Le Grand-Bornand 10km Sprint #####
### Dec 19, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP03/SMSP/C73B_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\S+))?"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "prone1", "standing2", "total", "result", "behind")

df <- df %>%
  mutate(across(c(rank, bib, prone1, standing2, total), as.integer))

event6 <- df

##### Annecy-Le Grand-Bornand 12.5km Pursuit #####
### Dec 20, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP03/SMPU/C73D_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\S+)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\d+))?\\s+(\\S+)"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "start_behind", "prone1", "prone2", "standing3", "standing4", "total",
                  "behind", "wc", "pursuit_only")

df <- df %>%
  mutate(across(c(rank, bib, prone1, prone2, standing3, standing4, total), as.integer)) %>%
  mutate(wc = as.integer(wc))

event7 <- df

##### Annecy-Le Grand-Bornand 15km Mass Start #####
### Dec 21, 2025

url <- "https://ibu.blob.core.windows.net/docs/2526/BT/SWRL/CP03/SMMS/C73E_v1.pdf"
pdf_text_raw <- pdf_text(url)

all_text <- paste(pdf_text_raw, collapse = "\n")
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

pattern <- "^(\\d+)\\s+(\\d+)\\s+([A-Z\\s-]+?)\\s+([A-Z][a-z]+-?(?:[A-Z][a-z]+)?(?:\\s[A-Z][a-z]+-?(?:[A-Z][a-z]+)*)*)\\s+([A-Z]{2,3})\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d)\\s+(\\d+)\\s+(\\S+)(?:\\s+(\\d+))?"

parsed <- str_match(lines[str_detect(lines, "^\\d+\\s+\\d+")], pattern)

df <- as.data.frame(parsed[!is.na(parsed[,1]), -1], stringsAsFactors = FALSE)
colnames(df) <- c("rank", "bib", "last_name", "first_name", "nationality",
                  "prone1", "prone2", "standing3", "standing4", "total",
                  "time", "wc")

df <- df %>%
  mutate(across(c(rank, bib, prone1, prone2, standing3, standing4, total), as.integer)) %>%
  mutate(wc = as.integer(wc))

event8 <- df
