
library(dplyr)
library(tidyr)
library(tibble)
library(rvest)
library(stringr)
library(purrr)
library(scales)
library(text2vec)
library(tm)
library(stringdist)


### 2026 ----

current_url <- "https://www.hockey-reference.com/leagues/NHL_2026_games.html"

web <- read_html(current_url)

table_node <- web %>% html_nodes("table")

table_content <- html_table(table_node)[[1]]

colnames(table_content) <- c("Date", "Time", "Visitor", "Visitor Score", "Home", "Home Score", "Blank", "Attendance", "LOG", "Notes")

binder <- table_content

### 2025 ----

current_url <- "https://www.hockey-reference.com/leagues/NHL_2025_games.html"

web <- read_html(current_url)

table_node <- web %>% html_nodes("table")

table_content <- html_table(table_node)[[1]]

colnames(table_content) <- c("Date", "Time", "Visitor", "Visitor Score", "Home", "Home Score", "Blank", "Attendance", "LOG", "Notes")

binder <- rbind(binder, table_content)


### 2024 ----

current_url <- "https://www.hockey-reference.com/leagues/NHL_2024_games.html"

web <- read_html(current_url)

table_node <- web %>% html_nodes("table")

table_content <- html_table(table_node)[[1]]

colnames(table_content) <- c("Date", "Time", "Visitor", "Visitor Score", "Home", "Home Score", "Blank", "Attendance", "LOG", "Notes")

binder <- rbind(binder, table_content)


### 2023 ----

current_url <- "https://www.hockey-reference.com/leagues/NHL_2023_games.html"

web <- read_html(current_url)

table_node <- web %>% html_nodes("table")

table_content <- html_table(table_node)[[1]]

colnames(table_content) <- c("Date", "Time", "Visitor", "Visitor Score", "Home", "Home Score", "Blank", "Attendance", "LOG", "Notes")

binder <- rbind(binder, table_content)

### 2022 ----

current_url <- "https://www.hockey-reference.com/leagues/NHL_2022_games.html"

web <- read_html(current_url)

table_node <- web %>% html_nodes("table")

table_content <- html_table(table_node)[[1]]

colnames(table_content) <- c("Date", "Time", "Visitor", "Visitor Score", "Home", "Home Score", "Blank", "Attendance", "LOG", "Notes")

binder <- rbind(binder, table_content)


### 2021 ----

current_url <- "https://www.hockey-reference.com/leagues/NHL_2021_games.html"

web <- read_html(current_url)

table_node <- web %>% html_nodes("table")

table_content <- html_table(table_node)[[1]]

colnames(table_content) <- c("Date", "Time", "Visitor", "Visitor Score", "Home", "Home Score", "Blank", "Attendance", "LOG", "Notes")

binder <- rbind(binder, table_content)


saveRDS(binder, "C:/Users/thigg/Desktop/Hockey Models/Seasons/Historic Season Data.RDS")
