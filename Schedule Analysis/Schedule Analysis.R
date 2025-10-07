

library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(openxlsx)


schedule <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Next Week Games.csv")

schedule <- schedule %>%
  select(-Time)

schedule$Date <- as.Date(schedule$Date, format = "%m/%d/%Y")

#Always start on a Monday
start_date <- as.Date("10/06/2025", format = "%m/%d/%Y")

filter_date <- start_date

dates <- seq.Date(from = start_date, to = as.Date("01/01/2027", format = "%m/%d/%Y"), by = "days")

dates <- as.data.frame(dates)

num <- nrow(dates)/7

num <- ceiling(num)

weeks <- rep(1:num, each=7)

weeks <- weeks[1:nrow(dates)]

dates$Week <- weeks

colnames(dates) <- c("Date", "Week")

# schedule$Week <- lubridate::isoweek(schedule$Date)
# 
# schedule$Year <- lubridate::year(schedule$Date)

schedule <- left_join(schedule, dates, by = "Date")

analysis <- schedule %>%
  group_by(Week, Date) %>%
  summarise(Num_Games = n()) %>%
  arrange(Num_Games) %>%
  mutate(ranking = 1:n()) %>%
  mutate(Target = ifelse(ranking <= 4, 1, 0)) %>%
  ungroup() %>%
  group_by(Week) %>%
  mutate(Min_Date = min(Date)) %>%
  ungroup()

colnames(schedule) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")

schedule$Winner <- ifelse(schedule$`Vistor Score` > schedule$`Home Score`, "V", "H")

schedule$Home_Win <- ifelse(schedule$Winner == "H", 1, 0)
schedule$Visitor_Win <- ifelse(schedule$Winner == "V", 1, 0)

homes <- schedule %>%
  select(Date, Home, `Home Score`, Home_Win, Vistor, `Vistor Score`) %>%
  rename("Team" = Home, "GF" = `Home Score`, "Win" = Home_Win, "Opponent" = Vistor, "GA" = `Vistor Score`) %>%
  mutate(Location = "Home")

visits <- schedule %>%
  select(Date, Vistor, `Vistor Score`, Visitor_Win, Home, `Home Score`) %>%
  rename("Team" = Vistor, "GF" = `Vistor Score`, "Win" = Visitor_Win, "Opponent" = Home, "GA" = `Home Score`) %>%
  mutate(Location = "Road")

schedule <- rbind(homes, visits)

schedule <- left_join(schedule, analysis, by = "Date")

analysis2 <- schedule %>%
  group_by(Team, Week, Min_Date) %>%
  summarise(Num_Games = n(), Light_Days = sum(Target)) %>%
  mutate(Heavy_Days = Num_Games - Light_Days) %>%
  arrange(Min_Date) %>%
  filter(Min_Date >= filter_date) %>%
  ungroup() %>%
  mutate(Light_Multiplier = Light_Days * 1.5) %>%
  mutate(Heavy_Multiplier = Heavy_Days * 0.5) %>%
  mutate(Score = Num_Games + Light_Multiplier + Heavy_Multiplier) %>%
  arrange(Min_Date) %>%
  group_by(Team) %>%
  mutate(Next_Week_Score = lead(Score, n = 1)) %>%
  mutate(Two_Week_Score = lead(Score, n = 2)) %>%
  mutate(Three_Week_Score = lead(Score, n = 3)) %>%
  mutate(Two_Week_Total = Score + Next_Week_Score) %>%
  mutate(Three_Week_Total = Score + Next_Week_Score + Two_Week_Score) %>%
  mutate(Month_Total = Score + Next_Week_Score + Two_Week_Score + Three_Week_Score) %>%
  mutate(Next_Week_Num_Games = lead(Num_Games, n = 1)) %>%
  mutate(Two_Week_Num_Games = lead(Num_Games, n = 2)) %>%
  mutate(Three_Week_Num_Games = lead(Num_Games, n = 3)) %>%
  mutate(Two_Week_Games = Num_Games + Next_Week_Num_Games) %>%
  mutate(Three_Week_Games = Num_Games + Next_Week_Num_Games + Two_Week_Num_Games) %>%
  mutate(Month_Games = Num_Games + Next_Week_Num_Games + Two_Week_Num_Games + Three_Week_Num_Games) %>%
  mutate(Next_Week_Light_Days = lead(Light_Days, n = 1)) %>%
  mutate(Two_Week_Light_Days = lead(Light_Days, n = 2)) %>%
  mutate(Three_Week_Light_Days = lead(Light_Days, n = 3)) %>%
  mutate(Two_Week_Light = Light_Days + Next_Week_Light_Days) %>%
  mutate(Three_Week_Light = Light_Days + Next_Week_Light_Days + Two_Week_Light_Days) %>%
  mutate(Month_Light = Light_Days + Next_Week_Light_Days + Two_Week_Light_Days + Three_Week_Light_Days) %>%
  slice(1) %>%
  ungroup() %>%
  select(Team, Min_Date, Num_Games, Light_Days, Heavy_Days, Score, Next_Week_Num_Games, Next_Week_Light_Days, Next_Week_Score, Two_Week_Num_Games, Two_Week_Light_Days, Two_Week_Score, Three_Week_Num_Games, Three_Week_Light_Days, Three_Week_Score) %>%
  arrange(desc(Score)) %>%
  ungroup() %>%
  mutate(Score = ifelse(is.na(Score), 0, Score)) %>%
  mutate(Next_Week_Score = ifelse(is.na(Next_Week_Score), 0, Next_Week_Score)) %>%
  mutate(Two_Week_Score = ifelse(is.na(Two_Week_Score), 0, Two_Week_Score)) %>%
  mutate(Three_Week_Score = ifelse(is.na(Three_Week_Score), 0, Three_Week_Score)) %>%
  mutate(Total_Score = Score + Next_Week_Score + Two_Week_Score + Three_Week_Score) %>%
  mutate(Num_Games = ifelse(is.na(Num_Games), 0, Num_Games)) %>%
  mutate(Next_Week_Num_Games = ifelse(is.na(Next_Week_Num_Games), 0, Next_Week_Num_Games)) %>%
  mutate(Two_Week_Num_Games = ifelse(is.na(Two_Week_Num_Games), 0, Two_Week_Num_Games)) %>%
  mutate(Three_Week_Num_Games = ifelse(is.na(Three_Week_Num_Games), 0, Three_Week_Num_Games)) %>%
  mutate(Total_Games = Num_Games + Next_Week_Num_Games + Two_Week_Num_Games + Three_Week_Num_Games) %>%
  mutate(Light_Days = ifelse(is.na(Light_Days), 0, Light_Days)) %>%
  mutate(Next_Week_Light_Days = ifelse(is.na(Next_Week_Light_Days), 0, Next_Week_Light_Days)) %>%
  mutate(Two_Week_Light_Days = ifelse(is.na(Two_Week_Light_Days), 0, Two_Week_Light_Days)) %>%
  mutate(Three_Week_Light_Days = ifelse(is.na(Three_Week_Light_Days), 0, Three_Week_Light_Days)) %>%
  mutate(Total_Light_Games = Light_Days + Next_Week_Light_Days + Two_Week_Light_Days + Three_Week_Light_Days) %>%
  arrange(desc(Total_Score)) %>%
  mutate(Score_Rank = min_rank(Total_Score)) %>%
  mutate(Games_Rank = min_rank(Total_Games)) %>%
  mutate(Light_Rank = min_rank(Total_Light_Games)) %>%
  mutate(Total_Rank = Score_Rank + Games_Rank + Light_Rank)
  

next_week_analysis3 <- schedule %>%
  arrange(Date) %>%
  filter(Date >= filter_date) %>%
  filter(Date <= (filter_date + days(6))) %>%
  mutate(week_day = wday(Date, label = TRUE, week_start = 1)) %>%
  group_by(Team, week_day) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = week_day, values_from = count)

four_week_analysis3 <- schedule %>%
  arrange(Date) %>%
  filter(Date >= filter_date) %>%
  filter(Date <= (filter_date + weeks(4))) %>%
  mutate(week_day = wday(Date, label = TRUE, week_start = 1)) %>%
  group_by(Team, week_day) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = week_day, values_from = count)

binder <- four_week_analysis3[1,]

binder$Team <- NA
binder$Mon <- NA
binder$Tue <- NA
binder$Wed <- NA
binder$Thu <- NA
binder$Fri <- NA
binder$Sat <- NA
binder$Sun <- NA

next_week_analysis3 <- rbind(binder, next_week_analysis3) 

next_week_analysis3 <- next_week_analysis3 %>%
  filter(!is.na(Team))

next_week_analysis3[is.na(next_week_analysis3)] <- 0

next_week_analysis3 <- next_week_analysis3 %>%
  mutate(Total_Light = Mon + Wed + Fri + Sun) %>%
  mutate(Total_Heavy = Tue + Thu + Sat)

four_week_analysis3[is.na(four_week_analysis3)] <- 0

four_week_analysis3 <- four_week_analysis3 %>%
  mutate(Total_Light = Mon + Wed + Fri + Sun) %>%
  mutate(Total_Heavy = Tue + Thu + Sat)

# analysis3 <- rbind(four_week_analysis3, binder)
# 
# analysis3 <- rbind(analysis3, next_week_analysis3)

this_week <- analysis2 %>%
  select(Team, Num_Games, Light_Days, Score) %>%
  arrange(desc(Score))

next_week <- analysis2 %>%
  select(Team, Next_Week_Num_Games, Next_Week_Light_Days, Next_Week_Score) %>%
  arrange(desc(Next_Week_Score))


wb <- createWorkbook()

DF = this_week
name = "This Week SA"
addWorksheet(wb, sheetName = name)
writeData(wb, sheet = name, x = DF)
addFilter(wb, name, row = 1, cols = 1:ncol(DF))
width_vec <- apply(DF, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(DF))  + 2
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, name, cols = 1:ncol(DF), widths = max_vec_header)

DF = next_week
name = "Next Week SA"
addWorksheet(wb, sheetName = name)
writeData(wb, sheet = name, x = DF)
addFilter(wb, name, row = 1, cols = 1:ncol(DF))
width_vec <- apply(DF, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(DF))  + 2
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, name, cols = 1:ncol(DF), widths = max_vec_header)

DF = analysis2
name = "All Schedule Analysis"
addWorksheet(wb, sheetName = name)
writeData(wb, sheet = name, x = DF)
addFilter(wb, name, row = 1, cols = 1:ncol(DF))
width_vec <- apply(DF, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(DF))  + 2
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, name, cols = 1:ncol(DF), widths = max_vec_header)

DF = next_week_analysis3
name = "This Week DOW"
addWorksheet(wb, sheetName = name)
writeData(wb, sheet = name, x = DF)
addFilter(wb, name, row = 1, cols = 1:ncol(DF))
width_vec <- apply(DF, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(DF))  + 2
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, name, cols = 1:ncol(DF), widths = max_vec_header)

DF = four_week_analysis3
name = "All DOW"
addWorksheet(wb, sheetName = name)
writeData(wb, sheet = name, x = DF)
addFilter(wb, name, row = 1, cols = 1:ncol(DF))
width_vec <- apply(DF, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(DF))  + 2
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, name, cols = 1:ncol(DF), widths = max_vec_header)


saveWorkbook(wb, "C:/Users/thigg/Desktop/Hockey Models/Schedule Analysis/Schedule Analysis.xlsx", overwrite = TRUE)


# write_csv(analysis2, "C:/Users/thigg/Desktop/Hockey_Model/Schedule Analysis/Schedule Analysis.csv")  
