

library(tidyverse)
library(lubridate)
library(scales)
library(readxl)

schedule <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Next Week Games.csv")

schedule$Date <- as.Date(schedule$Date, format = "%m/%d/%Y")

schedule$Week <- lubridate::week(schedule$Date)

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
  filter(Min_Date >= as.Date("2/26/2024", format = "%m/%d/%Y")) %>%
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
  mutate(Total_Score = Score + Next_Week_Score + Two_Week_Score + Three_Week_Score) %>%
  mutate(Total_Games = Num_Games + Next_Week_Num_Games + Two_Week_Num_Games + Three_Week_Num_Games) %>%
  mutate(Total_Light_Games = Light_Days + Next_Week_Light_Days + Two_Week_Light_Days + Three_Week_Light_Days) %>%
  arrange(desc(Total_Score)) %>%
  mutate(Score_Rank = min_rank(Total_Score)) %>%
  mutate(Games_Rank = min_rank(Total_Games)) %>%
  mutate(Light_Rank = min_rank(Total_Light_Games)) %>%
  mutate(Total_Rank = Score_Rank + Games_Rank + Light_Rank)
  

write_csv(analysis2, "C:/Users/thigg/Desktop/Hockey Models/Schedule Analysis/Schedule Analysis.csv")  
