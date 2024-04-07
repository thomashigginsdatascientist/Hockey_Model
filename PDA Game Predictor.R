

library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
# library(car)

schedule2021 <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Seasons/2021.csv")
schedule2022 <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Seasons/2022.csv")
schedule2023 <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Seasons/2023.csv")

colnames(schedule2021) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")
colnames(schedule2022) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")
colnames(schedule2023) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")

games <- rbind(schedule2021, schedule2022, schedule2023)

games$Winner <- ifelse(games$`Vistor Score` > games$`Home Score`, "V", "H")

games$Home_Win <- ifelse(games$Winner == "H", 1, 0)
games$Visitor_Win <- ifelse(games$Winner == "V", 1, 0)

homes <- games %>%
  select(Date, Home, `Home Score`, Home_Win, Vistor, `Vistor Score`) %>%
  rename("Team" = Home, "GF" = `Home Score`, "Win" = Home_Win, "Opponent" = Vistor, "GA" = `Vistor Score`) %>%
  mutate(Location = "Home")

visits <- games %>%
  select(Date, Vistor, `Vistor Score`, Visitor_Win, Home, `Home Score`) %>%
  rename("Team" = Vistor, "GF" = `Vistor Score`, "Win" = Visitor_Win, "Opponent" = Home, "GA" = `Home Score`) %>%
  mutate(Location = "Road")

games <- rbind(homes, visits)

sequence_data <- games %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Date, Team) %>%
  arrange(Date) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(Previous_Opponent = lag(Opponent)) %>%
  mutate(Previous_GF = lag(GF)) %>%
  mutate(Previous_GA = lag(GA)) %>%
  mutate(Previous_3_GF = lag(GF, n = 1) + lag(GF, n = 2) + lag(GF, n = 3)) %>%
  mutate(Previous_3_GA = lag(GA, n = 1) + lag(GA, n = 2) + lag(GA, n = 3)) %>%
  mutate(Previous_Result = lag(Win)) %>%
  mutate(Previous_3_Results = lag(Win, n = 1) + lag(Win, n = 2) + lag(Win, n = 3)) %>%
  mutate(Previous_Location = lag(Location)) %>%
  mutate(Previous_7_GF = lag(GF, n = 1) + 
           lag(GF, n = 2) + 
           lag(GF, n = 3) + 
           lag(GF, n = 4) +
           lag(GF, n = 5) +
           lag(GF, n = 6) +
           lag(GF, n = 7)) %>%
  mutate(Previous_7_GA = lag(GA, n = 1) + 
           lag(GA, n = 2) + 
           lag(GA, n = 3) + 
           lag(GA, n = 4) +
           lag(GA, n = 5) +
           lag(GA, n = 6) +
           lag(GA, n = 7)) %>%
  mutate(Previous_7_Results = lag(Win, n = 1) + 
           lag(Win, n = 2) + 
           lag(Win, n = 3) + 
           lag(Win, n = 4) +
           lag(Win, n = 5) +
           lag(Win, n = 6) +
           lag(Win, n = 7)) %>%
  mutate(Previous_15_GF = lag(GF, n = 1) + 
           lag(GF, n = 2) + 
           lag(GF, n = 3) + 
           lag(GF, n = 4) +
           lag(GF, n = 5) +
           lag(GF, n = 6) +
           lag(GF, n = 7) +
           lag(GF, n = 8) + 
           lag(GF, n = 9) + 
           lag(GF, n = 10) +
           lag(GF, n = 11) +
           lag(GF, n = 12) +
           lag(GF, n = 13) +
           lag(GF, n = 14) +
           lag(GF, n = 15)) %>%
  mutate(Previous_15_GA = lag(GA, n = 1) + 
           lag(GA, n = 2) + 
           lag(GA, n = 3) + 
           lag(GA, n = 4) +
           lag(GA, n = 5) +
           lag(GA, n = 6) +
           lag(GA, n = 7) +
           lag(GA, n = 8) + 
           lag(GA, n = 9) + 
           lag(GA, n = 10) +
           lag(GA, n = 11) +
           lag(GA, n = 12) +
           lag(GA, n = 13) +
           lag(GA, n = 14) +
           lag(GA, n = 15)) %>%
  mutate(Previous_15_Resuts = lag(Win, n = 1) + 
           lag(GA, n = 2) + 
           lag(GA, n = 3) + 
           lag(GA, n = 4) +
           lag(GA, n = 5) +
           lag(GA, n = 6) +
           lag(GA, n = 7) +
           lag(GA, n = 8) + 
           lag(GA, n = 9) + 
           lag(GA, n = 10) +
           lag(GA, n = 11) +
           lag(GA, n = 12) +
           lag(GA, n = 13) +
           lag(GA, n = 14) +
           lag(GA, n = 15)) %>%
  mutate(DOW = wday(Date, week_start = 1)) %>%
  mutate(Games_Since_Last_Game = Date - lag(Date)) %>%
  mutate(Games_Between_Last_3 = lag(Games_Since_Last_Game, n = 1) + lag(Games_Since_Last_Game, n = 2) + lag(Games_Since_Last_Game, n = 3)) %>%
  mutate(Games_Between_Last_7 = lag(Games_Since_Last_Game, n = 1) + 
           lag(Games_Since_Last_Game, n = 2) + 
           lag(Games_Since_Last_Game, n = 3) +
           lag(Games_Since_Last_Game, n = 4) +
           lag(Games_Since_Last_Game, n = 5) +
           lag(Games_Since_Last_Game, n = 6) +
           lag(Games_Since_Last_Game, n = 7)) %>%
  mutate(Games_Since_Last_Game = as.numeric(Games_Since_Last_Game)) %>%
  mutate(Games_Between_Last_3 = as.numeric(Games_Between_Last_3)) %>%
  mutate(Games_Between_Last_7 = as.numeric(Games_Between_Last_7)) %>%
  arrange(desc(Date)) %>%
  slice(1:120) %>%
  arrange(Date) %>%
  ungroup() %>%
  select(Date, Team, GF, Location, Opponent, GA, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Win)

sequence_data1 <- sequence_data[complete.cases(sequence_data),]

games1 <- games %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

dates <- unique(games1$Date)
dates <- as.data.frame(dates)
dates <- dates %>%
  filter(dates >= as.Date("01/01/2022", format = "%m/%d/%Y"))

teams <- unique(games1$Team)

binder <- games1 %>%
  select(Team, Date) %>%
  mutate(hot_score = 0)

binder <- binder[1,]
binder <- binder[-1,]
binder <- binder %>%
  select(Team, hot_score, Date)

# all_binder <- team_data[1,]
# saveRDS(all_binder, "C:/Users/thigg/Desktop/Hockey Models/temp binder.RDS")
all_binder <- readRDS("C:/Users/thigg/Desktop/Hockey Models/temp binder.RDS")
all_binder <- all_binder[-1,]

for(i in 1:nrow(dates)){

  print(i)

  current_date <- dates$dates[i]

  current_date_one <- current_date - days(1)

  current_date_two_weeks <- current_date - days(15)

  league_data <- games1 %>%
    filter(Date <= current_date_one) %>%
    filter(Date >= current_date_two_weeks) %>%
    mutate(group = 1) %>%
    group_by(group) %>%
    summarise(Avg_Win_Percentage_League = 0.5, Avg_GF_League = mean(GF), Avg_GA_League = mean(GA))

    team_data <- games1 %>%
      filter(Date <= current_date_one) %>%
      filter(Date >= current_date_two_weeks) %>%
      group_by(Team) %>%
      mutate(Num_Games = n()) %>%
      mutate(Num_Wins = sum(Win)) %>%
      mutate(GF_Team = mean(GF)) %>%
      mutate(GA_Team = mean(GA)) %>%
      mutate(Win_Percentage = Num_Wins/Num_Games) %>%
      ungroup() %>%
      mutate(Avg_Win_Percentage_League = 0.5) %>%
      mutate(Avg_GF_League = league_data$Avg_GF_League) %>%
      mutate(Avg_GA_League = league_data$Avg_GA_League) %>%
      mutate(variable_1 = (Win_Percentage - Avg_Win_Percentage_League)) %>%
      mutate(variable_2 = (GF_Team - Avg_GF_League)) %>%
      mutate(variable_3 = -(GA_Team - Avg_GA_League)) %>%
      mutate(hot_score = Win_Percentage + variable_1 + variable_2 + variable_3)
    
    all_binder <- rbind(all_binder, team_data)
    
    team_data <- team_data %>%
      distinct(Team, .keep_all = TRUE) %>%
      select(Team, hot_score)

    team_data$Date <- current_date

    binder <- rbind(binder, team_data)

}

colnames(all_binder)[16:18] <- c("Win % - Avg League Win %", "GF - League Avg GF", "Neg GA - League Avg GA")

current_hot_scores <- all_binder %>%
  select(Date, Team, hot_score, Num_Games, Num_Wins, GF_Team, GA_Team, Win_Percentage, Avg_GF_League, Avg_GA_League, `Win % - Avg League Win %`, `GF - League Avg GF`, `Neg GA - League Avg GA`) %>%
  arrange(desc(Date)) %>%
  group_by(Team) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(hot_score))

for(i in 3:length(current_hot_scores)){
  
  current_hot_scores[,i] <- round(current_hot_scores[,i], 2)
  
}

write_csv(current_hot_scores, "C:/Users/thigg/Desktop/Hockey Models/Current Hot Scores.csv")


sequence_data1 <- left_join(sequence_data1, binder, by = c("Team", "Date"))

sequence_data1$Win <- ifelse(sequence_data1$Win == 1, "W", "L")

sequence_data1 <- sequence_data1 %>%
  select(-Date)

library(caret)

train <- sequence_data1 %>%
  select(-GF, -GA)

train <- train[complete.cases(train),]
  
# start <- Sys.time()
# 
# model <- train(Win ~ .,
#                   data = train,
#                   method="pda"
#                   )
# 
# end <- Sys.time()
# 
# end - start
# 
# saveRDS(model, "C:/Users/thigg/Desktop/Hockey Models/PDA18.RDS")

model <- readRDS("C:/Users/thigg/Desktop/Hockey Models/PDA18.RDS")

preds <- read_excel("C:/Users/thigg/Desktop/Hockey Models/PDA Lifetime Predictions.xlsx", sheet = "Predictions")

Calc <- preds

Calc$Win1 <- ifelse(Calc$Model == 1, 1, 0)

Calc$Pred1 <- 1

Calc$Residual <- Calc$Win1 - Calc$Pred1

Calc$Residual <- abs(Calc$Residual)

Calc$Residual <- Calc$Residual^2

RSS <- sum(Calc$Residual)

Calc$Residual <- Calc$Win1 - mean(Calc$Win1)

Calc$Residual <- Calc$Residual^2

TSS <- sum(Calc$Residual)

Rsquared <- abs(1-(RSS/TSS))

library(MLmetrics)

Calc1 <- preds %>%
  select(Winner1, `Winner Probability`, Model) %>%
  rename("Team" = Winner1, "W" = `Winner Probability`, "obs" = Model) %>%
  mutate(W = as.numeric(W)) %>%
  mutate(L = 1-W) %>%
  mutate(pred = "W") %>%
  mutate(obs = ifelse(obs == 1, "W", "L"))

Calc2 <- preds %>%
  select(Loser1, `Loser Probability`, Model) %>%
  rename("Team" = Loser1, "L" = `Loser Probability`, "obs" = Model) %>%
  mutate(L = as.numeric(L)) %>%
  mutate(W = 1-L) %>%
  mutate(pred = "L") %>%
  mutate(obs = ifelse(obs == 1, "L", "W"))

Calc <- rbind(Calc1, Calc2)

Calc <- Calc %>%
  select(-Team) %>%
  mutate(pred = as.factor(pred)) %>%
  mutate(obs = as.factor(obs))

metrics <- data.frame(prSummary(Calc, lev = levels(Calc$obs)))
metrics$metric <- rownames(metrics)
colnames(metrics)[1] <- "Score"

Rsquared1 <- data.frame(Score = Rsquared, metric = "R Squared")

metrics <- rbind(metrics, Rsquared1)

write_csv(metrics, "C:/Users/thigg/Desktop/Hockey Models/Current Model Metrics.csv")

next_week <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Next Week Games.csv")
next_week$Date <- as.Date(next_week$Date, format = "%m/%d/%Y")

max_date <- max(dates$dates) + days(1)

next_week <- next_week %>%
  filter(Date == max_date)

colnames(next_week) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")

next_week$Winner <- ifelse(next_week$`Vistor Score` > next_week$`Home Score`, "V", "H")

next_week$Home_Win <- ifelse(next_week$Winner == "H", 1, 0)
next_week$Visitor_Win <- ifelse(next_week$Winner == "V", 1, 0)

attributes <- games %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Team) %>%
  arrange(desc(Date)) %>%
  slice(1:30) %>%
  arrange(Date) %>%
  ungroup()

homes <- next_week %>%
  select(Date, Home, `Home Score`, Home_Win, Vistor, `Vistor Score`) %>%
  rename("Team" = Home, "GF" = `Home Score`, "Win" = Home_Win, "Opponent" = Vistor, "GA" = `Vistor Score`) %>%
  mutate(Location = "Home")

visits <- next_week %>%
  select(Date, Vistor, `Vistor Score`, Visitor_Win, Home, `Home Score`) %>%
  rename("Team" = Vistor, "GF" = `Vistor Score`, "Win" = Visitor_Win, "Opponent" = Home, "GA" = `Home Score`) %>%
  mutate(Location = "Road")

next_week <- rbind(homes, visits)

next_week$Date <- as.Date(next_week$Date, format = "%m/%d/%Y")

next_week <- rbind(attributes, next_week)

next_week <- next_week %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Date, Team) %>%
  arrange(Date) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(Previous_Opponent = lag(Opponent)) %>%
  mutate(Previous_GF = lag(GF, n = 1)) %>%
  mutate(Previous_GA = lag(GA, n = 1)) %>%
  mutate(Previous_3_GF = lag(GF, n = 1) + lag(GF, n = 2) + lag(GF, n = 3)) %>%
  mutate(Previous_3_GA = lag(GA, n = 1) + lag(GA, n = 2) + lag(GA, n = 3)) %>%
  mutate(Previous_Result = lag(Win)) %>%
  mutate(Previous_3_Results = lag(Win, n = 1) + lag(Win, n = 2) + lag(Win, n = 3)) %>%
  mutate(Previous_Location = lag(Location)) %>%
  mutate(Previous_7_GF = lag(GF, n = 1) + 
           lag(GF, n = 2) + 
           lag(GF, n = 3) + 
           lag(GF, n = 4) +
           lag(GF, n = 5) +
           lag(GF, n = 6) +
           lag(GF, n = 7)) %>%
  mutate(Previous_7_GA = lag(GA, n = 1) + 
           lag(GA, n = 2) + 
           lag(GA, n = 3) + 
           lag(GA, n = 4) +
           lag(GA, n = 5) +
           lag(GA, n = 6) +
           lag(GA, n = 7)) %>%
  mutate(Previous_7_Results = lag(Win, n = 1) + 
           lag(Win, n = 2) + 
           lag(Win, n = 3) + 
           lag(Win, n = 4) +
           lag(Win, n = 5) +
           lag(Win, n = 6) +
           lag(Win, n = 7)) %>%
  mutate(Previous_15_GF = lag(GF, n = 1) + 
           lag(GF, n = 2) + 
           lag(GF, n = 3) + 
           lag(GF, n = 4) +
           lag(GF, n = 5) +
           lag(GF, n = 6) +
           lag(GF, n = 7) +
           lag(GF, n = 8) + 
           lag(GF, n = 9) + 
           lag(GF, n = 10) +
           lag(GF, n = 11) +
           lag(GF, n = 12) +
           lag(GF, n = 13) +
           lag(GF, n = 14) +
           lag(GF, n = 15)) %>%
  mutate(Previous_15_GA = lag(GA, n = 1) + 
           lag(GA, n = 2) + 
           lag(GA, n = 3) + 
           lag(GA, n = 4) +
           lag(GA, n = 5) +
           lag(GA, n = 6) +
           lag(GA, n = 7) +
           lag(GA, n = 8) + 
           lag(GA, n = 9) + 
           lag(GA, n = 10) +
           lag(GA, n = 11) +
           lag(GA, n = 12) +
           lag(GA, n = 13) +
           lag(GA, n = 14) +
           lag(GA, n = 15)) %>%
  mutate(Previous_15_Resuts = lag(Win, n = 1) + 
           lag(GA, n = 2) + 
           lag(GA, n = 3) + 
           lag(GA, n = 4) +
           lag(GA, n = 5) +
           lag(GA, n = 6) +
           lag(GA, n = 7) +
           lag(GA, n = 8) + 
           lag(GA, n = 9) + 
           lag(GA, n = 10) +
           lag(GA, n = 11) +
           lag(GA, n = 12) +
           lag(GA, n = 13) +
           lag(GA, n = 14) +
           lag(GA, n = 15)) %>%
  mutate(DOW = wday(Date, week_start = 1)) %>%
  mutate(Games_Since_Last_Game = Date - lag(Date)) %>%
  mutate(Games_Between_Last_3 = lag(Games_Since_Last_Game, n = 1) + lag(Games_Since_Last_Game, n = 2) + lag(Games_Since_Last_Game, n = 3)) %>%
  mutate(Games_Between_Last_7 = lag(Games_Since_Last_Game, n = 1) + 
           lag(Games_Since_Last_Game, n = 2) + 
           lag(Games_Since_Last_Game, n = 3) +
           lag(Games_Since_Last_Game, n = 4) +
           lag(Games_Since_Last_Game, n = 5) +
           lag(Games_Since_Last_Game, n = 6) +
           lag(Games_Since_Last_Game, n = 7)) %>%
  mutate(Games_Since_Last_Game = as.numeric(Games_Since_Last_Game)) %>%
  mutate(Games_Between_Last_3 = as.numeric(Games_Between_Last_3)) %>%
  mutate(Games_Between_Last_7 = as.numeric(Games_Between_Last_7)) %>%
  arrange(Date) %>%
  ungroup() %>%
  select(Date, Team, GF, Location, Opponent, GA, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Win)
  
next_week1 <- next_week %>%
  filter(Date == max_date) %>%
  # filter(Date >= as.Date("12/13/2023", format = "%m/%d/%Y")) %>%
  select(-Win, -GF, -GA)

binder1 <- binder %>%
  arrange(desc(Date)) %>%
  group_by(Team) %>%
  slice(1) %>%
  select(Team, Date, hot_score) %>%
  rename("Score_Date" = Date) %>%
  ungroup() %>%
  filter(Team %in% next_week1$Team)

next_week1 <- left_join(next_week1, binder1, by = "Team")

next_week1 <- next_week1[complete.cases(next_week1),]

today_preds <- predict(model, newdata = next_week1, type = "prob")

next_week1 <- cbind(next_week1, today_preds)

next_week1$Winner <- ifelse(next_week1$L > next_week1$W, next_week1$Opponent, next_week1$Team)

next_week1$Loser <- ifelse(next_week1$L > next_week1$W, next_week1$Team, next_week1$Opponent)

next_week1$Confidence <- ifelse(next_week1$L > next_week1$W, next_week1$L, next_week1$W)

next_week1 <- next_week1 %>%
  select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, hot_score, W, L, Winner, Loser, Confidence)

next_week1$Win_Probability <- next_week1$W

prediction_data <- next_week1

prediction_data <- prediction_data %>%
  mutate(Team1 = ifelse(Location == "Home", Team, Opponent)) %>%
  mutate(Team2 = ifelse(Location == "Home", Opponent, Team)) %>%
  mutate(Team1_Score = ifelse(Team1 == Team, W, L)) %>%
  mutate(Team2_Score = ifelse(Team2 == Team, W, L)) %>%
  group_by(Team1) %>%
  mutate(Avg_Score1 = mean(Team1_Score)) %>%
  ungroup() %>%
  group_by(Team2) %>%
  mutate(Avg_Score2 = mean(Team2_Score)) %>%
  ungroup() %>%
  mutate(Winner1 = ifelse(Avg_Score1 > Avg_Score2, Team1, Team2)) %>%
  mutate(Loser1 = ifelse(Avg_Score1 > Avg_Score2, Team2, Team1)) %>%
  mutate(Confidence = ifelse(Winner1 == Team1, Avg_Score1, Avg_Score2))

thomas <- prediction_data %>%
  ungroup() %>%
  filter(Location == "Home") %>%
  mutate(Loser_Confidence = 1 - Confidence) %>%
  mutate(Site = paste("Playing At: ", Team, " ", Location)) %>%
  select(Date, Winner1,  Confidence, Loser1,  Loser_Confidence, Site, hot_score) %>%
  arrange(desc(Confidence)) %>%
  rename("Winner Probability" = Confidence, "Loser Probability" = Loser_Confidence) %>%
  mutate(Category = case_when(`Winner Probability` > .85 ~ "85 <",
                              `Winner Probability` > .8 & `Winner Probability` <= .85 ~ "80-85",
                              `Winner Probability` > .75 & `Winner Probability` <= .8 ~ "75-80",
                              `Winner Probability` > .70 & `Winner Probability` <= .75 ~ "70-75",
                              `Winner Probability` > .6 & `Winner Probability` <= .7 ~ "60-70",
                              `Winner Probability` > .55 & `Winner Probability` <= .6 ~ "55-60",
                              `Winner Probability` <= .55 ~ "55 >",
                              TRUE ~ "Thomas")) %>%
  mutate(`Winner Probability` = percent(`Winner Probability`)) %>%
  mutate(`Loser Probability` = percent(`Loser Probability`)) %>%
  select(Date, Winner1,  `Winner Probability`, Loser1,  `Loser Probability`, Site, Category, hot_score)

write_csv(thomas, "C:/Users/thigg/Desktop/Hockey Models/PDA Today Results.csv")


# predictions <- read_csv("C:/Users/thigg/Desktop/Hockey Models/temp.csv")
# 
# predictions <- predictions %>%
#   mutate(Category = case_when(`Winner Probability` > .85 ~ "85 <",
#                               `Winner Probability` > .8 & `Winner Probability` <= .85 ~ "80-85",
#                               `Winner Probability` > .75 & `Winner Probability` <= .8 ~ "75-80",
#                               `Winner Probability` > .70 & `Winner Probability` <= .75 ~ "70-75",
#                               `Winner Probability` > .6 & `Winner Probability` <= .7 ~ "60-70",
#                               `Winner Probability` > .55 & `Winner Probability` <= .6 ~ "55-60",
#                               `Winner Probability` <= .55 ~ "55 >",
#                               TRUE ~ "Thomas"))
# 
# write_csv(predictions, "C:/Users/thigg/Desktop/Hockey Models/temp.csv")

# 
# predictions$Site <- gsub("Playing At: ", "", predictions$Site)
# predictions$Site <- gsub(" Home", "", predictions$Site)
# predictions$Site <- trimws(predictions$Site)
# 
# predictions$UID <- paste(predictions$Site, predictions$Date, sep = "---")
# 
# 
# schedule20231 <- schedule2023 %>%
#   mutate(UID = paste0(Home, "---", Date)) %>%
#   select(UID, OTSO)
# 
# predictions <- left_join(predictions, schedule20231, by = "UID")
# 
# SOOT <- predictions %>%
#   filter(!is.na(OTSO))
# 
# predictions <- predictions %>%
#   filter(is.na(OTSO))
# 
# predictions1 <- predictions %>%
#   group_by(Category) %>%
#   summarise(Total = n(), Total_Correct = sum(Model)) %>%
#   mutate(Percent = Total_Correct/Total)
# 
# sum(predictions1$Total_Correct)/sum(predictions1$Total)
# 
# test70 <- predictions1[3,2] + predictions1[4,2] + predictions1[5,2] + predictions1[6,2]
# 
# test70c <- predictions1[3,3] + predictions1[4,3] + predictions1[5,3] + predictions1[6,3]
# 
# test70c/test70
# 
# test80 <- predictions1[4,2] + predictions1[5,2] + predictions1[6,2]
# 
# test80c <- predictions1[4,3] + predictions1[5,3] + predictions1[6,3]
# 
# test80c/test80




# all_binder1 <- all_binder %>%
#   mutate(UID = paste(Team, Date, sep = "---")) %>%
#   distinct(UID, .keep_all = TRUE) %>%
#   group_by(Team) %>%
#   arrange(Date) %>%
#   mutate(next_3_games = Win + lead(Win, n = 1) + lead(Win, n = 2)) %>%
#   mutate(Next_Win = lead(Win, n=1)) %>%
#   mutate(Streak_Signal = ifelse(Win == 1 & Next_Win == 1, 1, 0)) %>%
#   mutate(Streak = cumsum(Streak_Signal %in% 0) + 1) %>%
#   ungroup() %>%
#   group_by(Team, Streak) %>%
#   mutate(Total_Streak = sum(Streak_Signal)) %>%
#   ungroup() %>%
#   filter(Total_Streak > 1) %>%
#   group_by(Team, Streak) %>%
#   slice(2:n()) %>%
#   mutate(Streak_Min_Date = min(Date)) %>%
#   ungroup()
# 
# combos <- all_binder1 %>%
#   mutate(UID = paste(Team, Streak_Min_Date, sep = "&&&")) %>%
#   distinct(UID, .keep_all = TRUE) %>%
#   select(UID, Date, Team)
# 
# 
# combo_binder <- all_binder %>%
#   select(Team, Date) %>%
#   slice(1)
# 
# combo_binder$Previous_3_Hot_Score <- 0
# combo_binder$Previous_7_Hot_Score <- 0
# 
# combo_binder <- combo_binder %>%
#   select(Team, Previous_3_Hot_Score, Previous_7_Hot_Score, Date)
# 
# combo_binder <- combo_binder[-1,]
# 
# for(i in 1:nrow(combos)){
#   
#   print(i)
#   
#   current_combo <- combos[i,]
#   current_team <- current_combo$Team
#   current_date <- current_combo$Date
#   current_date1 <- current_date - days(1)
#   
#   lookup_data <- all_binder %>%
#     mutate(UID = paste(Team, Date, sep = "---")) %>%
#     distinct(UID, .keep_all = TRUE) %>%
#     filter(Team == current_team) %>%
#     filter(Date <= current_date1) %>%
#     arrange(Date) %>%
#     mutate(Previous_3_Hot_Score = hot_score + lag(hot_score, n = 1) + lag(hot_score, n = 2)) %>%
#     mutate(Previous_7_Hot_Score = hot_score + 
#              lag(hot_score, n = 1) + 
#              lag(hot_score, n = 2) + 
#              lag(hot_score, n = 3) +
#              lag(hot_score, n = 4) +
#              lag(hot_score, n = 5) +
#              lag(hot_score, n = 6) +
#              lag(hot_score, n = 7)) %>%
#     arrange(desc(Date)) %>%
#     slice(1) %>%
#     select(Team, Previous_3_Hot_Score, Previous_7_Hot_Score)
#   
#   lookup_data$Date <- current_date
#   
#   combo_binder <- rbind(combo_binder, lookup_data)
#   
# }
# 
# combo_binder <- combo_binder[complete.cases(combo_binder),]
# 
# all_binder1 <- left_join(all_binder1, combo_binder, by = c("Team", "Date"))
# 
# all_binder1 <- all_binder1 %>%
#   filter(!is.na(Previous_3_Hot_Score)) %>%
#   filter(!is.na(Previous_7_Hot_Score)) %>%
#   mutate(Previous_3_Hot_Score1 = round(Previous_3_Hot_Score, 0)) %>%
#   mutate(Previous_7_Hot_Score1 = round(Previous_7_Hot_Score, 0))
# 
# all_binder2 <- all_binder1 %>%
#   group_by(Total_Streak, Previous_3_Hot_Score1) %>%
#   summarise(Total = n()) %>%
#   ungroup() %>%
#   mutate(Total_Obs_Streak_Above_3 = sum(Total)) %>%
#   mutate(Total_Streaks_Above_1 = nrow(all_binder1)) %>%
#   mutate(Above_0 = ifelse(Previous_3_Hot_Score1 > 0, Total, 0)) %>%
#   mutate(Total_Obs_Above_0_Streak_Above_3 = sum(Above_0))
# 
# all_binder3 <- all_binder1 %>%
#   group_by(Total_Streak, Previous_7_Hot_Score1) %>%
#   summarise(Total = n()) %>%
#   ungroup() %>%
#   mutate(Total_Obs_Streak_Above_3 = sum(Total)) %>%
#   mutate(Total_Streaks_Above_1 = nrow(all_binder1)) %>%
#   mutate(Above_0 = ifelse(Previous_7_Hot_Score1 > 0, Total, 0)) %>%
#   mutate(Total_Obs_Above_0_Streak_Above_3 = sum(Above_0))
# 
# 
# all_binder_not_streaks <- all_binder %>%
#   mutate(UID = paste(Team, Date, sep = "---")) %>%
#   distinct(UID, .keep_all = TRUE) %>%
#   group_by(Team) %>%
#   arrange(Date) %>%
#   mutate(next_3_games = Win + lead(Win, n = 1) + lead(Win, n = 2)) %>%
#   mutate(Next_Win = lead(Win, n=1)) %>%
#   mutate(Streak_Signal = ifelse(Win == 1 & Next_Win == 1, 1, 0)) %>%
#   mutate(Streak = cumsum(Streak_Signal %in% 0) + 1) %>%
#   ungroup() %>%
#   group_by(Team, Streak) %>%
#   mutate(Total_Streak = sum(Streak_Signal)) %>%
#   ungroup() %>%
#   filter(Total_Streak <= 1) %>%
#   group_by(Team, Streak) %>%
#   slice(2:n()) %>%
#   mutate(Streak_Min_Date = min(Date)) %>%
#   ungroup()
# 
# 
# combos <- all_binder_not_streaks %>%
#   mutate(UID = paste(Team, Streak_Min_Date, sep = "&&&")) %>%
#   distinct(UID, .keep_all = TRUE) %>%
#   select(UID, Date, Team)
# 
# 
# combo_binder <- all_binder %>%
#   select(Team, Date) %>%
#   slice(1)
# 
# combo_binder$Previous_3_Hot_Score <- 0
# combo_binder$Previous_7_Hot_Score <- 0
# 
# combo_binder <- combo_binder %>%
#   select(Team, Previous_3_Hot_Score, Previous_7_Hot_Score, Date)
# 
# combo_binder <- combo_binder[-1,]
# 
# for(i in 1:nrow(combos)){
#   
#   print(i)
#   
#   current_combo <- combos[i,]
#   current_team <- current_combo$Team
#   current_date <- current_combo$Date
#   current_date1 <- current_date - days(1)
#   
#   lookup_data <- all_binder %>%
#     mutate(UID = paste(Team, Date, sep = "---")) %>%
#     distinct(UID, .keep_all = TRUE) %>%
#     filter(Team == current_team) %>%
#     filter(Date <= current_date1) %>%
#     arrange(Date) %>%
#     mutate(Previous_3_Hot_Score = hot_score + lag(hot_score, n = 1) + lag(hot_score, n = 2)) %>%
#     mutate(Previous_7_Hot_Score = hot_score + 
#              lag(hot_score, n = 1) + 
#              lag(hot_score, n = 2) + 
#              lag(hot_score, n = 3) +
#              lag(hot_score, n = 4) +
#              lag(hot_score, n = 5) +
#              lag(hot_score, n = 6) +
#              lag(hot_score, n = 7)) %>%
#     arrange(desc(Date)) %>%
#     slice(1) %>%
#     select(Team, Previous_3_Hot_Score, Previous_7_Hot_Score)
#   
#   lookup_data$Date <- current_date
#   
#   combo_binder <- rbind(combo_binder, lookup_data)
#   
# }
# 
# 
# all_binder_not_streaks <- left_join(all_binder_not_streaks, combo_binder, by = c("Team", "Date"))
# 
# all_binder_not_streaks <- all_binder_not_streaks %>%
#   filter(!is.na(Previous_3_Hot_Score)) %>%
#   filter(!is.na(Previous_7_Hot_Score)) %>%
#   mutate(Previous_3_Hot_Score1 = round(Previous_3_Hot_Score, 0)) %>%
#   mutate(Previous_7_Hot_Score1 = round(Previous_7_Hot_Score, 0))
# 
# Not_streaks3 <- all_binder_not_streaks %>%
#   group_by(Total_Streak, Previous_3_Hot_Score1) %>%
#   summarise(Total = n()) %>%
#   ungroup() %>%
#   mutate(Total_Obs_Streak_Above_3 = sum(Total)) %>%
#   mutate(Total_Streaks_Above_1 = nrow(all_binder_not_streaks)) %>%
#   mutate(Above_0 = ifelse(Previous_3_Hot_Score1 > 0, Total, 0)) %>%
#   mutate(Total_Obs_Above_0_Streak_Above_3 = sum(Above_0))
# 
# Not_streaks7 <- all_binder_not_streaks %>%
#   group_by(Total_Streak, Previous_7_Hot_Score1) %>%
#   summarise(Total = n()) %>%
#   ungroup() %>%
#   mutate(Total_Obs_Streak_Above_3 = sum(Total)) %>%
#   mutate(Total_Streaks_Above_1 = nrow(all_binder1)) %>%
#   mutate(Above_0 = ifelse(Previous_7_Hot_Score1 > 0, Total, 0)) %>%
#   mutate(Total_Obs_Above_0_Streak_Above_3 = sum(Above_0)) 
# 
# 
# prev3 <- rbind(Not_streaks3, all_binder2)
# 
# prev3 <- prev3 %>%
#   mutate(Previous_3_Hot_Score1 = ifelse(Previous_3_Hot_Score1 < 0, -1, Previous_3_Hot_Score1)) %>%
#   mutate(Total_Streak = ifelse(Total_Streak > 5, 5, Total_Streak)) %>%
#   group_by(Previous_3_Hot_Score1, Total_Streak) %>%
#   summarise(Total = sum(Total)) %>%
#   mutate(UID = paste(Previous_3_Hot_Score1, Total_Streak, sep = "---")) %>%
#   ungroup() %>%
#   mutate(Total_obs = sum(Total)) %>%
#   mutate(Perc = Total/Total_obs) %>%
#   arrange(desc(Perc)) %>%
#   mutate(Perc_sum = cumsum(Perc))
# 
# library(plotly)
# 
# plot_ly(data = prev3, x = ~UID, y = ~Total)
# 
# all_binder_loss <- all_binder %>%
#   filter(Win == 0)
# 
# hist(all_binder_loss$hot_score)
# 
# all_binder_win <- all_binder %>%
#   filter(Win == 1)
# 
# hist(all_binder_win$hot_score)