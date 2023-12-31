

library(tidyverse)
library(lubridate)
library(scales)

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
  select(Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Win)

sequence_data1 <- sequence_data[complete.cases(sequence_data),]

location_data <- games %>%
  group_by(Team, Location) %>%
  arrange(desc(Date)) %>%
  slice(1:82) %>%
  summarise(Wins = sum(Win), Avg_GF = mean(GF), Total_GF = sum(GF), num_games = n()) %>%
  mutate(Losses = num_games - Wins) %>%
  mutate(Win_Percentage = Wins/num_games) %>%
  select(Team, Location, Avg_GF, Win_Percentage)

sequence_data1 <- left_join(sequence_data1, location_data, by = c("Team", "Location"))

sequence_data1$Win <- ifelse(sequence_data1$Win == 1, "W", "L")

# sequence_data1$Win <- as.factor(sequence_data1$Win)

# sequence_data2 <- sequence_data1 %>%
#   mutate(Team = as.factor(Team)) %>%
#   mutate(Location = as.factor(Location)) %>%
#   mutate(Opponent = as.factor(Opponent)) %>%
#   mutate(Previous_Opponent = as.factor(Previous_Opponent)) %>%
#   mutate(Previous_Location = as.factor(Previous_Location))

#previous location played at
#was previous location road or home
#day of week
#predict final score and probability to win

library(caret)

set.seed(31)
Train_Index <- createDataPartition(sequence_data1$Win, p = .95, 
                                   list = F, 
                                   times = 1)
train <- sequence_data1[ Train_Index,]
test  <- sequence_data1[-Train_Index,]

train <- train %>%
  ungroup() %>%
  as.data.frame()

test <- test %>%
  ungroup() %>%
  as.data.frame()

test1 <- test %>%
  select(-Win)

actuals <- test %>%
  select(Win)

# control <- trainControl(method='repeatedcv',
#                         number=10,
#                         repeats=3,
#                         verboseIter = TRUE)
# 
# 
# start <- Sys.time()
# 
# gbm_model <- train(Win ~.,
#                data = train,
#                method = "gbm",
#                trControl = control,
#                verbose = TRUE)
# 
# end <- Sys.time()
# 
# end - start

# saveRDS(gbm_model, "C:/Users/thigg/Desktop/Hockey Models/GBM2.RDS")

#R Squared of .52883771

#AUC of .6623322

gbm_model <- readRDS("C:/Users/thigg/Desktop/Hockey Models/GBM2.RDS")

library(gbm)

summary(gbm_model)

varImp(gbm_model)

preds <- predict(gbm_model, newdata = test1, type = "prob")

test <- cbind(test, preds)

test$pred_abs <- ifelse(test$L > test$W, "L", "W")

test$pred_abs1 <- ifelse(test$Win == test$pred_abs, 1, 0)


Calc <- test

Calc$Win1 <- ifelse(Calc$Win == "W", 1, 0)

Calc$Pred1 <- ifelse(Calc$pred_abs == "W", 1, 0)

Calc$Residual <- Calc$Win1 - Calc$Pred1

Calc$Residual <- abs(Calc$Residual)

Calc$Residual <- Calc$Residual^2

RSS <- sum(Calc$Residual)

Calc$Residual <- Calc$Win1 - mean(Calc$Win1)

Calc$Residual <- Calc$Residual^2

TSS <- sum(Calc$Residual)

Rsquared <- abs(1-(RSS/TSS))

library(MLmetrics)

Calc <- test %>%
  select(Win, W, L) %>%
  mutate(pred = factor(ifelse(W > L, "W", "L"))) %>%
  rename("obs" = Win) %>%
  mutate(obs = as.factor(obs))

prSummary(Calc, lev = levels(Calc$obs))


sum(test$pred_abs1)/nrow(test)

confusionMatrix(as.factor(test$pred_abs), as.factor(test$Win), mode = "prec_recall")

test$highest <- ifelse(test$L > test$Win, test$L, test$W)

test2 <- test %>%
  filter(highest >= .7)

sum(test2$pred_abs1)/nrow(test2)

next_week <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Next Week Games.csv")
next_week$Date <- as.Date(next_week$Date, format = "%m/%d/%Y")
next_week <- next_week %>%
  filter(Date == Sys.Date())

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
  select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Win)

  
next_week1 <- next_week %>%
  filter(Date == Sys.Date()) %>%
  # filter(Date >= as.Date("12/13/2023", format = "%m/%d/%Y")) %>%
  select(-Win)

next_week1 <- next_week1[complete.cases(next_week1),]

next_week1 <- left_join(next_week1, location_data, by = c("Team", "Location"))

today_preds <- predict(gbm_model, newdata = next_week1, type = "prob")

next_week1 <- cbind(next_week1, today_preds)

next_week1$Winner <- ifelse(next_week1$L > next_week1$W, next_week1$Opponent, next_week1$Team)

next_week1$Loser <- ifelse(next_week1$L > next_week1$W, next_week1$Team, next_week1$Opponent)


next_week1$Confidence <- ifelse(next_week1$L > next_week1$W, next_week1$L, next_week1$W)

thomas <- next_week1 %>%
  ungroup() %>%
  filter(Location == "Home") %>%
  mutate(Loser_Confidence = 1 - Confidence) %>%
  mutate(Site = paste("Playing At: ", Team, " ", Location)) %>%
  select(Date, Winner, Confidence, Loser, Loser_Confidence, Site) %>%
  arrange(desc(Confidence)) %>%
  mutate(Confidence = percent(Confidence)) %>%
  mutate(Loser_Confidence = percent(Loser_Confidence)) %>%
  rename("Winner Probability" = Confidence, "Loser Probability" = Loser_Confidence)

write_csv(thomas, "C:/Users/thigg/Desktop/Hockey Models/GBM Today Predictions.csv")



