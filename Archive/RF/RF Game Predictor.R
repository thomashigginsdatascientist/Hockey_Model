

library(tidyverse)
library(lubridate)
library(scales)
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

location_data <- games %>%
  group_by(Team, Location) %>%
  arrange(desc(Date)) %>%
  slice(1:82) %>%
  summarise(Wins = sum(Win), Avg_GF = mean(GF), Total_GF = sum(GF), num_games = n()) %>%
  mutate(Losses = num_games - Wins) %>%
  mutate(Win_Percentage = Wins/num_games) %>%
  select(Team, Location, Avg_GF, Win_Percentage)

sequence_data1 <- left_join(sequence_data1, location_data, by = c("Team", "Location"))

# games1 <- games %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
#   # filter(Date >= as.Date("01/01/2022", format = "%m/%d/%Y"))
# 
# dates <- unique(games1$Date)
# dates <- as.data.frame(dates)
# dates <- dates %>%
#   filter(dates >= as.Date("01/01/2022", format = "%m/%d/%Y"))
# 
# teams <- unique(games1$Team)
# 
# binder <- games1 %>%
#   select(Team, Date) %>%
#   mutate(hot_score = 0)
# 
# binder <- binder[1,]
# binder <- binder[-1,]
# binder <- binder %>%
#   select(Team, hot_score, Date)
# 
# for(i in 1:nrow(dates)){
# 
#   print(i)
# 
#   current_date <- dates$dates[i]
# 
#   current_date_one <- current_date - days(1)
# 
#   current_date_two_weeks <- current_date - days(15)
# 
#   league_data <- games1 %>%
#     filter(Date <= current_date_one) %>%
#     filter(Date >= current_date_two_weeks) %>%
#     mutate(group = 1) %>%
#     group_by(group) %>%
#     summarise(Avg_Win_Percentage_League = 0.5, Avg_GF_League = mean(GF), Avg_GA_League = mean(GA))
# 
#     team_data <- games1 %>%
#       filter(Date <= current_date_one) %>%
#       filter(Date >= current_date_two_weeks) %>%
#       group_by(Team) %>%
#       mutate(Num_Games = n()) %>%
#       mutate(Num_Wins = sum(Win)) %>%
#       mutate(GF_Team = mean(GF)) %>%
#       mutate(GA_Team = mean(GA)) %>%
#       mutate(Win_Percentage = Num_Wins/Num_Games) %>%
#       ungroup() %>%
#       mutate(Avg_Win_Percentage_League = 0.5) %>%
#       mutate(Avg_GF_League = league_data$Avg_GF_League) %>%
#       mutate(Avg_GA_League = league_data$Avg_GA_League) %>%
#       mutate(variable_1 = (Win_Percentage - Avg_Win_Percentage_League)) %>%
#       mutate(variable_2 = (GF_Team - Avg_GF_League)) %>%
#       mutate(variable_3 = -(GA_Team - Avg_GA_League)) %>%
#       mutate(hot_score = Win_Percentage + variable_1 + variable_2 + variable_3) %>%
#       distinct(Team, .keep_all = TRUE) %>%
#       select(Team, hot_score)
# 
#     team_data$Date <- current_date
# 
#     binder <- rbind(binder, team_data)
# 
# }
# 
# sequence_data1 <- left_join(sequence_data1, binder, by = c("Team", "Date"))


# power_rankings <- read_csv("C:/Users/thigg/Desktop/Hockey Models/ESPN Power Rankings.csv")
# 
# sequence_data1 <- left_join(sequence_data1, power_rankings, by = c("Team"))

sequence_data1$Win <- ifelse(sequence_data1$Win == 1, "W", "L")

sequence_data1 <- sequence_data1 %>%
  select(-Date)

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

train <- train[complete.cases(train),]

test <- test %>%
  ungroup() %>%
  as.data.frame()

test <- test[complete.cases(test),]

test1 <- test %>%
  select(-Win, -GF, -GA)

actuals <- test %>%
  select(Win, GF, GA)

train_rf <- train %>%
  select(Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Win, Avg_GF, Win_Percentage, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7)

# start <- Sys.time()
# 
# rf_model <- train(Win ~ .,
#                   data = train_rf,
#                   method="rf"
#                   )
# 
# end <- Sys.time()
# 
# end - start
# 
# saveRDS(rf_model, "C:/Users/thigg/Desktop/Hockey Models/RF9.RDS")

#R squared of 0.612609

#AUC of 0.601434

#Precision of 0.6097

#Recall of 0.5263

rf_model <- readRDS("C:/Users/thigg/Desktop/Hockey Models/RF8.RDS")

varImp(rf_model)

training <- rf_model$trainingData

preds <- predict(rf_model, newdata = test1, type = "prob")

test1 <- cbind(test1, preds)

test1$Win_Probability <- test1$W

test1$pred_abs <- ifelse(test1$L > test1$W, "L", "W")

test1 <- cbind(test1, actuals)

test1$pred_abs1 <- ifelse(test1$Win == test1$pred_abs, 1, 0)

Calc <- test1

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

Calc <- test1 %>%
  select(Win, W, L) %>%
  mutate(pred = factor(ifelse(W > L, "W", "L"))) %>%
  rename("obs" = Win) %>%
  mutate(obs = as.factor(obs))

prSummary(Calc, lev = levels(Calc$obs))


sum(test1$pred_abs1)/nrow(test1)

confusionMatrix(as.factor(test1$pred_abs), as.factor(test1$Win))

test1$highest <- ifelse(test1$L > test1$Win, test1$L, test1$W)

test2 <- test1 %>%
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
  select(Date, Team, GF, Location, Opponent, GA, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Win)
  
next_week1 <- next_week %>%
  filter(Date == Sys.Date()) %>%
  # filter(Date >= as.Date("12/13/2023", format = "%m/%d/%Y")) %>%
  select(-Win)

next_week1 <- left_join(next_week1, location_data, by = c("Team", "Location"))

today_preds <- predict(rf_model, newdata = next_week1, type = "prob")

next_week1 <- cbind(next_week1, today_preds)

next_week1$Winner <- ifelse(next_week1$L > next_week1$W, next_week1$Opponent, next_week1$Team)

next_week1$Loser <- ifelse(next_week1$L > next_week1$W, next_week1$Team, next_week1$Opponent)


next_week1$Confidence <- ifelse(next_week1$L > next_week1$W, next_week1$L, next_week1$W)

next_week1 <- next_week1 %>%
  select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Avg_GF, Win_Percentage, W, L, Winner, Loser, Confidence)

next_week1$Win_Probability <- next_week1$W

thomas <- next_week1 %>%
  ungroup() %>%
  filter(Location == "Home") %>%
  mutate(Loser_Confidence = 1 - Confidence) %>%
  mutate(Site = paste("Playing At: ", Team, " ", Location)) %>%
  select(Date, Winner,  Confidence, Loser,  Loser_Confidence, Site) %>%
  arrange(desc(Confidence)) %>%
  mutate(Confidence = percent(Confidence)) %>%
  mutate(Loser_Confidence = percent(Loser_Confidence)) %>%
  rename("Winner Probability" = Confidence, "Loser Probability" = Loser_Confidence)

write_csv(thomas, "C:/Users/thigg/Desktop/Hockey Models/RF Today Predictions.csv")


predictions <- read_csv("C:/Users/thigg/Desktop/Hockey Models/RF Predictions.csv")

predictions$Site <- gsub("Playing At: ", "", predictions$Site)
predictions$Site <- gsub(" Home", "", predictions$Site)
predictions$Site <- trimws(predictions$Site)

predictions$UID <- paste(predictions$Site, predictions$Date, sep = "---")


schedule20231 <- schedule2023 %>%
  mutate(UID = paste0(Home, "---", Date)) %>%
  select(UID, OTSO)

predictions <- left_join(predictions, schedule20231, by = "UID")

SOOT <- predictions %>%
  filter(!is.na(OTSO))

predictions <- predictions %>%
  filter(is.na(OTSO))

predictions1 <- predictions %>%
  group_by(Category) %>%
  summarise(Total = n(), Total_Correct = sum(Model)) %>%
  mutate(Percent = Total_Correct/Total)

sum(predictions1$Total_Correct)/sum(predictions1$Total)

test70 <- predictions1[3,2] + predictions1[4,2] + predictions1[5,2] + predictions1[6,2]

test70c <- predictions1[3,3] + predictions1[4,3] + predictions1[5,3] + predictions1[6,3]

test70c/test70

test80 <- predictions1[4,2] + predictions1[5,2] + predictions1[6,2]

test80c <- predictions1[4,3] + predictions1[5,3] + predictions1[6,3]

test80c/test80
