

library(tidyverse)
library(lubridate)
library(scales)

schedule2021 <- read_csv("C:/Users/thigg/Desktop/Hockey Prediction Models/Seasons/2021.csv")
schedule2022 <- read_csv("C:/Users/thigg/Desktop/Hockey Prediction Models/Seasons/2022.csv")
schedule2023 <- read_csv("C:/Users/thigg/Desktop/Hockey Prediction Models/Seasons/2023.csv")

colnames(schedule2021) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")
colnames(schedule2022) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")
colnames(schedule2023) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")

games <- rbind(schedule2021, schedule2022, schedule2023)

games$Winner <- ifelse(games$`Vistor Score` > games$`Home Score`, "V", "H")

games$Home_Win <- ifelse(games$Winner == "H", 1, 0)
games$Visitor_Win <- ifelse(games$Winner == "V", 1, 0)

homes <- games %>%
  select(Date, Home, `Home Score`, Home_Win, Vistor) %>%
  rename("Team" = Home, "GF" = `Home Score`, "Win" = Home_Win, "Opponent" = Vistor) %>%
  mutate(Location = "Home")



visits <- games %>%
  select(Date, Vistor, `Vistor Score`, Visitor_Win, Home) %>%
  rename("Team" = Vistor, "GF" = `Vistor Score`, "Win" = Visitor_Win, "Opponent" = Home) %>%
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
  mutate(Previous_3_GF = lag(GF, n = 1) + lag(GF, n = 2) + lag(GF, n = 3)) %>%
  mutate(Previous_Result = lag(Win)) %>%
  mutate(Previous_3_Results = lag(Win, n = 1) + lag(Win, n = 2) + lag(Win, n = 3)) %>%
  mutate(Previous_Location = lag(Location)) %>%
  mutate(DOW = wday(Date, week_start = 1)) %>%
  select(Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_3_GF, Previous_Result, Previous_3_Results, Previous_Location, DOW, Win)

sequence_data1 <- sequence_data[complete.cases(sequence_data),]

location_data <- games %>%
  group_by(Team, Location) %>%
  summarise(Wins = sum(Win), Avg_GF = mean(GF), Total_GF = sum(GF), num_games = n()) %>%
  mutate(Losses = num_games - Wins) %>%
  mutate(Win_Percentage = Wins/num_games) %>%
  select(Team, Location, Avg_GF, Win_Percentage)

sequence_data1 <- left_join(sequence_data1, location_data, by = c("Team", "Location"))

sequence_data1$Win <- ifelse(sequence_data1$Win == 1, "W", "L")

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

set.seed(27)
Train_Index <- createDataPartition(sequence_data1$Win, p = .95, 
                                   list = F, 
                                   times = 1)
train <- sequence_data1[ Train_Index,]
test  <- sequence_data1[-Train_Index,]

train <- train %>%
  ungroup() %>%
  as.data.frame()

# model <- lm(Win ~ ., data = train, family="multinomial")

test <- test %>%
  ungroup() %>%
  as.data.frame()

test1 <- test %>%
  select(-Win)

actuals <- test %>%
  select(Win)

# preds <- predict(model, newdata = test1, type = "response")
# 
# test$preds <- preds
# 
# test$pred_abs <- ifelse(test$preds >= 0.5, 1, 0)
# 
# confusionMatrix(as.factor(test$pred_abs), as.factor(test$Win))
# 
# summary(model)

# start <- Sys.time()
# 
# rf_model <- train(Win ~ ., data = train, method="rf")
# 
# end <- Sys.time()
# 
# end - start

saveRDS(rf_model, "C:/Users/thigg/Desktop/Hockey Prediction Models/RF1.RDS")

rf_model <- readRDS("C:/Users/thigg/Desktop/Hockey Prediction Models/RF1.RDS")

# preds <- predict(rf_model, newdata = test1, type = "prob")

# test <- test[,c(1:13)]

# test <- cbind(test, preds)
# 
# test$pred_abs <- ifelse(test$L > test$W, "L", "W")
# 
# test$pred_abs1 <- ifelse(test$Win == test$pred_abs, 1, 0)
# 
# sum(test$pred_abs1)/nrow(test)
# 
# confusionMatrix(as.factor(test$pred_abs), as.factor(test$Win))
# 
# test$highest <- ifelse(test$L > test$Win, test$L, test$W)
# 
# test2 <- test %>%
#   filter(highest >= .85)
# 
# sum(test2$pred_abs1)/nrow(test2)

next_week <- read_csv("C:/Users/thigg/Desktop/Hockey Prediction Models/Next Week Games.csv")

colnames(next_week) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")

next_week$Winner <- ifelse(next_week$`Vistor Score` > next_week$`Home Score`, "V", "H")

next_week$Home_Win <- ifelse(next_week$Winner == "H", 1, 0)
next_week$Visitor_Win <- ifelse(next_week$Winner == "V", 1, 0)

attributes <- games %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Team) %>%
  arrange(desc(Date)) %>%
  slice(1:3) %>%
  arrange(Date) %>%
  ungroup()


homes <- next_week %>%
  select(Date, Home, `Home Score`, Home_Win, Vistor) %>%
  rename("Team" = Home, "GF" = `Home Score`, "Win" = Home_Win, "Opponent" = Vistor) %>%
  mutate(Location = "Home")



visits <- next_week %>%
  select(Date, Vistor, `Vistor Score`, Visitor_Win, Home) %>%
  rename("Team" = Vistor, "GF" = `Vistor Score`, "Win" = Visitor_Win, "Opponent" = Home) %>%
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
  mutate(Previous_GF = lag(GF)) %>%
  mutate(Previous_3_GF = lag(GF, n = 1) + lag(GF, n = 2) + lag(GF, n = 3)) %>%
  mutate(Previous_Result = lag(Win)) %>%
  mutate(Previous_3_Results = lag(Win, n = 1) + lag(Win, n = 2) + lag(Win, n = 3)) %>%
  mutate(Previous_Location = lag(Location)) %>%
  mutate(DOW = wday(Date, week_start = 1)) %>%
  select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_3_GF, Previous_Result, Previous_3_Results, Previous_Location, DOW)

next_week1 <- next_week %>%
  filter(Date >= as.Date("12/9/2023", format = "%m/%d/%Y"))

next_week1 <- next_week1[complete.cases(next_week1),]

next_week1 <- left_join(next_week1, location_data, by = c("Team", "Location"))

today_preds <- predict(rf_model, newdata = next_week1, type = "prob")

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

write_csv(thomas, "C:/Users/thigg/Desktop/Hockey Prediction Models/Today Predictions.csv")
