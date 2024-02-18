

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


# schedule <- read_csv("C:/Users/thigg/Desktop/Hockey Models/Seasons/2014-2020.csv")
# 
# colnames(schedule) <- c("Date", "Vistor", "Vistor Score", "Home", "Home Score", "OTSO", "ATT", "LOG", "Notes")
# 
# games <- schedule

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

vegas_dates <- seq(as.Date("10/7/2017", format = "%m/%d/%Y"), as.Date("11/18/2017", format = "%m/%d/%Y"), by = 1)

dates <- unique(games$Date)
dates <- as.data.frame(dates)
colnames(dates) <- "Date"
dates$Date <- as.Date(dates$Date, format = "%m/%d/%Y")
filter_dates <- dates
filter_dates <- filter_dates %>%
  filter(Date >= as.Date("10/15/2023", format = "%m/%d/%Y")) %>%
  filter(!Date%in% vegas_dates) #removed because vegas enters the league in 2017, caused error in testing pipe


games1 <- games %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
# filter(Date >= as.Date("01/01/2022", format = "%m/%d/%Y"))

dates <- unique(games1$Date)
dates <- as.data.frame(dates)
# dates <- dates %>%
#   filter(dates >= as.Date("01/04/2015", format = "%m/%d/%Y"))

teams <- unique(games1$Team)

binder <- games1 %>%
  select(Team, Date) %>%
  mutate(hot_score = 0)

binder <- binder[1,]
binder <- binder[-1,]
binder <- binder %>%
  select(Team, hot_score, Date)

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
    mutate(hot_score = Win_Percentage + variable_1 + variable_2 + variable_3) %>%
    distinct(Team, .keep_all = TRUE) %>%
    select(Team, hot_score)
  
  team_data$Date <- current_date
  
  binder <- rbind(binder, team_data)
  
}


# train_dates <- data.frame(Date = c("12/9/2023", "12/17/2023", "12/24/2023", "12/31/2023", "1/7/2024", "1/14/2024"))
# train_dates$Date <- as.Date(train_dates$Date, format = "%m/%d/%Y")

library(caret)
library(lubridate)

results <- data.frame(Date = as.Date("1/1/1900", format = "%m/%d/%Y"), Winner1 = "Thomas", WinnerP = 100, Loser1 = "Thomas", LoserP = 100, Site = "Thomas")
colnames(results)[3] <- "Winner Probability"
colnames(results)[5] <- "Loser Probability"
results <- results[-1,]

start <- Sys.time()

for(i in 1:nrow(filter_dates)){
  
  print(i)
  
  current_date <- filter_dates[i,]
  current_dow <- lubridate::wday(current_date)
  
  sequence_data <- games %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    filter(Date <= current_date) %>%
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
    select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, Win)
  
  
  sequence_data1 <- sequence_data[complete.cases(sequence_data),]
  
  sequence_data1 <- sequence_data1 %>%
    filter(Date < current_date)
  
  sequence_data1 <- left_join(sequence_data1, binder, by = c("Team", "Date"))
  
  sequence_data1$Win <- ifelse(sequence_data1$Win == 1, "W", "L")
  
  sequence_data1 <- sequence_data1 %>%
    select(-Date) %>%
    ungroup() %>%
    as.data.frame()
  
  sequence_data1 <- sequence_data1[complete.cases(sequence_data1),]
  
  if(current_dow == 1){
    
    print("Training New Model")
    
    start1 <- Sys.time()

  rf_model <- train(Win ~ .,
                    data = sequence_data1,
                    method="bagFDA"
  )
  
  end1 <- Sys.time()
  
  end1 - start1
  
  }else{
    print(current_date)
  }
  
  prediction_data <- sequence_data %>%
    filter(Date == current_date) %>%
    select(-Win) %>%
    ungroup() %>%
    as.data.frame()
  
  prediction_data <- left_join(prediction_data, binder, by = c("Team", "Date"))
  
  prediction_data <- prediction_data[complete.cases(prediction_data),]
  
  today_preds <- predict(rf_model, newdata = prediction_data, type = "prob")
  
  prediction_data <- cbind(prediction_data, today_preds)
  
  prediction_data$Winner <- ifelse(prediction_data$L > prediction_data$W, prediction_data$Opponent, prediction_data$Team)
  
  prediction_data$Loser <- ifelse(prediction_data$L > prediction_data$W, prediction_data$Team, prediction_data$Opponent)
  
  
  prediction_data$Confidence <- ifelse(prediction_data$L > prediction_data$W, prediction_data$L, prediction_data$W)
  
  prediction_data <- prediction_data %>%
    select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, W, L, Winner, Loser, Confidence)
  
  prediction_data$Win_Probability <- prediction_data$W
  
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
    select(Date, Winner1,  Confidence, Loser1,  Loser_Confidence, Site) %>%
    arrange(desc(Confidence)) %>%
    # mutate(Confidence = percent(Confidence)) %>%
    # mutate(Loser_Confidence = percent(Loser_Confidence)) %>%
    rename("Winner Probability" = Confidence, "Loser Probability" = Loser_Confidence)
  
  
  results <- rbind(results, thomas)
  
}

end <- Sys.time()

end - start

results <- results %>%
  mutate(Category = case_when(`Winner Probability` > .85 ~ "85 <",
                              `Winner Probability` > .8 & `Winner Probability` <= .85 ~ "80-85",
                              `Winner Probability` > .7 & `Winner Probability` <= .8 ~ "70-80",
                              `Winner Probability` > .6 & `Winner Probability` <= .7 ~ "60-70",
                              `Winner Probability` > .55 & `Winner Probability` <= .6 ~ "55-60",
                              `Winner Probability` <= .55 ~ "55 >",
                              TRUE ~ "Thomas")) %>%
  mutate(Team = Winner1)


actuals <- games1 %>%
  mutate(Winner_Actual = ifelse(GF > GA, Team, Opponent)) %>%
  select(Date, Team, Winner_Actual)

results <- left_join(results, actuals, by = c("Date", "Team"))

results <- results %>%
  mutate(Model = ifelse(Winner_Actual == Winner1, 1, 0)) %>%
  select(-Team)

results_summary <- results %>%
  group_by(Category) %>%
  summarise(Total = n(), Correct = sum(Model))

test <- results_summary[1,2] + results_summary[2,2] + results_summary[3,2] + results_summary[4,2] + results_summary[5,2] + results_summary[6,2]
testc <- results_summary[1,3] + results_summary[2,3] + results_summary[3,3] + results_summary[4,3] + results_summary[5,3] + results_summary[6,3]
acc <- testc/test

print(paste0("Total Data Set: ", test, " predictions; ", testc, " correct; ", percent(acc$Correct), " accuracy"))


test <- results_summary[1,2] + results_summary[2,2] + results_summary[3,2] 
testc <- results_summary[1,3] + results_summary[2,3] + results_summary[3,3] 
acc <- testc/test

print(paste0("Below 70% Prob: ", test, " predictions; ", testc, " correct; ", percent(acc$Correct), " accuracy"))

test <- results_summary[4,2] + results_summary[5,2] + results_summary[6,2]
testc <- results_summary[4,3] + results_summary[5,3] + results_summary[6,3]
acc <- testc/test

print(paste0("Above 70% Prob: ", test, " predictions; ", testc, " correct; ", percent(acc$Correct), " accuracy"))

test <- results_summary[5,2] + results_summary[6,2]
testc <- results_summary[5,3] + results_summary[6,3]
acc <- testc/test

print(paste0("Above 80% Prob: ", test, " predictions; ", testc, " correct; ", percent(acc$Correct), " accuracy"))

results$year <- year(results$Date)

write.csv(results, "C:/Users/thigg/Desktop/Hockey Models/2014 To 2020 Back Testing PDA.csv", row.names = FALSE)

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
  select(-Win, -GF, -GA)

binder1 <- binder %>%
  arrange(desc(Date)) %>%
  group_by(Team) %>%
  slice(1) %>%
  select(Team, hot_score) %>%
  ungroup() %>%
  filter(Team %in% next_week1$Team)

next_week1 <- left_join(next_week1, binder1, by = "Team")

next_week1 <- next_week1[complete.cases(next_week1),]

# next_week1 <- left_join(next_week1, location_data, by = c("Team", "Location"))

today_preds <- predict(rf_model, newdata = next_week1, type = "prob")

next_week1 <- cbind(next_week1, today_preds)

next_week1$Winner <- ifelse(next_week1$L > next_week1$W, next_week1$Opponent, next_week1$Team)

next_week1$Loser <- ifelse(next_week1$L > next_week1$W, next_week1$Team, next_week1$Opponent)


next_week1$Confidence <- ifelse(next_week1$L > next_week1$W, next_week1$L, next_week1$W)

next_week1 <- next_week1 %>%
  select(Date, Team, Location, Opponent, Previous_Opponent, Previous_GF, Previous_GA, Previous_Result, Previous_3_GF, Previous_3_GA, Previous_3_Results, Previous_7_GF, Previous_7_GA, Previous_7_Results, Previous_15_GF, Previous_15_GA, Previous_15_Resuts, Previous_Location, DOW, Games_Since_Last_Game, Games_Between_Last_3, Games_Between_Last_7, W, L, Winner, Loser, Confidence)

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
  select(Date, Winner1,  Confidence, Loser1,  Loser_Confidence, Site) %>%
  arrange(desc(Confidence)) %>%
  rename("Winner Probability" = Confidence, "Loser Probability" = Loser_Confidence) %>%
  mutate(Category = case_when(`Winner Probability` > .85 ~ "85 <",
                              `Winner Probability` > .8 & `Winner Probability` <= .85 ~ "80-85",
                              `Winner Probability` > .7 & `Winner Probability` <= .8 ~ "70-80",
                              `Winner Probability` > .6 & `Winner Probability` <= .7 ~ "60-70",
                              `Winner Probability` > .55 & `Winner Probability` <= .6 ~ "55-60",
                              `Winner Probability` <= .55 ~ "55 >",
                              TRUE ~ "Thomas")) %>%
  mutate(`Winner Probability` = percent(`Winner Probability`)) %>%
  mutate(`Loser Probability` = percent(`Loser Probability`))

write_csv(thomas, "C:/Users/thigg/Desktop/Hockey Models/Back Testing Predictions PDA.csv")

saveRDS(rf_model, "C:/Users/thigg/Desktop/Hockey Models/PDA1.RDS")
