

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

  current_date_one <- current_date

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

# sequence_data1$Win <- as.factor(sequence_data1$Win)

library(caret)

set.seed(31)
Train_Index <- createDataPartition(sequence_data1$Win, p = .95, 
                                   list = F, 
                                   times = 1)
train <- sequence_data1[ Train_Index,]
test  <- sequence_data1[-Train_Index,]

train <- train %>%
  select(-GF, -GA)

train <- train[complete.cases(train),]

test <- test[complete.cases(test),]

actuals <- test %>%
  select(Win)

test <- test %>%
  select(-GF, -GA, -Win)

  
start <- Sys.time()

model <- train(Win ~ .,
                  data = train,
                  method="pda",
               preProcess = "scale"
                  )

end <- Sys.time()

end - start

preds <- predict(model, test, type = "prob")

test <- cbind(test, preds)

test$pred <- ifelse(test$L > test$W, "L", "W")

test <- cbind(test, actuals)

test$pred_acc <- ifelse(test$pred == test$Win, 1, 0)

sum(test$pred_acc)/nrow(test)

"varImp.train" <- function(object, useModel = TRUE, nonpara = TRUE, scale = TRUE, ...) {
  code <- object$modelInfo
  if(is.null(code$varImp)) useModel <- FALSE
  if(useModel) {
    checkInstall(code$library)
    for(i in seq(along = code$library))
      do.call("requireNamespaceQuietStop", list(package = code$library[i]))
    imp <- code$varImp(object$finalModel, ...)
    modelName <- object$method
  } else {
    if(inherits(object, "train.recipe")) {
      x_dat <- recipes::juice(object$recipe, all_predictors())
      x_dat <- as.data.frame(x_dat, stringsAsFactors = FALSE)
      y_dat <- recipes::juice(object$recipe, all_outcomes())
      y_dat <- getElement(y_dat, names(y_dat))
    } else {
      isX <- which(!(colnames(object$trainingData) %in% ".outcome"))
      x_dat <- object$trainingData[, isX,drop = FALSE]
      y_dat <- object$trainingData[, -isX]
      y_dat$.outcome <- ifelse(y_dat$.outcome == "W", 1, 0)
    }
    imp <- filterVarImp(x_dat, y_dat,
                        nonpara = nonpara,
                        ...)
    modelName <- ifelse(is.factor(y_dat),
                        "ROC curve",
                        ifelse(nonpara, "loess r-squared", "Linear model"))
  }
  if(scale) {
    if(class(object$finalModel)[1] == "pamrtrained") imp <- abs(imp)
    imp <- imp - min(imp, na.rm = TRUE)
    imp <- imp/max(imp, na.rm = TRUE)*100
  }
  out <- list(importance = imp,
              model = modelName,
              calledFrom = "varImp")
  
  structure(out, class = "varImp.train")
}


asNumeric <- function(data){
  fc <- sapply(data, is.factor)
  modifyList(data, lapply(data[, fc], as.numeric))
}

rocPerCol <- function(dat, cls){
  roc_auc <- auc(cls, dat)
  max(roc_auc, 1 - roc_auc)
}

filterVarImp <- function(x, y, nonpara = nonpara, ...){
  # converting factors to numeric
  notNumber <- sapply(x, function(x) !is.numeric(x))
  x = asNumeric(x)
  
  if(is.factor(y)){
    classLevels <- levels(y)
    k <- length(classLevels)
    
    if(k > 2){
      
      Combs <- combn(classLevels, 2)
      CombsN <- combn(1:k, 2)
      
      lStat <- lapply(1:ncol(Combs), FUN = function(cc){
        yLevs <- as.character(y) %in% Combs[,cc]
        tmpX <- x[yLevs,]
        tmpY <- as.numeric(y[yLevs] == Combs[,cc][2])
        apply(tmpX, 2, rocPerCol, cls = tmpY)
      })
      Stat = do.call("cbind", lStat)
      
      loutStat <- lapply(1:k, function(j){
        apply(Stat[,CombsN[,j]], 1, max)
      })
      
      outStat = do.call("cbind", loutStat)
      
    } else {
      tmp <- apply(x, 2, rocPerCol, cls = y)
      outStat <- cbind(tmp, tmp)
    }
    
    outStat <- as.data.frame(outStat, stringsAsFactors = FALSE)
    colnames(outStat) <- classLevels
    rownames(outStat) <- dimnames(x)[[2]]
    outStat <- data.frame(outStat)
  } else {
    
    paraFoo <- function(data, y) abs(coef(summary(lm(y ~ data, na.action = na.omit)))[2, "t value"])
    nonparaFoo <- function(x, y, ...)
    {
      meanMod <- sum((y - mean(y, rm.na = TRUE))^2)
      nzv <- nearZeroVar(x, saveMetrics = TRUE)
      
      if(nzv$zeroVar) return(NA)
      if(nzv$percentUnique < 20)
      {
        regMod <- lm(y~x, na.action = na.omit, ...)
      } else {
        regMod <- try(loess(y~x, na.action = na.omit, ...), silent = TRUE)
        
        if(inherits(regMod, "try-error") | any(is.nan(regMod$residuals))) try(regMod <- lm(y~x, ...))
        if(inherits(regMod, "try-error")) return(NA)
      }
      
      pR2 <- 1 - (sum(resid(regMod)^2)/meanMod)
      if(pR2 < 0) pR2 <- 0
      pR2
    }
    
    testFunc <- if(nonpara) nonparaFoo else paraFoo
    
    y_unlist = unlist(y)
    outStat <- apply(x, 2, testFunc, y = y_unlist)
    outStat <- data.frame(Overall = outStat)
  }
  outStat
}

varImp.train(model)



