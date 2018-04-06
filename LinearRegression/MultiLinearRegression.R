setwd("E:\\EDrive\\R\\Machine Learning with R\\PrepMaterial\\Lecture2");
##Loading the data sets
NBA_train <- read.csv("NBA_train.csv");

##Looking at the data
summary(NBA_train)
str(NBA_train)

## Checking the correlation between the independent variables
cor(NBA_train[,c(6:20)]) > 0.7

NBA_train$TotalAttempts = NBA_train$X2PA + NBA_train$X3PA + NBA_train$FTA;

simpleLM = lm(formula = PTS ~ TotalAttempts , data = NBA_train)
summary(simpleLM)
## R2 of 0.775

linearModel = lm(formula = PTS ~ X2P + X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA_train)
summary(linearModel)  
## R2 of 0.9803
## Does including X2P make sense here ?


linearModel1 = lm(formula = PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA_train)
summary(linearModel1)
## R2 of 0.8991 and Adjusted R2 of 0.8982


linearModel2 = lm(formula = PTS ~ X2PA + X3PA + FTA + AST +ORB + STL , data = NBA_train)
summary(linearModel2)
## R2 of 0.8991 and Adjusted R2 of 0.8983
## Adjusted R2 has improved minutely with R2 being the same.



## This R2 is on the data we used .. what about the new data ?
## Predicting Using our models

NBA_test <- read.csv("NBA_test.csv");

summary(NBA_test);
str(NBA_test);

predPoints = predict(linearModel , NBA_test);
predPoints1 = predict(linearModel1 , NBA_test);
predPoints2 = predict(linearModel2 , NBA_test);

plot(predPoints,NBA_test$PTS);
plot(predPoints1,NBA_test$PTS);
plot(predPoints2,NBA_test$PTS);

## R2 can be defined as 1 - (SSE/SST)

SSE = sum((predPoints1-NBA_test$PTS)^2)
SST = sum((mean(NBA_train$PTS) - NBA_test$PTS)^2)
R2.1 = 1 - (SSE/SST)

SSE = sum((predPoints2-NBA_test$PTS)^2)
SST = sum((mean(NBA_train$PTS) - NBA_test$PTS)^2)
R2.2 = 1 - (SSE/SST)

rmse <-function(error)
{
  sqrt(mean(error^2))
}

rmse(NBA_test$PTS - predPoints1);
rmse(NBA_test$PTS - predPoints2);
