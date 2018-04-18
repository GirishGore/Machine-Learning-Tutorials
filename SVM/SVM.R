setwd('E:\\EDrive\\R\\Machine Learning with R\\PrepMaterial\\Lecture5\\');

## Reading the training data set
data_train = read.csv("train.csv");
data_train$is_train = 1;
data_test <- read.csv("test.csv")
data_test$is_train = 0 ;
data_test$Loan_Status = 'N';
data_train <- rbind(data_train , data_test)

table(data_train$is_train)
## General data checks
summary(data_train);
str(data_train)

summary(data_train$LoanAmount)
data_train$LoanAmount[is.na(data_train$LoanAmount)] = mean(data_train$LoanAmount, na.rm=TRUE)

summary(data_train$Loan_Amount_Term)
data_train$Loan_Amount_Term[is.na(data_train$Loan_Amount_Term)] = mean(data_train$Loan_Amount_Term, na.rm=TRUE)

table(data_train$Credit_History)
data_train$Credit_History[is.na(data_train$Credit_History)] = median(data_train$Credit_History, na.rm=TRUE)

str(data_train)
sum(is.na(data_train))


data_test <- data_train[data_train$is_train == 0,]
data_train <- data_train[data_train$is_train == 1,]

## Checking for class balance
table(data_train$Loan_Status)

library(caret)

### SVM algorithm
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
###  with C you can adjust how hard or soft your large margin classification should be
grid <- expand.grid(C = c(0.00001,0.0001,0.001,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

svmModel <- train(Loan_Status ~ Education + Self_Employed
                + ApplicantIncome
                + CoapplicantIncome + LoanAmount
                + Loan_Amount_Term + Credit_History 
                + Property_Area, data = data_train, method = "svmLinear", 
                trControl = ctrl, 
##                tuneGrid = grid,
                preProcess = c("center","scale"),
                tuneLength = 20)
svmModel


svmRadialModel <- train(Loan_Status ~ Education + Self_Employed
                  + ApplicantIncome
                  + CoapplicantIncome + LoanAmount
                  + Loan_Amount_Term + Credit_History 
                  + Property_Area, data = data_train, method = "svmRadial", 
                  trControl = ctrl, 
                  ##                tuneGrid = grid,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
svmRadialModel
## Technically speaking, large gamma leads to high bias and low variance models, and vice-versa.

#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(svmModel)
## Look at model summary
pred <- predict(svmModel , newdata= data_test)

summary(pred)

##pred <- ifelse(model == 1 , 'N' , 'Y')

loan_ids <- lapply(data_test$Loan_ID, as.character)
final_loan_ids <- data.frame(matrix(unlist(loan_ids), nrow=367, byrow=T),stringsAsFactors=FALSE)
final_submission <- c(data.frame(final_loan_ids),data.frame(pred))

names(final_submission) <- names(read.csv('Sample_Submission.csv',header=T))
soln="svmRadial";
write.csv(final_submission,file=paste(soln,".csv"),row.names = FALSE);

