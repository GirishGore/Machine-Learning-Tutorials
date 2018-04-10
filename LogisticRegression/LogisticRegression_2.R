setwd("E:\\EDrive\\R\\Machine Learning with R\\PrepMaterial\\Lecture4");

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
data_train$Credit_History = as.factor(data_train$Credit_History)

data_test <- data_train[data_train$is_train == 0,]
data_train <- data_train[data_train$is_train == 1,]


## Checking for class balance
table(data_train$Loan_Status)

##install.packages("caret");
library("caret");

set.seed(8888);
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Loan_Status ~ Education + Self_Employed + ApplicantIncome
               + CoapplicantIncome + LoanAmount
               + Loan_Amount_Term + Credit_History 
               + Property_Area,
               data=data_train, trControl=train_control, 
               method="glm" ,
               family=binomial())
# summarize results
print(model)

## Look at model summary
summary(model)

summary(data_train$Credit_History)
exp(1.87)

predictTrainSet <- predict(model, data=data_train, type='prob')
summary(predictTrainSet)
tapply(predictTrainSet$Y , data_train$Loan_Status , mean);
tapply(predictTrainSet$Y , data_train$Loan_Status , median);

#### Finding the threshold
table(data_train$Loan_Status,predictTrainSet$Y > 0.5)
(415+84)/614

table(data_train$Loan_Status,predictTrainSet$Y > 0.7)
(355+133)/664

### Higher Threshold : True Positive Rate Decreases ; True Negative Rate Increases

table(data_train$Loan_Status,predictTrainSet$Y > 0.3)
(312+62)/460

##install.packages("ROCR")
library("ROCR")
ROCRPred = prediction(predictTrainSet$Y , data_train$Loan_Status)
ROCRPerf = performance(ROCRPred,"tpr","fpr" )
plot(ROCRPerf)
plot(ROCRPerf , colorize=TRUE , print.cutoffs.at=seq(0,1,0.1))


##exp(cbind(Odd_Ratio = coef(model), confint(model)))

str(data_test);
pred_loan_status = predict(model , newdata =data_test , type='prob');
nrow(pred_loan_status)

final_pred <- ifelse(pred_loan_status$Y > 0.25, 'Y' ,'N') 
final_pred[is.na(final_pred)] <- 'Y'

#loan_ids <- data.frame(lapply(data_test$Loan_ID, as.character), stringsAsFactors=FALSE)
loan_ids <- lapply(data_test$Loan_ID, as.character)
final_loan_ids <- data.frame(matrix(unlist(loan_ids), nrow=367, byrow=T),stringsAsFactors=FALSE)
final_submission <- c(data.frame(final_loan_ids),data.frame(final_pred))

names(final_submission) <- names(read.csv('Sample_Submission.csv',header=T))
soln="lr_10cv3_t0.25_removeVariables_impute";
write.csv(final_submission,file=paste(soln,".csv"),row.names = FALSE);

