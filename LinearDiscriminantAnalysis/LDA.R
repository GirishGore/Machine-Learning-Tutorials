setwd('E:\\EDrive\\R\\Machine Learning with R\\PrepMaterial\\Lecture3\\');

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

data_test <- data_train[data_train$is_train == 0,]
data_train <- data_train[data_train$is_train == 1,]

## Checking for class balance
table(data_train$Loan_Status)

##install.packages("MASS");
library("MASS");

## Logistic regression model
model <- lda(Loan_Status ~  Gender + Married + Dependents +
               Education + Self_Employed + ApplicantIncome
             + CoapplicantIncome + LoanAmount
             + Loan_Amount_Term + Credit_History 
             + Property_Area, data = data_train)

## Look at model summary
summary(model)

print(model)

summary(data_test);
str(data_test);
pred_loan_status = predict(model , data_test)

summary(pred_loan_status$class)

loan_ids <- lapply(data_test$Loan_ID, as.character)
final_loan_ids <- data.frame(matrix(unlist(loan_ids), nrow=367, byrow=T),stringsAsFactors=FALSE)
final_submission <- c(data.frame(final_loan_ids),data.frame(pred_loan_status$class))

names(final_submission) <- names(read.csv('Sample_Submission.csv',header=T))
soln="lda";
write.csv(final_submission,file=paste(soln,".csv"),row.names = FALSE);

