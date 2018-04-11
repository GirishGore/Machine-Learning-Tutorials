setwd('E:\\EDrive\\R\\Machine Learning with R\\PrepMaterial\\Lecture4\\');

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

#install.packages("e1071");
library("e1071");

## Logistic regression model
model <- naiveBayes(Loan_Status ~ Gender + Married + Dependents +
                      Education + Self_Employed + ApplicantIncome
                    + CoapplicantIncome + LoanAmount
                    + Loan_Amount_Term + Credit_History 
                    + Property_Area, 
                    data = data_train,
                    laplace = 3)
## Look at model summary
summary(model)
print(model)
#### Trying to generate same class probabilities via code

## table w.r.t Loan Status
tbl_list <- sapply(data_train[,2:12], table, data_train[ , 13])
### Transposing the list to get Columns back
tbl_list <- lapply(tbl_list, t)

head(tbl_list)
### Conditional probabilites are applied for 
cond_probs <- sapply(tbl_list, 
  function(x) 
  { 
  apply(x, 1, function(x) { x / sum(x) }) 
  })

cond_probs <- lapply(cond_probs, t)

print(cond_probs$Gender)
print(model$tables$Gender)
summary(data_train$Gender)

print(cond_probs$Property_Area)
print(model$tables$Property_Area)

print(model)

## Calculating the accuracy and confusion matrix

preds <- predict(model, newdata = data_train)
table(preds, data_train$Loan_Status)

## Accuracy 
(90 + 400) / nrow(data_train)

##exp(cbind(Odd_Ratio = coef(model), confint(model)))

data_test <- read.csv("test.csv")
summary(data_test);
str(data_test);
pred_loan_status = predict(model , data_test)

table(pred_loan_status)

loan_ids <- lapply(data_test$Loan_ID, as.character)
final_loan_ids <- data.frame(matrix(unlist(loan_ids), nrow=367, byrow=T),stringsAsFactors=FALSE)
final_submission <- c(data.frame(final_loan_ids),data.frame(pred_loan_status))

names(final_submission) <- names(read.csv('Sample_Submission.csv',header=T))
soln="basic_naive_bayes";
write.csv(final_submission,file=paste(soln,".csv"),row.names = FALSE);

