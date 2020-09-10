# Import the data
bank_data <- read.csv(file.choose())  
View(bank_data)

#Lets's remove NA values
bank_data <- na.omit(bank_data)
str(bank_data)

install.packages("caret")
library(caret)

# Let's convert categorical variables 
dmy <- dummyVars(" ~ .", data = bank_data,fullRank = T)
View(dmy)
str(bank_data)

# Applying the logistic model
model <- glm(term.deposit~.,data = bank_data,family = "binomial")
summary(model)
prob <- predict(model,type = c("response"),bank_data)
prob
confusion <- table(prob>0.5,bank_data$term.deposit)
confusion
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy #0.901838

#Roc Curve
install.packages("ROCR")
library(ROCR)
rocpred <- prediction(prob,bank_data$term.deposit)
rocperf <- performance(rocpred,'tpr','fpr')
plot(rocperf)

#####
pred_values <- NULL
yes_no <- NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

bank_data[,"prob"] <- prob
bank_data[,"pred_values"] <- pred_values
bank_data[,"yes_no"] <- yes_no
View(bank_data)

View(bank_data[,c(17,18,19,20)])

# Accuracy 
acc <- table(bank_data$term.deposit,pred_values)
acc
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy#0.901838
