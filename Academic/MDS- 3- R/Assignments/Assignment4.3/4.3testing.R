#setting up working directory 
getwd()
setwd("C:/Users/ramom/Desktop/MDS/Academic/1st Semester/MDS- 3- R/Assignments/Assignment4.3")
getwd()

#1. Read the titanic.csv data with base R function and save it as "data" 
#and remove the name column and save again as data

#load data from csv file 
data <- read.csv("titanic.csv")
head(data)

#remove name column by index
data <- data[, -3]
str(data)

#converting pClass and  sex to the factor variable 
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)
str(data)

# Perform one-hot encoding for "Pclass"
encoded_pclass <- model.matrix(~ Pclass - 1, data = data)

# Perform one-hot encoding for "Sex"
encoded_sex <- model.matrix(~ Sex - 1, data = data)

# Combine the encoded columns with the original data
data_encoded <- cbind(data[, -which(names(data) %in% c("Pclass", "Sex"))], encoded_pclass, encoded_sex)

#checking
head(data_encoded)
str(data_encoded)

#2. Fit binary logistic regression model with "Survived" variable as dependent 
#variable and rest of variables as independent variables using "data", 
#get summary of the model, check VIF and interpret the results carefully

#Fit binary logistic regression model
Binary_Logistic_reg <- glm(Survived ~ ., data = data, family = binomial)
summary(Binary_Logistic_reg)  #summary of the model

library(car)
vif(Binary_Logistic_reg)

#intepretation

data <- data_encoded
#str(data)

set.seed(26)

#3. Randomly split the data into 70% and 30% with replacement of 
#samples as "train" and "test" data
RSplit <- sample(2, nrow(data), replace = T, prob = c(0.7,0.3))
data_train <- data[RSplit == 1, ]
data_test <- data[RSplit == 2, ]

#4. Fit binary logistic regression classifier, knn classifier, ann classifier, 
#naive bayes classifier, svm classifier, decision tree classifier, 
#decision tree bagging classifier, random forest classifier, tuned random forest
#classifier and random forest boosting classifier models using the "train" data

#Fit binary logistic regression classifier using train data
blrc_Model <- glm(Survived ~ ., data = data_train, family = binomial)
summary(blrc_Model)

predict <- predict(blrc_Model, type="response")
predcted.blrc_Model <- as.numeric(ifelse(predict>0.5,1,0))
(cm <- table(predcted.blrc_Model, data_train$Survived))
(accuracy <- sum(diag(cm))/sum(cm))
(error <- 1 - accuracy)

#Fit Knn Classifier using train data
library(class)

knn_model <- knn(train = data_train[, -1], 
                 test = data_test[, -1], cl = data_train$Survived, k = 3)
summary(knn_model)

#fit the Ann classifier 
library(neuralnet)
ann_model <- neuralnet(Survived ~ ., data = data_train, hidden = 5)
summary(ann_model)

# Fit the Naive Bayes classifier
library(e1071)
naive_bayes_model <- naiveBayes(Survived ~ ., data = data_train)
summary(naive_bayes_model)

# Support Vector Machine (SVM) classifier
svm_model <- svm(formula = Survived ~ ., data = data_train, 
                 type = 'C-classification',
                 kernel = 'linear')
summary(svm_model)

# Fit the Decision tree classifier
library(party)
tree_model <- ctree(Survived ~ ., data = data_train)
tree_model

# Fit the Decision tree bagging classifier
library(ipred)
bagging_model <- bagging(Survived ~ ., data = data_train, coob = T)
print(bagging_model)

# Fit  Random forest classifier
library(randomForest)
rf_model <- randomForest(as.factor(as.character(data_train$Survived)) ~ ., data = data_train)
print(rf_model)

# Fit Tuned random forest classifier 
tuned_rf_model <- tuneRF(data_train[, -1], as.factor(as.character(data_train$Survived)), ntreeTry = 100)


# Fit Random forest boosting classifier
#install.packages("xgboost")
library(xgboost)
boosting_model <- xgboost(data = as.matrix(data_train[, -1]), 
                          label = data_train$Survived, 
                          objective = "binary:logistic", 
                          nrounds = 100)
# 
# 5. Get confusion matrix and accuracy/misclassification error for all the 
#classifier models and interpret them carefully



# Function to calculate confusion matrix and accuracy
calc_metrics <- function(pred, actual) {
  conf_mat <- table(pred, actual)
  accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
  misclassification_error <- 1 - accuracy
  
  return(list(confusion_matrix = conf_mat, accuracy = accuracy, misclassification_error = misclassification_error))
}

# Get confusion matrix and accuracy for all classifier models

log_reg_metrics <- calc_metrics(predict(blrc_Model, data_test, type = "response") > 0.5, data_test$Survived)
knn_metrics <- calc_metrics(knn_model, data_test$Survived)
ann_metrics <- calc_metrics(predict(ann_model, data_test) > 0.5, data_test$Survived)
naive_bayes_metrics <- calc_metrics(predict(naive_bayes_model, data_test), data_test$Survived)
svm_metrics <- calc_metrics(predict(svm_model, data_test), data_test$Survived)
tree_metrics <- calc_metrics(predict(tree_model, data_test, type = "response"), data_test$Survived)
bagging_metrics <- calc_metrics(predict(bagging_model, data_test), data_test$Survived)
rf_metrics <- calc_metrics(predict(rf_model, data_test), data_test$Survived)
#tuned_rf_metrics <- calc_metrics(predict(tuned_rf_model, data_test[, -1]), as.factor(as.character(data_train$Survived)))
# boosting_metrics <- calc_metrics(predict(boosting_model, data_test, n.trees = 100), data_test$Survived)

log_reg_metrics
knn_metrics
ann_metrics
naive_bayes_metrics
svm_metrics
tree_metrics
bagging_metrics
rf_metrics


# Function to predict on test data and calculate confusion matrix and accuracy
predict_metrics <- function(model, data) {
  pred <- predict(model, newdata = data)
  conf_mat <- table(pred, data$Survived)
  accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
  misclassification_error <- 1 - accuracy
  
  return(list(confusion_matrix = conf_mat, accuracy = accuracy, misclassification_error = misclassification_error))
}

# Get confusion matrix and accuracy for all predicted models
#log_reg_pred_metrics <- predict_metrics(blrc_Model, data_test)
#knn_pred_metrics <- predict_metrics(knn_model, data_test)
#ann_pred_metrics <- predict_metrics(ann_model, data_test)
naive_bayes_pred_metrics <- predict_metrics(naive_bayes_model, data_test)
svm_pred_metrics <- predict_metrics(svm_model, data_test)
tree_pred_metrics <- predict_metrics(tree_model, data_test)
bagging_pred_metrics <- predict_metrics(bagging_model, data_test)
rf_pred_metrics <- predict_metrics(rf_model, data_test)

#log_reg_pred_metrics
#knn_pred_metrics 
#ann_pred_metrics
naive_bayes_pred_metrics
svm_pred_metrics
tree_pred_metrics
bagging_pred_metrics
rf_pred_metrics 



#svm_pred_metrics# predict_BLC <- predict(blrc_Model, type="response")
# predcted.blrc_Model <- as.numeric(ifelse(predict>0.5,1,0))
# (cm_blc <- table(predcted.blrc_Model, data_train$Survived))
# (accuracy_blc <- sum(diag(cm_blc))/sum(cm_blc))
# (error_blc <- 1 - accuracy_blc)
# 
# #for Knn Classification model 
# 
# # Create a confusion matrix for KNN
# confusionMatrix_knn <- confusionMatrix(knn_model, data_train$Survived)
# 
# # Extract accuracy and misclassification error for KNN
# accuracy_knn <- confusionMatrix_knn$overall["Accuracy"]
# misclassification_error_knn <- 1 - accuracy_knn
# 
# 
# # 6. Get confusion matrix and accuracy/misclassification error for all the predicted models and interpret them carefully
# # 7. Compare accuracy and misclassification error of predicted models based on "test" data to decide the "best" model
# # 8. Write a reflection on your own word focusing on "what did I learn from this assignment?"
# 
# 















