#setting up working directory 
getwd()
setwd("C:/Users/ramom/Desktop/MDS/Academic/1st Semester/MDS- 3- R/Assignments/Assignment4.3")
getwd()

#1. Read the titanic.csv data with base R function and save it as "data" 
# and remove the name column and save again as data

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

#2. Fit binary logistic regression model with "Survived" variable as dependent 
#variable and rest of variables as independent variables using "data", 
#get summary of the model, check VIF and interpret the results carefully

Binary_Logistic_reg <- glm(Survived ~ ., data = data, family = binomial)
summary(Binary_Logistic_reg)


library(car)
vif(Binary_Logistic_reg)

#3. Randomly split the data into 70% and 30% with replacement of 
#samples as "train" and "test" data

set.seed(26)

RSplit <- sample(2, nrow(data), replace = T, prob = c(0.7,0.3))
data_train <- data[RSplit == 1, ]
data_test <- data[RSplit == 2, ]


#4. Fit binary logistic regression classifier, knn classifier, ann classifier, 
#naive bayes classifier, svm classifier, decision tree classifier, decision 
#tree bagging classifier, random forest classifier, tuned random forest 
#classifier and random forest boosting classifier models using the "train" data


#Fit binary logistic regression classifier using train data
blrc_Model <- glm(Survived ~ ., data = data_train, family = binomial)
summary(blrc_Model)

predict <- predict(blrc_Model, type="response")
predcted.blrc_Model <- as.numeric(ifelse(predict>0.5,1,0))
(cm <- table(predcted.blrc_Model, data_train$Survived))
(accuracy <- sum(diag(cm))/sum(cm))
(error <- 1 - accuracy)

####################333The above code can be done using caret package as well
library(caret)
predict <- predict(blrc_Model, type="response")
predicted <- factor(ifelse(predict>0.5,1,0))
reference <- factor(data_train$Survived)
confusionMatrix(predicted, reference)

#Fit Knn Classifier using train data
library(class)

knn_model <- knn(train = data_train[, -c(1,2,3)], 
                 test = data_test[, -c(1,2,3)], cl = data_train$Survived, k = 3)

# # Function to calculate confusion matrix and accuracy
# calc_metrics <- function(pred, actual) {
#   conf_mat <- table(pred, actual)
#   accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
#   misclassification_error <- 1 - accuracy
#   
#   return(list(confusion_matrix = conf_mat, accuracy = accuracy, misclassification_error = misclassification_error))
# }
# 
# knn_metrics <- calc_metrics(knn_model, data_test$Survived)
# knn_metrics
# summary(knn_model)
# 
# predict <- predict(knn_model, type="response")
# predicted <- factor(ifelse(predict>0.5,1,0))
# reference <- factor(data_test$Survived)
# confusionMatrix(predicted, reference)


#Fit ANN Classifier using train data
#install.packages("nauralnet")
# Assuming you have loaded the necessary libraries and have the data frame with the "Pclass" and "Sex" columns

str(data_train)

# Perform one-hot encoding for "Pclass"
encoded_pclass <- model.matrix(~ Pclass - 1, data = data_train)

# Perform one-hot encoding for "Sex"
encoded_sex <- model.matrix(~ Sex - 1, data = data_train)

# Combine the encoded columns with the original data
data_encoded <- cbind(data_train[, -which(names(data_train) %in% c("Pclass", "Sex"))], encoded_pclass, encoded_sex)

library(neuralnet)
ann_model <- neuralnet(Survived ~ ., data = data_encoded, hidden = 5)

#Fit naive bayes Classifier using train data

#Fit SVM Classifier using train data

#Fit Decision Tree Classifier using train data

#Fit Tree Bagging Classifier using train data

#Fit Random forest Classifier using train data

#Fit tuned random forest classifier using train data

#Fit random forest boosting classifier using train data

# 5. Get confusion matrix and accuracy/misclassification error for all the classifier models and interpret them carefully
# 6. Get confusion matrix and accuracy/misclassification error for all the predicted models and interpret them carefully
# 7. Compare accuracy and misclassification error of predicted models based on "test" data to decide the "best" model
# 8. Write a reflection on your own word focusing on "what did I learn from this assignment?"











