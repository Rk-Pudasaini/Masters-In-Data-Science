#setting up working directory 
getwd()
setwd("C:/Users/ramom/Desktop/MDS/Academic/1st Semester/MDS- 3- R/Assignments/Assignment4.3")
getwd()

#load data from csv file 
data <- read.csv("titanic.csv")
head(data)

#remove name column by index
data <- data[, -3]
str(data)

#2. Fit binary logistic regression model with "Survived" variable as dependent 
#variable and rest of variables as independent variables using "data", 
#get summary of the model, check VIF and interpret the results carefully

#Fit binary logistic regression model
Binary_Logistic_reg <- glm(Survived ~ ., data = data, family = binomial)
summary(Binary_Logistic_reg)  #summary of the model

# # Generate predictions on the training data
# predicted <- predict(Binary_Logistic_reg, newdata = data, type = "response")
# predicted <- ifelse(predicted >= 0.5, 1, 0)
# 
# # Compute confusion matrix on the training data
# confusion_matrix_train <- table(data$Survived, predicted)
# print(confusion_matrix_train)
# 
# # Compute accuracy on the training data
#accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
#misclassification_error_train <- 1 - accuracy_train
#print(paste("Accuracy (Training):", accuracy_train))
#print(paste("Misclassification Error (Training):", misclassification_error_train))

library(car)
vif(Binary_Logistic_reg)


#3. Randomly split the data into 70% and 30% with replacement of 
#samples as "train" and "test" data

#converting pClass and  sex to the factor variable 
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)

# Perform one-hot encoding for "Pclass"
encoded_pclass <- model.matrix(~ Pclass - 1, data = data)

# Perform one-hot encoding for "Sex"
encoded_sex <- model.matrix(~ Sex - 1, data = data)

# Combine the encoded columns with the original data
data_encoded <- cbind(data[, -which(names(data) %in% c("Pclass", "Sex"))], encoded_pclass, encoded_sex)

#checking
head(data_encoded)
str(data_encoded)

set.seed(26)
#split the data set
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

# Generate predictions on the training data with classification model 
predicted_blrc_training <- predict(blrc_Model, newdata = data_train, type = "response")
predicted_blrc_model <- ifelse(predicted_blrc_training>= 0.5, 1, 0)

#create confusion matrix, calculate accuracy and error
(cm_blrc <- table(predicted_blrc_model, data_train$Survived))
(accuracy_blrc_training <- sum(diag(cm_blrc))/sum(cm_blrc))
(error_blrc_training <- 1 - accuracy_blrc_training)

# Generate predictions on the test data
predicted_blrc_testing <- predict(blrc_Model, newdata = data_test, type = "response")
predicted_blrc_test_model <- ifelse(predicted_blrc_testing>= 0.5, 1, 0)

#create confusion matrix, calculate accuracy and error
(cm_blrc_test <- table(predicted_blrc_test_model, data_test$Survived))
(accuracy_blrc_testing <- sum(diag(cm_blrc_test))/sum(cm_blrc_test))
(error_blrc_testing <- 1 - accuracy_blrc_testing)

#FITTING THE KNN CLASSIFIER 
library(class)
data <- data_encoded #knn use all the encoded data where we 
#split factors to individual variables 

set.seed(26)
#split the data set with encoded data
RSplit <- sample(2, nrow(data), replace = T, prob = c(0.7,0.3))
data_train <- data[RSplit == 1, ]
data_test <- data[RSplit == 2, ]

# Scale the features
train_data_scaled <- as.data.frame(scale(data_train[, -1]))
test_data_scaled <- as.data.frame(scale(data_test[, -1]))

#fit the knn classifier 
knn_model <- knn(train = train_data_scaled, 
                 test = test_data_scaled, data_train$Survived, k = 3)
#get the summary 
summary(knn_model)

# Compute confusion matrix and accuracy for the classifier model
(knn_model_cm <- table(knn_model, data_test$Survived))
(accuracy_knn <- sum(diag(knn_model_cm)) / sum(knn_model_cm))
(misclassification_error_knn <- 1 - accuracy_knn)


# Generate predictions on the training data
knn_predicted_train_model <- knn(scale(data_train[, -1]), 
                                 scale(data_train[, -1]), 
                                 data_train$Survived, k = 3)

# Compute confusion matrix and accuracy for training data
(knn_predicted_train_cm <- table(knn_predicted_train_model,data_train$Survived))
(accuracy_knn_training <- sum(diag(knn_predicted_train_cm) / 
                                sum(knn_predicted_train_cm)))
(misclassification_error_knn_training <- 1 - accuracy_knn_training)


# Generate predictions on the test data
knn_predicted_test_model <- knn(scale(data_train[, -1]), scale(data_test[, -1]), 
                                data_train$Survived, k = 3)

# Compute confusion matrix and accuracy for training data
(knn_predicted_test_cm <- table(knn_predicted_test_model, data_test$Survived))
(accuracy_knn_testing <- sum(diag(knn_predicted_test_cm) / 
                               sum(knn_predicted_test_cm)))
(misclassification_error_knn_testing <- 1 - accuracy_knn_testing)


#FITTING THE ANN MODEL CLASSIFIER
library(neuralnet)

# Fit the ANN classifier model
ann_model <- neuralnet(Survived ~ ., data = data_train, hidden = 5, 
                       linear.output = FALSE)
summary(ann_model)

# Generate predictions on the training data
ann_predicted_training <- compute(ann_model, data_train[, -1])$net.result
ann_predicted_training <- ifelse(ann_predicted_training >= 0.5, 1, 0)

# Compute confusion matrix and accuracy for training data
(ann_confusion_matrix_training <- table(ann_predicted_training, 
                                        data_train$Survived))
(ann_accuracy_training <- sum(diag(ann_confusion_matrix_training )) / 
    sum(ann_confusion_matrix_training))
(ann_misclassification_error_training <- 1 - ann_accuracy_training)

# Generate predictions on the testing data
ann_predicted_testing <- compute(ann_model, data_test[, -1])$net.result
ann_predicted_testing <- ifelse(ann_predicted_testing >= 0.5, 1, 0)

# Compute confusion matrix and accuracy for testing data
(ann_confusion_matrix_testing <- table(ann_predicted_testing, 
                                       data_test$Survived))
(ann_accuracy_testing <- sum(diag(ann_confusion_matrix_testing )) / 
    sum(ann_confusion_matrix_testing))
(ann_misclassification_error_testing <- 1 - ann_accuracy_testing)

#FITTING THE NAIVE BAYES CLASSIFIER 
library(e1071)
# Fit the Naive Bayes classifier
naive_bayes_model <- naiveBayes(Survived ~ ., data = data_train)
summary(naive_bayes_model)

# Make predictions on the training data
nb_predicted_train <- predict(naive_bayes_model, data_train[, -1])

# Compute confusion matrix and accuracy for training data
(nb_confusion_matrix_training <- table(nb_predicted_train, 
                                       data_train$Survived))
(nb_accuracy_train <- sum(diag(nb_confusion_matrix_training)) / 
    sum(nb_confusion_matrix_training))
(nb_misclassification_error_train <- 1 - nb_accuracy_train)


# Make predictions on the test data
nb_predicted_test <- predict(naive_bayes_model, data_test[, -1])

# Compute confusion matrix and accuracy for testing data
(nb_confusion_matrix_testing <- table(nb_predicted_test, data_test$Survived))
(nb_accuracy_test <- sum(diag(nb_confusion_matrix_testing)) / 
    sum(nb_confusion_matrix_testing))
(nb_misclassification_error_test <- 1 - nb_accuracy_test)


#FITTING THE SUPPORT VECTOR MACHINE 
# Support Vector Machine (SVM) classifier
svm_model <- svm(formula = Survived ~ ., data = data_train, 
                 type = 'C-classification',
                 kernel = 'radial')  #use euclidean distance to calculate 
#similarities between points
#summary of the model
summary(svm_model)

# Make predictions on the training data
svm_predicted_train <- predict(svm_model, data_train[, -1])

# Compute confusion matrix and accuracy for training data
(svm_confusion_matrix_training <- table(svm_predicted_train, 
                                        data_train$Survived))
(svm_accuracy_train <- sum(diag(svm_confusion_matrix_training)) / 
    sum(svm_confusion_matrix_training))
(svm_misclassification_error_train <- 1 - svm_accuracy_train)

# Make predictions on the test data
svm_predicted_test <- predict(svm_model, data_test[, -1])

# Compute confusion matrix and accuracy for testing data
(svm_confusion_matrix_testing <- table(svm_predicted_test, data_test$Survived))
(svm_accuracy_test <- sum(diag(svm_confusion_matrix_testing)) / 
    sum(svm_confusion_matrix_testing))
(svm_misclassification_error_test <- 1 - svm_accuracy_test)

#FITTING DECISION TREE CLASSIFIER
# loading the required library 
library(party)
#library(rpart)

data_train$Survived <- as.factor(data_train$Survived)
data_test$Survived <- as.factor(data_test$Survived)

# Fit the Decision Tree classifier
# dt_model <- rpart(Survived ~ ., data = data_train, method = "class")
# dt_model
dt_tree_model <- ctree(Survived ~ ., data = data_train)
dt_tree_model

# Visualize the decision tree
plot(dt_tree_model)

# Make predictions on the training data
dt_predicted_train <- predict(dt_tree_model, newdata = data_train)

# Compute confusion matrix and accuracy for training data
(dt_confusion_matrix_train <- table(dt_predicted_train, data_train$Survived))
(dt_accuracy_train <- sum(diag(dt_confusion_matrix_train)) / 
    sum(dt_confusion_matrix_train))
(dt_misclassification_error_train <- 1 - dt_accuracy_train)

# Make predictions on the testing data
dt_predicted_test <- predict(dt_tree_model, newdata = data_test)

# Compute confusion matrix and accuracy for testing data
(dt_confusion_matrix_test <- table(dt_predicted_test, data_test$Survived))
(dt_accuracy_test <- sum(diag(dt_confusion_matrix_test)) / 
    sum(dt_confusion_matrix_test))
(dt_misclassification_error_test <- 1 - dt_accuracy_test)


#FITTING THE DECiSION TREE BAGGING CLASSIFIER
library(ipred)
dt_bagging_model <- bagging(Survived ~ ., data = data_train, coob = T)
print(dt_bagging_model)

# Make predictions on the training data
dt_bagging_predicted_train <- predict(dt_bagging_model, newdata = data_train)

# Compute confusion matrix and accuracy for training data
(dt_bag_confusion_matrix_train <- table(dt_bagging_predicted_train, 
                                        data_train$Survived))
(dt_bag_accuracy_train <- sum(diag(dt_bag_confusion_matrix_train)) / 
    sum(dt_bag_confusion_matrix_train))
(dt_bag_misclassification_error_train <- 1 - dt_bag_accuracy_train)

# Make predictions on the testing data
dt_bagging_predicted_test <- predict(dt_bagging_model, newdata = data_test)

# Compute confusion matrix and accuracy for testing data
(dt_bag_confusion_matrix_test <- table(dt_bagging_predicted_test, 
                                       data_test$Survived))
(dt_bag_accuracy_test <- sum(diag(dt_bag_confusion_matrix_test)) / 
    sum(dt_bag_confusion_matrix_test))
(dt_bag_misclassification_error_test <- 1 - dt_bag_accuracy_test)

#FITTING RANDOM FOREST CLASSIFIER
library(randomForest)
rf_model <- randomForest(Survived ~ ., data = data_train, ntree = 100)
rf_model

# Make predictions on the training data
rf_predicted_train <- predict(rf_model, newdata = data_train)

# Compute confusion matrix and accuracy for training data
(rf_confusion_matrix_train <- table(rf_predicted_train, data_train$Survived))
(rf_accuracy_train <- sum(diag(rf_confusion_matrix_train)) / 
    sum(rf_confusion_matrix_train))
(rf_misclassification_error_train <- 1 - rf_accuracy_train)

# Make predictions on the testing data
rf_predicted_test <- predict(rf_model, newdata = data_test)

# Compute confusion matrix and accuracy for testing data
(rf_confusion_matrix_test <- table(rf_predicted_test, data_test$Survived))
(rf_accuracy_test <- sum(diag(rf_confusion_matrix_test)) / 
    sum(rf_confusion_matrix_test))
(rf_misclassification_error_test <- 1 - rf_accuracy_test)

#FITTING TUNED RANDOM FOREST CLASSIFIER
# Tune the number of variables using tuneRF first before fitting itinto model
mtry_tuned <- tuneRF(
  x = data_train[, -1],  # Exclude the target variable
  y = data_train$Survived,
  ntree = 300,
  stepFactor = 0.5,
  improve = 0.05,
  plot = TRUE
)

# Fit the Random Forest classifier with tuned parameters
trf_model <- randomForest(
  Survived ~ .,
  data = data_train,
  mtry = mtry_tuned,
  ntree = 300
)

# Make predictions on the training data
trf_predicted_train <- predict(trf_model, newdata = data_train)

# Compute confusion matrix and accuracy for training data
(trf_confusion_matrix_train <- table(trf_predicted_train, data_train$Survived))
(trf_accuracy_train <- sum(diag(trf_confusion_matrix_train)) / 
    sum(trf_confusion_matrix_train))
(trf_misclassification_error_train <- 1 - trf_accuracy_train)

# Make predictions on the test data
trf_predicted_test <- predict(trf_model, newdata = data_test)

# Compute confusion matrix and accuracy for testing data
(trf_confusion_matrix_test <- table(trf_predicted_test, data_test$Survived))
(trf_accuracy_test <- sum(diag(trf_confusion_matrix_test)) / 
    sum(trf_confusion_matrix_test))
(trf_misclassification_error_test <- 1 - trf_accuracy_test)

#FITTING RANDOM FOREST BOOSTING CLASSIFIER 
library(xgboost)
library(caret)

# Define the training control parameters
control <- trainControl(
  method = "cv",  # Cross-validation resampling method
  number = 10,  # Number of cross-validation folds
  verboseIter = TRUE,  # Print verbose output
  allowParallel = TRUE  # Enable parallel processing if available
)

# Define the parameter grid for tuning
param_grid <- expand.grid(
  nrounds = c(50, 100, 150),  # Number of boosting iterations
  max_depth = c(3, 4, 5),  # Maximum tree depth
  eta = c(0.1, 0.2, 0.3),  # Learning rate
  gamma = 0,  # Minimum loss reduction required to make a further partition on a leaf node
  colsample_bytree = 1,  # Subsample ratio of columns when constructing each tree
  min_child_weight = 1,  # Minimum sum of instance weight (hessian) needed in a child
  subsample = 1  # Subsample ratio of the training instances
)

#Fit the boosting of random forest model using cross-validation&parameter tuning
boosting_rf_model <- train(
  Survived ~ .,
  data = data_train,
  method = "xgbTree",
  trControl = control,
  tuneGrid = param_grid
)

# Make predictions on the training data
xgb_predicted_train <- predict(boosting_rf_model, newdata = data_train)

# Compute confusion matrix and accuracy for training data
(xgb_confusion_matrix_train <- table(xgb_predicted_train, data_train$Survived))
(xgb_accuracy_train <- sum(diag(xgb_confusion_matrix_train)) / 
    sum(xgb_confusion_matrix_train))
(xgb_misclassification_error_train <- 1 - xgb_accuracy_train)

# Make predictions on the test data
xgb_predicted_test <- predict(boosting_rf_model, newdata = data_test)

# Compute confusion matrix and accuracy for test data
(xgb_confusion_matrix_test <- table(xgb_predicted_test, data_test$Survived))
(xgb_accuracy_test <- sum(diag(xgb_confusion_matrix_test)) / 
    sum(xgb_confusion_matrix_test))
(xgb_misclassification_error_test <- 1 - xgb_accuracy_test)


#create the comparison table based on the test data

# Create the data frame
comparison <- data.frame(
  model = c("blrc_testing", "knn_testing",
            "ann_testing", "nb_test", "svm_test",
            "dt_test", "dt_bag_test", "rf_test",
            "trf_test", "xgb_test"),
  accuracy = c( 
               accuracy_blrc_testing, 
               accuracy_knn_testing, 
               ann_accuracy_testing, 
               nb_accuracy_test, 
               svm_accuracy_test, 
               dt_accuracy_test, 
               dt_bag_accuracy_test, 
               rf_accuracy_test, 
               trf_accuracy_test, 
               xgb_accuracy_test),
  error = c(error_blrc_testing, 
            misclassification_error_knn_testing,
            ann_misclassification_error_testing,
            nb_misclassification_error_test,
            svm_misclassification_error_test,
            dt_misclassification_error_test,
            dt_bag_misclassification_error_test,
            rf_misclassification_error_test,
            trf_misclassification_error_test,
            xgb_misclassification_error_test)
)

# Print the data frame
comparison

# Find the row index of the model with the highest accuracy
best_model_index <- which.max(comparison$accuracy)

# Get the details of the best model
best_model <- comparison[best_model_index, ]

# Print the best model
print(best_model)

#Write a reflection on your own word focusing on 
#"what did I learn from this assignment?"
#From this assignment we learn to fit the different type of classifer with the 
# given dataset. we split the data set and fit the various type of machine 
#learning model that is generally used in the classification problem.
#we also calculate the confusion matrix and accuracy and error for each model.
#then we compare the accuracy of the test data set with different model and 
#finally determine the best model.

#In our case the best model is the random forest boosting model with 
#xgb boosting which gives the accuracy of 86% in the test data.
