#setting up working directory 
getwd()
setwd("C:/Users/ramom/Desktop/MDS/Academic/1st Semester/MDS- 3- R/Assignments/Assignment4.2")
getwd()

# Set the seed 
set.seed(26)

#1. Generate a 1000 random data with 10 variables 
#[five continuous: age (18 to 90 years), 
#height (150 - 180 cm), weight (50 - 90 kg), 
#income (10000 - 200000), diastolic blood pressure (70 - 170 mm Hg) and 
#five categorical: sex (male/female), education (no education, primary, 
#secondary, tertiary), place of residence (rural/urban), 
#socio-economic status (low/medium/high) and exercise (yes/no)] 
#using set.seed(your roll number and save it as SR object

#generating five continuous variable 
age <- sample(18:90, 1000, replace = TRUE)
height <- sample(150:180, 1000, replace = TRUE)
weight <- sample(50:90, 1000, replace = TRUE)
income <- sample(10000:200000, 1000, replace = T)
diastolic_bp <- sample(70:170, 1000, replace = T)

#create categorical variables
sex <- sample(c("male", "female"), 1000, replace = TRUE)
education <- sample(c("no education", "primary", "secondary", 
                      "tertiary"), 1000, replace = TRUE)
place_of_residence <- sample(c("rural", "urban"), 1000, replace = TRUE)
socioeconomic_status <- sample(c("low", "medium", "high"), 1000, replace = TRUE)
exercise <- sample(c("yes", "no"), 1000, replace = TRUE)

#create the dataframe 
SR <- data.frame(age, height, weight, income, diastolic_bp, sex, 
                 education, place_of_residence, socioeconomic_status, exercise)
head(SR)  #check the head of the dataframe
str(SR)

#changing categorical variable into factor
SR$sex <- as.factor(SR$sex)
SR$education <- as.factor(SR$education)
SR$place_of_residence <- as.factor(SR$place_of_residence)
SR$socioeconomic_status <- as.factor(SR$socioeconomic_status)
SR$exercise <- as.factor(SR$exercise)

str(SR)  #check the structure

#2. Randomly split the SR object data as SR.train (70%) and SR.test (30%) with 
#replacement sampling and fit multiple linear regression with diastolic 
#blood pressure as dependent variable and rest of variables as independent 
#variable and get fit indices (R-Square, MSE, RMSE and MAE) for the SR.test data

#randomly split into 70% train and 30% test
ind <- sample(2, nrow(SR), replace = T, prob = c(0.7, 0.3))

#split into training and testing dataset
SR.train <- SR[ind == 1, ]
SR.test <- SR[ind == 2, ]

#fitting the multiple linear regression model with diastolic_bp as 
#dependent variable
mlr_model <- lm(diastolic_bp ~ ., data = SR.train)

#model accuracy for training data set
summary(mlr_model)

#loading necessary library
library(dplyr)
library(caret)

predictions <- mlr_model %>%
  predict(SR.test)

#model accuracy for testing data set 
data.frame(R2 = R2(predictions, SR.test$diastolic_bp),
           MSE = mean(mlr_model$residuals^2),
           RMSE = RMSE(predictions, SR.test$diastolic_bp),
           MAE = MAE(predictions, SR.test$diastolic_bp))

#3.Fit the multiple linear regression model with Leave One Out Cross-Validation,
#k-fold cross validation, repeated k-fold cross validation methods and get fit 
#indices for SR.test data and, compare the fit indices of supervised 
#regression models fitted in step 2 and 3 above with careful interpretation

#Leave One Out Cross-Validation
# Define training control
train.control <- trainControl(method = "LOOCV")

# Train the model
model1 <- train(diastolic_bp ~ ., data = SR, method = 
                  "lm", trControl = train.control)

#summarize the result
summary(model1)
print(model1)

#Prediction with LOOCV and module accuracy
predictions1 <- model1 %>% 
  predict(SR.test)

data.frame(R2 = R2(predictions1, SR.test$diastolic_bp),
           MSE = mean((predictions1 - SR.test$diastolic_bp)^2),
           RMSE = RMSE(predictions1, SR.test$diastolic_bp),
           MAE = MAE(predictions1, SR.test$diastolic_bp))


# Fit multiple linear regression model using k-Fold Cross-Validation (k = 10)
lm_model_kfold <- train(diastolic_bp ~ ., 
                        data = SR, method = "lm", 
                        trControl = trainControl(method = "cv", number = 10))

#prediction with k fold cross validation 
predictions_kfold <- lm_model_kfold %>%
  predict(SR.test)

#getting the required performance indicators
data.frame(R2_kfold = R2(predictions_kfold, SR.test$diastolic_bp),
           MSE_kfold = mean((predictions_kfold - SR.test$diastolic_bp)^2),
           RMSE_kfold = RMSE(predictions_kfold, SR.test$diastolic_bp),
           MAE_kfold = MAE(predictions_kfold, SR.test$diastolic_bp))

# Fit multiple linear regression model using Repeated k-Fold Cross-Validation 
#(k = 10, repeats = 5)
lm_model_repkfold <- train(diastolic_bp ~ ., 
                           data = SR, method = "lm", 
                           trControl = trainControl(method = "repeatedcv", 
                                                    number = 10, repeats = 5))
#getting the prediction using repeated k fold
predictions_repkfold <- lm_model_repkfold %>%
  predict(SR.test)

#getting the required performance indicators for repeated k fold
data.frame(R2_repkfold = R2(predictions_repkfold, SR.test$diastolic_bp),
           MSE_repkfold = mean((predictions_repkfold - SR.test$diastolic_bp)^2),
           RMSE_repkfold = RMSE(predictions_repkfold, SR.test$diastolic_bp),
           MAE_repkfold = MAE(predictions_repkfold, SR.test$diastolic_bp))

#4. Fit KNN regression, Decision Tree regression, SVM regression and Neural 
#Network regression using the same dependent and independent variables, get 
#and compare fit indices of these models for SR.test data

# Fit KNN regression
knn_model <- train(diastolic_bp ~ ., data = SR.train, 
                   method = "knn", trControl = trainControl(method = "none"))

#prediction using knn
predictions_knn <- knn_model %>%
  predict(SR.test)

#getting the required performance indicators for knn
data.frame(R2_knn = R2(predictions_knn, SR.test$diastolic_bp),
           MSE_knn = mean((predictions_knn - SR.test$diastolic_bp)^2),
           RMSE_knn = RMSE(predictions_knn, SR.test$diastolic_bp),
           MAE_knn = MAE(predictions_knn, SR.test$diastolic_bp))


# Fit Decision Tree regression
dt_model <- train(diastolic_bp ~ ., data = SR.train, 
                  method = "rpart", trControl = trainControl(method = "none"))

#prediction using decision tree
predictions_dt <- dt_model %>%
  predict(SR.test)

#getting the required performance indicators for decision tree
data.frame(R2_dt = R2(predictions_dt, SR.test$diastolic_bp),
           MSE_dt = mean((predictions_dt - SR.test$diastolic_bp)^2),
           RMSE_dt = RMSE(predictions_dt, SR.test$diastolic_bp),
           MAE_dt = MAE(predictions_dt, SR.test$diastolic_bp))


library(kernlab)
# Fit SVM regression
svm_model <- train(diastolic_bp ~ ., data = SR.train, 
                   method = "svmRadial", 
                   trControl = trainControl(method = "none"))

#prediction using SVM
predictions_svm <- svm_model %>%
  predict(SR.test)

#getting the required performance indicators for SVM
data.frame(R2_svm = R2(predictions_svm, SR.test$diastolic_bp),
           MSE_svm = mean((predictions_svm - SR.test$diastolic_bp)^2),
           RMSE_svm = RMSE(predictions_svm, SR.test$diastolic_bp),
           MAE_svm = MAE(predictions_svm, SR.test$diastolic_bp))


# Fit Neural Network regression
nn_model <- train(diastolic_bp ~ ., data = SR.train, 
                  method = "nnet", trControl = trainControl(method = "none"))

#prediction using neural net
predictions_nn <- nn_model %>%
  predict(SR.test)

#getting the required performance indicators for SVM
data.frame(R2_nn = R2(predictions_nn, SR.test$diastolic_bp),
           MSE_nn = mean((predictions_nn - SR.test$diastolic_bp)^2),
           RMSE_nn = RMSE(predictions_nn, SR.test$diastolic_bp),
           MAE_nn = MAE(predictions_nn, SR.test$diastolic_bp))

#from multiple linear regression model 
# R2          MSE           RMSE         MAE
# 0.02563123  869.9229      27.75389     24.06366

#multiple linear regression model with Leave One Out Cross-Validation
# R2          MSE           RMSE         MAE
# 0.03548656  762.16        27.60725     23.97041


# multiple linear regression model using k-Fold Cross-Validation (k = 10)
#R2_kfold       MSE_kfold       RMSE_kfold        MAE_kfold
#0.03548656   762.16          27.60725          23.97041


#multiple linear regression model using Repeated k-Fold Cross-Validation 
#R2_repkfold        MSE_repkfold        RMSE_repkfold       MAE_repkfold
#0.03548656         762.16              27.60725            23.97041


#knn regression 
#R2_knn     MSE_knn     RMSE_knn      MAE_knn
#0.01140901 892.1185    29.86835      24.275

#decision tree
#R2_dt   MSE_dt     RMSE_dt       MAE_dt
#NA      787.7586   28.06704      24.19944

#svm
#R2_svm     MSE_svm       RMSE_svm      MAE_svm
#0.01192118 786.2485      28.04012      24.19448

#neural network
#R2_nn   MSE_nn      RMSE_nn      MAE_nn
#NA      15347.77    123.8861     120.6688

#the best model has the highest R-Square value and the 
#lowest MSE, RMSE, and MAE values.
# from the above analysis the linear model with LOOCV, k- fold CV and 
#repeated k fold CV gives the highest R2 value (0.03548656) and lowest 
#MSE, RMSE, and MAE values. So these model are the best model for prediction 


#Predict diastolic blood pressure of a person with 50 years, 175mm height, 
#80 kg weight, 90000 income, male, tertiary level education, living in urban 
#area, medium socio-economic status and no exercise and 
#interpret the result carefully

# From above we can use any of the model loocv or k fold cv or 
#repeated k fold cv Linear Regression model as the best model
# Assuming the Multiple Linear Regression model as the best model
prediction<-predict(model1, newdata = data.frame(age = 50, 
                                                height = 175, weight = 80, 
                                                income = 90000, sex = "male", 
                                                education = "tertiary", 
                                                place_of_residence = "urban", 
                                                socioeconomic_status = "medium", 
                                                exercise = "no"))
prediction  #here the diastolic blood pressure based the given data is 118.389

prediction_lm_K_fold<-predict(lm_model_kfold, newdata = data.frame(age = 50, 
                                                 height = 175, weight = 80, 
                                                 income = 90000, sex = "male", 
                                                 education = "tertiary", 
                                                 place_of_residence = "urban", 
                                                 socioeconomic_status = "medium", 
                                                 exercise = "no"))
prediction_lm_K_fold
#7. Write a reflection of the assignment on your own words focusing on 
#"what did I learn with this assignment?"

#Here in this assignment we have fitted the multiple linear regression model
#and get the required indicators for the analysis. We randomly create the 
#variable data and fit different model and find the best model.

# The best model for predicting diastolic blood pressure is the multiple linear 
# regression model with Leave One Out Cross-Validation (LOOCV), k-fold 
# cross-validation, or repeated k-fold cross-validation. These models have 
# the highest R-squared value and the lowest mean squared error (MSE), root 
# mean squared error (RMSE), and mean absolute error (MAE) values compared to 
# other models such as KNN regression, decision tree regression, 
# SVM regression, and neural network regression.









