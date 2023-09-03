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

# Determine the number of rows for training and testing datasets
n_train <- round(0.7 * nrow(SR))  # 70% for training
n_test <- nrow(SR) - n_train     # Remaining 30% for testing

# Randomly sample row indices for training and testing datasets
train_indices <- sample(1:nrow(SR), n_train, replace = TRUE)
test_indices <- setdiff(1:nrow(SR), train_indices)

# Split the data into training and testing datasets
SR.train <- SR[train_indices, ]
SR.test <- SR[test_indices, ]

# Fit the multiple linear regression model
lm_model <- lm(diastolic_bp ~ ., data = SR.train)

# Get predictions for the testing dataset
predictions <- predict(lm_model, newdata = SR.test)

# Calculate fit indices
rsq <- summary(lm_model)$r.squared
mse <- mean((predictions - SR.test$diastolic_bp)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - SR.test$diastolic_bp))

library(caret)  # Required for k-Fold CV and repeated k-Fold CV

# Fit multiple linear regression model using Leave-One-Out Cross-Validation (LOOCV)
lm_model_loocv <- train(diastolic_bp ~ ., data = SR, method = "lm", trControl = trainControl(method = "LOOCV"))
predictions_loocv <- predict(lm_model_loocv, newdata = SR.test)
rsq_loocv <- cor(predictions_loocv, SR.test$diastolic_bp)^2
mse_loocv <- mean((predictions_loocv - SR.test$diastolic_bp)^2)
rmse_loocv <- sqrt(mse_loocv)
mae_loocv <- mean(abs(predictions_loocv - SR.test$diastolic_bp))



# Fit multiple linear regression model using k-Fold Cross-Validation (k = 10)
lm_model_kfold <- train(diastolic_bp ~ ., data = SR, method = "lm", trControl = trainControl(method = "cv", number = 10))
predictions_kfold <- predict(lm_model_kfold, newdata = SR.test)
rsq_kfold <- cor(predictions_kfold, SR.test$diastolic_bp)^2
mse_kfold <- mean((predictions_kfold - SR.test$diastolic_bp)^2)
rmse_kfold <- sqrt(mse_kfold)
mae_kfold <- mean(abs(predictions_kfold - SR.test$diastolic_bp))

# Fit multiple linear regression model using Repeated k-Fold Cross-Validation (k = 10, repeats = 5)
lm_model_repkfold <- train(diastolic_bp ~ ., data = SR, method = "lm", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))
predictions_repkfold <- predict(lm_model_repkfold, newdata = SR.test)
rsq_repkfold <- cor(predictions_repkfold, SR.test$diastolic_bp)^2
mse_repkfold <- mean((predictions_repkfold - SR.test$diastolic_bp)^2)
rmse_repkfold <- sqrt(mse_repkfold)
mae_repkfold <- mean(abs(predictions_repkfold - SR.test$diastolic_bp))


# Compare fit indices
cat("Fit Indices - Supervised Regression Models:\n")
cat("-------------------------------------------\n")
cat("Multiple Linear Regression:\n")
cat("R-Square:", rsq, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("\n")

cat("Leave-One-Out Cross-Validation (LOOCV):\n")
cat("R-Square:", rsq_loocv, "\n")
cat("MSE:", mse_loocv, "\n")
cat("RMSE:", rmse_loocv, "\n")
cat("MAE:", mae_loocv, "\n")
cat("\n")

cat("k-Fold Cross-Validation (k = 10):\n")
cat("R-Square:", rsq_kfold, "\n")
cat("MSE:", mse_kfold, "\n")
cat("RMSE:", rmse_kfold, "\n")
cat("MAE:", mae_kfold, "\n")

# Fit KNN regression
knn_model <- train(diastolic_bp ~ ., data = SR.train, method = "knn", trControl = trainControl(method = "none"))
predictions_knn <- predict(knn_model, newdata = SR.test)
rsq_knn <- cor(predictions_knn, SR.test$diastolic_bp)^2
mse_knn <- mean((predictions_knn - SR.test$diastolic_bp)^2)
rmse_knn <- sqrt(mse_knn)
mae_knn <- mean(abs(predictions_knn - SR.test$diastolic_bp))

# Fit Decision Tree regression
dt_model <- train(diastolic_bp ~ ., data = SR.train, method = "rpart", trControl = trainControl(method = "none"))
predictions_dt <- predict(dt_model, newdata = SR.test)
rsq_dt <- cor(predictions_dt, SR.test$diastolic_bp)^2
mse_dt <- mean((predictions_dt - SR.test$diastolic_bp)^2)
rmse_dt <- sqrt(mse_dt)
mae_dt <- mean(abs(predictions_dt - SR.test$diastolic_bp))

# Fit SVM regression
svm_model <- train(diastolic_bp ~ ., data = SR.train, method = "svmRadial", trControl = trainControl(method = "none"))
predictions_svm <- predict(svm_model, newdata = SR.test)
rsq_svm <- cor(predictions_svm, SR.test$diastolic_bp)^2
mse_svm <- mean((predictions_svm - SR.test$diastolic_bp)^2)
rmse_svm <- sqrt(mse_svm)
mae_svm <- mean(abs(predictions_svm - SR.test$diastolic_bp))

# Fit Neural Network regression
nn_model <- train(diastolic_bp ~ ., data = SR.train, method = "nnet", trControl = trainControl(method = "none"))
predictions_nn <- predict(nn_model, newdata = SR.test)
rsq_nn <- cor(predictions_nn, SR.test$diastolic_bp)^2
mse_nn <- mean((predictions_nn - SR.test$diastolic_bp)^2)
rmse_nn <- sqrt(mse_nn)
mae_nn <- mean(abs(predictions_nn - SR.test$diastolic_bp))

# Compare fit indices
cat("Fit Indices - Regression Models:\n")
cat("---------------------------------\n")
cat("Multiple Linear Regression:\n")
cat("R-Square:", rsq, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("\n")

cat("KNN Regression:\n")
cat("R-Square:", rsq_knn, "\n")
cat("MSE:", mse_knn, "\n")
cat("RMSE:", rmse_knn, "\n")
cat("MAE:", mae_knn, "\n")
cat("\n")

cat("Decision Tree Regression:\n")
cat("R-Square:", rsq_dt, "\n")
cat("MSE:", mse_dt, "\n")
cat("RMSE:", rmse_dt, "\n")
cat("MAE:", mae_dt, "\n")
cat("\n")

cat("SVM Regression:\n")
cat("R-Square:", rsq_svm, "\n")
cat("MSE:", mse_svm, "\n")
cat("\n")

    




