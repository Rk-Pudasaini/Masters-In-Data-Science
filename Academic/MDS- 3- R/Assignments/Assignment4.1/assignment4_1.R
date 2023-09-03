
getwd()
setwd("C:/Users/ramom/Desktop/MDS/Academic/1st Semester/MDS- 3- R/Assignments/Assignment4.1")
getwd()

#Use the "mtcars" data and do as follows in R Studio and submit the compiled 
#PDF report file with codes and outputs here:

#Before fitting the model first load the data and  confirm that the 
#dependent variable is #normally distributed

data <- mtcars  #load the data 
#str(data) #check the structure of data

#check the normality of dependent variable i.e  mpg
#suggestive check 
qqnorm(data$mpg)
qqline(data$mpg)

#looking the graph we are not sure whether the data is normal or not.
#some data points align with the line and some are away 
#so we do confirmation test for the normality of dependent variable 
#we will use Shapiro-Wilk test

shapiro.test(data$mpg)

#the p value(0.12) is greater than 0.05 so we can confirm that the data is
#normally distributed

#Now we can move ahead for modeling 
#1. Fit multiple linear regression with mpg as dependent variable and rest 
#of the variables in the mtcars data as independent variables and 
#save it as mlr object
set.seed(26)
mlr <- lm(mpg ~., data = mtcars)

#2. Get the summary of mlr and interpret the result carefully
summary(mlr)

#interpretation 
#This provide the summary output of the linear regression model
#performance and significant of each predictor variables
#the estimate columns present the slope for each predictor variable. it 
#represent the expected changes in response to mpg variable
#for example the coefficient for "cyl" is -0.11144, suggesting that for 
#every one-unit increase in the number of cylinders, the expected change 
#in mpg is a decrease of 0.11144 units.

#the last column gives the p value associated with each coefficient estimate.
#it provide the significance of each predictors contribution to the model. 
#here the p value is greater than the significance level (pvalue > 0.05)
#it means that the predictor variable doesnot contribute more in the model 
#expect wt which is significantly close to 0.05

#the residual or error is low 

#Multiple R-squared is 0.869 means 86% of variation in mpg can be 
#explained by predictor variable in the model

# The adjusted R-squared value of 0.8066 means that about 80.66% of the 
#variation in mpg can be explained by the predictors, taking into 
#consideration the number of predictors in the model.

#it means that the accuracy is 80% considering all the predictor variables

#3. Get the VIF of mlr model and drop variables with VIF > 10 one-by-one 
#until none of the predictors have VIF > 10
library(car)
vif(mlr)

#dropping the variable disp as it is highest and VIF > 10
mlr1 <- lm(mpg ~ cyl+hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars)
summary(mlr1)
vif(mlr1)

#dropping the variable cly variable  as it is highest now and VIF > 10
mlr2 <- lm(mpg ~ hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars)
vif(mlr2)

#4. Fit the mlr model with predictors having VIF <=10, get the summary of mlr 
#and interpret the result carefully
summary(mlr2)

#Intrepretation
#by dropping two insignificant variable we get the summary of the model again 
#and find out that wt variable is significant as the pvalue of wt is 0.03 which
#which is less than 0.05 and also we get the model accuracy as 81%

#5. Fit lasso regression with mpg as dependent variable and rest of the 
#variables in the mtcars data as independent variables as cv_model object 
#using cv.glmnet model included in the glmnet 

# Install and load the required packages
#install.packages("glmnet")
library(glmnet)

# Separate the dependent variable (mpg) and independent variables
mpg <- mtcars$mpg
independent_vars <- mtcars[, -1] # Exclude the first column (mpg)
#print(independent_vars)
# fits a Lasso regression model and performs cross-validation
cv_model <- cv.glmnet(x = as.matrix(independent_vars), y = mpg, alpha = 1)

#cv.glmnet() function used for fitting regularized regression models, 
#such as Lasso or Ridge regression, with built-in cross-validation.

#x specifies the predictor variables (independent_vars) 
#as the input matrix x for the model. 

#y specifies the response variable (mpg) as the input vector y for the model.

#The alpha argument controls the type of regularization used in the model. 
#A value of 1 indicates Lasso regression, which applies L1 regularization to 
#encourage sparsity in the coefficient estimates. L1 regularization can set 
#some coefficients to exactly zero, effectively performing feature selection 
#by eliminating irrelevant predictors.

print(cv_model)

#The number of non zero column is 5 when lambda is 0.55 and when the lamda 
#with the standard error of mean square error(MSE), the the non zero column is 3

#6. Get the best lambda value from the lasso regression fitted above, plot 
#the cv_model and interpret them carefully
# Get the best lambda value
best_lambda <- cv_model$lambda.min
#the lambda value with minimum mean squared error (MSE) during cross-validation 

# Plot the cv_model
plot(cv_model)
#the plot provide cross-validated performance of the Lasso regression model 
#across different lambda values.

# Add a vertical line at the best lambda value
abline(v = best_lambda, col = "red")


#7 . Fit the best lasso regression model as best_model using the 
#best_lambda value obtained above
# Fit the best Lasso regression model using the best lambda value
best_model <- glmnet(x = independent_vars, y = mpg, alpha = 1, 
                     lambda = best_lambda)

#8. Get the coefficients of the best_model and identify the 
#important variables with s0 non-missing values
# Get the coefficients of the best model
coefficients <- coef(best_model, s = best_lambda)
print(coefficients)

# Identify important variables with non-missing values
important_variables <- rownames(coefficients)[coefficients[, 1] != 0][-1]
print(important_variables)

# Step 9: Fit multiple linear regression using independent variables 
#from best_model
mlr_final <- lm(mpg ~ ., data = mtcars[, c("mpg", important_variables)])
#now we have fitted the multiple linear regression model using the 
#independent variables obtained from Lasso Regression 


#10. Compare the statistically significant variables obtained 
#from step 4 and step 9
summary(mlr2)
summary(mlr_final)

#for mlr2
#Residual standard error: 2.566 
#Multiple R-squared:  0.8655
#Adjusted R-squared:  0.8187 (accuracy of 82%)

#for mlr_final using Lasso regression 
#Residual standard error: 2.502 
#Multiple R-squared:  0.8555	
#Adjusted R-squared:  0.8277 (Accuracy of 83%)

#also checking the summary based on p value
summary(mlr2)$coefficients[summary(mlr2)$coefficients[, "Pr(>|t|)"] < 0.05, ]
#Analysis of first summary
#each unit increase in the predictor variable, the expected change in the 
#response variable (mpg) is -2.60967758 units. 
#here  p value (0.03416499 )is less 0.05 so we can conclude that the predictor 
#variable is statistically significant in predicting the response variable.

summary(mlr_final)$coefficients[summary
                                (mlr_final)$coefficients[, "Pr(>|t|)"] < 0.05, ]

#Analysis of final model
#the weight variable has a significant effect on mpg, with higher weights 
#leading to lower mpg values.

#model using the lasso regression is best for now

#11. Write a summary for handling multicollinearity with VIF 
#dropouts and LASSO regression

#Both VIF dropouts and LASSO regression are useful techniques for handling 
#multicollinearity. VIF dropouts allow for a direct assessment of collinearity 
#by examining the VIF values, and removing highly correlated variables based 
#on a predetermined threshold. On the other hand, LASSO regression provides a 
#more automated approach to variable selection, by estimating the importance 
#of variables and shrinking less important ones towards zero




