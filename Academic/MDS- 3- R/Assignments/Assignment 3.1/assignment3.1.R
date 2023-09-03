data("mtcars")
str(mtcars)

#converting to the data frame 
df <- as.data.frame(mtcars)
View(df)

#1. Create a barplot of cyl variable of mtcars data with and without using factor argument

#bar plot for the clylender
barplot(df$cyl, main = "Number of cylinders for 50 cars")

#Define cyl as factor variable
cyl <- as.factor((df$cyl))

#barplot(df_Cars$cly)   #we do not get bar plot simply like this convert it into table
barplot(table(df$cyl))

#by assign it as object also we can get bar plot 
barplot_cyl <- table(df$cyl)
#get the bar plot again
barplot(barplot_cyl, main = "Barplot of cly variable", xlab = "Number of Cylinder", ylab = "Frequency")

#2. Locate median of mpg variable of mtcars data graphically and compare the median value with in-built function of R
hist(df$mpg , main = "Histogram of mpg Variable", xlab = "mpg", ylab = "Frequency")
abline(v = median(df$mpg), lwd = 3, lty = 4, col = "red")  #redline drawn from the median point 

#this can be done graphically as well 
mpg_sorted <- sort(df$mpg)

# Calculate the cumulative sum of frequencies for the "mpg" data
freq_more_than <- cumsum(table(mpg_sorted))
freq_less_than <- rev(cumsum(rev(table(mpg_sorted))))

# Plot the cumulative frequency curves
plot(names(freq_more_than), freq_more_than, type = "l", xlab = "mpg", ylab = "Cumulative Frequency", main = "Cumulative Frequency of More & Less mpg")
lines(names(freq_less_than), freq_less_than, type = "l", col = "red")

#Locate the point of intersection and draw the perpendicular line to the x-axis
intersection_index <- which.min(abs(freq_more_than - freq_less_than))
median_mpg <- names(freq_more_than)[intersection_index]
abline(v=median_mpg, col="green")


# Compare the obtained median value with the result of the "median" function in R
median_value <- median(df$mpg)
cat("Graphical median:", median_mpg, "\n")
cat("R median function:", median_value)


#3. Locate mode of mpg variable of mtcars data graphically and compare the mode 
#value with the in-built/custom function of R
mode(df$mpg)    #doesnot gives any result so need to calculate manually
#but we can get the mode by 3*median - 2*mean
mode_Value <- (3* median(df$mpg) - 2 * mean(df$mpg))   #17.42
hist(df$mpg , main = "Histogram of mpg Variable", xlab = "mpg", ylab = "Frequency")
abline(v = mode_Value, lwd = 3, lty = 4, col = "blue")


#4. Create a scatterplot of mgp (dependent) and wt (independent) variable of mtcars 
#data, add ablines at mean plus-minus 2 standard deviation and show the name of 
#the cars with mpg > 2 standard deviation from mean inside the scatterplot obtained above

plot(df$wt, df$mpg, pch =16, main = "Satterplot of mpg vs weigth", xlab = "Weight", ylab = "Mpg", ylim = c(5,35))
abline(h = mean(mtcars$mpg), col = "red", lwd = 2)

# calculate the upper and lower bounds for the ablines
mpg_upper <- (mean(df$mpg) + 2 * sd(df$mpg))
mpg_lower <- (mean(df$mpg) - 2 * sd(df$mpg))

#adding abline at mean +- 2 standard deviation 
abline(h = mpg_upper, col = "green", lwd = 2)
abline(h = mpg_lower, col = "green", lwd = 2)

#also can be done like this
#abline(h = (mean(df$mpg) + 2 * sd(df$mpg)), col = "green", lwd = 2)
#abline(h = (mean(df$mpg) - 2 * sd(df$mpg)), col = "green", lwd = 2)

# label the points with mpg > 2 standard deviation from mean inside the scatterplot
text(df$wt[df$mpg > mpg_upper], df$mpg[df$mpg > mpg_upper], labels = rownames(df)[df$mpg > mpg_upper], pos = 4, col = "red")
#text(df$wt[df$mpg > (mean(df$mpg) + 2 * sd(df$mpg))], df$mpg[df$mpg > (mean(df$mpg) + 2 * sd(df$mpg))], labels = rownames(mtcars)[df$mpg > (mean(df$mpg) + 2 * sd(df$mpg))], pos = 4, col = "red")

#diaplay the name only
NameofCars <- rownames(df)[df$mpg > mpg_upper]
NameofCars

#display the name of the cars with  milage lower than 15
Low_milage_Cars <- rownames(df)[df$mpg < 15]
Low_milage_Cars

#5. Create a x variable with 100 random numbers and y variable with x + 100 
# random numbers; create a factor variable with 100 monthly random observations, 
# create a time series data with 100 random values starting from January 1970; 
# create a date variable with 100 random values starting from 1970/01/01 
# increasing each day; create a new variable z with square of x variable
set.seed(26)
# Create x variable with 100 random numbers
x <- rnorm(100)

# Create y variable with x + 100 random numbers
y <- x + rnorm(100)

# Create factor variable with 100 monthly random observations
month <- sample(month.name, 100, replace = TRUE)
factor_var <- factor(month)

# Create time series data with 100 random values starting from January 1970
ts_data <- ts(rnorm(100), start = c(1970,1), frequency = 12)

# Create date variable with 100 random values starting from 1970/01/01 increasing each day
date_var <- seq(as.Date("1970/01/01"), by = "day", length = 100)

# Create new variable z with square of x variable
z <- function(x)x^2

#6. Create a 2 x 3 plot window with scatterplot, barplot, boxplot, time series 
#plot, date plot and square function plot

par(mfrow = c(2,3))
plot(x,y, main = "Scatterplot")
plot(factor_var, main = "Barplot")
plot(factor_var, rnorm(100), main = "Boxplot")
plot(ts_data, main = "Time Series Plot")
plot(date_var, rnorm(100), main = "Time based Plot")
plot(z, 0,100, main = "Square Plot")

# converting to 1 x 1 plot 
par(mfrow = c(1,1))

#7. Perform log transformation on x and z variables i.e. log of x, log of z and 
#log of x and z in the plot command and interpret the results carefully 

# Log transformation of x and z variables
set.seed(26)
x <- sample(1:300, 100, replace = TRUE)
z <- x^2

log_x <- log(x)
log_z <- log(z)
log_xz <- log_x + log_z

# Plot the log transformed variables
par(mfrow = c(2, 3))
plot(x, main = "x")
plot(log_x, main = "Log of x")
plot(z, main = "z")
plot(log_z, main = "Log of z")
plot(log_xz, main = "Log of x and z")

# converting to 1 x 1 plot 
par(mfrow = c(1,1))

# 8. Create a 1x1 plot window with correlation matrix plot of first 
#three numerical variable of mtcars data (cyl is not numeric, its factor!)
plot(df[, c(1,3,4)], main = "Correlation matrix plot of First three Numerical Variable")

# 9. Interpret the result of the correlation matrix plot obtained above with 
#respect to the correlation coefficient to be used for each pair
cor(df[, c(1,3,4)])
#mpg and disp negative correlation 
#mpg to hp negative correlation 
#disp to hp positive correlation


# 10. Write advantage and limitations of plots created using R base packages based on these nine steps


