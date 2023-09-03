
#PART II

library(tidyverse)
library(dplyr)

#Set and Get the working directory
setwd("C:/Users/ramom/Documents/imdb")
getwd()  #checking the working directory

#import the data using read tsv function 
df_ratings<-read_tsv("title.ratings.tsv")
head(df_ratings)

#get teh structure of the data
str(df_ratings)
summary(df_ratings) #get the summary of the data

colSums(is.na(df_ratings))  #check the NA in each column 

#create the histogram of the required data 
hist(df_ratings$averageRating, main = "Distribution of Average rating Data",xlab ="Average Rating")
#creating the box plot 
boxplot(df_ratings$averageRating, main="Boxplot of Average Rating")


#Checking the null value
colSums(is.na(df_ratings))

#getting the scatter plot 
plot(df_ratings$averageRating, df_ratings$numVotes,xlab='Average Rating', ylab='Number of Votes', main='Scatter Plot of Average ratting vs No. of Votes')

#getting the corelation between variables
cor(df_ratings$averageRating,df_ratings$numVotes, method = "spearman")

#Importing another data set  
df_basics<-read_tsv("title.basics.tsv")
head(df_basics)

#checking the structure
str(df_basics)

#deselecting or dropping columns
df_basics<-df_basics%>%select(-c("primaryTitle","endYear"))
str(df_basics)  #checking structure after drop

#changing the title to factor type 
df_basics$titleType<-as.factor(df_basics$titleType)
class(df_basics$titleType)  #checking the calsss of the variable after change

#changing the data type to integer
df_basics$startYear<-as.integer(df_basics$startYear)
class(df_basics$startYear)

#changing the data type to integer 
df_basics$runtimeMinutes<-as.integer(df_basics$runtimeMinutes)
class(df_basics$runtimeMinutes)

#run the summary of the avaliable data
summary(df_basics)

#checking the null value 
colSums(is.na(df_basics))

#performing left join operations
df_ratings<-df_basics%>%left_join(df_ratings, by="tconst")
head(df_ratings)
str(df_ratings) #checking str after the join 

#Data-Preprocessing: removing 
df_ratings$genres<-gsub("\\N","",df_ratings$genres)

#Data-Preprocessing: removing excess spaces
df_ratings$genres<-gsub(" ","",df_ratings$genres)

#taking only the first genre of each movie
df_ratings$genres<-gsub(",.*","",df_ratings$genres)

#checking genres variable after pre processing 
head(df_ratings$genres)
str(df_ratings)

#plotting the data 
plot(df_ratings$runtimeMinutes,df_ratings$averageRating,main = "ScatterPlot ofRun time Minuts Vs Average rating", xlab = "Run time in Min", ylab = "Average Rating")
plot(df_ratings$runtimeMinutes,df_ratings$averageRating,main = "ScatterPlot ofRun time Minuts Vs Average rating", xlim= c(0,500),xlab = "Run time in Min", ylab = "Average Rating")

#assigning NA to field consisting \\N
df_ratings["genres"][df_ratings["genres"]=="\\N"]<-NA

#getting the frequency table 
library(plyr)
df_ratings$genres<-as.factor(df_ratings$genres)
ffreqTable<-df_ratings%>%filter(!is.na(genres))
FT<-count(ffreqTable,"genres")
FT

#creating by genres object 
#by_genre<-group_by(df_ratings,genres)  #equivalent to below 
by_genres<-df_ratings%>%group_by(genres)  
by_genres

RuntimeMean <- by_genres %>%
  filter(!is.na(runtimeMinutes))%>%group_by(genres) %>%
  summarise(mean = mean(runtimeMinutes,na.rm = TRUE))

RuntimeMean

##Get mean of runtimeMinutes variables using summarise by genres without creating "by_genres" object
RuntimeMeanWithout <- df_ratings %>%
  filter(!is.na(runtimeMinutes))%>%group_by(genres) %>%
  summarise(mean = mean(runtimeMinutes,na.rm = TRUE))

RuntimeMeanWithout

##Filter "df_ratings" data with runtimeMinutes less than 150 minutes and save it as "df_ratings_movie150m"
df_rating_movie150m<-df_ratings%>%filter(runtimeMinutes<150)
df_rating_movie150m

#Get scatterplot of runtimeMinutes and averageRating variable for this new data and interpret then carefully
plot(df_rating_movie150m$runtimeMinutes,df_rating_movie150m$averageRating,main = "Plot between runtimeMinuts vs Average rating", xlab = "Runtimein Min", ylab = "Ratings")

#Arrange the df_rating_movie150m data in descending order by averageRating and save it as "best_worst_movies"
best_worst_movies<-arrange(df_rating_movie150m,desc(averageRating))
best_worst_movies

#Show the top 6 and last 6 movies based on the arranged dataset above
head(best_worst_movies)
tail(best_worst_movies)

#Get the averageRating of adult movies (isAdult variable) using mutate function and interpret it carefully
best_worst_movies %>% mutate(adult = ifelse(isAdult == 1, averageRating,"No") )
best_worst_movies

#Divide the "df_ratings_movies150m" into training and testing dataset with 80% and 20% split with slice function
totalRow<-nrow(df_rating_movie150m)
totalRow

#spliting to tranining data
traning_data<-df_rating_movie150m%>%slice(1:round(0.8*totalRow))
traning_data
dim(traning_data)

#spliting to testing data
testing_data<-df_rating_movie150m%>%slice(round(0.8*totalRow)+1:totalRow)
testing_data
dim(testing_data)

#Get mean, standard deviation, median and interquartile range of averageRating, 
#numVotes and runtimeMinutes variable of training and testing data and interpret them carefully

train_Df<-traning_data%>% summarise(mean_averageRating = mean(averageRating,na.rm = TRUE),
                           mean_numVotes = mean(numVotes,na.rm = TRUE),
                           mean_runtimeMinutes = mean(runtimeMinutes,na.rm = TRUE),
                           sd_averageRating = sd(averageRating,na.rm=TRUE),
                           sd_numVotes = sd(numVotes,na.rm=TRUE),
                           sd_runtimeMinutes = sd(runtimeMinutes,na.rm = TRUE),
                           median_averageRating = median(averageRating,na.rm=TRUE),
                           median_numVotes = median(numVotes,na.rm=TRUE),
                           median_runtimeMinutes = median(runtimeMinutes,na.rm = TRUE),
                           IQR_averageRating = IQR(averageRating,na.rm=TRUE),
                           IQR_numVotes = IQR(numVotes,na.rm=TRUE),
                           IQR_runtimeMinutes = IQR(runtimeMinutes,na.rm = TRUE))
table(train_Df)
View(train_Df)
test_df<-testing_data%>% summarise(mean_averageRating = mean(averageRating,na.rm = TRUE),
                          mean_numVotes = mean(numVotes,na.rm = TRUE),
                          mean_runtimeMinutes = mean(runtimeMinutes,na.rm = TRUE),
                          sd_averageRating = sd(averageRating,na.rm=TRUE),
                          sd_numVotes = sd(numVotes,na.rm=TRUE),
                          sd_runtimeMinutes = sd(runtimeMinutes,na.rm = TRUE),
                          median_averageRating = median(averageRating,na.rm=TRUE),
                          median_numVotes = median(numVotes,na.rm=TRUE),
                          median_runtimeMinutes = median(runtimeMinutes,na.rm = TRUE),
                          IQR_averageRating = IQR(averageRating,na.rm=TRUE),
                          IQR_numVotes = IQR(numVotes,na.rm=TRUE),
                          IQR_runtimeMinutes = IQR(runtimeMinutes,na.rm = TRUE))
table(test_df)
View(test_df)

#Get histogram of averageRating, numVotes and runtimeMinutes variables of training and 
#testing data; compare them and interpret them carefully
hist(traning_data$averageRating,
     main="Histogram of averageRating variable in training",
     xlab="averageRating")
hist(traning_data$numVotes,
     main="Histogram of numVotes variable in training data",
     xlab="numVotes")
hist(as.numeric(traning_data$runtimeMinutes),
     main="Histogram of runtimeMinutes variable in training data",
     xlab="runtimeMinutes")
hist(testing_data$averageRating,
     main="Histogram of averageRating variable in testing data",
     xlab="averageRating")
hist(testing_data$numVotes,
     main="Histogram of numVotes variable in testing data",
     xlab="numVotes")
hist(as.numeric(testing_data$runtimeMinutes),
     main="Histogram of runtimeMinutes variable in training data",
     xlab="runtimeMinutes")


#Get boxplot of averageRating, numVotes and runtimeMinutes variables of training and 
#testing data; compare them and interpret them carefully
boxplot(traning_data$averageRating, main="Boxplot of averageRating in training ")
boxplot(traning_data$numVotes, main="Boxplot of numVotes in training ")
boxplot(traning_data$runtimeMinutes, main="Boxplot of runtimeMinutes  in training ")

#For testing Data
boxplot(testing_data$averageRating, main="Boxplot of averageRating  in testing ")
boxplot(testing_data$numVotes, main="Boxplot of numVotes  in testing")
boxplot(testing_data$runtimeMinutes, main="Boxplot of runtimeMinutes  in testing")


#PART I
#Get this data in R, make it tidy and do the necessary transformations: https://www.mohfw.gov.in/ 
#(Table: COVID-19 Statewise Status (Click to expand))
library(rvest)
library(tidyr)
library(rjson)

#load data from json 
get_data<-jsonlite::fromJSON("https://www.mohfw.gov.in/data/datanew.json")

#covert to data frame
covid_data<-as.data.frame(get_data)

#check the structure 
str(covid_data)

#view data in tabular format 
View(covid_data)

#Tidy the data 

#check for null in any column 
colSums(is.na(covid_data))

# select only the relevant columns
tidy_data <- covid_data%>%
  select(state_name, total, cured, new_death)
# rename the columns
names(tidy_data) <- c("State", "Total Cases", "Cured", "Deaths")
#print
tidy_data


#Get this data in R, make it tidy and do the necessary transformation: 
#https://covid19.who.int/table

covidWhoTable<- jsonlite::fromJSON("https://covid19.who.int/page-data/sq/d/3713876948.json")[[1]][[1]][[1]][[1]]
View(covidWhoTable)

covidWhoTable<- gsub(pattern = "[\\]","",covidWhoTable)
covidWhoTable<- jsonlite::fromJSON(covidWhoTable)
colnames(covidWhoTable)   <- gsub(" ","_",colnames(covidWhoTable))
covidWhoTable <- transmute(covidWhoTable,Country,Confirmed,Cases_Last_7_Days,Deaths,
                        Deaths_Last_7_Days,Doses_Per_100,Persons_fully_vaccinated_with_last_dose_of_primary_series,Boosters_per_100)
head(covidWhoTable)
View(covidWhoTable)


