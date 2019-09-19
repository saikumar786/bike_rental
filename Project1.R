#First remove the data in environment
rm(list = ls())

#Loading data
day <- read.csv("day.csv")

#copying for safety
bike <- day

#checking the head of the data
head(bike)

#Dimension of Data
dim(bike) #731-Observations/rows and 16-variables/features

#Columns of the data
names(bike)

#Checking the structure of the data
str(bike)

#making the bike as dataframe
bike <- data.frame(bike)

#To get the Summary Statistics on columns of data frame
summary(bike)

#Checking if any null exist in dataframe
missing <- data.frame(apply(bike, 2, function(x) {sum(is.na(x))})) #2 means column wise operation
print(missing)

#Now, we can see that No column is having any null value. So we can proceed further

#We can extract numeric columns only
numeric_cols <- which(sapply(bike, is.numeric))
bike_numcols <- data.frame(bike[, numeric_cols])


#Temparature Information

#temp: Normalized temperature in Celsius. The values are divided to 41 (max)

#atemp: Normalized feeling temperature in Celsius. The values are divided to 50 (max)

#hum: Normalized humidity. The values are divided to 100 (max)

#windspeed: Normalized wind speed. The values are divided to 67 (max)

bike$new_temp <- bike$temp * 41
bike$new_atemp <- bike$atemp * 50
bike$new_hum <- bike$hum * 100
bike$new_windspeed <- bike$windspeed * 67



#------------------Season-----------------------
#season: season (1:spring, 2:summer, 3:fall, 4:winter)
#---converting season(1,2,3,4) to season(Spring,Summer,Rainy,Winter)--------
bike$season <- factor(format(bike$season, format="%A"),
                      levels = c("1", "2", "3", "4"), labels = c("Spring", "Summer", "Rainy", "Winter"))
table(bike$season)

#---------------converting holiday column 0->Working Day 1->Holiday -----------------
bike$holiday <- factor(format(bike$holiday, format="%A"),
                       levels = c("0", "1"), labels = c("Working Day", "Holiday"))
table(bike$holiday)

#-----------------converting weathersit from numeric to categorical------------------
bike$weathersit <- factor(format(bike$weathersit, format="%A"),
                          levels = c("1", "2", "3", "4"),
                          labels = c("Good", "Moderate", "Bad", "Worse"))
table(bike$weathersit)


#*************************Exploratory Data Analysis**************************************
spring <- subset(bike, bike$season == "Spring")$new_temp

summer <- subset(bike, bike$season == "Summer")$new_temp

rainy <- subset(bike, bike$season == "Rainy")$new_temp

winter <- subset(bike, bike$season == "Winter")$new_temp


#Temparatures in Spring
hist(spring, xlab = 'Temparature in Celcius', ylab = 'Days', main = paste("Histogram of showing Temparatures in Spring"))

#Temparatures in Summer
hist(summer, xlab = 'Temparature in Celcius', ylab = 'Days', main = paste("Histogram of showing Temparatures in Summer"))

#Temparatures in Rainy
hist(rainy, xlab = 'Temparature in Celcius', ylab = 'Days', main = paste("Histogram of showing Temparatures in Rainy"))


#Temparatures in Winter
hist(winter, xlab = 'Temparature in Celcius', ylab = 'Days', main = paste("Histogram of showing Temparatures in Winter"))


#Bike Rental distribution
plot1 <- hist(bike$cnt, breaks = 40, xlab = "Bike Rental count", ylab = "Frequency of Rental",
              main = "Bike Rental Distribution", col = 'red')
#From the above plot, we can see that the target variable 'cnt' is normally distributed

#Bike Rental vs season
boxplot(bike$cnt ~ bike$season, data = bike, xlab = "Season", ylab = "Bike Rental", 
        main = "Bike Rental vs Season", col = c("blue", "lightblue", "green", "lightgreen"))

#Bike Rental vs holiday
boxplot(bike$cnt ~ bike$holiday, data = bike, xlab = "Holiday or Working Day", ylab = "Bike Rental",
        main = "Bike Rental vs Holiday/Workingday", col=c("blue", "lightblue"))

#Bike Rental vs weathersit
boxplot(bike$cnt ~ bike$weathersit, data = bike, xlab = "Weather", ylab = "Bike Rental", levels = c("1","2","3","4"),
        labels = c("Good","Moderate", "Bad", "Worse"), main = "Bike Rental vs Weather Situation", col=c("blue", "lightblue", "green","lightgreen"))

#plots related to Temparatures
#new_temp vs cnt
plot(bike$new_temp, bike$cnt, type='h', col="lightblue", xlab = "converted temp", ylab = "Bike Rental")

#new_atemp vs cnt
plot(bike$new_atemp, bike$cnt, type='h', col="lightblue", xlab = "converted atemp", ylab = "Bike Rental")

#new_hum vs cnt
plot(bike$new_hum, bike$cnt, type='h', col="lightblue", xlab = "converted hum", ylab = "Bike Rental")

#new_windspeed vs cnt
plot(bike$new_windspeed, bike$cnt, type='h', col="lightblue", xlab = "converted windspeed", ylab = "Bike Rental")

#Scatterplot with Regression line
library(ggplot2)
#Temparature vs bike rentals
temp_cnt <- ggplot(bike, aes(x=temp, y=cnt)) + geom_point() + geom_smooth(method = "lm")
temp_cnt

#atemp vs bike rentals
atemp_cnt <- ggplot(bike, aes(x=atemp, y=cnt)) + geom_point() + geom_smooth(method = "lm")
atemp_cnt

#-----------------------------------Correlation----------------------------------------

#Plotting Correlation plot
#First install and load the required package named 'corrplot'
install.packages("corrplot")
library(corrplot)

#Calculates correlation with all variables of numeric type(as we have extracted all numeric
#type columns in 'bike_numcols')
bike_cor <- cor(bike_numcols)
#Plotting correlation plot
corrplot(bike_cor, method = "number")

#Normality check
for (feature in colnames(bike_numcols)) {
  qqnorm(bike[,feature],main = paste(feature,"QQPlot"))
}

#---------------------------------Split data into train and test---------------------------
require(caTools)

set.seed(123)
#splitting data into 70% TRUE and 30% FALSE as sample
sample1 <- sample.split(bike, SplitRatio = 0.7)

#the rows which have TRUE while splitting will be in this train dataframe
train <- subset(bike, sample1 == TRUE)

#remaining 30% data which hasn't marked as TRUE fall under this
test <- subset(bike, sample1 == FALSE)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Model Building>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Building a Linear Regression between Total Bike Rental and (new_atemp/new_temp/new_hum/new_windspeed)
#Linear Regression Model
lm_model <- lm(train$cnt ~ (train$new_temp+train$new_atemp+train$windspeed+train$casual+train$registered))

#Summary of model
summary(lm_model)

#predictions
pred1 <- predict(lm_model, newdata = train)

error <- train[,'cnt'] - pred1

#Root Mean Squared Error
rmse <- sqrt(mean(error^2))
print(rmse)

#Looking at significance values of predictors
library(knitr)
kable(anova(lm_model), booktabs = T)

#plot
plot(lm_model, col="lightgreen", main = "Linear Regression: Bike Rentals, temp, atemp, windspeed, casual, registered")

#If we increase more features and see how it is
lm_model2 <- lm(train$cnt ~ (train$season+train$holiday+train$weathersit+train$temp+train$atemp+
                               train$casual+train$registered))

#Summary of model2
summary(lm_model2)

plot(lm_model2, col="lightblue", main="Linear Regression: Bike Rentals, season,holiday,weather,
     temp,atemp, casual, registered")

predicted <- predict(lm_model2, newdata = train)

error <- train[,'cnt'] - predicted

#Root Mean Squared Error
rmse <- sqrt(mean(error^2))
print(rmse)

#We can clearly observe that the lm_model2 is not that much performed as lm_model does.
#So, lm_model is the best fit for our data


#-------------------------Decision Tree-------------------------------------------
#Load CART packages
library(rpart)
#install rpart.plot
install.packages("rpart.plot")
library(rpart.plot)

#CART model
dt_model <- rpart(cnt ~ season + weathersit + new_temp + new_atemp + casual + registered, data = train)

#plot the tree using prp command defined in rpart.plot package
prp(dt_model)

#Predictions
tree.pred <- predict(dt_model, newdata = test)
tree.sse <- sum((tree.pred - test$cnt)^2)
tree.sse


#---------------------------Random Forest----------------------------------
library(randomForest)
set.seed(32)
rf <- randomForest(cnt ~ (season + weathersit + new_temp + new_atemp + casual + registered),data = bike, ntree=100)

print(rf)

#predictions
pred_rf <- predict(rf, newdata = test)
print(pred_rf)

summary(rf)



#Finally, we can say that Linear Regression is only the best fit for this data


