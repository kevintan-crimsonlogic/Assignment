# Day 4 Assignment Regression & Model Evaluation

# require libraries
require(dplyr)
require(gplots)
require(RColorBrewer)

# load libraries
library(dplyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(perturb)

# Set working directory
setwd("D:\\Data Science Training\\Core Level\\Day 4 - Regression Models\\Assignment")

# Question 1
# Red Wines - Load the dataset csv file
wine_red <- read.csv2('./winequality-red.csv',header=T,na.strings=c(""))

# Red Wines - List type of variables
sapply(wine_red,class) 

# Red Wines - Transform variable type from factor to numeric
wine_red$fixed.acidity <- as.numeric(levels(wine_red$fixed.acidity))[wine_red$fixed.acidity]
wine_red$volatile.acidity <- as.numeric(levels(wine_red$volatile.acidity))[wine_red$volatile.acidity]
wine_red$citric.acid <- as.numeric(levels(wine_red$citric.acid))[wine_red$citric.acid]
wine_red$residual.sugar <- as.numeric(levels(wine_red$residual.sugar))[wine_red$residual.sugar]
wine_red$chlorides <- as.numeric(levels(wine_red$chlorides))[wine_red$chlorides]
wine_red$free.sulfur.dioxide <- as.numeric(levels(wine_red$free.sulfur.dioxide))[wine_red$free.sulfur.dioxide]
wine_red$total.sulfur.dioxide <- as.numeric(levels(wine_red$total.sulfur.dioxide))[wine_red$total.sulfur.dioxide]
wine_red$density <- as.numeric(levels(wine_red$density))[wine_red$density]
wine_red$pH <- as.numeric(levels(wine_red$pH))[wine_red$pH]
wine_red$sulphates <- as.numeric(levels(wine_red$sulphates))[wine_red$sulphates]
wine_red$alcohol <- as.numeric(levels(wine_red$alcohol))[wine_red$alcohol]

# White Wines - Load the dataset csv file
wine_white <- read.csv2('./winequality-white.csv',header=T,na.strings=c(""))

# White Wines - List type of variables
sapply(wine_white,class) 

# White Wines - Transform variable type from factor to numeric
wine_white$fixed.acidity <- as.numeric(levels(wine_white$fixed.acidity))[wine_white$fixed.acidity]
wine_white$volatile.acidity <- as.numeric(levels(wine_white$volatile.acidity))[wine_white$volatile.acidity]
wine_white$citric.acid <- as.numeric(levels(wine_white$citric.acid))[wine_white$citric.acid]
wine_white$residual.sugar <- as.numeric(levels(wine_white$residual.sugar))[wine_white$residual.sugar]
wine_white$chlorides <- as.numeric(levels(wine_white$chlorides))[wine_white$chlorides]
wine_white$free.sulfur.dioxide <- as.numeric(levels(wine_white$free.sulfur.dioxide))[wine_white$free.sulfur.dioxide]
wine_white$total.sulfur.dioxide <- as.numeric(levels(wine_white$total.sulfur.dioxide))[wine_white$total.sulfur.dioxide]
wine_white$density <- as.numeric(levels(wine_white$density))[wine_white$density]
wine_white$pH <- as.numeric(levels(wine_white$pH))[wine_white$pH]
wine_white$sulphates <- as.numeric(levels(wine_white$sulphates))[wine_white$sulphates]
wine_white$alcohol <- as.numeric(levels(wine_white$alcohol))[wine_white$alcohol]

# Question 2
# Red Wines - Average quality
mean(c(wine_red$quality))
# Answer 5.636023

# White Wines - Average quality
mean(c(wine_white$quality))
# Answer 5.877909

# Question 3
# Red Wines - Split dataset into 80% training and 20% testing
# first 80% of 1599 rows (1279 rows) for training set
# last 20% of 1599 rows (320 rows) for testing set
wine_red_train <- wine_red[1:1279,]
wine_red_test <- wine_red[1280:1599,]

# White Wines - Split dataset into 80% training and 20% testing
# first 80% of 4898 rows (3918 rows) for training set
# last 20% of 4898 rows (980 rows) for testing set
wine_white_train <- wine_white[1:3918,]
wine_white_test <- wine_white[3919:4898,]

# Question 4
# Red Wines - Multiple regression model - multi-factor / predictors
wine_red_fit <- lm(quality ~ ., data = wine_red_train)
summary(wine_red_fit)

# Red Wines - Using step function to step through the predictors
wine_red_fit_formula <- formula(wine_red_fit)
wine_red_step.model = step(wine_red_fit, direction='both', scope=wine_red_fit_formula)
summary(wine_red_step.model)

# Red Wines - Using function stepAIC to determine the best combination of predictors
wine_red_step <- stepAIC(wine_red_fit, direction="both")
wine_red_step$anova # display results
summary(wine_red_step)

# Red Wines - Adjusted R Squared
summary(wine_red_step)$adj.r.square
# Answer 0.3651379

# White Wines - Multiple regression model - multi-factor / predictors
wine_white_fit <- lm(quality ~ ., data = wine_white_train)
summary(wine_white_fit)

# White Wines - Using step function to step through the predictors
wine_white_fit_formula <- formula(wine_white_fit)
wine_white_step.model = step(wine_white_fit, direction='both', scope=wine_white_fit_formula)
summary(wine_white_step.model)

# White Wines - Using function stepAIC to determine the best combination of predictors
wine_white_step <- stepAIC(wine_white_fit, direction="both")
wine_white_step$anova # display results
summary(wine_white_step)

# White Wines - Adjusted R Squared
summary(wine_white_step)$adj.r.square
# Answer 0.2977597

# Question 5
# Red Wines - Alternate to Variance Inflationary Factors, we can use the function colldiag
print(colldiag(wine_red_train),fuzz=.3)
# Answer Yes (fixed.acidity, density and alcohol)
# Answer alcohol is already included in the model
# Answer fixed.acidity and density were added and removed due to reduced adjusted r-squared value

# White Wines - Alternate to Variance Inflationary Factors, we can use the function colldiag
print(colldiag(wine_white_train),fuzz=.3)
# Answer Yes (residual.sugar, density and alcohol)
# Answer residual.sugar, density and alcohol are already included in the model

# Question 6
# Similarities and differences between red and white wine models
# Answer Quality of both wines are affected by volatile.acidity, free.sulfur.dioxide, pH, sulphates and alcohol
# Answer Quality of red wines are affected by chlorides and total.sulfur.dioxide but not for white wines
# Answer Quality of white wines are affected by fixed.acidity, residual.sugar and density but not for red wines

# Question 7
# Red Wines - Predict wine quality
wine_red_result <- predict(wine_red_step.model,newdata=wine_red_test)
wine_red_result <- as.integer(wine_red_result)
wine_red_error <- mean(wine_red_result != wine_red_test$quality)
print(paste('Accuracy',1-wine_red_error))

# White Wines - Predict wine quality
wine_white_result <- predict(wine_white_step.model,newdata=wine_white_test)
wine_white_result <- as.integer(wine_white_result)
wine_white_error <- mean(wine_white_result != wine_white_test$quality)
print(paste('Accuracy',1-wine_white_error))

# Question 8
# Red wines - RMSE value
wine_red_quality <- wine_red_test$quality
rmse(wine_red_result, wine_red_quality)
# Answer 0.8177714 (very high error rate)

# White wines - RMSE value
wine_white_quality <- wine_white_test$quality
rmse(wine_white_result, wine_white_quality)
# Answer 0.8024198 (very high error rate)

# Question 9
# Red Wines - R-squared value for test set
wine_red_fit_2 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = wine_red_test)
summary(wine_red_fit_2)$r.square
# Answer 0.3619674

# White Wines - R-squared value for test set
wine_white_fit_2 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = wine_white_test)
summary(wine_white_fit_2)$r.square
# Answer 0.2235977

# Answer Both models are equally not accurate
# Answer Increase dataset to improve accuracy
