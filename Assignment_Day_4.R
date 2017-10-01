# set working directory
setwd("D:\\Data Science Training\\Core Level\\Day 4 - Regression Models\\Assignment")

# Question 1
# read red wines dataset
wine_red <- read.csv2("winequality-red.csv")

# read white wines dataset
wine_white <- read.csv2("winequality-white.csv")

# Question 2
# average quality for red wines
mean(c(wine_red$quality))
# Answer 5.636023

# average quality for white wines
mean(c(wine_white$quality))
# Answer 5.877909

# Question 3
# split 80% red wines dataset into training set
# calculate total rows for red wines dataset
n_red <- nrow(wine_red)

# split first 80% of rows into training set
wine_red_training <- wine_red[1:round(0.8 * n_red), ]

# split 20% red wines dataset into testing set
# split the remaining 20% of rows into testing set
wine_red_testing <- wine_red[(round(0.8 * n_red) + 1):n_red, ]

# split 80% white wines dataset into training set
# calculate total rows for white wines dataset
n_white <- nrow(wine_white)

# split first 80% of rows into training set
wine_white_training <- wine_white[1:round(0.8 * n_white), ]

# split 20% white wines dataset into testing set
# split the remaining 20% of rows into testing set
wine_white_testing <- wine_white[(round(0.8 * n_white) + 1):n_white, ]

# Question 4
# rought plot of all paris of data
pairs(wine_red)

# multiple regression model - multi-factor / predictors
fit <- lm(quality ~ ., data = wine_red)
summary(fit)