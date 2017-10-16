# Lecture Example on Logistic Regression

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

setwd("D:\\Data Science Training\\Core Level\\Day 5 - Model Evaluation\\Assignment\\")

# Question 1
# Load the training csv file
train_raw <- read.csv('train.csv',header=T,na.strings=c(""))
test_raw <- read.csv('test.csv',header=T,na.strings=c(""))

# Question 2
# Data transformation for train set
train_set1 <- subset(train_raw, select = c(duration))
protocol_type <- model.matrix(~ protocol_type - 1, data=train_raw)
#service <- model.matrix(~ service - 1, data=train_raw)
train_set1 <- cbind(train_set1, protocol_type)
train_set1 <- cbind(train_set1, subset(train_raw, select = c(src_bytes:dst_host_srv_rerror_rate)))
intrusion <- ifelse(train_raw$outcome=='normal', 0, 1)
train_set1 <- cbind(train_set1, intrusion)

# Data transformation for test set
test_set1 <- subset(test_raw, select = c(duration))
protocol_type <- model.matrix(~ protocol_type - 1, data=test_raw)
#service <- model.matrix(~ service - 1, data=test_raw)
test_set1 <- cbind(test_set1, protocol_type)
test_set1 <- cbind(test_set1, subset(test_raw, select = c(src_bytes:dst_host_srv_rerror_rate)))
intrusion <- ifelse(test_raw$outcome=='normal', 0, 1)
test_set1 <- cbind(test_set1, intrusion)

# create a logistic regression model with the training data
# target is intrusion, predictor variables are all values
model <- glm(intrusion ~., family=binomial(link='logit'), data=train_set1)
# review the model
summary(model)

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
#anova(model, test="Chisq")

# Data transformation for train set
train_set2 <- subset(train_set1, select = -c(protocol_typeudp, num_failed_logins, num_compromised, su_attempted, count, srv_count, same_srv_rate, diff_srv_rate, dst_host_same_srv_rate, dst_host_diff_srv_rate, dst_host_srv_rerror_rate))

# Data transformation for train set
test_set2 <- subset(test_set1, select = -c(protocol_typeudp, num_failed_logins, num_compromised, su_attempted, count, srv_count, same_srv_rate, diff_srv_rate, dst_host_same_srv_rate, dst_host_diff_srv_rate, dst_host_srv_rerror_rate))

# create a logistic regression model with the training data
# target is intrusion, predictor variables are all values
model <- glm(intrusion ~., family=binomial(link='logit'), data=train_set2)
# review the model
summary(model)

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
#anova(model, test="Chisq")

# Data transformation for train set
train_set3 <- subset(train_set2, select = -c(root_shell, num_root, is_guest_login, serror_rate, dst_host_srv_count))

# Data transformation for train set
test_set3 <- subset(test_set2, select = -c(root_shell, num_root, is_guest_login, serror_rate, dst_host_srv_count))

# Question 3
# create a logistic regression model with the training data
# target is intrusion, predictor variables are all values
model <- glm(intrusion ~., family=binomial(link='logit'), data=train_set3)
# review the model
summary(model)

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
#anova(model, test="Chisq")

# Question 4
# use the model to predict for test set
fitted.results <- predict(model, test_set3, type='response')
# the results of the model is a probability value and must be mapped to 1 or 0
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
actual.results <- test_set3$intrusion

# determine the mis-classification error
misClasificError <- mean(fitted.results != test_set3$intrusion)

# Question 4
# calculate the accuracy
print(paste('Accuracy',1-misClasificError))
# Answer Accuracy = 0.814065180102916

# Confusion matrix
conf_matrix<-table(fitted.results, actual.results)
conf_matrix

# calculate the specificity
library(caret)
print(paste('Specificity',specificity(conf_matrix)))
# Answer Specificity = 0.73179347826087

# calculate the sensitivity
print(paste('Sensitivity',sensitivity(conf_matrix)))
# Answer Sensitivity = 0.954883720930233

# Question 5
# Answer: 2053 / (2053 + 987) = 0.6753

# Question 6
# Answer: D

# Question 7
# ROC Curve
library(pROC)
fitted.results <- predict(model, test_set3, type='response')
roccurve <- roc(intrusion ~ fitted.results, data = test_set3)
plot(roccurve)

# AUC value
auc(roccurve)
# Answer: AUC value = 0.7615
