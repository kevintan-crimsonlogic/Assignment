# Question 1
mydata <- read.csv("D:\\Data Science Training\\Core Level\\Day 1 - Introduction\\Assignment\\35478-0001-Data.csv")
cols <- (colnames(mydata)=="AGE" | colnames(mydata)=="DEGREE" | colnames(mydata)=="EDUC" | colnames(mydata)=="HAPPY" | colnames(mydata)=="HOMPOP" | colnames(mydata)=="INCOME" | colnames(mydata)=="MARITAL" | colnames(mydata)=="RACE" | colnames(mydata)=="RINCOME" | colnames(mydata)=="SEX")
subset_mydata <- subset(mydata, , cols)
nrow(subset_mydata)
# Answer = 4820

# Question 2
age_unique <- unique(subset_mydata$AGE)
age_missing <- subset(age_unique, age_unique>89)
age_missing
# Answer = 99 (No asnwer)

degree_unique <- unique(subset_mydata$DEGREE)
degree_missing <- subset(degree_unique, degree_unique>4)
degree_missing
# Answer = Nil

educ_unique <- unique(subset_mydata$EDUC)
educ_missing <- subset(educ_unique, educ_unique>20)
educ_missing
# Answer = 98 (Don't know), 99 (No answer)

happy_unique <- unique(subset_mydata$HAPPY)
happy_missing <- subset(happy_unique, happy_unique>3)
happy_missing
# Answer = 8 (Don't know), 9 (No answer)

hompop_unique <- unique(subset_mydata$HOMPOP)
hompop_missing <- subset(hompop_unique, hompop_unique>10)
hompop_missing
# Answer = 99 (No answer)

income_unique <- unique(subset_mydata$INCOME)
income_missing <- subset(income_unique, income_unique>12)
income_missing
# Answer = 13 (Refused), 98 (Don't know)

marital_unique <- unique(subset_mydata$MARITAL)
marital_missing <- subset(marital_unique, marital_unique>5)
marital_missing
# Answer = 9 (No answer)

race_unique <- unique(subset_mydata$RACE)
race_missing <- subset(race_unique, race_unique>3)
race_missing
# Answer = Nil

rincome_unique <- unique(subset_mydata$RINCOME)
rincome_missing <- subset(rincome_unique, rincome_unique<1 | rincome_unique>12)
rincome_missing
# Answer = 0 (Inapplicable), 13 (Refused), 98 (Don't know)

sex_unique <- unique(subset_mydata$SEX)
sex_missing <- subset(sex_unique, sex_unique>2)
sex_missing
# Answer = Nil

# Question 3
subset2_mydata <- subset_mydata[subset_mydata$AGE<90 & subset_mydata$DEGREE<5  & subset_mydata$EDUC<21  & subset_mydata$HAPPY<4  & subset_mydata$HOMPOP<11  & subset_mydata$INCOME<13  & subset_mydata$MARITAL<6  & subset_mydata$RACE<4  & subset_mydata$RINCOME>0  & subset_mydata$RINCOME<13  & subset_mydata$SEX<3,]
nrow(subset2_mydata)
#Answer = 2749

# Question 4
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==1] <- 0
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==2] <- 1000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==3] <- 3000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==4] <- 4000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==5] <- 5000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==6] <- 6000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==7] <- 7000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==8] <- 8000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==9] <- 10000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==10] <- 15000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==11] <- 20000
subset2_mydata$PERCAPITA[subset2_mydata$INCOME==12] <- 25000
subset2_mydata$PERCAPITA <- subset2_mydata$PERCAPITA / subset2_mydata$HOMPOP

# Question 5
summary(subset2_mydata$PERCAPITA)
# Answer
# Minimum = $0, Maximum = $25000, Median = 8333, Mean = $10960
# 25 percentile = $6250, 75 percentile = $6250

# Question 6
sum(subset_mydata$DEGREE>2)/sum(subset_mydata$DEGREE<5)*100
#Answer = 30.08%

# Question 7
subset2_mydata$AGE[subset2_mydata$AGE>=18 & subset2_mydata$AGE<=35] <- "Young"
subset2_mydata$AGE[subset2_mydata$AGE>=36 & subset2_mydata$AGE<=55] <- "Middle"
subset2_mydata$AGE[subset2_mydata$AGE>=56 & subset2_mydata$AGE<=89] <- "Old"

# Question 8
Q8 <- table(subset2_mydata$HAPPY,subset2_mydata$AGE)
prop.table(Q8, 2)
#Answer = Yes

# Question 9
Q9 <- table(subset2_mydata$DEGREE,subset2_mydata$RINCOME)
Q9
#Answer = No

# Question 10
Q10 <- table(subset2_mydata$RACE,subset2_mydata$RINCOME)
Q10
#Answer = White
