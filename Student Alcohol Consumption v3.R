#                              John Scott Monger
#                                 July 2016
#                             Ryerson University
#                            CKME 136 - Capstone
#            Predicting Secondary School Student Alcohol Consumption
#                               Project Code
#
# Data Source: https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION
#-------------------------------------------------------------------------------------
#-------------------------------Load Data Sets and Initial Exploration----------------
# Set working directory
setwd("C:/Users/DAD-LAPTOP/Dropbox/Education/Ryerson/Certificate in Data Analytics/CKME 136 - Capstone/Student Alcohol Consumption")

# Import the math course student data
math_url <- "./_Student Data/student-mat.csv"
math <- read.csv(math_url,sep=";")

# Import the Portuguese course student data
port_url <- "./_Student Data/student-por.csv"
port <- read.csv(port_url,sep=";")
  
# determine basic structure and summary of math and port data sets
str(math)
head(math)
tail(math)
summary(math)

str(port)
head(port)
tail(port)
summary(port)

# Merge two classes?
students <- merge(math, port, by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime","studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic","famrel","freetime","goout","Dalc","Walc","health"),all=TRUE,suffixes=c(".mat", ".por"))
nrow(students) # 882 students

nrow(students[is.na(students$G3.mat),]) #487 port only
nrow(students[is.na(students$G3.por),]) #233 math only, so only 162 in common

#------------------------------- Categorical Data Preprocessing----------------------------
# Drop mid-term grades - only interested in final grade
math$G1 <- NULL
math$G2 <- NULL
port$G1 <- NULL
port$G2 <- NULL

# Add average alcohol columns - weight workday 5/7 and weekend 2/7
math$alc <- round(((math$Dalc * 5) + (math$Walc * 2))/7)
port$alc <- round(((port$Dalc * 5) + (port$Walc * 2))/7)

# Factors: fix categorical and ordinal factors from input defaults (no ordinal for later SMOTE)
math$famsize<-factor(math$famsize, order=FALSE, levels = c("LE3", "GT3"))

math$Medu<-factor(math$Medu, order=FALSE, levels = c("0", "1", "2", "3", "4"))
math$Fedu<-factor(math$Fedu, order=FALSE, levels = c("0", "1", "2", "3", "4"))
levels(math$Medu) <- c("none", "primary_to_4th", "primary_to_9th", "secondary", "higher")
levels(math$Fedu) <- c("none", "primary_to_4th", "primary_to_9th", "secondary", "higher")

math$traveltime<-factor(math$traveltime, order=FALSE, levels = c("1", "2", "3", "4"))
math$studytime<-factor(math$studytime, order=FALSE, levels = c("1", "2", "3", "4"))
levels(math$traveltime) <- c("<15 min.", "15 to 30 min.", "30 min. to 1 hour", ">1 hour")
levels(math$studytime) <- c("<2 hours", "2 to 5 hours", "5 to 10 hours", ">10 hours")

math$famrel<-factor(math$famrel, order=FALSE, levels = c("1", "2", "3", "4", "5"))
math$freetime<-factor(math$freetime, order=FALSE, levels = c("1", "2", "3", "4", "5"))
math$goout<-factor(math$goout, order=FALSE, levels = c("1", "2", "3", "4", "5"))
math$health<-factor(math$health, order=FALSE, levels = c("1", "2", "3", "4", "5"))
math$Dalc<-factor(math$Dalc, order=FALSE, levels = c("1", "2", "3", "4", "5"))
math$Walc<-factor(math$Walc, order=FALSE, levels = c("1", "2", "3", "4", "5"))
math$alc<-factor(math$alc, order=FALSE, levels = c("1", "2", "3", "4", "5"))

port$famsize<-factor(port$famsize, order=FALSE, levels = c("LE3", "GT3"))

port$Medu<-factor(port$Medu, order=FALSE, levels = c("0", "1", "2", "3", "4"))
port$Fedu<-factor(port$Fedu, order=FALSE, levels = c("0", "1", "2", "3", "4"))
levels(port$Medu) <- c("none", "primary_to_4th", "primary_to_9th", "secondary", "higher")
levels(port$Fedu) <- c("none", "primary_to_4th", "primary_to_9th", "secondary", "higher")

port$traveltime<-factor(port$traveltime, order=FALSE, levels = c("1", "2", "3", "4"))
port$studytime<-factor(port$studytime, order=FALSE, levels = c("1", "2", "3", "4"))
levels(port$traveltime) <- c("<15 min.", "15 to 30 min.", "30 min. to 1 hour", ">1 hour")
levels(port$studytime) <- c("<2 hours", "2 to 5 hours", "5 to 10 hours", ">10 hours")

port$famrel<-factor(port$famrel, order=FALSE, levels = c("1", "2", "3", "4", "5"))
port$freetime<-factor(port$freetime, order=FALSE, levels = c("1", "2", "3", "4", "5"))
port$goout<-factor(port$goout, order=FALSE, levels = c("1", "2", "3", "4", "5"))
port$health<-factor(port$health, order=FALSE, levels = c("1", "2", "3", "4", "5"))
port$Dalc<-factor(port$Dalc, order=FALSE, levels = c("1", "2", "3", "4", "5"))
port$Walc<-factor(port$Walc, order=FALSE, levels = c("1", "2", "3", "4", "5"))
port$alc<-factor(port$alc, order=FALSE, levels = c("1", "2", "3", "4", "5"))

#-------------------------------Numeric Data Exploration & Preprocessing-------------------
# revisit basic structure and summary of math and port data sets
str(math)
head(math)
tail(math)
summary(math)

str(port)
head(port)
tail(port)
summary(port)

# Investigate ages
prop.table(table(math$age))
hist(math$age, main="Math Class: Age Histogram", xlab="Age")
prop.table(table(port$age))
hist(port$age, main="Portuguese Class: Age Histogram", xlab="Age")

# Boxplot age by class
boxplot(math$age, port$age, ylab="Class", xlab="Age", ylim=c(10, max(math$age)), horizontal = TRUE, main="Age Distributions by Class", col=c("green", "red"))
axis(2, 1:2, c("Math", "Portuguese"))

# Collapse 19+ into 1 Bucket and create factors for age brackets
math$age2 <- 5
math$age2[math$age == 15] <- 1
math$age2[math$age == 16] <- 2
math$age2[math$age == 17] <- 3
math$age2[math$age == 18] <- 4
math$age2 <- factor(math$age2, order=FALSE, levels = c("1", "2", "3", "4", "5"))
levels(math$age2) <- c("15", "16", "17", "18", "19+")
plot(math$age2, xlab = "Age", ylab = "Frequency", main = "Math Class: Histogram of Age")

port$age2 <- 5
port$age2[port$age == 15] <- 1
port$age2[port$age == 16] <- 2
port$age2[port$age == 17] <- 3
port$age2[port$age == 18] <- 4
port$age2 <- factor(port$age2, order=FALSE, levels = c("1", "2", "3", "4", "5"))
levels(port$age2) <- c("15", "16", "17", "18", "19+")
plot(port$age2, xlab = "Age", ylab = "Frequency", main = "Portuguese Class: Histogram of Age")

# Barplot new age variable by class
rbind(prop.table(table(math$age2)), prop.table(table(port$age2)))
barplot(rbind(prop.table(table(math$age2)), prop.table(table(port$age2))), beside = TRUE, xlab = "Age Band", ylab = "Proportion of Respondents", legend = c("Math", "Portuguese"), main = "Age Bands by Class", ylim = c(0, .8), args.legend = list(title = "Class", x = "topright"), col=c("red", "green"))

# Boxplot age by gender
#boxplot(math[math$sex == "M",]$age, math[math$sex=="F",]$age, ylab="Gender", xlab="Age", main="Math: Age Distributions by Gender", col=c("green", "red"), horizontal = TRUE)
#axis(2, 1:2, c("M", "F"))
#boxplot(port[port$sex == "M",]$age, port[port$sex=="F",]$age, ylab="Gender", xlab="Age", main="Portuguese: Age Distributions by Gender", col=c("green", "red"), horizontal = TRUE)
#axis(2, 1:2, c("M", "F"))
boxplot(math[math$sex == "M",]$age, math[math$sex=="F",]$age, port[port$sex == "M",]$age, port[port$sex=="F",]$age, xlab="Age", main="Class Age Distributions by Gender", col=c("green", "red", "blue", "yellow"), horizontal = TRUE)
par(las=1)
axis(2, 1:4, c("Math M", "Math F", "Port M", "Port F"))
par(las=0)

# Boxplot age by goout
boxplot(math[math$goout == 1,]$age, math[math$goout==2,]$age, math[math$goout == 3,]$age, math[math$goout==4,]$age, math[math$goout==5,]$age, xlab="Going Out With Friends Rating", ylab="Age", ylim=c(10, max(math$age)), main="Math: Age Distributions by Going Out With Friends Rating", col=c("green", "red", "blue", "yellow", "purple"))
axis(1, 1:5, c(1:5))
boxplot(port[port$goout == 1,]$age, port[port$goout==2,]$age, port[port$goout == 3,]$age, port[port$goout==4,]$age, port[port$goout==5,]$age, xlab="Going Out With Friends Rating", ylab="Age", ylim=c(10, max(port$age)), main="Portuguese: Age Distributions by Going Out With Friends Rating", col=c("green", "red", "blue", "yellow", "purple"))
axis(1, 1:5, c(1:5))

# Barplot age2 by goout
table(math$goout, math$age2)
prop.table(table(math$goout, math$age2), margin=1)
barplot(prop.table(table(math$goout, math$age2), margin=1), beside = TRUE, xlab = "Age Band", ylab = "Proportion of Goout Rating Respondents", legend = c("1", "2", "3", "4", "5"), main = "Math Class: Bar Chart of Ages by Going Out Rating", ylim = c(0, .4), args.legend = list(title = "Going Out", x = "topright"), col=rainbow(5))
table(port$goout, port$age2)
prop.table(table(port$goout, port$age2), margin=1)
barplot(prop.table(table(port$goout, port$age2), margin=1), beside = TRUE, xlab = "Age Band", ylab = "Proportion of Goout Rating Respondents", legend = c("1", "2", "3", "4", "5"), main = "Portuguese Class: Bar Chart of Ages by Going Out Rating", ylim = c(0, .4), args.legend = list(title = "Going Out", x = "topright"), col=rainbow(5))

# Investigate failures
prop.table(table(math$failures)) # 21% with previous failures
hist(math$failures, main="Math Failures", xlab="Failures")
prop.table(table(port$failures)) # 15% with previous failures
hist(port$failures, main="Portuguese Failures", xlab="Failures")

# Investigate G3
prop.table(table(math$G3))
hist(math$G3, main="Math G3 Grade", xlab="G3 Grade", col="green")
prop.table(table(port$G3))
hist(port$G3, main="Portuguese G3 Grade", xlab="G3 Grade", col="red")
length(math$G3[math$G3<10])/nrow(math) # 33% current class failure rate
length(port$G3[port$G3<10])/nrow(port) # 15% current class failure rate

# Boxplot G3 by gender
boxplot(math[math$sex == "M",]$G3, math[math$sex=="F",]$G3, xlab="Gender", ylab="G3: Final Grade", ylim=c(0, max(math$G3)), main="Math: G3: Final Grade Distributions by Gender", col=c("green", "red"))
axis(1, 1:2, c("M", "F"))
boxplot(port[port$sex == "M",]$G3, port[port$sex=="F",]$G3, xlab="Gender", ylab="G3: Final Grade", ylim=c(0, max(port$G3)), main="Portuguese: G3: Final Grade Distributions by Gender", col=c("green", "red"))
axis(1, 1:2, c("M", "F"))

# Boxplot G3 by goout
boxplot(math[math$goout == 1,]$G3, math[math$goout==2,]$G3, math[math$goout == 3,]$G3, math[math$goout==4,]$G3, math[math$goout==5,]$G3, xlab="Going Out With Friends Rating", ylab="G3: Final Grade", ylim=c(0, max(math$G3)), main="Math: G3-Final Grade Distributions by Going Out With Friends Rating", col=rainbow(5))
axis(1, 1:5, c(1:5))
boxplot(port[port$goout == 1,]$G3, port[port$goout==2,]$G3, port[port$goout == 3,]$G3, port[port$goout==4,]$G3, port[port$goout==5,]$G3, xlab="Going Out With Friends Rating", ylab="G3: Final Grade", ylim=c(0, max(port$G3)), main="Portuguese: G3-Final Grade Distributions by Going Out With Friends Rating", col=rainbow(5))
axis(1, 1:5, c(1:5))

# Collapse G3 into 5 factors for letter grades
math$G3_2 <- 5
math$G3_2[math$G3 > 15] <- 1
math$G3_2[math$G3 == 14 | math$G3 == 15] <- 2
math$G3_2[math$G3 == 12 | math$G3 == 13] <- 3
math$G3_2[math$G3 == 10 | math$G3 == 11] <- 4
math$G3_2 <- factor(math$G3_2, order=FALSE, levels = c("1", "2", "3", "4", "5"))
levels(math$G3_2) <- c("A", "B", "C", "D", "F")
prop.table(table(math$G3_2))
plot(math$G3_2, xlab = "Letter Grade", ylab = "Frequency", main = "Math Class: Histogram of Letter Grades", col="green")

port$G3_2 <- 5
port$G3_2[port$G3 > 15] <- 1
port$G3_2[port$G3 == 14 | port$G3 == 15] <- 2
port$G3_2[port$G3 == 12 | port$G3 == 13] <- 3
port$G3_2[port$G3 == 10 | port$G3 == 11] <- 4
port$G3_2 <- factor(port$G3_2, order=FALSE, levels = c("1", "2", "3", "4", "5"))
levels(port$G3_2) <- c("A", "B", "C", "D", "F")
plot(port$G3_2, xlab = "Letter Grade", ylab = "Frequency", main = "Portuguese Class: Histogram of Letter Grades", col="red")

# Investigate failures/ G3 correlations
plot(math$failures, math$G3)
plot(port$failures, port$G3)
cor(math$failures, math$G3, method = "spearman") # -0.3612235
cor(port$failures, port$G3, method = "spearman") # -0.4483603
cor(math$failures, math$G3, method = "pearson") # -0.3604149
cor(port$failures, port$G3, method = "pearson") # -0.3933155

# Add failures2 - binary feature including G3 result
math$failures2 <- 0
math$failures2[math$failures > 0] <- 1
math$failures2[math$G3 < 10] <- 1
prop.table(table(math$failures2)) # now 41% failures up from 21% pre G3

port$failures2 <- 0
port$failures2[port$failures > 0] <- 1
port$failures2[port$G3 < 10] <- 1
prop.table(table(port$failures2)) # now 23% failures up from 15% pre G3

# Investigate absences
prop.table(table(math$absences))
hist(math$absences, main="Math Absences", xlab="Absences", col="green")
summary(math$absences)
mean(math$absences)
prop.table(table(port$absences))
hist(port$absences, main="Portuguese Absences", xlab="Absences", col="red")
summary(port$absences)
mean(port$absences)

# Boxplot absences by gender
boxplot(math[math$sex == "M",]$absences, math[math$sex=="F",]$absences, xlab="Gender", ylab="Absences", ylim=c(0, max(math$absences)), main="Math: Absences Distributions by Gender", col=c("green", "red"))
axis(1, 1:2, c("M", "F"))
boxplot(port[port$sex == "M",]$absences, port[port$sex=="F",]$absences, xlab="Gender", ylab="Absences", ylim=c(0, max(port$absences)), main="Portuguese: Absences Distributions by Gender", col=c("green", "red"))
axis(1, 1:2, c("M", "F"))

# Boxplot absences by goout
boxplot(math[math$goout == 1,]$absences, math[math$goout==2,]$absences, math[math$goout == 3,]$absences, math[math$goout==4,]$absences, math[math$goout==5,]$absences, xlab="Going Out With Friends Rating", ylab="Absences", ylim=c(0, max(math$absences)), main="Math: Absences Distributions by Going Out With Friends Rating", col=rainbow(5))
axis(1, 1:5, c(1:5))
boxplot(port[port$goout == 1,]$absences, port[port$goout==2,]$absences, port[port$goout == 3,]$absences, port[port$goout==4,]$absences, port[port$goout==5,]$absences, xlab="Going Out With Friends Rating", ylab="Absences", ylim=c(0, max(port$absences)), main="Portuguese: Absences Distributions by Going Out With Friends Rating", col=rainbow(5))
axis(1, 1:5, c(1:5))

# Investigate alc ratings with tables and simple plots
prop.table(table(math$Dalc))
plot(math$Dalc, xlab = "Workday Alcohol Consumption Rating", ylab = "Frequency", main = "Math Class: Histogram of Workday Alcohol Consumption", col="green")
prop.table(table(math$Walc))
plot(math$Walc, xlab = "Weekend Alcohol Consumption Rating", ylab = "Frequency", main = "Math Class: Histogram of Weekend Alcohol Consumption", col="green")
prop.table(table(math$alc))
plot(math$alc, xlab = "Combined Alcohol Consumption Rating", ylab = "Frequency", main = "Math Class: Histogram of Combined Alcohol Consumption", col="green")

prop.table(table(port$Dalc))
plot(port$Dalc, xlab = "Workday Alcohol Consumption Rating", ylab = "Frequency", main = "Portuguese Class: Histogram of Workday Alcohol Consumption", col="red")
prop.table(table(port$Walc))
plot(port$Walc, xlab = "Weekend Alcohol Consumption Rating", ylab = "Frequency", main = "Portuguese Class: Histogram of Weekend Alcohol Consumption", col="red")
prop.table(table(port$alc))
plot(port$alc, xlab = "Combined Alcohol Consumption Rating", ylab = "Frequency", main = "Portuguese Class: Histogram of Combined Alcohol Consumption", col="red")

# Investigate alc ratings by gender with tables and bar charts
prop.table(table(math$sex, math$Dalc), margin=1)
barplot(prop.table(table(math$sex, math$Dalc), margin=1), beside = TRUE, xlab = "Workday Alcohol Consumption Rating", ylab = "Proportion of Gender Respondents", legend = unique(math$sex), main = "Math Class: Bar Chart of Workday Alcohol Consumption by Gender", ylim = c(0, .8), args.legend = list(title = "Gender", x = "topright"), col=c("red", "green"))
prop.table(table(math$sex, math$Walc), margin=1)
barplot(prop.table(table(math$sex, math$Walc), margin=1), beside = TRUE, xlab = "Weekend Alcohol Consumption Rating", ylab = "Proportion of Gender Respondents", legend = unique(math$sex), main = "Math Class: Bar Chart of Weekend Alcohol Consumption by Gender", ylim = c(0, .8), args.legend = list(title = "Gender", x = "topright"), col=c("red", "green"))
prop.table(table(math$sex, math$alc), margin=1)
barplot(prop.table(table(math$sex, math$alc), margin=1), beside = TRUE, xlab = "Combined Alcohol Consumption Rating", ylab = "Proportion of Gender Respondents", legend = unique(math$sex), main = "Math Class: Bar Chart of Combined Alcohol Consumption by Gender", ylim = c(0, .8), args.legend = list(title = "Gender", x = "topright"), col=c("red", "green"))

prop.table(table(port$sex, port$Dalc), margin=1)
barplot(prop.table(table(port$sex, port$Dalc), margin=1), beside = TRUE, xlab = "Workday Alcohol Consumption Rating", ylab = "Proportion of Gender Respondents", legend = unique(port$sex), main = "Portuguese Class: Bar Chart of Workday Alcohol Consumption by Gender", ylim = c(0, .8), args.legend = list(title = "Gender", x = "topright"), col=c("red", "green"))
prop.table(table(port$sex, port$Walc), margin=1)
barplot(prop.table(table(port$sex, port$Walc), margin=1), beside = TRUE, xlab = "Weekend Alcohol Consumption Rating", ylab = "Proportion of Gender Respondents", legend = unique(port$sex), main = "Portuguese Class: Bar Chart of Weekend Alcohol Consumption by Gender", ylim = c(0, .8), args.legend = list(title = "Gender", x = "topright"), col=c("red", "green"))
prop.table(table(port$sex, port$alc), margin=1)
barplot(prop.table(table(port$sex, port$alc), margin=1), beside = TRUE, xlab = "Combined Alcohol Consumption Rating", ylab = "Proportion of Gender Respondents", legend = unique(port$sex), main = "Portuguese Class: Bar Chart of Combined Alcohol Consumption by Gender", ylim = c(0, .8), args.legend = list(title = "Gender", x = "topright"), col=c("red", "green"))

# Investigate alc ratings by goout with tables and bar charts
prop.table(table(math$goout, math$Dalc), margin=1)
barplot(prop.table(table(math$goout, math$Dalc), margin=1), beside = TRUE, xlab = "Workday Alcohol Consumption Rating", ylab = "Proportion of Go Out Respondents", legend = sort(unique(math$goout)), main = "Math Class: Bar Chart of Workday Alcohol Consumption by Going Out Rating", ylim = c(0, .9), args.legend = list(title = "Go Out Rating", x = "topright"), col=rainbow(5))
prop.table(table(math$goout, math$Walc), margin=1)
barplot(prop.table(table(math$goout, math$Walc), margin=1), beside = TRUE, xlab = "Weekend Alcohol Consumption Rating", ylab = "Proportion of Go Out Respondents", legend = sort(unique(math$goout)), main = "Math Class: Bar Chart of Weekend Alcohol Consumption by Going Out Rating", ylim = c(0, .9), args.legend = list(title = "Go Out Rating", x = "topright"), col=rainbow(5))
prop.table(table(math$goout, math$alc), margin=1)
barplot(prop.table(table(math$goout, math$alc), margin=1), beside = TRUE, xlab = "Average Alcohol Consumption Rating", ylab = "Proportion of Go Out Respondents", legend = sort(unique(math$goout)), main = "Math Class: Bar Chart of Average Alcohol Consumption by Going Out Rating", ylim = c(0, .9), args.legend = list(title = "Go Out Rating", x = "topright"), col=rainbow(5))

prop.table(table(port$goout, port$Dalc), margin=1)
barplot(prop.table(table(port$goout, port$Dalc), margin=1), beside = TRUE, xlab = "Workday Alcohol Consumption Rating", ylab = "Proportion of Go Out Respondents", legend = sort(unique(port$goout)), main = "Portuguese Class: Bar Chart of Workday Alcohol Consumption by Going Out Rating", ylim = c(0, .9), args.legend = list(title = "Go Out Rating", x = "topright"), col=rainbow(5))
prop.table(table(port$goout, port$Walc), margin=1)
barplot(prop.table(table(port$goout, port$Walc), margin=1), beside = TRUE, xlab = "Weekend Alcohol Consumption Rating", ylab = "Proportion of Go Out Respondents", legend = sort(unique(port$goout)), main = "Portuguese Class: Bar Chart of Weekend Alcohol Consumption by Going Out Rating", ylim = c(0, .9), args.legend = list(title = "Go Out Rating", x = "topright"), col=rainbow(5))
prop.table(table(port$goout, port$alc), margin=1)
barplot(prop.table(table(port$goout, port$alc), margin=1), beside = TRUE, xlab = "Average Alcohol Consumption Rating", ylab = "Proportion of Go Out Respondents", legend = sort(unique(port$goout)), main = "Port. Class: Bar Chart of Average Alcohol Consumption by Go Out Rating", ylim = c(0, .9), args.legend = list(title = "Go Out Rating", x = "topright"), col=rainbow(5))

# Add predictive binary alcohol columns
math$drinker <- 0
math$drinker[math$alc == 3 | math$alc == 4 | math$alc == 5] <- 1
port$drinker <- 0
port$drinker[port$alc == 3 | port$alc == 4 | port$alc == 5] <- 1
  
# Investigate drinkers - 18% drinkers in both data sets
table(math$drinker)
prop.table(table(math$drinker))

table(port$drinker)
prop.table(table(port$drinker))

aggregate(drinker ~ sex + goout, data=math, FUN=function(x) {sum(x)/length(x)})
aggregate(drinker ~ sex + goout, data=port, FUN=function(x) {sum(x)/length(x)})
aggregate(drinker ~ failures2 + goout, data=math, FUN=function(x) {sum(x)/length(x)})
aggregate(drinker ~ failures2 + goout, data=port, FUN=function(x) {sum(x)/length(x)})
aggregate(drinker ~ sex + studytime, data=math, FUN=function(x) {sum(x)/length(x)})
aggregate(drinker ~ sex + studytime, data=port, FUN=function(x) {sum(x)/length(x)})

library(corrplot)
# Plot numeric features - only failures2/failures and failures2/G3 moderately correlated
math_numeric_column_ix <- sapply(math, is.numeric)
port_numeric_column_ix <- sapply(port, is.numeric)
cor(math[, math_numeric_column_ix], method = "pearson")
cor(math[, math_numeric_column_ix], method = "spearman")
cor(port[, port_numeric_column_ix], method = "pearson")
cor(port[, port_numeric_column_ix], method = "spearman")
corrplot(cor(math[, math_numeric_column_ix]), method="ellipse")
corrplot(cor(port[, port_numeric_column_ix]), method="ellipse")

#-------------------------------Simple DT--------------------------------------
# Load rpart, rattle, and RColorBrewer packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Set seed for reproducibility
set.seed(111)

# Randomly split class datasets into 70% training samples and 30% test samples
idx_math_train <- sample(nrow(math), floor(nrow(math) * 0.7))
math_train <- math[idx_math_train,]
math_test <- math[-idx_math_train,]

idx_port_train <- sample(nrow(port), floor(nrow(port) * 0.7))
port_train <- port[idx_port_train,]
port_test <- port[-idx_port_train,]

# Build formulas
formula_text <- paste(names(math)[36] , "~", paste(names(math[c(seq(1, 26), seq(29, 31))]), collapse = "+"))
formula <- as.formula(formula_text)
formula_text2 <- paste(names(math)[36] , "~", paste(names(math[c(seq(1, 26), seq(29, 31), seq(33, 35))]), collapse = "+"))
formula2 <- as.formula(formula_text2)

formula_text_math1 <- "drinker ~ goout+sex+freetime+studytime+absences+nursery"
formula_math1 <- as.formula(formula_text_math1)
formula_text_math2 <- paste(formula_text_math1, "+traveltime+reason+Fedu+G3_2+age+failures", sep = "")
formula_math2 <- as.formula(formula_text_math2)

formula_text_port1 <- "drinker ~ sex+goout+absences+G3+address+age+freetime"
formula_port1 <- as.formula(formula_text_port1)
formula_text_port2 <- paste(formula_text_port1, "+failures2+Medu+higher+internet+Fjob", sep = "")
formula_port2 <- as.formula(formula_text_port2)

# Build decision trees
math_tree <- rpart(formula2, data = math_train, method = "class")
#!math_tree <- rpart(formula_math1, data = math_train, method = "class")
#!math_tree <- rpart(formula_math2, data = math_train, method = "class")
#math_tree <- rpart(formula, data = math_train, method = "class", control = rpart.control(cp=0.00001)) #cp default is 0.01
#math_tree <- rpart(formula, data = math_train, method = "class", control = rpart.control(minsplit = 30, cp=0.001)) #minsplit (parent node) default = 20
#math_tree <- rpart(formula, data = math_train, method = "class", parms = list(split = "information")) #default split is gini
#new_math_tree <- prp(math_tree, snip=TRUE)$obj
fancyRpartPlot(math_tree)
print(math_tree)
summary(math_tree)

port_tree <- rpart(formula2, data = port_train, method = "class")
#!port_tree <- rpart(formula_port1, data = port_train, method = "class")
#!port_tree <- rpart(formula_port2, data = port_train, method = "class")
#port_tree <- rpart(formula, data = port_train, method = "class", control = rpart.control(cp=0.00001)) #cp default is 0.01
#port_tree <- rpart(formula, data = port_train, method = "class", control = rpart.control(minsplit = 30, cp=0.001)) #minsplit (parent node) default = 20
#port_tree <- rpart(formula, data = port_train, method = "class", parms = list(split = "information")) #default split is gini
#new_port_tree <- prp(port_tree, snip=TRUE)$obj
fancyRpartPlot(port_tree)
print(port_tree)
summary(port_tree)

library(ROCR)
# Check performance measures on the test sets
# Make predictions on the test sets
math_pred_test <- predict(math_tree, math_test, type = "class")
port_pred_test <- predict(port_tree, port_test, type = "class")

# Predict probability values using the models
math_probs_test <- predict(math_tree, math_test, type = "prob")[,2]
port_probs_test <- predict(port_tree, port_test, type = "prob")[,2]

# Make prediction objects
math_prob_pred_test <- prediction(math_probs_test, math_test$drinker)
port_prob_pred_test <- prediction(port_probs_test, port_test$drinker)

# Make ROC performance objects
math_test_perf <- performance(math_prob_pred_test, "tpr", "fpr")
port_test_perf <- performance(port_prob_pred_test, "tpr", "fpr")

# Plot ROC curves
plot(math_test_perf)
abline(a=0, b= 1)
plot(port_test_perf)
abline(a=0, b= 1)

# Make AUC performance objects
math_test_perf2 <- performance(math_prob_pred_test, "auc")
port_test_perf2 <- performance(port_prob_pred_test, "auc")

# Determine AUC's (area under curves) = prediction accuracy
math_test_perf2@y.values[[1]] # 0.73
port_test_perf2@y.values[[1]] # 0.69

# Make confusion matrices
math_conf_test <- table(math_test$drinker, math_pred_test)
port_conf_test <- table(port_test$drinker, port_pred_test)

# Calculate accuracy
math_acc_test <- sum(diag(math_conf_test))/sum(math_conf_test)
port_acc_test <- sum(diag(port_conf_test))/sum(port_conf_test)

# Calculate precision
math_prec_test <- math_conf_test[2,2]/sum(math_conf_test[,2])
port_prec_test <- port_conf_test[2,2]/sum(port_conf_test[,2])

# Calculate recall/sensitivity
math_rec_test <- math_conf_test[2,2]/sum(math_conf_test[2,])
port_rec_test <- port_conf_test[2,2]/sum(port_conf_test[2,])

# Calculate specitivity/TN rate
math_spec_test <- math_conf_test[1,1]/sum(math_conf_test[1,])
port_spec_test <- port_conf_test[1,1]/sum(port_conf_test[1,])

#Print out performance measures
math_acc_test
math_prec_test
math_rec_test
math_spec_test
port_acc_test
port_prec_test
port_rec_test
port_spec_test

#-------------------------10 Fold Cross Validation with Simple DT------------------------
# Shuffle the datasets
math_n <- nrow(math)
math_shuffled <- math[sample(math_n),]
port_n <- nrow(port)
port_shuffled <- port[sample(port_n),]

# Initialize model performance vectors
math_auc <- rep(0, 10)
port_auc <- rep(0, 10)
math_accs <- rep(0, 10)
port_accs <- rep(0, 10)
math_precs <- rep(0, 10)
port_precs <- rep(0, 10)
math_recs <- rep(0, 10)
port_recs <- rep(0, 10)
math_specs <- rep(0, 10)
port_specs <- rep(0, 10)

for (i in 1:10) {
  #Create indices indicating the intervals of the test sets
  if (i == 10) { #Limit to max rows for last fold
    math_indices <- (((i-1) * round((1/10)*nrow(math_shuffled))) + 1):nrow(math_shuffled)
    port_indices <- (((i-1) * round((1/10)*nrow(port_shuffled))) + 1):nrow(port_shuffled)
  } else {
    math_indices <- (((i-1) * round((1/10)*nrow(math_shuffled))) + 1):((i*round((1/10) * nrow(math_shuffled))))
    port_indices <- (((i-1) * round((1/10)*nrow(port_shuffled))) + 1):((i*round((1/10) * nrow(port_shuffled))))
  }
  
  # Exclude indices from the train sets
  math_train_cv <- math_shuffled[-math_indices,]
  port_train_cv <- port_shuffled[-port_indices,]
  
  # Include indicies in the test sets
  math_test_cv <- math_shuffled[math_indices,]
  port_test_cv <- port_shuffled[port_indices,]
  
  #-------------------------------DT--------------------------------------
  # Create models for each training set using previous DT formulas
  math_tree_cv <- rpart(formula2, data = math_train_cv, method = "class")
  #!math_tree_cv <- rpart(formula_math1, data = math_train_cv, method = "class")
  #!math_tree_cv <- rpart(formula_math2, data = math_train_cv, method = "class")
  port_tree_cv <- rpart(formula2, data = port_train_cv, method = "class")
  #!port_tree_cv <- rpart(formula_port1, data = port_train_cv, method = "class")
  #!port_tree_cv <- rpart(formula_port2, data = port_train_cv, method = "class")
  
  # Make predictions on test sets using trees
  math_prediction <- predict(math_tree_cv, math_test_cv, type = "class")
  port_prediction <- predict(port_tree_cv, port_test_cv, type = "class")

  # Predict probability values using the models
  math_probs_test_cv <- predict(math_tree_cv, math_test_cv, type = "prob")[,2]
  port_probs_test_cv <- predict(port_tree_cv, port_test_cv, type = "prob")[,2]
  
  # Make prediction objects
  math_prob_pred_test_cv <- prediction(math_probs_test_cv, math_test_cv$drinker)
  port_prob_pred_test_cv <- prediction(port_probs_test_cv, port_test_cv$drinker)
  
  # Make ROC performance objects
  math_test_perf_cv <- performance(math_prob_pred_test_cv, "tpr", "fpr")
  port_test_perf_cv <- performance(port_prob_pred_test_cv, "tpr", "fpr")
  
  # Make AUC performance objects
  math_test_perf2_cv <- performance(math_prob_pred_test_cv, "auc")
  port_test_perf2_cv <- performance(port_prob_pred_test_cv, "auc")
  
  # Assign AUC's (area under curves) = prediction accuracy
  math_auc[i] <- math_test_perf2_cv@y.values[[1]]
  port_auc[i] <- port_test_perf2_cv@y.values[[1]]

  # Assign confusion matrices
  math_conf <- table(math_test_cv$drinker, math_prediction)
  port_conf <- table(port_test_cv$drinker, port_prediction)
  
  # Assign the accuracy of this model to the ith index in accs
  math_accs[i] <- sum(diag(math_conf))/sum(math_conf)
  port_accs[i] <- sum(diag(port_conf))/sum(port_conf)

  # Assign the precision of this model to the ith index in precs
  math_precs[i] <- math_conf[2,2]/sum(math_conf[,2])
  port_precs[i] <- port_conf[2,2]/sum(port_conf[,2])

  # Assign the recall of this model to the ith index in recs
  math_recs[i] <- math_conf[2,2]/sum(math_conf[2,])
  port_recs[i] <- port_conf[2,2]/sum(port_conf[2,])

  # Assign the specificity/TP rate of this model to the ith index in specs
  math_specs[i] <- math_conf[1,1]/sum(math_conf[1,])
  port_specs[i] <- port_conf[1,1]/sum(port_conf[1,])
}

#------------------------10 Fold Cross Validation DT Averages----------------------------
# Print mean auc's
mean(math_auc)
mean(port_auc)

# Print mean accuracy
mean(math_accs)
mean(port_accs)

# Print mean precision
mean(math_precs)
mean(port_precs)

# Print mean recall
mean(math_recs)
mean(port_recs)

# Print mean specificity/TP rate
mean(math_specs)
mean(port_specs)
  
#-----------------------------------Random Forest---------------------------------------
# Load randomForest package
library(randomForest)

# Set seed for reproducibility
set.seed(111)

# Test for missing values - RF's can't have any missing values
sum(complete.cases(math)) == nrow(math)
sum(complete.cases(port)) == nrow(port)

# Use Previous Randomly split class datasets into 70% training samples and 30% test samples
#idx_math_train <- sample(nrow(math), floor(nrow(math) * 0.7))
#math_train <- math[idx_math_train,]
#math_test <- math[-idx_math_train,]

#idx_port_train <- sample(nrow(port), floor(nrow(port) * 0.7))
#port_train <- port[idx_port_train,]
#port_test <- port[-idx_port_train,]

# Apply Random Forest Algorithm using custom formula due to factor requirement
math_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                              Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                              failures + schoolsup + famsup + paid + activities + nursery + 
                              higher + internet + romantic + famrel + freetime + goout + 
                              health + absences + G3 + age2 + failures2 + G3_2, data = math_train, importance = TRUE, ntree = 1000)
#!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery, data = math_train, importance = TRUE, ntree = 1000)
#!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery+traveltime+reason+Fedu+G3_2+age+failures, data = math_train, importance = TRUE, ntree = 1000)

port_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                              Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                              failures + schoolsup + famsup + paid + activities + nursery + 
                              higher + internet + romantic + famrel + freetime + goout + 
                              health + absences + G3 + age2 + failures2 + G3_2, data = port_train, importance = TRUE, ntree = 1000)
#!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime, data = port_train, importance = TRUE, ntree = 1000)
#!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime+failures2+Medu+higher+internet+Fjob, data = port_train, importance = TRUE, ntree = 1000)

# plot OOB (out of bag) and misclassification error rate curves (other colours)
plot(math_forest)
legend("topright", colnames(math_forest$err.rate),col=1:4,cex=0.8,fill=1:4)
plot(port_forest)
legend("topright", colnames(port_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Make predictions using the test sets
math_rf_prediction <- predict(math_forest, math_test)
port_rf_prediction <- predict(port_forest, port_test)

# Predict probability values using the models
math_probs_test <- predict(math_forest, math_test, type = "prob")[,2]
port_probs_test <- predict(port_forest, port_test, type = "prob")[,2]

# Make prediction objects
math_prob_pred_test <- prediction(math_probs_test, math_test$drinker)
port_prob_pred_test <- prediction(port_probs_test, port_test$drinker)

# Make ROC performance objects
math_test_perf <- performance(math_prob_pred_test, "tpr", "fpr")
port_test_perf <- performance(port_prob_pred_test, "tpr", "fpr")

# Plot ROC curves
plot(math_test_perf)
abline(a=0, b= 1)
plot(port_test_perf)
abline(a=0, b= 1)

# Make AUC performance objects
math_test_perf2 <- performance(math_prob_pred_test, "auc")
port_test_perf2 <- performance(port_prob_pred_test, "auc")

# Determine AUC's (area under curves) = prediction accuracy
math_test_perf2@y.values[[1]] # 0.81
port_test_perf2@y.values[[1]] # 0.78

# Make confusion matrices
math_conf_test <- table(math_test$drinker, math_rf_prediction)
port_conf_test <- table(port_test$drinker, port_rf_prediction)

# Calculate accuracy
math_acc_test <- sum(diag(math_conf_test))/sum(math_conf_test)
port_acc_test <- sum(diag(port_conf_test))/sum(port_conf_test)

# Calculate precision
math_prec_test <- math_conf_test[2,2]/sum(math_conf_test[,2])
port_prec_test <- port_conf_test[2,2]/sum(port_conf_test[,2])

# Calculate recall/sensitivity
math_rec_test <- math_conf_test[2,2]/sum(math_conf_test[2,])
port_rec_test <- port_conf_test[2,2]/sum(port_conf_test[2,])

# Calculate specitivity/TN rate
math_spec_test <- math_conf_test[1,1]/sum(math_conf_test[1,])
port_spec_test <- port_conf_test[1,1]/sum(port_conf_test[1,])

#Print out performance measures
math_acc_test
math_prec_test
math_rec_test
math_spec_test
port_acc_test
port_prec_test
port_rec_test
port_spec_test

# Plot Dotcharts of variable importance as measured by Random Forests
# Accuracy tests to see how worse the model performs without each variable,
# so a high decrease in accuracy would be expected for very predictive variables.
# Gini measures how pure the nodes are at the end of the tree, and
# tests to see the result if each variable is taken out.
varImpPlot(math_forest)
varImpPlot(port_forest)

#-------------------------------Simple Oversample--------------------------------------
# Oversample drinkers from previously created training sets
math_columns <- math_train[0,]
math_non_drinkers <- rbind(math_train[math_train$drinker == 0, ], math_columns)
num_non_drinkers <- nrow(math_non_drinkers)
math_drinkers <- math_train[math_train$drinker == 1, ]
num_drinkers <- nrow(math_drinkers)
oversample_idx <- sample(num_drinkers, num_non_drinkers, replace = TRUE)
math_oversampled <- rbind(math_drinkers[oversample_idx, ], math_non_drinkers)
prop.table(table(math_oversampled$drinker))

port_columns <- port_train[0,]
port_non_drinkers <- rbind(port_train[port_train$drinker == 0, ], port_columns)
num_non_drinkers <- nrow(port_non_drinkers)
port_drinkers <- port_train[port_train$drinker == 1, ]
num_drinkers <- nrow(port_drinkers)
oversample_idx <- sample(num_drinkers, num_non_drinkers, replace = TRUE)
port_oversampled <- rbind(port_drinkers[oversample_idx, ], port_non_drinkers)
prop.table(table(port_oversampled$drinker))

#-------------------------------------DT-----------------------------------------------
# Load rpart, rattle, and RColorBrewer packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Set seed for reproducibility
set.seed(111)

# Build decision trees using formulas defined previously
math_tree <- rpart(formula2, data = math_oversampled, method = "class")
#!math_tree <- rpart(formula_math1, data = math_oversampled, method = "class")
#!math_tree <- rpart(formula_math2, data = math_oversampled, method = "class")
fancyRpartPlot(math_tree)
print(math_tree)
summary(math_tree)

port_tree <- rpart(formula2, data = port_oversampled, method = "class")
#!port_tree <- rpart(formula_port1, data = port_oversampled, method = "class")
#!port_tree <- rpart(formula_port2, data = port_oversampled, method = "class")
fancyRpartPlot(port_tree)
print(port_tree)
summary(port_tree)

library(ROCR)
# Check performance measures on the test sets
# Make predictions on the test sets
math_pred_test <- predict(math_tree, math_test, type = "class")
port_pred_test <- predict(port_tree, port_test, type = "class")

# Predict probability values using the models
math_probs_test <- predict(math_tree, math_test, type = "prob")[,2]
port_probs_test <- predict(port_tree, port_test, type = "prob")[,2]

# Make prediction objects
math_prob_pred_test <- prediction(math_probs_test, math_test$drinker)
port_prob_pred_test <- prediction(port_probs_test, port_test$drinker)

# Make ROC performance objects
math_test_perf <- performance(math_prob_pred_test, "tpr", "fpr")
port_test_perf <- performance(port_prob_pred_test, "tpr", "fpr")

# Plot ROC curves
plot(math_test_perf)
abline(a=0, b= 1)
plot(port_test_perf)
abline(a=0, b= 1)

# Make AUC performance objects
math_test_perf2 <- performance(math_prob_pred_test, "auc")
port_test_perf2 <- performance(port_prob_pred_test, "auc")

# Determine AUC's (area under curves) = prediction accuracy
math_test_perf2@y.values[[1]] # 0.74
port_test_perf2@y.values[[1]] # 0.66

# Make confusion matrices
math_conf_test <- table(math_test$drinker, math_pred_test)
port_conf_test <- table(port_test$drinker, port_pred_test)

# Calculate accuracy
math_acc_test <- sum(diag(math_conf_test))/sum(math_conf_test)
port_acc_test <- sum(diag(port_conf_test))/sum(port_conf_test)

# Calculate precision
math_prec_test <- math_conf_test[2,2]/sum(math_conf_test[,2])
port_prec_test <- port_conf_test[2,2]/sum(port_conf_test[,2])

# Calculate recall/sensitivity
math_rec_test <- math_conf_test[2,2]/sum(math_conf_test[2,])
port_rec_test <- port_conf_test[2,2]/sum(port_conf_test[2,])

# Calculate specitivity/TN rate
math_spec_test <- math_conf_test[1,1]/sum(math_conf_test[1,])
port_spec_test <- port_conf_test[1,1]/sum(port_conf_test[1,])

#Print out performance measures
math_acc_test
math_prec_test
math_rec_test
math_spec_test
port_acc_test
port_prec_test
port_rec_test
port_spec_test

#-------------------------------------RF with Simple Oversample--------------------------
# Load randomForest package
library(randomForest)

# Set seed for reproducibility
set.seed(111)

# Apply Random Forest Algorithm using custom formula due to factor requirement
math_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                              Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                              failures + schoolsup + famsup + paid + activities + nursery + 
                              higher + internet + romantic + famrel + freetime + goout + 
                              health + absences + G3 + age2 + failures2 + G3_2, data = math_oversampled, importance = TRUE, ntree = 1000)
#!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery, data = math_oversampled, importance = TRUE, ntree = 1000)
#!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery+traveltime+reason+Fedu+G3_2+age+failures, data = math_oversampled, importance = TRUE, ntree = 1000)

port_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                              Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                              failures + schoolsup + famsup + paid + activities + nursery + 
                              higher + internet + romantic + famrel + freetime + goout + 
                              health + absences + G3 + age2 + failures2 + G3_2, data = port_oversampled, importance = TRUE, ntree = 1000)
#!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime, data = port_oversampled, importance = TRUE, ntree = 1000)
#!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime+failures2+Medu+higher+internet+Fjob, data = port_oversampled, importance = TRUE, ntree = 1000)

# plot OOB (out of bag) and misclassification error rate curves (other colours)
plot(math_forest)
legend("topright", colnames(math_forest$err.rate),col=1:4,cex=0.8,fill=1:4)
plot(port_forest)
legend("topright", colnames(port_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Make predictions using the test sets
math_rf_prediction <- predict(math_forest, math_test)
port_rf_prediction <- predict(port_forest, port_test)

# Predict probability values using the models
math_probs_test <- predict(math_forest, math_test, type = "prob")[,2]
port_probs_test <- predict(port_forest, port_test, type = "prob")[,2]

# Make prediction objects
math_prob_pred_test <- prediction(math_probs_test, math_test$drinker)
port_prob_pred_test <- prediction(port_probs_test, port_test$drinker)

# Make ROC performance objects
math_test_perf <- performance(math_prob_pred_test, "tpr", "fpr")
port_test_perf <- performance(port_prob_pred_test, "tpr", "fpr")

# Plot ROC curves
plot(math_test_perf)
abline(a=0, b= 1)
plot(port_test_perf)
abline(a=0, b= 1)

# Make AUC performance objects
math_test_perf2 <- performance(math_prob_pred_test, "auc")
port_test_perf2 <- performance(port_prob_pred_test, "auc")

# Determine AUC's (area under curves) = prediction accuracy
math_test_perf2@y.values[[1]] # 0.79
port_test_perf2@y.values[[1]] # 0.79

# Make confusion matrices
math_conf_test <- table(math_test$drinker, math_rf_prediction)
port_conf_test <- table(port_test$drinker, port_rf_prediction)

# Calculate accuracy
math_acc_test <- sum(diag(math_conf_test))/sum(math_conf_test)
port_acc_test <- sum(diag(port_conf_test))/sum(port_conf_test)

# Calculate precision
math_prec_test <- math_conf_test[2,2]/sum(math_conf_test[,2])
port_prec_test <- port_conf_test[2,2]/sum(port_conf_test[,2])

# Calculate recall/sensitivity
math_rec_test <- math_conf_test[2,2]/sum(math_conf_test[2,])
port_rec_test <- port_conf_test[2,2]/sum(port_conf_test[2,])

# Calculate specitivity/TN rate
math_spec_test <- math_conf_test[1,1]/sum(math_conf_test[1,])
port_spec_test <- port_conf_test[1,1]/sum(port_conf_test[1,])

#Print out performance measures
math_acc_test
math_prec_test
math_rec_test
math_spec_test
port_acc_test
port_prec_test
port_rec_test
port_spec_test

#----------------------SMOTE Oversampling - NO ORDERED FACTORS ALLOWED--------------------
library(DMwR) #SMOTE
# Set seed for reproducibility
set.seed(111)

# Oversample drinkers using SMOTE (Synthetic Minority Over-sampling TEchnique)
math_train_copy <- math_train
math_train_copy$drinker <- as.factor(math_train_copy$drinker)
math_oversampled <- SMOTE(formula2, math_train_copy)
table(math_oversampled$drinker)
prop.table(table(math_oversampled$drinker))

port_train_copy <- port_train
port_train_copy$drinker <- as.factor(port_train_copy$drinker)
port_oversampled <- SMOTE(formula2, port_train_copy)
table(port_oversampled$drinker)
prop.table(table(port_oversampled$drinker))

#-------------------------------------DT-----------------------------------------------
math_tree <- rpart(formula2, data = math_oversampled, method = "class")
#!math_tree <- rpart(formula_math1, data = math_oversampled, method = "class")
#!math_tree <- rpart(formula_math2, data = math_oversampled, method = "class")
port_tree <- rpart(formula2, data = port_oversampled, method = "class")
#!port_tree <- rpart(formula_port1, data = port_oversampled, method = "class")
#!port_tree <- rpart(formula_port2, data = port_oversampled, method = "class")

# Check performance measures on the test sets
# Make predictions on the test sets
math_pred_test <- predict(math_tree, math_test, type = "class")
port_pred_test <- predict(port_tree, port_test, type = "class")

# Predict probability values using the models
math_probs_test <- predict(math_tree, math_test, type = "prob")[,2]
port_probs_test <- predict(port_tree, port_test, type = "prob")[,2]

# Make prediction objects
math_prob_pred_test <- prediction(math_probs_test, math_test$drinker)
port_prob_pred_test <- prediction(port_probs_test, port_test$drinker)

# Make AUC performance objects
math_test_perf2 <- performance(math_prob_pred_test, "auc")
port_test_perf2 <- performance(port_prob_pred_test, "auc")

# Determine AUC's (area under curves) = prediction accuracy
math_test_perf2@y.values[[1]]
port_test_perf2@y.values[[1]]

# Make confusion matrices
math_conf_test <- table(math_test$drinker, math_pred_test)
port_conf_test <- table(port_test$drinker, port_pred_test)

# Calculate accuracy
math_acc_test <- sum(diag(math_conf_test))/sum(math_conf_test)
port_acc_test <- sum(diag(port_conf_test))/sum(port_conf_test)

# Calculate precision
math_prec_test <- math_conf_test[2,2]/sum(math_conf_test[,2])
port_prec_test <- port_conf_test[2,2]/sum(port_conf_test[,2])

# Calculate recall/sensitivity
math_rec_test <- math_conf_test[2,2]/sum(math_conf_test[2,])
port_rec_test <- port_conf_test[2,2]/sum(port_conf_test[2,])

# Calculate specitivity/TN rate
math_spec_test <- math_conf_test[1,1]/sum(math_conf_test[1,])
port_spec_test <- port_conf_test[1,1]/sum(port_conf_test[1,])

#Print out performance measures
math_acc_test
math_prec_test
math_rec_test
math_spec_test
port_acc_test
port_prec_test
port_rec_test
port_spec_test

#-------------------------------------RF-----------------------------------------------
# Load randomForest package
library(randomForest)

# Set seed for reproducibility
set.seed(111)

# Apply Random Forest Algorithm using custom formula due to factor requirement
math_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                              Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                              failures + schoolsup + famsup + paid + activities + nursery + 
                              higher + internet + romantic + famrel + freetime + goout + 
                              health + absences + G3 + age2 + failures2 + G3_2, data = math_oversampled, importance = TRUE, ntree = 1000)
#!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery, data = math_oversampled, importance = TRUE, ntree = 1000)
#!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery+traveltime+reason+Fedu+G3_2+age+failures, data = math_oversampled, importance = TRUE, ntree = 1000)

port_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                              Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                              failures + schoolsup + famsup + paid + activities + nursery + 
                              higher + internet + romantic + famrel + freetime + goout + 
                              health + absences + G3 + age2 + failures2 + G3_2, data = port_oversampled, importance = TRUE, ntree = 1000)
#!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime, data = port_oversampled, importance = TRUE, ntree = 1000)
#!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime+failures2+Medu+higher+internet+Fjob, data = port_oversampled, importance = TRUE, ntree = 1000)

# plot OOB (out of bag) and misclassification error rate curves (other colours)
plot(math_forest)
legend("topright", colnames(math_forest$err.rate),col=1:4,cex=0.8,fill=1:4)
plot(port_forest)
legend("topright", colnames(port_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Make predictions using the test sets
math_rf_prediction <- predict(math_forest, math_test)
port_rf_prediction <- predict(port_forest, port_test)

# Predict probability values using the models
math_probs_test <- predict(math_forest, math_test, type = "prob")[,2]
port_probs_test <- predict(port_forest, port_test, type = "prob")[,2]

# Make prediction objects
math_prob_pred_test <- prediction(math_probs_test, math_test$drinker)
port_prob_pred_test <- prediction(port_probs_test, port_test$drinker)

# Make ROC performance objects
math_test_perf <- performance(math_prob_pred_test, "tpr", "fpr")
port_test_perf <- performance(port_prob_pred_test, "tpr", "fpr")

# Plot ROC curves
plot(math_test_perf)
abline(a=0, b= 1)
plot(port_test_perf)
abline(a=0, b= 1)

# Make AUC performance objects
math_test_perf2 <- performance(math_prob_pred_test, "auc")
port_test_perf2 <- performance(port_prob_pred_test, "auc")

# Determine AUC's (area under curves) = prediction accuracy
math_test_perf2@y.values[[1]] # 0.76
port_test_perf2@y.values[[1]] # 0.77

# Make confusion matrices
math_conf_test <- table(math_test$drinker, math_rf_prediction)
port_conf_test <- table(port_test$drinker, port_rf_prediction)

# Calculate accuracy
math_acc_test <- sum(diag(math_conf_test))/sum(math_conf_test)
port_acc_test <- sum(diag(port_conf_test))/sum(port_conf_test)

# Calculate precision
math_prec_test <- math_conf_test[2,2]/sum(math_conf_test[,2])
port_prec_test <- port_conf_test[2,2]/sum(port_conf_test[,2])

# Calculate recall/sensitivity
math_rec_test <- math_conf_test[2,2]/sum(math_conf_test[2,])
port_rec_test <- port_conf_test[2,2]/sum(port_conf_test[2,])

# Calculate specitivity/TN rate
math_spec_test <- math_conf_test[1,1]/sum(math_conf_test[1,])
port_spec_test <- port_conf_test[1,1]/sum(port_conf_test[1,])

#Print out performance measures
math_acc_test
math_prec_test
math_rec_test
math_spec_test
port_acc_test
port_prec_test
port_rec_test
port_spec_test

#-----------------------10 Fold Cross Validation RF with SMOTE---------------------------
# Load randomForest package
library(randomForest)
library(DMwR) #SMOTE

# Set seed for reproducibility
set.seed(111)

# Shuffle the datasets
math_n <- nrow(math)
math_shuffled <- math[sample(math_n),]
port_n <- nrow(port)
port_shuffled <- port[sample(port_n),]

# Initialize model performance vectors
math_auc <- rep(0, 10)
port_auc <- rep(0, 10)
math_accs <- rep(0, 10)
port_accs <- rep(0, 10)
math_precs <- rep(0, 10)
port_precs <- rep(0, 10)
math_recs <- rep(0, 10)
port_recs <- rep(0, 10)
math_specs <- rep(0, 10)
port_specs <- rep(0, 10)

for (i in 1:10) {
  #Create indices indicating the intervals of the test sets
  if (i == 10) { #Limit to max rows for last fold
    math_indices <- (((i-1) * round((1/10)*nrow(math_shuffled))) + 1):nrow(math_shuffled)
    port_indices <- (((i-1) * round((1/10)*nrow(port_shuffled))) + 1):nrow(port_shuffled)
  } else {
    math_indices <- (((i-1) * round((1/10)*nrow(math_shuffled))) + 1):((i*round((1/10) * nrow(math_shuffled))))
    port_indices <- (((i-1) * round((1/10)*nrow(port_shuffled))) + 1):((i*round((1/10) * nrow(port_shuffled))))
  }
  
  # Exclude indices from the train sets
  math_train_cv <- math_shuffled[-math_indices,]
  port_train_cv <- port_shuffled[-port_indices,]
  
  # Oversample training sets using SMOTE (Synthetic Minority Over-sampling TEchnique)
  math_train_cv_copy <- math_train_cv
  math_train_cv_copy$drinker <- as.factor(math_train_cv_copy$drinker)
  math_cv_oversampled <- SMOTE(formula2, math_train_cv_copy)

  port_train_cv_copy <- port_train_cv
  port_train_cv_copy$drinker <- as.factor(port_train_cv_copy$drinker)
  port_cv_oversampled <- SMOTE(formula2, port_train_cv_copy)

  # Include indicies in the test sets
  math_test_cv <- math_shuffled[math_indices,]
  port_test_cv <- port_shuffled[port_indices,]
  
  #-------------------------------RF--------------------------------------
  # Apply Random Forest Algorithm using custom formula due to factor requirement
  math_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                                Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                                failures + schoolsup + famsup + paid + activities + nursery + 
                                higher + internet + romantic + famrel + freetime + goout + 
                                health + absences + G3 + age2 + failures2 + G3_2, data = math_cv_oversampled, importance = TRUE, ntree = 1000)
  #!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery, data = math_cv_oversampled, importance = TRUE, ntree = 1000)
  #!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+freetime+studytime+absences+nursery+traveltime+reason+Fedu+G3_2+age+failures, data = math_cv_oversampled, importance = TRUE, ntree = 1000)
  #!math_forest <- randomForest(as.factor(drinker) ~ goout+sex+studytime+absences+freetime+nursery+age2+G3_2+Fedu+famsup+failures2+traveltime+health+school, data = math_cv_oversampled, importance = TRUE, ntree = 1000)
  
  port_forest <- randomForest(as.factor(drinker) ~ school + sex + age + address + famsize + Pstatus + Medu + 
                                Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
                                failures + schoolsup + famsup + paid + activities + nursery + 
                                higher + internet + romantic + famrel + freetime + goout + 
                                health + absences + G3 + age2 + failures2 + G3_2, data = port_cv_oversampled, importance = TRUE, ntree = 1000)
  #!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime, data = port_cv_oversampled, importance = TRUE, ntree = 1000)
  #!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+absences+G3+address+age+freetime+failures2+Medu+higher+internet+Fjob, data = port_cv_oversampled, importance = TRUE, ntree = 1000)
  #!port_forest <- randomForest(as.factor(drinker) ~ sex+goout+G3+absences+address+age2+higher+famrel+Fjob+Medu+school+freetime+failures2+internet, data = port_cv_oversampled, importance = TRUE, ntree = 1000)
  
  # Make predictions using the test sets
  math_rf_prediction <- predict(math_forest, math_test_cv)
  port_rf_prediction <- predict(port_forest, port_test_cv)
  
  # Predict probability values using the models
  math_probs_test_cv <- predict(math_forest, math_test_cv, type = "prob")[,2]
  port_probs_test_cv <- predict(port_forest, port_test_cv, type = "prob")[,2]
  
  # Make prediction objects
  math_prob_pred_test_cv <- prediction(math_probs_test_cv, math_test_cv$drinker)
  port_prob_pred_test_cv <- prediction(port_probs_test_cv, port_test_cv$drinker)
  
  # Make ROC performance objects
  math_test_perf_cv <- performance(math_prob_pred_test_cv, "tpr", "fpr")
  port_test_perf_cv <- performance(port_prob_pred_test_cv, "tpr", "fpr")

  # Make AUC performance objects
  math_test_perf2_cv <- performance(math_prob_pred_test_cv, "auc")
  port_test_perf2_cv <- performance(port_prob_pred_test_cv, "auc")
  
  # Assign AUC's (area under curves) = prediction accuracy
  math_auc[i] <- math_test_perf2_cv@y.values[[1]]
  port_auc[i] <- port_test_perf2_cv@y.values[[1]]

  # Assign confusion matrices
  math_conf <- table(math_test_cv$drinker, math_rf_prediction)
  port_conf <- table(port_test_cv$drinker, port_rf_prediction)
  
  # Assign the accuracy of this model to the ith index in accs
  math_accs[i] <- sum(diag(math_conf))/sum(math_conf)
  port_accs[i] <- sum(diag(port_conf))/sum(port_conf)
  
  # Assign the precision of this model to the ith index in precs
  math_precs[i] <- math_conf[2,2]/sum(math_conf[,2])
  port_precs[i] <- port_conf[2,2]/sum(port_conf[,2])
  
  # Assign the recall of this model to the ith index in recs
  math_recs[i] <- math_conf[2,2]/sum(math_conf[2,])
  port_recs[i] <- port_conf[2,2]/sum(port_conf[2,])
  
  # Assign the specificity/TP rate of this model to the ith index in specs
  math_specs[i] <- math_conf[1,1]/sum(math_conf[1,])
  port_specs[i] <- port_conf[1,1]/sum(port_conf[1,])
}

#-------------------------------RF Averages--------------------------------------
# Print mean auc's
mean(math_auc)
mean(port_auc)

# Print mean accuracy
mean(math_accs)
mean(port_accs)

# Print mean precision
mean(math_precs)
mean(port_precs)

# Print mean recall
mean(math_recs)
mean(port_recs)

# Print mean specificity/TP rate
mean(math_specs)
mean(port_specs)

