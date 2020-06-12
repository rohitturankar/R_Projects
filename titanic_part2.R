rm(list=ls())
getwd()
setwd("C:/Users/user/Documents/R/Practical_1")
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
tail(titanic.train)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)
str(titanic.test)
titanic.test$IsTrainSet <- FALSE
titanic.train$IsTrainSet <- TRUE
names(titanic.test)
names(titanic.train)
titanic.test$Survived <- NA
ncol(titanic.train)
titanic.full <- rbind(titanic.train, titanic.test)
tail(titanic.full)
table(titanic.full$IsTrainSet)
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#clean missing values of Age

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median
table(is.na(titanic.full$Age))

#Categorical Casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
boxplot(titanic.full$Fare)
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]

fare.equation <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"

fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)

is.na(titanic.full$Fare)
fare.row <- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass","Sex","Age","SibSp","Parch","Embarked")
]

fare.predictions <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
titanic.full[1044,]


titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.train$Survived<- as.factor(titanic.train$Survived)

titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]
titanic.test

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

titanic.model <- randomForest::randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.1 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

Survived <- predict(titanic.model, newdata = titanic.test)
Survived
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file ="titanic_result_part2.csv" , row.names = FALSE)

