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
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median
age.median <- median(titanic.full$Age, na.rm = TRUE)
table(is.na(titanic.full$Age))
#clean missing values of fare
table(is.na(titanic.full$Fare))
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

#categorical casting
table(titanic.full$Survived)
str(titanic.train)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.train$Survived<- as.factor(titanic.train$Survived)

titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]
titanic.test

##making equation for survived ####

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")

install.packages("C:/Users/user/AppData/Local/Temp/RtmpeiADkY/downloaded_packages/rgl_0.100.19.zip", 
                  lib="C:/Users/user/Documents/R/win-library/3.5",repos = NULL) 

install.packages("rgl")

titanic.model <- randomForest::randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.1 * nrow(titanic.test))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)
Survived
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
tail(output.df)
  write.csv(output.df, file ="titanic_result.csv" , row.names = FALSE)
