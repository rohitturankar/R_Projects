
#load raw data 
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)


#Add survive variable in test
library(tibble)
test.survived <- add_column(test, "Survived"= rep("NONE",nrow(test)), .after = variable.names(test[1]))

#Combine test.survived and train
data.combined <- rbind(train, test.survived)

#Compactly Display the Structure of an Arbitrary R Object
str(data.combined)

#change sturcture of Pclass as Factor
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

#Take a look at survival rate
table(data.combined$Survived)
table(data.combined$Pclass)

#install.packages("ggplot2")
library(ggplot2)
str(train) 
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)

#Hypothesis that rich survied at higher rate
ggplot(train, aes(x = Pclass, fill = Survived)) +
         geom_bar() +
         xlab("Pclass")+
         ylab("Total count")+ 
         labs(fill = "Survived")

#see frist few name
head(train$Name)


#unique datasets in combined
length(unique(as.character(data.combined$Name)))
data.combined$Name <- as.character(data.combined$Name)

#Two dupicate, find the dupicate
duplicated(data.combined$Name)
dup.name <- data.combined[which(duplicated(data.combined$Name)), "Name"]
#Check that two names
data.combined[which(data.combined$Name %in% dup.name), ]

#All misses in data
library(stringr)
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")) ,]
misses[1:5,]

#All misses in data
mrs <- data.combined[which(str_detect(data.combined$Name,"Mrs.")) ,]
mrs[1:5,]

#Utility function to add row with factor of title
grepl("Mrs. ", "adfadpo Mrs. sdfasdf")

extract_title <- function(name){
  if(grepl("Mrs.", name)){
    print(name + "Mrs")
    return ("Mrs.")
}
  else if(grepl("Mr.", name)){ 
    return ("Mr.")
  }
  else if(grepl("Master.", name)){
    return ("Master.")
  }
  else if(grepl("Miss.", name)){
    return ("Miss.")
  }
  else{
    return("Others")
  }
}

Title <- NULL
for (i in 1:nrow(data.combined)) {
  Title <- c(Title, extract_title(data.combined[,"Name"]))
}
Title
Title <- as.factor(Title)
str(Title)
data.combined[ , ]



