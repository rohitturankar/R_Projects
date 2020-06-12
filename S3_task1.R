####Import - graphs and plots ####3 TASK 1
rm(list=ls())
getwd()
setwd("C:/Users/user/Documents/R/Practical_1")

beginExperiment <- read.table("diet_study_begin.txt",sep = "\t",header=TRUE)
str(beginExperiment)
beginExperiment
endExperiment <- read.table("diet_study_end.txt",sep = "\t",header=TRUE)
str(endExperiment)

# First, identify all the elements in end.experiment that are also in begin.experiment.
(indexVec <- is.element(beginExperiment$name,endExperiment$name))

# Create a vector of those patient names that started in the beginning and ended in the end.
(fullExperiment <-beginExperiment$name[indexVec])

# Merge the data for the weights at the beginning and end of the experiment.
# ... the data
beginExperiment[indexVec, ]
endExperiment[is.element(endExperiment$name, fullExperiment), ]

names(beginExperiment)[1 : 2] <- c('name', 'weight1')
names(endExperiment)[1 : 2] <- c('name', 'weight3')

# ...merge() does work here in the sense, that a full wide format is created 
(mergedDf <- merge(beginExperiment[indexVec, ],endExperiment[is.element(endExperiment$name, fullExperiment), ], all=TRUE))

mean(mergedDf$weight1)
mean(mergedDf$weight3)


# Stack names and then weights.
(weightStack <- stack(mergedDf, select = c("weight1", "weight3")))

# Calculate the mean wheight for this group of participants at the beginning and end of the study
# Because we reorganized our data we can easily do this with tapply: 
tapply(weightStack$values,weightStack$ind, mean)
