#####task_2_graphs_and_plots############
rm(list=ls())
getwd()
setwd("C:/Users/user/Documents/R/Practical_1")
names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
scores <- c(59, 51, 72, 79, 79, 83, 69, 81, 51, 87)
##making data frame for scores and names
dfSN <- data.frame(scores, names, stringAsFactors = FALSE)
names(dfSN)
str(dfSN)

#making list with data frame
# Make a list with the first element named "names" and the second named "score."
(lsNamesScores <- list(names, scores))
names(lsNamesScores) <- c("names" ,"scores")
lsNamesScores
str(lsNamesScores)

# There are three main subsetting operators: [, [[ and $.
lsNamesScores$names
str(lsNamesScores$names)
# ==> lsNamesScores$names extracts the values of list element "names" as a vector.  
# We can extract a single value from that vector with [
lsNamesScores$names[1]
str(lsNamesScores$names[1])

lsNamesScores[["names"]]
str(lsNamesScores[["names"]])
# ==> lsNamesScores[["names"]] extracts the values of list element "names" as a vector.  
# We can extract a single value from that vector with [
lsNamesScores[["names"]][1]
str(lsNamesScores[["names"]][1])

lsNamesScores["names"]
str(lsNamesScores["names"])
# ==> lsNamesScores["names"] returns a list with one element: 
#     the list element "names" as a list.  
# We cannot extract a single value from that list with [. 
# [ will always return a list with one list as its only component.
lsNamesScores["names"][1]
str(lsNamesScores["names"][1])
lsNamesScores["names"][1][1]
lsNamesScores["names"][1][1][1]
# [[ returns a vector with the components again.
# Of course in an real world situation it would  
# be silly to first use [ and then [[ instead of 
# using [[ in the first place.
lsNamesScores["names"][[1]]
str(lsNamesScores["names"][[1]])
# We can extract a single value from that vector with [
lsNamesScores["names"][[1]][1]
str(lsNamesScores["names"][[1]][1])

lsNamesScores[[1]]
str(lsNamesScores[[1]])
# ==> in order to extract a single value [ works here
lsNamesScores[[1]][1]

lsNamesScores[1]
str(lsNamesScores[1])
# ==> [ does not work here
lsNamesScores[1][1]
# ==> this (silly) combination of [ and [[ would work 
lsNamesScores[1][[1]][1]


