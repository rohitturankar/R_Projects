
rm(list=ls())
getwd()
setwd("C:/Users/user/Documents/R/Practical_1")
dat08<-read.table("Sight2008.txt",sep = "\t",header=TRUE)
dat09<-read.table("sight2009.txt",sep = "\t", header=TRUE)
names(dat08)
dim(dat08)
str(dat08)
summary(dat08)
dat08[6,3]

#If data frames is having different columns and rows add them using merge all=true
dat<- merge(dat08, dat09, all=TRUE)
dat


#For fetching a column for data frame use square brackets 
temps<- dat[,"Temperature"]
tempsF<- temps*9/5+32
id<- dat[,"Indiv_ID"]
id
#for adding a coloumn use cbind
dat<-cbind(dat,tempsF)
dat

#Exporting data of data frame to Excel that can be readed.

write.table(dat, "SightE.xls", sep = "\t", row.names = FALSE, col.names = TRUE)
typeof(temps)
length(dat)
objects()

names(dat) 
abcd<- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
names(abcd)
