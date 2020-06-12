# Ralf Darius
# 20140407
# Course: Data Analysis and Statistics
# Suggested techniques for exercise 4:
# Import, Explore, Graph 2


# Data and parts of the script from:
# Getting Started with R
# http://www.r4all.org


#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Tell R where to find our data 
# (you will need to customize this)
#----------------------------------------------------------
getwd()
setwd("C:/Users/user/Documents/R/Practical_1")
getwd()

#----------------------------------------------------------
# get the data 
#----------------------------------------------------------
growthData<-read.csv("growth.csv")
str(growthData)



#==========================================================
#----------------------------------------------------------
#----------------------------------------------------------
# Growth Data
# This data is about weight gain in cows measured under a 
# factorial design of four feeding supplements and three 
# different grains.
#
# data.frame':	48 obs. of  3 variables:
# $ supplement: Factor w/ 4 levels "agrimore","control",..: 3 3 3 3 2 2 2 2 4 4 ...
# $ diet      : Factor w/ 3 levels "barley","oats",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ gain      : num  17.4 16.8 18.1 15.8 17.7 ...
#
# ==> 3 Variables, 48 observations 
#       - gain: numerical, response variable
#       - diet: categorical, explanatory
#       - supplement: categorical, explanatory
#
# We should describe and analyse the data with respect to 
# possible effects of the categorical variables on the 
# the growth ("weight gain") of cows.
#
# When multivariate data like here require a description of 
# a quantitative variable in dependence of one or more qualitative 
# variables, the quantitative values are best viewed as separate 
# samples/populations, each set identified by levels/categories 
# of the qualitative variable.
# 
# Each sample/population can then be described with any 
# of the the numerical and graphical techniques that 
# have been discussed for univariate data. The results can 
# be displayed side-by-side for easy comparison. 
#----------------------------------------------------------
#----------------------------------------------------------
#==========================================================

# numerical review the total data first
growthData
summary(growthData)
# range(growthData$gain)

#=====================================
#=====================================
# Frequency Distributions - Histograms
#=====================================
#=====================================

#=====================================
# Distribution of weight gain
# ==> Set up intervals (width 1.5) for histograms
bins <- seq(15,30, by=1.5)
growthHist <- hist(growthData$gain, 
                   breaks = bins,
                   main = "Histogram of Total Weight Gain",
                   xlab = "Weight Gain")
#growthHist
#=====================================

#=====================================
# By diet
# One histogram on top of another one
#=====================================
# draw the first histogram
wheatHist <- hist(growthData[,3][growthData[,2]=="wheat"], 
                  breaks=bins, 
                  col="cornflowerblue",
                  #density=12, angle=80, 
                  xlab="Gain", 
                  ylim = c(0, 8),
                  main="Distribution of Weight Gain")

# add a second histogram to the existing plot
# ==> option add=TRUE tells R not to draw a fresh diagram
# ==> rgb() to determine the color of the bars, with option "alpha" transparent bar can be created 
oatsHist <- hist(growthData[,3][growthData[,2]=="oats"], 
                 col=rgb(red=0, green = 0, blue = 0, alpha=1),  ## <== transrancy through alpha 
                 density=8, angle=45, 
                 breaks=bins,  
                 add=TRUE)    

# add a third histogram to the existing plot
# ==> option add=TRUE tells R not to draw a fresh diagram
# ==> rgb() to determine the color of the bars, with option "alpha" transparent bar can be created 
barleyHist <- hist(growthData[,3][growthData[,2]=="barley"], 
                   col=rgb(red=0.7, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                   #density=18, angle=145,                  
                   breaks=bins,  
                   add=TRUE)    

# add a legend
legend(x=27,y=8, 
       fill = c("cornflowerblue", rgb(red=0, green = 0, blue = 0, alpha=1), rgb(red=0.7, green = 0, blue = 0, alpha=0.2)),
       density=c(NA, 8, NA),
       angle=c(FALSE, 45, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0,5, alpha=0.5),"cornflowerblue"), 
       legend=c("wheat", "oats", "barley")
)

#=====================================
# Compare different supplements with 
# the contol group
#=====================================
#parBackup <- par()
#par(mfrow=c(3,1))

#---------------------first comparison: supergain - control
# draw the control histogram for the first comparison
controlHist <- hist(growthData[,3][growthData[,1]=="control"], 
                    col=rgb(red=0, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                    density=18, angle=145,                  
                    breaks=bins,
                    xlab="Gain", 
                    ylim = c(0, 4),
                    main="Distribution of Weight Gain")    


# add the second histogram to the existing plot
supergainHist <- hist(growthData[,3][growthData[,1]=="supergain"], 
                      breaks=bins, 
                      col=rgb(red=0, green = 0, blue = 0.7, alpha=0.2),  ## <== transrancy through alpha ,
                      #density=12, angle=80, 
                      add=TRUE)

# add a legend
legend(x=26,y=4, 
       fill = c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0, blue = 0.7, alpha=0.2)),
       density=c(18, NA),
       angle=c(145, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0, blue = 0.7, alpha=0.2)), 
       legend=c("control", "supergain")
)

#---------------------second: agrimore - control
# draw the contol histogram for the second comparison
controlHist <- hist(growthData[,3][growthData[,1]=="control"], 
                    col=rgb(red=0, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                    density=18, angle=145,                  
                    breaks=bins,
                    xlab="Gain", 
                    ylim = c(0, 4),
                    main="Distribution of Weight Gain") 


# add the second histogram to the existing plot
agrimoreHist <- hist(growthData[,3][growthData[,1]=="agrimore"], 
                     col=rgb(red=0.7, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                     #density=8, angle=45, 
                     breaks=bins,  
                     add=TRUE)    

# add a legend
legend(x=26,y=4, 
       fill = c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0.7, green = 0, blue = 0, alpha=0.2)),
       density=c(18, NA),
       angle=c(145, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0.7, green = 0, blue = 0, alpha=0.2)), 
       legend=c("control", "agrimore")
)


#---------------------third: supersupp - control
# draw the contol histogram for the third comparison
controlHist <- hist(growthData[,3][growthData[,1]=="control"], 
                    col=rgb(red=0, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                    density=18, angle=145,                  
                    breaks=bins,
                    xlab="Gain", 
                    ylim = c(0, 4),
                    main="Distribution of Weight Gain") 


# add the second histogram to the existing plot
supersuppHist <- hist(growthData[,3][growthData[,1]=="supersupp"], 
                      col=rgb(red=0, green = 0.7, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                      #density=8, angle=45, 
                      breaks=bins,  
                      add=TRUE)    

# add a legend
legend(x=26,y=4, 
       fill = c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0.7, blue = 0, alpha=0.2)),
       density=c(18, NA),
       angle=c(145, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0.7, blue = 0, alpha=0.2)), 
       legend=c("control", "supersupp")
)

#par(parBackup)




#=====================================
#=====================================
# Frequency Distributions - Dotcharts
#=====================================
#=====================================

#=====================================
# Dotplot: Sorted, Grouped by Diet and Colored by Supplement
#=====================================

# Sort by gain 
x <- growthData[order(growthData$gain),] 
str(x)
# prepare colors for the different supplements
x$color[x$supplement=="control"] <- "black"
x$color[x$supplement=="supergain"] <- "blue"
x$color[x$supplement=="supersupp"] <- "red"
x$color[x$supplement=="agrimore"] <- "darkgreen"

# create dotchart grouped by diet
dotchart(x$gain,cex=.7,groups= x$diet,
         main="Weightgain for supplements grouped by Diet",
         xlab="Weightgain", gcolor="black", color=x$color) 

# add a legend
legend(x=26,y=20, cex=.6,
       fill = c("black","blue","red","darkgreen"),
       col=c("black","blue","red","darkgreen"), 
       legend=c("control", "supergain", "supersupp", "agrimore")
)


#=====================================
# Dotplot: Sorted, Grouped by Supplement and Colored by Diet
#=====================================

# Sort by gain 
x <- growthData[order(growthData$gain),] 

# prepare colors for the different diets
x$color[x$diet=="barley"] <- "black"
x$color[x$diet=="oats"] <- "blue"
x$color[x$diet=="wheat"] <- "red"

# create dotchart grouped by supplement
dotchart(x$gain,cex=.7,groups= x$supplement,
         main="Weightgain for Different Diets grouped by Supplements",
         xlab="Weightgain", gcolor="black", color=x$color) 

# add a legend
legend(x=26,y=23, cex=.6,
       fill = c("black","blue","red"),
       col=c("black","blue","red"), 
       legend=c("barley", "oats", "wheat")
)


#=====================================
#=====================================
# Frequency Distributions - Boxplots
#=====================================
#=====================================

#=====================================
# Boxplot of weightgain by diet
#=====================================
boxplot(gain~diet,data=growthData, main="Weightgain by Diet",xlab="Weightgain", ylab="Diet",horizontal=TRUE)

# Alternative usage of boxplot() --> same result
boxplot(subset(growthData,diet=="barley")[,3],
        subset(growthData,diet=="oats")[,3],
        subset(growthData,diet=="wheat")[,3],
        names=c("barley","oats","wheat"),
        main="Weightgain by Diet",xlab="Weightgain", ylab="Diet",horizontal=TRUE)


#=====================================
# Boxplot of weightgain by diet and supplement
#=====================================
parBackup <- par()
#par("mar")
#par("mgp")
#par("las")

par(mar=c(5.1, 6.1, 4.1, 1.1), mgp=c(4,0.5,0) ) # create enough space for the labels

# create a the boxplot
zz <-boxplot(gain~supplement+diet,data=growthData, 
             col=c("black", "blue", "red", "darkgreen"),
             main="Weightgain by Diet and Supplement",
             xlab="Weightgain", 
             ylab="Diet",
             pars = list(show.names=TRUE, las=1, cex.axis=0.5, mgp=c(3, 0.5, 0)),
             #names=growthData$boxLabels,
             horizontal=TRUE )

par(parBackup)

# boxplot() returns an object which provides all 
# the measures that R calculated in order to draw
# the boxplot
zz


#================================================
#================================================
# Interaction Plots
#
# An interaction plot is a visual representation of the interaction 
# between the effects of two factors, or between a factor and a numeric 
# variable. It is suitable for experimental data. You can create an 
# interaction plot with the interaction.plot function.
#
#  Use an interaction plot to show how the relationship between one categorical 
# factor and a continuous response depends on the value of the second categorical factor. 
# This plot displays means (or other summary) for the levels of one factor on the x-axis 
# and a separate line for each level of another factor.
# 
# Evaluate the lines to understand how the interactions affect the relationship between the 
# factors and the response.
#
# Parallel lines
# ==> No interaction occurs. 
#
# Nonparallel lines
# ==> An interaction occurs. The more nonparallel the lines are, the greater the strength of 
# the interaction. 
#
# Although you can use this plot to display the effects, be sure to perform the appropriate 
# ANOVA test and evaluate the statistical significance of the effects. If the interaction 
# effects are significant, you cannot interpret the main effects without considering the 
# interaction effects. 
#================================================
#================================================
interaction.plot(growthData$supplement,
                 growthData$diet,
                 growthData$gain,
                 main = "Interaction Plot of Gain vs Supplement grouped by Diet",
                 xlab = "Supplement",
                 ylab = "Weight Gain")
interaction.plot(growthData$diet,
                 growthData$supplement,
                 growthData$gain,
                 main = "Interaction Plot of Gain vs Diet grouped by Supplement",
                 xlab = "Diet",
                 ylab = "Weight Gain")



#================================================
#================================================
# Numerical Summaries by Categories - Visualized
#================================================
#================================================
# get the numerical summaries
summary(growthData)
summary(subset(growthData, diet=="barley" ))
summary(subset(growthData, diet=="barley" & supplement=="control"))
summary(subset(growthData, diet=="barley" & supplement=="supergain"))
#...


# Use tapply in order to call the functions for the 
# numerical summaries (mean, sd and sample size) on 
# different groups of values. The groups/subsets of
# the data are defined by the INDEX arfgument, i.e. 
# a list of one or more factors.
# tapply returns a matrix with the results.
meanGain <- tapply(growthData$gain,
                   list(growthData$supplement, growthData$diet),
                   mean)
meanGain

sdGain <- tapply(growthData$gain,
                 list(growthData$supplement, growthData$diet),
                 sd)
sdGain

nGain <- tapply(growthData$gain,
                list(growthData$supplement, growthData$diet),
                length)
nGain


# Draw a barplot, in order to visualize the 
# means of the different categories.
mids<- barplot(meanGain,
               beside=T, legend=T,
               xlab="Feed type",
               ylab="Weight gain",
               ylim=c(0, 35),
               col=grey(c(0, 0.3, 0.6, 1))
)

# One can add error bars to the bars representing 
# the means
arrows(mids,
       meanGain + sdGain,
       mids,
       meanGain - sdGain,
       angle=90,
       code=3,
       length=0.1)

# Add some text, with alternating colour
text(mids, 2, paste(nGain), col=c("white", rep("black", 3)))

# custom legend - not necessary with legend = T in barplot call

# legend("topright",
# legend=rownames(mean.gain),
# fill=grey(c(0, 0.3, 0.6, 1)))


#================================================
# ANOVA ==> Eta???
#================================================
#-----------------------------------------------------------------------
# Eta SQUARED --> (Total Variation - Group Variation)/Total Variation
# Out of the total variation in Y the proportion that can be attributed 
# to a specific x
#
# Eta??? is a PRE-measure for the association between one or more independent 
# categorical and one dependent metric variable. Eta??? is based on the partition 
# of sums of squares into various components. 
#
# The total sum of squares (TSS or SST) is a quantity that appears as part of 
# a standard way of presenting results of statistical data analyses. It is 
# defined as being the sum, over all observations, of the squared differences 
# of each observation from the overall mean. In statistical linear models, 
# (particularly in standard regression models), the TSS is the sum of the squares 
# of the difference of the dependent variable and its mean.
#
# For wide classes of linear models, the total sum of squares equals the explained 
# sum of squares plus the residual sum of squares. In analysis of variance (ANOVA) 
# the total sum of squares is the sum of the so-called "within-samples/groups" sum of 
# squares and "between-samples/groups" sum of squares. The samples/groups repesent
# the different values of the independent categorical variable(s). So the partition 
# of the total sum of squares in ANOVA looks like this:
#
#           TSS = SS_between_groups + SS_within_groups
#
# Eta???  is based on this partion of the TSS:

# Eta??? = (TSS - SS_within_groups)/TSS 
#
# ==> also:  
#
# Eta??? = SS_between_groups/TSS
#
# The PRE interpretation of Eta???: 
# If there are no differences between the groups, then there will be noch reduction of 
# error when considering the assumed factors for a prediction of the dependent variable,
# i.e. when using the means of the groups for a prediction Y instead of the mean of Y itself.
# The greater the difference between SS_between_groups and SS_within_groups, i.e. the more 
# of the total variance is  found between the groups than within the groups, the better
# the prediction will be based on the means of the groups compard to the prediction 
# based on the mean of the observed Y values.
#
#-------
# ANOVA
#-------
# Analysis of variance (ANOVA) is a collection of statistical models and their associated
# estimation procedures (such as the "variation" among and between groups) used to analyze 
# the differences among group means in a sample. In the ANOVA setting, the observed variance 
# in a particular variable is partitioned into components attributable to different sources 
# of variation. In its simplest form, ANOVA provides a statistical test of whether the 
# population means of several groups are equal, and therefore generalizes the t-test to more 
# than two groups. ANOVA is useful for testing three or more group means for statistical significance.
#
#-----------------------------------------------------------------------
#install.packages("lsr")
#install.packages("Hmisc")
library("lsr")
#library("Hmisc")

# We need to do an ANOVA first.
# The ANOVA object will provide the sum of squares components that are required for 
# the calculation of Eta???.
growthData_ANOVA <- aov(growthData$gain ~ growthData$diet)
etaSquared(growthData_ANOVA)

growthData_ANOVA <- aov(growthData$gain ~ growthData$supplement)
etaSquared(growthData_ANOVA)

growthData_ANOVA <- aov(growthData$gain ~ growthData$supplement + growthData$diet)
etaSquared(growthData_ANOVA)

growthData_ANOVA <- aov(growthData$gain ~ growthData$supplement * growthData$diet )
etaSquared(growthData_ANOVA)



#==========================================================
#----------------------------------------------------------
#----------------------------------------------------------
# Compensation Data
# These data are about fruit production and initial root 
# diameter in grazed or ungrazed conditions. Grazing reduces 
# above-ground biomass. How is fruit production affected by 
# this reduction? And how is the fruit production related  
# to the initial root diameter of the plants.
#
# data.frame':	40 obs. of  3 variables:
# $ Root   : num  6.22 6.49 4.92 5.13 5.42 ...
# $ Fruit  : num  59.8 61 14.7 19.3 34.2 ...
# $ Grazing: Factor w/ 2 levels "Grazed","Ungrazed": 2 2 2 2 2 2 2 2 2 2 ...
#
# ==> 3 Variables, 40 observations 
#       - Root: numerical, explanatory variable
#       - Fruit: numerical, response
#       - Grazing: categorical, explanatory
#
# We should describe and analyse the data with respect to 
# possible effects of what we expect to be the explanatory  
# variables on the amount of fruits produced. Those effects 
# would be considered main effects.
#
# Additionally we should try to identify possible effects 
# between the different exolanatory variables on their   
# relationship with the response variable ==>
# interaction effects.
#----------------------------------------------------------
#----------------------------------------------------------
#==========================================================
# review the total data first
compensationData
summary(compensationData)

# ==> Set up intervals (width 1.5) for a histogram
(bins <- seq(14,117, by=( (117-14)/15 ) ))
compensationHist <- hist(compensationData$Fruit, 
                         breaks=bins,
                         main = "Histogram of Fruit Production ",
                         xlab = "Fruit Production")
#compensationHist

#=====================================
#=====================================
# Frequency Distributions - Histograms
#=====================================
#=====================================

#=====================================
# By Grazing
# One histogram on top of another one
#=====================================
# draw the first histogram
grazedHist <- hist(compensationData[,2][compensationData[,3]=="Grazed"], 
                   breaks=bins, 
                   col="cornflowerblue",
                   xlab="Gain", 
                   ylim = c(0, 5),
                   main="Amount of Fruit")

# add a second histogram to the existing plot
ungrazedHist <- hist(compensationData[,2][compensationData[,3]=="Ungrazed"], 
                     col=rgb(red=0, green = 0, blue = 0, alpha=1),  ## <== transrancy through alpha 
                     density=8, angle=45, 
                     breaks=bins,  
                     add=TRUE)    

# add a legend
legend(x=90,y=5, 
       fill = c("cornflowerblue", rgb(red=0, green = 0, blue = 0, alpha=1)),
       density=c(NA, 8),
       angle=c(FALSE, 45),
       col=c("cornflowerblue", rgb(red=0, green = 0, blue = 0, alpha=1)), 
       legend=c("Grazed", "Ungrazed")
)


#=====================================
#=====================================
# Frequency Distributions - Dotcharts
#=====================================
#=====================================

#=====================================
# Dotplot: Sorted, Grouped by Grazing
#=====================================

# Sort by Fruit 
fruitSorted <- compensationData[order(compensationData$Fruit),] 

# prepare colors for the different supplements
fruitSorted$color[fruitSorted$Grazing=="Grazed"] <- "red"
fruitSorted$color[fruitSorted$Grazing=="Ungrazed"] <- "blue"

# create dotchart grouped by Grazing
dotchart(fruitSorted$Fruit,cex=.7,groups= fruitSorted$Grazing,
         main="Fruit Production by Grazing",
         xlab="Amount of Fruits", gcolor="black", color=fruitSorted$color) 

# add a legend
legend(x=100,y=15, cex=.6,
       fill = c("red","blue"),
       col=c("red","blue"), 
       legend=c("Grazed", "Ungrazed")
)

#=====================================
#=====================================
# Frequency Distributions - Boxplots
#=====================================
#=====================================

#=====================================
# Boxplot of Fruit Production by Grazing
#=====================================
boxplot(Fruit~Grazing,
        data=compensationData, 
        main="Fruit Production by Grazing",
        xlab="Amount of Fruit", 
        ylab="Grazing",
        horizontal=TRUE,
        varwidth=TRUE
)


#================================================
#================================================
# Numerical Summaries by Categories - Visualized
#================================================
#================================================
summary(compensationData)
summary(subset(compensationData,Grazing=="Grazed"))
summary(subset(compensationData,Grazing=="Ungrazed"))


(meanFruit <- tapply(compensationData$Fruit,
                     list(compensationData$Grazing),
                     mean))

(sdFruit <- tapply(compensationData$Fruit,
                   list(compensationData$Grazing),
                   sd))

(nFruit <- tapply(compensationData$Fruit,
                  list(compensationData$Grazing),
                  length))

# Draw a barplot, in order to visualize the 
# means of the different categories.
midsFruit<- barplot(meanFruit,
                    beside=T, legend=F,
                    xlab="Grazing",
                    ylab="Amount of Fruit",
                    ylim=c(0, 100),
                    col="blue"
)

# One can add error bars to the bars representing 
# the means
arrows(midsFruit,
       meanFruit + sdFruit,
       midsFruit,
       meanFruit - sdFruit,
       angle=90,
       code=3,
       length=0.1)

# Add some text, with alternating colour
text(midsFruit, 10, paste("n = ", nFruit), col=c("white"))


#================================================
#================================================
# Correlation
#================================================
#================================================
## Grazing --> Fruit Production
# ==> Point-biserial Correlation
# first some necessary preparations
compensationData <- cbind(compensationData, Grazing_Code = as.numeric(compensationData[,"Grazing"]=="Grazed"))
compensationData
# call the cor.test function accordingly
cor.test(compensationData[,"Grazing_Code"],
         compensationData[,"Fruit"], 
         method = "pearson" )

## Initial Root Diameter --> Fruit Production
# call the cor.test function accordingly
cor.test(compensationData[,"Root"],
         compensationData[,"Fruit"], 
         method = "pearson" )

# alternatively
cor(compensationData[,c("Root", "Fruit", "Grazing_Code")]) 


#================================================
#================================================
# Partial Correlation
#================================================
#================================================
library(ggm)
pcor(c("Fruit","Root","Grazing_Code"), var(compensationData))
pcor(c("Fruit","Grazing_Code", "Root"), var(compensationData))


#================================================
#================================================
# Scatterplot of the numerical Variables (Fruit  vs. Root)
# ==> Describe the relationship between fruit
# production and initial root biomass (the idea is,
# that root biomass affects the fruit production).
# In addition we want to visualize if Grazing 
# affects the relationship.
# ==> main and interaction effects
#================================================
#================================================

# prepare the colors of the plotted dots in
# dependence of the Grazing factor
compColorDat <- compensationData
compColorDat$color[compColorDat$Grazing=="Grazed"] <- "blue"
compColorDat$color[compColorDat$Grazing=="Ungrazed"] <- "red"

# scatterplot
plot(compColorDat$Root, compColorDat$Fruit,
     xlab="Initial Root Biomass",
     ylab="Fruit Production",
     col=compColorDat$color
)
# add lines for the means of x and y
abline(meanFruit["Grazed"], 0, lty=2, col="blue")
abline(meanFruit["Ungrazed"], 0, lty=2, col="red")
# add a legend
legend(x=5,y=115, cex=.6,
       fill = c("blue", "red"),
       col=c("blue", "red"), 
       legend=c("Grazed", "Ungrazed")
)

#================================================
#================================================
# Linear Regression
# lm is used to fit linear models.==> It can be used to 
# carry out regression.
# 
# The models fit by the lm function are specified in a 
# compact symbolic form. The ~ operator is basic in the 
# formation of such models. An expression of the form 
# y ~ model is interpreted as a specification that the 
# response y is modelled by a linear predictor specified 
# symbolically by model. 
#
# Such a model consists of a series of terms separated 
# by + operators. The terms themselves consist of variable 
# and factor names separated by : operators. Such a term 
# is interpreted as the interaction of all the variables 
# and factors appearing in the term.
#
# In addition to + and :, a number of other operators are 
# useful in model formulae. The * operator denotes factor 
# crossing: a*b interpreted as a+b+a:b....
#================================================
#================================================

#-------------------------------------------------------
# simple linear regression (Fruit vs Root) with all data points
fruit_root_Mmodel <- lm(compColorDat$Fruit~compColorDat$Root)

# add the regression line to the scatterplot Fruit Production vs. Root Biomass from above
abline(fruit_root_Mmodel)

# All the stats (Regression coefficients, Coefficient of Determination and more ...
summary(fruit_root_Mmodel)

# Alternatively the regression coefficients only 
coef(fruit_root_Mmodel)

# The values predicted by the model
fittedFR <- fitted(fruit_root_Mmodel)
# add the fitted values to the plot 
points(compColorDat$Root, fittedFR, col="black" )

#---------------------------------------------------------
# simple linear regression (Fruit vs Root) only with all data points for Grazed treatment
fruit_root_Grazed_Model <- lm(compColorDat[compColorDat$Grazing=="Grazed", ]$Fruit~compColorDat[compColorDat$Grazing=="Grazed",]$Root)
abline(fruit_root_Grazed_Model, col="blue" )

# Coefficient of Determination and more ...
summary(fruit_root_Grazed_Model)

fittedFRgrazed <- fitted(fruit_root_Grazed_Model)
points(compColorDat[compColorDat$Grazing=="Grazed",]$Root, fittedFRgrazed, col="blue", bg="grey" )


#---------------------------------------------------------
# simple linear regression (Fruit vs Root) only with all data points for Ungrazed treatment
fruit_root_Ungrazed_Model <- lm(compColorDat[compColorDat$Grazing=="Ungrazed", ]$Fruit~compColorDat[compColorDat$Grazing=="Ungrazed",]$Root)
abline(fruit_root_Ungrazed_Model, col="red"  )

# Coefficient of Determination and more ...
summary(fruit_root_Ungrazed_Model)

fittedFRungrazed <- fitted(fruit_root_Ungrazed_Model)
points(compColorDat[compColorDat$Grazing=="Ungrazed",]$Root, fittedFRungrazed, col="red", bg="grey" )

#---------------------------------------------------------
# multiple linear regression
#---------------------------------------------------------
fruit_root_Grazing_Model <- lm(Fruit~Root*Grazing, data=compColorDat)
# !!Since this model comprises 2 independent and one dependent variable,
# it does not represent a straight line and can not be plotted into 
# 2-dimensional coordinate systems!!! 
# See below how to visualize this type of data and relationship in a 
# 3-dimensional plot.

# Coefficient of Determination and more ...
summary(fruit_root_Grazing_Model)
coef(fruit_root_Grazing_Model)
fittedFRG <- fitted(fruit_root_Grazing_Model )

# with a an alternative model description
fruit_Grazing_root_Model <- lm(Fruit~Grazing*Root, data=compColorDat)

# Coefficient of Determination and more ...
summary(fruit_Grazing_root_Model)
resid(fruit_Grazing_root_Model)
coef(fruit_Grazing_root_Model)
fittedFGR <- fitted(fruit_Grazing_root_Model )


# ======================================================================================
# Graphical Diagnostics:  Fit of the model to the given data and check the assumption with 
# respect to the statistical significance of the model
# ======================================================================================
plot(fruit_root_Mmodel)
plot(fruit_root_Grazed_Model)
plot(fruit_root_Ungrazed_Model)

# graphical check of the results
plot(fruit_root_Grazing_Model)
plot(fruit_Grazing_root_Model)

# ... or this way
parBackup <- par()
par(mfrow=c(2,2))
plot(fruit_root_Mmodel)
par(parBackup)
## plot one - upper left corner = Residuals vs Fitted (or predicted) values
##  Check for the magnitude und the homogeneity of the distribution of the residuals
##  ==> Smaller values and homogenously distributed residuals are what we want 
##  ==> Heteroscedasticity (heterogeneity of the variance of the residuals) would negatively affect the 
## Ideally the residuals equal zero (=dashed line). 
## The red line represents the observed residuals


#========================================================================================
# Numerical diagnostics that relate to the overall model or specific variables 
# --> Residual standard error
# --> Multiple R-squared
# --> Adjusted R-squared 
# Adjusted R??? indicates like R??? how well the model fits the data, but adjusts R???
# for the number of predictors in the model and the number of observations used. 
# If you add more and more useless variables to a model, adjusted r-squared will 
# decrease. If you add more useful variables, adjusted r-squared will increase.
# Adjusted R2 will always be less than or equal to R2. 
# e.g. ...

summary(fruit_root_Mmodel) # Fruit vs Root with all data points
summary(fruit_root_Grazed_Model) # Fruit vs Root only with all data points for Grazed treatment
summary(fruit_root_Ungrazed_Model) # Fruit vs Root only with all data points for Ungrazed treatment

summary(fruit_root_Grazing_Model) # multiple regession
summary(fruit_Grazing_root_Model)

#========================================================================================
# Diagnostics that relate to cases --> Outliers and influential cases 
# These are supposed to tell us if our model is overly influenced by 
# one case or a small number of cases?
# e.g. for fruit_root_Grazing_Model
dfResidual <- data.frame( Residual = resid(fruit_root_Grazing_Model),
                          RStandard = rstandard(fruit_root_Grazing_Model), # Standardized residuals 
                          # --> comparison of different models
                          # --> detection of outliers
                          RStudent = rstudent(fruit_root_Grazing_Model), # Studentized residuals
                          # = (Adjusted predicted value - original observation)/Standard error
                          # --> Assess the influence of a case on the ability of the model to 
                          #     predict that case
                          DFFits = dffits(fruit_root_Grazing_Model), # DFFit
                          #  = Adjusted predicted value - original predicted value
                          # --> Identify influential cases                          
                          CooksDistance = cooks.distance(fruit_root_Grazing_Model), # Cook's Distance
                          # --> Measure of the overall influence of a case 
                          # on the model as a whole (> 1 is a problem)
                          Leverage = hatvalues(fruit_root_Grazing_Model), # Leverages/ Hat values
                          # measure of how far away the independent variable values of an 
                          # observation are from those of the other observations
                          DFBeta = dfbeta(fruit_root_Grazing_Model) # DFBeta
                          # Difference between a parameter of the model estimated using all
                          # cases and estimated when one case is excluded.
                          # --> Influence of a case on the whole model. 
)

# for further analysis ...
dfResidual



#-----------------------------------------------------------------------
# Multiple Regression - Plotting 3-dimensional scatter plots
#----------------------------------------------------------------------
mod1 <- lm(Fruit ~ Root + Grazing, data=compColorDat)

# Creating a scatter plot with the formula that includes more than one 
# independent variable leads to multiple 2 dimensional plots for all 
# possible bivariate combinations 
plot(compColorDat$Fruit ~ compColorDat$Root + compColorDat$Grazing)


## Plot the multiple regression results 
# ==> max 3 dimensions, i.e. 2 independent + 1 dependent Variable!!!

# Prepare the data first
x <- compColorDat$Root 
z <- compColorDat$Grazing 
y <- compColorDat$Fruit

# Different packages offer appropriate functions ...

# install.packages("scatterplot3d")
library("scatterplot3d")
multiScat <- scatterplot3d(x, z, y,
                           main="3D Scatterplot with Regression Plane",
                           pch=16, 
                           highlight.3d=FALSE,
                           type="p" ,
                           color = "darkgrey",
                           grid = TRUE, 
                           box = FALSE,  
                           mar = c(2.5, 2.5, 2, 1.5), 
                           angle = 35,
                           xlab = "Root Diameter",
                           ylab = "Grazing",
                           zlab = "Fruits")

# Plot a surface that represents the model into the scatter plot
# if there are no interactions between the independent variables 
# a plane will do --> Regression Plane
multiScat$plane3d(mod1,
                  draw_polygon = TRUE, 
                  draw_lines = FALSE,
                  polygon_args = list(col = rgb(.1, .2, .7, .5)))

# Overlay positive residuals
posResiduals <- resid(mod1) > 0
multiScat$points3d(x[posResiduals], 
                   z[posResiduals], 
                   y[posResiduals], pch = 19)

# Plot the residuals
# compute locations of segments
orig     <- multiScat$xyz.convert(x, z, y)
plane    <- multiScat$xyz.convert(x, z, fitted(mod1))
i.negpos <- 1 + (resid(mod1) > 0) # which residuals are above the plane?

# draw residual distances to regression plane
segments(orig$x, orig$y, plane$x, plane$y, col = "red", lty = c(2, 1)[i.negpos], 
         lwd = 1.5)

# draw the regression plane
multiScat$plane3d(mod1, draw_polygon = TRUE, draw_lines = TRUE, 
                  polygon_args = list(col = rgb(0.8, 0.8, 0.8, 0.8)))

# redraw positive residuals and segments above the plane
segments(orig$x[posResiduals], 
         orig$y[posResiduals], 
         plane$x[posResiduals], 
         plane$y[posResiduals], 
         col = "red", 
         lty = 1, 
         lwd = 1.5)
multiScat$points3d(x[posResiduals], 
                   z[posResiduals], 
                   y[posResiduals], 
                   pch = 19)


## alternatively with rgl Lib
library("rgl")
# define the model and fit it to the data
mod2 <- lm(z~x+y)
# open a grafics window that allows for interactive usage 
open3d()
# plot the 3d graph
plot3d(x=x, y=y, z=z, type="s", col="yellow", size=1)

#-----------------------------------------
# In case you want to add a regression plane (only if interaction
# is not part of the model), use the planes3d function
# 1 - Prepare the parameters for the regression plane
coefs <- coef(mod2)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
# 2 - plot the regression plane
planes3d(a, b, c, d, alpha = 0.5)

#-----------------------------------------
# In case interaction effects have to be taken into consideration
# the regression model will not represent a plane but a different
# type of surface --> use the following if a plane is not what 
# you need
# 1 - Prepare data points for the surface. 
grd <- expand.grid(x = sort(unique(x)), 
                   y = sort(unique(y)) )
grd$pred <-predict(mod2, newdata=grd)
# 2 - Plot the surface
persp3d(x=unique(grd[[1]]), 
        y=unique(grd[[2]]), 
        z=matrix(grd[[3]],
                 length(unique(grd[[1]])),
                 length(unique(grd[[2]]))), 
        add=TRUE)


#========================================================
## The same combination of variables as before, this time with 
# an interaction term

# define the model and fit it to the data
mod3 <- lm(z~x*y)
# open a grafics window that allows for interactive usage 
open3d()
# plot the 3d graph
plot3d(x=x, y=y, z=z, type="s", col="yellow", size=1)
# Add a surface that represents the regression model
# 1 - Prepare data points for the surface. 
grd <- expand.grid(x = sort(unique(x)), 
                   y = sort(unique(y)) )
grd$pred <-predict(mod3, newdata=grd)
# 2 - Plot the surface
persp3d(x=unique(grd[[1]]), 
        y=unique(grd[[2]]), 
        z=matrix(grd[[3]],
                 length(unique(grd[[1]])),
                 length(unique(grd[[2]]))), 
        add=TRUE)

