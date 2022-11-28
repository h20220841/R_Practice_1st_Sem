#Basic Statistics
#mean, var, cor and t.test.
library(tidyverse)
#generate a random sampling of 100 numbers between 1 and 100.
x <- sample(x=1:100, size=100, replace=TRUE)
#Setting replace=TRUE means that the same number can be drawn multiple times.
mean(x)
# copy x
y <- x
# choose a random 20 elements, using sample, to set to NA
y[sample(x=1:100, size=20, replace=FALSE)] <- NA
y
#Using mean on y will return NA
mean(y) 
#To have the NAs removed before calculating the mean, set na.rm to TRUE.
mean(y, na.rm=TRUE) 
#To calculate the weighted mean of a set of numbers, the function weighted.mean takes
#a vector of numbers and a vector of weights.
grades <- c(95, 72, 87, 66)
weights <- c(1/2, 1/4, 1/8, 1/8)
mean(grades)
weighted.mean(x=grades, w=weights)
var(x) #variance
sqrt(var(x)) #standard deviation
sd(x)    # sd()-----> standard function
sd(y, na.rm=TRUE)
#other commonly used functions for summary statistics are min, max and median
min(x)
max(x)
median(x)
min(y) #NA
min(y, na.rm=TRUE)
#A helpful function that computes the mean, minimum, maximum and median is
#summary. There is no need to specify na.rm because if there are NAs, they are
#automatically removed and their count is included in the results.

summary(x)
# compute  quantiles
quantile(x, probs=c(.25, .75))
quantile(x, probs=c(.1, .25, .5, .75, .99))

#Correlation and Covariance
head(economics) #from ggplot2 package
?economics
cor(economics$pce, economics$psavert)

#To compare multiple variables at once, use cor on a matrix (only for
#numeric variables).
cor(economics[, c(2, 4:6)])  # start from the 2nd column 
GGally::ggpairs(economics[, c(2, 4:6)])
#This shows a scatterplot of every variable in the data against every other variable.
#rather than load GGally, we call its function using the :: operator, which
#allows access to functions within a package without loading it.

# load the reshape package for melting the data
library(reshape2)
# load the scales package for some extra plotting features
library(scales)
# build the correlation matrix
econCor <- cor(economics[, c(2, 4:6)])
econCor
# melt it into the long format
econMelt <- melt(econCor, varnames=c("x", "y"), value.name="Correlation")
# order it according to the correlation
econMelt <- econMelt[order(econMelt$Correlation), ]
# display the melted data
econMelt

## plot it with ggplot
# initialize the plot with x and y on the x and y axes
# draw tiles filling the color based on Correlation
# make the fill (color) scale a three color gradient with muted
# red for the low point, white for the middle and steel blue for the high point
# the guide should be a colorbar with no ticks, whose height is 10 lines
# limits indicates the scale should be filled from -1 to 1
# use the minimal theme so there are no extras in the plot
# make the x and y labels blank
ggplot(econMelt, aes(x=x, y=y)) +geom_tile(aes(fill=Correlation))+
 scale_fill_gradient2(low=muted("red"), mid="white",high="steelblue",
 guide=guide_colorbar(ticks=FALSE, barheight=10),limits=c(-1, 1))+
  theme_minimal()+labs(x=NULL, y=NULL)

#Missing data is just as much a problem with cor as it is with mean and var, but is dealt
#with differently because multiple columns are being considered simultaneously
#Instead of specifying na.rm=TRUE to remove NA entries, one of "all.obs", "complete.obs",
#"pairwise.complete.obs", "everything" or "na.or.complete" is used.
m <- c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4)
n <- c(2, NA, 1, 6, 6, 4, 1, 1, 6, 7)
p <- c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9)
q <- c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2)
r <- c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10)
# combine them together
theMat <- cbind(m, n, p, q, r)   # cbines----> column bind

#The first option for use is "everything", which means that the entirety of all
#columns must be free of NAs; otherwise the result is NA.
cor(theMat, use="everything")

#With the option—"all.obs"— even a single NA in any column will cause an error.
cor(theMat, use="all.obs")

#The "complete.obs" and "na.or.complete"—work similarly to each other in 
#that they keep only rows where every entry is not NA.
#That means our matrix will be reduced to rows 1, 4, 7, 9 and 10, and then have its correlation
#computed. The difference is that "complete.obs" will return an error if not a
#single complete row can be found, while "na.or.complete" will return NA in that case.
cor(theMat, use="complete.obs")
cor(theMat, use="na.or.complete")

#The final option is "pairwise.complete", which is much more inclusive. It
#compares two columns at a time and keeps rows—for those two columns—where neither
#entry is NA. same as "complete.obs".
cor(theMat, use="pairwise.complete.obs")

?ggpairs
GGally::ggpairs(tips, package="reshape2") 
#tips data from the reshape2 package
#This shows every pair of variables in relation to each other building either
#histograms, boxplots or scatterplots depending on the combination of continuous and
#discrete variables.

#T-test
?tips
head(tips)
skim(tips) #skimr package
unique(tips$sex) # sex of the bill payer
unique(tips$day) # day of the week

#one sample t-test
#conduct a one-sample t-test on whether the average tip is equal to $2.50
t.test(tips$tip, alternative="two.sided", mu=2.50)  # mu---> muea

## build a t distribution
randT <- rt(30000, df=NROW(tips)-1) # df--> degree of freedom
# get t-statistic and other information
tipTTest <- t.test(tips$tip, alternative="two.sided", mu=2.50)
# plot it
ggplot(data.frame(x=randT)) + geom_density(aes(x=x), fill="grey", color="grey") +
   geom_vline(xintercept=tipTTest$statistic) +
 geom_vline(xintercept=mean(randT) + c(-2, 2)*sd(randT), linetype=2)
#last line is for dotted lines of +/- 2


#conduct a one-sided t-test to see if the mean is greater than $2.50.
t.test(tips$tip, alternative="greater", mu=2.50)

#Two-Sample T-Test -  used for comparing two samples
#Continuing with the tips data, we compare how female and male diners tip.
#Before running the t-test, we first need to check the variance of each sample.

# calculate the variance of tip for each level of sex
aggregate(tip ~ sex, data=tips, var)


#test for normality of tip distribution
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])

# all the tests fail so inspect visually
ggplot(tips, aes(x=tip, fill=sex)) + geom_histogram(binwidth=.5, alpha=1/2)

#Since the data do not appear to be normally distributed, neither the standard F-test (via
#the var.test function) nor the Bartlett test (via the bartlett.test function) will suffice. 
#So we use the nonparametric Ansari-Bradley test to examine the equality of variances.
ansari.test(tip ~ sex, tips)
?ansari.test
#This test indicates that the variances are equal, meaning we can use the standard
#two-sample t-test.
t.test(tip ~ sex, data=tips, var.equal=TRUE)
# setting var.equal=TRUE runs a standard two sample t-test
# var.equal=FALSE (the default) would run the Welch test
#female and male diners tip roughly equally

#While all this statistical rigor is nice, a simple rule of thumb would be 
#to see if the two means are within two standard deviations of each other.
library(plyr)
#ddply is used to split the data according to the levels of sex. It then 
#applied the summarize function to each subset of the data.
tipSummary <- ddply(tips, "sex", summarize,
                     tip.mean=mean(tip), tip.sd=sd(tip),
                     Lower=tip.mean - 2*tip.sd/sqrt(NROW(tip)),
                     Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))
tipSummary


ggplot(tipSummary, aes(x=tip.mean, y=sex)) + geom_point() +
   geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)
#the confidence intervals overlapping, suggesting that the means for the 
#two sexes are roughly equivalent.

#ANOVA - comparing multiple groups
colnames(tips)
tipAnova <- aov(tip ~ day - 1, tips)   #aov---> for anova
#left side is the variable of interest and the right side contains the
#variables that control grouping.
tipIntercept <- aov(tip ~ day, tips) #recall discussion on categorical variables
tipAnova$coefficients
tipIntercept$coefficients
summary(tipAnova)

#Since the test had a significant p-value, we would like to see which group differed from
#the others. The simplest way is to make a plot of the group means and confidence intervals
#and see which overlap.

tipsByDay <- ddply(tips, "day", plyr::summarize,
                   tip.mean=mean(tip), tip.sd=sd(tip),
                   Length=NROW(tip),
                   tfrac=qt(p=.90, df=Length-1),
                   Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
                   Upper=tip.mean + tfrac*tip.sd/sqrt(Length))

ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
   geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)
#This shows that Sunday tips differ from Thursday and Friday tips.

#The use of NROW instead of nrow is to guarantee computation. Where nrow works
#only on data.frames and matrices, NROW returns the length of objects that
#have only one dimension.
nrow(tips)
NROW(tips)
nrow(tips$tip)
NROW(tips$tip)

#An alternative to the ANOVA is to fit a linear regression with one categorical variable
#and no intercept.

#SUMMARY#
#Means, variances and standard deviations are computed with #mean, var and sd, respectively. 
#Correlation and covariance are computed with cor and cov. 
#For t-tests t.test is used, while aov is used for ANOVA.
