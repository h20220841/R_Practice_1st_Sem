#LINEAR MODELS#

#Simple linear regression#

#response = dependent and predictor = independent
library(tidyverse)
data(father.son, package='UsingR')
head(father.son)

#scatter plot with regression line
ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point() +
   geom_smooth(method="lm", se=TRUE) + labs(x="Fathers", y="Sons")
#The blue line running through the points is the regression line 
#and the gray band (se) around it represents the uncertainty in the fit.

#regression using 'lm' function
heightsLM <- lm(sheight ~ fheight, data=father.son)
heightsLM
summary(heightsLM)

#ANOVA Alternative
#An alternative to running an ANOVA test is to fit a regression
#with just one categorical variable and no intercept term.
data(tips, package="reshape2")
head(tips)
tipsAnova <- aov(tip ~ day - 1, data=tips)
coefficients(tipsAnova)
# putting -1 in the formula indicates that the intercept should not be
# included in the model;
# the categorical variable day is automatically setup to have a
# coefficient for each level
tipsLM <- lm(tip ~ day - 1, data=tips)
summary(tipsAnova)
summary(tipsLM)

#Visualizing the coefficients and standard errors should show
#the same results as computing them using the ANOVA formula.

#calculate the means and CI manually
library(dplyr)
tipsByDay <- tips %>%
  group_by(day) %>%
   dplyr::summarize(
     tip.mean=mean(tip), tip.sd=sd(tip),
     Length=NROW(tip),
     tfrac=qt(p=.90, df=Length-1),
     Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
     Upper=tip.mean + tfrac*tip.sd/sqrt(Length))

# now extract them from the summary for tipsLM
 tipsInfo <- summary(tipsLM)
tipsCoef <- as.data.frame(tipsInfo$coefficients[, 1:2])
tipsCoef <- within(tipsCoef, {
   Lower <- Estimate - qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
   Upper <- Estimate + qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
   day <- rownames(tipsCoef)})

# plot them both
 ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
   ggtitle("Tips by day calculated manually")

ggplot(tipsCoef, aes(x=Estimate, y=day)) + geom_point() +
   geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
   ggtitle("Tips by day calculated from regression model")

#within, which is similar to with in that it lets us refer to columns in a data.frame by name but
#different in that we can create new columns within that data.frame, hence the name.
#This function has largely been superceded by mutate in dplyr but is still good to know.
#Second, one of the columns was named Std. Error with a space. In order to refer to a
#variable with spaces in its name, even as a column in a data.frame, we must enclose the
#name in back ticks (`).

#Multiple Regression
housing <- read.table("housing.csv",
                       sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)
#stringsAsFactors leaves character columns as they are and does not convert them
#to factors
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                     "SqFt", "Income", "IncomePerSqFt", "Expense",
                     "ExpensePerSqFt", "NetIncome", "Value",
                     "ValuePerSqFt", "Boro")
head(housing)
#The first step is to visualize the data in some exploratory data analysis. The
#natural place to start is with a histogram of ValuePerSqFt,
ggplot(housing, aes(x=ValuePerSqFt)) +
   geom_histogram(binwidth=10) + labs(x="Value per Square Foot")
#Histogram of value per square foot for NYC condos. It appears to be bimodal.

#Mapping color to Boro
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
   geom_histogram(binwidth=10) + labs (x="Value per Square Foot")

#faceting on Boro in Figure reveal that
#Brooklyn and Queens make up one mode and Manhattan makes up the other, while there
#is not much data on the Bronx and Staten Island.
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
   geom_histogram(binwidth=10) + labs (x="Value per Square Foot") +
   facet_wrap(~Boro)

#histograms for square footage and the number of units.
ggplot(housing, aes(x=SqFt)) + geom_histogram()
ggplot(housing, aes(x=Units)) + geom_histogram()

#The distributions are highly right skewed in the top two graphs, so they were repeated after removing buildings with more than
#1,000 units.
ggplot(housing[housing$Units < 1000, ], aes(x=SqFt)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ], aes(x=Units)) + geom_histogram()

#Plotting scatterplots of the value per square foot versus both number
#of units and square footage, with and without those outlying buildings, gives us an idea
#whether we can remove them from the analysis.

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000, ], aes(x=SqFt, y=ValuePerSqFt)) +
   geom_point()
ggplot(housing[housing$Units < 1000, ], aes(x=Units, y=ValuePerSqFt)) +
   geom_point()

# how many need to be removed?
sum(housing$Units >= 1000)
# remove them
housing <- housing[housing$Units < 1000, ]

#Even after we remove the outliers, taking the log of square footage and number of units might prove helpful.
 # plot ValuePerSqFt against SqFt
 ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
 ggplot(housing, aes(x=log(SqFt), y=ValuePerSqFt)) + geom_point()
 ggplot(housing, aes(x=SqFt, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=log(ValuePerSqFt))) +
   geom_point()

#plot ValuePerSqFt against Units
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(Units), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(Units), y=log(ValuePerSqFt))) +geom_point()

#Fitting the model uses the formula interface in lm.for multiple regression
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
summary(house1)

library(coefplot)
coefplot(house1)
#Figure shows the result, where each coefficient is plotted as a point with a thick line
#representing the one standard error confidence interval and a thin line representing the
#two standard error confidence interval.

#Interactions between variables can be equally powerful. To enter them in a formula, separate the
#desired variables with a * instead of +. Doing so results in the individual variables plus the
#interaction term being included in the model. 
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
summary(house2)

house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data=housing)
summary(house3)

#If three variables all interact together, the resulting coefficients will be the three
#individual terms, three two-way interactions and one three-way interaction.
house4 <- lm(ValuePerSqFt ~ SqFt*Units*Income, housing)
summary(house4)

#Interacting a continuous variable like SqFt with a factor like Boro results in
#individual terms for the continuous variable and each non-baseline level of the factor
#plus an interaction term between the continuous variable and each non-baseline level of
#the factor. Interacting two (or more) factors yields terms for all the individual
#non-baseline levels in both factors and an interaction term for every combination
#of non-baseline levels of the factors.
house5 <- lm(ValuePerSqFt ~ Class*Boro, housing)
summary(house5)

#Neither SqFt nor Units appear to be significant in any model when viewed in a
#coefficient plot. However, zooming in on the plot shows that the coefficients for Units
#and SqFt are non-zero.
coefplot(house1, sort='mag') + scale_x_continuous(limits=c(-.25, .1))
coefplot(house1, sort='mag') + scale_x_continuous(limits=c(-.0005, .0005))

#This is likely a scaling issue. This can be resolved by standardizing, or scaling, the variables.
#Whereas before a coefficient was the change in the response corresponding
#to a one-unit increase in the predictor, the coefficientis now the change in the response
#corresponding to a one-standard-deviation increase in the predictor. Standardizing can be
#performed within the formula interface with the scale function.

house1.b <- lm(ValuePerSqFt ~ scale(Units) + scale(SqFt) + Boro,
                 + data=housing)
coefplot(house1.b, sort='mag')
summary(house1.b)
#each change in the standard deviation of SqFt there is a change of 21.95 in ValuePerSqFt.

#visualizing the coefficients from multiple models is a handy tool.
# also from the coefplot package
multiplot(house1, house2, house3)

#Regression is often used for prediction, which in R is enabled by the predict function
housingNew <- read.table("files/housingNew.csv",
                          sep=",", header=TRUE, stringsAsFactors=FALSE)
# make prediction with new data and 95% confidence bounds
housePredict <- predict(house1, newdata=housingNew, se.fit=TRUE,
                          + interval="prediction", level=.95)
# view predictions with upper and lower bounds based on standard errors
head(housePredict$fit)













