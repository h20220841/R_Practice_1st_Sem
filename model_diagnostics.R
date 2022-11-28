#Model Diagnostics##

#How do we judge the quality of a model?
#analysis of residuals, the results of an ANOVA test, drop-in deviance, the AIC, or BIC score

##residuals##
#if the model is appropriately fitted to the data, the residuals should be normally distributed as well.

# read in the housing data
library(tidyverse)
housing <- read.table("housing.csv", sep=",", header=TRUE,
                       stringsAsFactors=FALSE)
head(housing)
# give the data good names
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                       "SqFt", "Income", "IncomePerSqFt", "Expense",
                       "ExpensePerSqFt", "NetIncome", "Value",
                       "ValuePerSqFt", "Boro")
# eliminate some outliers
housing <- housing[housing$Units < 1000, ]
head(housing)

# fit a model
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing) #lm for linear regression
summary(house1)
coefficients(house1)

# visualize the model
 library(coefplot)
coefplot(house1)

#For linear regression, three important residual plots are: fitted values against residuals,
#Q-Q plots and the histogram of the residuals.

head(fortify(house1))
#notice .resid, .stdresid, predicted
# save a plot to an object
# notice we are using the created columns for the x- and y-axes # they are .fitted and .resid

 h1 <- ggplot(aes(x=.fitted, y=.resid), data = house1) +  geom_point() +
  geom_hline(yintercept = 0) +    geom_smooth(se = FALSE) +
   labs(x="Fitted Values", y="Residuals")
 # se - Display confidence interval around smooth (TRUE by default)
h1 # print the plot, This clearly shows a pattern in the 
#data that does not appear to be random.

h2 <- ggplot(aes(x=.fitted, y=.stdresid), data = house1) +    geom_point() +
  geom_hline(yintercept = 0) +    geom_smooth(se = FALSE) +
  labs(x="Fitted Values", y="Residuals")
h2 #plot of standardized residuals and the predicted values

#Plot of residuals versus fitted values for house1 colored by Boro.
h1 + geom_point(aes(color=Boro)) 
h2 + geom_point(aes(color=Boro))

#Q-Q plot
#If the model is a good fit, the standardized residuals should all
#fall along a straight line when plotted against the theoretical quantiles of the normal
#distribution.
ggplot(house1, aes(sample=.stdresid)) + stat_qq() + geom_abline()
#The tails drift away from the ideal theoretical line, indicating
#that we do not have the best fit.

# a histogram of the residuals.
ggplot(house1, aes(x=.resid)) + geom_histogram()
ggplot(house1, aes(x=.stdresid)) + geom_histogram()
#does not look normally distributed, meaning our model is incomplete.

##COMPARING MODELS##
#we will fit a number of models in order to compare them to each other.
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + Class,data=housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class,data=housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class, data=housing)
multiplot(house1, house2, house3, house4, house5, pointSize=2)
#This shows that only Boro and some condominium types matter.

#return a table of results including the residual sum of squares (RSS), 
#which is a measure of error, the lower the better
anova(house1, house2, house3, house4, house5)
#fourth model, house4
#RSS is that it always improves when an additional variable is added to the model. 
#This can lead to excessive model complexity and overfitting.

#Another metric, which penalizes model complexity, is the Akaike Information
#Criterion (AIC). As with RSS, the model with the lowest AIC—even negative values—is
#considered optimal. The BIC (Bayesian Information Criterion) is a similar measure where,
#once again, lower is better. 
AIC(house1, house2, house3, house4, house5)
BIC(house1, house2, house3, house4, house5)

#Stepwise Variable Selection#
#The step function iterates through possible models. The scope argument specifies a
#lower and upper bound on possible models. The direction argument specifies whether
#variables are just added into the model, just subtracted from the model or added and
#subtracted as necessary. 
#the mode of stepwise search, can be one of "both", "backward", or "forward", with a default of "both". 
#If the scope argument is missing the default for direction is "backward".
#When run, step prints out all the iterations it has taken to arrive at
#what it considers the optimal model.

# the lowest model is the null model, basically the straight average
nullModel <- lm(ValuePerSqFt ~ 1, data=housing)
# the largest model we will accept
fullModel <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Boro*Class, data=housing)
# try different models
# start with nullModel
# do not go above fullModel
# work in both directions
houseStep <- step(nullModel,scope=list(lower=nullModel, upper=fullModel),
                  direction="both")
# reveal the chosen model
 summary(houseStep)











