#Logistic Regression#

library(tidyverse)
data_1<-read_csv("files/Simmons.csv") #use your file path
head(data)
str(data)

coupon1<-glm(Coupon~Spending+Card, data=data_1, family=binomial(link ="logit"))
summary(coupon1)

library(coefplot)
coefplot(coupon1)
#note the value of null deviance , residual deviance, and null-residual deviance

invlogit <- function(x)
  {
     1 / (1 + exp(-x))
}
#Interpreting the coefficients from a logistic regression necessitates taking the
#inverse logit.

 invlogit(coupon1$coefficients)
