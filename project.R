#ECMT6002 Applied Project

library(foreign)
library(ggplot2)
library(lattice)
library(quantreg)
library(lmtest)
library(quantreg)
library(car)
library(bstats)
library(texreg)

setwd ("~/Econometrics Applications")

houseprice <- read.dta ("hpricen.dta")
summary(houseprice)


#standard dev
sd(houseprice$price)
sd(houseprice$crime)
sd(houseprice$nox)
sd(houseprice$rooms)
sd(houseprice$dist)
sd(houseprice$proptax)
sd(houseprice$disadv)

houseprice <- read.table("hpricen.csv",sep=",", header=TRUE)
summary(houseprice)

####################################

regressprice4 <- lm(log(price) ~ log(nox) + crime + rooms + dist + disadv, data=houseprice)
summary(regressprice4)
anova(regressprice4)

####################################


#Correlation bwtween price, nox , and price, crime
cor(houseprice$price, houseprice$nox)
cor(houseprice)

regressprice <- lm(log(price) ~ log(nox) + crime + rooms + dist + disadv, data=houseprice)
summary(regressprice)
anova(regressprice)

#Restricted regression for question 6

regresspriceRestricted <- lm(log(price) ~ log(nox) , data=houseprice)
summary(regresspriceRestricted)
anova(regresspriceRestricted)

######################################

cor(log(houseprice$price), houseprice$proptax)
cor(houseprice)


###################Question 8###################
regressprice8 <- lm(log(price) ~ log(nox) + crime + rooms + dist + disadv + proptax, data=houseprice)
summary(regressprice8)

###################Question 9###################

new <- data.frame(nox= 5.473947, crime=2.5, rooms=6, dist=4, disadv=15, proptax=38)
predict(regressprice8, new, se.fit=TRUE, interval="confidence", level=0.99, cond=TRUE) # conditional
predict(regressprice8, new, se.fit=TRUE, interval="confidence", level=0.99, cond=FALSE)

###################Question 10###################

#transform variables
houseprice$lognox <- (log(houseprice$nox) - 1.7)
houseprice$crime2.5 <- (houseprice$crime - 2.5)
houseprice$rooms6 <- (houseprice$rooms - 6)
houseprice$dist4 <- (houseprice$dist - 4)
houseprice$disadv15 <- (houseprice$disadv - 15)
houseprice$proptax38 <- (houseprice$proptax - 38)

regressprice9 <- lm(log(price) ~ lognox + crime2.5 + rooms6 + dist4 + disadv15 + proptax38, data=houseprice)
summary(regressprice9)
(summary(regressprice9)$sigma) # variance

#Below gives the within sample confidence interval at 99%.
new <- data.frame(nox= 5.473947, crime=2.5, rooms=6, dist=4, disadv=15, proptax=38)
predict(regressprice8, new, se.fit=TRUE, interval="confidence", level=0.99, cond=TRUE) # conditional

###################Question 11###################

new <- data.frame(lognox= 1.7, crime2.5=2.5, rooms6=6, dist4=4, disadv15=15, proptax38=38)
predict(regressprice8, new, se.fit=TRUE, interval="confidence", level=0.99, cond=FALSE) # Unconditional

###################Question 12###################
regressprice12 <- lm(price ~ log(nox) + crime + rooms + dist + disadv + proptax, data=houseprice)
summary(regressprice12)

new <- data.frame(nox= 5.473947, crime=2.5, rooms=6, dist=4, disadv=15, proptax=38)
predict(regressprice12, new, se.fit=TRUE, interval="confidence", cond=TRUE) # conditional

###################Question 13###################

regressprice13 <- lm(log(price) ~ log(nox) + crime + rooms + dist + disadv + proptax, data=houseprice)
summary(regressprice13)

resettest (regressprice13 , power=2:3, type="regressor")


###################Question 14###################
regressprice14 <- lm(log_price ~ log_nox + crime + rooms + dist + disadv + proptax, data=houseprice)
summary(regressprice14)
white.test(regressprice14)


###################Question 15###################

taus <- c(.1, .25, .50, .75, .90)

#Quantile Regression
rq15 <- rq(log(price) ~ log(nox) + crime + rooms + dist + disadv + proptax, data=houseprice,  taus)
summary(rq15, se = "boot")
summary(rq15$coef)

summary(rq15$se)

plot(summary(rq15))

fit1 <- summary(rq(log(price) ~ log(nox) + crime + rooms + dist + disadv + proptax, data=houseprice, taus))

plot(fit1, parm=2)


###################Question 16###################

summary(houseprice)

#Create the indicator variable dpriceh

houseprice$dpriceh <- 0

houseprice$dpriceh  <- ifelse(houseprice$price > 22200, 1, 0)

#Perform LPM using OLS
regressprice16 <- lm(dpriceh ~ log(nox) + crime + rooms + dist + disadv + proptax, data=houseprice)
summary(regressprice16)



###################Question 18###################

regressionprobit18 <- glm(dpriceh ~ log(nox) + crime + rooms + dist + disadv + proptax , family = binomial(link=probit), data = houseprice)
summary(regressionprobit18)

###################Question 19###################

houseprice$dpriceh <- 0

houseprice$dpriceh  <- ifelse(houseprice$price > 22200, 1, 0)

#resticted model

regressionprobit19_unrestricted <- glm(dpriceh ~ log(nox) + crime + rooms + dist + disadv + proptax , family = binomial(link=probit), data = houseprice)
summary(regressionprobit19_unrestricted)

regressionprobit19_restricted <- glm(dpriceh ~  crime + rooms + dist + disadv + proptax , family = binomial(link=probit), data = houseprice)
summary(regressionprobit19_restricted)

anova(regressionprobit19_restricted ,regressionprobit19_unrestricted, test="Chisq")

