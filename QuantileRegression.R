library(foreign)
library(ggplot2)
library(lattice)
library(quantreg)
library(lmtest)
library(car)

setwd ("C:/Econometrics Applications")
loandata <- read.dta ("loanapp5.dta")

olsreg <- lm(approve~ white, data=loandata)
summary(olsreg)
coef(olsreg)

c(1,1)%*%olsreg$coef
c(1,0)%*%olsreg$coef
by(loandata$approve, loandata$white, mean)

obrat, loanprc, unem, male, married, dep, sch and cosign

olsreg2 <- lm(approve~ white + obrat+ loanprc + unem + male + married + dep + sch + cosign, data=loandata)
summary(olsreg2)
coef(olsreg2)
bptest(olsreg2)

#GLM
glm2a <- glm(approve ~ white, family=binomial(link=probit), data=loandata)
summary(glm2a)

pnorm(c(1,1)%*%glm2a$coef)
pnorm(c(1,0)%*%glm2a$coef)

#Quantile Regression
quantreg25 <- rq(approve ~ white, data=loandata, tau=0.25)
summary(quantreg25)

quantreg50 <- rq(approve ~ white, data=loandata, tau=0.5)
summary(quantreg50)

quantreg75 <- rq(approve ~ white, data=loandata, tau=0.75)
summary(quantreg75)

#Similtaneus Quantile Regression
quantreg2575 <- rq(approve ~ white, data=loandata, tau=c(0.25, 0.75))
summary(quantreg2575)

#Plotting data
quantreg.all <- rq(approve ~ white, tau= seq(0.05, 0.95, by=0.05), data=loandata)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)
