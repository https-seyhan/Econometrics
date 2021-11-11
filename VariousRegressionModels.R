library(foreign)
library(ggplot2)
library(lattice)
library(quantreg)
library(lmtest)
library(car)
library(AER)
library(systemfit)
library(sem)

# OLS, IVR, TLS models
setwd("C:/Econometrics Applications")

nlsy13 <- read.dta("nlsy13.dta")
summary(nlsy13)

olsreg <- lm(lwage ~ educ, data= nlsy13)
print(olsreg)
summary(olsreg)

#95% confidence intervals
cbind(CO = coef(olsreg),  confint(olsreg))

coef(run.Pooledfare)+as.matrix(sqrt(diag(Pooledfareavcov))*qnorm(0.975))%*%c(-1,1)
coef(run.Pooledfare)+as.matrix(sqrt(diag(Pooledfarebvcov))*qnorm(0.975))%*%c(-1,1)

#Question2
olsreg2 <- lm(lwage ~ educ + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18, data= nlsy13 )
summary(olsreg2)

cbind(CO = coef(olsreg2),  confint(olsreg2))

#Question3

olsreg3 <- lm(educ ~  CUFees + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18, data= nlsy13)
summary(olsreg3)
educhat <- fitted(olsreg3)

#Question4
olsreg4 <- lm(educ ~ CUFees, data=nlsy13)
summary(olsreg4)

#Question 5
olsreg5 <- lm(lwage ~ educhat + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18, data= nlsy13)
summary(olsreg5)
cbind(CO = coef(olsreg5),  confint(olsreg5))

#Question6
tls6 <- tsls(lwage ~ educ + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18, ~ CUFees + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18, data= nlsy13)
summary(tls6)
cbind(CO = coef(tls6),  confint(tls6))

ivreg6 <-  ivreg(lwage ~ educ + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18 | CUFees + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18, data= nlsy13)
summary(ivreg6)
cbind(CO = coef(ivreg6),  confint(ivreg6))

#Question 8
olsreg8 <-  lm(lwage ~  educ + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18 + abil, data= nlsy13)
summary(olsreg8)

cbind(CO = coef(olsreg8),  confint(olsreg8))

tls9 <- tsls(lwage ~ educ + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18 + abil, ~ CUFees + exper + expersq + nc + south + west + nc18 + south18 + west18 + urban + urban18 + abil, data= nlsy13)
summary(tls9)
cbind(CO = coef(tls9),  confint(tls9))
