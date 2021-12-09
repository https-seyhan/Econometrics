library(foreign)
library(ggplot2)
library(lattice)
library(quantreg)
library(lmtest)
library(quantreg)
library(car)
library(bstats)
library(texreg)
library("sampleSelection")

# Probit regression model in R
rm(list=ls()) #clean memory
setwd ("C:/Econometrics Applications")

mroz <- read.dta ("mroz9.dta")
summary(mroz)
#Perform LPM using OLS
regressprice16 <- lm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6,  data=mroz)
summary(regressprice16)

regressionprobit18 <- glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, family = binomial(link=probit), data = mroz)
summary(regressionprobit18)

regressionprobit18_new <- glm(inlf ~ nwifeinc + educ + exper + expersq + age , family = binomial(link=probit), data = mroz)
summary(regressionprobit18_new)

anova(regressionprobit18_new ,regressionprobit18, test="Chisq")

probit(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, data = mroz)
summary(mroz)
