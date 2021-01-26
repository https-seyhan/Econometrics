library(foreign)
library(ggplot2)
library(lattice)

setwd ("C:/Econometrics Applications")

bweight <- read.dta ("bweight4.dta")

write.csv(bweight, file="bweight.csv", sep=",", row.names=FALSE) # This line is a one off run

bweight <- read.table("bweight.csv", sep=",", header=TRUE)

#Regression in question ii)
regressweight <- lm(bwght~ mage + mage2, data=bweight)
summary(regressweight)

#Regression in question v)
regressweight <- lm(bwght~ mage + mage2 + nanvis + nanvis2, data=bweight)
rs <- summary(regressweight)
res <- residuals(regressweight)

fitted.regressweight <- fitted(regressweight) # predicted values

new <- data.frame(mage=30, mage2=900, nanvis=12, nanvis2=144)

predict(regressweight, new, se.fit=TRUE, interval="confidence", level=0.90, cond=TRUE)

#Regression in question v)
regressweight2 <- lm(bwght~ mage + mage2 + nanvis + nanvis2, data=bweight)
summary(regressweight2)

(summary(regressweight2)$sigma)

regressweight20 <- lm(bwght~ mage0 + mage20 + nanvis0 + nanvis20, data=bweight)
summary(regressweight20)

predict(regressweight20, new, se.fit=TRUE, interval="confidence", level=0.90, cond=FALSE)

#Regression of log(btwght) in question ix)
regressweight3 <- lm(log(bwght)~ mage + mage2 + nanvis + nanvis2, data=bweight)
summary(regressweight3)

fitted.regressweight3 <- exp(fitted(regressweight3)) # predicted values


coef(regressweight3)
res3 <- residuals(regressweight3)

(summary(regressweight3)$sigma)
(summary(regressweight)$sigma)

