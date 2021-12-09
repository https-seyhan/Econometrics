library(foreign)
library(xlsReadWrite) 
library(ggplot2)

setwd ("C:/Econometrics Applications")

return <-read.table("return.csv",sep=",", header=TRUE)

returnmodel <- lm(return~dkr + eps + netinc + salary, data= return)
summary(returnmodel)

returnmodel <- lm(return~dkr + eps + lnetinc + lsalary, data= return)
summary(returnmodel)

mba <- read.dta("mba3.dta")
mbamodel <- lm(log(salary)~ testsc + wam + libsize + rank, data=mba)
summary(mbamodel)
anova(mbamodel)

mbamodel2 <- lm(log(salary)~ log(libsize) + rank, data=mba)
summary(mbamodel2)
