library(foreign)

setwd ("C:/Econometrics Applications")

#Week 2

houseprice <- read.dta("hprice2.dta")


linerreg <- lm(price ~ sqrmtr + lotsize + bdrms, data=houseprice)


#week3

mba3 <- read.dta("mba3.dta")

