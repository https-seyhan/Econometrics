# Fitting Linear Models
library(foreign)
setwd ("C:/Econometrics Applications")

houseprice <- read.dta("hprice2.dta")
linerreg <- lm(price ~ sqrmtr + lotsize + bdrms, data=houseprice)
