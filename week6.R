library(foreign)
library(ggplot2)
library(lattice)
library(quantreg)
library(lmtest)
library(car)

setwd ("C:/Econometrics Applications")

wagedata <- read.dta ("wage6.dta")
