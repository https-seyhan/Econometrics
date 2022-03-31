# ------------------------------------------------------------------------------
#  PWT_80.R
#  Reads in Penn World Table 8.0, sets up spreadsheet for class, and does growth
#  accounting and related calculations.  Uses output-based GDP numbers.    
#  From:  http://www.rug.nl/research/ggdc/data/penn-world-table
#  Related information:  
#     rgdpe =  Expenditure-side real GDP at chained PPPs (in mil. 2005US$)
#     rgdpo  = Output-side real GDP at chained PPPs (in mil. 2005US$)
#  Reference: Nalewaik, BPEA, 2011 
#  Adapted from programs by Paul Backus (Oct 12) and Patrick Toche (Jul 13).
#  Patrick's contact info:  Faculty of Business, University of Saint Joseph, 
#  Rua de Londres 16, Macau, China, patrick.toche@usj.edu.mo
#  Additional comments by Dave Backus (dbackus@stern.nyu.edu)
# ------------------------------------------------------------------------------
# clear memory, set directory for output 
rm(list=ls())
# remember to change \ to /
dir = "c:/Users/dbackus/Documents/Papers/BCH/data/PWT"

#  1. Get data 
# ------------------------------------------------------------------------------
require(xlsx)
library(gdata)
#pwdata<- "pwt81.xlsx"
#pwt <- read.xls("C:/Macroeconomics Analysis 1/Assigment/Assignment/pwt81.xlsx", sheet="Data")
pwt <-read.csv("pwt81.csv", sep=",", header=TRUE)
summary(pwt)

# 2. Create variables and csv file for class    
# ------------------------------------------------------------------------------
# select variables of interest
variables <- c("country", "countrycode", "year", "rgdpe", "rgdpo", "emp", "pop", "avh",  
               "ck")

pwt <- pwt[variables]

# create variables for class with understandable labels 
# NB:  output, emplmbesammleoyment, etc in millions 
pwt$Y    <- pwt$rgdpo 
pwt$POP  <- pwt$pop 
pwt$YPOP <- pwt$Y/pwt$pop
pwt$YL   <- pwt$Y/pwt$emp 
pwt$LPOP <- pwt$emp/pwt$pop 
pwt$KY   <- pwt$ck/pwt$Y 
pwt$KL   <- pwt$ck/pwt$emp 
pwt$hours<- pwt$avh 
pwt$TFP  <- pwt$YL/pwt$KL^(1/3) 

# check to see if reasonable 
mean(pwt$KY, na.rm=TRUE)
summary(pwt$KY, na.rm=TRUE)

# variables to keep for spreadsheet 
variables <- c("country", "countrycode", "year", "POP", "LPOP", "YPOP", "YL", "KL", "KY", 
               "hours", "TFP")

pwt_some <-pwt[variables]
# write csv file 
write.csv(pwt_some, file="pwt80.csv", row.names=FALSE)

# save as rdata file for easy access later
pwt <- pwt_some 
save(pwt, file="pwt_global81.RData")

# 3. Level and growth accounting 
# ------------------------------------------------------------------------------
# you can start the program here once you've set up the databases 
# need to set dir first 
rm(list=ls())
load("pwt_global81.RData")

# plots
plot_YL <- function(data, country) {
  # select country 
  data <- data[data$countrycode == country,]
  head(data)
  
  # plot
  plot(data$year, data$YL/1000,  # convert to thousands  
       , type="l",cex=1.5, lwd=2, col="red",
       main="", xlab="", ylab="GDP per worker (000s of 2005 USD)", 
       #     ylim=c(10, 90),
       mar=c(2,4,2,2),   # better than default in most cases 
       mgp=c(2.5,1,0)  
  )
  
  mtext(country, side=3, adj=0, line=1.0, cex=1.25)
  dev.print(device=pdf, file=paste(country,"_YL.pdf",sep=""),width=8,height=6)
}

plot_YL(pwt,c("USA"))
plot_YL(pwt,c("TUR"))

# level comparison 
level_comp <-function(pwt, countries, years) {
  
  # subset data
  data <- subset(pwt, countrycode %in% countries & year %in% years)
  # compute ratios 
  ratioYL <- data$YL[1]/data$YL[2]
  ratioKL <- data$KL[1]/data$KL[2]
  ratioA  <- data$TFP[1]/data$TFP[2]
  print(data$A[1]/data$A[2])

  # collect in matrix and return 
  country1 <- c(data$YL[1], data$KL[1], data$TFP[1])
  country2 <- c(data$YL[2], data$KL[2], data$TFP[2])
  ratio  <- c(ratioYL, ratioKL, ratioA)
  contri <- c(ratioYL, ratioKL^(1/3), ratioA)
  print(c("YL", "KL", "TFP"))
  return(rbind(country1, country2, ratio, contri))
}

# test 
#level_comp(pwt, c("MEX", "USA"), c("2011"))

# growth accounting 
growth_acc <- function(data, code, years) {
  # subset data
  data <- subset(data, countrycode %in% code & year %in% years)
  head(data)
 
  # compute growth rates 
  gYL <- (log(data$YL[2])-log(data$YL[1]))/(data$year[2]-data$year[1])
  gKL <- (log(data$KL[2])-log(data$KL[1]))/(data$year[2]-data$year[1])
  gA  <- (log(data$TFP[2])-log(data$TFP[1]))/(data$year[2]-data$year[1])

  # collect in matrix and return 
  date1 <- c(data$YL[1], data$KL[1], data$TFP[1])
  date2 <- c(data$YL[2], data$KL[2], data$TFP[2])
  growth <- c(gYL, gKL, gA)
  
  contri <- c(gYL, gKL/3, gA)
  #  print(growth)
  #  print(contri)
  return(rbind(date1, date2, growth, contri))
}

# test
growth_acc(pwt, c("USA"), c("1950", "2011"))

# plots
plot_YL(pwt,c("USA"))
plot_YL(pwt,c("ITA"))
plot_YL(pwt,c("FRA"))
plot_YL(pwt,c("TUR"))
plot_YL(pwt,c("ZAF"))
plot_YL(pwt,c("IDN"))
plot_YL(pwt,c("ZWE"))
plot_YL(pwt,c("KOR"))
plot_YL(pwt,c("JPN"))
plot_YL(pwt,c("CHN"))
plot_YL(pwt,c("IND"))
plot_YL(pwt,c("VEN"))
plot_YL(pwt,c("ARG"))
plot_YL(pwt,c("CHL"))
plot_YL(pwt,c("IDN"))
plot_YL(pwt,c("VNM"))
plot_YL(pwt,c("SYR"))
plot_YL(pwt,c("CUB"))

# level comparisions
level_comp(pwt, c("MEX", "USA"), c("2011")) 
level_comp(pwt, c("CHN", "IND"), c("2011"))

# growth accounting 
growth_acc(pwt, c("USA"), c("1950", "2011")) 

growth_acc(pwt, c("KOR"), c("1960", "2011"))

growth_acc(pwt, c("ITA"), c("1990", "2000"))
growth_acc(pwt, c("ITA"), c("2000", "2011"))

growth_acc(pwt, c("CHN"), c("1952", "1978")) 
growth_acc(pwt, c("CHN"), c("1978", "2011"))

growth_acc(pwt, c("IND"), c("1950", "1985")) 
growth_acc(pwt, c("IND"), c("1980", "2011"))

growth_acc(pwt, c("VNM"), c("1990", "2011"))

growth_acc(pwt, c("SYR"), c("1990", "2011"))

# VietNam, fall 2013 
data <- pwt[pwt$countrycode == "VNM",]
plot(data$year, data$YL/1000, 
     , type="l",cex=1.5, lwd=2, col="red",
     main="", xlab="", ylab="GDP per worker (000s of 2005 USD)", 
     #     ylim=c(2, 10),
     mar=c(2,4,2,2),   # better than default in most cases 
     mgp=c(2.5,1,0)  
)

dev.print(device=pdf, file="VNM_YL.pdf", width=8, height=6)

# India and Pakistan, midterm, spring 2013 
data <- pwt[pwt$countrycode == "PAK",]

plot(data$year, data$YL/1000, 
     , type="l",cex=1.5, lwd=2, col="red",
     main="", xlab="", ylab="GDP per worker (000s of 2005 USD)", 
     ylim=c(2, 10),
     mar=c(2,4,2,2),   # better than default in most cases 
     mgp=c(2.5,1,0)  
)

data <- pwt[pwt$countrycode == "IND",]
lines(data$year, data$YL/1000, type="l",cex=1.5, lwd=2, col="blue") 
text(x=1965, y=4, "Pakistan") 
text(x=1980, y=2, "India") 
#dev.print(device=pdf, file="PAKIND_YL.pdf", width=8, height=6)
