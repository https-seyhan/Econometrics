library(foreign)
library(ggplot2)
library(lattice)
library(quantreg)
library(lmtest)
library(quantreg)
library(car)
library(bstats)
library(texreg)
library(rms)
library(erer)
library(mlogit)
library(MASS)
library("sampleSelection")

rm(list=ls()) #clean memory
setwd ("C:/Econometrics Applications")

drinks <- read.dta ("drinks10.dta")
summary(drinks)

#show tables
table(drinks$drinker)
table(drinks$age40s)
table(drinks$age50s)
table(drinks$married)
table(drinks$edba)
table(drinks$dchron)

drinks2 <- within (drinks, {drinker <-  factor(drinker)})
summary(drinks2)

ologit <- lrm(drinker ~ age40s + age50s + ednchs + edba + married + dchron, data = drinks)
print(ologit)
summary(ologit)

t2 <- polr(drinker ~ age40s + age50s + ednchs + edba + married + dchron, data = drinks2)
print(summary(ologit2))

ea <- maBina(w = ologic, x.mean = TRUE, rev.dum = TRUE)
mfx(ologit)
mfx(probit1)
summary(ologit)
anova(ologit,test="Chisq")

mfx<-function(x){
  bb<-data.frame(x$coefficients)
  bb2<-data.frame(mean(x$data,na.rm=T))
  bbh<-as.character(rownames(bb))
  bbmeans<-bb2[bbh[2:nrow(bb)],]
  jkj <-bb[2:nrow(bb),]*bbmeans
  M3<-sum(jkj) + bb[1,]
  
  if (ll<-x$family$link=="probit"){
    probitmfx <- data.frame(mfx=dnorm(M3)*bb[2:nrow(bb),],row.names=bbh[2:nrow(bb)])
  }
  else{
    probitmfx <- data.frame(mfx=dlogis(M3)*bb[2:nrow(bb),],row.names=bbh[2:nrow(bb)])
  }
  bbse<-bb/summary(x)$coef[,2]
  mfxse<-probitmfx/bbse[2:nrow(bb),]
 
  probitmfxfull <- data.frame(mfx=probitmfx,SE=mfxse,bbmeans,summary(x)$coef[2:nrow(bb),3],summary(x)$coef[2:nrow(bb),4],row.names=bbh[2:nrow(bb)])
  colnames(probitmfxfull) <- c("mfx","SE","Mean Value","z","Pr(>|z|)")
  
  logl <- 0.5*(-x$aic + 2*nrow(bb))
  depen <- x$data[,1]
  depenglm <- glm(depen ~ 1, family=binomial(link=x$family$link),data=x$data)
  logldepen <- 0.5*(-depenglm$aic + 2)
  psr2<- 1 - (logl/logldepen)

  obs<-nrow(x$data)
  #SBC/BIC
  sbc<- -2*logl + log(obs)*nrow(bb)
  #HQIC
  HIC<- -2*logl + 2*log(log(obs))*nrow(bb)
  #CDF of Mean Model
  if (ll == TRUE){
    BigProb<-pnorm(M3)
  }
  else {BigProb<-plogis(M3)}
  
  #LR Test
  LRTest<- -2*(logl - logldepen)
  dfLR<-nrow(bb) - 1
  LRp<-dchisq(LRTest,df=dfLR)
  LRdata <- data.frame(LRTest, dfLR,LRp)
  colnames(LRdata) <- c("Test Statistic","DF","P-Value")
  rownames(LRdata) <- "LR Test" #Likelihood-ratio test
  
  tests<-rbind(BigProb,logl,psr2,x$aic,HIC,sbc)
  tests<-data.frame(tests)
  
  colnames(tests)<-""
  rownames(tests)<-c("CDF(Evaluated at the Mean):","Log-Likelihood:","McFadden's R2:","Akaike Information Criterion:","Hannan-Quinn Criterion:","Schwarz's Bayesian Criterion:")
 
  cat("MFX Function for Logit and Probit", "\n")
  cat("", "\n")
  if (qq<-x$family$link=="logit"){ 
 
    cat("This is a Logit Model","\n")
  }
  else if (qq<-x$family$link=="probit"){ 
    cat("This is a Probit Model","\n")
  }
  else {
    cat("","\n")
  }
  
  cat("", "\n")
  cat("Reporting Marginal Effects, Evaluated at the Mean", "\n")
  cat("", "\n")
  printCoefmat(probitmfxfull, P.value=TRUE, has.Pvalue=TRUE)
  cat("", "\n")
  cat("Observations:", obs, "\n")
  cat("", "\n")
  printCoefmat(tests, P.value=F, has.Pvalue=F)
  cat("", "\n")
  cat("Likelihood-Ratio Test:", "\n")
  printCoefmat(LRdata, P.value=T, has.Pvalue=T)
  cat("", "\n")
}
