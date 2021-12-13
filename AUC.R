setwd("C:/Econometrics/Lectures/Lec1/")

mytable <-read.table("stock.csv", sep=",", header=T)

setwd("C:/Comp/") # change the working directory

mytable <-read.table("kaggle_training_dataset_formatted2.txt", sep=",", header=T)
prediction1<-c(rep(2,10),rep(1,10))

lass1<-c(rep(1,5),rep(-1,5),rep(1,5),rep(-1,5))

AUCGINI<-function(prediction=prediction1,lass=lass1){
  totalA<-0
  totalB<-0
  valA<-1
  valB<--1
  i<-1

  prediction<-sort(prediction,decreasing=TRUE)
 
  while (!is.na(prediction[i])) {
    thisVal<-lass[i]
    if (thisVal==valA) {
      totalA<-totalA+1}
    else {if (thisVal==valB) {
      totalB=totalB+1}
          else {
            cat("Unspecified class -", thisVal,"\n" )}
    }
    i<-i+1
  }
  
  if ((totalA==0)|(totalB==0)){
    cat ("Only 1 Class")
  }
 
  j<-i
  next_is_same<-FALSE
  this_percent_A<-0
  this_percent_B<-0
  area1<-0
  countA<-0
  countB<-0
  k<-1

  for (k in 1:(j-1)){
    if (next_is_same==FALSE){
      last_percent_A<-this_percent_A
      last_percent_B<-this_percent_B
    }
   
    if ((lass[k]==valA) &(!is.na(lass[k]))) {
      countA<-countA+1}
    else {countB<-countB+1}
 
    next_is_same<-FALSE
    if (!is.na(prediction[k])&(!is.na(prediction[k+1]))&(k<j-1)) {
      if (prediction[k]==prediction[k+1]) {
        next_is_same<-TRUE}
    }
   
    if (next_is_same==FALSE) {
      this_percent_A<-countA/totalA
      this_percent_B<-countB/totalB
      triangle<-(this_percent_B-last_percent_B)*(this_percent_A-last_percent_A)*0.5
      rectangle<-(this_percent_B-last_percent_B)*last_percent_A
      A1<-rectangle+triangle
      area1<-area1+A1
    }
  }
  
  AUC<-area1
  GINI<-2*(AUC-0.5)
  cat("Records=", totalA+totalB,"\n")
  cat("AUC=",AUC,"\n")
  cat("GINI=",GINI,"\n")
}
AUCGINI()
