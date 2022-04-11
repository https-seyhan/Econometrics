# Libraries
library(randomForest) #Random Forest
library(e1071) #SVM
library(LiblineaR)
library(earth) # Additive Regression
library(Metrics)
library(mlbench)
library(caret)
library(AUCRF)
library(ROCR)
library(ggplot2)
library(reshape2)
library(outliers)
library(plyr)

setwd("~/Econometrics Applications")

data <- read.csv("~/Econometrics Applications/stock_data.csv", sep=",", header=T, stringsAsFactors= T)
summary(data)

boxplot(data[,-1])
corr.data<- findCorrelation(data, cutoff = 0.60)
names(corr.data) # There is no highy correlared prerdictors

#checking dimensions of data
dim(data)

#remove outliners
#findOutlier <- function(data, cutoff = 3) {
  ## Calculate the sd
#  sds <- apply(data, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
#  result <- mapply(function(d, s) {
#    which(d > cutoff * s)
#  }, data, sds)
#  result
#}

#outliers <- findOutlier(data)
#summary(outliers)
#removeOutlier <- function(data, outliers) {
#  result <- mapply(function(d, o) {
#    res <- d
#    res[o] <- NA
#    return(res)
#  }, data, outliers)
#  return(as.data.frame(result))
#}

#dataFilt <- removeOutlier(data, outliers)

#boxplot(dataFilt)
#Replace previous data with Filtered data
#data <- dataFilt

#Normalise Data
norm.data <- scale(data[,!(names(data) %in% c('TargetVariable'))]) # normalise all variables except targetVariable
summary(norm.data)
boxplot(norm.data)

TargetVariable <- as.factor(data[,c("TargetVariable")])
summary(TargetVariable)

plot(TargetVariable)
#Complete normalise dataset
merged.data <- cbind(norm.data, TargetVariable)
tt<- data.frame(merged.data)
tt$TargetVariable <- as.factor(tt$TargetVariable)
summary(tt)

data <- tt #Normalised Data
#Generate TargetVariable
#generateTarget <- function(row)
#{
#  if (row[101]== 1) "Down" else "Up"
#}

#data$TargetVariable <- apply(data,1,generateTarget)
#data$TargetVariable<-as.factor(data$TargetVariable)

#summary(data$TargetVariable)
summary(data)
#Divide Data into Training and Test
train <- data[1:2000, ]
summary(train)
test <- data [2000:3000,]
summary(test)

model_rf <- randomForest(TargetVariable~., data=train,  maxnodes=10, ntree=1500, nproximity=TRUE, importance=TRUE)
#model_rf <- randomForest(TargetVariable~ X6 + X7 + X9 + X12 + X18 + X20 + X23 + X24 + X25 + X29 + X32 + X33 + 
 #    X35 + X37 + X42 + X44 + X45 + X48 + X51 + X60 + X63 + X65 + X68
  #   + X69+ X76 + X77 + X82 + X83 + X89 + X90 + X93 + X98, maxnodes=10, ntree=1500,  nproximity=TRUE, importance=TRUE)
summary(model_rf$Length)

#model variables
names(model_rf)
print(model_rf$classes)

#plot MSE Error Rates
plot(model_rf, log="y")

#per-class error
model_rf$confusion[, 'class.error']
model_rf$confusion

#Show Training Classes
plot(model_rf$predicted)
###########################################################
#show training predictions
predictions <-as.vector(model_rf $votes[,2])
plot(predictions)
write.csv(predictions, file = "randomforestTrainingProbs.csv", row.names = FALSE)

pred <- prediction(predictions,train$TargetVariable)
perf_AUC <- performance(pred,"auc") #Calculate the AUC value
AUC <- perf_AUC@y.values[[1]]
perf_ROC <- performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Training AUC = ",format(AUC, digits=5, scientific=FALSE)))

#Plot important Variables
varImpPlot(model_rf,main=" Average Importance plots")
varImpPlot(model_rf,class="1",main=" Class= 1 Importance plots")
varImpPloSVMt(model_rf,class="-1",main=" Class= -1 Importance plots")

impVariables <- importance(model_rf,type = 1) 
rownames(impVariables)[which(impVariables > 1)]
#data.model_rf <- randomForest(Y~  X6 + X7 + X9 + X12 + X18 + X20 + X23 + X24 + X25 + X29 + X32 + X33 + X35 + X37 + X42 + X44 + X45 + X48 + X51 + X60 + X63 + X65 + X68
#                             + X69+ X76 + X77 + X82 + X83 + X89 + X90 + X93 + X98, maxnodes=20, ntree=1500)

#perf_ROC <- performance(pred, "fpr","tpr")
#plot (perf_ROC)

#perf_RP <- performance(pred, "rec","prec");
#plot (perf_RP);
#plot  (perf_RP@alpha.values[[1]],perf_RP@x.values[[1]])
#lines (perf_RP@alpha.values[[1]],perf_RP@y.values[[1]])
#lines (perf_ROC@alpha.values[[1]],perf_ROC@x.values[[1]])
##################################################################

margins.rf <- margin(model_rf)
plot(margins.rf )

#Predict Test data
preds_rf<-predict(object <- model_rf,test[,-101], type="prob") #remove type="prob" to see confusion matrix
write.csv(preds_rf, file = "randomforestTestinggProbs.csv", row.names = FALSE)

#Confusion Matrix
table(pred = preds_rf , true = test[,101])

#plot MSE
plot(preds_rf, log="y")

#Checking Test AUC
auc(preds,test$TargetVariable)

#plot test ppredictions
plot(preds_rf)
#plot actual test Values
plot(test$TargetVariable)

#SVM
summary(train)
names(train)

traindata <- subset(train, select  = -TargetVariable)
target <- subset(train, select = TargetVariable)

model_svm <- svm(traindata, target$TargetVariable,type='C', kernel='sigmoid', cost = 1000, gamma = 0.001, probability = TRUE)
print(model_svm)
summary(model_svm)

#Predict training data
predictSVM <- predict(model_svm, traindata, probability = T)

# Check accuracy:
table(predictSVM, target$TargetVariable)
train_pred <- attr(predictSVM, "probabilities")[,2] #Probabilities of SVM prediction
plot(train_pred)

#write.csv(train_pred, file = "SVMTrainingProbs.csv", row.names = FALSE)

#SVM TRAINING ROC curve#######################################################
pred <- prediction(train_pred,train$TargetVariable)
perf_AUC <- performance(pred,"auc") #Calculate the AUC value
AUC <- perf_AUC@y.values[[1]]

perf_ROC <- performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Training AUC = ",format(AUC, digits=5, scientific=FALSE)))

###################################################################################
# compute decision values and probabilities:
predictSVM <- predict(model_svm, traindata, decision.values = TRUE)
attr(predictSVM, "decision.values")[1:4,]
# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(train[,-101])),
     col = as.integer(train[,101]),
     pch = c("o","+")[1:50 %in% model_svm$index + 1])

#predict Test DAta
preds_svm <- predict(model_svm, test[,-101], probability = T)
write.csv(predictions, file = "randomforestTestingProbs.csv", row.names = FALSE)

##confusion matrix
table(pred = preds_svm, true = test[,101])
summary(preds_svm)
plot(preds_svm)

#SVM Training AUC
auc(preds_svm,test$TargetVariable)

#SVM TRAINING ROC curve#######################################################
pred <- prediction(preds_svm, test$TargetVariable)
perf_AUC <- performance(pred,"auc") #Calculate the AUC value
AUC <- perf_AUC@y.values[[1]]

perf_ROC <- performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Testing AUC = ",format(AUC, digits=5, scientific=FALSE)))

#Additive Regression
glm_model <- earth(train$TargetVariable~ ., data=train, degree=2, glm=list(family=binomial))
summary(glm_model)

glm_model_predict <- predict(glm_model, train)
plot(glm_model_predict)

names(glm_model)
print(glm_model$fitted.values)
plot(glm_model$fitted.values)
print(glm_model$glm.coefficients)

earth.mod <- earth(train$TargetVariable~ ., data=train, degree=2)
plot(earth.mod)
