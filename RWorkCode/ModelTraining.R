attach(final.reduced.data)
summary(final.reduced.data)

set.seed(9)
Rindex<-1:nrow(final.reduced.data)
Rtestindex<-sample(Rindex,trunc(length(Rindex)*0.3))
Rtestset<-final.reduced.data[Rtestindex,]
Rtrainset<-final.reduced.data[-Rtestindex,]
library(rattle)
rattle()
#LOGISTIC REGRESSION
logitmodel<-glm(formula=TARGET~.,data=Rtrainset,family=binomial(link="logit"))
summary(logitmodel)
predictedmodel<-glm(formula = TARGET ~ .,data = Rtestset, family = binomial(link = "logit") )
summary(predictedmodel)
anova(logitmodel)
  
pred.probs <- predict(logitmodel)
length(pred.probs) 
pred.default <- rep("No", nrow(TARGET))
pred.default[pred.probs >0.5] <- "Yes"
table(pred.default)

confusion.matrix <- table(TARGET, pred.default)
confusion.matrix
addmargins(confusion.matrix)
accuracy <-((72495+20)/76020)
accuracy #0.9538937

#SVM
library(ISLR)
library(e1071)
tuned<-tune.svm(TARGET~.,data=Rtrainset,gamma=10^(-6:-1),cost=10^(-1:1))
summary(tuned)
set.seed(9)
tune.output <- tune(svm, TARGET ~ ., data = Rtrainset, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000)), gamma =seq(.5, .9, by = .1))
summary(tune.output)
set.seed(9)
tune.output.poly <- tune(svm, TARGET ~ ., data = Rtrainset, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100), degree = c(2, 3, 4)))
summary(tune.output.poly)
set.seed(9)
tune.output.rad <- tune(svm, TARGET ~ ., data = Rtrainset, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1, 10, 100)))
summary(tune.output.rad)

svm.linear.fit <- svm(TARGET ~ ., data = Rtestset, kernel = "linear", cost = 1, scale =FALSE)
svm.linear.fit
svm.poly.fit <- svm(TARGET ~ ., data = Rtestset, kernel = "polynomial", cost = 100, degree = 2, scale =FALSE)
svm.poly.fit
svm.rad.fit <- svm(TARGET ~ ., data = Rtestset, kernel = "radial", cost = 100, gamma = 0.01,scale =FALSE)
svm.rad.fit

pred.probs.svm.linear.fit <- predict(svm.linear.fit)
length(pred.probs) 
pred.default <- rep("No", nrow(TARGET))
pred.default[pred.probs >0.5] <- "Yes"
table(pred.default)

confusion.matrix <- table(TARGET, pred.default)
confusion.matrix
addmargins(confusion.matrix)


pred.probs.svm.poly.fit <- predict(svm.poly.fit)
length(pred.probs) 
pred.default <- rep("No", nrow(TARGET))
pred.default[pred.probs >0.5] <- "Yes"
table(pred.default)

confusion.matrix <- table(TARGET, pred.default)
confusion.matrix
addmargins(confusion.matrix)

pred.probs.svm.rad.fit <- predict(svm.rad.fit)
length(pred.probs) 
pred.default <- rep("No", nrow(TARGET))
pred.default[pred.probs >0.5] <- "Yes"
table(pred.default)

confusion.matrix <- table(TARGET, pred.default)
confusion.matrix
addmargins(confusion.matrix)


#Decision Tree
fit<-rpart(TARGET~.,method="class",data=Rtrainset,control=rpart.control(minsplit=1))
printcp(fit)

#Classification Tree
rpart(formula = TARGET ~ ., data = Rtrainset, method = "class", control = rpart.control(minsplit = 1))
decisiontree.pred<-predict(fit,testset[,-1],type="class")
tab<-table(pred=decisiontree.pred,actual=testset[,1])
#Root node error calculation
confusionMatrix(tab)

#Pruning
fancyRpartPlot(fit,main="Decision Tree Before Pruning")
printcp(fit)
rpart(formula = TARGET ~ ., data = Rtrainset, method = "class", 
      control = rpart.control(minsplit = 1))
#Root node error calculation
Plotcp(fit)
#Pruning and after pruning
pfit<-prune(fit,cp=0.019704)
prediction<-predict(pfit,testset[,-1],type="class")
tabnew<-table(pred=prediction,actual=testset[,1])
confusionMatrix(tabnew)






                  

