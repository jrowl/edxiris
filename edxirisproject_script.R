#import data (https://github.com/jrowl/edxiris)

data <- read.csv("iris.data", header=FALSE)

#set comlum names

colnames(data) <- c("sepal_length", "sepal_width", "petal_length" , "petal_width" , "class")

#view summary for data for outliers

summary(data)


#visually inspect data correlations

plot(data)

#create train and test index

set.seed(918)
index <- sample(1:nrow(data), nrow(data)*.75, replace = F)

#fit LDA model

library(MASS)

fit.lda <- lda(class~. , data[index,])

#build a confusion matrix of lda model results on train data
predict.lda <- predict(fit.lda, newdata = data[index,], type = "class")
table(predict.lda$class, data$class[index])

#calculate accuracy of model on train data
error.train.lda <- 1-(sum(predict.lda$class == data$class[index])/length(data$class[index]))

#fit qda model
fit.qda <- qda(class~. , data[index,])

#build a confusion matrix of qda model results on trianing data
predict.qda <- predict(fit.qda, newdata = data[index,], type = "class")
table(predict.qda$class, data$class[index])

#calculate accuracy of model on training data
error.train.qda <- 1-(sum(predict.qda$class == data$class[index])/length(data$class[index]))

#fit and prune tree model
library(rpart)
fit.tree <- rpart(class~. , data[index,])
fit.tree <- prune(fit.tree, cp = fit.tree$cptable[min(fit.tree$cptable[,3]) == fit.tree$cptable[,3],1])

#build a confusion matrix of tree model results on trianing data
predict.tree <- predict(fit.tree, newdata = data[index,], type = "class" , cp = fit.tree$cptable[min(fit.tree$cptable[,3]) == fit.tree$cptable[,3],1])
table(predict.tree, data$class[index])

#calculate accuracy of model on training data
error.train.tree <- 1-(sum(predict.tree == data$class[index])/length(data$class[index]))

#fit rf model
library(randomForest)
fit.rf <- randomForest(class~. , data[index,])

#build a confusion matrix of rf model results on trianing data
predict.rf <- predict(fit.rf, newdata = data[index,], type = "class")
table(predict.rf, data$class[index])

#calculate accuracy of model on training data
error.train.rf <- 1-(sum(predict.rf == data$class[index])/length(data$class[index]))

#lda test set performance

predict.lda <- predict(fit.lda, newdata = data[-index,], type = "class")
error.test.lda <- 1-(sum(predict.lda$class == data$class[-index])/length(data$class[-index]))

#qda test set performance

predict.qda <- predict(fit.qda, newdata = data[-index,], type = "class")
error.test.qda <- 1-(sum(predict.qda$class == data$class[-index])/length(data$class[-index]))

#tree test set petreeormance

predict.tree <- predict(fit.tree, newdata = data[-index,], type = "class")
error.test.tree <- 1-(sum(predict.tree == data$class[-index])/length(data$class[-index]))

#rf test set performance

predict.rf <- predict(fit.rf, newdata = data[-index,], type = "class")
error.test.rf <- 1-(sum(predict.rf == data$class[-index])/length(data$class[-index]))

#build comparison table
comparison <- data.frame(matrix(ncol=2, nrow=4))
colnames(comparison) <- c("train", "test")
rownames(comparison) <- c("lda", "qda", "tree" ,"rf")

comparison[1,1] <- error.train.lda
comparison[1,2] <- error.test.lda
comparison[2,1] <- error.train.qda
comparison[2,2] <- error.test.qda
comparison[3,1] <- error.train.tree
comparison[3,2] <- error.test.tree
comparison[4,1] <- error.train.rf
comparison[4,2] <- error.test.rf

round(comparison, 3)
