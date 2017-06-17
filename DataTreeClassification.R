library(caret)
library(randomForest)
library(FME)
data("iris")
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_ctree)
set.seed(12345)
part.Train<-createDataPartition(iris$Species,p=0.7,list=FALSE) #To partition the given subset of the database 
to.train<-iris[inTrain,]                              
to.test<-iris[-inTrain,]

x <- train(Species~.,method="rpart",data=training)
modFIT <- train(Species~.,method="rpart",data=training)

predicted <- predict(modFIT,testing[,-5])
View(predicted)