install.packages("party")
library(party)
str(iris)
head(iris)
?set.seed
set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
ind
tab
#Sepereate the data into train &test data
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
myFormula
iris_ctree <- ctree(myFormula, data=trainData)
#Function ctree() provides some parameters, such as 
#MinSplit,
# MinBusket,
# MaxSurrogate 
#MaxDepth, to control the training of decision trees. 
plot(iris_ctree)
#train_predict <- predict(iris_ctree)
train_predict <- predict(iris_ctree,trainData,type="response")
table(train_predict,trainData$Species)
mean(train_predict != trainData$Species) * 100#gives 96.4 % confidence
test_predict <- predict(iris_ctree, newdata= testData,type="response")
table(test_predict, testData$Species)
mean(test_predict != testData$Species) * 100# gives 94% confidence
print(iris_ctree)
plot(iris_ctree, type="simple")

