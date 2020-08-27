install.packages("pROC")
install.packages("mlbench")
install.packages("caret")
library(pROC)
library(mlbench)
library(caret)
zoo <- read.csv(choose.files())
"Implement a KNN model to classify the animals in to categorie"

zoo <-  read.csv(choose.files())
head(zoo)
str(zoo)
table(zoo$type)
barplot(table(zoo$type),col = "forestgreen",xlab = "Types of animal",ylab = "Count")
barplot(table(zoo$fins),col = "forestgreen",xlab = "Types of animal",ylab = "Count")
barplot(table(zoo$hair),col = "forestgreen",xlab = "Types of animal",ylab = "Count")
barplot(table(zoo$eggs),col = "forestgreen",xlab = "Types of animal",ylab = "Count")
barplot(table(zoo$airborne),col = "forestgreen",xlab = "Types of animal",ylab = "Count")
barplot(table(zoo$aquatic),col = "forestgreen",xlab = "Types of animal",ylab = "Count")
nrow(zoo)
table(aquatic=zoo$aquatic,hair = zoo$hair) # there are animals which are aquatic and with hair 
plot(1:101,zoo$aquatic,col=ifelse(zoo$aquatic == zoo$hair,"blue","yellow"),pch = 20,cex=2) #aquatic animal with hair
# Upper blue dotted animals are Haired as well as aquatic and yellows are aquatic but not haired
# Lower blue coloured animals are non haired and non aquatic and yellows are non aquatic but haired
normalize_dummy <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

# Normalizing the data
Zoo <- data.frame("animal"=zoo$animal.name,"type"=zoo$type,normalize_dummy(zoo[,-c(1,18)]))

# there are 7 types of animal in the Zoo
set.seed(101)
splits<- sample(x = 1:nrow(Zoo),size = round(nrow(Zoo)*.3),replace = F)
Zoo_train <- Zoo[-splits,]
Zoo_test <- Zoo[splits,]
install.packages("class")
library(class)
set.seed(2) # I am fixing the rendomstate to an arbitrary number i.e. 2
model_2z <- knn(train = Zoo_train[,-(1:2)],test = Zoo_test[,-(1:2)],cl = factor(Zoo_train$type),k = 3)
pred_2z <- model_2z
confusionmatrix2 <- table(Zoo_test$type,model_2z)
mean(Zoo_test$type==model_2z) # Accuracy is 0.9

train_zacc <- c();test_zacc <- c()
for (i in seq(1,50,2)){
  # I am fixing the rendomstate to an arbitrary number i.e. 3 dont remove it, it may lead to different answers
  set.seed(3)
  pred_knns <- knn(train=Zoo_train[,-(1:2)],test=Zoo_train[,-(1:2)],cl=Zoo_train$type,k=i)
  train_zacc <- c(train_zacc,mean(Zoo_train$type==pred_knns))
  set.seed(3)
  perd_test_knn <- knn(train = Zoo_train[,-(1:2)], test = Zoo_test[,-(1:2)], cl = Zoo_train$type, k=i)
  test_zacc <- c(test_zacc,mean(Zoo_test$type==perd_test_knn))
};test_zacc;train_zacc


plot(seq(1,50,2),train_zacc,col="red",type='b',pch=20,main="Train and Test  Accuracy",xlab = "K value",ylab = "Accuracy")
lines(seq(1,50,2),test_zacc,col=ifelse(test_zacc==max(test_zacc),"blue","green"),type='b',pch=20,cex=ifelse(test_zacc==max(test_zacc),2,1))
legend(x = "topr",legend = c("Train Accuracy","Test Accuracy"),fill = c("red","green"),bty = "n")

set.seed(3);zoomodel <- knn(train = Zoo_train[,-(1:2)],test = Zoo_test[,-(1:2)],cl = factor(Zoo_train$type),k = 1)
confusionMat_Zoo <- table(Zoo_test$type,zoomodel)

ggplot(data = data.frame(test_zacc,train_zacc,k= seq(1,50,2)),aes(x=k))+geom_point(aes(y =test_zacc,x=k,col="Test Accuracy"))+
  geom_line(aes(y =test_zacc,x=k),col="pink")+geom_point(aes(y =train_zacc,x=k,col="Train Accuracy"))+
  geom_line(aes(y =train_zacc,x=k),colour="blue")+ylab(label = "")

sum(diag(confusionMat_Zoo)/sum(confusionMat_Zoo))

