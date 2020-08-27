# Question no 1 ----
"Prepare a model for glass classification using KNN"

glass <- read.csv(choose.files())
table(glass$Type)
barplot(table(glass$Type),col='red',xlab="type of glass",ylab='Count')
str(glass)
df_glass <- data.frame(glass[,-10],type=factor(glass$Type))
head(df_glass)
round(prop.table(table(glass$Type))*100,2)
summary(glass[,-10])
boxplot(df_glass)
# Outlier are obvious in my data, but I won't remove according to my domain Ides.

#write.csv(summary(glass[,-10]),"out/summaryglass.csv")
normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col){
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for(j in 1:row)
      {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes =((x[j,i] - minx) / (maxx - minx)),no = 0)
      }
    }
    
  }
  f <- c()
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
  if(is.null(f)){
    output <- x
  }
  else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
}

df_glass <- data.frame(normalize_dummy(glass[,-10]),type=glass$Type)
set.seed(101)
split <- sample(x =1:nrow(df_glass) ,size = round(nrow(glass)*.3),replace = F)
train_df <- df_glass[-split,];dim(train_df)
test_df <- df_glass[split,];dim(test_df)

# Method 1 ----
library(caret)

model_knn1 <- knn3(x = train_df[,-10],y = factor(train_df$type),k = 5)
yhat <- round(predict(model_knn1,newdata = test_df[,-10]))
pred_1 <- c()
for (i in 1:nrow(yhat)){
  for(j in 1:ncol(yhat)){
    if(yhat[i,j] == 1){
      pred_1[i] <- as.numeric(colnames(yhat)[j])
    }
    
  }
}
table(test_df$type,pred_1)
mean(test_df$type==pred_1,na.rm=T) # 0.6491228
# In  This model some of the records are not classified at all so 
# The Classification method using this caret is inapprepriate for us, we may move to other functions.

# Method 2 ----

library("class")
set.seed(2) # I am fixing the rendomstate to an arbitrary number i.e. 2
model_2 <- knn(train = train_df[,-10],test = test_df[,-10],cl = factor(train_df$type),k = 5)
pred_2 <- model_2
confusionmatrix2 <- table(test_df$type,model_2)
mean(test_df$type==model_2) # Accuracy is 0.625

# From the above two methods we can perform any
train_acc <- c();test_acc <- c()
for (i in seq(1,50,2)){
  # I am fixing the rendomstate to an arbitrary number i.e. 3 dont remove it, it may lead to different answers
  set.seed(2);pred_knns <- knn(train=train_df[,-10],test=train_df[,-10],cl=train_df$type,k=i)
  train_acc <- c(train_acc,mean(train_df$type==pred_knns))
  set.seed(2);perd_test_knn <- knn(train = train_df[,-10], test = test_df[,-10], cl = train_df$type, k=i)
  test_acc <- c(test_acc,mean(perd_test_knn==test_df$type))
};test_acc


plot(seq(1,50,2),train_acc,col="red",type='b',pch=20,main="Train and Test  Accuracy",xlab = "K value",ylab = "Accuracy")
lines(seq(1,50,2),test_acc,col=ifelse(test_acc==max(test_acc),"blue","green"),type='b',pch=20,cex=ifelse(test_acc==max(test_acc),2,1))
legend(x = "topr",legend = c("Train Accuracy","Test Accuracy"),fill = c("red","green"),bty = "n")

ggplot(data = data.frame(test_acc,train_acc,k= seq(1,50,2)),aes(x=k))+geom_point(aes(y =test_acc,x=k,col="Test Accuracy"))+
  geom_line(aes(y =test_acc,x=k),col="pink")+geom_point(aes(y =train_acc,x=k,col="Train Accuracy"))+
  geom_line(aes(y =train_acc,x=k),colour="blue")+ylab(label = "")



#From this plot we can see taht train accuracy is highest for k = 1 and Test Accuracy is highest for k= 3
set.seed(2);model_final <- knn(train = train_df[,-10],test = test_df[,-10],cl = factor(train_df$type),k = 3)
pred_final <- model_final
confusionmatrixfinal <- table(test_df$type,model_final)
mean(test_df$type==model_final) # Accuracy is 0.71875
