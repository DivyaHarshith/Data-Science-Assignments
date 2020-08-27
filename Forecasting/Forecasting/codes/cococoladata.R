#=======================================================================================================================================
# Question no 2

df_cococola = read.csv(choose.files())
str(df_cococola)
df_cococola$ï..Quarter1
plot(df_cococola$Sales) # its increased on the basis of quarters
boxplot(df_cococola$Sales) # No outliers
nrow(df_cococola)
# __________________Pre processing my data__________________________

year <- rep(1986:1996,c(rep(4,length(1986:1996))))[1:42]
Quarters<- data.frame(outer(rep(c("Q1","Q2","Q3","Q4"),length = nrow(df_cococola)),c("Q1","Q2","Q3","Q4"),"==") + 0 )
colnames(Quarters) <- c("Q1","Q2","Q3","Q4")

df_cococola1 <-data.frame(year,Quarters,rn=1:nrow(df_cococola),Sales=df_cococola$Sales) 
head(df_cococola1)

# Train Test splitting
df_train <-df_cococola1[1:30,]
df_test <- df_cococola1[31:42,]

#_________________________1 Linear Trend model________________________

model_ltC1 <- lm(df_train$Sales~rn,data = df_train[,-7])
summary(model_ltC1) # Here my Rsquard value is 0.7079
pred_ltC1 <- predict(model_ltC1,df_test[,-7])
plot(df_test$Sales,type = "b",col="blue")
lines(pred_ltC1,type = "b",col="red")
cor(pred_ltC1,df_test$Sales) # I am getting 74.48% accuracy, not bad
rmse_ltC1<-sqrt(mean((df_test$Sales-pred_ltC1)^2)) # 714.0144

#_________________________2 Exponential Model____________________________
model_emC1 <- lm(log(df_train$Sales)~rn,data = df_train[,-7])
summary(model_emC1) # Here my Rsquard value is 0.7067
pred_emC1_ <- predict(model_emC1,df_test[,-7])
pred_emC1 <- exp(pred_emC1_)
plot(df_test$Sales,type = "b",col="blue")
lines(pred_emC1,type = "b",col="red")
cor(pred_emC1,df_test$Sales) # I am getting 74.15% accuracy, not bad
rmse_emC1<-sqrt(mean((df_test$Sales-pred_emC1)^2)) # 552.2821



#__________________________________3 MODEL 1 Additive Seasonality with linear trend________________________________
modelC1 <- lm(df_train$Sales~.,data = df_train[,-7])
summary(modelC1) # Here my Rsquard value is 0.8457
predC1 <- predict(modelC1,df_test[,-7])
plot(df_test$Sales,type = "b",col="blue")
lines(predC1,type = "b",col="red")
cor(predC1,df_test$Sales) # I am getting 97% accuracy, not bad
rmse_C1<-sqrt(mean((df_test$Sales-predC1)^2)) # 637.9405

#_____________________________________4 MODEL 2 Multiplicative Seasonality Linear trend________________________________
modelC2 <- lm(log(df_train$Sales)~.,data = df_train[,-7])
summary(modelC2) # Here my Rsquard value is 0.8586
predC2_ <- predict(modelC2,df_test[,-7])
predC2 <- exp(predC2_)
plot(df_test$Sales,type = "b",col="blue")
lines(predC2,type = "b",col="red")
cor(predC2,df_test$Sales) # I am getting 96.98% accuracy, not bad
rmse_C2<-sqrt(mean((df_test$Sales-predC2)^2)) # 410.2497

#_____________________________________5 MODEL 3 Multiplicative Seasonality Linear trend_______________________________

par(mfrow=c(1,2));plot(df_cococola$Sales)
plot(df_cococola$Sales[6:30]);par(mfrow=c(1,1))
# Neglecting the erratic compont in my data i.e. record 1 to 8
modelC3 <- lm(log(df_train$Sales[8:30])~.,data = df_train[8:30,-7])
summary(modelC3) # Here my Rsquard value is 0.9655
predC3_ <- predict(modelC3,df_test[,-7])
predC3 <- exp(predC3_)
plot(df_test$Sales,type = "b",col="blue")
lines(predC3,type = "b",col="red")
cor(predC3,df_test$Sales) # I am getting 97.36% accuracy, not bad
rmse_C3<-sqrt(mean((df_test$Sales-predC3)^2)) # 146.4575

# I am considering my Model 3 Multiplicative Seasonality Linear trend as my final model
