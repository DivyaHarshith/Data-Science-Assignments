#airlines data set
df <- read.csv(choose.files())
plot(df$Passengers,type = "b")
plot(df$Passengers)
plot(log(df$Passengers))#from this plot wecan see the seasonality and trend ofthe data
table(df$ï..Month)
str(df)
as.Date(df$ï..Month)
df$ï..Month
#-------------------------DATA PREPROCESSING----------------------------------------------
# Adding the season columns
year <- rep(1995:2002,c(rep(12,length(1995:2002))))
month<- data.frame(outer(rep(month.abb,length = nrow(df)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

df_1 <-data.frame(year,month,rn=1:nrow(df),Passengers=df$Passengers,day=seq(1,2880,by = 30)) 

# train and test splitting
df_train <-df_1[1:80,]
df_test <- df_1[81:96,]
#_________________________1 Linear Trend model________________________

model_lt1 <- lm(df_train$Passengers~day,data = df_train[,-15])
summary(model_lt1) # Here my Rsquard value is 0.7829

pred_lt1 <- predict(model_lt1,df_test[,-15])

plot(df_test$Passengers,type = "b",col="blue")

lines(pred_lt1,type = "b",col="red")

cor(pred_lt1,df_test$Passengers) # I am getting 42% accuracy, not bad

rmse_lt1<-sqrt(mean((df_test$Passengers-pred_lt1)^2)) # 47.5462
rmse_lt1
#_________________________2 Exponential Model____________________________
model_em1 <- lm(log(df_train$Passengers)~day,data = df_train[,-15])

summary(model_em1) # Here my Rsquard value is 0.8181

pred_em1 <- predict(model_em1,df_test[,-15])

pred_em2 <- exp(pred_em1_)
plot(df_test$Passengers,type = "b",col="blue")
lines(pred_em2,type = "b",col="red")
cor(pred_em2,df_test$Passengers) # I am getting 42% accuracy, not bad
rmse_em1<-sqrt(mean((df_test$Passengers-pred_em1)^2)) # 43.79374


#_________________________3 MODEL 1 Additive Seasonality_________________________________
model1 <- lm(df_train$Passengers~.,data = df_train[,-15])
summary(model1) # Here my Rsquard value is 0.9533
pred1 <- predict(model1,df_test[,-15])
plot(df_test$Passengers,type = "b",col="blue")
lines(pred1,type = "b",col="red")
cor(pred1,df_test$Passengers) # I am getting 98% accuracy, not bad
rmse_1<-sqrt(mean((df_test$Passengers-pred1)^2)) # 33.04

#_________________________4 MODEL 2 Multiplicative Seasonality Linear trend_________________________________

model2 <- lm(log(df_train$Passengers)~.,data = df_train[,-15])
summary(model2) # Here Rsquare is 0.9743
pred2_ <- predict(model2,newdata = df_test[,-15])
pred2 <- exp(pred2_)
plot(df_test$Passengers,type = "b",col="blue")
lines(pred2,type = "b",col="red")
cor(pred2,df_test$Passengers) # I am getting 98.8% accuracy, not bad
rmse_2<-sqrt(mean((df_test$Passengers-pred2)^2)) # 9.469

#conclusion
#i may choose 4th model as it has 98.8% accuracy,