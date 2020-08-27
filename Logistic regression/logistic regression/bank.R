install.packages("car")
install.packages("rocr")


library(car)
library(ROCR)
# Question 1 ----
"Whether the client has subscribed a term deposit or not "
# Reading the bank data
Bank <- read.csv(choose.files())
head(Bank)
colnames(Bank)
str(Bank)

# Clean unnecessary columns 
table(Bank$job);table(Bank$marital);table(Bank$education);table(Bank$default);table(Bank$housing);table(Bank$loan);table(Bank$contact);table(Bank$day);table(Bank$campaign);table(Bank$previous);table(Bank$poutcome);table(Bank$y);class(Bank$y)

df_bank <- Bank[,-c(9)]
df_bank$y

 # Model 1 __________________________________________________________
model_B_1 <- glm(y~.,data = df_bank,family = "binomial")
summary(model_B_1) # AIC= 22184


Y_B_1 <- predict(model_B_1,df_bank)
plot(Y_B_1,df_bank$y)
plot(Y_B_1)
prob_B_1 <-predict(model_B_1,df_bank,type = "response")
confu_B_1 <- table(prob_B_1>0.5,df_bank$y)
effi_B_1 <- sum(diag(confu_B_1))/sum(confu_B_1);effi_B_1 # Efficiency of my model is 0.9011303 

# Train and Test Split model
set.seed(101)
Test_Spl <- sample(x = 1:nrow(df_bank),size = round(nrow(df_bank)*(30/100)),replace = F)
Train_B_1 <- df_bank[-c(Test_Spl),]
Test_B_1 <- df_bank[c(Test_Spl),]
model_BT_1 <- glm(y~.,data = Train_B_1,family = "binomial")
summary(model_BT_1) # AIC = 15393
Y_BT_1 <- predict(model_BT_1,Test_B_1)
prob_BT_1 <-predict(model_B_1,Test_B_1,type = "response")
plot(prob_BT_1,Test_B_1$y,
     col=ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),"green","red")
     ,pch = ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),1,4) # Uncomment it (Ctrl+Shift+C) if you want different character also
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_BT_1,col=ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),"green","red"))

confu_BT_1 <- table(prob_BT_1>0.5,Test_B_1$y) ;confu_B_1
effi_BT_1 <- sum(diag(confu_BT_1))/sum(confu_BT_1);effi_BT_1 # Efficiency of my model is 0.9005382 


influencePlot(model_B_1)
influencePlot(model_BT_1)
influ <-as.integer(intersect(rownames(influencePlot(model_B_1)),rownames(influencePlot(model_BT_1))));influ
# ROC Curve
rocrpred<-prediction(prob_BT_1,Test_B_1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')


str(rocrperf)
rocrperf@x.values
colorize(x=rocrperf,colors = c(1,2,3))
plot(rocrperf,colorize=T) #text.adj=c(-0.2,1.7)

# Model2 _______________________________________________________
df_bank2 <- df_bank[-c(influ),]
model_B_2 <- glm(y~.,data = df_bank2,family = "binomial")
summary(model_B_2) # AIC= 22177
Y_B_2 <- predict(model_B_2,df_bank2)
plot(Y_B_2,df_bank2$y)
plot(Y_B_2)
prob_B_2 <-predict(model_B_2,df_bank2,type = "response")
confu_B_2 <- table(prob_B_2>0.5,df_bank2$y)
effi_B_2 <- sum(diag(confu_B_2))/sum(confu_B_2);effi_B_2 # Efficiency of my model is 0.901148 

# Train and Test Split and Model Evaluation
set.seed(101)
Test_Spl_2 <- as.integer(sample(x = rownames(df_bank2),size = round(nrow(df_bank2)*(30/100)),replace = F))
Train_B_2 <- df_bank2[-c(Test_Spl_2),]
Test_B_2 <- df_bank2[c(Test_Spl_2),]
model_BT_2 <- glm(y~.,data = Train_B_2,family = "binomial")
summary(model_BT_2) # AIC = 15456
Y_BT_2 <- predict(model_BT_2,Test_B_2)
prob_BT_2 <-predict(model_B_2,Test_B_2,type = "response")

plot(prob_BT_2,Test_B_2$y,
     col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red")
     #,pch = ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_BT_2,col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red"))

confu_BT_2 <- table(prob_BT_2>0.5,Test_B_2$y) ;confu_BT_2
effi_BT_2 <- sum(diag(confu_BT_2))/sum(confu_BT_2);effi_BT_2 # Efficiency of my model is 0.9005382 


#Cutoff value

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
######Conclusion-Here In model 1 and model 2 we can't see any major differences in our AIC,
  #Efficiency as well as F1Score in both of the models up to 3 decimal point is 
   ###almost same. As we know in our model 1 we have considered many insignificant
  ###variables and in model 2 we have considered only the significant variables for
  ##our model building. So I may consider my Model 2 as my final model