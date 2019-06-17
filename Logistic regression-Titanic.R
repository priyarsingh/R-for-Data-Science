Titanic <-read.csv("Titanic.csv",na.strings = "")
View(Titanic)
summary(Titanic)

library(dplyr)
#data preparation-------------------------------------------------
#removing unwanted columns:
Titanic1 <- Titanic[,c(-3,-8,-10,-12,-13,-14)]
View(Titanic1)

#mutate categorical columns sex and embarked:
Titanic1 <-mutate(Titanic1,sex1= ifelse(sex=="female",1,0))
Titanic1 <-mutate(Titanic1,embarked1= ifelse(embarked=="S",2,ifelse(embarked=="C",0,1)))

#replace NA with mean values for age and fare:
variable <-mean(Titanic1$age ,na.rm = TRUE)
variable
Titanic1$age[is.na(Titanic1$age)] <-variable 
variable
is.na(Titanic1$age)

variable1 <-mean(Titanic1$fare ,na.rm = TRUE) 
Titanic1$fare[is.na(Titanic1$fare)] <-variable1
View(Titanic1)

#selcting required columns after removing NA and mutate function:
Titanic1 <-select(Titanic1,1,2,4,5,6,7,9,10)
View(Titanic1)
summary(Titanic1)

#factoring:
Titanic1$pclass <- factor(Titanic1$pclass)
Titanic1$survived <- factor(Titanic1$survived)
Titanic1$sibsp <- factor(Titanic1$sibsp)
Titanic1$parch <- factor(Titanic1$parch)
Titanic1$sex1 <- factor(Titanic1$sex1)
Titanic1$embarked1 <- factor(Titanic1$embarked1)

#Sampling---------------------------------------------------------
Tsample <- sample(2,nrow(Titanic1),replace = TRUE,prob = c(.8,.2))
Tsample

#train data:
train_Tsample <-Titanic1[Tsample==1, ]
train_Tsample

#test data:
test_Tsample <-Titanic1[Tsample==2, ]
test_Tsample


#Model Building---------------------------------------------------------
#building model on train data:
Model_Titanic <- glm(survived~.,family = binomial,data = train_Tsample)

#predicting on test data:
predict_value <-predict(Model_Titanic,test_Tsample,type ="response")
predict_value

#creating data frame to compare actual data and predicted data:
predict_actual_df <-data.frame(predict_value,test_Tsample$survived)
predict_actual_df

#use mutate  function to replace values above .5 to 1 and below .5 to 0:
predict_actual_df <- mutate(predict_actual_df,predict_value = ifelse(predict_value > .5,1,0))
colnames(predict_actual_df) <-c("predict","actual")
predict_actual_df


table <-table(predict_actual_df$predict,predict_actual_df$actual)
table

